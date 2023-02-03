library(admiral)
library(metacore)
library(metatools)
library(xportr)
library(tidyverse)

# Read and prepare metadata ----

specs <- read_all_sheets("metadata/specs.xlsx")

ds_spec <- specs |>
  spec_type_to_ds_spec()

ds_vars <- specs |>
  spec_type_to_ds_vars(key_seq_sep_sheet = FALSE)

var_spec <- specs |>
  spec_type_to_var_spec()

value_spec <- specs |>
  spec_type_to_value_spec(where_sep_sheet = FALSE)

deriviation <- specs |>
  spec_type_to_derivations() |>
  filter(derivation |> str_detect("ADSL", negate = TRUE)) |>
  filter(derivation_id != derivation)

codelist <- specs |>
  spec_type_to_codelist()

meta <- metacore(ds_spec, ds_vars, var_spec, value_spec, deriviation, codelist) |>
  select_dataset("ADSL")

# Read SDTM ----

list(dm = "sdtm/dm.xpt", suppdm = "sdtm/suppdm.xpt") |>
  map(haven::read_xpt)

dm <- haven::read_xpt("sdtm/dm.xpt") |>
  mutate(across(where(is.character), ~ na_if(.x, "")))

suppdm <- haven::read_xpt("sdtm/suppdm.xpt") |>
  mutate(across(where(is.character), ~ na_if(.x, "")))

vs <- haven::read_xpt("sdtm/vs.xpt") |>
  mutate(across(where(is.character), ~ na_if(.x, "")))

sc <- haven::read_xpt("sdtm/sc.xpt") |>
  mutate(across(where(is.character), ~ na_if(.x, "")))

ds <- haven::read_xpt("sdtm/ds.xpt") |>
  mutate(across(where(is.character), ~ na_if(.x, "")))

sv <- haven::read_xpt("sdtm/sv.xpt") |>
  mutate(across(where(is.character), ~ na_if(.x, "")))

mh <- haven::read_xpt("sdtm/mh.xpt") |>
  mutate(across(where(is.character), ~ na_if(.x, "")))

ex <- haven::read_xpt("sdtm/ex.xpt") |>
  mutate(across(where(is.character), ~ na_if(.x, "")))

qs <- haven::read_xpt("sdtm/qs.xpt") |>
  mutate(across(where(is.character), ~ na_if(.x, "")))

# Build ADSL ----

## Generic using metacore ----

adsl_0 <- meta |>
  build_from_derived(
    ds_list = list(dm = dm, suppdm = suppdm),
    predecessor_only = FALSE,
    keep = FALSE)

## Create needed sub data.frames for using the code lists etc. ----

armn <- codelist |>
  filter(code_id == "ARMN") |>
  pluck("codes", 1) |>
  mutate(TRT01P = decode,
         TRT01A = decode,
         code = as.numeric(code))

agegr1n <- codelist |>
  filter(code_id == "AGEGR1N") |>
  pluck("codes", 1) |>
  mutate(AGEGR1N = as.numeric(code))

race <- codelist |>
  filter(code_id == "RACE") |>
  pluck("codes", 1) |>
  mutate(RACE = code,
         decode = as.numeric(decode))

dm_flags <- suppdm |>
  filter(str_detect(QNAM, "COMPLT") |
           QNAM %in% c("EFFICACY", "SAFETY", "ITT")) |>
  select(USUBJID, QNAM, QVAL) |>
  mutate(QNAM = case_when(str_detect(QNAM, "COMPLT") ~ str_remove(QNAM, "LT"),
                          TRUE ~ str_sub(QNAM, 1, 3)) |>
           paste0("FL")) |>
  pivot_wider(names_from = QNAM, values_from = QVAL)

## Add additional variables ----

adsl_1 <- adsl_0 |>
  ### Derive ARM variables ----
  derive_vars_merged(dataset_add = dm,
                     by_vars = vars(USUBJID),
                     new_vars = vars(ARM)) |>
  derive_vars_merged(dataset_add = armn,
                     by_vars = vars(TRT01P),
                     new_vars = vars(TRT01PN = code)) |>
  ### Derive date variables ----
  # derive_vars_dt(new_vars_prefix = "RFST", dtc = RFSTDTC) |>
  derive_vars_dt(new_vars_prefix = "RFEN", dtc = RFENDTC) |>
  ### Derive TRT variables and demographics  ----
  mutate(
    TRT01A = TRT01P,
    TRT01AN = TRT01PN,
    SITEGR1 = "900",
    AGEGR1N = case_when(AGE < 65 ~ 1,
                        AGE > 80 ~ 3,
                        TRUE ~ 2),
    ) |>
  derive_vars_merged(dataset_add = agegr1n,
                     by_vars = vars(AGEGR1N),
                     new_vars = vars(AGEGR1 = decode)) |>
  derive_vars_merged(dataset_add = race,
                     by_vars = vars(RACE),
                     new_vars = vars(RACEN = decode)) |>
  derive_vars_merged(dataset_add = sc,
                     by_vars = vars(USUBJID),
                     new_vars = vars(EDUCLVL = SCSTRESN),
                     filter_add = SCTESTCD == "EDLEVEL") |>
  ### Derive baseline ----
  derive_vars_merged(dataset_add = vs,
                     by_vars = vars(USUBJID),
                     new_vars = vars(HEIGHTBL = VSSTRESN),
                     filter_add = VSTESTCD == "HEIGHT" & VISITNUM == 1) |>
  derive_vars_merged(dataset_add = vs,
                     by_vars = vars(USUBJID),
                     new_vars = vars(WEIGHTBL = VSSTRESN),
                     order = vars(VISITNUM),
                     mode = "last",
                     filter_add = VSTESTCD == "WEIGHT" & VISITNUM <= 3) |>
  mutate(
    BMIBL = WEIGHTBL / (HEIGHTBL/100)**2,
    BMIBLGR1 = case_when(BMIBL < 25 ~ "<25",
                         BMIBL >= 30 ~ ">=30",
                         !is.na(BMIBL) ~ "25-<30")
    ) |>
  ### Derive population flags ----
  left_join(dm_flags, by = "USUBJID") |>
  mutate(across(names(dm_flags), ~ coalesce(.x, "N"))) |>
  ### Derive disposition variables ----
  derive_vars_merged(dataset_add = ds,
                     by_vars = vars(USUBJID),
                     new_vars = vars(DCDECOD = DSDECOD),
                     filter_add = DSCAT == "DISPOSITION EVENT") |>
  mutate(
    DCSREAS = str_to_title(DCDECOD),
    DISCONFL = if_else(DCDECOD == "COMPLETED", NA_character_, "Y"),
    DSRAEFL = if_else(DCDECOD == "ADVERSE EVENT", "Y", NA_character_)
    ) |>
  ### Derive dates of different timings ----
  derive_vars_merged(dataset_add = sv,
                     by_vars = vars(USUBJID),
                     new_vars = vars(VISIT1DT = SVSTDTC),
                     filter_add = VISITNUM == 1) |>
  derive_vars_merged(dataset_add = sv,
                     by_vars = vars(USUBJID),
                     new_vars = vars(TRTSDT = SVSTDTC),
                     filter_add = VISITNUM == 3) |>
  derive_vars_merged(dataset_add = mh,
                     by_vars = vars(USUBJID),
                     new_vars = vars(DISONSDT = MHSTDTC),
                     filter_add = MHCAT == "PRIMARY DIAGNOSIS") |>
  derive_vars_merged(dataset_add = ex,
                     by_vars = vars(USUBJID),
                     new_vars = vars(TRTEDT = EXENDTC),
                     order = vars(VISITNUM),
                     mode = "last") |>
  mutate(across(c(VISIT1DT, TRTSDT, DISONSDT, TRTEDT), convert_dtc_to_dt)) |>
  derive_vars_duration(new_var = TRTDURD, start_date = TRTSDT, end_date = TRTEDT) |>
  derive_vars_duration(new_var = DURDIS,
                       start_date = VISIT1DT,
                       end_date = DISONSDT,
                       out_unit = "months") |>
  mutate(DURDSGR1 = case_when(DURDIS >= 12 ~ ">=12",
                              !is.na(DURDIS) ~ "<12")) |>
  ### Summary stats from SDTM ----
  derive_var_merged_summary(dataset_add = qs,
                            by_vars = vars(USUBJID),
                            new_var = MMSETOT,
                            filter_add = QSCAT == "MINI-MENTAL STATE",
                            analysis_var = QSSTRESN,
                            summary_fun = sum)

## Check progress ----

adsl_1

summary(adsl_1)

summary(is.na(adsl_1))

meta$var_spec$variable |>
  setdiff(names(adsl_1)) |>
  sort()

# Check data and export to XPT ----

adsl_2 <- adsl_1 |>
  check_variables(meta) |>
  check_ct_data(meta) |>
  sort_by_key(meta) |>
  xportr_type(meta) |>
  xportr_length(meta) |>
  xportr_label(meta) |>
  xportr_df_label(meta)

xportr_write(adsl_2, "adam/adsl.xpt")
