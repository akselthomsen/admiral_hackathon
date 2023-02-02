library(admiral)
library(metacore)
library(metatools)
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

dm <- haven::read_xpt("sdtm/dm.xpt") |>
  mutate(across(where(is.character), ~ na_if(.x, "")))

suppdm <- haven::read_xpt("sdtm/suppdm.xpt") |>
  mutate(across(where(is.character), ~ na_if(.x, "")))

vs <- haven::read_xpt("sdtm/vs.xpt") |>
  mutate(across(where(is.character), ~ na_if(.x, "")))

# Build ADSL ----

## Generic using metacore ----

adsl_0 <- meta |>
  build_from_derived(
    ds_list = list(dm = dm, suppdm = suppdm),
    predecessor_only = FALSE,
    keep = FALSE)

## Create needed sub data.frames for using the codelists ----

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

## Add additional variables ----

adsl_1 <- adsl_0 |>
  derive_vars_merged(dataset_add = dm,
                     by_vars = vars(USUBJID),
                     new_vars = vars(ARM)) |>
  derive_vars_dt(new_vars_prefix = "RFST", dtc = RFSTDTC) |>
  derive_vars_dt(new_vars_prefix = "RFEN", dtc = RFENDTC) |>
  derive_vars_merged(dataset_add = armn,
                     by_vars = vars(TRT01P),
                     new_vars = vars(TRT01PN = code)) |>
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
  derive_vars_merged(dataset_add = vs,
                     by_vars = vars(USUBJID),
                     new_vars = vars(HEIGHTBL = VSSTRESN),
                     filter_add = VSTESTCD == "HEIGHT" & VISITNUM == 1) |>
  derive_vars_merged(dataset_add = vs,
                     by_vars = vars(USUBJID),
                     new_vars = vars(WEIGHTBL = VSSTRESN),
                     filter_add = VSTESTCD == "WEIGHT" & VISITNUM == 3) |>
  mutate(
    BMIBL = WEIGHTBL / (HEIGHTBL/100)**2,
    BMIBLGR1 = case_when(BMIBL < 25 ~ "<25",
                         BMIBL >= 30 ~ ">=30",
                         !is.na(BMIBL) ~ "25-<30")
    )

## Check progress ----

summary(adsl_1)

meta$var_spec$variable |>
  setdiff(names(adsl_1)) |>
  sort()

# Export to XPT ----





