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
  na_if("")

suppdm <- haven::read_xpt("sdtm/suppdm.xpt") |>
  na_if("")

# Build ADSL ----

## Generic using metacore ----

adsl_0 <- meta |>
  build_from_derived(
    ds_list = list(dm = dm, suppdm = suppdm),
    predecessor_only = FALSE,
    keep = FALSE)

## Add additional variables ----

adsl_1 <- adsl_0 |>
  derive_vars_merged(dataset_add = dm,
                     by_vars = vars(USUBJID),
                     new_vars = vars(ARM)) |>
  derive_vars_dt(new_vars_prefix = "RFST", dtc = RFSTDTC) |>
  derive_vars_dt(new_vars_prefix = "RFEN", dtc = RFENDTC) |>
  mutate(TRT01A = TRT01P,
         SITEGR1 = "900") |>
  derive_vars_merged(dataset_add = meta$codelist)



adsl_1

adsl_1 |>
  is.na() |>
  summary()

meta$ds_vars$variable |>
  setdiff(names(adsl_1))

adsl_1 |>
  select(any_of(meta$ds_vars$variable))

