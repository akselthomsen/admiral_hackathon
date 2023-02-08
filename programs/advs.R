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
  spec_type_to_var_spec() |>
  mutate(type = case_when(type == "float" ~ "numeric",
                          type == "text" ~ "character",
                          TRUE ~ type)
  )

value_spec <- specs |>
  spec_type_to_value_spec(where_sep_sheet = FALSE)

deriviation <- specs |>
  spec_type_to_derivations() |>
  filter(derivation |> str_detect("ADVS", negate = TRUE)) |>
  filter(derivation_id != derivation) |>
  filter(derivation |> str_detect("^[:alnum:]+\\.[:alnum:]+$"))

codelist <- specs |>
  spec_type_to_codelist()

meta <- metacore(ds_spec, ds_vars, var_spec, value_spec, deriviation, codelist) |>
  select_dataset("ADVS")

# Read VS and ADSL ----

vs <- haven::read_xpt("sdtm/vs.xpt") |>
  convert_blanks_to_na()

vs_vars <- meta$derivations |>
  filter(derivation |> str_detect("^VS\\.")) |>
  filter(derivation_id != "ADVS.BASETYPE") |> # REMOVED
  mutate(new = derivation_id |> str_remove("^ADVS\\."),
         old = derivation |> str_remove("^VS\\.")) |>
  with(set_names(old, new))

adsl <- haven::read_xpt("adam/adsl.xpt") |>
  convert_blanks_to_na()

adsl_vars <- meta$derivations |>
  filter(derivation |> str_detect("^ADSL\\.")) |>
  mutate(new = derivation_id |> str_remove("^ADVS\\."),
         old = derivation |> str_remove("^ADSL\\.")) |>
  with(set_names(old, new))

# Build ADVS ----

## Generic  ----

advs_0 <- adsl |>
  select(all_of(adsl_vars)) |>
  left_join(vs |> select(USUBJID, all_of(vs_vars)),
            by = "USUBJID",
            multiple = "all")

## Add additional variables ----

advs_0 |>
  derive_var_base(by_vars = vars(USUBJID, PARAMCD, ATPT))

meta$var_spec$variable |>
  setdiff(names(advs_0)) |>
  sort()

