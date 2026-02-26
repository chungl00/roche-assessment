# question_2
# This script will create the ADSL using the input SDTM data, the {admiral} family of packages, and tidyverse tools
# The following variables will be derived: AGEGR9 & AGEGR9N, TRTSDTM & TRTSTMF, ITTFL, LSTALVDT 

install.packages(c("admiral", "dplyr", "pharmaversesdtm", "lubridate", "stringr", "this.path"))

library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)
library(this.path)

dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
lb <- pharmaversesdtm::lb

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
lb <- convert_blanks_to_na(lb)

adsl <- dm %>%
  select(-DOMAIN)

# Derive birth date from BRTHDTC
adsl <- adsl %>%
  derive_vars_dt(
    new_vars_prefix = "BRTH",
    dtc = BRTHDTC
  )

# Convert character date to numeric date without imputation
ds_ext <- derive_vars_dt(
  ds,
  dtc = DSSTDTC,
  new_vars_prefix = "DSST"
)

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    filter_add = DSDECOD == "RANDOMIZED",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(RANDDT = DSSTDT)
  )

# Derive AAGE and AAGEU
adsl <- adsl %>%
  derive_vars_aage(
    start_date = BRTHDT,
    end_date = RANDDT
  )

# Derive grouping AGEGR9 & AGEGR9N: “<18”, “18 - 50”, “>50”
# Create lookup tables
agegr9_lookup <- exprs(
  ~condition,            ~AGEGR1, ~AGEGR1N,
  is.na(AGE),          "Missing",        4,
  AGE < 18,                "<18",        1,
  between(AGE, 18, 50),  "18-50",        2,
  !is.na(AGE),             ">50",        3
)

adsl <- adsl %>%
  derive_vars_cat(
    definition = agegr9_lookup
  )

# Derive TRTSDTM & TRTSTMF
# Impute start time of exposure for missing hours and minutes but not seconds
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    highest_imputation = "h", # Impute up to hours
    time_imputation = "00:00:00", # Set missing time to 00:00:00
    ignore_seconds_flag = TRUE # Do not flag seconds as imputed
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  # Treatment End Datetime
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
    # Treatment Start Date and End Date
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) %>% # Convert Datetime variables to date
    # Treatment Start Time
  derive_vars_dtm_to_tm(source_vars = exprs(TRTSDTM))

# Derive ITTFL
adsl <- adsl %>%
  derive_var_merged_exist_flag(
    dataset_add = dm,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = ITTFL,
    false_value = "N",
    missing_value = "N",
    condition = (!is.na(ARM))
  )

# Derive LSTAVLDT: derive_vars_extreme_event selects the maximum date from the series of events
adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      event(
        dataset_name = "vs",
        order = exprs(VSSTRESN, VSSTRESC, VSDTC, VSSEQ),
        condition = ((!is.na(VSSTRESN) & !is.na(VSSTRESC)) & !is.na(VSDTC)),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(VSDTC, highest_imputation = "M"),
          seq = VSSEQ
        ),
      ),
      event(
        dataset_name = "ae",
        order = exprs(AESTDTC, AESEQ),
        condition = !is.na(AESTDTC),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(AESTDTC, highest_imputation = "M"),
          seq = AESEQ
        ),
      ),
      event(
        dataset_name = "ds",
        order = exprs(DSSTDTC, DSSEQ),
        condition = !is.na(DSSTDTC),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(DSSTDTC, highest_imputation = "M"),
          seq = DSSEQ
        ),
      ),
      event(
        dataset_name = "adsl",
        condition = !is.na(TRTEDT),
        set_values_to = exprs(LSTALVDT = TRTEDT, seq = 0),
      )
    ),
    source_datasets = list(ae = ae, vs = vs, ds = ds, adsl = adsl),
    tmp_event_nr_var = event_nr,
    order = exprs(LSTALVDT, seq, event_nr),
    mode = "last",
    new_vars = exprs(LSTALVDT)
  )

# Basic export, ensuring row names are removed
setwd(this.path::this.dir())
write.csv(adsl, "adsl.csv", row.names = FALSE)