# question_1.R
# This script will create the DS domain with the following variables: STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY

install.packages(c("admiral", "sdtm.oak", "gt", "ggplot2", "pharmaverseraw", "pharmaversesdtm", "crayon", "this.path"))

library(sdtm.oak)
library(pharmaverseraw)
library(pharmaversesdtm)
library(dplyr)
library(this.path)

# Read in data
ds_raw <- pharmaverseraw::ds_raw
dm <- pharmaversesdtm::dm

# Create oak_id_vars
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

# Read in CT
# study_ct file missing VISIT/VISITNUM for "Unscheduled 6.1", "Unscheduled 1.1", "Unscheduled 5.1", "Unscheduled 4.1", "Unscheduled 8.2", and "Unscheduled 13.1".
study_ct <- read.csv("metadata/sdtm_ct.csv")

# Adding synonyms to study_ct to match raw data
study_ct[study_ct$collected_value == "Study Terminated By Sponsor",]$term_synonyms <- "Study Terminated by Sponsor"
study_ct[study_ct$collected_value == "Trial Screen Failure",]$term_synonyms <- "Screen Failure"
study_ct[study_ct$collected_value == "Lost To Follow-Up",]$term_synonyms <- "Lost to Follow-Up"
study_ct[study_ct$collected_value == "Ambul ECG Removal",]$term_synonyms <- "Ambul Ecg Removal"

# Map topic variable
ds <-
  # Derive topic variable
  # Map DSTERM using assign_no_ct, raw_var=IT.DSTERM, tgt_var=DSTERM
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

# Map rest of variables
ds <- ds %>%
  # Map DSTERM using condition_add and assign_no_ct, raw_var=OTHERSP, tgt_var=DSTERM
  assign_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  # Map DSDECOD using condition_add, assign_ct, and assign_no_ct
  # If OTHERSP is null then map to DSDECOD
  # If OTHERSP is not null then map OTHERSP to DSDECOD
  assign_ct(
    raw_dat = condition_add(ds_raw, is.na(OTHERSP)),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  # Map DSCAT using hardcode_ct and condition_add
  # If IT.DSDECOD = Randomized then DSCAT = PROTOCOL MILESTONE else DSCAT = DISPOSITION EVENT
  # If OTHERSP is not null then DSCAT = OTHER EVENT
  hardcode_ct(
    raw_dat = condition_add(ds_raw, IT.DSDECOD == "Randomized"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "PROTOCOL MILESTONE",
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(ds_raw, IT.DSDECOD != "Randomized"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "DISPOSITION EVENT",
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSCAT",
    tgt_val = "OTHER EVENT",
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  ) %>%
  # Map VISIT from INSTANCE using assign_ct
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  # Map VISITNUM from INSTANCE using assign_ct
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  )

# Map date variables
ds <- ds %>%
  # Map DSSTDTC using assign_datetime, raw_var=IT.DSSTDAT
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = c("m-d-y")
  ) %>%
  # Map DSDTC using assign_datetime, raw_var=IT.DSSTDAT
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL","DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("m-d-y","H:M"),
  )
  
# Create SDTM derived variables
ds <- ds %>%
  dplyr::mutate(
    STUDYID = ds_raw$STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM),
    DSTERM = toupper(DSTERM)
  ) %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSTERM")
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFSTDTC",
    study_day_var = "DSSTDY"
  ) %>%
  select(
    "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT", "VISITNUM", "VISIT", "DSDTC", 
    "DSSTDTC", "DSSTDY"
  )

# Basic export, ensuring row names are removed
write.csv(ds, "ds.csv", row.names = FALSE)

# Stop diverting output and close the connection
sink()