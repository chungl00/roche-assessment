# question_3
# This script will create the AE summary table

install.packages(c("gt", "gtsummary", "pharmaverseadam", "this.path"))

# Load libraries & data -------------------------------------
library(dplyr)
library(gtsummary)
library(gt)
library(this.path)

adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

# Pre-processing --------------------------------------------
adae <- adae |>
  filter(
    # safety population
    SAFFL == "Y",
    # treatment-emergent adverse events
    TRTEMFL == "Y"
  )

tbl <- adae |>
  tbl_hierarchical(
    variables = c(AETERM, AESOC),
    by = TRT01A,
    id = USUBJID,
    denominator = adsl,
    overall_row = TRUE,
    label = "..ard_hierarchical_overall.." ~ "Any TEAE"
  ) |>
  # Add the 'Total' column
  add_overall(last = FALSE, col_label = "**Total**  \nN = {style_number(N)}")

# Sort all variables by descending frequency (default)
tbl <- sort_hierarchical(tbl)

setwd(this.path::this.dir())

tbl |> 
  as_gt() |>
  gt::gtsave(filename = "ae_summary_table.html")
