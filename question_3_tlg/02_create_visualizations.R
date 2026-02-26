# question_3
# This script will create the visualizations using {ggplot2}

install.packages(c("gt", "gtsummary", "pharmaverseadam", "this.path", "binom", "tidyverse", "forcats"))

library(pharmaverseadam)
library(gtsummary)
library(ggplot2)
library(dplyr)
library(this.path)
library(binom)
library(tidyverse)
library(forcats)

# Plot 1: AE severity distribution by treatment

adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

ggplot(adae, aes(x = TRT01A, fill = AESEV)) +
  geom_bar() +
  xlab("Treatment Arm") +
  ylab("Count of AEs") +
  ggtitle("AE severity distribution by treatment")

setwd(this.path::this.dir())
ggsave("plot1.png")

# Plot 2: Top 10 most frequent AEs (with 95% CI for incidence rates)

# Event Rates
tbl <- adae |>
  tbl_hierarchical(
  variables = c(AESOC, AETERM),
  by = TRT01A,
  denominator = cards::ADSL,
  id = USUBJID,
  digits = everything() ~ list(p = 1),
  overall_row = TRUE,
  label = list(..ard_hierarchical_overall.. = "Any Adverse Event")
  ) |>
  # Add the 'Total' column
  add_overall(last = FALSE, col_label = "**Total**  \nN = {style_number(N)}")

# Sort all variables by descending frequency (default)
tbl <- sort_hierarchical(tbl)

df <- tbl$table_body
df_plot <- df %>%
  filter(row_type == "level") %>%
  select(label, stat_0) %>%
  slice(c(2:11)) # Selects top 10 most frequent AEs

df_plot <- df_plot %>%
  separate(stat_0, into = c("n","N"), sep = " \\(", remove = FALSE) %>%
  mutate(
    n = as.numeric(n),
    N = 254
  )

df_plot <- df_plot %>%
  mutate(
    percent = 100 * n / N
  )

ci <- binom.confint(df_plot$n, df_plot$N, method = "exact")

df_plot <- df_plot %>%
  mutate(
    lower = 100 * ci$lower,
    upper = 100 * ci$upper
  )

ggplot(df_plot,
       aes(x = fct_reorder(label, 100*n/N),
           y = percent)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2,
                size = 0.8) +
  coord_flip() +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = "n = 254 subjects; 95% Clopper–Pearson Confidence Intervals",
    x = NULL,
    y = "Percentage of Patients (%)"
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())

ggsave("plot2.png")
