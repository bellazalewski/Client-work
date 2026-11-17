# Memorial Health: Snowflake monthly campaign reporting workflow
#
# Uses the Snowflake export currently at the path below. When rerunning from a
# different computer, update campaign_file or copy the export into data/raw/.
# CTM's call-level export is still needed separately for caller outcomes,
# durations, holds, transfers, and access-quality analysis.

# Install once if necessary (do not run on every analysis):
# install.packages(c("tidyverse", "readr", "tibble", "dplyr", "tidyr"))
library(tidyverse)
library(readr)
install.packages("scales")
library(scales)
install.packages("lubridate")
library(lubridate)
install.packages("readr")

campaign_file <- "/Users/bellazalewski/Downloads/memorial_reporting_2026.csv"
output_dir <- file.path("outputs", "memorial_reporting")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

report_start <- as.Date("2026-01-01")
report_end <- as.Date("2026-10-31")

stopifnot(file.exists(campaign_file))

report_raw <- read_csv(campaign_file, show_col_types = FALSE, name_repair = "unique")
required_columns <- c("DATE", "CAMPAIGN", "SERVICELINE", "CHANNEL", "TACTIC",
                      "SPEND", "CONVERSIONS", "CALL_CONVERSIONS")
missing_columns <- setdiff(required_columns, names(report_raw))
stopifnot(length(missing_columns) == 0)

# Inventory and schema checks make changes in future Snowflake extracts visible.
write_csv(
  tibble(
    file = basename(campaign_file),
    rows = nrow(report_raw),
    columns = ncol(report_raw),
    missing_cells = sum(is.na(report_raw)),
    date_values = n_distinct(report_raw$DATE)
  ),
  file.path(output_dir, "input_inventory.csv")
)

report <- report_raw %>%
  mutate(
    report_date = ymd(as.character(DATE), quiet = TRUE),
    month = floor_date(report_date, unit = "month") %>% as.Date(),
    campaign = str_squish(as.character(CAMPAIGN)),
    service_line = str_squish(as.character(SERVICELINE)),
    channel = str_squish(as.character(CHANNEL)),
    tactic = str_squish(as.character(TACTIC)),
    nomenclature_status = recode(
      as.character(NEWNOMENCLATURE),
      "TRUE" = "True",
      "FALSE" = "False",
      .missing = "Missing in source",
      .default = as.character(NEWNOMENCLATURE)
    ),
    spend = parse_number(as.character(SPEND)),
    conversions = parse_number(as.character(CONVERSIONS)),
    reported_call_conversions = parse_number(as.character(CALL_CONVERSIONS)),
    web_conversions = parse_number(as.character(WEB_CONVERSIONS))
  ) %>%
  filter(!is.na(month), month >= report_start, month <= report_end)

# DATE is month-start in this export. Rows are below campaign/month grain and
# appear split across placements, creatives, sites, and other reporting fields.
# Roll up additive metrics, retaining channel, tactic, and nomenclature status
# so the analyst can spot unlike reporting populations before combining them.
campaign_month <- report %>%
  group_by(month, campaign, service_line, channel, tactic, nomenclature_status) %>%
  summarise(
    spend = sum(spend, na.rm = TRUE),
    conversions = sum(conversions, na.rm = TRUE),
    reported_call_conversions = sum(reported_call_conversions, na.rm = TRUE),
    web_conversions = sum(web_conversions, na.rm = TRUE),
    source_rows = n(),
    rows_missing_spend = sum(is.na(spend)),
    rows_missing_conversions = sum(is.na(conversions)),
    rows_missing_call_conversions = sum(is.na(reported_call_conversions)),
    rows_with_web_conversions = sum(!is.na(web_conversions)),
    .groups = "drop"
  )

monthly_performance <- report %>%
  group_by(month) %>%
  summarise(
    spend = sum(spend, na.rm = TRUE),
    conversions = sum(conversions, na.rm = TRUE),
    reported_call_conversions = sum(reported_call_conversions, na.rm = TRUE),
    source_rows = n(),
    rows_missing_spend = sum(is.na(spend)),
    rows_missing_conversions = sum(is.na(conversions)),
    .groups = "drop"
  ) %>%
  arrange(month)

service_line_month <- report %>%
  group_by(month, service_line) %>%
  summarise(
    spend = sum(spend, na.rm = TRUE),
    conversions = sum(conversions, na.rm = TRUE),
    reported_call_conversions = sum(reported_call_conversions, na.rm = TRUE),
    cost_per_reported_conversion = spend / na_if(conversions, 0),
    source_rows = n(),
    .groups = "drop"
  )

# The export has rows with NEWNOMENCLATURE TRUE, FALSE, and missing. Keep all
# three in the rollups and expose their metric coverage for review; do not assume
# they share a conversion definition until the Snowflake owner confirms it.
nomenclature_check <- report %>%
  group_by(nomenclature_status) %>%
  summarise(
    source_rows = n(),
    spend = sum(spend, na.rm = TRUE),
    conversions = sum(conversions, na.rm = TRUE),
    reported_call_conversions = sum(reported_call_conversions, na.rm = TRUE),
    rows_missing_spend = sum(is.na(spend)),
    rows_missing_call_conversions = sum(is.na(reported_call_conversions)),
    rows_with_web_conversions = sum(!is.na(web_conversions)),
    .groups = "drop"
  )

quality_checks <- tibble(
  check = c(
    "Rows in source file",
    "Rows in Jan-Oct reporting window",
    "Distinct source dates in reporting window",
    "Rows with missing spend in reporting window",
    "Rows with missing CONVERSIONS in reporting window",
    "Rows with missing CALL_CONVERSIONS in reporting window",
    "Rows with WEB_CONVERSIONS reported in reporting window"
  ),
  value = c(
    nrow(report_raw), nrow(report), n_distinct(report$report_date),
    sum(is.na(report$spend)), sum(is.na(report$conversions)),
    sum(is.na(report$reported_call_conversions)), sum(!is.na(report$web_conversions))
  )
)

#--------------------- Writing CSVs -------------------------------------------
write_csv(campaign_month, file.path(output_dir, "campaign_month_rollup.csv"))
write_csv(monthly_performance, file.path(output_dir, "monthly_performance.csv"))
write_csv(service_line_month, file.path(output_dir, "service_line_monthly_performance.csv"))
write_csv(nomenclature_check, file.path(output_dir, "nomenclature_metric_check.csv"))
write_csv(quality_checks, file.path(output_dir, "quality_checks.csv"))

# Monthly total conversions as bars with spend on a secondary axis. The chart
# uses CONVERSIONS as delivered; CALL_CONVERSIONS is not added because the
# Snowflake fields may overlap. When CTM arrives, compare its call counts and
# mapped outcomes with CALL_CONVERSIONS instead of treating them as additive.
conversion_max <- max(monthly_performance$conversions, na.rm = TRUE)
spend_max <- max(monthly_performance$spend, na.rm = TRUE)
spend_scale <- max(conversion_max, 1) / max(spend_max, 1)

####---------------------------------- Conversions/spend graph ----------------------------
conversion_spend_chart <- ggplot(monthly_performance, aes(x = month)) +
  geom_col(aes(y = conversions), fill = "#217CAA", width = 24) +
  geom_line(aes(y = spend * spend_scale), color = "#DB1217", linewidth = 1.1, group = 1) +
  geom_point(aes(y = spend * spend_scale), color = "#DB1217", size = 2) +
  scale_y_continuous(
    name = "Reported conversions",
    labels = label_comma(),
    sec.axis = sec_axis(~ . / spend_scale, name = "Spend ($)", labels = label_dollar())
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Memorial Health: reported conversions and spend",
    subtitle = "Bars = conversions; Line = spend",
    x = NULL,
    caption = "Source: Snowflake memorial_reporting_2026 export. CALL_CONVERSIONS is not added to CONVERSIONS."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.y.right = element_text(color = "#DB1217")
  ) 

conversion_spend_chart








####---------------------------------- bubble chart
paid_search_by_serviceline <- report %>%
  mutate(
    report_date = ymd(DATE),
    spend = parse_number(as.character(SPEND)),
    conversions = parse_number(as.character(CONVERSIONS)),
    call_conversions = parse_number(as.character(CALL_CONVERSIONS))
  ) %>%
  filter(
    CHANNEL == "Paid Search",
    report_date >= as.Date("2026-01-01"),
    report_date <= as.Date("2026-10-31"),
    !is.na(SERVICELINE)
  ) %>%
  group_by(service_line = SERVICELINE) %>%
  summarise(
    spend = sum(spend, na.rm = TRUE),
    conversions = sum(conversions, na.rm = TRUE),
    reported_calls = sum(call_conversions, na.rm = TRUE),
    rows_missing_call_data = sum(is.na(call_conversions)),
    .groups = "drop"
  ) %>%
  mutate(cost_per_conversion = spend / na_if(conversions, 0)) %>%
  filter(conversions > 0, spend > 0)

median_conversions <- median(paid_search_by_serviceline$conversions, na.rm = TRUE)
median_cost <- median(paid_search_by_serviceline$cost_per_conversion, na.rm = TRUE)

ggplot(paid_search_by_serviceline, aes(x = conversions, y = cost_per_conversion)) +
  geom_vline(xintercept = median_conversions,
             linetype = "dashed",
             color = "gray60",
             alpha = 0.8) +
  geom_hline(yintercept = median_conversions,
             linetype = "dashed",
             color = "gray60",
             alpha = 0.8) +
  geom_point(aes(size = reported_calls),
             color = unlock_colors["sky"],
             alpha = 0.9) +
  geom_text(aes(label = service_line),
            check_overlap = TRUE,
            vjust = -0.8,
            size = 3) +
  scale_x_continuous(
    name = "Reported conversions, Jan-Oct",
    labels = label_comma(accuracy = 1)
  ) +
  scale_y_log10(
    name = "Spend per reported conversion (log scale)",
    label = label_currency()
  ) +
  scale_size_area(
    name = "Reported call conversions",
    max_size = 14
  ) +
  labs(
    title = "Paid Search service-line performance",
    subtitle = "Bubble size shows reported call conversions; dashed lines mark service-line medians"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

names(report)

