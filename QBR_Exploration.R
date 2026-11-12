install.packages("readr")
install.packages("tidyverse")
install.packages("tibble")
install.packages("dplyr")
install.packages("tidyr")
library(tidyr)
library(tibble)
library(readr)
library(tidyverse)
library(dplyr)
campaign_data <- read.csv("campaign_summary.csv", stringsAsFactors = FALSE)
secondary_data <- read.csv("secondary_tags.csv", stringsAsFactors = FALSE)

#inspect data
summary(campaign_data)
summary(secondary_data)
names(campaign_data)
names(secondary_data)
ncol(campaign_data)
ncol(secondary_data)
anyNA(campaign_data)
anyNA(secondary_data)

campaign_data <- as_tibble(campaign_data)

unlock_colors <- c(
  #Primary
  # Primary
  unlock_red    = "#DB1217",
  unlock_black  = "#161616",
  white         = "#FFFFFF",
  
  # Accents
  barn_red      = "#740316",
  salmon        = "#FAC2B9",
  ocean         = "#217CAA",
  sky           = "#7DC8EF",
  ice           = "#DFF0F8",
  
  # Neutrals
  dark_gray     = "#494343",
  medium_gray   = "#7D7577",
  light_gray    = "#D0D0D2",
  alabaster     = "#FCF0E6"
)

#exploratory charts
#top 5 campaigns by calls
top_5_campaigns <- campaign_data |>
  arrange(desc(campaign)) |>
  head(n = 5)

ggplot(top_5_campaigns, aes(x = campaign, y = total_calls, fill = campaign)) +
    geom_col(color = "black") +
    coord_flip() +
    labs(
      x = "Campaign",
      y = "Total Calls",
      title = "Total Calls by Campaign",
      subtitle = "Which campaigns had the most calls?"
    ) +
    theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

#qualified vs unqualified callers for top 5
campaign_call_counts <- campaign_data %>%
  select(campaign, qualified_count, disqualified_count) %>%
  pivot_longer(
    cols = c(qualified_count, disqualified_count),
    names_to = "call_status",
    values_to = "call_count"
  ) %>%
  mutate(
    call_status = recode(
      call_status,
      qualified_count = "Qualified",
      disqualified_count = "Disqualified"
    )
  )

top_5_calls <- campaign_call_counts %>%
  group_by(campaign) %>%
  mutate(campaign_total = sum(call_count, na.rm = TRUE)) %>%
  ungroup() %>%
  slice_max(campaign_total, n=10, with_ties = FALSE)

status_colors <- c(
  "Qualified" = unname(unlock_colors["ocean"]),
  "Disqualified" = unname(unlock_colors["barn_red"])
)

ggplot(top_5_calls, aes(x = campaign, y = call_count, fill = call_status)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_fill_manual(values = status_colors) +
  labs(
    x = "Campaign",
    y = "Call Count",
    title = "Qualified and Disqualified Calls by Campaign",
    fill = "Call Status"
  ) +
  theme_minimal()
unique(campaign_call_counts$call_status)


