install.packages("readr")
install.packages("tidyverse")
install.packages("tibble")
library(tibble)
library(readr)
library(tidyverse)
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

#exploratory charts
top_5_campaigns_ <- campaign_data |>
  order(desc) |>
  head(n = 5)

  ggplot(campaign_data, aes(x = campaign, y = total_calls)) +
    geom_col() +
    coord_flip() +
    labs(
      x = "Campaign",
      y = "Total Calls",
      title = "Total Calls by Campaign",
      subtitle = "Which campaigns had the most calls?"
    ) +
    theme_minimal()
  

