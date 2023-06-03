library(dplyr)
#happiness <- read.csv(".csv")
suicide <- read.csv("death_rate_from_suicides_gho.csv")
combined_data <- left_join(happiness, suicide, by = c("school_ID" = "school.ID"))