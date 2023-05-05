suicide_df <-  read.csv("death-rate-from-suicides-gho.csv")
happiness_2019_df <- read.csv("2019.csv")
asian_df <- read.csv("AsiaPopulation2020.csv")


suicide_2019_df <- filter(suicide_df, str_detect(Year, "2019"))
combined_df <- left_join(happiness_2019_df, suicide_2019_df, by = c("Country.or.region" = "Entity"))

asian_df2 <- asian_df[,-2:-10]


combined_asian_df <- merge(combined_df, asian_df2, by.x = "Country.or.region", by.y = "Country")
combined_asian_df <- combined_asian_df[,-13]

happiness_average <- mean(happiness_2019_df$Score)

combined_asian_df$Higher.than.average <- NA

if (combined_asian_df$Score > happiness_average) {
  combined_asian_df$Higher.than.average == TRUE
} else {
  combined_asian_df$Higher.than.average == FALSE
}