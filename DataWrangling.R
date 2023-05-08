library(ggplot2)

suicide_df <-  read.csv("death-rate-from-suicides-gho.csv")
happiness_2019_df <- read.csv("2019.csv")
asian_df <- read.csv("AsiaPopulation2020.csv")


suicide_2019_df <- filter(suicide_df, str_detect(Year, "2019"))
combined_df <- left_join(happiness_2019_df, suicide_2019_df, by = c("Country.or.region" = "Entity"))

asian_df2 <- asian_df[,-2:-10]


combined_asian_df <- merge(combined_df, asian_df2, by.x = "Country.or.region", by.y = "Country")
combined_asian_df <- combined_asian_df[,-13]
combined_asian_df <- combined_asian_df[-c(11, 36), ]

happiness_average <- mean(happiness_2019_df$Score)

combined_asian_df$Higher.than.average <- ifelse(combined_asian_df$Score > happiness_average, TRUE, FALSE)

combined_asian_df$Happiness_to_Suicide_Ratio <- combined_asian_df$Score / combined_asian_df$Age.standardized.suicide.rate...Sex..both.sexes

ggplot(data = combined_asian_df, aes(x = Score, y = Age.standardized.suicide.rate...Sex..both.sexes)) + 
  geom_point(aes(col = Country.or.region))
getwd()
write.csv(combined_asian_df, "/Users/bennguyen/Downloads/Project/combined_asian_df.csv", row.names = FALSE)

summary_df <- aggregate(cbind(Score, GDP.per.capita, Social.support, 
                              Healthy.life.expectancy, Freedom.to.make.life.choices, 
                              Generosity, Perceptions.of.corruption, Age.standardized.suicide.rate...Sex..both.sexes) ~ Higher.than.average, 
                        data = combined_asian_df, 
                        FUN = function(x) c(mean = mean(x)))
count_table <- table(combined_asian_df$Higher.than.average)
summary_df$counts <- as.data.frame(count_table)$Freq
