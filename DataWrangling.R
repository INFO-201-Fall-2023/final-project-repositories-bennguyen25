library(shiny)
library(scrollytell)
library(shinyjs)
library(ggvis)
library(plotly)

theme_set(theme_minimal())

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
  geom_point(aes(col = Region)) +
  geom_text(aes(label = Country.or.region))
getwd()
write.csv(combined_asian_df, "/Users/bennguyen/Downloads/Project/combined_asian_df.csv", row.names = FALSE)

summary_df <- aggregate(cbind(Score, GDP.per.capita, Social.support, 
                              Healthy.life.expectancy, Freedom.to.make.life.choices, 
                              Generosity, Perceptions.of.corruption, Age.standardized.suicide.rate...Sex..both.sexes) ~ Higher.than.average, 
                        data = combined_asian_df, 
                        FUN = function(x) c(mean = mean(x)))
count_table <- table(combined_asian_df$Higher.than.average)
summary_df$counts <- as.data.frame(count_table)$Freq

get_region <- function(country) {
  if (country %in% c("China", "Japan", "Mongolia", "South Korea")) {
    return("East Asia")
  } else if (country %in% c("Afghanistan", "Bangladesh", "Bhutan", "India", "Nepal", "Pakistan", "Sri Lanka")) {
    return("South Asia")
  } else if (country %in% c("Cambodia", "Indonesia", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam")) {
    return("Southeast Asia")
  } else if (country %in% c("Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan")) {
    return("Central Asia")
  } else {
    return("Middle East")
  }
}

combined_asian_df <- mutate(combined_asian_df, Region = sapply(combined_asian_df$Country.or.region, get_region))

print(combined_asian_df)

combined_asian_df <- rename(combined_asian_df, Country = Country.or.region)

population_df <- select(AsiaPopulation2020, Country, Population)
combined_asian_df <- left_join(combined_asian_df, population_df, by = "Country")


south_asia_population <- sum(combined_asian_df$Population[combined_asian_df$Region == "South Asia"])

print(south_asia_population)

regions <- c("East Asia", "South Asia", "Southeast Asia", "Central Asia", "Middle East")

population_sum_df <- data.frame(Region = regions, Population_Sum = numeric(length(regions)),
                                Score = numeric(length(regions)),
                                Suicide_Rate = numeric(length(regions)))

for (i in 1:length(regions)) {
  region_population <- sum(combined_asian_df$Population[combined_asian_df$Region == regions[i]])
  region_score <- mean(combined_asian_df$Score[combined_asian_df$Region == regions[i]])
  region_suicide_rate <- mean(combined_asian_df$Age.standardized.suicide.rate...Sex..both.sexes[combined_asian_df$Region == regions[i]])
  
  population_sum_df$Population_Sum[i] <- region_population
  population_sum_df$Score[i] <- region_score
  population_sum_df$Suicide_Rate[i] <- region_suicide_rate
}


