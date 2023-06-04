library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggrepel)
library(tools)
library(readxl)
library(tidyverse)
library(knitr)
library(shiny)
library(shinyjs)
library(ggvis)
library(plotly)
library(metathis)
library(formattable)
# devtools::install_github("statistiekcbs/scrollytell")
library(scrollytell)
library(here)

options(scipen=999)
theme_set(theme_minimal())
data <- read.csv("combined_asian_df.csv")

### FUNCTIONS & TEXT

longdiv <- function(...){
  div(
    ...,
    class = "container",
    style = "height:100vh"
  )
}

render_text <- function(num){
  
  div(
    text(num), class = "text"
  )
  
}

text <- function(num){
  p(
    switch(num,
           text1,
           text2,
           text3,
           text4,
           text5,
           text6,
    )
  )
}

text0 <- HTML("<span style='font-size:20px'> How does suicide rate impact Asian countries’ happiness scores? </span>
              <br><br> 
              <p> Welcome to our interactive scrollytelling project: How does Suicide Rate Impact Countries' Happiness Scores? 
              This immersive exploration dives into the complex relationship between suicide rates and the overall happiness of nations, with a particular focus on Asia.
              <br> Asia's vibrant tapestry of cultures, religions, and socioeconomic frameworks offers an insightful perspective on global mental health and wellbeing. 
              It's a continent where age-old traditions intersect with rapid modernization, creating a unique amalgam that shapes its societies. In this project, we took a dive into how these facets impact the happiness of its nations.
              <br><br> Suicide rates, a stark manifestation of mental health struggles, can pose a significant blow to national happiness. 
              Our project elucidates this link and addresses the broader implications, striving to promote understanding, encourage discourse, and drive change.
              <br>Through our scrollytelling, we had separated all the Asian countries into different groups by their location. 
              You can clearly see the population of each country by looking at the size of each dot, also each region that the countries belonged to by looking at the colors. 
              <br>We invite you to join us on this interactive exploration. Unravel the complexities, appreciate the nuances, and contribute to the conversation that could pave the way for happier, healthier societies across Asia.
              <br>
              For some context, Suicide Rate in this case represents suicides per 100,000 people and the Happiness Score is based on a variety of factors, but we will focus more on GDP. This data also focuses on just the Asian continent in the year 2019, which is the most recent data found.
              <br>
              <span style='font-size:18px'> How does suicide rate impact Asian countries’ happiness scores? </span>")
              
text1 <- HTML("<H2> South Asia </H2>
              <br> <p> There are 1,855,836,119 people located in <font color='#A00042'>South Asia</font>, they have an average happiness score of <font color='#A00042'>4.53</font> and have a suicide rate of <font color='#A00042'>8.6</font>.
              The South Asia region is actually the unhappiest region of Asia. GDP also is not a big factor in their region as opposed to other regions. This suggests that countries are not doing as well
              financially. Sri Lanka, the country that has the highest GDP factor, actually has the highest suicide rate in the region.")



text2 <- HTML("<H2> East Asia </H2>
              <br> <p> There are 1,620,347,712 people located in <font color='#F56C42'>East Asia</font>, they have an average happiness score of <font color='#F56C4'>5.56</font> and have a suicide rate of <font color='#F56C4'>14.52</font>.
              It is interesting to note in this case, that despite South Korea having the highest Happiness Score of East Asia, they also have the highest suicide rate. GDP plays a big factor in South Korea's happiness index, which conversly
              could mean that there may be wealth inequality aswell, which may explain their high suicide rate. East Asia is an interesting region since they have high societal pressures and cultural norms that influence their societies greatly.")

text3 <- HTML("<H2> Southeast Asia </H2>
              <br> <p> There are 659,588,356 people located in <font color='#008640'>Southeast Asia</font>, they have an average happiness score of <font color='#008640'>5.33</font> and have a suicide rate of <font color='#008640'>5.52</font>.
              However, amidst these statistics, one notable outlier emerges—Singapore. With a remarkable GDP factor of 1.572, Singapore has the highest happiness score and suicide rate among its neighboring countries. This difference in economic 
              prosperity, coupled with its impact on the well-being of its citizens, sets Singapore apart as a unique case within the region. The country's strong GDP factor reflects its robust economy and affluent lifestyle, which can contribute to 
              overall well-being. However, the comparison of high happiness scores and suicide rates suggests the presence of underlying complexities related to societal pressures, mental health, and the impact of rapid modernization, making 
              Singapore a fascinating case study in the region.")


text4 <- HTML("<H2> Central Asia </H2>
              <br> <p> There are 74,338,950  people located in <font color='#3487BD'>Central Asia</font>, they have an average happiness score of <font color='#3487BD'>5.59</font> and have a suicide rate of <font color='#3487BD'>9.2</font>.
              It's interesting to note that, in contrast to other regions, happiness levels in Central Asia do not appear to be strongly influenced by GDP. But Kazakhstan stands out as an anomaly and offers a distinctive situation. Kazakhstan 
              has the unlucky distinction of having the highest suicide rate among the region at 18 per 100,000 people, which is twice as high as the rate seen in Uzbekistan, who has the second-highest GDP factor. This disparity suggests that economic 
              disparities within Central Asia may have an impact on their happiness scores, raising the possibility that wealth inequality is a contributing factor.")

text5 <- HTML("<H2> Western Asia/Middle east  </H2>
              <br> <p> There are 360,697,223 people located in <font color='#C71C7E'>Western Asia/Middle East</font>, they have an average happiness score of <font color='#C71C7E'>5.3</font> and have a suicide rate of <font color='#C71C7E'>4.44</font>.
              Notably, this region exhibits the lowest suicide rate among the continents, with just half of Central Asia and South Asia's rates and a mere third of East Asia's rates. Furthermore, within Western Asia/Middle East, Israel emerges as the 
              happiest country. Interestingly, despite Israel's high GDP factor, Israel's high happiness levels can be attributed to strong social cohesion, a rich cultural heritage, and investments in education and healthcare. 
              These factors, along with a sense of community, personal growth opportunities, and the ability to find joy in everyday life, contribute to Israel's reputation as a happy country")


text6 <- HTML("<H2> In Summary </H2>
              <br> <p>Asia is a diverse continent with many different regions. In all five of Asia's regions, a notable pattern can be seen: in four of the five, the region with the highest or nearly the highest GDP factor also has the highest suicide rate. 
              This correlation points to a complex relationship between economic success and happiness. Economic growth can result in material wealth, but it can also lead to societal pressures, inequality, and the breakdown of social support 
              networks—all of which have a negative impact on mental health and drive suicide rates up. This emphasizes how crucial it is to focus on social and mental well-being in addition to economic growth in order to ensure overall development and reduce the potentially harmful 
              effects of wealth disparities.</b>
              <br>")

concludingtext <- HTML("<p><span style='font-size:24px'><b>Suicide and Happiness in Asia</b></span>
                        <br>
                            <span style='font-size:18px'>Thank you for scrolling through the data that we analyzed. Navigating through the complexities of Asia's mental health landscape has offered us profound insights. 
                            Despite the rich cultural diversity and economic progress, Asia grapples with significant mental health challenges. High suicide rates, particularly in East and South Asia, underscore an urgent need for addressing mental health issues. 
                            Simultaneously, the lower happiness scores in these regions highlight the urgent call for positive psychology and wellbeing interventions.
                        <br>
                            <br>In contrast, the Middle East, Southeast Asia, and Central Asia, while having somewhat comparable happiness scores, demonstrate the varying extents of suicide rates. 
                            This discrepancy suggests the diverse mental health challenges in different socio-cultural contexts and the different stressors impacting the individuals' wellbeing in these regions.
                        <br>
                            <br> Conclusively, Asia's mental health crisis is not a monolithic issue. It's a complex puzzle shaped by a range of factors - cultural stigma, inadequate healthcare infrastructure, socioeconomic disparities, and rapid societal changes, among others. 
                       But acknowledging this crisis is our first step towards a collective solution. Our hope is that this exploration instigates dialogue, awareness, and action for better mental health outcomes across Asia.")

technicalnotes <- HTML("<p>
                <span style='font-size:18px'><i>Technical Notes</i></span><br>
                <br>
                <span style='font-size:12px'>
                Suicide rate per 100,000 people data comes from
                <a href='https://www.who.int/data/gho/indicator-metadata-registry/imr-details/4664' target='_blank'>The World Health Organization</a>. 
                <br>
                Asia population data from 2019 comes from <a href='https://www.kaggle.com/datasets/sansuthi/asian-countries-by-population' target='_blank'>Kaggle</a>.
                <br>
                Data on suicide rates for 2019 come from <a href='https://www.kaggle.com/datasets/unsdsn/world-happiness' target='_blank'>SUSTAINABLE DEVELOPMENT SOLUTIONS NETWORK(2019)</a>. 
                <br>
                It is important to note that GDP per capita in this case is not the actual GDP per capita of the country, but rather how much of a factor it plays into the country's happiness score
                <br>
                The R packages powering this site include 
                <a href='https://www.tidyverse.org/' target='_blank'>tidyverse</a>,
                <a href='http://shiny.rstudio.com/' target='_blank'>shiny</a>,
                <a href='https://github.com/ropensci/plotly' target='_blank'>plotly</a>, and 
                <a href='https://github.com/statistiekcbs/scrollytell' target='_blank'>scrollytell</a>.
                </span>
                </p>")


### ALL PLOT OBJECTS

# helpers for all plots:
cols <- c('South Asia' = '#A00042','East Asia' = '#F56C42',
          "Southeast Asia" = '#008640', "Central Asia" = '#3487BD', 
          "Western Asia/Middle East" = '#C71C7E') 

## Intro plot
# Intro static ggplot
introggPlot <- data %>% 
  ggplot() +
  geom_point(mapping=aes(x=Score, y=Age_standardized_suicide_rate_Sex_both_sexes, size=Population,
                         alpha= 1/7, col=Region,
                         text = glue::glue('<span style = "font-size:1.5em">{Country}</span><br>
                                                <i>Suicide rate </i>: {Age_standardized_suicide_rate_Sex_both_sexes}
                                                <i>Happiness Score</i>: {comma(Score, digits = 2)}
                                                <i>Region</i>: {Region}
                                                <i>GDP Factor</i>: {GDP.per.capita}
                                                <i>Population</i>: {comma(Population, digits = 0)}'))) +
  scale_size(range = c(1, 20), guide = 'none') +
  xlab("\nHappiness Score") +
  ylab("Suicide rate") +
  # ggtitle("Suicide Rates vs. Happiness Scores in Asia") +
  labs(size= "", col= "", alpha = "") + 
  scale_color_manual(values = cols) +
  scale_x_continuous(limits = c(3, 8)) +
  scale_y_continuous(limits = c(0,100)) +
  # theme(legend.position = "top", legend.direction = "horizontal") +
  # legend.text = element_text(colour = ifelse(add == reveal, "black", "grey"))) +
  # legend.text = element_text(colour="black", size = ifelse(add == reveal, 20, 12))) +
  # cr::drop_axis(axis = "y")
  theme(axis.line.x = ggplot2::element_line(colour = NULL, 
                                            size = NULL, linetype = NULL, lineend = NULL), 
        axis.line.y = ggplot2::element_blank(),
        panel.grid.major.x = element_blank()) 

# Convert into ggplotly
introPlot <- ggplotly(introggPlot, tooltip = 'text') %>%
  layout(
    title = element_blank(),
    legend = list(x = 0.65, y = 0.925),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = FALSE, showSendToCloud = FALSE, displayModeBar = FALSE)
