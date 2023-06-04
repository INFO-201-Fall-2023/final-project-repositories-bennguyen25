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
data <- readr::read_csv(here("data/final_data.csv"))

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
           text7,
           text8
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
              <br><br>
              <span style='font-size:18px'> How does suicide rate impact Asian countries’ happiness scores? </span>")
              
text1 <- HTML("<H2> South Asia </H2>
              <br> <p> There are 1855836119 people located in, <font color='#A00042'>South Asia</font> they have a average happiness score of <font color='#A00042'>4.526857%, they have a suicide rate of 8.602357%.")



text2 <- HTML("<H2> East Asia </H2>
              <br> <p> There are 1620347712 people located in, <font color='#F56C42'>East Asia</font> they have a average happiness score of <font color='#F56C4'>5.564250%, they have a suicide rate of 14.524575%.")

text3 <- HTML("<H2> Southeast Asia </H2>
              <br> <p> There are 659588356 people located in, <font color='#008640'>East Asia</font> they have a average happiness score of <font color='#008640'>5.333375%, they have a suicide rate of 5.516250%.")


text4 <- HTML("<H2> Central Asia </H2>
              <br> <p> There are 74338950  people located in, <font color='#3487BD'>East Asia</font> they have a average happiness score of <font color='#3487BD'>5.5916%, they have a suicide rate of 9.200460%.")

text5 <- HTML("<H2> Western Asia/Middle east  </H2>
              <br> <p> There are 360697223 people located in, <font color='#C71C7E'>East Asia</font> they have a average happiness score of <font color='#C71C7E'>5.298%, they have a suicide rate of 4.438889%.")


text8 <- HTML("<H2> In Sum </H2>
              <br> <p>All things considered, the average happiness rate of an average Asian person is <b>111%</b>.
              <br>")

concludingtext <- HTML("<p><span style='font-size:24px'><b>The Risk of Automation</b></span>
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
                To learn more about how I made this app, please see the <a href='https://connorrothschild.github.io/r/automation-scrollytell/' target='_blank'>accompanying blog post</a>
                <br>
                Employment and education data comes from the
                <a href='https://www.bls.gov/emp/documentation/education-training-system.htm' target='_blank'>Bureau of Labor Statistics</a>. 
                <br>
                Employment and income data also comes from the <a href='https://www.bls.gov/oes/current/oes_nat.htm#11-0000' target='_blank'>BLS</a>.
                <br>
                Data on occupation and the risk of automation comes from <a href='https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf' target='_blank'>Frey and Osborne (2013)</a>. 
                <br>
                <br>
                Education is coded as typical education, meaning that the coded variable corresponds to the level of education that is most prevalent within a given occupation.
                If 51% of accountants hold a bachelor's degree, their typical education will be coded as such.
                Summary statistics for each level of education are calculated via the weighted mean of each occupation given its number of workers.
                <br>
                <br>
                For more information on the technical details of this analysis, please see the <a href='https://connorrothschild.github.io/r/automation/' target='_blank'>accompanying blog post</a>. 
                <br>
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
cols <- c('No formal educational credential' = '#A00042','High school diploma or equivalent' = '#F56C42',
          "Postsecondary nondegree award" = '#008640', "Associate's degree" = '#3487BD', 
          "Bachelor's degree" = '#C71C7E', "Master's degree" = '#5E4FA2',
          "Doctoral or professional degree" = '#1A1A1A') 

legend_ord <- levels(with(data, reorder(typicaled, reveal)))

## Intro plot
# Intro static ggplot
introggPlot <- data %>% 
  filter(typicaled != "Some college, no degree") %>%
  ggplot() +
  geom_point(mapping=aes(x=A_MEDIAN, y=probability, size=TOT_EMP,
                         alpha= 1/7, col=typicaled,
                         text = glue::glue('<span style = "font-size:1.5em">{occupation}</span><br>
                                                <i>Probability of Automation</i>: {probability}%
                                                <i>Median Income</i>: ${comma(A_MEDIAN, digits = 0)}
                                                <i>Number of Workers</i>: {comma(TOT_EMP, digits = 0)}'))) +
  scale_size(range = c(1, 20), guide = 'none') +
  xlab("\nMedian Income") +
  ylab("Probability of Automation") +
  # ggtitle("Likelihood of Job Automation vs Median Income") +
  labs(size= "", col= "", alpha = "") + 
  scale_color_manual(values = cols, breaks = legend_ord) +
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), limits = c(25000,200000)) +
  scale_y_continuous(labels=scales::number_format(suffix="%"), limits = c(0,100)) +
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
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)
