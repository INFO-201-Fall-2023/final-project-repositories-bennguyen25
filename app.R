library(shiny)
library(scrollytell)
library(shinyjs)
library(ggvis)
library(plotly)
source("source_code_for_shiny-1.R")
theme_set(theme_minimal())

ui <- fluidPage(
  
  title = "Suicide Rates and Happiness in Asia",
  
  # suppress warning messages while data is loading on-screen
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$head(includeCSS("style.css")),
  
  # article title & name
  fluidRow(
    HTML(
      "<center>
                <h1>Suicide and Happiness in Asia</h1>
                <p style='font-size:26px'> by <a href='https://github.com/INFO-201-Fall-2023/final-project-repositories-bennguyen25' target='_blank'>Ben Nguyen and Weixuan Teng</a></p>
                </center>"
    )
  ),
  
  br(),
  
  fluidRow(column(1),
           
           column(
             10,
             # intro text
             fluidRow(id = 'text',
                      column(1),
                      column(
                        10,
                        br(),
                        text0,
                        hr(),
                        h1(
                          class = "instructions",
                          "How to read this chart:",
                          br(),
                          br(),
                          "The size of each",
                          icon("circle"),
                          "corresponds to population of that country.",
                          br(),
                          "Hover over each",
                          icon("circle"),
                          "to see details on the country.",
                          br(),
                          "Double click on a",
                          icon("circle"),
                          "in the legend to focus on a country."
                        )
                      ),
                      column(1)),
             # plot object for intro
             plotlyOutput("introPlot", height = '400px')
           ),
           
           column(1)),
  
  # scrollytelling plot
  scrolly_container(
    "scr"
    ,
    scrolly_graph(
      br(),
      br(),
      textOutput("section"),
      br(),
      HTML('<center>'),
      plotlyOutput("plot", height = '600px'),
      HTML('</center>')
      
    )
    ,
    scrolly_sections(
      HTML('<center>'),
      scrolly_section(id = 0, render_text(0)),
      scrolly_section(id = 1, render_text(1)),
      scrolly_section(id = 2, render_text(2)),
      scrolly_section(id = 3, render_text(3)),
      scrolly_section(id = 4, render_text(4)),
      scrolly_section(id = 5, render_text(5)),
      scrolly_section(id = 6, render_text(6)),
      scrolly_section(id = "buffer", br()),
      # add a scrolly_section with nothing in it;
      # this buffer prevents the plot from disappearing while reading last section
      scrolly_section(id = "buffer", br()),
      HTML('</center>')
    )
    
  ),
  
  # concluding text
  div(fluidRow(
    id = 'text',
    column(2),
    column(8,
           concludingtext,
           br()),
    column(2)
  ), style = 'margin-top: -300px;'),
  
  br(),
  br(),
  br(),
  hr(),
  
  fluidRow(column(1),
           column(10,
                  technicalnotes),
           column(1)),
  br(),
  br(),
  column(1)
  
)

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    add <- input$scr
    plot <- data %>%
      filter(if (add != 6) add >= reveal else reveal %in% c(1:6)) %>%
      ggplot() +
      geom_point(mapping=aes(x=Score, y=Age_standardized_suicide_rate_Sex_both_sexes, size=Population,
                             alpha=ifelse(add == reveal, 1/5, 1/10), col=Region,
                             text = glue::glue('<b>Country</b>: {Country}
                                            <b>Suicide Rate</b>: {Age_standardized_suicide_rate_Sex_both_sexes}
                                            <b>Happiness Score</b>: {comma(Score, digits = 2)}
                                            <b>Region</b>: {Region}
                                            <b>GDP factor</b>: {GDP.per.capita}
                                            <b>Population</b>: {comma(Population, digits = 0)}'))) +
      scale_size(range = c(1, 20)) +
      xlab("\nHappiness Score") +
      ylab("Suicide Rate") +
      labs(size = "", col = "", alpha = "") +
      scale_color_manual(values = cols) +
      scale_x_continuous(limits = c(3, 8)) +
      scale_y_continuous(limits = c(0,100)) +
      theme(axis.line.x = ggplot2::element_line(colour = NULL,
                                                size = NULL, linetype = NULL, lineend = NULL),
            axis.line.y = ggplot2::element_blank(),
            panel.grid.major.x = element_blank())
    
    data <- data %>%
      mutate(reveal = case_when(
        Region == "South Asia" ~ 1,
        Region == "East Asia" ~ 2,
        Region == "Southeast Asia" ~ 3,
        Region == "Central Asia" ~ 0,
        Region == "Middle East" ~ 4,
      ))
    
    ggplotly(plot, tooltip = 'text') %>%
      layout(
        title = list(element_blank()),
        legend = list(x = 0.65, y = 0.925),
        font = list(family = 'Lato'),
        margin = list(t = 50),
        hoverlabel = list(bgcolor = 'whitesmoke', color = 'darkGray')
      ) %>%
      config(
        displaylogo = F,
        showSendToCloud = F,
        displayModeBar = F
      )
    
  })
  
  output$introPlot <- renderPlotly({
    introPlot
  })
  output$scr <- renderScrollytell({
    scrollytell()
  })
  renderText(paste0("Section: ", input$scr))
  observe({
    cat("section:", input$scr, "\n")
  })
  
}

shinyApp(ui = ui, server = server)