# ---- Libraries ----
library(shiny)
library(pwr)
library(ggplot2)
library(tidyverse)
library(ggbeeswarm)
library(flexdashboard)
library(thematic)
library(waiter)
library(bslib)



# ---- UI ----
ui <- fixedPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  navbarPage("Power Analysis", collapsible = TRUE,
             
             # Start Tab
             tabPanel("Start", icon = icon("play"),
                      fixedRow(
                        column(6, includeMarkdown("txt/start.md")),
                        column(6,
                               plotOutput("corPlot"),
                               div(style = "margin: auto; width: 80%",
                                   sliderInput("corr", h4("Correlation:"), min = 0, max = 1, value = 0.5, step = 0.1, width = "100%"))
                        )
                      )
             ),
             
             # Effect Size Tab
             tabPanel("Effect Size", icon = icon("balance-scale"),
                      fixedRow(
                        column(6, includeMarkdown("txt/effectT.md")),
                        column(6,
                               div(style = "margin: auto; width: 80%",
                                   sliderInput("distmean", h4("Mean differences:"), min = 0, max = 60, value = 30, step = 10, width = "100%")),
                               plotOutput("distPlot"),
                               div(style = "margin: auto; width: 80%",
                                   sliderInput("distsd", h4("Standard deviation:"), min = 10, max = 60, value = 30, step = 10, width = "100%"))
                        )
                      )
             ),
             
             # Power Tab
             tabPanel("Power", icon = icon("plug"),
                      fixedRow(
                        column(6, includeMarkdown("txt/power_text.md")),
                        column(6,
                               div(style = "margin: auto; width: 80%",
                                   sliderInput("decimal2", h4("Effect size group 1:"), min = 0, max = 1, value = 0.2, step = 0.1, width = "100%")),
                               plotOutput("powerPlot"),
                               div(style = "margin: auto; width: 80%",
                                   sliderInput("decimal", h4("Effect size group 2:"), min = 0, max = 1, value = 0.8, step = 0.1, width = "100%"))
                        )
                      )
             ),
             
             # Estimate Tab
             tabPanel("Estimate", icon = icon("robot"),
                      fixedRow(
                        column(6,
                               includeMarkdown("txt/estimate_text.md"),
                               verbatimTextOutput("model")
                        ),
                        column(6,
                               div(style = "margin: auto; width: 80%",
                                   sliderInput("strength", h4("Effect (d):"), min = 0.1, max = 1, value = 0.5, step = 0.1, width = "100%")),
                               plotOutput("Nplot"),
                               div(style = "margin: auto; width: 80%",
                                   sliderInput("power", h4("Power:"), min = 0.1, max = 0.99, value = 0.8, step = 0.1, width = "100%"))
                        )
                      )
             ),
             
             # Low Power Tab
             tabPanel("Low Power", icon = icon("battery-quarter"),
                      fixedRow(
                        column(6, includeMarkdown("txt/lowpower_text.md")),
                        column(6,
                               plotOutput("participants"),
                               div(style = "margin: auto; width: 80%",
                                   sliderInput("cases", h4("Number of observations:"), min = 50, max = 2000, value = 100, width = "100%"))
                        )
                      )
             )
  )
)

# ---- Server ----
server <- function(input, output) {

  output$distPlot <- renderPlot({
    make_dist_plot(input$distmean, input$distsd)
  })
  
  output$corPlot <- renderPlot({
    make_cor_plot(input$corr)
  })
  
  output$powerPlot <- renderPlot({
    make_power_plot(input$decimal, input$decimal2)
  })
  
  output$participants <- renderPlot({
    make_low_power_plot(input$cases)
  })
  
  output$Nplot <- renderPlot({
    make_n_plot(input$strength, input$power)
  })
  
  output$model <- renderPrint({
    estimate_sample_size(input$strength, input$power)
  })
  
}

# ---- Run App ----
shinyApp(ui = ui, server = server)
