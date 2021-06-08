#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(pwr) # for power calcs
#library(tidyr) # for data manipulation
#library(ggrepel)
library(ggplot2)
#library(cowplot)
library(tidyverse)
library(ggbeeswarm)
#library(shinyWidgets)
#library(shinydashboard)
library(flexdashboard)
library(thematic)
library(waiter)
library(bslib)

#thematic_shiny(font = "auto")

material <- bs_theme(
    bg = "white", 
    fg = "black", 
    primary = "red", 
    secondary = "blue",
    success = "#4F9B29",
    info = "#28B3ED",
    warning = "#FD7424",
    danger = "#F7367E",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Proza Libre"),
    code_font = font_google("Fira Code")
)



# Define UI for application that draws a histogram
ui <- fixedPage(
    use_waiter(), 
    waiter_show_on_load(html = spin_fading_circles()),
    
    theme = bslib::bs_theme(bootswatch = "flatly"),
    
    # Application title
    #titlePanel("Power Analysis"),

    # Sidebar with a slider input for number of bins 
    navbarPage("Power Analysis", collapsible = TRUE,
        tabPanel("Start", icon = icon("play"),
                 #tags$style(".fa {color:black}"),
                 fixedRow(
                     column(6,
                            includeMarkdown("start.Rmd")),
                     column(6,
                            plotOutput("corPlot"),
                            div(style="margin: auto; width: 80%",
                                sliderInput("corr", h4("Correlation:"),
                                            min = 0, max = 1,
                                            value = 0.5, step = 0.1, width = "100%"))
                     )
                 ),
        ),
        tabPanel("Effect Size", icon = icon("balance-scale"),
                 fixedRow(
                     column(width = 6,
                            includeMarkdown("effectT.md")
                     ),
                     column(width = 6,
                            div(style="margin: auto; width: 80%",
                                sliderInput("distmean", h4("Mean differences:"),
                                            min = 0, max = 60,
                                            value = 30, step = 10, 
                                            width = "100%")),
                            plotOutput("distPlot"),
                            div(style="margin: auto; width: 80%",
                                sliderInput("distsd", h4("Standard deviation:"),
                                            min = 10, max = 60,
                                            value = 30, step = 10, 
                                            width = "100%"))
                     )
                 )
        ),
        tabPanel("Power", icon = icon("plug"),
                 fixedRow(
                     column(width = 6,
                            includeMarkdown("power_text.md")
                     ),
                     column(width = 6,
                            div(style="margin: auto; width: 80%",
                                sliderInput("decimal2", h4("Effect size group 1:"),
                                            min = 0, max = 1,
                                            value = 0.2, step = 0.1, 
                                            width = "100%")),
                            plotOutput("powerPlot"),
                            div(style="margin: auto; width: 80%",
                                sliderInput("decimal", h4("Effect size group 2:"),
                                            min = 0, max = 1,
                                            value = 0.8, step = 0.1,
                                            width = "100%"))
                     )
                 )
        ),
        tabPanel("Estimate", icon = icon("robot"),
                 fixedRow(
                     column(width = 6,
                            includeMarkdown("estimate_text.md"),
                            verbatimTextOutput("model")
                     ),
                     column(width = 6,
                            div(style="margin: auto; width: 80%",
                                sliderInput("strength", h4("Effect (d):"),
                                            min = 0.1, max = 1,
                                            value = 0.5, step = 0.1, width = "100%")),
                            plotOutput("Nplot"),
                            div(style="margin: auto; width: 80%",
                                sliderInput("power", h4("Power:"),
                                            min = 0.1, max = 0.99,
                                            value = 0.8, step = 0.1, width = "100%"))
                     )
                 )
                        ),
        tabPanel("Low Power", icon = icon("battery-quarter"),
                 fixedRow(
                     column(width = 6,
                            includeMarkdown("lowpower_text.md")
                     ),
                     column(width = 6,
                            plotOutput("participants"),
                            div(style="margin: auto; width: 80%",
                                sliderInput("cases", h4("Number of observations:"),
                                            min = 50,
                                            max = 2000,
                                            value = 100, 
                                            width = "100%"))   
                            
                     )
                 )
                 )
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    Sys.sleep(3) # do something that takes time

    output$distPlot <- renderPlot({
        thematic_on()
        
        x <- 1 + rnorm(1000, 80, 1 + input$distsd)
        y <- 1 + rnorm(1000, 80 + input$distmean, 1 + input$distsd)
        z <- rep(c("A", "B"), each = 500)
        
        df <- data.frame(x, y , z)
        head(df)
        
        
        df2 <- df %>%
            pivot_longer(!z, names_to = "Group", values_to = "Outcome")
        
        
        ggplot(df2, aes(x = Outcome, color = Group, fill = Group))+
            geom_density(alpha=0.5)+
            theme_bw(base_size = 20)+
            theme(legend.position="bottom")
        
    })
    

    
    
    
    output$corPlot <- renderPlot({

        
        set.seed(6687)
        
        x <- 1 + rnorm(1000, 100, 500)
        df <- data.frame(x)
        
        df$random1<- runif(nrow(df),min=min(df$x),max=max(df$x))
        
        df$y <- df$x*input$corr+ df$random1*(1 - input$corr)
        
        ggplot(df, aes(x=x, y = y))+
            geom_jitter()+
            geom_smooth(method = lm, se = FALSE)+
            theme_bw(base_size = 20)+
            annotate(x=0, y=-1700, 
                     label=paste("R = ", round(cor(df$x, df$y),2)), 
                     geom="text", size=5)
        
    })
    
    
    
    output$powerPlot <- renderPlot({

        ptab <- cbind(NULL, NULL)       
        
        for (i in seq(4, 500)){
            pwrt1 <- pwr.r.test(r=input$decimal, sig.level=0.05, power=NULL, n=i, alternative = "two.sided")
            pwrt2 <- pwr.r.test(r=input$decimal2, sig.level=0.05, power=NULL, n=i, alternative = "two.sided")
            ptab <- rbind(ptab, cbind(pwrt1$power, pwrt1$n,
                                      pwrt2$power, pwrt2$n))
        }
        
        #Combine data and name it
        ptab <- cbind(seq_len(nrow(ptab)), ptab)
        
        colnames(ptab) <- c("id","r = 0,1.power","r = 0,1.n",
                            "r = 0,2.power","r = 0,2.n")
        
        # get data into right format for ggplot2
        temp <- ptab %>%
            as.data.frame() %>%
            gather(key = name, value = val, 2:5) %>%
            separate(col = name, into = c("group", "var"), sep = "\\.") %>%
            spread(key = var, value = val)
        
        temp$group <- factor(temp$group, 
                             levels = c("r = 0,1", "r = 0,2"))
        #Color Scheme
        cbPalette <- c("#C69472", "#8A8FA1", "#29303B", "#0057AD", "#C70C0B")
        
        # Plot it
        ggplot(temp, aes(x = `n`, y = power , color = group, linetype = group))+
            geom_hline(yintercept = 0.80, linetype = 1, size=1.2, color="darkgray")+
            geom_line(size=1) +
            theme_bw(base_size = 20) +
            geom_label(data = temp %>% filter(`n` == 50),
                       aes(label = paste0("d: ", c(input$decimal, input$decimal2)), fill=group),
                       #hjust = -1.0,
                       #vjust = 1.5,
                       size = 5,
                       color="white")+
            ylab("Power")+
            xlab("N")+
            theme(legend.position="none",
                  #legend.direction="horizontal",
                  legend.title = element_blank())
        
        
    })
    

    
    output$participants <- renderPlot({
        
        ptab <- cbind(NULL, NULL)       
        
        for (i in seq(4, input$cases)){
            pwrt1 <- pwr.r.test(r=0.1, sig.level=0.05, power=NULL, n=i, alternative = "two.sided")
            pwrt2 <- pwr.r.test(r=0.2, sig.level=0.05, power=NULL, n=i, alternative = "two.sided")
            pwrt3 <- pwr.r.test(r=0.3, sig.level=0.05, power=NULL, n=i, alternative = "two.sided")
            ptab <- rbind(ptab, cbind(pwrt1$power, pwrt1$n,
                                      pwrt2$power, pwrt2$n,
                                      pwrt3$power, pwrt3$n))
        }
        
        #Combine data and name it
        ptab <- cbind(seq_len(nrow(ptab)), ptab)
        
        colnames(ptab) <- c("id","d = 0,1.power","d = 0,1.n",
                            "d = 0,2.power","d = 0,2.n",
                            "d = 0,3.power","d = 0,3.n")
        
        # get data into right format for ggplot2
        temp <- ptab %>%
            as.data.frame() %>%
            gather(key = name, value = val, 2:7) %>%
            separate(col = name, into = c("group", "var"), sep = "\\.") %>%
            spread(key = var, value = val)
        
        temp$group <- factor(temp$group, 
                             levels = c("d = 0,1", "d = 0,2", "d = 0,3"))
        #Color Scheme
        cbPalette <- c("#C69472", "#8A8FA1", "#29303B", "#0057AD", "#C70C0B")
        
        # Plot it
        ggplot(temp, aes(x = `n`, y = power , color = group, linetype = group))+
            geom_hline(yintercept = 0.80, linetype = 1, color="darkgray", size=1.2)+
            geom_line(size=1) +
            theme_bw(base_size = 20) +
            geom_label(data = temp %>% filter(`n` == 50),
                       aes(label = group, fill=group),
                       #hjust = -1.0,
                       #vjust = 1.5,
                       size = 5,
                       color="white")+
            ylab("Power")+
            xlab("N")+
            #scale_color_manual(values=cbPalette)+
            #scale_fill_manual(values = cbPalette)+
            theme(legend.position="none",
                  #legend.direction="horizontal",
                  legend.title = element_blank())
        
    })
    
    output$Nplot <- renderPlot({

        
        df <- pwr.t.test(d=input$strength, sig.level=0.05, power=input$power, n=NULL,
                         type = "two.sample", alternative = "two.sided")
        
        n <- round(df$n)
        
        x <- "N"
        y <- runif(n)
        
        df2 <- data.frame(x, y, n)
        
        df2 %>%
            mutate(x = paste0(x, "\n", "n=", n))%>%
            ggplot(aes(x=x, y=y, color = y))+
            geom_quasirandom(size = 3)+
            theme_bw(base_size = 24)+
            theme(legend.position = "none")+
            xlab("")+
            ylab("")+
            theme(axis.title.y=element_blank(), 
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
        
    })
    
    output$model <- renderPrint({
        #pwr.r.test(r=input$strength, sig.level=0.05, power=input$power, n=NULL, 
                   #alternative = "two.sided")
                   
        pwr.t.test(d=input$strength, sig.level=0.05, power=input$power, n=NULL,
                                  type = "two.sample", alternative = "two.sided")
        

        
    })
    
    waiter_hide()
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
