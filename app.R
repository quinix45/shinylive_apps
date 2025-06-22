library(shiny)
library(munsell)
library(ggplot2)
library(bslib)

theme_set(theme_classic(base_size = 16, 
                        base_family = 'serif'))

ui <- bslib::page_fluid(
  # Main plot area
  
  mainPanel(
    
    fluidRow(column(12, plotOutput("Sampling_Distribution")))),
  
  fluidRow(
    column(2, numericInput("Mean", "\u03BC", value = 10, step = 1)),
    column(2, numericInput("SD", "\u03C3", value = 3, min = 0.1, max = 10, step = 0.25)),
    column(2, numericInput("N", "N", value = 25, min = 2, step = 1)),
    column(2, numericInput("SM", "X\u0304", value = 9, step = .1)),
    column(2,  selectInput("tail", "Tail",
                           c("Lower" = "low",
                             "Upper" = "up")))
  ))



server <- function(input, output, session) {
  
  output$Sampling_Distribution <- renderPlot({
    # Get values from inputs
    
    
    if(input$tail == "low"){
      # calculate p-value
      
      p_val <- pnorm(input$SM, mean = input$Mean, sd = input$SD/sqrt(input$N))
      
      p_val <- ifelse(p_val < .001, "p < .001", paste("p =", round(p_val, 3)))
      
      # Create the plot
      ggplot() +
        xlim(input$Mean-3*(input$SD)/sqrt(input$N), input$Mean + 3*(input$SD)/sqrt(input$N))+
        geom_function(fun = dnorm, args = list(mean = input$Mean,
                                               sd = input$SD/sqrt(input$N)),
                      color = "#1b305c") +
        xlab("Sampling Distribution") +
        geom_segment(aes(x = input$SM,
                         xend = input$SM,
                         y = 0,
                         yend = dnorm(input$SM, mean = input$Mean, sd = input$SD/sqrt(input$N))),
                     lty = 2) +
        geom_ribbon(data = data.frame(x = seq(input$SM, input$SM - (abs(input$SM)*input$SD), length.out = 10000)), 
                    aes(x = x, ymin = 0, ymax = dnorm(x, mean = input$Mean, sd = input$SD/sqrt(input$N))), 
                    fill = "#1b305c", alpha = 0.5) +
        
        annotate("text", 
                 x = Inf,
                 y = Inf,
                 label = p_val, hjust=1,vjust = 1) +
        scale_y_continuous(expand = c(0,0)) + 
        theme( axis.text.y=element_blank(),
               axis.ticks.y=element_blank(),
               axis.line.y = element_blank(),
               axis.title.y = element_blank())
      
    }else{
      
      
      p_val <- 1 - pnorm(input$SM, mean = input$Mean, sd = input$SD/sqrt(input$N))
      
      p_val <- ifelse(p_val < .001, "p < .001", paste("p =", round(p_val, 3)))
      
      # Create the plot
      ggplot() +
        xlim(input$Mean-3*(input$SD)/sqrt(input$N), input$Mean + 3*(input$SD)/sqrt(input$N))+
        geom_function(fun = dnorm, args = list(mean = input$Mean,
                                               sd = input$SD/sqrt(input$N)),
                      color = "#1b305c") +
        xlab("Sampling Distribution") +
        geom_segment(aes(x = input$SM,
                         xend = input$SM,
                         y = 0,
                         yend = dnorm(input$SM, mean = input$Mean, sd = input$SD/sqrt(input$N))),
                     lty = 2) +
        geom_ribbon(data = data.frame(x = seq(input$SM, input$SM + (abs(input$SM)*input$SD), length.out = 10000)), 
                    aes(x = x, ymin = 0, ymax = dnorm(x, mean = input$Mean, sd = input$SD/sqrt(input$N))), 
                    fill = "#1b305c", alpha = 0.5) +
        
        annotate("text", 
                 x = Inf,
                 y = Inf,
                 label = p_val, hjust=1,vjust = 1) +
        scale_y_continuous(expand = c(0,0)) + 
        theme( axis.text.y=element_blank(),
               axis.ticks.y=element_blank(),
               axis.line.y = element_blank(),
               axis.title.y = element_blank())
      
      
    }
    
    
    
  }, res = 100)
}


# Run the Shiny app
shinyApp(ui = ui, server = server)


# shinylive::export(appdir = ".",
#                   destdir = "docs/")
