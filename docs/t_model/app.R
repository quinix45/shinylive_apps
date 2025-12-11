library(shiny)
library(munsell)
library(ggplot2)
library(bslib)

# Define the UI 
ui <- bslib::page_fluid(
  # Main plot area
  
  
  
  mainPanel(
    
    fluidRow(column(12, plotOutput("distPlot", height = "370px", width = "150%")))),
  
  fluidRow(
    column(2, numericInput("b", HTML("b<sub>i</sub>"), value = 0, min = -10, max = 10, step = 0.25)),
    column(2, numericInput("d", HTML("d<sub>i</sub>"), value = 1.5, min = 0.1, max = 10, step = 0.25)),
    column(2, numericInput("sigma", HTML("&sigma;<sub>i</sub>"), value = .5, min = 0, max = 5, step = 0.25)),
    column(2, numericInput("a", HTML("a<sub>i</sub>"), value = 1, min = 0, max = , step = 0.25)),
    column(2, numericInput("theta", HTML("&theta;<sub>j</sub>"), value = 0, min = -4, max = 4, step = 0.25)),
    column(2, numericInput("df", HTML("df<sub>i</sub>"), value = 20, min = 0.1, max = 200, step = 1))))


####### Plot function #######

library(extraDistr)

theme_set(theme_classic(base_size = 16, 
                        base_family = 'serif'))



Model_plot_full <- function( b = 0, d = 2, sd = 1, df = 8, theta = 0, a = 1, title = ""){
  
  Q <- c(-2, -1, 0, 1, 2)
  
  
  means <- c(b + Q[1]*d,
             b + Q[2]*d,
             b + Q[3]*d,
             b + Q[4]*d,
             b + Q[5]*d) 
  
  p <- ggplot() +
    geom_function(fun = dlst, args = list(mu = means[1], sigma = sd/exp(a*theta), df = df), color = "#1b305c") +
    geom_function(fun = dlst, args = list(mu = means[2], sigma = sd/exp(a*theta), df = df), color = "#1b305c") +
    geom_function(fun = dlst, args = list(mu = means[3], sigma = sd/exp(a*theta), df = df), color = "#1b305c") +
    geom_function(fun = dlst, args = list(mu = means[4], sigma = sd/exp(a*theta), df = df), color = "#1b305c") +
    geom_function(fun = dlst, args = list(mu = means[5], sigma = sd/exp(a*theta), df = df), color = "#1b305c") +
    geom_segment(aes(x = means[1], xend = means[1], y = 0, yend = dlst(means[1], mu = means[1], sigma = sd/exp(a*theta), df =df)), linetype = 2) +
    annotate("text", x= means[1], y = dlst(means[1], mu = means[1], sigma = sd/exp(a*theta), df =df) +.05, label="5%") +
    geom_segment(aes(x = means[2], xend = means[2], y = 0, yend = dlst(means[2], mu = means[2], sigma = sd/exp(a*theta), df =df)), linetype = 2) +
    annotate("text", x= means[2], y = dlst(means[2], mu = means[2], sigma = sd/exp(a*theta), df =df) +.05, label="25%") +
    geom_segment(aes(x = means[3], xend = means[3], y = 0, yend = dlst(means[3], mu = means[3], sigma = sd/exp(a*theta), df =df)), linetype = 2) +
    annotate("text", x= means[3], y = dlst(means[3], mu = means[3], sigma = sd/exp(a*theta), df =df) +.05, label="50%") +
    geom_segment(aes(x = means[4], xend = means[4], y = 0, yend = dlst(means[4], mu = means[4], sigma = sd/exp(a*theta), df =df)), linetype = 2) +
    annotate("text", x= means[4], y = dlst(means[4], mu = means[4], sigma = sd/exp(a*theta), df =df) +.05, label="75%") +
    geom_segment(aes(x = means[5], xend = means[5], y = 0, yend = dlst(means[5], mu = means[5], sigma = sd/exp(a*theta), df =df)), linetype = 2) +
    annotate("text", x= means[5], y = dlst(means[5], mu = means[5], sigma = sd/exp(a*theta), df =df) +.05, label="95%") +
    xlab("Expected Signed Error Distribution") +
    xlim(-7, 7) +
    ylab("") +
    ggtitle(title) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,dlst(means[3], mu = means[3], sigma = sd/exp(a*theta), df = df) +.1)) +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  
  print(p)
}


# Define the server function for the Shiny app
server <- function(input, output, session) {
  
  output$distPlot <- renderPlot({
    # Get values from inputs
    
    
    # Create the plot
    Model_plot_full(b = input$b,
                    d = input$d,
                    sd = input$sigma,
                    df = input$df,
                    theta = input$theta ,
                    a = input$a)
    
    
  }, res = 100)
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


# shinylive::export(appdir = ".",
#                   destdir = "docs/")
