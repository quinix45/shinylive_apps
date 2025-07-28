library(shiny)
library(munsell)
library(ggplot2)
library(bslib)

NLL <- function(df, a = 1, theta = 0, b = 0, d = 1, q = 0, Y =.5, sigma = 2) {
  term1 <- 0.5 * df * log(df)
  term2 <- 0.5 * (-1 - df) * log(df + (exp(2 * a * theta) * ( -b - d * q + Y)^2) / sigma^2)
  term3 <- log(exp(-a * theta) * sigma)
  term4 <- log(beta(df / 2, 1 / 2))
  
  result <- term1 + term2 - term3 - term4
  
  # return negative LL
  return(-result)
}

find_min_theta <- function(a = 1, df = 3, b = 0, d = 1, q = 0, Y = 0.5, sigma = 2) {
  # We restrict df > 0 to avoid domain issues
  opt <- optimize(
    f = NLL,
    interval = c(-20, 20), 
    a = a, df = df, b = b, d = d, q = q, Y = Y, sigma = sigma,
    tol = 1e-8
  )
  
  return(list(
    theta_min = opt$minimum,
    NLL_min = opt$objective
  ))
}


theme_set(theme_classic(base_size = 16, 
                        base_family = 'serif'))

# Define the UI 
ui <- bslib::page_fluid(
  # Main plot area
  
  mainPanel(
    
    fluidRow(column(12, plotOutput("distPlot", height = "370px", width = "150%")))),
  
  fluidRow(
    column(1, numericInput("Y", "Y", value = .5, min = -20, max = 20, step = .2)),
    column(1, numericInput("b", "b", value = 0, min = -10, max = 10, step = 0.25)),
    column(1, numericInput("d", "d", value = 1, min = 0.1, max = 10, step = 0.25)),
    column(1, numericInput("sigma", "sigma", value = 1, min = 0.1, max = 8, step = 0.25)),
    column(1, numericInput("a", "a", value = 1, min = .1, max = 4, step = 0.25)),
    column(1, numericInput("q", "q", value = 0, min = -2, max = 2, step = 1)),
    column(1, numericInput("df", "df", value = 3, min = 0.1, max = 200, step = 1)))
)



server <- function(input, output, session) {
  
  
  
  output$distPlot <- renderPlot({
    # Get values from inputs
    
    
    if(input$Y == input$b + input$b*input$q){
      
      lab_plot <- paste("theta[MLE] == infinity")
      
      min_theta <- c(-30, 30)
      
    }else{
      
      min_theta <- find_min_theta(a = input$a, 
                                  Y = input$Y,
                                  df = input$df, 
                                  sigma = input$sigma, 
                                  b = input$b,
                                  d = input$d, 
                                  q = input$q)
      
      lab_plot <-  paste("theta[MLE] == ", round(unlist(min_theta[1]), 2))
    }
    
    
    # Create the plot
    ggplot() +
      geom_function(fun = NLL, args = list(a = input$a, 
                                           Y = input$Y,
                                           df = input$df, 
                                           sigma = input$sigma, 
                                           b = input$b,
                                           d = input$d, 
                                           q = input$q), 
                    color = "#1b305c")+
      geom_point(aes(x = unlist(min_theta[1]),
                     y = unlist(min_theta[2])),
                 col = "red",
                 size = 2) +
      annotate("text", 
               x = -Inf, y = Inf, 
               label = lab_plot,
               parse = TRUE,
               hjust = -.3, vjust = 1, 
               size = 5) +
      xlab("\u03b8") +
      ylab(expression(NLL*(theta))) +
      xlim(-6, 6)
    
    
    
  }, res = 100)
}


# Run the Shiny app
shinyApp(ui = ui, server = server)
# 
# 
# shinylive::export(appdir = ".",
#                  destdir = "docs/")

