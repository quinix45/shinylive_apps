library(shiny)
library(munsell)
library(ggplot2)
library(bslib)


information <- function(a, theta, df, sigma, b, d, q, lb, ub) {
  
  # Precompute reused terms
  sqrt_nu <- sqrt(df)
  exp_a_theta <- exp(a * theta)
  exp_2a_theta <- exp(2 * a * theta)
  
  # Define terms
  x1 <- b - lb + d * q
  x2 <- b + d * q - ub
  
  num1 <- exp_a_theta * x1
  denom1 <- exp_2a_theta * x1^2 + df * sigma^2
  
  num2 <- exp_a_theta * x2
  denom2 <- exp_2a_theta * x2^2 + df * sigma^2
  
  atan_term1 <- atan(num1 / (sqrt_nu * sigma))
  atan_term2 <- atan(num2 / (sqrt_nu * sigma))
  
  # Final expression
  prefactor <- a^2 * exp(-a * theta) * df * (1 + df) * sigma^2
  
  result <- prefactor * (
    -num1 / denom1 + 
      num2 / denom2 + 
      atan_term1 / (sqrt_nu * sigma) - 
      atan_term2 / (sqrt_nu * sigma)
  )
  
  return(result)
}


theme_set(theme_classic(base_size = 16, 
                        base_family = 'serif'))

# Define the UI 
ui <- bslib::page_fluid(
  # Main plot area
  
  mainPanel(
    
    fluidRow(column(12, plotOutput("distPlot", height = "370px", width = "150%")))),
  
  fluidRow(
    column(2, numericInput("b", "b", value = 0, min = -10, max = 10, step = 0.25)),
    column(2, numericInput("d", "d", value = 1, min = 0.1, max = 10, step = 0.25)),
    column(2, numericInput("sigma", "sigma", value = 1, min = 0.1, max = 5, step = 0.25)),
    column(2, numericInput("a", "a", value = 1, min = -3, max = 3, step = 0.25)),
    column(2, numericInput("q", "q", value = 0, min = -2, max = 2, step = 1)),
    column(2, numericInput("df", "df", value = 5, min = 0.1, max = 200, step = 1)))
)



server <- function(input, output, session) {
  
  output$distPlot <- renderPlot({
    # Get values from inputs
    
    
    
    # Create the plot
    ggplot() +
      geom_function(fun = information, args = list(a = input$a, 
                                                   df = input$df, 
                                                   sigma = input$sigma, 
                                                   b = input$b,
                                                   d = input$d, 
                                                   q = input$q, 
                                                   lb = -3, 
                                                   ub = 3), color = "#1b305c") +
      xlab("\u03b8") +
      ylab(expression(Epsilon~I*(theta))) +
      xlim(-6, 6)
    
    
    
    
  }, res = 100)
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

# shinylive::export(appdir = ".",
#                   destdir = "docs/")
