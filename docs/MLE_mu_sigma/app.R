library(shiny)
library(munsell)
library(bslib)
library(Deriv)
library(plotly)
library(curl)

ui <- bslib::page_fluid(
  # Main plot area
  
  mainPanel(
    
    fluidRow(
      column(2, "Generate data:",
             numericInput("Mean", "\u03BC", value = 8, step = 1),
             numericInput("SD", "\u03C3", value = 10, min = 0.1, step = 0.25),
             numericInput("N", "N", value = 200, min = 2, step = 20)),
      
      column(10,  bslib::card(plotlyOutput(outputId = "NLL_plot"), full_screen = TRUE),
             "Note: Plane is slightly off near MLEs due to rounding needed for fast rendering of visualization."))
  ))



server <- function(input, output, session) {
  
  
  output$NLL_plot <- renderPlotly({
    nll_one <- deriv(~ -2 * (-0.5 * log(2 * pi * sigma^2) - ((x - mu)^2) / (2 * sigma^2)),
                     c("mu", "sigma"), function.arg = TRUE, hessian = TRUE)
    
    nll <- function(b) {
      v <- nll_one(b[1], b[2])
      f <- sum(v)
      gr <- colSums(attr(v, "gradient"))
      hess <- apply(attr(v, "hessian"), c(2, 3), sum)
      attributes(f) <- list(gradient = gr, hessian = hess)
      f
    }
    
    
    # Sample data
    set.seed(87824)
    
    parameters <- c(input$Mean, input$SD)
    N <- input$N
    
    # global variable
    x <<- rnorm(N, mean = parameters[1], sd = parameters[2])
    
    # Grid
    sigma_lb <- ifelse(parameters[2] <= 5, 0.1, parameters[2] - 5)
    
    mu_values <- seq(parameters[1] - 5, parameters[1] + 5, by = 0.1)
    sigma_values <- seq(sigma_lb, parameters[2] + 5, by = 0.1)
    
    # Allocate matrices
    LL_matrix <- matrix(NA, length(mu_values), length(sigma_values))
    hover_text <- matrix("", length(mu_values), length(sigma_values))
    
    # Compute LL, gradients, and Hessians
    for (i in 1:length(mu_values)) {
      for (j in 1:length(sigma_values)) {
        mu <- mu_values[i]
        sigma <- sigma_values[j]
        result <- nll(c(mu, sigma))
        
        grad <- attr(result, "gradient")
        hess <- attr(result, "hessian")
        
        LL_matrix[i, j] <- result
        
        hover_text[j, i] <- paste0(
          "mu: ", round(mu, 2), "<br>",
          "sigma: ", round(sigma, 2), "<br>",
          "NLL: ", round(result, 2), "<br>",
          "∇mu: ", round(grad[1], 2), ", ∇²mu: ", round(hess[1, 1], 2), "<br>",
          "∇sigma: ", round(grad[2], 2),", ∇²sigma: ", round(hess[2, 2], 2), "<br>",
          #,"∇²mu_sigma: ", round(hess[1, 2], 2
          "Update Rule: par - (∇par/∇²par)", "<br>",
          "mu<sub>new</sub> = ", round(round(mu, 2) - round(grad[1], 2)/round(hess[1, 1], 2), 2),
          ", sigma<sub>new</sub> = ", round(round(sigma, 2) - round(grad[2], 2)/round(hess[2, 2], 2), 2))
      }
    }
    
    
    #  minimum
    mu_min <- round(mean(x), 2)
    sigma_min <- round(sd(x), 2)
    nll_min <- round(nll(c(mu_min, sigma_min)), 2)
    
    # Plot
    plot_ly(
      x = mu_values,
      y = sigma_values,
      z = LL_matrix,
      type = "surface",
      text = hover_text,
      hoverinfo = "text"
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = "mu"),
          yaxis = list(title = "sigma"),
          zaxis = list(title = "Negative Log-Likelihood")
        )
      ) %>%
      # Add point at estimated mean/sd
      add_markers(
        x = mu_min,
        y = sigma_min,
        z = nll_min,
        marker = list(color = "red", size = 7),
        name = "Minimum",
        hoverinfo = "text",
        text = paste0(
          "<b>Maximum Likelihood Estimate </b><br>",
          "mu = ", round(mu_min, 2), "<br>",
          "sigma = ", round(sigma_min, 2), "<br>",
          "NLL = ", round(nll_min, 2)
        )
      )
    
    
    
  })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)




# shinylive::export(appdir = ".",
#                  destdir = "docs/")
