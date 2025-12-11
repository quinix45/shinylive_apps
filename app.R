library(shiny)
library(munsell)
library(bslib)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(2, 
           "Prior Shape:",
           numericInput("alpha", "\u03B1", value = 8, min = 0.1, step = 1),
           numericInput("beta", "\u03B2", value = 9, min = 0.1, step = 1),
           numericInput("true_prob", "True Probability", value = 0.6, min = 0.01, max = 0.99, step = 0.01),
           numericInput("N", "N Total", value = 100, min = 1, max = 1000, step = 1),
           numericInput("N_available", "N Available", value = 10, min = 1, max = 100, step = 1),
           actionButton("sample_btn", "Draw Samples")
    ),
    column(10,  
           bslib::card(plotOutput(outputId = "Prior_plot"), full_screen = TRUE),
           HTML("Note: The plot will show the estimated true probability of a binary variable with (MAP) and without priors (ML) if N data points are available, indicated by <b>N available</b>. The values of the <b>N available</b> data point (0 or 1) is also shown. Increase to see how the Estimates update!")
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamically update the max of N_available when N changes
  observe({
    updateNumericInput(session, "N_available", max = input$N)
    if (input$N_available > input$N) {
      updateNumericInput(session, "N_available", value = input$N)
    }
  })
  
  # Reactive value to store sampled data
  sampled_data <- reactiveVal(NULL)
  
  # Update sampled data when button is pressed
  observeEvent(input$sample_btn, {
    sampled_data(rbinom(input$N, size = 1, prob = input$true_prob))
  })
  
  output$Prior_plot <- renderPlot({
    
    alpha <- input$alpha
    beta <- input$beta
    true_prob <- input$true_prob
    mode_beta <- (alpha - 1)/(alpha + beta - 2)
    max_dens <- dbeta(mode_beta, alpha, beta)
    
    p_grid <- seq(from = 0.01, to = 0.99, length.out = 300)
    
    if (is.null(sampled_data())) {
      # Plot before any samples: only prior + true probability
      segments_df <- data.frame(
        x = true_prob,
        xend = true_prob,
        y = 0,
        yend = max_dens/2,
        label = "True Probability"
      )
      
      ggplot(data = data.frame(x = c(0, 1)), aes(x = x)) +
        geom_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta), 
                      color = "blue", lty = 2) +
        annotate("text", x = mode_beta, y = max_dens/1.2, label = "Prior \n Distribution") +
        geom_segment(data = segments_df,
                     aes(x = x, xend = xend, y = y, yend = yend, color = label),
                     size = 1.5) +
        theme_classic() +
        scale_color_manual(values = c("black")) +
        scale_y_continuous(expand = c(0,0)) +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.line.y = element_blank()) +
        labs(color = "")
      
    } else {
      # Plot after samples: prior + ML/MAP estimates
      prior <- dbeta(p_grid, shape1 = alpha, shape2 = beta)
      data <- sampled_data()
      ML_res <- MAP_res <- numeric(length(data))
      
      for(i in 1:length(data)){
        likelihood <- sapply(data[1:i], function(x) dbinom(x, size = 1, prob = p_grid))
        max_ml <- which.max(rowSums(log(likelihood)))
        max_map <- which.max(rowSums(log(likelihood)) + log(prior))
        ML_res[i] <- p_grid[max_ml]
        MAP_res[i] <- p_grid[max_map]
      }
      
      N_data <- min(input$N_available, length(data))
      segments_df <- data.frame(
        x = c(true_prob, ML_res[N_data], MAP_res[N_data]),
        xend = c(true_prob, ML_res[N_data], MAP_res[N_data]),
        y = 0,
        yend = max_dens/2,
        label = c("True Probability", "No Prior", "With Prior")
      )
      
      ggplot(data = data.frame(x = c(0, 1)), aes(x = x)) +
        geom_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta), color = "blue", lty = 2) +
        annotate("text", x = mode_beta, y = max_dens/1.2, label = "Prior \n Distribution") +
        annotate("text", x = mode_beta, y = max_dens/1.4, label = paste("data point", input$N_available, "\n was a", data[input$N_available]))+
        geom_segment(data = segments_df,
                     aes(x = x, xend = xend, y = y, yend = yend, color = label),
                     size = 1) +
        scale_color_manual(values = c("red", "black", "blue")) +
        theme_classic() +
        scale_y_continuous(expand = c(0,0)) +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.line.y = element_blank()) +
        labs(color = "")
    }
    
  })
}

shinyApp(ui = ui, server = server)


# create shinylive files (then move to approapriate shinyapp directory)

# shinylive::export(appdir = ".",
#                  destdir = "docs/")