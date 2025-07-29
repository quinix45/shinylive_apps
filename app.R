library(shiny)
library(plotly)
library(munsell)
library(bslib)

# Define 2PL IRF function
irf_2PL <- function(theta, a = 1, b = 0) {
  exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
}

# Set serif theme
theme_set(theme_classic(base_size = 16, base_family = "Times New Roman"))

# Define UI
ui <- bslib::page_fluid(
  mainPanel(
    fluidRow(column(12, plotlyOutput("distPlot", height = "370px", width = "100%")))
  ),
  
  fluidRow(
    column(3, numericInput("a", "a (discrimination)", value = 1, min = 0, max = 8, step = 0.1)),
    column(3, numericInput("b", "b (difficulty)", value = 0, min = -6, max = 6, step = 0.1))
  )
)

# Define server
server <- function(input, output, session) {
  
  output$distPlot <- renderPlotly({
    
    theta <- seq(-4, 4, length.out = 500)
    prob <- irf_2PL(theta, a = input$a, b = input$b)
    
    # Custom hover text
    hover_text <- paste0(
      "θ = ", sprintf("%.2f", theta), "<br>",
      "P(Y = 1|θ) = ", sprintf("%.2f", prob)
    )
    
    plot_ly(
      x = ~theta, y = ~prob,
      type = "scatter", mode = "lines",
      text = hover_text, hoverinfo = "text",
      line = list(color = "#1b305c"),
      name = "2PL IRF"
    ) %>%
      layout(
        xaxis = list(
          title = "θ",
          range = c(-4, 4),
          zeroline = FALSE,
          showline = TRUE,
          linecolor = 'black'
        ),
        yaxis = list(
          title = "P(Y = 1|θ)",
          range = c(0, 1),
          zeroline = FALSE,
          showline = TRUE,
          linecolor = 'black'
        ),
        template = "plotly_white"
      )
  })
}

# Run app
shinyApp(ui = ui, server = server)
# 
# 
# shinylive::export(appdir = ".",
#                  destdir = "docs/")

