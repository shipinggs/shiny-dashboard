library(shiny)

ui <- navbarPage(title = "My Shiny App",
  theme = "css/styles.css",
  tabPanel(title = "Normal data",
           plotOutput("norm"),
           actionButton("renorm", "Resample")
  ),
  tabPanel(title = "Uniform data",
           plotOutput("unif"),
           actionButton("reunif", "Resample")
  ),
  tabPanel(title = "Chi Squared data",
           plotOutput("chisq"),
           actionButton("rechisq", "Resample")
  ),
  fluidRow(
    column(12, tags$img(style = "display: block; margin: 0 auto",
                        height = 100,
                        width = "auto",
                        src = "img/sutd-logo.png")
    )
  ),
  fluidRow(
    tags$p(style = "text-align: center", "This is a",
           tags$strong("Shiny"),
           "app.")
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "num",
                  label = "Choose a number:",
                  value = 25, min = 1, max = 100),
      tags$hr(style="border-color: #BDBDBD"),
      textInput(inputId = "title",
                label = "Write a title:",
                value = "Histogram of random normal values")
      # actionButton(inputId = "go",
      #              label = "Go "),
    ),
    mainPanel(
      plotOutput("hist"),
      verbatimTextOutput("stats")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      actionButton("norm", "Normal"),
      actionButton("unif", "Uniform")
    ),
    
    mainPanel(
      plotOutput("hist2")
    )
  )
)

server <- function(input, output) {
  rvTabs <- reactiveValues(
    norm = rnorm(500), 
    unif = runif(500),
    chisq = rchisq(500, 2))
  
  observeEvent(input$renorm, { rvTabs$norm <- rnorm(500) })
  observeEvent(input$reunif, { rvTabs$unif <- runif(500) })
  observeEvent(input$rechisq, { rvTabs$chisq <- rchisq(500, 2) })
  
  output$norm <- renderPlot({
    hist(rvTabs$norm, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a standard normal distribution")
  })
  output$unif <- renderPlot({
    hist(rvTabs$unif, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a standard uniform distribution")
  })
  output$chisq <- renderPlot({
    hist(rvTabs$chisq, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a Chi Square distribution with two degree of freedom")
  })
  
  rv <- reactiveValues(data = rnorm(100))
  
  observeEvent(input$norm, { rv$data <- rnorm(1000) })
  observeEvent(input$unif, { rv$data <- runif(1000) })
  
  output$hist2 <- renderPlot({
    hist(rv$data)
  })
  
  data <- reactive({
    rnorm(input$num)
  })
 
  # data <- eventReactive(input$go, {
  #   rnorm(input$num)
  # })
  
  output$hist <- renderPlot({ 
    hist(data(), 
         main = input$title)
  })
  output$stats <- renderPrint({
    summary(data())
  })
} 

shinyApp(ui = ui, server = server)