library(shiny)

# Define UI for application that plots random distributions
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Karte: eine simulierte Epidemie"),

  # Sidebar with a slider input for lambda (Poisson distribution parameter)
  sidebarPanel(
    sliderInput("lambda",
                "Lambda (Poisson distribution):",
                min = 1,
                max = 50,
                value = 30)
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("geoPlot", height = 250)
  )
))
