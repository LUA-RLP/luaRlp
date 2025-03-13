library(shiny)
library(sf)
library(ggplot2)

shinyServer(function(input, output) {

  output$geoPlot <- renderPlot({

    # Generate Poisson-distributed incidence values using user-selected lambda
    RLP_geo[["Kreise"]]$incidence <- rpois(nrow(RLP_geo[["Kreise"]]), lambda = input$lambda)

    # Plot using ggplot2
    ggplot(data = RLP_geo[["Kreise"]]) +
      geom_sf(aes(fill = incidence)) +  # Color polygons based on incidence
      scale_fill_viridis_c(option = "magma", name = "Incidence") +  # Color scale
      theme_minimal()
  })

})
