#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(palmerpenguins)
library(tidyverse)
library(DT)
library(here)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  hydrology_df <- readr::read_csv(here("data", "hydrology_data_combined_tidy.csv"))
  
  penguins <- reactive({
    palmerpenguins::penguins |> 
      drop_na() |> 
      dplyr::mutate(highlight_species = ifelse(species == input$species_sel, "highlight", "normal"),
                    highlight_island = ifelse(island == input$island_sel, "highlight", "normal"))
  })
  
  hydrology_df_reactive <- reactive({
    hydrology_df |>
      drop_na() |>
      dplyr::mutate(highlight_hydrology = ifelse(hydrology == input$hydrology_sel, "highlight", "normal"))
  })
  
  # Generating summary data
  summary_data <- reactive({hydrology_df_reactive() |>
    dplyr::mutate(hydrology = factor(hydrology, 
                                     levels = c("stress_test", 
                                                "pluvial_removed", 
                                                "paleo_conditioned", 
                                                "direct_paleo_natural_flow", 
                                                "cmip_3",
                                                "full_hydro" 
                                     ), 
                                     labels = c("Stress Test", 
                                                "Pluvial Removed", 
                                                "Paleo Conditioned", 
                                                "Paleo Resampled", 
                                                "CMIP3", 
                                                "Observed (Full)"
                                     )
    )) |>
    dplyr::group_by(hydrology) |>
    dplyr::summarize(median = stats::median(yearly_flow), 
                     q1 = stats::quantile(yearly_flow, 0.25), 
                     q3 = stats::quantile(yearly_flow, 0.75)
    ) |>
    tidyr::pivot_longer(median:q3, names_to = "type", values_to = "y") |>
    dplyr::mutate(label = scales::unit_format(unit = "M", scale = 1e-6, accuracy = .01)(y))
  })
  
  user_df <- tibble::tribble(~penguin_id, ~species, ~island, ~hydrology)
  
  user_df <- reactiveVal(user_df)
  
  output$create_penguins <- DT::renderDataTable({
    DT::datatable(
      user_df(),
      editable = FALSE,
      escape = FALSE,
      selection = 'none',
      #options = list(dom = 't', paging = FALSE, ordering = FALSE),
      colnames = c("Penguin ID", "Species", "Island", "Hydrology"),
      rownames = FALSE
    )
  })
  
  #------------------------------
  # observe edit on datatable and update original data frame
  # shiny::observeEvent(input$create_penguins_cell_clicked, {
  #   # capture cell edit event
  #   i <- str(input$create_penguins_cell_clicked)
  # })
  
  # Remove Penguin Button
  shiny::observeEvent(input$remove_penguin, {
    user_df(
      user_df()[-nrow(user_df()),]
    )
  })
  
  # Add Penguin Button
  shiny::observeEvent(input$add_penguin, {
    table <- rbind(user_df(), data.frame(penguin_id = paste("Penguin", nrow(user_df()) + 1),
                                         species = input$species_sel,
                                         island = input$island_sel,
                                         hydrology = input$hydrology_sel))
    
    user_df(table)
    
    shinyWidgets::show_alert(
      title = "Penguin Added!",
      type = "success",
      btn_labels = "Close"
    )
    })
  
  output$view_penguins = renderPrint({
    #input[["species_sel11"]]
  })
  
  # Plots
  output$penguin_plot_1 <- renderPlot({
    ggplot2::ggplot(data = penguins(), aes(x = species, fill = highlight_species)) +
      ggplot2::geom_bar() +
      scale_fill_manual(values=c("#EF0303", "grey")) +
      theme_light() +
      theme(legend.position = "none")
  })
  
  output$penguin_plot_2 <- renderPlot({
    ggplot2::ggplot(data = penguins(), aes(x = island, fill = highlight_island)) +
      ggplot2::geom_bar() +
      scale_fill_manual(values=c("#EF0303", "grey")) +
      theme_light() +
      theme(legend.position = "none")
  })

  # Hydrology Plot
  output$penguin_plot_3 <- renderPlot({
    ggplot2::ggplot(hydrology_df_reactive(), ggplot2::aes(x = factor(hydrology, 
                                                                     levels = c("stress_test", 
                                                                                "pluvial_removed",
                                                                                "full_hydro" ,
                                                                                "paleo_conditioned", 
                                                                                "direct_paleo_natural_flow", 
                                                                                "cmip_3"
                                                                                
                                                                     ), 
                                                                     labels = c("Stress Test", 
                                                                                "Pluvial Removed", 
                                                                                "Observed (Full)",
                                                                                "Paleo Conditioned", 
                                                                                "Paleo Resampled", 
                                                                                "CMIP3"
                                                                                
                                                                     )
    ), y = yearly_flow, fill = highlight_hydrology)) +
      see::geom_violinhalf(position = ggplot2::position_nudge(x = 0.1),
                           trim = FALSE
      ) +
      ggplot2::theme_classic(base_size = 16) +
      ggplot2::geom_boxplot(position = ggplot2::position_nudge(x = -0.1), 
                            width = 0.1, outlier.color = NA,
                            show.legend = FALSE) +
      ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-06), 
                                  name = "Average Annual Flow at Lees Ferry (MAF)") +
      ggplot2::xlab("Hydrology") +
      scale_fill_manual(values=c("#EF0303", "grey")) +
      theme(legend.position = "none") +
      ggplot2::geom_text(data = summary_data(), ggplot2::aes(x = hydrology, y = y, label = label), 
                         inherit.aes = FALSE, 
                         position = ggplot2::position_nudge(x = -0.3), 
                         size = 4
      )
  })
  
})
