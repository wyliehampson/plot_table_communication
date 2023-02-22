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
  
  penguins <- palmerpenguins::penguins
  hydrology_df <- readr::read_csv(here("data", "hydrology_data_combined_tidy.csv"))
  
  # Generating summary data
  summary_data = hydrology_df |>
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
  
  counter_species <- reactiveVal(0)
  counter_island <- reactiveVal(0)
  counter_hydrology <- reactiveVal(0)

  get_species_sel_id <- reactive({
    isolate(counter_species(counter_species() + 1))
    paste0("species_sel", counter_species())
  })

  get_island_sel_id <- reactive({
    isolate(counter_island(counter_island() + 1))
    paste0("island_sel", counter_island())
  })

  get_hydrology_sel_id <- reactive({
    isolate(counter_hydrology(counter_hydrology() + 1))
    paste0("hydrology_sel", counter_hydrology())
  })

  user_df <- reactive({
    df <- tibble::tribble(~penguin_id, ~species, ~island, ~hydrology,
                          NA, NA, NA, NA)

    for (i in 1:nrow(df)) {
      df$penguin_id[i] <- paste("Penguin", i)
      df$species[i] <- as.character(selectInput(paste0(get_species_sel_id(), i),
                                                NULL,
                                                choices = unique(penguins$species),
                                                width = "100px"))
      df$island[i] <- as.character(selectInput(paste0(get_island_sel_id(), i),
                                                NULL,
                                                choices = unique(penguins$island),
                                                width = "100px"))
      df$hydrology[i] <- as.character(selectInput(paste0(get_hydrology_sel_id(), i),
                                                NULL,
                                                choices = unique(hydrology_df$hydrology),
                                                width = "100px"))
    }
    df
  })
  
  # user_df <- tibble::tribble(~penguin_id, ~species, ~island, ~hydrology,
  #                           "Penguin 1",
  #                           as.character(selectInput(inputId = "species_sel1", label = NULL, choices = unique(penguins$species))),
  #                           as.character(selectInput(inputId = "island_sel1", label = NULL, choices = unique(penguins$island))),
  #                           as.character(selectInput(inputId = "hydrology_sel1", label = NULL, choices = unique(hydrology_df$hydrology)))
  # )
  # 
  # user_df <- shiny::reactiveVal(user_df)
  
  # num_rows <- reactiveVal(2)
  # 
  # user_df <- reactive({
  #   #print(paste("Penguin", num_rows()))
  #   df <- data.frame()
  # 
  # 
  #   for (i in 1:num_rows()) {
  #     df$penguin_id[i] <- paste("Penguin", num_rows())
  #     df$species[i] <- as.character(selectInput(inputId = paste0("species_sel", nrow(df)), label = NULL, choices = unique(penguins$species), width = "100px"))
  #     df$island[i] <- as.character(selectInput(inputId = paste0("island_sel", nrow(df)), label = NULL, choices = unique(penguins$island), width = "100px"))
  #     df$hydrology[i] <- as.character(selectInput(inputId = paste0("hydrology_sel", nrow(df)), label = NULL, choices = unique(hydrology_df$hydrology), width = "100px"))
  #   }
  # 
  #   df
  # })

  # row_counter <- reactiveVal(1)
  # 
  # user_df <- reactive({
  #   
  #   for (i in 1:row_counter()) {
  #     df <- data.frame(penguin_id = paste("Penguin", row_counter()),
  #                      species = as.character(selectInput(inputId = paste0("species_sel", row_counter()), label = NULL, choices = unique(penguins$species))),
  #                      island = as.character(selectInput(inputId = paste0("island_sel", row_counter()), label = NULL, choices = unique(penguins$island))),
  #                      hydrology = as.character(selectInput(inputId = paste0("hydrology_sel", row_counter()), label = NULL, choices = unique(hydrology_df$hydrology)))
  #                      )
  #     row_counter(row_counter() + 1)
  #   }
  #   df
  # })
  
  
  output$create_penguins <- DT::renderDataTable({
    DT::datatable(
      user_df(),
      #editable = FALSE,
      escape = FALSE,
      selection = 'none',
      options = list(dom = 't', paging = FALSE, ordering = FALSE),
      callback = JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());"),
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
                                         species = as.character(selectInput(paste0("species_sel", (nrow(user_df()) + 1)), label = NULL, choices = unique(penguins$species))),
                                         island = as.character(selectInput(paste0("island_sel", (nrow(user_df()) + 1)), label = NULL, choices = unique(penguins$island))),
                                         hydrology = as.character(selectInput(paste0("hydrology_sel", (nrow(user_df()) + 1)), label = NULL, choices = unique(hydrology_df$hydrology)))))

    user_df(table)
    #print(input$species_sel2)
    })
  
  output$view_penguins = renderPrint({
    #input[["species_sel11"]]
  })
  
  # Plots
  output$penguin_plot_1 <- renderPlot({
      ggplot2::ggplot(data = penguins, aes(x = species)) +
        ggplot2::geom_bar()
  })
  
  output$penguin_plot_2 <- renderPlot({
    ggplot2::ggplot(data = penguins, aes(x = island)) +
      ggplot2::geom_bar()
  })

  # Hydrology Plot
  output$penguin_plot_3 <- renderPlot({
    ggplot2::ggplot(hydrology_df, ggplot2::aes(x = factor(hydrology, 
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
    ), y = yearly_flow, fill = hydrology)) +
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
      ggplot2::scale_fill_brewer(palette = "Accent", 
                                 guide = 'none'
      ) +
      ggplot2::geom_text(data = summary_data, ggplot2::aes(x = hydrology, y = y, label = label), 
                         inherit.aes = FALSE, 
                         position = ggplot2::position_nudge(x = -0.3), 
                         size = 4
      )
  })
  
})
