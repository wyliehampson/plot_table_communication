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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  penguins <- palmerpenguins::penguins
  
  counter_species <- reactiveVal(0)
  counter_island <- reactiveVal(0)
  counter_sex <- reactiveVal(0)

  get_species_sel_id <- reactive({
    isolate(counter_species(counter_species() + 1))
    paste0("species_sel", counter_species())
  })

  get_island_sel_id <- reactive({
    isolate(counter_island(counter_island() + 1))
    paste0("island_sel", counter_island())
  })

  get_sex_sel_id <- reactive({
    isolate(counter_sex(counter_sex() + 1))
    paste0("sex_sel", counter_sex())
  })

  user_df <- reactive({
    df <- tibble::tribble(~penguin_id, ~species, ~island, ~sex,
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
      df$sex[i] <- as.character(selectInput(paste0(get_sex_sel_id(), i),
                                                NULL,
                                                choices = unique(penguins$sex),
                                                width = "100px"))
    }
    df
  })
  
  # user_df <- tibble::tribble(~penguin_id, ~species, ~island, ~sex,
  #                           "Penguin 1",
  #                           as.character(selectInput(inputId = "species_sel1", label = NULL, choices = unique(penguins$species))),
  #                           as.character(selectInput(inputId = "island_sel1", label = NULL, choices = unique(penguins$island))),
  #                           as.character(selectInput(inputId = "sex_sel1", label = NULL, choices = unique(penguins$sex)))
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
  #     df$sex[i] <- as.character(selectInput(inputId = paste0("sex_sel", nrow(df)), label = NULL, choices = unique(penguins$sex), width = "100px"))
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
  #                      sex = as.character(selectInput(inputId = paste0("sex_sel", row_counter()), label = NULL, choices = unique(penguins$sex)))
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
      colnames = c("Penguin ID", "Species", "Island", "Sex"),
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
                                         sex = as.character(selectInput(paste0("sex_sel", (nrow(user_df()) + 1)), label = NULL, choices = unique(penguins$sex)))))

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

  output$penguin_plot_3 <- renderPlot({
    ggplot2::ggplot(data = penguins, aes(x = sex)) +
      ggplot2::geom_bar()
  })
  
})
