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
  
  user_df <- tibble::tribble(~penguin_id, ~species, ~island, ~sex,
                            "Penguin 1", 
                            as.character(selectInput(paste0("species_sel1"), "", choices = unique(penguins$species), width = "100px")),
                            as.character(selectInput(paste0("island_sel1"), "", choices = unique(penguins$island), width = "100px")), 
                            as.character(selectInput(paste0("sex_sel1"), "", choices = unique(penguins$sex), width = "100px"))
  )

  user_df <- shiny::reactiveVal(user_df)
  
  output$create_penguins <- DT::renderDataTable({
    DT::datatable(
      user_df(),
      editable = TRUE,
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
  shiny::observeEvent(input$create_penguins_cell_clicked, {
    # capture cell edit event
    i <- str(input$create_penguins_cell_clicked)
    print(i)
  })
  
  # Remove Penguin Button
  shiny::observeEvent(input$remove_penguin, {
    user_df(
      user_df()[-nrow(user_df()),]
    )
  })
  
  # Add Penguin Button
  shiny::observeEvent(input$add_penguin, {
    table <- rbind(user_df(), data.frame(penguin_id = paste("Penguin", nrow(user_df()) + 1),
                                         species = as.character(selectInput(paste0("species_sel", (nrow(user_df()) + 1)), "", choices = unique(penguins$species), width = "100px")),
                                         island = as.character(selectInput(paste0("island_sel", (nrow(user_df()) + 1)), "", choices = unique(penguins$island), width = "100px")),
                                         sex = as.character(selectInput(paste0("sex_sel", (nrow(user_df()) + 1)), "", choices = unique(penguins$sex), width = "100px"))))
    
    user_df(table)
    })
  
  reactive(print(input$species_sel1))
  
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
