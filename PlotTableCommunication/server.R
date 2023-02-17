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
  
  penguins <- reactive({
    palmerpenguins::penguins |> 
      drop_na() |> 
      dplyr::mutate(highlight_species = ifelse(species == input$species_sel, "highlight", "normal"),
                    highlight_island = ifelse(island == input$island_sel, "highlight", "normal"),
                    highlight_sex = ifelse(sex == input$sex_sel, "highlight", "normal"))
    })
  
  user_df <- tibble::tribble(~penguin_id, ~species, ~island, ~sex)

  user_df <- shiny::reactiveVal(user_df)
  
  output$create_penguins <- DT::renderDataTable({
    DT::datatable(
      user_df(),
      editable = FALSE,
      escape = FALSE,
      selection = 'none',
      #options = list(dom = 't', paging = FALSE, ordering = FALSE),
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
                                         species = input$species_sel,
                                         island = input$island_sel,
                                         sex = input$sex_sel))
    
    user_df(table)
    
    shinyWidgets::show_alert(
      title = "Penguin Added!",
      type = "success",
      btn_labels = "Close"
    )
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

  output$penguin_plot_3 <- renderPlot({
    ggplot2::ggplot(data = penguins(), aes(x = sex, fill = highlight_sex)) +
      ggplot2::geom_bar() +
      scale_fill_manual(values=c("#EF0303", "grey")) +
      theme_light() +
      theme(legend.position = "none")
  })
  
})
