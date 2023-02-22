#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(palmerpenguins)
library(tidyverse)
library(DT)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Penguin Data"),

    fluidRow(column(4, plotOutput("penguin_plot_1")),
             column(4, plotOutput("penguin_plot_2")),
             column(4, plotOutput("penguin_plot_3"))),
    hr(),
    fluidRow(
      h3("Select values to create a new penguin:")
    ),
    fluidRow(
      column(4, selectizeInput("species_sel", "Species:", choices = unique(penguins$species), selected = NULL)),
      column(4, selectizeInput("island_sel", "Island:", choices = unique(penguins$island), selected = NULL)),
      column(4, selectizeInput("sex_sel", "Sex:", choices = unique(penguins$sex), selected = NULL))),
    fluidRow(
      column(10),
      column(2, bs4Dash::actionButton(
        style = "color: #FFFFFF; background-color: #5874FF;",
        "add_penguin",
        "Add a penguin",
        icon = shiny::icon("plus"),
        width = "100%"
      ))
    ),
    hr(),
    h3("Here are your penguins:"),
    fluidRow(column(12, DT::dataTableOutput("create_penguins"))),
    shiny::fluidRow(
      shinyVirga::col_10(),
      shinyVirga::col_2(
        bs4Dash::actionButton(
          style = "color: #FFFFFF; background-color: #5874FF;",
          "remove_penguin",
          "Remove last penguin",
          icon = shiny::icon("minus"),
          width = "100%"
        )
      ),
      shinyVirga::col_2(
        bs4Dash::actionButton(
          style = "color: #FFFFFF; background-color: #5874FF;",
          "add_penguin",
          "Add a penguin",
          icon = shiny::icon("plus"),
          width = "100%"
        )
      )
    ),
    verbatimTextOutput('view_penguins')
))
