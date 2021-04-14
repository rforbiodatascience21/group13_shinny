library(ggseqlogo)
library(shiny)
library(tidyverse)
library(shinythemes)

data(ggseqlogo_sample)

ui <- fluidPage(
  
  theme = shinytheme("cyborg"), 
  
  headerPanel("TF-binding Site Seq Logo Maker"),
  
  sidebarPanel(
    selectInput("transfactorsite", label = "JASPAR ID", 
                choices = ls(seqs_dna)),
  ),
  mainPanel(
    tags$label("Sequence Logo"),
    plotOutput("Seqlogo")
  )
  
)

server <- function(input, output, session) {

  output$Seqlogo <- renderPlot({
    ggseqlogo(pluck(seqs_dna,input$transfactorsite))
  })
}

shinyApp(ui, server)

