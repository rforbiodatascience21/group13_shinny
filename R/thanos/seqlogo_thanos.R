library(shiny)
library(ggseqlogo)
library(tidyverse)
library(shinythemes)
library(BiocManager)
library(motifStack)
library(MotifDb)

#install("motifStack")
#install("MotifDb")


ui <- fluidPage(
  
  theme = shinytheme("superhero"), 
  
  headerPanel("TF-binding Site Seq Logo Maker"),
  
  sidebarPanel(
    selectInput("transfactorsite", label = "Sequence:", 
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