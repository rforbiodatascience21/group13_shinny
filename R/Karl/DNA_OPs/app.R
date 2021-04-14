source("DNA_utils.R")

library(shiny)
library(shinythemes)
library(readr)

ui <- fluidPage(
  
  theme = shinytheme("cyborg"), 
  
  headerPanel("PlayDNA"),
  
  sidebarPanel(
    textInput("sequence", label = NULL, placeholder = "Paste Your Sequence Here"),
    fileInput("upload", "Or Upload a Sequence in txt format"),
    actionButton("random","Make Random DNA",class = "btn-block")
    ),
  
  mainPanel(
    tabsetPanel(
    tabPanel(
    "Complemented Sequence",
    textOutput("compseq")
    ),
    tabPanel(
    "Translated Sequence:",
    textOutput("transseq")
    )
    )
  )
)


server <- function(input, output, session) {
  
  observeEvent(
    input$random,{
      a <- random_dna(sample.int(21,1))
      updateTextInput(session, "sequence", value = a)
    }
  )
  
  observeEvent(
    input$upload,{
    seq <- read_file(input$upload$datapath)
    updateTextInput(session, "sequence", value = seq)
    }
  )
  
  output$compseq <- renderText(
    if (input$sequence==""){
      return(NULL)
    }
    else{
    complement(input$sequence)
    }
  )
  
  output$transseq <- renderText(
    if (nchar(input$sequence)<3){
      return(NULL)
    }
    else{
    dna_codons_to_aa(mk_codons(input$sequence)) 
    }
  )
  
}
  

shinyApp(ui, server)
