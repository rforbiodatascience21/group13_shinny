library(shiny)
library(shinythemes)
library(tidyverse)
library(ggdark)


load("gravier_nested_long_with_models")

significant_identification <- function(dataset,p){
  dataset <-
    dataset %>% 
    mutate(identified_as = 
                         case_when(p.value<p~"significant",
                                   TRUE~"unsignificant"))
}

manhatten_plot <- function(dataset,p){
  dataset %>% 
  mutate(gene = fct_reorder(as.factor(gene),
                            neg_log_p,
                            .desc = TRUE)) %>% 
  ggplot(aes(gene,
             neg_log_p,
             colour = identified_as)) + 
  geom_point(size = 2) + 
  geom_hline(yintercept = -log10(p),
             linetype = "dashed") + 
  labs(x="Gene",
       y="Minus log10(p)") +
  dark_theme_gray() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=45,
                                   size=3))
  }

filter_sig_genes <- function(dataset){
  data <-
    dataset %>% 
    filter(identified_as=="significant") %>% 
    select("gene","p.value") %>% 
    mutate(p.value = as.character(p.value))
  return(data)
  
  
}

count_sig_genes <- function(dataset){
  data <-
    dataset %>% 
    filter(identified_as=="significant") %>% 
    select("gene","p.value") %>% 
    mutate(p.value = as.character(p.value))%>% 
    count()
  return(data)
  
  
}
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  fluidRow(
    column(8,
  sliderInput("p","p-value",1e-3,0.1,value=0.05,step=0.01)
    ),
  column(4,
  checkboxInput("bon","Bonferroni Correction")
  )
  ),
  fluidRow(
  plotOutput("plot"),
  h3("Number of genes:"),
  tableOutput("gene_count"),
  h3("Genes Identified as Significant:"),
  tableOutput("sig_genes"),
  
  
  )
)

server <- function(input,output,session){
  
 data <- eventReactive(
   {input$p
     input$bon},{
    if (input$bon==FALSE){
    significant_identification(gravier_data_nested_long,input$p)
    }
    else{
    significant_identification(gravier_data_nested_long,input$p/100)
    }
    }
  )
 
  output$plot <- renderPlot(manhatten_plot(data(),input$p))
  
  output$sig_genes <- renderTable(filter_sig_genes(data()))
  
  output$gene_count <- renderTable(count_sig_genes(data()))
}

shinyApp(ui, server)

