#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    titlePanel("Super-Simple Model Evaluations"),
    sidebarLayout(
      sidebarPanel(
        p("Input a file with headers score and outcome. Both should be numeric, and outcome should be either 1 or 0."),
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"'),
        downloadButton('downloadData', 'Download Sample Data')
      ),
      mainPanel(
        p(textOutput('type')),
        h4(textOutput('cor')),
        plotOutput('plt')
#        tableOutput('contents')
      )
    )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  df <- reactive({
    
    inFile <- input$file1

    if (is.null(inFile)){
      n <- 500
      d_f <- data.frame(
        score = round(runif(n, 0, 100), 1)
      )
      d_f$outcome <- rbinom(n, 1, d_f$score * 0.01)
      d_f$type <- 'test'
      
      return(d_f)
    }
      
    read.csv(inFile$datapath, header=input$header, sep=input$sep,
             quote=input$quote)
  })

  #output$contents <- renderTable(
  #  {df()}
  #  )
  
  output$type <- renderText({
    
    d_f <- df()
    if(!is.null(d_f$type)){
      return("This is just using example data. Why don't you upload your own dataset?")
    } else {
      return(sprintf('Your dataset has %s observations.', nrow(d_f)))
    }
    
  })
  output$cor <- renderText(
    sprintf('The correlation between the model score and your outcome is %s.',
            round(cor(df()$score, df()$outcome), 3))
    )
  
  output$plt <- renderPlot({
    
    d_f <- df()
    d_f$score_bucket <- cut(d_f$score, 
                             breaks = quantile(d_f$score, probs = seq(0, 1, 0.1)),
                             include.lowest = T)
    
    p <- ggplot(d_f, aes(x = score_bucket, fill = factor(outcome)))  + geom_bar() + 
      theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p)
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 'sample_modeleval.csv' },
    content = function(file) {
      d_f <- df()
      d_f$type <- NULL
      write.csv(d_f, file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

