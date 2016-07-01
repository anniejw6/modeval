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
library(pROC)

# Create initial dataset

gen_df <- function(n){
  df_initial <- data.frame(
    score = round(runif(n, 0, 100), 1),
    type = 'test'
  )
  df_initial$outcome <- rbinom(n, 1, df_initial$score * 0.01)
  df_initial
}

df_initial <- gen_df(500)

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
        h4(textOutput('auc')),
        p(paste0('The AUCROC is the probability that if you randomly choose someone with a ', 
                 'positive outcome (e.g., supports the D candidate, or whatever your model is trying to predict), ',
                 'that person has a higher model score ',
                 'than a randomly chosen person with a negative outcome ',
                 '(e.g, supports the R candidate).')),
        tags$ul(
           tags$li("If the probability is 1, then you are always right (best)."),
           tags$li('If the probability is 0.5, then you are guessing randomly (equivalent to not having a model).'),
           tags$li('If the probability is 0, you are always wrong (worst).')
        ),
        p('A "good" support model during a general election has an AUCROC of at least 0.7 and usually 0.8.'),
        h4(textOutput('cor')),
        p('This is the correlation between the model score and your outcome. This ranges between -1 (worst) and 1 (best).'),
        h4('Decile Plot'),
        p('This is a plot of outcomes by decile (these are calculated using score percentiles, not hard cut-offs).'),
        plotOutput('plt')
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  df <- reactive({
    
    inFile <- input$file1

    if (is.null(inFile)){
      return(df_initial)
    }
      
    read.csv(inFile$datapath, header=input$header, sep=input$sep,
             quote=input$quote)
  })

  
  output$type <- renderText({
    
    d_f <- df()
    if(!is.null(d_f$type)){
      return("This is just using example data. Why don't you upload your own dataset?")
    } else {
      return(sprintf('Your dataset has %s observations.', nrow(d_f)))
    }
    
  })
  output$cor <- renderText(
    sprintf('Correlation: %s',
            round(cor(df()$score, df()$outcome), 3))
    )
  
  output$auc <- renderText(
    sprintf('AUCROC: %s',
            round(pROC::auc(df()$outcome, df()$score)[1], 3)
            )
  )
  
  output$plt <- renderPlot({
    
    d_f <- df()
    d_f$score_bucket <- cut(d_f$score, 
                             breaks = quantile(d_f$score, probs = seq(0, 1, 0.1)),
                             include.lowest = T)
    d_f$outcome <- factor(d_f$outcome)
    
    p <- ggplot(d_f, aes(x = score_bucket, fill = outcome))  + geom_bar() + 
      theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
                         axis.ticks.x = element_blank()) + 
      xlab('')
    
    print(p)
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 'sample_modeleval.csv' },
    content = function(file) {
      d_f <- gen_df(1000)
      d_f$type <- NULL
      write.csv(d_f, file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

