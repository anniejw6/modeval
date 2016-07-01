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
library(shinyBS)
library(plotly)
library(dplyr)
library(scales)

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
  
  titlePanel("Super-Simple Model Evaluation"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
      # h4('Data'),
      p(textOutput('type')),
      # h4(textOutput('auc')),
      bsCollapse(id = "auc_panel", #open = "Panel 2",
                 bsCollapsePanel(title = textOutput('auc'), 
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
                                 style = 'info'
                 )
      ),
      #h4(textOutput('cor')),
      bsCollapse(id = "cor_panel", #open = "Panel 2",
                 bsCollapsePanel(title = textOutput('cor'), 
                                 p('Correlation describes the statistical relationship between the model score and your outcome. This ranges between -1 (a strongly negative relationship, the worst) and 1 (a strong positive relationship, the best).'),
                                 style = 'info'
                 )
      ),
      bsCollapse(id = "plot_panel", #open = "Panel 2",
                 bsCollapsePanel(title = 'Decile Plot',
                                 p(paste('This is a plot of outcomes by decile.',
                                         'Decile means that we split the model score into equal-sized groups of 10 (as opposed to using hard cut-offs).',
                                         'In a good model, the number of positive outcomes (1s) will increase as the score increases,',
                                         'and the blue bars should get longer as you go to higher deciles.')),
                                 style = 'info'
                 )
      ),
      plotlyOutput('plt')
      #plotOutput('plt')
      #ggiraphOutput('plt')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Grab DF
  df <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(df_initial)
    }
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep,
             quote=input$quote)
  })
  
  # Data type
  output$type <- renderText({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(paste0("We're using some simulated data right now. ",
                    "Why don't you upload your own dataset? ",
                    "It needs to be a CSV with column names 'score' and 'outcome'. ",
                    "Your score should be a number, and your outcome should be either 0 or 1 ",
                    "where 1 is the outcome that your model is trying to predict, e.g., supporting the Democrat.")
      )
    } else {
      return(sprintf('Your dataset has %s observations.', nrow(df())))
    }
    
  })
  
  # Correlation
  output$cor <- renderText(
    sprintf('Correlation: %s',
            round(cor(df()$score, df()$outcome), 3))
  )
  
  # AUC
  output$auc <- renderText(
    sprintf('AUCROC: %s',
            round(pROC::auc(df()$outcome, df()$score)[1], 3)
    )
  )
  
  # Plot
  output$plt <- renderPlotly({
    
    # Data munging
    d_f <- df()
    # Add score bucket
    d_f$score_bucket <- cut(d_f$score, 
                            breaks = quantile(d_f$score, probs = seq(0, 1, 0.1)),
                            include.lowest = T)
    d_f$score_bucket <- factor(d_f$score_bucket, 
                               levels = rev(unique(d_f$score_bucket[order(d_f$score_bucket)])))
    d_f$outcome <- factor(d_f$outcome, levels = 1:0, ordered = T)
    
    # Generate summary dfs
    p <- d_f %>% 
      group_by(score_bucket, outcome) %>% 
      summarise(c = n()) %>%
      group_by(score_bucket) %>%
      mutate(p = c/sum(c),
             lab = sprintf('%s (%s)', c, scales::percent(c/sum(c)))
      ) %>%
      # Plot
      ggplot(aes(x = score_bucket, y = p,
                 fill = outcome,
                 text = lab))  +
      theme_bw() + xlab('') + ylab('') +
      scale_fill_manual(values = c('#5ab4ac', '#d8b365')) +
      theme(#axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.ticks.y = element_blank(),
        legend.position = 'none') + 
      scale_y_continuous(label = percent) + 
      geom_bar(stat = 'identity') + coord_flip()
    
    (gg <- ggplotly(p, tooltip = c("x", "fill", "text")))
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

