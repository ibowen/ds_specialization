library(shiny)
library(DT)

shinyUI(fluidPage(
    
    # Application title
    titlePanel("Text Prediction"),
    helpText("This application will predict the next word given a word, phrase, or sentence"),
    
    # Sidebar 
    sidebarLayout(
        sidebarPanel(      
            helpText("Type two or more words below, and press Enter to see the next word predictions:"),
            
            textInput("text", "Text Input", "such a great"),           
            br(), 
            radioButtons('ngram', 'Ngrams', c('Bigram'='2g',
                                              'Trigram'='3g',
                                              'Quargram'='4g',
                                              'Stupid Backoff'='backoff'), selected = 'Bigram', inline = FALSE, width = NULL),
            br(),
            submitButton("Submit"),      
            br(),
            br(),
            
            h4(helpText("Instruction:")),
            helpText("- The application implemented ngram prediction, you can choose which-gram in the options. The default is bigram."),
            helpText("- The predictions show three candidate words and their probabilities.")
        )
        ,
        
        # Main panel
        mainPanel(
            
            br(),
            h3("Priors:"),
            verbatimTextOutput("priors"),
            h3("Predictions:"),
            br(),
            DT::dataTableOutput('preds')
        )
    )
))