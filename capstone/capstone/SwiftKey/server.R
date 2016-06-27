library(shiny)
library(dplyr)
library(DT)

# Load dataframes
df.train.2g <- readRDS(file = './train.2g.df')
df.train.3g <- readRDS(file = './train.3g.df')
df.train.4g <- readRDS(file = './train.4g.df')

# Load ngram functions
pred <- function(predictor, df) {
    preds <- arrange(filter(df, prior == predictor), desc(freq.term))[1:3, c('posterior', 'proba')]
    preds <- data.frame(na.omit(preds))
    preds
}

# backoff
pred.backoff <- function(predictor) {
    pred.4 <- pred(predictor, df.train.4g)
    # back off to 3g
    predictor <- paste(unlist(strsplit(predictor, ' '))[2:3], collapse = ' ')
    pred.3 <- pred(predictor, df.train.3g)
    pred.3$proba <- log(exp(pred.3$proba) * 0.4)
    
    # back off to 2g
    predictor <- paste(unlist(strsplit(predictor, ' '))[2:3], collapse = ' ')
    pred.2 <- pred(predictor, df.train.2g)
    pred.2$proba <- log(exp(pred.2$proba) * 0.4)

    # cbind all preds together
    preds <- rbind(pred.4, pred.3, pred.2)
    preds
   
}

# Load input processing functions
get_prior3 <- function(text) {
    text <- as.character(text)
    text <- tolower(trimws(text))
    words = unlist(strsplit(text, ' '))
    len <- length(words)
    paste(words[(len-2):len], collapse = ' ')
}

get_prior2 <- function(text) {
    text <- as.character(text)
    text <- tolower(trimws(text))
    text <- as.character(text)
    words = unlist(strsplit(text, ' '))
    len <- length(words)
    paste(words[(len-1):len], collapse = ' ')
}

get_prior1 <- function(text) {
    text <- as.character(text)
    text <- tolower(trimws(text))
    text <- as.character(text)
    words = unlist(strsplit(text, ' '))
    len <- length(words)
    words[len]
}


# Read all the necessary files before starting
shinyServer(function(input, output, session) {
    preds <- reactive({
        preds <- NULL
        if (input$ngram == '2g') {
            priors <- get_prior1(input$text)
            print(priors)
            output$priors <- renderText(priors)
            preds <- pred(priors, df.train.2g)
            print(preds)
        }
        if (input$ngram == '3g') {
            priors <- get_prior2(input$text)
            print(priors)
            output$priors <- renderText(priors)
            preds <- pred(priors, df.train.3g)
            print(preds)
        }
        if (input$ngram == '4g') {
            priors <- get_prior3(input$text)
            print(priors)
            output$priors <- renderText(priors)
            preds <- pred(priors, df.train.4g)
            print(preds)
        }
        if (input$ngram == 'backoff') {
            priors <- get_prior3(input$text)
            print(priors)
            output$priors <- renderText(priors)
            preds <- pred.backoff(priors)
            print(preds)
        }
        
        preds
    })
    
    output$preds <- DT::renderDataTable({
        DT::datatable(preds(), colnames = c('Predictions', 'Log Probability'), options = list(dom = 't'))
    })

    
    ## Render results
   
})

# Data Product
# 
# Does the link lead to a Shiny app with a text input box that is running on shinyapps.io?
# Does the app load to the point where it can accept input?
# When you type a phrase in the input box do you get a prediction of a single word after pressing submit and/or a suitable delay for the model to compute the answer?
# Put five phrases drawn from Twitter or news articles in English leaving out the last word. Did it give a prediction for every one?
# Slide Deck
