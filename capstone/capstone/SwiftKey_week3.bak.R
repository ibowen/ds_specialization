##

library(tm)
Sys.setenv(JAVA_HOME="")
options(java.parameters="-Xmx6g")
library(RWeka)

### Split datasets into train, validation, and test data

set.seed(10000)
# training for 70%, validation for 10%, test for 10%
train_percentage = 0.7
vali_percentage = 0.15
test_percentage = 0.15

for (file in c('twitter', 'blogs', 'news')) {
    lines <- readLines(sprintf('./data/en_US/en_US.%s.txt', file), skipNul = T, encoding = 'UTF-8')
    len <- length(lines)
    
    # 1. train data
    train_size <- train_percentage * len
    train_index <- sample(1:length(lines), train_size)
    train_lines <- lines[train_index]
    saveRDS(train_lines, file = sprintf('./data/en_US/train/en_US.%s_train.rds', file))
    # 2. validation data
    lines <- lines[-train_index]
    vali_size <- vali_percentage * len
    vali_index <- sample(1:length(lines), vali_size)
    vali_lines <- lines[vali_index]
    saveRDS(vali_lines, file = sprintf('./data/en_US/vali/en_US.%s_vali.rds', file))
    # 3. test data
    lines <- lines[-vali_index]
    test_size <- test_percentage * len
    test_index <- sample(1:length(lines), test_size)
    test_lines <- lines[test_index]
    saveRDS(test_lines, file = sprintf('./data/en_US/test/en_US.%s_test.rds', file))
}

### Define functions

# function to tranform one line list into document term frequency
clean_corpus <- function(corpus) {
    # to lower case
    corpus <- tm_map(corpus, content_transformer(tolower))
    # remove punctuation
    corpus <- tm_map(corpus, removePunctuation)
    # remove numbers
    corpus <- tm_map(corpus, removeNumbers)
    # strip whitespace among words
    corpus <- tm_map(corpus, stripWhitespace)
    corpus
}


### Train data preprocessing, transform text into freq dataframe

# transform train text into corpus
corpus.train <- VCorpus(DirSource(directory="./data/en_US/train", encoding="UTF-8"), readerControl=list(language="en"))

# clean corpus
corpus.train <- clean_corpus(corpus.train)

# transform corpus into tdm
tokenizer.1g <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tokenizer.2g <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tokenizer.3g <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tokenizer.4g <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

# Sets the default number of threads to use
#options(mc.cores=1)
tdm.train.1g <- TermDocumentMatrix(corpus.train, control = list(tokenize = tokenizer.1g, wordLengths = c(1, Inf)))
tdm.train.2g <- TermDocumentMatrix(corpus.train, control = list(tokenize = tokenizer.2g))
tdm.train.3g <- TermDocumentMatrix(corpus.train, control = list(tokenize = tokenizer.3g))
tdm.train.4g <- TermDocumentMatrix(corpus.train, control = list(tokenize = tokenizer.4g))

tdm.train.1g
tdm.train.2g
tdm.train.3g
tdm.train.4g

# save tdf
saveRDS(tdm.train.1g, file = './data/en_US/tdm/train.1g.tdm')
saveRDS(tdm.train.2g, file = './data/en_US/tdm/train.2g.tdm')
saveRDS(tdm.train.3g, file = './data/en_US/tdm/train.3g.tdm')
saveRDS(tdm.train.4g, file = './data/en_US/tdm/train.4g.tdm')

inspect(tdm.train.1g[1:20, 1:3])
inspect(tdm.train.2g[1:20, 1:3])
inspect(tdm.train.3g[1:20, 1:3])
inspect(tdm.train.4g[1:20, 1:3])

### calculate conditional probalibity based on tdm

# functions to get priors and posterior
get_1of2 <- function(bigram) {
    bigram <- as.character(bigram)
    unlist(strsplit(bigram, ' '))[1]
}

get_12of3 <- function(trigram) {
    trigram <- as.character(trigram)
    split_words <- unlist(strsplit(trigram, ' '))
    paste(split_words[1], split_words[2], sep=' ')
}

get_123of4 <- function(quargram) {
    quargram <- as.character(quargram)
    words <- unlist(strsplit(quargram, ' '))
    paste(words[1], words[2], words[3], sep=' ')
}

get_posterior <- function(ngram) {
    ngram <- as.character(ngram)
    words = unlist(strsplit(ngram, ' '))
    words[length(words)]
}

#library(parallel)
# unigram part
tdm.train.1g <- as.matrix(tdm.train.1g)
tdm.train.1g <- rowSums(tdm.train.1g)
tdm.train.1g <- sort(tdm.train.1g, decreasing = TRUE)
head(tdm.train.1g)

df.train.1g <- data.frame(term = names(tdm.train.1g), freq.term = tdm.train.1g)
# total word occurance
freq.sum.1g <- sum(df.train.1g$freq.term)
# total unique words
term.sum.1g <- nrow(df.train.1g)
# maxiumu likelyhood estimate
#df.train.1g$mle <- log(df.train.1g$freq.term / freq.sum.1g)
# laplace smoothing
#df.train.1g$mle_laplace <- log((df.train.1g$freq.term + 1) / (freq.sum.1g + term.sum.1g))
head(df.train.1g)
saveRDS(df.train.1g, file = './data/en_US/df/train.1g.df')
gc()

# bigram part
tdm.train.2g <- as.matrix(tdm.train.2g)
tdm.train.2g <- rowSums(tdm.train.2g)
tdm.train.2g <- sort(tdm.train.2g, decreasing = TRUE)
head(tdm.train.2g)

df.train.2g <- data.frame(term = names(tdm.train.2g), freq.term = tdm.train.2g)
df.train.2g$prior <- sapply(df.train.2g$term, get_1of2)
#df.train.2g$freq.prior <- mclapply(df.train.2g$prior, FUN = function(x) (tdm.train.1g[x]), mc.cores = 6L)
#df.train.2g$freq.prior <- sapply(df.train.2g$freq.prior, FUN = as.vector)

# total bigram occurence
freq.sum.2g <- sum(df.train.2g$freq.term)
# total unique bigrams
term.sum.2g <- nrow(df.train.2g)

# maxiumu likelyhood estimate
#df.train.2g$mle <- log(df.train.2g$freq.term / df.train.2g$freq.prior)
# laplace smoothing
#df.train.2g$mle_laplace <- log((df.train.2g$freq.term + 1) / (df.train.2g$freq.prior + term.sum.1g))

head(df.train.2g)
saveRDS(df.train.1g, file = './data/en_US/df/train.2g.df')
gc()

# trigram part
tdm.train.3g <- as.matrix(tdm.train.3g)
tdm.train.3g <- rowSums(tdm.train.3g)
tdm.train.3g <- sort(tdm.train.3g, decreasing = TRUE)
head(tdm.train.3g)

df.train.3g <- data.frame(term = names(tdm.train.3g), freq.term = tdm.train.3g)

df.train.3g$prior <- sapply(df.train.3g$term, get_12of3)
#df.train.3g$freq.prior <- mclapply(df.train.3g$prior, FUN = function(x) (tdm.train.2g[x]), mc.cores = getOption("mc.cores"))

# total trigram occurence
freq.sum.3g <- sum(df.train.3g$freq.term)
# total unique trigrams
term.sum.3g <- nrow(df.train.3g)

# maxiumu likelyhood estimate
# df.train.3g$mle <- log(df.train.3g$freq.term / df.train.3g$freq.prior)
# laplace smoothing
# df.train.3g$mle_laplace <- log((df.train.3g$freq.term + 1) / (df.train.3g$freq.prior + term.sum.2g))

head(df.train.3g)
saveRDS(df.train.1g, file = './data/en_US/df/train.3g.df')
gc()

# quargram part
tdm.train.4g <- as.matrix(tdm.train.4g)
tdm.train.4g <- rowSums(tdm.train.4g)
tdm.train.1g <- sort(tdm.train.4g, decreasing = TRUE)
head(tdm.train.4g)

df.train.4g <- data.frame(term = names(tdm.train.4g), freq.term = tdm.train.4g)

#df.train.4g$prior <- sapply(df.train.4g$term, get_123of4)
#df.train.4g$freq.prior <- sapply(df.train.4g$prior, FUN = function(x) (tdm.train.3g[x]))

# total quargram occurence
freq.sum.4g <- sum(df.train.4g$freq.term)
# total unique trigrams
term.sum.4g <- nrow(df.train.4g)

# maxiumu likelyhood estimate
#df.train.4g$mle <- log(df.train.4g$freq.term / df.train.4g$freq.prior)
# laplace smoothing
#df.train.4g$mle_laplace <- log((df.train.4g$freq.term + 1) / (df.train.4g$freq.prior + term.sum.3g))

head(df.train.4g)
saveRDS(df.train.1g, file = './data/en_US/df/train.4g.df')
gc()

### Validation data preprocessing, transform text into freq dataframe

### Test data preprocessing, transform text into freq dataframe

### Model prediction using dataframe
train_1g_df <- readRDS('./data/en_US/en_US.train_1g_df.rds')
train_2g_df <- readRDS('./data/en_US/en_US.train_2g_df.rds')
train_3g_df <- readRDS('./data/en_US/en_US.train_3g_df.rds')

vali_1g_df <- readRDS('./data/en_US/en_US.vali_1g_df.rds')
vali_2g_df <- readRDS('./data/en_US/en_US.vali_2g_df.rds')
vali_3g_df <- readRDS('./data/en_US/en_US.vali_3g_df.rds')

# top 5 word for unigram
train_1g_df$word <- rownames(train_1g_df)
train_1g_df <- train_1g_df[order(train_1g_df$count, decreasing = T), ]
train_1g_top5 <- train_1g_df[1:5,]

train_2g_df$word <- rownames(train_2g_df)
train_3g_df$word <- rownames(train_3g_df)

vali_2g_df$word <- rownames(vali_2g_df)
vali_3g_df$word <- rownames(vali_3g_df)

vali_2g_df_sample <- vali_2g_df[1:10000,]
vali_3g_df_sample <- vali_3g_df[1:10000,]


# bigram prediction
# lapply return a list, sapply return a vector
train_2g_df$one <- sapply(train_2g_df$word, get_1of2)
train_2g_df$two <- sapply(train_2g_df$word, get_2of2)

vali_2g_df_sample$one <- sapply(vali_2g_df_sample$word, get_1of2)
vali_2g_df_sample$two <- sapply(vali_2g_df_sample$word, get_2of2)

# function to predict bigram
pred_2g <- function(predictor) {
    arrange(filter(train_2g_df, one == predictor), desc(count))[1, 'two']
}

ptm <- proc.time()
vali_2g_df_sample$pred <- sapply(vali_2g_df_sample$one, pred_2g)
print(proc.time() - ptm)

length(vali_2g_df_sample[vali_2g_df_sample$pred == vali_2g_df_sample$two,])
arrange(vali_2g_df_sample, two == pred)

# trigram prediction
train_3g_df$onetwo <- sapply(train_3g_df$word, get_12of3)
train_3g_df$two <- sapply(train_3g_df$word, get_2of3)
train_3g_df$three <- sapply(train_3g_df$word, get_3of3)

vali_3g_df_sample$onetwo <- sapply(vali_3g_df_sample$word, get_12of3)
vali_3g_df_sample$two <- sapply(vali_3g_df_sample$word, get_2of3)
vali_3g_df_sample$three <- sapply(vali_3g_df_sample$word, get_3of3)

pred_3g <- function(predictor) {
    arrange(filter(train_3g_df, onetwo == predictor), desc(count))[1, 'three']
}


ptm <- proc.time()
vali_3g_df_sample$pred <- sapply(vali_3g_df_sample$onetwo, pred_3g)
print(proc.time() - ptm)

length(vali_3g_df_sample[vali_3g_df_sample$pred == vali_3g_df_sample$three,])

# backoff
pred_backoff <- function(onetwo) {
    pred <- pred_3g(onetwo)
    if (is.null(pred)) {
        two <- unlist(strsplit(onetwo, ' '))[2]
        pred <- pred_2g(two)
    }
    if (is.null(pred)) {
        pred <- 'the'
    }
    pred
}

vali_3g_df_sample$pred <- sapply(vali_3g_df_sample$onetwo, pred_backoff)
length(vali_3g_df_sample[vali_3g_df_sample$pred == vali_3g_df_sample$three,])

# Questions to consider:
# 1. How does the model perform for different choices of the parameters and size of the model?
# 2. How much does the model slow down for the performance you gain?
# 3. Does perplexity correlate with the other measures of accuracy?
# 4. Can you reduce the size of the model (number of parameters) without reducing performance?

# # convert to String for NLP
# twitter_train <- as.String(twitter_train)
# # create annotators for words and sentences
# sent_annotator <- Maxent_Sent_Token_Annotator()
# word_annotator <- Maxent_Word_Token_Annotator()
# # annotate the text
# ptm <- proc.time()
# twitter_train_ann <- annotate(twitter_train, list(sent_annotator, word_annotator))

