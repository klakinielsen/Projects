
library(dplyr)
require(magrittr)
library(tm)
library(ggplot2)
library(stringr)
library(NLP)
library(openNLP)
library(jsonlite)

require(jsonlite)

data <- stream_in(file('C:\\Users\\niels\\OneDrive\\NYU\\Business Analytics\\Assignment8\\PatioLawnGarden.json.gz'))
data2 <- select(data, reviewText, reviewerName)

reviews <- data2$reviewText

#Cleaning corpus
stopwords <- stopwords("SMART")
## additional junk words showing up in the data
stopwwords <- c(stopwords, "will", "like", "can", "make", "also", "go","put", 
                "seem", "can", "come", "two", "thing")
stopwords <- tolower(stopwords)

#Additional housekeeping
reviews <- gsub("'", "", reviews) # remove apostrophes
reviews <- gsub("[[:punct:]]", " ", reviews)  # replace punctuation with space
reviews <- gsub("[[:cntrl:]]", " ", reviews)  # replace control characters with space
reviews <- gsub("^[[:space:]]+", "", reviews) # remove whitespace at beginning of documents
reviews <- gsub("[[:space:]]+$", "", reviews) # remove whitespace at end of documents
reviews <- gsub("[^a-zA-Z -]", " ", reviews) # allows only letters
reviews <- tolower(reviews)  # force to lowercase

## get rid of blank docs
reviews <- reviews[reviews != ""]

# tokenize on space and output as a list:
doclist <- strsplit(reviews, "[[:space:]]+")

# compute the table of terms:
termtable <- table(unlist(doclist))
termtable <- sort(termtable, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 10 times:
del <- names(termtable) %in% stopwords | termtable < 10
termtable <- termtable[!del]
termtable <- termtable[names(termtable) != ""]
vocab <- names(termtable)

# now put the documents into the format required by the lda package:
getterms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doclist, getterms)

#############
# Compute some statistics related to the data set:
numberofdocs <- length(documents)  # number of documents (13258)
numberofterms <- length(vocab)  # number of terms in the vocab (6819)
doclength <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [24, 12, 74, 11, 49, 23, ...]
N <- sum(doclength)  # total number of tokens in the data (770574)
termfrequency <- as.integer(termtable)

# MCMC and model tuning parameters:
K <- 15
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()

## display runtime
t2 - t1  

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

dp <- dim(phi)
dt <- dim(theta)

reviews_for_LDA <- list(phi = phi,
                     theta = theta,
                     doclength = doclength,
                     vocab = vocab,
                     termfrequency = termfrequency)

library(LDAvis)
library(servr)

# create the JSON object to feed the visualization:
json <- createJSON(phi = reviews_for_LDA$phi, 
                   theta = reviews_for_LDA$theta, 
                   doc.length = reviews_for_LDA$doclength, 
                   vocab = reviews_for_LDA$vocab, 
                   term.frequency = reviews_for_LDA$termfrequency)

serVis(json, out.dir = 'visualization', open.browser = TRUE, as.gist = TRUE)
