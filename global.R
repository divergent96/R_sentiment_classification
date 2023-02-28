
set.seed(0723983)
# Retain result consistency

library(formattable)
library(scales) #

# Import relevant shiny libraries
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)

library(tidyverse) # Standard R package for data frame and other common functionality
library(stringr) # String processing library
library(stopwords) # Stopword library to remove unneeded connector terms from text
library(textstem) # Stemmer and lemmetizer library. We will use lemmetizer to retain meaning of words
library(text2vec) # library for easy to implement word embeddings
library(qdapDictionaries) # Filter sensible words from corpus for easy vocab
library(DT) # Data table library
library(wordcloud2) # Library to make wordclouds
library(RColorBrewer) # Color customization library
library(tidytext)

# Our main model for training and predictions
library(xgboost)

# Import custom function created for cleaning text
source('functions/text_processor.R') 
source('functions/custom_wordcloud2.R') 
source('functions/performance_metrics.R') 


# Custom input lists

text_processor_list <- c("Document frequency","Term Frequency","N-grams: 2 words")

############################################
# Process corpus based on inputs provided
# Old modules that didnot work out

# source('server/s_input.R', local = TRUE)
# source('server/s_text_vectorizer.R', local = TRUE)
# source('server/s_wordcloud.R', local = TRUE)


