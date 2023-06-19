#--------Performing Sentiment Analysis on Elon Musk's Twitter Data------------#

#Importing libraries 
library(readxl)
library(tm) #for text mining and preprocessing
library(wordcloud)
library(stringr)
library(ggplot2) 
library(RColorBrewer)
library(syuzhet)

#Importing dataset
dataset <- read.csv("D:/downloads/Sentiment Analysis on Elon Musk's Twitter Data using R/sentiment_analysis.csv")
head(dataset) 

#-----------------------------------------------------------------------------#

#Data Cleaning
corpus <- Corpus(VectorSource(dataset))
class(corpus) 

# Convert text to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)

# Remove numbers
corpus <- tm_map(corpus, removeNumbers)

# Remove URLs
corpus <- tm_map(corpus, content_transformer(function(x) gsub("http\\S+|www\\S+", "", x)))

# Remove white spaces and new lines
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\s+|\\n", " ", x)))

# Remove emojis
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to='ASCII', sub='')))

# Remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Perform stemming
corpus <- tm_map(corpus, stemDocument)

content(corpus[[2]]) #Checking the corpus 

#-----------------------------------------------------------------------------#

# Convert the preprocessed corpus back to a character vector for the sentiment analysis
preprocessed_documents <- sapply(corpus, as.character)
class(preprocessed_documents)
length(preprocessed_documents)

mysentiment_tesla<-get_nrc_sentiment((preprocessed_documents))
Sentimentscores_tesla<-data.frame(colSums(mysentiment_tesla[,]))

names(Sentimentscores_tesla) <- "Score"
Sentimentscores_tesla<-cbind("sentiment" = rownames(Sentimentscores_tesla), Sentimentscores_tesla)
rownames(Sentimentscores_tesla) <- NULL

ggplot(data=Sentimentscores_tesla,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people for Elon Musk")

#-----------------------------------------------------------------------------#
library(textclean)
library(syuzhet)

wordcloud_data = dataset

# Adding the new columns for sentiment scores of each tweet 
wordcloud_data$senti_scores <- get_sentiment(wordcloud_data$Text)

#Labeling of Positive and Negative Tweets
wordcloud_data$pos_scores <- ifelse(wordcloud_data$senti_scores >= 0, "positive", "negative")

#Separating the Positive and Negative Tweets 
positive_tweets <- data.frame(wordcloud_data[wordcloud_data$pos_scores == 'positive', ])
negative_tweets <- data.frame(wordcloud_data[wordcloud_data$pos_scores == 'negative', ])

#Data Cleaning

#Converting the dataframe to corpus for text mining
pos_data <- as.data.frame(positive_tweets)
neg_data <- as.data.frame(negative_tweets)
class(pos_data)
class(neg_data)

positive_corpus <- Corpus(VectorSource(pos_data$Text))
negative_corpus <- Corpus(VectorSource(neg_data$Text))

preprocess_corpus <- function(corpus) {
  # Convert text to lowercase
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  
  # Remove numbers
  corpus <- tm_map(corpus, removeNumbers)
  
  # Remove URLs
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("http\\S+|www\\S+", "", x)))
  
  # Remove white spaces and new lines
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\s+|\\n", " ", x)))
  
  # Remove emojis
  corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to='ASCII', sub='')))
  
  # Remove stopwords
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  # Perform stemming
  corpus <- tm_map(corpus, stemDocument)
  
  return(corpus)
}

#Utilizing the function to perform the data cleaning
preprocessed_pos_tweets <- preprocess_corpus(positive_corpus)
content(preprocessed_pos_tweets)

preprocessed_neg_tweets <- preprocess_corpus(negative_corpus)
content(preprocessed_neg_tweets)

## Convert the preprocessed corpus back to a character vector for the sentiment analysis
pos_tweets <- sapply(preprocessed_pos_tweets, as.character)
neg_tweets <- sapply(preprocessed_neg_tweets, as.character)
class(pos_tweets)
class(neg_tweets)

#Plotting the Word Cloud for Positive Tweets
set.seed(1234)
wordcloud(words =pos_tweets, max.words=100, random.order=FALSE,  
          rot.per=0.50, use.r.layout=TRUE, colors = brewer.pal(8,"Dark2"))

set.seed(1234)
wordcloud(words =neg_tweets, max.words=100, random.order=FALSE,  
          rot.per=0.50, use.r.layout=TRUE, colors = brewer.pal(8,"Dark2"))



