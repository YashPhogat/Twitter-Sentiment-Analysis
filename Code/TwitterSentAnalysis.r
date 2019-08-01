library(twitteR) 
library(ROAuth) 
library(tm) 
library(wordcloud)

key="***" 
secret="***"
authenticate <- OAuthFactory$new(
consumerKey=key, consumerSecret=secret,
requestURL="https://api.twitter.com/oauth/request_token", accessURL="https://api.twitter.com/oauth/access_token", authURL=“https://api.twitter.com/oauth/authorize")
setup_twitter_oauth(key, secret)

#Get tweets

tweets <- searchTwitter("#Algorithms", n=500, lang="en")
#Get text from tweets
tweets.text <- sapply(tweets, function(x) x$getText())


#Remove non graphic characters
tweets.text <- gsub("[^[:graph:]]", " ", tweets.text)
#Convert all text to lower case tweets.text <- tolower(tweets.text)
#Replace blank space (“rt”)
tweets.text <- gsub("rt", "", tweets.text)
#Replace @UserName
tweets.text <- gsub("@\\w+", "", tweets.text)
#Remove punctuation
tweets.text <- gsub("[[:punct:]]", "", tweets.text)
#Remove links
tweets.text <- gsub("http\\w+", "", tweets.text)
#Remove tabs
tweets.text <- gsub("[ |\t]{2,}", "", tweets.text)
#Remove blank spaces at the beginning 
tweets.text <- gsub("^ ", "", tweets.text)
#Remove blank spaces at the end 
tweets.text <- gsub(" $", "", tweets.text)

#Create corpus
tweets.text.corpus <- Corpus(VectorSource(tweets.text))
#Remove stopwords
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))

#Get matrix
dtm <- as.matrix(DocumentTermMatrix(tweets.text.corpus))
#Get frequency
frequency <- colSums(dtm)
frequency <- sort(frequency, decreasing=TRUE)
#Get words
words <- names(frequency)

wordcloud(words[1:100], frequency[1:100])

#Get negative and positive word lists 
p_list <- readLines("positive.txt") 
n_list <- readLines("negative.txt")
 
#Get positive and negative words 
words.positive <- intersect(words, p_list) 
words.negative <- intersect(words, n_list)


barplot(frequency[words.positive]) 
barplot(frequency[words.negative])


#Get positive, negative and neutral scores 
score.positive <- sum(frequency[words.positive]) 
score.negative <- sum(frequency[words.negative])
score.neutral <- sum(frequency) - score.positive - score.negative

#Plot scores
barplot(c(score.negative, score.neutral, score.positive)) 
barplot(c(score.negative, score.positive))

#Get final score
score <- score.positive - score.negative 
score.positive
score.neutral 
score.negative score



