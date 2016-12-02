#setwd("~/study/Capstone/R")
library(dplyr)
library(tm)
library(wordcloud)
library(ggplot2)
library(tau)

# remember to replace the data path with your local data path
data_path <- "/Users/anhduc/study/Capstone/data/en_US/" 

source("utility.R")

keep <- 0.1 # only keep 10% data
topics <- c("blogs", "news", "twitter")

data_summary <- data.frame(topic = c("blogs", "news", "twitter"), 
                           total_rows = rep(0, 3),
                           total_words = rep(0, 3),
                           sample_rows = rep(0, 3),
                           sample_words = rep(0, 3))

data_summary$org_file <- paste0(data_path, "en_US.", data_summary[, "topic"], ".txt")
data_summary$sample_file <- paste0(data_path, "sample.en_US.", 
                                      data_summary[, "topic"], ".txt")
data_summary$preprocessed_file <- paste0(data_path, "preprocess.en_US.", 
                                         data_summary[, "topic"], ".txt")
set.seed(10)
# randomly pick 10% data from original dataset for analysis
for (i in 1:nrow(data_summary)) {
  get_smaller_data(data_summary[i, "org_file"], data_summary[i, "sample_file"], keep)
}

# preprocess the sampled data
corpus <- VCorpus(DirSource(directory = data_path, pattern = "^sample")) # read the sampled data
corpus <- tm_map(corpus, content_transformer(tolower)) # convert words into lowercase
# remove profane words
profane_words <- read.csv(file = "en_bad_words.txt", header = FALSE)[, 1] 
f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
corpus <- tm_map(corpus, removeWords, profane_words)

corpus <- tm_map(corpus, f, "[[:punct:]]") # remove punctuation
corpus <- tm_map(corpus, stripWhitespace) # remove white space

# write the corpus to file
writeCorpus(corpus, path = data_path, filenames = gsub("sample", "preprocess",names(corpus)))


# count number of lines in the original dataset and sampled dataset
for (i in 1:nrow(data_summary)) {
  data_summary[i, "total_rows"] <- count_num_lines(data_summary[i, "org_file"])
  data_summary[i, "sample_rows"] <- count_num_lines(data_summary[i, "preprocessed_file"])
}

# count number of words in the original dataset and sampled dataset
for (i in 1:nrow(data_summary)) {
  data_summary[i, "total_words"] <- count_words(data_summary[i, "org_file"])
  data_summary[i, "sample_words"] <- count_words(data_summary[i, "preprocessed_file"])
}

p1 <- ggplot(data_summary, aes(x = topic, y = total_rows)) + geom_bar(stat = "identity") + 
  ylab("total number of rows") +
  ggtitle("# of rows in original dataset")

p2 <- ggplot(data_summary, aes(x = topic, y = sample_rows)) + geom_bar(stat = "identity") + 
  ylab("total number of rows") +
  ggtitle("# of rows in sampled dataset")

p3 <- ggplot(data_summary, aes(x = topic, y = total_words)) + geom_bar(stat = "identity") + 
  ylab("total number of rows") +
  ggtitle("# of words in original dataset")

p4 <- ggplot(data_summary, aes(x = topic, y = sample_words)) + geom_bar(stat = "identity") + 
  ylab("total number of rows") +
  ggtitle("# of words in sampled dataset")

multiplot(p1, p2, p3, p4, cols=2)

# Exploratory Analysis

# word distributions
en_word_dist<-get_ngrams_dist(data_summary$preprocessed_file, n = 1)

# remove all not so popular word (appear less than 0.001% of word distribution)
en_word_dist$percentage <- en_word_dist$freq / sum(en_word_dist$freq)
en_word_dist <- arrange(en_word_dist, desc(percentage))
en_word_dist$cum_percentage <- cumsum(en_word_dist$percentage)

ggplot(en_word_dist[1:which(en_word_dist$cum_percentage > 0.3)[1],], 
       aes(x = ngram, y = percentage)) + geom_bar(stat = "identity") + 
  xlab("Word") + ylab("Percentage") + coord_flip() + 
  ggtitle("Percentage of all the words that cover 30% of word distribution")

wordcloud(as.character(en_word_dist[1:which(en_word_dist$cum_percentage > 0.7)[1],"ngram"]), 
          en_word_dist[1:which(en_word_dist$cum_percentage > 0.7)[1],"freq"], 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# number of words needed to cover certain percentage of word distribution
ggplot(en_word_dist, aes(x = 1:nrow(en_word_dist), y = cum_percentage)) + geom_line() +
  xlab("number of words") + ylab("percentage") + xlim(0, 50000) +
  ggtitle("number of words that need to cover certain percentage of word distribution")

en_2gram_dist<-get_ngrams_dist(data_summary$preprocessed_file, n = 2)

# remove all not so popular word (appear less than 0.001% of word distribution)
en_2gram_dist$percentage <- en_2gram_dist$freq / sum(en_2gram_dist$freq)
en_2gram_dist <- arrange(en_2gram_dist, desc(percentage))
en_2gram_dist$cum_percentage <- cumsum(en_2gram_dist$percentage)

ggplot(en_2gram_dist[1:which(en_2gram_dist$cum_percentage > 0.04)[1],], 
       aes(x = ngram, y = percentage)) + geom_bar(stat = "identity") + 
  xlab("2-gram") + ylab("Percentage") + coord_flip()

wordcloud(en_2gram_dist[1:which(en_2gram_dist$cum_percentage > 0.1)[1],"ngram"], 
          en_2gram_dist[1:which(en_2gram_dist$cum_percentage > 0.1)[1],"freq"], 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# number of words needed to cover certain percentage of distribution
ggplot(filter(en_2gram_dist, cum_percentage < 0.95), 
       aes(x = 1:nrow(filter(en_2gram_dist, cum_percentage < 0.95)), y = cum_percentage)) +
  geom_line() +  xlab("number of 2-grams") + ylab("percentage") + #xlim(0, 50000) +
  ggtitle("number of 2-grams that need to cover certain percentage of distribution")
