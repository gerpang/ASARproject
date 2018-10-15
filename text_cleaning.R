##testing the tm package for cleaning the location column
# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

countries = read.csv("data_static.csv") #read data of all countries from local file 
countries = countries[,1] #extract only the 1st col of country names 

docs <- Corpus(VectorSource(countries))
inspect(docs)

#test
corp2 <- tm_filter(corp,FUN=function(x) any(grep(countries,corp)))

#to remove as df
dataframe <- data.frame(text=sapply(corp, identity), stringsAsFactors=F)