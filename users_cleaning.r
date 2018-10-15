#install.packages("data.table")
library(data.table)

data = read.csv("users_cleaned.csv")
#data = download.file("https://www.kaggle.com/azathoth42/myanimelist/downloads/users_cleaned.csv/9")
data = setDT(data)
str(data)
View(data)

#remove access_rank (all NA's)
data$access_rank = NULL

#round days_spent_watching with ceiling(), create a new col called user_days
data[,user_days:=ceiling(user_days_spent_watching)]

#remove time from birth date, join date and last online
data[,birthdate:=as.Date(birth_date)]
data[,joindate:=as.Date(join_date)]
data[,lastdate:=as.Date(last_online)]
#convert to date

#convert dates into age, days_since_joined and days_since_last column 
data[,age:=Sys.Date()-birthdate] #in days
data[,age:=floor(age/365.25)] #convert to years 
data[,days_since_joined:=Sys.Date() - joindate]
data[,days_since_last:=Sys.Date() - lastdate]
#convert difftime to num
data[,age:=as.numeric(age)]
data[,days_since_joined:=as.numeric(days_since_joined)]
data[,days_since_last:=as.numeric(days_since_last)]

#extract bingers 
bingers = data[data$user_completed > 2000]

#visualizing some data 
qplot(data$age, 
      geom="histogram", 
      main= "Distribution of Users' Age (years)", 
      xlab="Age(years)", 
      ylab="Frequency",
      binwidth=0.5)

#clean LOCATION into country (remove fake locations)
countries = read.csv("data_static.csv") #read data of all countries 
countries = countries[,1] #extract only the 1st col of country names 

#create new column of country
data[,country:=iconv(data$location,"UTF-8","ASCII","")] # remove utf-8 characters 
data$country <- gsub('[[:punct:]]',data$country, replacement = "") #removes punctuation
data$country <- gsub('[[:digit:]]',data$country, replacement = "") #removes numbers
data$country <- tolower(data$country) #change to lower case

#########################stopped here 
# next:
#compare data$country with cleaned reference list of countries (Kevin)
#sample[lapply(sample,grep, pattern=countries, value=TRUE)] #test with sample: Returns all NA. 
##function to compare if user-submitted country name is:
### in reference list 
### a state in us (appears to be very common)


#ignore below: text mining
#test with sample dataset first 
sample = data$location

#or using corpus: 
#corp <- Corpus(VectorSource(data$location))

#make all lowercase
countries = tolower(countries) #sample ref
sample = tolower(sample)

tm_map()

#remove punctuation 
#remove whitespace? 
#remove stopwords 
#lapply grep 

