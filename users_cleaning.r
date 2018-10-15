library(data.table)

#data = read.csv("users_cleaned.csv")
data = download.file("https://www.kaggle.com/azathoth42/myanimelist/downloads/users_cleaned.csv/9")
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

#extract bingers 
bingers = data[data$user_completed > 2000]

#subset data by columns to look at distributions
eda <- data[,c(3:7,9,14:17,21:23)]

#clean location (remove fake locations)


