#install.packages("users.table")
library(data.table)

users = read.csv("users_cleaned.csv")
#users = download.file("https://www.kaggle.com/azathoth42/myanimelist/downloads/users_cleaned.csv/9")
users = setDT(users)

#remove access_rank (all NA's)
users$access_rank = NULL
#remove user ids - not found in other tables. 
users$user_id = NULL 

#round days_spent_watching with ceiling(), create a new col called user_days
users[,user_days:=ceiling(user_days_spent_watching)]

#remove time from birth date, join date and last online
users[,birthdate:=as.Date(birth_date)]
users[,joindate:=as.Date(join_date)]
users[,lastdate:=as.Date(last_online)]
#convert to date

#convert dates into age, days_since_joined and days_since_last column 
users[,age:=Sys.Date()-birthdate] #in days
users[,age:=floor(age/365.25)] #convert to years 
users[,days_since_joined:=Sys.Date() - joindate]
users[,days_since_last:=Sys.Date() - lastdate]
#convert difftime to num
users[,age:=as.numeric(age)]
users[,days_since_joined:=as.numeric(days_since_joined)]
users[,days_since_last:=as.numeric(days_since_last)]

# #extract bingers
# bingers = users[users$user_completed > 2000]
# 
# #visualizing some users 
# qplot(users$age, 
#       geom="histogram", 
#       main= "Distribution of Users' Age (years)", 
#       xlab="Age(years)", 
#       ylab="Frequency",
#       binwidth=0.5)

# #clean LOCATION into country (remove fake locations)
# countries = read.csv("users_static.csv") #read users of all countries 
# countries = countries[,1] #extract only the 1st col of country names 

# #create new column of country
# users[,country:=iconv(users$location,"UTF-8","ASCII","")] # remove utf-8 characters 
#   #should we remove, replace or? 
# users$country <- gsub('[[:punct:]]',users$country, replacement = "") #removes punctuation
# users$country <- gsub('[[:digit:]]',users$country, replacement = "") #removes numbers
# users$country <- tolower(users$country) #change to lower case

#remove stopwords
#etc CAN BE DONE WITH TM (see wordcloud.R)

#tokenize ? 
# which(users$country[users$country %like% "brazil"]) = "brazil" #option 1

# # next:
# #compare users$country with cleaned reference list of countries (Kevin)
# read.csv("countries.csv",stringsAsFactors = FALSE)
# countries = tolower(countries[,1])
# countries[66] = "Korea" #instead of Democratic Republic of Korea
# #same for congo, laos

#shortening countries that are too long because I want to make n-grams later
# countries[243] = "macedonia"
# countries[266] ="venezuela"
# countries[259] = "united kingdom"
# countries[281] = "wales"
# countries[282] = "england"
# countries[283] = "scotand"
# countries[284] = "northern ireland"
# countries[285] = "britain"
#to convert all these to britain later 
#because countries[259] = "united kingdom of great britain and northern ireland"
#change to countries[259] = "united kingdom" 

#sample[lapply(sample,grep, pattern=countries, value=TRUE)] #test with sample: Returns all NA. 
##function to compare if user-submitted country name is:
### in reference list 
### a state (appears to be very common) (extractcities.r > subcountry column)
### a city (see extractcities.R for the extraction of reference list; now how to use it?)

#lapply(extracted[1:6,],agrep, pattern = c$country[i], value=TRUE)

##Match the other way around:
# if the user input a country that is in the reference list, use that country. 

