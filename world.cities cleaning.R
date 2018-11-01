#install.packages("data.table")
#install.packages("maps")
library(data.table)
library(maps)
data(world.cities) # load reference dataset
# caveat: only cities with population > 40,000
countries = tolower(sort(unique(world.cities$country.etc)))
world.cities$country.etc = tolower(world.cities$country.etc)
world.cities$name = tolower(world.cities$name)
world.cities$name = gsub('[[:punct:]]',world.cities$name, replacement = "")

data = read.csv("data_cleaned.csv")
data = setDT(data)
data = data[,c(1,9)] #extract only name ids and countries 

# Manual cleaning ?? 
    # check stopwords OR ignore if contain stopwords
    stopwords = c("somewhere", "you","anime","place","room","home","hell","earth","house","wonderland","behind","world","bed","really","another","galaxy","rainbow","wifi","near","cave","heart","evil","computerhome","computer","starlight","really","hot","inside","dimension","another","rainbow","over","the","breaker")
    # ignore if contains negation words (NOT)

    # Common shortforms     
    data$country[grep("^uae$",data$country)] = "united arab emirates"
    data$country[grep("united states",data$country)] = "usa"
    data$country[grep("united kingdom",data$country)] = "uk"
    data$country[grep("brasil",data$country)] = "brazil"
    
   

data[,country:=iconv(data$location,"UTF-8","ASCII","")] # check utf-8 characters
  #should we check, replace or?
data$country <- gsub('[[:punct:]]',data$country, replacement = "") #checks punctuation
data$country <- gsub('[[:digit:]]',data$country, replacement = "") #checks numbers
data$country <- tolower(data$country) #change to lower case

# Phase 1 Comparison: Country Level 
# EXACT COUNTRY MATCH 
data = data[,cleaned:=""] # new empty column, initialize with NA
for(c in countries){
  ls = c()
  #ls = grep(paste0("^",c,"$"),data$country) # 18702 exact matches
  ls = grep(c,data$country) # 53494 matches without ^$
  for(i in ls){
    #data[i,"cleaned"] = c
    if(!(data[i,"cleaned"]=="")){
        data[i,"cleaned"] = "check"
    }else{
        data[i,"cleaned"]=c
        } 
  }
}

# clean out similar matches 
data$cleaned[grep("romania",data$country)] = "romania" # oman vs romania
data$cleaned[grep("ukraine",data$country)] = "ukraine"

# Phase 2 Comparison: State Level
    # label cases with repeated city name
    rep_cities = data.frame(table(world.cities$name))
    colnames(rep_cities) = c("Names","Freq")
    rep_list = rep_cities$Names[rep_cities$Freq >1] # list of cities that are repeated
    
    
    subdata = data[data$cleaned == ""]
    states = to.lower(state.name) # by State for USA 
    for(c in states){
        ls = c()
        ls = grep(c,subdata$country)
        for(i in ls){
            if(c %in% rep_list){
                subdata[i,"cleaned"] = "repeat"
            }else{
                #data[i,"cleaned"] = c
                if(!(subdata[i,"cleaned"]=="")){
                    subdata[i,"cleaned"] = "check"
                }else{
                    subdata[i,"cleaned"]="usa"
                } 
            }
        }
    } 
    
    # merge subdata with data 
    #sample = data[1:1000,] #Testing code; not really sure how it works
    UpdateData = function(){
        Cols = names(data)[-1]
        iCols = paste0('i.',Cols)
        setDT(data)[subdata,(Cols):=mget(iCols),on="username"][] #data.table functions
    }
    UpdateData()
    subdata = subdata[subdata$cleaned == ""] #keep only the nulls 
    
    capitals = world.cities[world.cities$capital==1,]
    for(c in capitals$name){
        ls = c()
        ls = grep(c,subdata$country)
        for(i in ls){
            if(c %in% rep_list){
                subdata[i,"cleaned"] = "repeat"
            }else{
                #data[i,"cleaned"] = c
                if(!(subdata[i,"cleaned"]=="")){
                    subdata[i,"cleaned"] = "check"
                }else{
                    subdata[i,"cleaned"]=capitals[capitals$name==c,"country.etc"]
                }
            }
        }
    } 
    
    # This takes forever to run because of the number of countries.
    # Extract "popular" cities first, then run this for more unique and less populated cities
    for(c in world.cities$name[world.cities$pop>mean(world.cities$pop)]){ 
        #take only more populated cities for even greater reduction
        ls = c()
        ls = grep(c,subdata$country)
        for(i in ls){
            if(c %in% rep_list){
                subdata[i,"cleaned"] = "repeat"
            }else{
                #data[i,"cleaned"] = c
                if(!(subdata[i,"cleaned"]=="")){
                    subdata[i,"cleaned"] = "check"
                }else{
                    subdata[i,"cleaned"]=world.cities[world.cities$name==c,"country.etc"]
                }
            }
        }
    } 
    #HOWEVER this lines gives a lot of wrong answers. 
    ####you stopped here. 
    
# For manual checking
subdata = data[data$cleaned==""]
UpdateData()
    # Manual Cleaning of Countries 
    subdata$cleaned[grep("melbourne",subdata$country)] = "australia"
    subdata$cleaned[grep("new south wales",subdata$country)] = "australia"
    subdata$cleaned[grep("sydney",subdata$country)] = "australia"
    subdata$cleaned[grep("toronto",subdata$country)] = "canada"
    subdata$cleaned[grep("istanbul",subdata$country)] = "turkey"
    subdata$cleaned[grep("ho chi minh",subdata$country)] = "vietnam"
    subdata$cleaned[grep("phillipines",subdata$country)] = "philippines"
    subdata$cleaned[grep(" or$",subdata$country)] = "usa" #portland or 
    subdata$cleaned[grep("^america$",subdata$country)] = "usa" #exact america matches only
    subdata$cleaned[grep("galway",subdata$country)] = "ireland"

# Length Checks: 
p = length(data$cleaned[data$cleaned != c("","check","repeat")]) / nrow(data)
p = round(p*100,digits =1)
# Write to CSV 
fwrite(data,paste0("cleaned",p,"_usercountries.csv"))
