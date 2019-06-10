# Prep
#list of packages required
list.of.packages <- c("shiny","shinydashboard","shinydashboardPlus","data.table","dplyr","RColorBrewer","leaflet","rgdal","maps","plotly","scales")
#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
library(shiny)
library(data.table)
library(dplyr)
library(rgdal)
library(RColorBrewer)
library(leaflet)
library(maps)
library(plotly)
library(scales)

# Data initialization
data <- fread("sample_finaldata.csv", stringsAsFactors = FALSE)
data = setDT(data)
users = fread("sample_users.csv")
users = setDT(users)
anime = unique(data[,29:50,by=animeid])
ctry_map = readOGR("./maps/temp", "country_mappings")

mybins=c(0,1,5,20,100,500,Inf) #change to colorQuantile ? 
mypalette = colorBin(palette="YlGn", domain=ctry_map@data$Count, na.color="transparent", bins=mybins)
mytext=paste("Country: ", ctry_map@data$NAME,"<br/>", "Viewer Count: ", ctry_map@data$Count, "<br/>", sep=""
            ) %>% lapply(htmltools::HTML)
## Pre-processing to add Viewer Freq to .shp file's dataframe 
# users_country_freq = unique(data[,.(ISO_A2,country),by=username])[,.N,by=.(country,ISO_A2)] 
# ctry_map@data = data.frame(ctry_map@data, aggr_users_by_country[match(ctry_map@data$ISO_A2, aggr_users_by_country$ISO_A2),])
#ctry_map@data[which(is.na(ctry_map@data$Count)),75:77] = 0
# writeOGR(ctry_map,"temp","country_mappings","ESRI Shapefile")

shinyServer(function(input, output) {
    
    #dashboard
    output$anime_count <- renderValueBox({
        valueBox(
            value = nrow(anime),
            subtitle = "Anime Shows",
            icon = icon("tv"),
            color = "green"
        )
    })
    output$users_count <- renderValueBox({
                    valueBox(
                            value = nrow(users),
                            subtitle = "Unique Users",
                            icon = icon("users"),
                            color = "light-blue"
                            )
                    })
    
    # output$anime_views <- renderValueBox({
    #     valueBox(
    #         value = nrow(data),
    #         subtitle = "View Count",
    #         icon = icon("mouse-pointer"),
    #         color = "gray"
    #     )
    # })
    
    output$anime_pie <-renderPlotly({ 
        plot_ly(anime,
                labels=~get(input$pie_var_choose),
                #values=count(anime),
                type="pie",
                text=~get(input$pie_var_choose),
                marker=list(colors=brewer.pal(12,"Set3"),line=list(color = '#FFFFFF', width = 1)),
                showlegend=FALSE
                )
    }) 

    output$top_anime <- renderTable({
        anime[head(order(rank),10),1:2] #can change the rank to other criteria, eg popularity/members 
    },align ="c",striped=TRUE,hover=TRUE)
    
    #5 point analysis
  output$analysis_5p_sum <- DT::renderDataTable({
   do.call(rbind,
           lapply(users[,as.numeric(input$analysis_5p_var_choose),with=FALSE], base::summary,digits=4)
           ) %>% 
          data.table(keep.rownames = FALSE) 
    },options = list(dom='t', ordering=F))
    
  # output$analysis_5p_sum <-renderTable({
  #     as.array(summary(users[,as.numeric(input$analysis_5p_var_choose),with=FALSE]),dimnames=NULL)
  # },
  # colnames=FALSE)
  
  # boxplot
  
  output$analysis_hist <- renderPlot({
      ggplot(users,
             aes(x=users[,as.numeric(input$analysis_5p_var_choose)])) +
          geom_bar(color="#428BCA",fill="#428BCA") +
          labs(x=names(users)[as.numeric(input$analysis_5p_var_choose)])
  }) # extra hist - code error! remove ? 
  output$analysis_5p_bp <- renderPlot({
    column_num_5p <- as.numeric(input$analysis_5p_var_choose)
    x<-users[,..column_num_5p]
    boxplot(x,
            col="#428BCA",
            border="black",
            main=names(users[,as.numeric(input$analysis_5p_var_choose)]),
            horizontal = TRUE)
  })
  
  # output$users_gender <- renderPlotly({
  #     plot_ly(users,
  #             labels=~gender,
  #             values=count(users),
  #             type="pie",
  #             text=~gender,
  #             # title = paste0("Users by ",var_choice),
  #             marker=list(colors=brewer.pal(3,"Set3"),line=list(color = '#FFFFFF', width = 1)),
  #             showlegend=FALSE)
  # # qplot(users$gender, users$age, 
  # #           geom=c("jitter","boxplot"), 
  # #           fill=users$gender,
  # #           xlab="Gender",
  # #           ylab="Age", 
  # #           main = "Age of Users by Gender") + 
  # #         theme(legend.title = element_blank()) +
  # #         scale_fill_brewer(palette="Blues")
  # }) 
  
  #geographic distribution
  output$geo_dist_map <- renderLeaflet({
    isolate(leaflet(ctry_map) %>% 
      addTiles() %>% 
      setView(lng = 1,
              lat=1,
              zoom=2
      ) %>% 
      addPolygons(data = ctry_map,
                  fillColor = ~mypalette(Count),
                  stroke= FALSE,
                  fillOpacity = 0.9,
                  color = "white",
                  weight = 0.3,
                  label = mytext,
                  labelOptions = labelOptions(
                    direction="auto"
                  )
      ) %>%
      addLegend(pal = mypalette,
                values = ~Count,
                opacity = 0.8,
                title = "Viewer Count",
                position = "bottomleft"
      ))
  })
  
  output$geo_dist_country_input <- renderUI({
      ctry <- unique(data[,country])
      ctry <- sort(ctry,decreasing = FALSE)
      tagList(selectInput("geo_dist_country_filter","Country: ", choices = c("All",ctry), selected = "All"))
  })
  
  output$geo_dist_user_table <- renderDataTable({
      #input$geo_dist_country_filter
      #input$geo_dist_user_aggr_col
      #input$geo_dist_top_n
      
      country_filter <- as.character(input$geo_dist_country_filter)
      user_aggr_col <- as.character(input$geo_dist_user_aggr_col) # assign
      top_n_x <- as.numeric(input$geo_dist_top_n)
      age_filter <- input$geo_dist_age
      top_x <- "title"
      if(country_filter == "All"){
          aggr <- data
      }
      else{
          aggr <- data[country==country_filter,]
      }
      aggr <- aggr[age >= age_filter[1]]
      aggr <- aggr[age <= age_filter[2]]
      aggr <- aggr[,lapply(.SD,function(x) mean(x)), by = "username", .SDcols = user_aggr_col]
      setorderv(aggr, user_aggr_col, -1)
      aggr <- head(aggr,top_n_x)
      top_n_users <- data[username %in% unique(aggr[,username])]
      setDT(top_n_users)
      aggr <- top_n_users[,c("username", "user_completed","gender","age","stats_episodes","days_since_joined","days_since_last")]
      aggr <- unique(aggr) 
      aggr
  })
  
  
  
  output$bar_analysis_bar <- renderPlot({
      #trialscore = ggplot(filtered, aes(title_english,score))
      #trialscore + stat_summary(fun.y = mean, geom = "bar", fill = "White", color = "Black")
      #bar_x <- "title_english"
      #bar_y <- x
      
      #aggregate(data[,score],list(data[,title]), mean)
      #head(setorder(aggregate(data[,score],list(data[,title]), mean), -x),10)[,"Group.1"]
      #aggr <- aggregate(data[,c("score"), with=FALSE],data[,c("title"), with=FALSE], mean)    Aggregate
      #setorder(aggr, -"score")    # Top descending score
      #head(aggr,n)   #Top n
      
      anime_aggr_col <- as.character("popularity") # assign
      top_n_x <- as.numeric(input$analysis_bar_top_n)
      top_y <- input$analysis_bar_top_y
      top_x <- "title"
      
      if(top_x == "username"){
          aggr <- data[,lapply(.SD,function(x) mean(x)), by = "username", .SDcols = anime_aggr_col]
      }
      else if(top_x == "title"){
          aggr <- data[,lapply(.SD,function(x) mean(x)), by = "title", .SDcols = anime_aggr_col]
          if(anime_aggr_col == "popularity"){
              setorderv(aggr, anime_aggr_col)
          }
          else if(anime_aggr_col == "rank"){
              setorderv(aggr, anime_aggr_col)
          }
          else{
              setorderv(aggr, anime_aggr_col,-1)
          }
      }
      aggr <- head(aggr, top_n_x)
      top_n_anime <- data[title %in% unique(aggr[,title])]
      aggr <- top_n_anime[,lapply(.SD,function(x) mean(x)), by = top_x, .SDcols = top_y]
      setorderv(aggr, top_y, -1)
      aggr <- copy(aggr)
      #plot
      plot_test <- ggplot(aggr, aes_string(x=paste("reorder(",top_x,",",top_y ,")"), y=top_y)) + geom_col(fill="#0dc5c1") + coord_flip() + xlab("Anime Series") +ggtitle(paste0("Top ",top_n_x, " Most Popular anime series, Sorted by ", top_y))
      plot_test
      
  })
  
  
  output$bar_analysis_tbl <- renderDataTable({
      anime_aggr_col <- as.character("popularity") # assign
      top_n_x <- as.numeric(input$analysis_bar_top_n)
      top_y <- input$analysis_bar_top_y
      top_x <- "title"
      
      if(top_x == "username"){
          aggr <- data[,lapply(.SD,function(x) mean(x)), by = "username", .SDcols = anime_aggr_col]
      }
      else if(top_x == "title"){
          aggr <- data[,lapply(.SD,function(x) mean(x)), by = "title", .SDcols = anime_aggr_col]
          if(anime_aggr_col == "popularity"){
              setorderv(aggr, anime_aggr_col)
          }
          else if(anime_aggr_col == "rank"){
              setorderv(aggr, anime_aggr_col)
          }
          else{
              setorderv(aggr, anime_aggr_col,-1)
          }
      }
      aggr <- head(aggr, top_n_x)
      top_n_anime <- data[title %in% unique(aggr[,title])]
      top_n_anime <- unique(top_n_anime[,29:50])
      top_n_anime[, c("title", "title_english", "aired_from_year", "genre", "score", "members", "favorites")]
  })
  
  #predict
  #lm1 <- reactive({lm(reformulate(input$IndVar, input$DepVar), data = data)})
  #regression part
  reg_mod_indvar <- reactive({input$IndVar})
  lm1 <- reactive({lm(reformulate(input$IndVar, input$DepVar), data = anime)})
  output$DepPrint <- renderPrint({input$DepVar})
  output$IndPrint <- renderPrint({input$IndVar})
  output$RegSum <- renderPrint({summary(lm1())})
  output$reg_mod_x_choice <- renderUI({
      tagList(selectInput("reg_mod_x_plot", "Independent Variable", choices = reg_mod_indvar()))
  })
  output$indvsdep <- renderPlot({
      ggplot(anime, aes_string(x=input$reg_mod_x_plot, y=input$DepVar)) + geom_point() + stat_smooth(method="lm",col="red")
  })
  output$residualPlot <- renderPlot({
      d <- anime
      d$predicted <- predict(lm1())
      d$residuals <- residuals(lm1())
      residual_plot <- resid(lm1())
      ggplot(d, aes_string(x = input$reg_mod_x_plot, y = input$DepVar)) +
          geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
          geom_segment(aes_string(xend = input$reg_mod_x_plot, yend = input$DepVar), alpha = .2) +
          
          # > Color AND size adjustments made here...
          geom_point(aes(color = abs(residuals), size = abs(residuals))) + # size also mapped
          scale_color_continuous(low = "black", high = "red") +
          guides(color = FALSE, size = FALSE) +  # Size legend also removed
          # <
          
          geom_point(aes(y = predicted), shape = 1) +
          theme_bw()
  })
  output$ui_anime_predict_indvsdep <- renderUI({
      if(length(reg_mod_indvar()) == 0){
          #h2("Please Input Independent Variable!")
      }
      else{
          plotOutput("indvsdep")
      }
  })
  output$ui_anime_predict_resplot <- renderUI({
      if(length(reg_mod_indvar()) == 0){
          #h2("Please Input Independent Variable!")
      }
      else{
          plotOutput("residualPlot")
          
      }
  })
  output$ui_anime_predict_verbatim <- renderUI({
      if(length(reg_mod_indvar()) == 0){
          helpText("Please Select an Independent Variable!")
      }
      else{
          verbatimTextOutput("RegSum")
          
      }
  })
  
  user_DepVar <- "user_days"
  #regression 2
  user_reg_mod_indvar <- reactive({input$user_IndVar})
  lm2 <- reactive({lm(reformulate(input$user_IndVar, user_DepVar), data = users)})
  output$user_DepPrint <- renderPrint({user_DepVar})
  output$user_IndPrint <- renderPrint({input$user_IndVar})
  output$user_RegSum <- renderPrint({summary(lm2())})
  output$user_reg_mod_x_choice <- renderUI({
      tagList(selectInput("user_reg_mod_x_plot", "Independent Variable", choices = user_reg_mod_indvar()))
  })
  output$user_indvsdep <- renderPlot({
      ggplot(users, aes_string(x=input$user_reg_mod_x_plot, y=user_DepVar)) + geom_point() + stat_smooth(method="lm",col="red")
  })
  output$user_residualPlot <- renderPlot({
      d <- users
      d$predicted <- predict(lm2())
      d$residuals <- residuals(lm2())
      residual_plot <- resid(lm2())
      ggplot(d, aes_string(x = input$user_reg_mod_x_plot, y = user_DepVar)) +
          geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
          geom_segment(aes_string(xend = input$user_reg_mod_x_plot, yend = user_DepVar), alpha = .2) +
          
          # > Color AND size adjustments made here...
          geom_point(aes(color = abs(residuals), size = abs(residuals))) + # size also mapped
          scale_color_continuous(low = "black", high = "red") +
          guides(color = FALSE, size = FALSE) +  # Size legend also removed
          # <
          
          geom_point(aes(y = predicted), shape = 1) +
          theme_bw()
  })
  output$ui_user_predict_indvsdep <- renderUI({
      if(length(user_reg_mod_indvar()) == 0){
          #h2("Please Input Independent Variable!")
      }
      else{
          plotOutput("user_indvsdep")
      }
  })
  output$ui_user_predict_resplot <- renderUI({
      if(length(user_reg_mod_indvar()) == 0){
          #h2("Please Input Independent Variable!")
      }
      else{
          plotOutput("user_residualPlot")
          
      }
  })
  output$ui_user_predict_verbatim <- renderUI({
      if(length(user_reg_mod_indvar()) == 0){
          helpText("Please Select an Independent Variable!")
      }
      else{
          verbatimTextOutput("user_RegSum")
          
      }
  })
 
})
