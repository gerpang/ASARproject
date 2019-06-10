#Prep
#list of packages required
list.of.packages <- c("shiny","shinydashboard","shinydashboardPlus","data.table","dplyr","RColorBrewer","leaflet","rgdal","maps","shinycssloaders","plotly","scales")
#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(data.table)
library(dplyr)
library(rgdal)
library(RColorBrewer)
library(leaflet)
library(maps)
library(shinycssloaders)
library(plotly)

# Define UI
shinyUI(
   dashboardPagePlus(enable_preloader = TRUE,skin = "black",
    header = dashboardHeaderPlus(title = "MyAnimeList",enable_rightsidebar = FALSE),
    sidebar = dashboardSidebar(
      sidebarMenu(menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                  menuItem("Popular Anime Explorer", tabName = "analysis_bar", icon = icon("fire")),
                  menuItem("User Demographics", tabName = "analysis_users", icon = icon("globe")),
                  menuItem("Regression", tabName = "analysis_predict", icon = icon("question"))
                  )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard", header = headerPanel("Hello Shiny!"),
                fluidRow(valueBoxOutput("users_count",width=6),
                        valueBoxOutput("anime_count",width=6)
                        # ,
                        # valueBoxOutput("anime_views")
                        ),
                fluidRow(
                    box(title="Users Info",
                        status = "primary",
                        width=6,
                           selectInput(inputId = "analysis_5p_var_choose",
                                       label="View User Distribution by:",
                                       choices=c("Age"="15", #indexing columns not as ideal as using colnames
                                                 "Days Spent Watching"="11", 
                                                 "Days Since Last Online"="17",
                                                 "Number of Episodes Watched" = "10"
                                                 ),
                                       selectize=FALSE
                                       ),
                           DT::dataTableOutput("analysis_5p_sum") %>% withSpinner(color="#0dc5c1"),
                           plotOutput("analysis_5p_bp")
                           ),
                    box(title="Anime Info",
                        status = "success",
                        width=6,
                           selectInput(inputId = "pie_var_choose",
                                       label="View Anime Categories by:",
                                       choices= c("Type"="type", #index 3
                                                 "Source"="source", #4
                                                 "Rating"="rating"),#10
                                       selectize=FALSE
                                       ),
                           plotlyOutput("anime_pie")  %>% withSpinner(color="#0dc5c1")
                           )
                    )
        ),
        
        tabItem(tabName = "analysis_users",
                fluidRow(box(width=3,
                             title = "MyAnimeList Users",
                             solidHeader = TRUE , #background = "teal",
                             helpText("Find Other Anime Lovers of similar demographics!"),
                             uiOutput("geo_dist_country_input"),
                             sliderInput("geo_dist_age", "Age Range", 10, 50, value = c(20, 30)),
                             sliderInput("geo_dist_top_n", "Top" , 10, 50, 10),
                             selectInput("geo_dist_user_aggr_col", "By", choices = c("Completed Anime" = "user_completed", "Total Watched Episodes" = "stats_episodes", "Mean Score for Animes" = "stats_mean_score", "N. Anime Currently Watching" = "user_watching", "N. Anime On-Hold" = "user_onhold", "N. Anime To Watch Later" = "user_plantowatch", "Days Since Joined" = "days_since_joined"))
                             
                         ),
                        column(width=9,
                               leafletOutput("geo_dist_map") %>% withSpinner(color="#0dc5c1")
                        )
                     ),
                     dataTableOutput("geo_dist_user_table")
        ),
        
        tabItem(tabName = "analysis_bar",
                sidebarLayout(
                    sidebarPanel(
                        h2("Anime Series Explorer"),
                        helpText("Learn from the cool kids, explore what makes the most popular anime!"),
                        sliderInput("analysis_bar_top_n", "Top" , 10, 50, 10),
                        selectInput("analysis_bar_top_y", "showing ", choices = c("Score"="score", "Number of Eps" = "episodes", "Members" = "members", "Favorites"="favorites"))
                        
                    ),
                    mainPanel(
                        plotOutput("bar_analysis_bar")
                    )
                ),
                dataTableOutput("bar_analysis_tbl")
        ),
        
        tabItem(tabName = "analysis_predict",
                tabBox(width = 12,title="Regression Model", id="analysis_predict_tab", 
                       tabPanel("Anime",div(style = 'overflow-x: hidden;overflow-y:scroll;height:800px;',
                                            p("What makes an anime popular?"),
                                            selectInput(inputId = "DepVar", 
                                                        label = "Dependent Variables: How do I define popularity?", 
                                                        choices = list("score", "popularity", "members"), 
                                                        selected="score", 
                                                        multiple=FALSE),
                                            selectizeInput(inputId = "IndVar", 
                                                           label = "Independent Variables: Factors Affecting Popularity", 
                                                           choices = list("score", "source", "scored_by", "aired_from_year", "favorites", "members", "episodes", "duration_min", "studio"), 
                                                           multiple=TRUE),
                                            fluidRow(
                                                column(5, boxPlus(collapsible = TRUE,closable = FALSE,solidHeader = TRUE, color = "purple", status="primary",title="Regression Output",width = 12,uiOutput("ui_anime_predict_verbatim"))),
                                                column(7, boxPlus(collapsible = TRUE, closable = FALSE, solidHeader = TRUE, color = "purple",status = "info",title="Dependent vs Selected Independent Variable",width = 12,
                                                               fluidRow(
                                                                   #column(6,
                                                                   
                                                                   #plotOutput("indvsdep"),
                                                                   column(12, uiOutput("ui_anime_predict_indvsdep"))
                                                                   #column(6,plotOutput("residualPlot"))
                                                                   #column(6, uiOutput("ui_anime_predict_resplot"))
                                                               ),
                                                               footer=uiOutput("reg_mod_x_choice"))
                                                )
                                            )
                       )
                       ),
                       tabPanel("User",
                                div(style = 'overflow-x: hidden;overflow-y:scroll;height:800px;',
                                    p("What gets users to spend more time watching anime?"),
                                    selectizeInput(inputId = "user_IndVar", label = "Independent Variables", choices = list("age", "gender", "stats_mean_score", "stats_episodes", "country", "user_watching", "days_since_joined"), selected="episodes", multiple=TRUE),
                                    fluidRow(
                                        column(5, boxPlus(collapsible = TRUE,closable = FALSE,solidHeader = TRUE, color = "purple", status="primary",title="Regression Output",width = 12,uiOutput("ui_user_predict_verbatim"))),
                                        column(7, boxPlus(collapsible = TRUE, closable = FALSE, solidHeader = TRUE, color = "purple",status = "info",title="Dependent vs Selected Independent Variable",width = 12,
                                                       fluidRow(
                                                           column(12, uiOutput("ui_user_predict_indvsdep"))
                                                           #column(6,plotOutput("residualPlot"))
                                                           #column(6, uiOutput("ui_user_predict_resplot"))
                                                           #column(6,plotOutput("user_indvsdep")),
                                                           #column(6,plotOutput("user_residualPlot"))
                                                       ),
                                                       footer=uiOutput("user_reg_mod_x_choice"))
                                        )
                                    )
                                )
                       )
                )
        )
      )
    )
  )
)
  