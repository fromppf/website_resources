#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#    http://shiny.rstudio.com/gallery/plot-interaction-advanced.html 
#


library("highcharter")
library(data.table)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(reshape2)
library(networkD3)
library(dygraphs)
#library('visNetwork') 
library(plotly)
library(leaflet)

setwd("C:/website_resources/websiteDashboard")

my_data <- read.csv("Data/simulated_data.csv", stringsAsFactors = FALSE, strip.white=TRUE)
assign("my_data", my_data, envir = .GlobalEnv)

initial_years <- function(){
  opt <- c("All",sort(unique(my_data[['Year_Reference']])))
  opt
}

years_inp <- initial_years()
years_inp <- as.numeric(years_inp[years_inp!='All'])

initial_group <- function(){
  opt <- c('All groups','Male and Female','International and National')
  opt
}


sidebar <- dashboardSidebar(

  
  sidebarMenu(
    
    menuItem("Copyright", tabName = "authors_info", icon=icon("copyright")),
    menuItem("Data sample", tabName = "sample_tab", icon=icon("group"),
             collapsible= 
               menuSubItem("Default2", tabName = "not_used2", icon=icon('filter')),
               menuSubItem("Groups", tabName = "groups_sample", icon=icon('pie-chart')),
               menuSubItem("Features", tabName = "features_sample", icon=icon('search'))
    ),
    menuItem("Data mining", tabName = "yearly_tab", icon= icon("gears"),
             collapsible= 
             menuSubItem("Default2", tabName = "not_used2", icon=icon('filter')),
             menuSubItem("Academics", tabName = "Score_selection", icon=icon('area-chart')),
             menuSubItem("Location", tabName = "Location_selection", icon=icon('map-marker')),
             menuSubItem("Qualifications", tabName = "Skills_selection", icon=icon('star'))
             #,
             #menuSubItem("Education", tabName = "Education_selection", icon=icon('graduation-cap'))
    )
    #,
    #menuItem("Predictive Models", tabName = "prediction_tab", icon=icon("line-chart"),
    #         collapsible= 
    #         menuSubItem("Default2", tabName = "not_used2", icon=icon('filter')),
    #         menuSubItem("Correlations", tabName = "correlations_predictive", icon=icon('spinner'))
    #)
  )
)




body <-   dashboardBody(
  
  #tags$head(
  #  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  #),
  
  tags$head(tags$style(HTML('
      .box-body {
        font-family: "Georgia", Times, "Times New Roman", serif;
                            font-size: 14px;
      }
      .box-header .box-title, .box-header>.fa, .box-header>.glyphicon, .box-header>.ion {
        font-family: "Georgia", Times, "Times New Roman", serif;
                            font-weight: bold;
                            font-size: 18px;
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 16px;
      }
      .main-sidebar .sidebar .sidebar-menu a:hover{
                          background-color: rgb(135,206,280);
                          color: rgb(255,255,255);
                          font-weight: bold; font-size: 14px;
      }
      .main-sidebar .sidebar .sidebar-menu a{
                            font-weight: bold; font-size: 14px;
      }
    '))),
  
  
  tabItems(
  
      
    tabItem(tabName =  "Score_selection",
            fluidRow(
              box(title = "Group Selection",  
                  background = "black",
                  selectInput("class_sel", "Select years to analyze: ", initial_years()),
                  radioButtons("group_sel", "Select group to analyze: ",initial_group() ),
                  #uiOutput("year_my_sel"),
                  textOutput("text_year_my_sel"),
                  width = 12
              ),
              box( mainPanel( 
                     tabsetPanel(
                       tabPanel("Score",
                                plotlyOutput("plotSCORE1",width="800px",height ="400px" ))
                       ,
                       tabPanel("By Degree",
                                plotlyOutput("plotDegree1",width="800px",height ="400px" )),
                       tabPanel("By Major", 
                                plotlyOutput("plotMajor1",width="800px",height ="400px" ))
                       #,
                       #tabPanel("By GMAT verbal", 
                       #       plotlyOutput("plotGMATverbal1",width="700px",height ="350px" ),height="auto")
                       )
                     ),
                   mainPanel( 
                     tabsetPanel(
                       tabPanel("Score",
                                plotlyOutput("plotSCORE2",width="800px",height ="400px" ))
                       ,
                       tabPanel("By Degree",
                                plotlyOutput("plotDegree2",width="800px",height ="400px" )),
                       tabPanel("By Major", 
                                plotlyOutput("plotMajor2",width="800px",height ="400px" ))
                       #,
                       #tabPanel("By GMAT verbal", 
                       #       plotlyOutput("plotGMATverbal2",width="700px",height ="350px" ),height="auto")
                      )
                   ),
                   width =  12
              )
            )
            
            #infoBox(
            #  title = "Important", 
            #  value = tags$p(style = "font-size: 16px;","These selection is required for next steps!") ,  
            #  icon = icon("warning")
            #)
    ),
    
    tabItem(tabName =  "Location_selection",
            fluidRow(
              box(title = "Group Selection",  
                  background = "black",
                  selectInput("class_sel_location", "Select years to analyze: ", initial_years()),
                  radioButtons("group_sel_location", "Select group to analyze: ",initial_group() ),
                  textOutput("text_year_my_sel_location"),
                  width = 12
              ),
              box( 
              mainPanel( 
                  tabsetPanel(
                    tabPanel("By Country", 
                              plotOutput("peopleMap1", width="900px",height ="450px"),height="auto"),
                    tabPanel( "List of countries",
                             div(DT::dataTableOutput("table_countries_1"),style="font-size: 85%; width: 100%" )),
                    tabPanel( "Other map-demo: LAST EARTHQUAKES",
                              leafletOutput("demoEarthquakes", width="900px",height ="450px" ))
                  ),
                  tabsetPanel(
                    tabPanel("By Country / State", 
                             plotOutput("peopleMap2", width="900px",height ="450px"),height="auto"),
                    tabPanel( "List of countries/states",
                              div(DT::dataTableOutput("table_countries_2"),style="font-size: 85%; width: 100%" ))
                  )
              ),
              width =  12
              )
            )
    ),
    
    tabItem(tabName =  "Skills_selection",
        fluidRow(
              box(title = "Group Selection", 
                  background = "black",
                  selectInput("class_sel_skills", "Select years to analyze: ", initial_years()),
                  radioButtons("group_sel_skills", "Select group to analyze: ",initial_group()),
                  #radioButtons("abilities_sel_skills", "Select qualifications: ",choices = list("Skills" = 'skill', "Languages" = 'language', "Interests"='interest'), selected = 'skill'),
                  radioButtons("abilities_sel_skills", "Select qualifications: ",choices = list("Skills" = 'skill', "Languages" = 'language'), selected = 'skill'),
                  textOutput("text_year_my_sel_skills"),
                  width = 12
              ),
              box(
                plotlyOutput("skillsByIndustry1",width="500px",height ="500px"), 
                width =  6
              ),
              box(
                plotlyOutput("skillsByIndustry2",width="500px",height ="500px"), 
                width =  6
              ),
              width =  12
        )
    ),
    
    tabItem(tabName =  "Education_selection",
            fluidRow(
              box(title = "Group Selection",  
                  background = "black",
                  selectInput("class_sel_education", "Select years to analyze: ", initial_years()),
                  radioButtons("group_sel_education", "Select group to analyze: ",initial_group() ),
                  textOutput("text_year_my_sel_education"),
                  width = 12
              ),
              box(
                    mainPanel( 
                      tabsetPanel(
                        tabPanel("By Degree",
                                 plotlyOutput("plotDegree1other",width="900px",height ="600px" )),
                        tabPanel("By Major", 
                                 plotlyOutput("plotMajor1other",width="900px",height ="600px" ))
                        #,
                        #tabPanel("By School", 
                        #         plotlyOutput("plotSchool1",width="900px",height ="600px" ))
                      )
                    ),
                    mainPanel( 
                      tabsetPanel(
                        tabPanel("By Degree",
                                 plotlyOutput("plotDegree2other",width="900px",height ="600px" )),
                        tabPanel("By Major", 
                                 plotlyOutput("plotMajor2other",width="900px",height ="600px" ))
                        #,
                        #tabPanel("By School", 
                        #         plotlyOutput("plotSchool2",width="900px",height ="600px" ))
                       )
                    ),
              width = 12
              ),
              width =  12
            )
    ),

    
    tabItem(tabName =  "groups_sample",
            fluidRow(
              box(title = "Description of the sample",
                  background = "black",
                  textOutput("text_my_groups_sample"),
                  width = 12
              ),
              box(
                title = "Male (M) and Female (F)",
                status = "primary",
                width = 3,
                solidHeader = FALSE,
                collapsible = FALSE,
                plotlyOutput("plotGroup1_sample",height = 200)
              ),
              box(
                title = "International (1) and National (0)",
                status = "primary",
                width = 3,
                solidHeader = FALSE,
                collapsible = FALSE,
                plotlyOutput("plotGroup2_sample",height = 200)
              ),
              box(
                title = "Industries",
                status = "primary",
                width = 6,
                solidHeader = FALSE,
                collapsible = FALSE,
                plotlyOutput("plotIndustries_sample",height = 200)
              ),
              box(
                title = "Gender per Year",
                status = "primary",
                width = 10,
                solidHeader = FALSE,
                collapsible = FALSE,
                plotlyOutput("plotYearsGender_sample",height = 250)
              ),
              box(
                title = "Nationality per Year",
                status = "primary",
                width = 10,
                solidHeader = FALSE,
                collapsible = FALSE,
                plotlyOutput("plotYearsInternational_sample",height = 250)
              ),
              box(
                title = "Industries per Year",
                status = "primary",
                width = 10,
                solidHeader = FALSE,
                collapsible = FALSE,
                plotlyOutput("plotYearsIndustry_sample",height = 250)
              )
            )
    ),
    
    tabItem(tabName =  "features_sample",
            fluidRow(
              box(title = "Select one feature",
                  background = "black",
                  uiOutput("feature_sel_sample"),
                  textInput("keyword_sel_sample", "or write a keyword to find in all features/descriptions:", ""),
                  textOutput("text_my_feature_sample"),
                  textOutput("error_my_feature_sample"),
                  width = 12
              ),
              box(title = "Feature Information",
                           div(DT::dataTableOutput("table_feature_"),style="font-size: 85%; width: 100%" ),
                  width =  12
              ),
              box(
                  tabsetPanel(
                    tabPanel("Individual Information",
                             plotlyOutput("plotFeatureIndividual",width="700px",height ="400px" ))
                  ),
                width = 12
              )
            )
    ),
    
    tabItem(tabName =  "correlations_predictive",
            fluidRow(
              box(title = "Correlations",  
                  background = "black",
                  textOutput("text_my_correlations_predictive"),
                  width = 12
              ),
              box(
                status = "primary",
                width = 10,
                solidHeader = FALSE,
                collapsible = FALSE,
                plotlyOutput("plotCorrelations",height = 750)
              ),
              width = 12
              )
    ),
    
    
    tabItem(tabName = "authors_info",
            box(title ="Project demo - profiles",
                background = "black",
                h3("M.S. Data Science"),
                h4 ("Maria L. Zamora Maass -", a("mailto:mzm239@nyu.edu")), 
                width = 9, height = 250
            )
            #imageOutput("image_nyu") 
    )
    
    
  )
)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title =  "Profiles demo"),
  sidebar,
  body
  
)
