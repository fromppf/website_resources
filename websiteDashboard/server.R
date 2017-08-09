#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny) #for shiny elements
library(shinydashboard) #To get the template 
library(DT) #Output tables in shiny
library(dplyr) #Helps manage dataframes
library(reshape) 
library(ggplot2) 
library(networkD3)
library(gplots)
library("gtable")
library(xts) #dynamic plotting
library(dygraphs) #dynamic plotting
library("Matrix")
library(ggfortify)
library("nnet") # for multinom
library("igraph") #For network graphs
library("plyr") #Similar to dplyr, to manage dataframes

#library("sna") #For centrality measures in the network
#library('visNetwork') 


library(plotly)
library(rworldmap)
library(raster)
library(leaflet)


########################################   INPUT   people    ##################################



get_initial_datasets <- function(year=2011){
  
  setwd("C:/website_resources/websiteDashboard")
  
  
  my_dictionary <- read.csv("Data/simulated_descriptive_summary.csv", stringsAsFactors = FALSE, strip.white=TRUE)
  my_data <- read.csv("Data/simulated_data.csv", stringsAsFactors = FALSE,  na.strings=c("", "NA"),strip.white=TRUE)
  my_data$Year_Reference <- as.factor(as.integer(round(my_data$Year_Reference,0) ) )
  
  
  my_group_names <- c('All','Male and Female','International and National')
  
  print(head(my_data$Year_Reference))
  assign("my_dictionary", my_dictionary, envir = .GlobalEnv)
  assign("my_data", my_data, envir = .GlobalEnv)
  assign("my_group_names",my_group_names, envir = .GlobalEnv)
  
  
  library(data.table)
  urlfile <- 'http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.csv'
  my_earthquakes <- fread(urlfile)
  assign("my_earthquakes ", my_earthquakes , envir = .GlobalEnv)
}

get_initial_datasets(2011)



##########################################  FILTER DATA  ##########################################

get_filter_group <- function(subdata_specific, name_group, group_number){
  # Gender Filter is option 2
  if (name_group==my_group_names[2] && group_number==1)
  { subdata_specific <- subdata_specific[ subdata_specific[c('male_gender')]==1, ] }
  if (name_group==my_group_names[2] && group_number==2)
  { subdata_specific <- subdata_specific[ subdata_specific[c('male_gender')]==0, ] }    
  # Interntional Filter is option 3
  if (name_group==my_group_names[3] && group_number==1)
  { subdata_specific <- subdata_specific[ subdata_specific[c('international')]==1, ] }
  if (name_group==my_group_names[3] && group_number==2)
  { subdata_specific <- subdata_specific[ subdata_specific[c('international')]==0, ] }
  return(subdata_specific)
}

get_filter_year <- function(subdata_specific,year_group){
  if (year_group != "All"){
    subdata_specific <- subdata_specific[ subdata_specific[c('Year_Reference')]==year_group, ]
  }
  return(subdata_specific)
}


get_filter_variable <- function(x_year, x_group, g_number, name_of_variable){
  variable <- name_of_variable
  my_data_specific <- get_filter_year(my_data,x_year)
  my_data_specific <- get_filter_group(my_data_specific,x_group,g_number)
  my_data_specific <- my_data_specific[c("ProfileType","IDREF",variable)]
  my_data_specific <- my_data_specific[my_data_specific[c(variable)] != 0,]
  return(my_data_specific)
}

get_filter_states <- function(x_year, x_group, g_number){
  variableCountry <- "citizen_cntry"
  variableState <- "previous_state"
  my_data_specific <- get_filter_year(my_data,x_year)
  my_data_specific <- get_filter_group(my_data_specific,x_group,g_number)
  my_data_specific <- my_data_specific[c("ProfileType","IDREF",variableCountry, variableState)]
  return(my_data_specific)
}

get_filter_skills <- function(x_year, x_group, g_number){
  my_data_specific <- get_filter_year(my_data,x_year)
  my_data_specific <- get_filter_group(my_data_specific,x_group,g_number)
  return(my_data_specific)
}

get_filter_education <- function(x_year, x_group, g_number,type_education){
  if (type_education == 'degree')
  { variable <- "previous_degree"} 
  else if (type_education == 'major')
  { variable <- "major"}
  else 
    { variable <- "school" }
  my_data_specific <- get_filter_year(my_data,x_year)
  my_data_specific <- get_filter_group(my_data_specific,x_group,g_number)
  my_data_specific <- my_data_specific[c("ProfileType","IDREF",variable)]
  return(my_data_specific)
}







shinyServer(function(input, output) {

  
  
  
  ######################################### GROUPS SELECTION ############################################
  
  output$text_my_groups_sample <- renderText({ 
    paste("The data set consists of 24 features describing over 1,960 people from 2011 to 2015." )
  })
  
  output$plotIndustries_sample <- renderPlotly({
          agg_data <- aggregate(IDREF ~ ProfileType, data = my_data, FUN = length)
          plot_agg <- get_group_plot(agg_data)
          ggplotly(plot_agg)
    })  
  
  output$plotGroup1_sample <- renderPlotly({
      agg_data <- aggregate(IDREF ~ male_gender, data = my_data, FUN = length)
      plot_agg <- get_group_plot(agg_data)
      ggplotly(plot_agg)
  })
  
  output$plotGroup2_sample <- renderPlotly({
    agg_data <- aggregate(IDREF ~ international, data = my_data, FUN = length)
    plot_agg <- get_group_plot(agg_data)
    ggplotly(plot_agg)
  })
  
  
  get_group_plot <- function(aggregated_data){
    colnames(aggregated_data) <- c("variable_i","people")
    aggregated_data$people.FREQ <- aggregated_data$people/sum(aggregated_data$people)
    aggregated_data <- aggregated_data[order(aggregated_data$variable_i, decreasing = TRUE),]
    # aggregated_data  <- aggregated_data [1:10,]  # Top 10 values
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    plotResult <- plot_ly(aggregated_data, labels = ~variable_i, values = ~people.FREQ,
                          textposition = 'inside', textinfo = 'percent',
                          insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                          text = ~paste('Total people:', people),showlegend = TRUE) %>%
      add_pie(hole = 0.3)  %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 

    return(plotResult)
  }

  
  output$plotYearsGender_sample <- renderPlotly({
    aggregated_data <- plyr::ddply(my_data,.(Year_Reference, male_gender ),summarize, count = length(IDREF) )
    industry_data <- plyr::ddply(my_data,.(Year_Reference),summarize, count_industry = length(IDREF) )
    summary_data <- join(aggregated_data,industry_data,by='Year_Reference' )
    summary_data$Frequency <- summary_data$count/summary_data$count_industry
    
    summary_data$gender_m <- factor(summary_data$male_gender ,levels=c(0,1),labels=c("Male","Female"))
    summary_data$Year_Reference <- as.character( summary_data$Year_Reference )
    
    p <- plot_ly(source = "source") %>% 
      add_lines(data = summary_data, x = ~Year_Reference, y = ~Frequency, 
                color = ~male_gender, mode = "lines", line = list(width = 3))
    p <- p %>%
      layout(title = "Distribution of gender per Year",
             xaxis = list(title = "Years", gridcolor = "#bfbfbf"),
             yaxis = list(title = "people's frequency", gridcolor = "#bfbfbf"), 
             plot_bgcolor = "#F5F1DA")
    ggplotly(p)
  })
  
  
  output$plotYearsInternational_sample <- renderPlotly({
    aggregated_data <- plyr::ddply(my_data,.(Year_Reference, international ),summarize, count = length(IDREF) )
    industry_data <- plyr::ddply(my_data,.(Year_Reference),summarize, count_industry = length(IDREF) )
    summary_data <- join(aggregated_data,industry_data,by='Year_Reference' )
    summary_data$Frequency <- summary_data$count/summary_data$count_industry
    
    summary_data$international <- factor(summary_data$international,levels=c(0,1),labels=c("National","International"))
    
    p <- plot_ly(source = "source") %>% 
      add_lines(data = summary_data, x = ~Year_Reference, y = ~Frequency, 
                color = ~international, mode = "lines", line = list(width = 3))
    p <- p %>%
      layout(title = "Distribution of international status per Year",
             xaxis = list(title = "Years", gridcolor = "#bfbfbf"),
             yaxis = list(title = "people's frequency", gridcolor = "#bfbfbf"), 
             plot_bgcolor = "#F5F1DA")
    ggplotly(p)
  })
  

  output$plotYearsIndustry_sample <- renderPlotly({
    aggregated_data <- plyr::ddply(my_data,.(Year_Reference,ProfileType),summarize, count = length(IDREF) )
    profile_data <- plyr::ddply(my_data,.(Year_Reference),summarize, count_type = length(IDREF) )
    summary_data <- join(aggregated_data,profile_data,by='Year_Reference' )
    summary_data$Frequency <- summary_data$count/summary_data$count_type
    p <- plot_ly(source = "source") %>% 
        add_lines(data = summary_data, x = ~Year_Reference, y = ~Frequency, 
                color = ~ProfileType, mode = "lines", line = list(width = 3))
    p <- p %>%
        layout(title = "Distribution of industries per Year",
               xaxis = list(title = "Years", gridcolor = "#bfbfbf"),
               yaxis = list(title = "people's frequency", gridcolor = "#bfbfbf"), 
               plot_bgcolor = "#F5F1DA")
    ggplotly(p)
  })
  
  
  
  
  ######################################### FEATURES SELECTION ############################################
  
  
  output$error_my_feature_sample <- renderText({ 
    if (input$keyword_sel_sample != "") {
      feature_data <- as.data.frame(my_dictionary[grep(paste("^",input$keyword_sel_sample,sep=''), my_dictionary$Feature),])
      if (length(rownames(feature_data))==0){ 
      paste("No information available for: ", input$keyword_sel_sample)
      }
    }
  })
  
  
  output$feature_sel_sample = renderUI({
    sel <- sort(unique(my_dictionary$Feature))
    sel <- sel[!grepl(pattern = 'objective',sel)]   
    # filter of some of the variables that include objective in their names'
    sel <- sel[!grepl(pattern = 'interest',sel)]
    selectInput("feature_sel_sample", "Select the feature: ", sel)
  })    

  
  output$table_feature_ <- renderDataTable({
    if (length(input$feature_sel_sample)>0){ 
      if (input$keyword_sel_sample != "") {
        list_index_grep <- grepl(paste(input$keyword_sel_sample,sep=''),my_dictionary$Feature,ignore.case=TRUE)|grepl(paste(input$keyword_sel_sample,sep=''), my_dictionary$Description,ignore.case=TRUE)
        feature_data <- as.data.frame(my_dictionary[list_index_grep,])    }
      else {
        feature_selected <- input$feature_sel_sample
        feature_data <- as.data.frame(my_dictionary[my_dictionary$Feature == feature_selected ,])  }
      if (length(rownames(feature_data))>0){  
          df_f <- as.data.frame(feature_data$Feature)
          colnames(df_f) <- 'Feature'
          df_f$Distinct <- feature_data$distinct
          if (!is.na(feature_data$Description)) { df_f$Description <- feature_data$Description }  
          if (!is.na(feature_data$min)) { df_f$Min <- feature_data$min }
          if (!is.na(feature_data$max)) { df_f$Max <- feature_data$max }
          if (!is.na(feature_data$median)) { df_f$Median <- feature_data$median }
          if (!is.na(feature_data$mean)) { df_f$Mean <- feature_data$mean }
          if (!is.na(feature_data$std)) { df_f$Std_Dev <- feature_data$std }
          DT::datatable(df_f,options = list(paging = FALSE, searching= FALSE, autoWidth= TRUE))
      }
    }
  })
  
  output$plotFeatureIndividual <- renderPlotly({
        variable_selected <- sub(' ', ".", input$feature_sel_sample)
        variable_selected <- sub(' ', ".", variable_selected)
        variable_selected <- sub('/', ".", variable_selected)
        variable_selected <- sub('-', ".", variable_selected)
        variable_selected <- sub("\\(", ".", variable_selected )
        variable_selected <- sub("\\)", ".", variable_selected )
        print (variable_selected)
        get_individual_features_plot(variable_selected)
  })
  
  get_individual_features_plot <- function(variable_individual){
      filtered_data <- my_data[c('IDREF',variable_individual)]
      if (length(rownames(filtered_data))>0){
        colnames(filtered_data) <- c('IDREF','variable')
        filtered_data <-   filtered_data  %>%
                            group_by(variable) %>%
                            dplyr::summarise(count = n_distinct(IDREF))
        filtered_data$percentage <-  round(filtered_data$count / length(my_data[,c('IDREF')]),3)
        filtered_data$variable[is.na(filtered_data$variable)] <- "Other_missing"
        print (filtered_data)
        p <- plot_ly(filtered_data) %>%
          add_trace(x= ~variable, y = ~percentage, type = 'bar', name = variable_individual,
                    marker = list(color = '#C9EFF9'),
                    hoverinfo = "text",
                    text = ~paste(variable,"= ",count,"people, ",percentage*100,"%")) %>%
          layout(margin = list(b = 160), 
                 xaxis = list(tickangle = 70,title = ""),
                 title = variable_individual,
                 yaxis = list(side = 'left', title = 'Percentage of people', showgrid = FALSE, zeroline = FALSE))
        p <- ggplotly( p ) 
        p
      }
  }
  
  
  
  
  ######################################### SCORES SELECTION ############################################
  
  
  output$year_my_sel = renderUI({
    selectInput("year_my_sel", "Select year to analyze: ", input$class_sel)
  })
  
  output$text_year_my_sel <- renderText({ 
    paste("You have selected   people: year ",input$class_sel, " , group ", input$group_sel  )
  })
  
  
  # https://github.com/oraza/sectarianviolencePK
  
  get_p1_plot <- function(data_fun_group, xvariable){
    p1 <- ggplot(data_fun_group, color = ProfileType) + 
      geom_density( aes(x =data_fun_group[c(xvariable)], ..scaled.., color = ProfileType),
                    show.legend = F, 
                    inherit.aes = T ) + 
      labs(x = xvariable) +
      ggthemes::theme_gdocs() 
    return(p1)
  }
  
  get_p1_plot_other <- function(data_fun_group, xvariable){
    p1 <- ggplot(data_fun_group, color = gender_m) + 
      geom_jitter( aes(data_fun_group[c(xvariable)], ProfileType, color = gender_m),
                   show.legend = F, 
                   inherit.aes = T, 
                   width = 0.5, 
                   height = 0.7,
                   size = 1,
                   alpha =0.5) + 
      ggthemes::theme_gdocs() 
    return(p1)
  }
  
  output$plotSCORE1 <- renderPlotly({
    data_group <- get_filter_variable(input$class_sel, input$group_sel, 1, "exam_score")
    ggplotly( get_p1_plot(data_group, "exam_score") )
  })
  
  
  output$plotSCORE2 <- renderPlotly({
    data_group <- get_filter_variable(input$class_sel, input$group_sel, 2, "exam_score")
    ggplotly( get_p1_plot(data_group, "exam_score") )
  })

  
  
  ######################################### LOCATION SELECTION ############################################  
  
  output$text_year_my_sel_location <- renderText({ 
    paste("You have selected   people: year ",input$class_sel_location, " , group ", input$group_sel_location )
  })
  
  # Change to plotly ->  https://plot.ly/r/choropleth-maps/

  output$demoEarthquakes <- renderLeaflet({
    x <- my_earthquakes$mag
    # Leaflet
    library(leaflet)
    #earthquakes_normalized <- (x-min(x))/(max(x)-min(x))
    #bins <- c(.1, .2, .3, .5, .6, .7,.8,.9, Inf)
    #pal <- colorBin("YlOrRd", domain = earthquakes_normalized, bins = bins)
    otherMap <- leaflet(my_earthquakes) %>% addTiles() %>%
      addCircles(lng = ~longitude, lat = ~latitude, 
                 weight = 1,
                 radius = ~20000*mag, 
                 popup = ~paste(paste(paste("place=",place),paste(" || magnitude=",mag)),paste("time",time)),
                 #color = ~pal(earthquakes_normalized)
                 color = 'red'
      )
    
    otherMap 
  })
  
  
    
  output$peopleMap1 <- renderPlot({
    ddf <- get_map_data_countries(1)
    spdf <- joinCountryData2Map(ddf, joinCode="NAME", nameJoinColumn="citizen_cntry")
    mapCountryData(spdf, nameColumnToPlot="count", catMethod="fixedWidth",mapTitle ="people per Country")
  })
  
  output$peopleMap2 <- renderPlot({
    if (input$group_sel_location=='International and National'){
      
      ddf_usa <- get_map_data_usa(2)
      ddf_usa$region <- tolower(ddf_usa$previous_state)
      ditch_the_axes <- theme(
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()
      )
      states_df <- map_data("state")
      baseMap <- ggplot(data = states_df, mapping = aes(x = long, y = lat, group = group)) + 
        coord_fixed(1.3) + 
        geom_polygon(color = "black", fill = "gray")
      cnames <- aggregate(cbind(long, lat) ~ region, data=states_df, FUN=function(x)mean(range(x))) 
      states_people <- inner_join(states_df, ddf_usa, by = "region")
      elbow_room1 <- 
        baseMap +
        geom_polygon(data = states_people, aes(fill = count), color = "white") +
        geom_polygon(color = "black", fill = NA) +
        theme_bw() 
      #xlim(-90, -65) + 
      #ylim(25, 50)
      ditch_the_axes
      elbow_room1 
    } 
    
    else {
      ddf <- get_map_data_countries(2)
      spdf <- joinCountryData2Map(ddf, joinCode="NAME", nameJoinColumn="citizen_cntry")
      mapCountryData(spdf, nameColumnToPlot="count", catMethod="fixedWidth",mapTitle ="people per Country")
    }
    
  })

  
  get_map_data_countries <- function(g_number){
    my_data_COUNTRIES <- get_filter_variable(input$class_sel_location, input$group_sel_location, g_number,"citizen_cntry")
    my_data_COUNTRIES <- my_data_COUNTRIES[complete.cases(my_data_COUNTRIES),]
    my_data_COUNTRIES <- my_data_COUNTRIES[my_data_COUNTRIES$citizen_cntry!="USA",]
    
    my_data_COUNTRIES$citizen_cntry[my_data_COUNTRIES$citizen_cntry=='Trinidad/Tobago'] = "Trinidad and Tobago"
    my_data_COUNTRIES$citizen_cntry[my_data_COUNTRIES$citizen_cntry=='DominicanRep.'] = "Dominican Republic"
    my_data_COUNTRIES$citizen_cntry[my_data_COUNTRIES$citizen_cntry=='SouthAfrica'] = "South Africa"
    my_data_COUNTRIES$citizen_cntry[my_data_COUNTRIES$citizen_cntry=='UnitedKingdom'] = "United Kingdom"
    my_data_COUNTRIES$citizen_cntry[my_data_COUNTRIES$citizen_cntry=='CostaRica'] = "Costa Rica"    
    my_data_COUNTRIES$citizen_cntry[my_data_COUNTRIES$citizen_cntry=='NewZealand'] = "New Zealand"  
    my_data_COUNTRIES$citizen_cntry[my_data_COUNTRIES$citizen_cntry=='ElSalvador'] = "El Salvador" 
    my_data_COUNTRIES$citizen_cntry[my_data_COUNTRIES$citizen_cntry=='HongKong'] = "Hong Kong S.A.R." 
    my_data_COUNTRIES$citizen_cntry[my_data_COUNTRIES$citizen_cntry=='SaudiArabia'] = "Saudi Arabia" 
    my_data_COUNTRIES$citizen_cntry[my_data_COUNTRIES$citizen_cntry=='Peop.Rep.China'] = "China" 
    my_data_COUNTRIES$citizen_cntry[my_data_COUNTRIES$citizen_cntry=='Korea'] = "Korea No Mans Area" 
    my_data_COUNTRIES$citizen_cntry[my_data_COUNTRIES$citizen_cntry=='Neth.Antilles'] = "Netherlands" 
    my_data_COUNTRIES$citizen_cntry[my_data_COUNTRIES$citizen_cntry=='Tanzania'] = "United Republic of Tanzania"
    
    return (plyr::ddply(my_data_COUNTRIES,.(citizen_cntry),dplyr::summarize, count = length(IDREF) ) )
  }
  
  get_map_data_usa <- function(g_number){
    my_data_STATES <- get_filter_states(input$class_sel_location, input$group_sel_location, g_number)
    my_data_STATES <- my_data_STATES[complete.cases(my_data_STATES),]
    my_data_STATES <- my_data_STATES[my_data_STATES$previous_state !="",]

    my_data_STATES$previous_state[ my_data_STATES$previous_state=='NewYork' ] = "new york"
    my_data_STATES$previous_state[ my_data_STATES$previous_state=='NewJersey' ] = "new jersey"
    my_data_STATES$previous_state[ my_data_STATES$previous_state=='DistrictofColumbia' ] = "district of columbia"
    my_data_STATES$previous_state[ my_data_STATES$previous_state=="RhodeIsland" ] = "rhode island"
    my_data_STATES$previous_state[ my_data_STATES$previous_state=="NorthCarolina" ] = "north carolina"
    my_data_STATES$previous_state[ my_data_STATES$previous_state=="SouthCarolina" ] = "south carolina"
    my_data_STATES$previous_state[ my_data_STATES$previous_state=="VirginIslands"] = "virgin islands"
    my_data_STATES$previous_state[ my_data_STATES$previous_state=="WestVirginia"] = "west virginia"
    my_data_STATES$previous_state[ my_data_STATES$previous_state=="PuertoRico"] = "puerto rico"
    my_data_STATES$previous_state[ my_data_STATES$previous_state=="NewHampshire"] = "new hampshire"
    
    return (plyr::ddply(my_data_STATES,.(previous_state),dplyr::summarize, count = length(IDREF) ) )
  }
  
  
  output$table_countries_1 <- renderDataTable({
        feature_data <- get_map_data_countries(1)
        colnames(feature_data) <- c('Country/state','count')
        feature_data$percentage <- round(feature_data$count / sum(feature_data$count ),4)*100
        DT::datatable(feature_data, options = list(paging = FALSE, searching= FALSE, autoWidth= TRUE))
  })
  
  output$table_countries_2 <- renderDataTable({
    if (input$group_sel_location=='International and National'){
      feature_data <- get_map_data_usa(2)
      colnames(feature_data) <- c('Country/state','count')
      feature_data$percentage <- round(feature_data$count / sum(feature_data$count ),4)*100
    } else {
      feature_data <- get_map_data_countries(2)
      print (feature_data)
      colnames(feature_data) <- c('Country/state','count')
      feature_data$percentage <- round(feature_data$count / sum(feature_data$count ),4)*100
    }
    DT::datatable(feature_data, options = list(paging = FALSE, searching= FALSE, autoWidth= TRUE))
  })
  
  
  
  ######################################### SKILLS SELECTION ############################################  
  
  output$text_year_my_sel_skills <- renderText({ 
    paste("You have selected   people: year ",input$class_sel_skills, " , group ", input$group_sel_skills )
  })
  
  output$skillsByIndustry1 <- renderPlotly({
    withProgress(message = "Rendering frequency per Profile", {
      skillName = input$abilities_sel_skills
      get_skills_plot(skillName,1) })
  })
  output$skillsByIndustry2 <- renderPlotly({
    withProgress(message = "Rendering frequency per Profile", {
      skillName = input$abilities_sel_skills
      get_skills_plot(skillName,2) })
  })
  
  get_skills_plot <- function(name_of_skill,g_number){
    filtered_data <- get_filter_skills(input$class_sel_skills, input$group_sel_skills, g_number)
    freq_data <- get_all_type_skills(filtered_data, name_of_skill)
    skills_dataframe <- get_complete_skills_dataframe(freq_data,name_of_skill)
    p <- ggplot2::qplot(percentage.of.people, concept, data=skills_dataframe, colour=Types, ylab = name_of_skill) 
    p <- p + ggplot2::theme(legend.position="bottom")
    p <- ggplotly( p ) 
  }
  
  get_all_type_skills <- function(filtered_for_skills,skill_type){
    columns_interest_names <- names(filtered_for_skills[grep(pattern = skill_type,names(filtered_for_skills))])
    data_interests <- filtered_for_skills[columns_interest_names]
    #data_interests$IDREF <- filtered_for_skills$IDREF
    data_interests$ProfileType <- filtered_for_skills$ProfileType
    freq_interest <- plyr::ddply(data_interests,.(ProfileType), numcolwise(mean) )
    return (freq_interest)
  }
  
  get_complete_skills_dataframe <- function(data_grouped_by,skillName){
    #i <- 1
    #plot.new()
    #par(new=TRUE)
    skills_dataframe <-  data.frame()
    for (onejob in data_grouped_by$ProfileType ){
      job_skills_list <-  data.frame( as.numeric( c( t(data_grouped_by[data_grouped_by$ProfileType==onejob,]) ) ))
      job_skills_list <- data.frame(job_skills_list[complete.cases(job_skills_list),])
      names(job_skills_list) = 'percentage.of.people'
      job_skills_list$concept <- names(data_grouped_by)[names(data_grouped_by)!='ProfileType']
      job_skills_list$concept<- gsub(paste(skillName,"_",sep=""),"", job_skills_list$concept)
      job_skills_list$Types <- replicate(length(job_skills_list$percentage.of.people), onejob)
      #plot( job_skills_list$percentage.of.people,type="p",xaxt='n',col=rainbow(i),ylab=skillName )
      #i <- i+1
      skills_dataframe <- rbind(skills_dataframe,data.frame(job_skills_list))
    }
    skills_dataframe <- filter(skills_dataframe, !grepl('missing', concept))
    skills_dataframe <- filter(skills_dataframe, !grepl('nan', concept))
    skills_dataframe <- filter(skills_dataframe, percentage.of.people>.01)
    #skills_list_names <- names(data_grouped_by)[ names(data_grouped_by) != 'ProfileType']
    #axis(1, at=1:length(skills_list_names), labels=as.list(skills_list_names) )
  }
  
  
  
  ######################################### EDUCATION SELECTION ############################################  
  
  output$text_year_my_sel_education <- renderText({ 
    paste("You have selected: year ",input$class_sel_education, " , group ", input$group_sel_education )
  })
  
  output$plotDegree1 <- renderPlotly({
    withProgress(message = "Rendering previous degree per Industry", {
      get_education_degree_plot(1) 
    })
  })
  
  output$plotMajor1 <- renderPlotly({
    withProgress(message = "Rendering previous degree per Industry", {
      get_education_major_plot(1) 
    })
  })
  
  
  output$plotSchool1 <- renderPlotly({
    withProgress(message = "Rendering previous degree per Industry", {
      get_education_school_plot(1) 
    })
  })
  
  output$plotDegree2 <- renderPlotly({
    withProgress(message = "Rendering previous degree per Industry", {
      get_education_degree_plot(2) 
    })
  })
  
  output$plotMajor2 <- renderPlotly({
    withProgress(message = "Rendering previous degree per Industry", {
      get_education_major_plot(2) 
    })
  })
  
  output$plotSchool2 <- renderPlotly({
    withProgress(message = "Rendering previous degree per Industry", {
      get_education_school_plot(2) 
    })
  })
  
  get_education_degree_plot <- function(g_number){
    filtered_data <- get_filter_education(input$class_sel, input$group_sel, g_number,"degree")
    #filtered_data <- get_filter_education(input$class_sel_education, input$group_sel_education, g_number,"degree")
    
    ## variable reference = previous degree
    grouped_data <- plyr::ddply(filtered_data,.(ProfileType,  previous_degree  ), summarize, count = length(IDREF) ) 
    industry_data <- plyr::ddply(grouped_data,.(ProfileType), numcolwise(sum) )
    industry_data$mycount <- industry_data[c('count')]
    summary_data <- join(grouped_data,industry_data[c('ProfileType','mycount')],by='ProfileType' )
    summary_data$Frequency.per.industry <- summary_data$count / summary_data$mycount
    ## variable reference = previous degree
    p <- ggplot(data = summary_data, aes(x = previous_degree, y = Frequency.per.industry, group = ProfileType, fill = ProfileType))
    p <- p + geom_bar(stat = "identity", width = 0.5, position = "dodge") + ggplot2::theme_bw() 
    p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))
    p <- ggplotly( p ) 
  }
  
  
  get_education_major_plot <- function(g_number){
    filtered_data <- get_filter_education(input$class_sel, input$group_sel, g_number,"major")
    #filtered_data <- get_filter_education(input$class_sel_education, input$group_sel_education, g_number,"major")
    
    #variable reference = major
    grouped_data <- plyr::ddply(filtered_data,.(ProfileType, major ), summarize, count = length(IDREF) ) 
    industry_data <- plyr::ddply(grouped_data,.(ProfileType), numcolwise(sum) )
    industry_data$mycount <- industry_data[c('count')]
    summary_data <- join(grouped_data,industry_data[c('ProfileType','mycount')],by='ProfileType' )
    summary_data$Frequency.per.industry <- summary_data$count / summary_data$mycount
    #variable reference = major
    p <- ggplot(data = summary_data, aes(x =major, y = Frequency.per.industry, group = ProfileType, fill = ProfileType))
    p <- p + geom_bar(stat = "identity", width = 0.5, position = "dodge") + ggplot2::theme_bw() 
    p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))
    p <- ggplotly( p ) 
  }
  
  
  get_education_school_plot <- function(g_number){
    
    filtered_data <- get_filter_education(input$class_sel_education, input$group_sel_education, g_number,"school")
    
    #variable reference = school
    grouped_data <- plyr::ddply(filtered_data,.(ProfileType, school ), summarize, count = length(IDREF) ) 
    industry_data <- plyr::ddply(grouped_data,.(ProfileType), numcolwise(sum) )
    industry_data$mycount <- industry_data[c('count')]
    summary_data <- join(grouped_data,industry_data[c('ProfileType','mycount')],by='ProfileType' )
    summary_data$Frequency.per.industry <- summary_data$count / summary_data$mycount
    
    summary_data <- summary_data[summary_data$Frequency.per.industry>.015,]
    
    grouped_data <- summary_data[c('ProfileType','school','count')]
    industry_data <- plyr::ddply(grouped_data,.(ProfileType), numcolwise(sum) )
    industry_data$mycount <- industry_data[c('count')]
    summary_data <- join(grouped_data,industry_data[c('ProfileType','mycount')],by='ProfileType' )
    summary_data$Frequency.per.industry <- summary_data$count / summary_data$mycount
    
    #variable reference = school
    p <- ggplot(data = summary_data, aes(x = school, y = Frequency.per.industry, group = ProfileType, fill = ProfileType))
    p <- p + geom_bar(stat = "identity", width = 0.5, position = "dodge") + ggplot2::theme_bw() 
    p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))
    p <- ggplotly( p ) 
  }
  
  
  
  
  
  ########################################## MODELS SELECTIONS ###############################################
  
  
  output$text_my_correlations_predictive <- renderText({ 
    paste("These graphs represent the correlation between each industry and the most important variables")
  })
  
  
  output$plotCorrelations <- renderPlotly({
    withProgress(message = "Rendering correlations", {
      unique_target <- unique(my_data$ProfileType)
      variables_correl <- c("pv_dg_1_dg_dsc", "pv_dg_mj_1_major_description",'pv_dg_mj_2',"pv_sc_1_sc_nm",
                            "gender_m","Year_Reference",'g_oa_scr','g_verbal','g_quant','pv_dg_gpa_1',
                            'race','ETHNIC_CODE_ethnc_dsc',"citizen_cntry","previous_state")
      data_correl <- my_data[,names(my_data) %in% variables_correl ]
      colnames(data_correl) <- c("Gender.Male","Class.Year",'GMAT.Total','GMAT.Math','GMAT.Verbal','Race',
                                 'Previous.GPA',"Previous.Major.2","Previous.School","Previous.Major.1",
                                 'Citizenship.Country',"Previous.Degree","Ethnicity",'State.of.Perm.USA')
      for (industry_i in seq(1,length(unique_target)) ){
        data_target <- as.integer(my_data$ProfileType == unique_target[industry_i])
        data_correl[c(paste("Target: ",unique_target[industry_i]))] <- data_target
      }
      data_correl <- data_correl[complete.cases(data_correl),]
      data_correl <- sapply(data_correl, function(x) 
        if(is.character(x)) {as.factor(x)} 
        else { x } )
      data_correl <- data.frame(data_correl)
      p <- qplot(x=X1, y=X2, data=melt(cor(data_correl)), fill=value, geom="tile")  +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Correlation") +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 70, vjust = 1,size = 8, hjust = 1))+
        coord_fixed() 
      p <- ggplotly(p)
      p
    })
  })
  
  
  
  
  ######################## CPR ##########################
  
  output$image_nyu<- renderImage({
    return(list(
    src = "./data/Images/nyu_logo.jpg",
    contentType = "image/jpeg",
    width = 250,
    height = 250
    ))
  },deleteFile = FALSE)
  
  
})

