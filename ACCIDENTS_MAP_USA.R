# An example from R-bloggers: Mapping Traffic Fatalities 
browseURL("https://www.r-bloggers.com/mapping-traffic-fatalities/")

# Data 
browseURL("ftp://ftp.nhtsa.dot.gov/fars/2015/National/")
  

# Data cleaning
path <- "C:/scrap_docs/"
filename <- 'accident.csv'
file <- paste(path,filename,sep="")
accidents <- read.csv(file)

accidents <- subset(accidents, LONGITUD!=999.99990 &  LONGITUD!=888.88880 & LONGITUD!=777.77770)
cont_us_accidents <- subset(accidents, STATE!=2 & STATE!=15)

# Mapping
library(ggplot2)
library(maps)

county_map_data <- map_data("county")
county_map_data[1:10,]

state_map <- map_data("state")
state_map[1:10,]

library(mapproj)

map <-ggplot() + 
  #Add county borders:
  geom_polygon(data=county_map_data, aes(x=long,y=lat,group=group), colour = alpha("grey", 1/4), size = 0.2, fill = NA) +
  #Add state borders:
  geom_polygon(data = state_map, aes(x=long,y=lat,group=group), colour = "grey", fill = NA) +
  
  #Add points (one per fatality):
  # THIS IS OUR DATAFRAME AND THE X,Y COLUMNS WITH LOCATION !!!!
  geom_point(data=cont_us_accidents, aes(x=LONGITUD, y=LATITUDE), alpha=0.05, size=0.5, col="blue") +
  
  
  #Adjust the map projection
  coord_map("stereographic") +
  #Add a title:
  ggtitle("National Highway Traffic Safety Administration (NHTSA), ") +
  #Adjust the theme:
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank(),
        axis.title = element_blank()
        )


# Output an image in a file .jpg
#jpeg(paste(path,'traffic_fatalities.jpg',sep=''), width = 1800, height = 1200, res = 280, quality = 100)

# Display here 
map



# Other data cleaning from URL in R
library(data.table)
urlfile <- 'http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.csv'
earthquakes <- fread(urlfile) 
x <- earthquakes$mag

# Leaflet
library(leaflet)

earthquakes_normalized <- (x-min(x))/(max(x)-min(x))
bins <- c(0,.1, .2, .3, .4, .5, .6, .7,.8,.9,1, Inf)
pal <- colorBin("YlOrRd", domain = earthquakes_normalized, bins = bins)

otherMap <- leaflet(earthquakes) %>% addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, 
             weight = 1,
             radius = ~20000*mag, 
             popup = ~paste(paste("place=",place),paste(" || magnitude=",mag)),
             color = ~pal(earthquakes_normalized)
             #color = cm.colors(earthquakes_normalized)
  )

otherMap 

#More options:  https://rstudio.github.io/leaflet/choropleths.html




dev.off()


