#
# objective: map voter % turnout by county highlighting popularity of outside candidates
#
# load libraries
library(shiny)
library(leaflet)
library(dygraphs)
library(rgdal)
library(magrittr)
require(rgeos)
library(RColorBrewer)
#
# initial data prep
setwd("../data-jam-august-2016-master/csv")
facts <- read.csv(file="county_facts.csv", header=T)
results <- read.csv(file="primary_results.csv", header=T)
setwd("../data-jam-august-2016-master/county_shapefiles")
shp = "cb_2014_us_county_500k.shp"
myshp = readOGR(shp, layer = basename(strsplit(shp, "\\.")[[1]])[1]) # no need to add layer name
texas <- subset(myshp, STATEFP == "48", )
myshp_proj = spTransform(texas, CRS("+proj=longlat +datum=WGS84"))
#
# map out all the counties (test shapefile integration)
leaflet(myshp_proj) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  setView(-99.9018, 31.9686, zoom = 5) %>%
  addPolygons(weight=2, stroke=TRUE, opacity=0.3, popup = paste("<strong>COUNTY: </strong>",myshp$NAME))
#
# combine two datasets: facts & results by county name
facts$county <- gsub(" County", "", facts$area_name) # normalize county name
merged <- merge(facts,results,by="county")
#
# combine final datasets into map
final <- merge(texas, merged, by.x="NAME", by.y="county", duplicateGeoms=TRUE)
#
# isolate bernie
bernie <- texas
bernieresults <- results[results$candidate == "Bernie Sanders", ]
bernieresults <- bernieresults[bernieresults$state == "Texas", ]
bernieresults$NAME <- bernieresults$county
bmerge <- merge(bernie,bernieresults,by="NAME")
#
# define the map color palette
pal <- colorBin("Blues", bmerge$fraction_votes, 8, pretty = FALSE)
#
# bernie map
leaflet(bmerge) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  setView(-99.9018, 31.9686, zoom = 6) %>%
  addPolygons(weight=2, stroke=TRUE, opacity=0.3, color = ~pal(fraction_votes), popup = paste("<strong>COUNTY: </strong>",bmerge$NAME,"<br/>Bernie Percentage: ",bmerge$fraction_votes,"<br/>Bernie Votes: ",bmerge$votes))
#
# isolate trump
trump <- texas
trumpresults <- results[results$candidate == "Donald Trump", ]
trumpresults <- trumpresults[trumpresults$state == "Texas", ]
trumpresults$NAME <- trumpresults$county
tmerge <- merge(trump,trumpresults,by="NAME")
#
# define the map color palette
paltrump <- colorBin("Reds", trump$fraction_votes, 8, pretty = FALSE)
#
# trump map
leaflet(tmerge) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  setView(-99.9018, 31.9686, zoom = 7) %>%
  addPolygons(weight=2, stroke=TRUE, opacity=0.3, color = ~paltrump(fraction_votes), popup = paste("<strong>COUNTY: </strong>",tmerge$NAME,"<br/>Trump Percentage: ",tmerge$fraction_votes,"<br/>Trump Votes: ",tmerge$votes))