rm(list = ls())

### github folder
pathdir <- "C:/Users/pdvd/Online for git/WKTRADE3"

setwd(paste(pathdir,"5 - Output/Interactive maps/Unfished management",sep="/"))

load("protect.RData")
plot(protect$fraction,protect$sarloss,type="l",
     xlab="Unfished area fraction (each MSFD hab in each subdivision)",
     ylab="Fractional loss of fishing effort",las=1,xaxt="n")
axis(1,c(0.05,seq(0.1,.9,0.1),0.95))

load("Region005.RData")
load("Region010.RData")
load("Region020.RData")
load("Region030.RData")
load("Region040.RData")
load("Region050.RData")
load("Region060.RData")
load("Region070.RData")
load("Region080.RData")
load("Region090.RData")
load("Region095.RData")

# get shapefiles subregions, subdivisions    
source(paste(pathdir,"5 - Output/Interactive maps/Shapefiles_subregions_subdivisions.R",sep="/"))

# get unfished area sourced    
source(paste(pathdir,"5 - Output/Interactive maps/Unfished management/IM - unfished source-a.R",sep="/"))

library(leaflet)
library(htmlwidgets)

steps <- c(5, seq(10,90,10),95)
steps <- steps[1:8]
namings <- paste(steps,"% untrawled area, ",round(protect[,2]*100,digits = 1)[1:8],"% effort loss",sep="")

mfs <- leaflet() %>%
  #addTiles() %>%  # Add default OpenStreetMap map tiles
  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  # areal extent
  # addPolygons(data = regall, group="Areal extent",
  #            stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor = "grey") %>%
  
  # region boundaries
  addPolygons(data = EcReg, group="(Sub-)regions",
              stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
  # subdivision boundaries
  addPolygons(data = subdiv, group="Subdivisions",
              stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "yellow") %>%
  
  # unfished area
  addPolygons(data = regunfished, group="Untrawled in the period 2013-2018",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "grey") %>%
  
  # core scenarios
  addPolygons(data = Region005, group=namings[1],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#ffffcc") %>%
  addPolygons(data = Region010, group=namings[2],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#ffffcc") %>%
  addPolygons(data = Region020, group=namings[3],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#ffffcc") %>%
  addPolygons(data = Region030, group=namings[4],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#ffffcc") %>%
  addPolygons(data = Region040, group=namings[5],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#ffffcc") %>%
  addPolygons(data = Region050, group=namings[6],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#ffffcc") %>%
  addPolygons(data = Region060, group=namings[7],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#ffffcc") %>%
  addPolygons(data = Region070, group=namings[8],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#ffffcc") %>%
 # addPolygons(data = Region080, group=namings[9],
 #             stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#ffffcc") %>%
 # addPolygons(data = Region090, group=namings[10],
 #              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#ffffcc") %>%
 # addPolygons(data = Region095, group=namings[11],
 #              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#ffffcc") %>%

  # Layers control
  addLayersControl(
    overlayGroups = c("(Sub-)regions","Subdivisions","Untrawled in the period 2013-2018",namings),
    options = layersControlOptions(collapsed = FALSE)
  )%>%
  
  addLegend(title = "" ,colors = c(NA,NA,NA) ,position = "bottomright",
            labels= c("Layers show the area where closures are needed (sorted from low to high", 
                      "fished grid cells) to reach x % of untrawled area for each MSFD habitat",
                      "in each subdivision and the resulting loss in total fishing effort")) %>%
  
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\"> <b> Area untrawled</label>');
        }
    ")

setwd(paste(pathdir,"5 - Output/Interactive maps/Unfished management",sep="/"))

# save output
saveWidget(mfs, file="Illustration of MSFD protection - untrawled.html")  

