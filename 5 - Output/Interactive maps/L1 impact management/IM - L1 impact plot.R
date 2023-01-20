rm(list = ls())

### github folder
pathdir <- "C:/Users/danie/Documents/Online for git/WKTRADE3"
pathdir_nogit <- "C:/Users/danie/Documents/Online for git/WKTRADE4 - Fisheries restricted"

### select time period
Period    <- 2009:2021    # period with fishing data to calculate impact
AssPeriod <- 2016:2021    # assessment period

# get shapefiles subregions, subdivisions    
source(paste(pathdir,"5 - Output/Interactive maps/Shapefiles_subregions_subdivisions-impact-only.R",sep="/"))

# calculate L1 impact for the two quality impact thresholds
source(paste(pathdir,"5 - Output/Interactive maps/L1 impact management/IM - L1impact005 calculations.R",sep = "/")) 
source(paste(pathdir,"5 - Output/Interactive maps/L1 impact management/IM - L1impact020 calculations.R",sep = "/")) 

library(leaflet)
library(htmlwidgets)

steps <- c(5, seq(10,90,10),95)
steps <- steps[c(4,6,8,10)]
namings1 <- paste("to reach ",steps,"% of area below 0.05",sep="")
namings2 <- paste("to reach ",steps,"% of area below 0.20",sep="")

mfs <- leaflet() %>%
  #addTiles() %>%  # Add default OpenStreetMap map tiles
  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  # region boundaries
  addPolygons(data = EcReg, group="(Sub-)regions",
              stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
  # subdivision boundaries
  addPolygons(data = subdiv, group="Subdivisions",
              stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "yellow") %>%
  
  # no information
  addPolygons(data = RegionNA, group="No L1 impact information",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "grey") %>%
  
  # area unfished
  addPolygons(data = Region_unfished, group="Untrawled",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#fff7bc") %>%
  
  # below threshold area
  addPolygons(data = Region_below_T1, group="Trawled area with L1 impact below 0.05",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#fdbb84") %>%
  
  # core scenarios
  addPolygons(data = Region010_T1, group=namings1[1],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#f03b20") %>%
  addPolygons(data = Region030_T1, group=namings1[2],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#f03b20") %>%
  addPolygons(data = Region060_T1, group=namings1[3],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#f03b20") %>%
  addPolygons(data = Region090_T1, group=namings1[4],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#f03b20") %>%
  
  # below threshold
  addPolygons(data = Region_below_T2, group="Trawled area with L1 impact below 0.20",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "#bcbddc") %>%
  
  addPolygons(data = Region010_T2, group=namings2[1],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "red") %>%
  addPolygons(data = Region030_T2, group=namings2[2],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "red") %>%
  addPolygons(data = Region060_T2, group=namings2[3],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "red") %>%
  addPolygons(data = Region090_T2, group=namings2[4],
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor = "red") %>%
  
  # Layers control
  addLayersControl(
    overlayGroups = c("(Sub-)regions","Subdivisions","No L1 impact information","Untrawled",
                      "Trawled area with L1 impact below 0.05",namings1,
                      "Trawled area with L1 impact below 0.20",namings2),
    options = layersControlOptions(collapsed = FALSE)
  )%>%
  
  addLegend(title = "" ,colors = c(NA,NA,NA) ,position = "bottomright",
            labels= c("Layers show the area where intervention is needed (sorted from low to high", 
                      "impacted grid cells) to reach x % of area below the L1 impact threshold of",
                      "0.05 or 0.2 for each MSFD habitat in each subdivision")) %>%
  
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\"> <b> L1 impact</label>');
        }
    ")

setwd(paste(pathdir,"5 - Output/Interactive maps/L1 impact management",sep="/"))

# save output
saveWidget(mfs, file="Illustration of MSFD protection - L1 impact.html")  

