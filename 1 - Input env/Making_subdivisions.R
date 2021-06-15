  
# subdivisions are based on HELCOM shapefiles, OSPAR shapefiles and draft input of TGSeabed
  setwd("C:/Users/pdvd/Online for git/WKTRADE3/1 - Input env")

# Celtic seas subdivision
  load("Celtic Seas_region_grid_sensitivity.RData")  
  Region@data$division <- NA
  Region@data$division[Region@data$OSPARreg == "L2.5.2"] <- "deep_CS"
  Region@data$division[Region@data$OSPARreg == "L2.1.3"] <- "deep_CS"
  Region@data$division[Region@data$OSPARreg == "L2.2.6"] <- "south_CS"  
  Region@data$division[Region@data$OSPARreg == "L2.3.1"] <- "south_CS"  
  Region@data$division[Region@data$OSPARreg == "L2.1.1"] <- "North_CS" 
  Region@data$division[Region@data$OSPARreg == "L2.1.6"] <- "North_CS" 
  Region@data$division[Region@data$OSPARreg == "L2.1.7"] <- "North_CS" 
  Region@data$division[Region@data$OSPARreg == "L2.1.8"] <- "North_CS" 
  Region@data$division[Region@data$OSPARreg == "L2.1.9"] <- "North_CS" 
  Region@data$division[Region@data$OSPARreg == "L2.2.3"] <- "North_CS" 
  Region@data$division[Region@data$OSPARreg == "L2.2.8"] <- "North_CS" 
  Region@data$division[Region@data$OSPARreg == "L2.2.4"] <- "North_CS" 
  Region@data$division[Region@data$OSPARreg == "L2.2.7"] <- "North_CS" 
  Region@data$division[Region@data$OSPARreg == "L2.2.9"] <- "North_CS" 
  Region@data$division[Region@data$OSPARreg == "L2.3.2"] <- "Irishsea_CS" 
  Region@data$division[Region@data$OSPARreg == "L2.3.3"] <- "Middle_CS" 
  save(Region, file = "Celtic Seas_region_grid_sensitivity.RData")  
  
# North sea subdivision
  load("Greater North Sea_region_grid_sensitivity.RData")  
  Region@data$division <- NA
  Region@data$division[Region@data$OSPARreg == "L2.1.1"] <- "Northern_NS" 
  Region@data$division[Region@data$OSPARreg == "L2.2.4"] <- "Northern_NS" 
  Region@data$division[Region@data$OSPARreg == "L2.2.7"] <- "Northern_NS" 
  Region@data$division[Region@data$OSPARreg == "L2.2.9"] <- "Northern_NS" 
  Region@data$division[Region@data$OSPARreg == "L2.2.1"] <- "Kattegat_NS" 
  Region@data$division[Region@data$OSPARreg == "L2.2.2"] <- "Channel_NS" 
  Region@data$division[Region@data$OSPARreg == "L2.3.4"] <- "Channel_NS" 
  Region@data$division[Region@data$OSPARreg == "L2.3.1"] <- "Channel_NS" 
  Region@data$division[Region@data$OSPARreg == "L2.2.5"] <- "Southern_NS" 
  Region@data$division[Region@data$Depth < -200 ] <- "NTrench_NS"
  Region@data$division[Region@data$long > 4 & Region@data$lat > 58.5 & Region@data$EEZ == "Norway" ] <- "NTrench_NS"
  Region@data$division[Region@data$long > 5 & Region@data$lat > 57.9  & Region@data$EEZ == "Norway" ] <- "NTrench_NS"
  Region@data$division[Region@data$long > 8 & Region@data$division == "Northern_NS" ] <- "NTrench_NS"
  
  save(Region, file = "Greater North Sea_region_grid_sensitivity.RData")  
  
# Baltic seas subdivision
  load("Baltic Sea_region_grid_sensitivity.RData")  
  #Region@data$HELCOMreg <- Region@data$division
  Region@data$division <- NA
  Region@data$division[Region@data$HELCOMreg == "Ã.land Sea"] <- "Bothnian_BS"  # check Aland sea spelling
  Region@data$division[Region@data$HELCOMreg == "Gulf of Finland"] <- "GulfF_BS" 
  Region@data$division[Region@data$HELCOMreg == "Gulf of Riga"] <- "GulfR_BS" 
  Region@data$division[Region@data$HELCOMreg == "Arkona Basin"] <- "ArkBor_BS" 
  Region@data$division[Region@data$HELCOMreg == "Bornholm Basin"] <- "ArkBor_BS" 
  Region@data$division[Region@data$HELCOMreg == "Bay of Mecklenburg"] <- "Western_BS" 
  Region@data$division[Region@data$HELCOMreg == "Great Belt"] <- "Western_BS" 
  Region@data$division[Region@data$HELCOMreg == "Kiel Bay"] <- "Western_BS" 
  Region@data$division[Region@data$HELCOMreg == "Kattegat"] <- "Western_BS"
  Region@data$division[Region@data$HELCOMreg == "The Sound"] <- "Western_BS" 
  Region@data$division[Region@data$HELCOMreg == "Gdansk Basin"] <- "Proper_BS" 
  Region@data$division[Region@data$HELCOMreg == "Eastern Gotland Basin"] <- "Proper_BS" 
  Region@data$division[Region@data$HELCOMreg == "Western Gotland Basin"] <- "Proper_BS" 
  Region@data$division[Region@data$HELCOMreg == "Northern Baltic Proper"] <- "Proper_BS" 
  Region@data$division[Region@data$HELCOMreg == "Bothnian Sea"] <- "Bothnian_BS" 
  Region@data$division[Region@data$HELCOMreg == "The Quark"] <- "Bothnian_BS" 
  Region@data$division[Region@data$HELCOMreg == "Bothnian Bay"] <- "Bothnian_BS" 
  
  save(Region, file = "Baltic Sea_region_grid_sensitivity.RData")  
  
# BoB-IC subdivisions
  load("Bay of Biscay and the Iberian Coast_region_grid_sensitivity.RData")  
  Region@data$division <- NA
  Region@data$division[Region@data$Depth > -200 & Region@data$EEZ == "France" ] <- "Shallow_BoB"
  Region@data$division[Region@data$Depth > -200 & Region@data$lat > 41 & Region@data$EEZ == "Spain" ] <- "ShallowNorth_IC"
  Region@data$division[Region@data$Depth > -200 & Region@data$lat > 41 & Region@data$EEZ == "Portugal" ] <- "ShallowNorth_IC"
  Region@data$division[Region@data$Depth > -200 & Region@data$lat < 41 & Region@data$long > -11 & Region@data$EEZ == "Portugal" ] <- "ShallowSouth_IC"
  Region@data$division[Region@data$Depth > -200 & Region@data$lat < 37.2 & Region@data$long > -8.9 ] <- "Galicia_IC"
  Region@data$division[Region@data$Depth < -200 & Region@data$lat < 41 & Region@data$EEZ == "Spain" ] <- "Galicia_IC"
  Region@data$division[Region@data$Depth < -200 & Region@data$lat < 41 & Region@data$EEZ == "Spain" ] <- "Deep_IC"
  Region@data$division[Region@data$Depth < -200 & Region@data$lat < 41 & Region@data$EEZ == "Portugal" ] <- "Deep_IC"
  Region@data$division[Region@data$Depth < -200 & is.na(Region@data$division)] <- "Deep_BoB"
  
  save(Region, file = "Bay of Biscay and the Iberian Coast_region_grid_sensitivity.RData")  
  
  
  
  
  
  
  
  