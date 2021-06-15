
# some manual edits for the different regions
# remove impact from Greater North Sea 200-800 m
  setwd(paste(pathdir,"5 - Output/(sub-)Region/Greater North Sea",sep="/"))
  dat <- read.csv(file = "Greater North Sea_Table_1.csv",check.names=FALSE)
  dat[6:9,3] <- NA
  write.csv(dat, file="Greater North Sea_Table_1.csv", row.names=FALSE)

  
# remove for the Celtic Seas all weight of landings
  setwd(paste(pathdir,"5 - Output/(sub-)Region/Celtic Seas",sep="/"))
  dat <- read.csv(file = "Celtic Seas_Table_2.csv",check.names=FALSE)
  dat[,4] <- NA
  write.csv(dat, file="Celtic Seas_Table_2.csv", row.names=FALSE)
  
  dat <- read.csv(file = "Celtic Seas_Table_3.csv",check.names=FALSE)
  dat[c(2,4),2:11] <- NA
  write.csv(dat, file="Celtic Seas_Table_3.csv", row.names=FALSE)
  
  dat <- read.csv(file = "Celtic Seas_Hab_1.csv",check.names=FALSE)
  dat[,6] <- NA
  write.csv(dat, file="Celtic Seas_Hab_1.csv", row.names=FALSE)
  
  dat <- read.csv(file = "Celtic Seas_Hab_2.csv",check.names=FALSE)
  dat[,6] <- NA
  write.csv(dat, file="Celtic Seas_Hab_2.csv", row.names=FALSE)
  
  dat <- read.csv(file = "Celtic Seas_Hab_3.csv",check.names=FALSE)
  dat[,6] <- NA
  write.csv(dat, file="Celtic Seas_Hab_3.csv", row.names=FALSE)
  
  dat <- read.csv(file = "Celtic Seas_Hab_4.csv",check.names=FALSE)
  dat[,6] <- NA
  write.csv(dat, file="Celtic Seas_Hab_4.csv", row.names=FALSE)
  
  dat <- read.table("Celtic Seas_habitat_weight.txt",header=T,sep=",",  colClasses = 'character')
  dat[,3:12] <- NA
  write.csv(dat, file="Celtic Seas_habitat_weight.txt", row.names=FALSE)
  
# now do the same for the subdivisions in Celtic Seas
  div <-   c("deep_CS" ,"south_CS", "North_CS" ,"Irishsea_CS", "Middle_CS")
  
  for (dix in 1:5){
  setwd(paste(pathdir,paste("5 - Output/Division",div[dix],sep="/"),sep="/"))
  dat <- read.csv(file = paste(div[dix],"Table_2.csv",sep="_"),check.names=FALSE)
  dat[,4] <- NA
  write.csv(dat, file=paste(div[dix],"Table_2.csv",sep="_"), row.names=FALSE)
  
  dat <- read.csv(file = paste(div[dix],"Table_3.csv",sep="_"),check.names=FALSE)
  dat[c(2,4),2:11] <- NA
  write.csv(dat,file = paste(div[dix],"Table_3.csv",sep="_"), row.names=FALSE)
  
  dat <- read.csv(file = paste(div[dix],"Hab_1.csv",sep="_"),check.names=FALSE)
  dat[,6] <- NA
  write.csv(dat, file=paste(div[dix],"Hab_1.csv",sep="_"), row.names=FALSE)
  
  dat <- read.csv(file = paste(div[dix],"Hab_2.csv",sep="_"),check.names=FALSE)
  dat[,6] <- NA
  write.csv(dat, file=paste(div[dix],"Hab_2.csv",sep="_"), row.names=FALSE)
  
  dat <- read.csv(file = paste(div[dix],"Hab_3.csv",sep="_"),check.names=FALSE)
  dat[,6] <- NA
  write.csv(dat, file=paste(div[dix],"Hab_3.csv",sep="_"), row.names=FALSE)
  
  dat <- read.csv(file =paste(div[dix],"Hab_4.csv",sep="_"),check.names=FALSE)
  dat[,6] <- NA
  write.csv(dat, file=paste(div[dix],"Hab_4.csv",sep="_"), row.names=FALSE)
  
  dat <- read.table(paste(div[dix],"habitat_weight.txt",sep="_"),header=T,sep=",",  colClasses = 'character')
  dat[,3:12] <- NA
  write.csv(dat, file=paste(div[dix],"habitat_weight.txt",sep="_"), row.names=FALSE)
  }
  
## check exceptions BoB-IC
  div <-   c("ShallowNorth_IC","ShallowSouth_IC", "Galicia_IC",  "Deep_IC", "Deep_BoB")
  
  for (dix in 1:5){
    setwd(paste(pathdir,paste("5 - Output/Division",div[dix],sep="/"),sep="/"))
    dat <- read.csv(file = paste(div[dix],"Table_2.csv",sep="_"),check.names=FALSE)
    dat[,4:5] <- NA
    write.csv(dat, file=paste(div[dix],"Table_2.csv",sep="_"), row.names=FALSE)
    
    dat <- read.csv(file = paste(div[dix],"Table_3.csv",sep="_"),check.names=FALSE)
    dat[c(2:5),2:11] <- NA
    write.csv(dat,file = paste(div[dix],"Table_3.csv",sep="_"), row.names=FALSE)
    
  }
  
## do the same for Bay of Biscay (sub-)region
  sreg <-   c("Bay of Biscay and the Iberian Coast")
  
  dix <- 1
  setwd(paste(pathdir,paste("5 - Output/(sub-)Region",sreg[dix],sep="/"),sep="/"))
  dat <- read.csv(file = paste(sreg[dix],"Table_2.csv",sep="_"),check.names=FALSE)
  dat[,4:5] <- NA
  write.csv(dat, file=paste(sreg[dix],"Table_2.csv",sep="_"), row.names=FALSE)
    
  dat <- read.csv(file = paste(sreg[dix],"Table_3.csv",sep="_"),check.names=FALSE)
  dat[c(2:5),2:11] <- NA
  write.csv(dat,file = paste(sreg[dix],"Table_3.csv",sep="_"), row.names=FALSE)

# now do the same for bay of Biscay subdivision
  div <-   c("Shallow_BoB")
  dix <- 1
  setwd(paste(pathdir,paste("5 - Output/Division",div[dix],sep="/"),sep="/"))
  dat <- read.csv(file = paste(div[dix],"Table_2.csv",sep="_"),check.names=FALSE)
  dat[,4] <- NA
  write.csv(dat, file=paste(div[dix],"Table_2.csv",sep="_"), row.names=FALSE)
    
  dat <- read.csv(file = paste(div[dix],"Table_3.csv",sep="_"),check.names=FALSE)
  dat[c(2,4),2:11] <- NA
  write.csv(dat,file = paste(div[dix],"Table_3.csv",sep="_"), row.names=FALSE)
    
  dat <- read.csv(file = paste(div[dix],"Hab_1.csv",sep="_"),check.names=FALSE)
  dat[,6] <- NA
  write.csv(dat, file=paste(div[dix],"Hab_1.csv",sep="_"), row.names=FALSE)
    
  dat <- read.csv(file = paste(div[dix],"Hab_2.csv",sep="_"),check.names=FALSE)
  dat[,6] <- NA
  write.csv(dat, file=paste(div[dix],"Hab_2.csv",sep="_"), row.names=FALSE)
    
  dat <- read.csv(file = paste(div[dix],"Hab_3.csv",sep="_"),check.names=FALSE)
  dat[,6] <- NA
  write.csv(dat, file=paste(div[dix],"Hab_3.csv",sep="_"), row.names=FALSE)
    
  dat <- read.csv(file =paste(div[dix],"Hab_4.csv",sep="_"),check.names=FALSE)
  dat[,6] <- NA
  write.csv(dat, file=paste(div[dix],"Hab_4.csv",sep="_"), row.names=FALSE)
    
  dat <- read.table(paste(div[dix],"habitat_weight.txt",sep="_"),header=T,sep=",",  colClasses = 'character')
  dat[,3:12] <- NA
  write.csv(dat, file=paste(div[dix],"habitat_weight.txt",sep="_"), row.names=FALSE)
  
  
  
  
  
   
