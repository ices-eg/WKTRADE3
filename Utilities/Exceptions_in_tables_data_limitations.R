
# ------------------------------------------------------------------------------
# The script removes output from tables where there is missing information
# or information is wrong
# ------------------------------------------------------------------------------

# issue 1 ----------------------------------------------------------------------
  # No sensitivity of benthic community is available in the greater North Sea 
  # in areas deeper than 200m 
  # Impact estimate from Greater North Sea 200-800 m is removed from table 1
    
    setwd(paste(pathdir,"5 - Output/(sub-)Region/Greater North Sea",sep="/"))
    dat <- read.csv(file = "Greater North Sea_Table_1.csv",check.names=FALSE)
    dat[6:9,3] <- NA
    write.csv(dat, file="Greater North Sea_Table_1.csv", row.names=FALSE)

# issue 2 ----------------------------------------------------------------------
  # No value of landings in Iberian Coast in ICES VMS datacall (2022) 
  # remove from all tables in subdivisions and sub-region
  # note that value of landings is available for Bay of Biscay 
  # --- hence not removed from Bay of Biscay subdivision ---
    
    # subdivisions
    div <-   c("ShallowNorth_IC","ShallowSouth_IC", "Galicia_IC",  "Deep_IC", "Deep_BoB")
    for (dix in 1:5){
      setwd(paste(pathdir,paste("5 - Output/Division",div[dix],sep="/"),sep="/"))
      dat <- read.csv(file = paste(div[dix],"Table_2.csv",sep="_"),check.names=FALSE)
      dat[,4:5] <- NA
      write.csv(dat, file=paste(div[dix],"Table_2.csv",sep="_"), row.names=FALSE)
      
      dat <- read.csv(file = paste(div[dix],"Table_3.csv",sep="_"),check.names=FALSE)
      dat[c(2,5),2:11] <- NA
      write.csv(dat,file = paste(div[dix],"Table_3.csv",sep="_"), row.names=FALSE)
    }
  
    # (sub-)region BoBIC
    sreg <-   c("Bay of Biscay and the Iberian Coast")
    dix <- 1
    setwd(paste(pathdir,paste("5 - Output/(sub-)Region",sreg[dix],sep="/"),sep="/"))
    dat <- read.csv(file = paste(sreg[dix],"Table_2.csv",sep="_"),check.names=FALSE)
    dat[,4:5] <- NA
    write.csv(dat, file=paste(sreg[dix],"Table_2.csv",sep="_"), row.names=FALSE)
      
    dat <- read.csv(file = paste(sreg[dix],"Table_3.csv",sep="_"),check.names=FALSE)
    dat[c(2,5),2:11] <- NA
    write.csv(dat,file = paste(sreg[dix],"Table_3.csv",sep="_"), row.names=FALSE)
  