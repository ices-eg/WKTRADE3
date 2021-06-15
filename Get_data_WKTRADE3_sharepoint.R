
################################################################################

# The below script allows you to link to the ICES WKTRADE3 
# sharepoint data folder. Access is restricted to 
# WK participants that have signed the terms of use document
#
# The below script will automatically create a folder structure and 
# download the fisheries data and benthic impact predictions. 
# After downloading the data, you can run "Ecoregion_Assessment.R"
#
# The sharepoint data outputs were obtained via "Ecoregion_Assessment_preprocessing.R"
#################################################################################

# set path to folders and specify username 
  pathdir <- "C:/Users/pdvd/Online for git/WKTRADE3" # path to github folder
  pathdir_nogit <- "C:/Users/pdvd/Online for git/WKTRADE3 - Fisheries restricted"  # path to new folder where you like to store the fisheries data
  dir.create(pathdir_nogit)   # create a folder 
  ices_username <- "vandenderen"  # set ices username

# install ICES library
  #remotes::install_github("ices-tools-prod/icesSharePoint")
  library(icesSharePoint)

# download data from sharepoint 
  options(icesSharePoint.username = ices_username)# set ices username
  options(icesSharePoint.site = "/ExpertGroups/WKTRADE3")  # set the site 
  spdir() # put password and check folder structure
  
  fnames <- spfiles("April Technical WK Meeting docs/06. Data/Fisheries and benthic state", full = TRUE)
  for (fname in fnames) {
    spgetfile(fname, destdir = pathdir_nogit)
  }

# delete username storage 
  keyring::key_delete("icesSharePoint", ices_username)

# double check that password is not stored on your computer, "Element not found" is okay 
  keyring::key_delete("icesSharePoint", ices_username)

# done


