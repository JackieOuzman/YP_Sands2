#https://www.rdocumentation.org/packages/SWTools/versions/1.1.0/topics/SILODownload

install.packages("SWTools")
library(SWTools)
library(readr)
## This package download the file and saves it as text file with the name of the station
## Last day of data, in the format "YYYYMMDD". Will default to yesterday if not specified



Site_number <- "25006"
email_jackie <-"jackie.ouzman@csiro.au"
START_2 <- "20050201"
FINISH_2 <- "20050203"
Files_saved <- "H:/Output-2/Analysis/Scripts/Jackie/Downloaded_files"


## this will download patchpoint data and save it to file directory that is specified
SILODownload(
  SiteList= Site_number,
  username = email_jackie,
  password = "gui",
  path = Files_saved,
  startdate = START_2,
  enddate = FINISH_2, 
  ssl = FALSE
)

################################################################################
##### I cant seem to get the other functions to load.

###below won't run?
SWTools::SILOImport(station =Site_number, 
                    path= Files_saved, 
                    startdate =START_2, 
                    enddate = FINISH_2)

SILOMap(Site_number, path)

df <- SILOLoad(sites=Site_number , 
         path = Files_saved, 
         startdate = START_2, 
         enddate = FINISH_2)
df
################################################################################



X25006 <- read_table("H:/Output-2/Analysis/Scripts/Jackie/Downloaded_files/25006.txt", 
                     col_types = cols(`(ddmmyyyy)` = col_date(format = "%d-%m-%Y")), 
                     skip = 51)
View(X25006)
