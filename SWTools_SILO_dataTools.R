#https://www.rdocumentation.org/packages/SWTools/versions/1.1.0/topics/SILODownload

install.packages("SWTools")
library(SWTools)

## This package download the file and saves it as text file with the name of the station
## Last day of data, in the format "YYYYMMDD". Will default to yesterday if not specified

getwd()

SILODownload(
  SiteList= "040004",
  username = email_1,
  password = "gui",
  path = getwd(),
  startdate = START_2,
  enddate = FINISH_2, 
  
  ssl = FALSE
)

df <- SILOLoad("040004", path = "C:/Users/ouz001/working_from_home_post_Sep2022/YP_Sands2")
df
SILOReport(df,"MyReport.docx") #
