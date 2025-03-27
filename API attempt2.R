## Attempting API to get climate data ------------------------------------------


# Worked example of API -------------------------------------------------------
#https://www.dataquest.io/blog/r-api-tutorial/
#install.packages(c("httr2"))
library(httr2)
library(tidyverse)

### Try APISM format

api_url_info= "https://www.longpaddock.qld.gov.au/cgi-bin/silo/"
type_info= "PatchedPointDataset.php?"
station_ID_info = "40004"
start_info="20160101"
finish_info="20160131"
#format_info = "apsim" #"JSON"
format_info = "JSON"
username_info = "jackie.ouzman@csiro.au"

APSIM_URL <- paste0(
  api_url_info, type_info,
  "station=", station_ID_info , "&",
  "start=", start_info, "&",
  "finish=", finish_info, "&",
  "format=", format_info , "&",
  "username=",username_info)
APSIM_URL
#APSIM_format <- GET(APSIM_URL)

APSIM_format <- request(APSIM_URL) |> 
  req_perform()
APSIM_format #this tells me i returned something

df <- APSIM_format |> 
  #resp_body_string() |> 
  resp_body_raw() |> 
  head()

resp_body_json(APSIM_format) |>
  str()

################################################################################

#https://data-science-master.github.io/lectures/05_web_scraping/05_apis.html
#install.packages("repurrrsive")
library(repurrrsive)


data("gh_users")
typeof(gh_users)
length(gh_users)
names(gh_users[[1]])

df <- tibble(users = gh_users) #get some of the list name = df
df
df2 <- tibble(APSIM_2 = APSIM_format[5])
df2
unnest_wider(df2, APSIM_format[5])
