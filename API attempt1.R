## Attempting API to get climate data ------------------------------------------


# Worked example of API -------------------------------------------------------
#https://www.dataquest.io/blog/r-api-tutorial/
#install.packages(c("httr2"))
library(httr2)


### Try APISM format

api_url_info= "https://www.longpaddock.qld.gov.au/cgi-bin/silo/"
type_info= "PatchedPointDataset.php?"
station_ID_info = "40004"
start_info="20160101"
finish_info="20160131"
format_info = "apsim"
username_info = "jackie.ouzman@csiro.au"

APSIM_URL <- paste0(
  api_url_info, type_info,
  "station=", station_ID_info , "&",
  "start=", start_info, "&",
  "finish=", finish_info, "&",
  "format=", format_info , "&",
  "username=",username_info)
APSIM_URL
APSIM_format <- GET(APSIM_URL)

APSIM_format
tail(APSIM_format)


################################################################################
## worked example from https://albert-rapp.de/posts/web_dev/07_httr2_ultimate_guide/07_httr2

#"https://api.weather.gov/points/38.8894,-77.0352"
NWS_base_url <- 'https://api.weather.gov'



NWS_response <- request(NWS_base_url) |> 
  req_url_path_append(
    'points',
    '38.8894,-77.0352' ## this bit is adding the '/points/38.8894,-77.0352' to the url
  ) |> 
  req_perform()
NWS_response
## <httr2_response>
## GET https://api.weather.gov/points/38.8894,-77.0352
## Status: 200 OK
## Content-Type: application/geo+json
## Body: In memory (3091 bytes)
NWS_response
NWS_response |> 
  resp_body_json()


library(tidyverse)
NWS_response |> 
  resp_body_json() |> 
  glimpse()



################################################################################
#https://stackoverflow.com/questions/19286072/extracting-australia-bom-weather-data-programmatically-with-r
#install.packages("bomrang")
if (!require("remotes")) {
  install.packages("remotes", repos = "http://cran.rstudio.com/")
  library("remotes")
}

install_github("ropensci/bomrang", build_vignettes = TRUE)

rain_066062 <- bomrang::get_historical(stationid = 066062,+type = 'rain',+meta = T)
 

head(rain_066062)
$`meta`
# A tibble: 1 x 10
site name                        lat   lon start      end        years percent AWS   ncc_obs_code
<int> <chr>                     <dbl> <dbl> <date>     <date>     <dbl>   <int> <chr> <chr>       
  1 66062 SYDNEY (OBSERVATORY HILL) -33.9  151. 1858-07-01 2018-11-01  160.     100 Y     136         

$historical_data
Product_code Station_number Year Month Day Rainfall Period Quality
1       IDCJAC0009          66062 1858     1   1       NA     NA        
2       IDCJAC0009          66062 1858     1   2       NA     NA        
3       IDCJAC0009          66062 1858     1   3       NA     NA        
4       IDCJAC0009          66062 1858     1   4       NA     NA        
5       IDCJAC0009          66062 1858     1   5       NA     NA      
<<SNIP>>
