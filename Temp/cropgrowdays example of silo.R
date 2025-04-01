
#https://r-packages.io/packages/cropgrowdays/get_silodata

#install.packages("cropgrowdays")
library(cropgrowdays)


latitude_1 <- "-27.9927"
longitude_1 <- "152.6906"
email_1 <- "jackie.ouzman@csiro.au"
START_1 <- "20201101"
FINISH_1 <- "20201231"
FORMAT_1 <- c("apsim", "fao56", "standard", "allmort", "ascepm", "evap_span", "span",
           "all2016", "alldata", "p51", "rainonly", "monthly", "cenw")

PASSWORD_1 <- "apitest"
URL_1 <- "https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php"


example <- get_silodata(
  latitude = latitude_1,
  longitude = longitude_1,
  email = email_1,
  START = START_1,
  FINISH = FINISH_1,
  FORMAT = FORMAT_1,
  PASSWORD = PASSWORD_1,
  extras = NULL,
  URL = URL_1
)

example

###############################################################################
#What about if I change the site?


latitude_2 <- "-27.6297"
longitude_2 <- "152.7111"
email_1 <- "jackie.ouzman@csiro.au"
START_2 <- "20160101"
FINISH_2 <- "20160102"
FORMAT_1 <- c("apsim", "fao56", "standard", "allmort", "ascepm", "evap_span", "span",
              "all2016", "alldata", "p51", "rainonly", "monthly", "cenw")

PASSWORD_1 <- "apitest"
URL_1 <- "https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php"


example2 <- get_silodata(latitude = latitude_2,
                         longitude = longitude_2,
                         email = email_1,
                         START = START_2,
                         FINISH = FINISH_2,
                         FORMAT = FORMAT_1,
                         PASSWORD = PASSWORD_1,
                         extras = NULL,
                         URL = URL_1)


example2

###############################################################################
#What about if I change the data drill for patch point?


latitude_2 <- "-27.6297"
longitude_2 <- "152.7111"
email_1 <- "jackie.ouzman@csiro.au"
START_2 <- "20160101"
FINISH_2 <- "20160102"
FORMAT_1 <- c("apsim", "fao56", "standard", "allmort", "ascepm", "evap_span", "span",
              "all2016", "alldata", "p51", "rainonly", "monthly", "cenw")

PASSWORD_1 <- "apitest"
URL_2 <- "https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php"


example3 <- get_silodata(latitude = latitude_2,
                         longitude = longitude_2,
                         email = email_1,
                         START = START_2,
                         FINISH = FINISH_2,
                         FORMAT = FORMAT_1,
                         PASSWORD = PASSWORD_1,
                         extras = NULL,
                         URL = URL_2)


example3

#NOPE it wont run:(

make_URL <- paste0(URL_1,
                   "?start=",START_2,
                   "&finish=",FINISH_2,
                   "&lat=", latitude_2,
                   "&lon=", longitude_2,
                   "&format=", FORMAT_1,
                   "&username=", email_1,
                   "&password=",PASSWORD_1)
example4 <- get_silodata(make_URL)  


make_URL

https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php?start=20110101
&finish=20110110&
  lat=-23.00&
  lon=135.00&
  format=alldata&
  username=jackie.ouzman@csiro.au&
  password=apirequest
