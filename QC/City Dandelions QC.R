user <- "jaha" #choose from hbatrouney or dhourani or jaha

#Save feature apparently does not preserve aspect ratio, better to just copy the files as images
#saveNormal <- "yes" #choose from save "yes" or "no" - only pick yes if you want to save dandelions
#saveMigrant <- "no" #choose from save "yes" or "no" - only pick yes if you want to save dandelions


library(ASGS)
library(ggplot2)
library(viridis)
library (rgeos)
library (dplyr)
library(stringr)
library(grattanCharts)
library(hutils)
library(mapproj)

if (user == "hbatrouney")
{setwd("C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure")
  #Read in basefile
  basefile <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/basefile_workSA2.csv")
  #Read in migrant basefile
  migrantBasefile <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/migrantBasefile")
}else if (user == "dhourani"){
  setwd("/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure")
  #Read in basefile
  basefile <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/basefile_workSA2.csv")
  #Read in migrant basefile
  migrantBasefile <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/migrantBasefile.csv")
}else if (user == "jaha"){
  setwd("/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure")
  #Read in basefile
  basefile <- fread("/Users/jaha/Documents/Spatial structure of cities/Basefile/basefile_workSA2.csv")
  #Read in migrant basefile
  migrantBasefile <- fread("/Users/jaha/Documents/Spatial structure of cities/Basefile/migrantBasefile.csv")
}

#Merge on SUA data 
SA2_to_SUA <- fread("/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Data/SA2_SUA_2016_AUST.csv") %>% .[ , c("SA2_NAME_2016" , "SUA_NAME_2016")]
setkey(SA2_to_SUA , SA2_NAME_2016)
setkey(basefile , work)
#This has ordered the files by SA2 name alphabetically and assigned the SA2 name column to be the key. In basefile, it's the work SA2s that are the key for now.
#The next line now merges the two data.tables using the key.
basefile <- basefile[SA2_to_SUA] %>% setnames(. , "SUA_NAME_2016" , "work_SUA")
setkey(basefile , live)
#Now we've set the live SA2s as the key, ready to merge SUA on again for the home address. 
basefile <- basefile[SA2_to_SUA] %>% setnames(. , "SUA_NAME_2016" , "live_SUA")

#Read in SUA shapefile
SUA_shapefile <- readOGR(dsn = "/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Data/SUA shapefile 2016" , layer = "SUA_2016_AUST")

#Add coastline data 

Australian_coastline <-
  fread("https://media.githubusercontent.com/media/HughParsonage/Australian-coastline/master/Australian-coastline.tsv")

Sydney_coastline <- 
  Australian_coastline %>% 
  .[lat %between% c(-34.1, -33.40)] %>% 
  .[long %between% c(150.7, 153)] %>%
  # Islands cause difficulties
  .[, N := .N, by = group] %>%
  .[N == max(N)]

Melbourne_coastline <- 
  Australian_coastline %>%
  #Greater Melbourne extends as far south as -38.5 (Mornington Peninsula), though -38.2 was the original input. It goes as east as 144.3 - 146.0 (144.5 - 145.2 original), and north to -37.3 (-37.5 original)
  .[lat %between% c(-38.5, -37.3)] %>%
  .[long %between% c(144.3, 146)]  %>%
  # Islands cause difficulties
  .[, N := .N, by = group] %>%
  .[N == max(N)]

Brisbane_coastline <- 
  Australian_coastline %>%
  .[lat %between% c(-28.14, -26.79)] %>%
  .[long %between% c(150.0, 153.40)]  %>%
  # Islands cause difficulties
  .[, N := .N, by = group] %>%
  .[N == max(N)]


Darwin_coastline <- 
  Australian_coastline %>%
  .[lat %between% c(-12.7 , -12.3)] %>%
  .[long %between% c(130.8, 131.2)]  %>%
  # Islands cause difficulties
  .[, N := .N, by = group] %>%
  .[N == max(N)]

Perth_coastline <-
  Australian_coastline %>%
  .[group == 4.10]


Hobart_coastline <- 
  Australian_coastline %>%
  .[lat %between% c(-43.2 , -42.71)] %>%
  .[long %between% c(147.1, 147.6)]  %>%
  # Islands cause difficulties
  .[, N := .N, by = group] %>%
  .[N == max(N)]

Newcastle_coastline <- 
  Australian_coastline %>%
  .[lat %between% c(-33.15, -32.65)] %>% 
  .[long %between% c(151.35, 151.85)] %>%
  .[, N := .N, by = group] %>%
  .[N == max(N)]

Cairns_coastline <- 
  Australian_coastline %>%
  .[lat %between% c(-17.1, -16.77)] %>% 
  .[long %between% c(145.66, 146)] %>%
  .[, N := .N, by = group] %>%
  .[N == max(N)]

Adelaide_coastline <- 
  Australian_coastline %>%
  .[lat %between% c(-35.30, -34.56)] %>% 
  .[long %between% c(138.45, 139)] %>%
  .[, N := .N, by = group] %>%
  .[N == max(N)]

GoldCoast_coastline <- 
  Australian_coastline %>%
  .[lat %between% c(-28.4, -27.74)] %>% 
  .[long %between% c(153.23, 153.55)] %>%
  .[, N := .N, by = group] %>%
  .[N == max(N)]

SunshineCoast_coastline <- 
  Australian_coastline %>%
  .[lat %between% c(-26.83, -26.32)] %>% 
  .[long %between% c(152.93, 153.14)] %>%
  .[, N := .N, by = group] %>%
  .[N == max(N)]


#Set global parameters 

work.asgs <- "SA2" #Select main statistical area of work from "DZN" and "SA2" (needs to be in "") )
live.asgs <- "SA2" #Keep as SA2
year <- "2016" #Select year of analysis  (needs to be in "") )

#Select the cities of interest (needs to be in "") - choose from ASGS greater capital city statistical area for capitals, and SUA for non-capitals)
mainCitiesList <- c("Greater Sydney" , "Greater Melbourne", "Greater Brisbane","Greater Perth","Greater Adelaide")
regionalCitiesCoastal <- c("Newcastle - Maitland" , "Cairns" , "Gold Coast - Tweed Heads" , "Sunshine Coast")
regionalCitiesInland <- c("Canberra - Queanbeyan", "Bendigo" , "Ballarat")
regionalCities <- c(regionalCitiesCoastal , regionalCitiesInland)
smallerCitiesList <- c(regionalCities ,"Canberra - Queanbeyan" ,"Geelong","Gold Coast","Sunshine Coast","Illawarra","Townsville","Cairns","Greater Darwin","Toowoomba","Ballarat" ,"Bendigo","Murray","Launceston and North East", "Greater Hobart")

regionList <- c(mainCitiesList, regionalCities)

############################################################################################################################################################################################################
#Total population journey to work dandelion (ie not just migrants)
############################################################################################################################################################################################################

#Function for drawing city dandelion (don't change this)

CityDandelions <- function(yr, region) {
  #set sample sizes below
  if (region %in% mainCitiesList) {sampleSize <- 200} else if (region %in% smallerCitiesList) {sampleSize <- 80}  
  
  #basefile <-basefile[work_region==live_region & work_region == region & year == yr & core_city_work ==1 ]
  
  #worth playing around with the "work == live" requirement, especially for smaller cities (where people may commute in from outside the city's SA2s). Comment out the relevant line.
  if (region %in% regionalCities){
    basefile2 <-basefile[work_SUA == live_SUA & live_SUA == region & year == yr ]
    #basefile2 <-basefile[work_SUA == region & year == yr ]
  } else {
    basefile2 <-basefile[work_region==live_region & work_region == region & year == yr ]}
    #basefile2 <-basefile[work_region == region & year == yr ]}
  ############
  # #here's a thought - what if I make a map of the MOST common routes, rather than a random sample? I'm thinking it will mainly be lines into the CBD with a few dots in other big SA2s
  # setkey(basefile2,workers)
  # #setkey orders them in ascending order, so I need to take the bottom 200 obs
  # ordered_sample <- basefile2[(length(basefile2$workers)-199):length(basefile2$workers),c("workers","dest_lon","dest_lat","orig_lon","orig_lat")]
  ############
  
  
  # Worth investigating whether replace = TRUE makes much difference, given how low the prob of any individual route actually is.
  samp_idx <- sample(seq_len(nrow(basefile2)), sampleSize, replace = TRUE, prob=basefile2$workers)
  randomSample <- basefile2[samp_idx, ][ , c("dest_lon" , "dest_lat" , "orig_lon" , "orig_lat")]
  dest_weight <- basefile2[ , .(jobs_weight = sum(workers)) , by = .(dest_lon, dest_lat)]
 
  setkey(dest_weight ,  dest_lon , dest_lat)
  #### UNCOMMENT THESE LINES FOR THE ACTUAL SCRIPT -- temporarily commented out for a test of the 200 most common routes
   setkey(randomSample ,  dest_lon , dest_lat)
   randomSample <- dest_weight[randomSample]
  ############
  #trial below only (used to make maps of the most popular routes)
  # setkey(ordered_sample ,  dest_lon , dest_lat)
  # ordered_sample <- dest_weight[ordered_sample]
  #############
  
  if (region %in% c(mainCitiesList, regionalCitiesCoastal)){
    
    border <- switch(region,
                     "Greater Melbourne" = Melbourne_coastline,
                     "Greater Sydney" = Sydney_coastline, 
                     "Greater Brisbane" = Brisbane_coastline ,
                     "Greater Perth" = Perth_coastline ,
                     "Greater Hobart" = Hobart_coastline ,
                     "Newcastle - Maitland" = Newcastle_coastline ,
                     "Cairns" = Cairns_coastline ,
                     "Greater Adelaide" = Adelaide_coastline ,
                     "Gold Coast - Tweed Heads" = GoldCoast_coastline ,
                     "Sunshine Coast" = SunshineCoast_coastline ,
                     "Greater Darwin" = Darwin_coastline)
    
    
  } else if (region %in% regionalCitiesInland) { 
    
    border <- SUA_shapefile[SUA_shapefile$"SUA_NAME16" == region,] %>% fortify %>% as.data.table
    
  }
  
  
  
  knitting <- isTRUE(getOption('knitr.in.progress'))
  #uncomment the below code when not calculating the top 200 journeys
  border <- cbind(border[piece==1] , randomSample ) 
  ######
  # border <- cbind(border[piece==1] , ordered_sample )
  ######
  plot <- border %>% ggplot(aes(x = long, y = lat,order = order, group = interaction(piece, group)) ) +
    geom_path(color = theGrey) +
    ggplot2::coord_map() +
    ggplot2::theme_void(base_family = if (knitting) "helvet" else "") +
    theme(legend.position="none") +
    theme(axis.line=element_blank(), axis.ticks=element_blank())  +
    geom_segment(data = border , aes(x = orig_lon, y = orig_lat, xend = dest_lon, yend =  dest_lat)  , color = "#F68B33")+
    geom_point(data = border , aes(dest_lon, dest_lat , size = border$jobs_weight ), color = "#AEAEAE" )
  
  if (region == "Greater Perth"){
    plot <- Perth_coastline  %>% ggplot(aes(x = long, y = lat,order = order, group = interaction(piece, group)) ) +
      geom_path(color = theGrey) +
      ggplot2::coord_map(xlim = c(115.5, 116.31), 
                         ylim = c(-32.7, -31.4))+
      ggplot2::theme_void(base_family = if (knitting) "helvet" else "") +
      theme(legend.position="none") +
      theme(axis.line=element_blank(), axis.ticks=element_blank())  +
      geom_segment(data = border , aes(x = orig_lon, y = orig_lat, xend = dest_lon, yend =  dest_lat)  , color = "#F68B33")+
      geom_point(data = border , aes(dest_lon, dest_lat , size = border$jobs_weight ), color = "#AEAEAE" )
  }
  
  assign(paste0(region,"plot"), plot , envir= globalenv())
  # 
  # if (saveNormal == "yes"){
  #   if (user == "hbatrouney"){
  #     ggsave(plot , filename = paste0("C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/CityDandelions/CityDandelion",region,year,".png"))    
  #   } else if (user == "dhourani"){
  #     ggsave(plot, filename = paste0("/Users/dhourani/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/CityDandelions/CityDandelion2/",region,year,".png" ), device = NULL, path = NULL,
  #            scale = 1, width = 5, height = 5, units = "in",
  #            dpi = 300, limitsize = TRUE)
  #   }
  # }
  
}

#Call function (don't change this)
for (j in regionList) {
  CityDandelions(yr = year , region = j )
}


############################################################################################################################################################################################################
##Migrant journey to work dandelion
############################################################################################################################################################################################################

migrantRegionList <- c(  "Greater Sydney" , "Greater Melbourne", "Greater Brisbane" )

#Function for drawing city dandelion (don't change this)
MigrantCityDandelions <- function(yr, region , yearsInAustralia ) {
  #set sample sizes here
  if (region %in% mainCitiesList) {sampleSize <- 200} else if (region %in% smallerCitiesList) {sampleSize <- 80}  
  
  
  migrantBasefile2 <-migrantBasefile[work_region==live_region & work_region == region & year == yr & yearsFromArrival == yearsInAustralia]
  #I'm editing this file from the original to set sampling WITH replacement.
  samp_idx <- sample(seq_len(nrow(migrantBasefile2)), sampleSize, prob=migrantBasefile2$workersNonStuds, replace = TRUE)
  randomSample <- migrantBasefile2[samp_idx, ]
  dest_weight <- migrantBasefile2[yearsFromArrival == yearsInAustralia , .(jobs_weight = sum(workersNonStuds)) , by = .(dest_lon, dest_lat)]
  
  setkey(dest_weight ,  dest_lon , dest_lat)
  setkey(randomSample ,  dest_lon , dest_lat)
  randomSample <- dest_weight[randomSample]
  
  
  #Get work shapefile
  workShapefile <- 
    switch(year,
           "2011" = {SA2_2011},
           "2016" = {SA2_2016})
  
  region_data <- workShapefile[workShapefile$"GCC_NAME16" == region,] %>% fortify
  
  coastline <- switch(region,
                      "Greater Melbourne" = Melbourne_coastline,
                      "Greater Sydney" = Sydney_coastline, 
                      "Greater Brisbane" = Brisbane_coastline ,
                      "Greater Perth" = Perth_coastline ,
                      "Greater Hobart" = Hobart_coastline ,
                      "Newcastle - Maitland" = Newcastle_coastline ,
                      "Cairns" = Cairns_coastline ,
                      "Greater Adelaide" = Adelaide_coastline ,
                      "Greater Darwin" = Darwin_coastline)
  
  knitting <- isTRUE(getOption('knitr.in.progress'))
  coastline <- cbind(coastline[piece==1] , randomSample ) 
  plot <- coastline %>% ggplot(aes(x = long, y = lat,order = order, group = interaction(piece, group)) ) +
    geom_path(color = theGrey) +
    ggplot2::coord_map() +
    ggplot2::theme_void(base_family = if (knitting) "helvet" else "") +
    theme(legend.position="none") +
    theme(axis.line=element_blank(), axis.ticks=element_blank())  +
    geom_segment(data = coastline , aes(x = orig_lon, y = orig_lat, xend = dest_lon, yend =  dest_lat)  , color = "#F68B33")+
    geom_point(data = coastline , aes(dest_lon, dest_lat , size = coastline$jobs_weight ), color = "#AEAEAE" )
  
  if (region == "Greater Perth"){
    plot <- Perth_coastline  %>% ggplot(aes(x = long, y = lat,order = order, group = interaction(piece, group)) ) +
      geom_path(color = theGrey) +
      ggplot2::coord_map(xlim = c(115.5, 116.31), 
                         ylim = c(-32.7, -31.4))+
      ggplot2::theme_void(base_family = if (knitting) "helvet" else "") +
      theme(legend.position="none") +
      theme(axis.line=element_blank(), axis.ticks=element_blank())  +
      geom_segment(data = coastline , aes(x = orig_lon, y = orig_lat, xend = dest_lon, yend =  dest_lat)  , color = "#F68B33")+
      geom_point(data = coastline , aes(dest_lon, dest_lat , size = coastline$jobs_weight ), color = "#AEAEAE" )
  }
  
  assign(paste0(region," migrant plot", yearsInAustralia," years"), plot , envir= globalenv())
  
  # if (saveMigrant == "yes"){
  #   if (user == "hbatrouney"){
  #     ggsave(plot , filename = paste0("C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/CityDandelions/MigCityDandelion",region,year,"Year", yearsInAustralia,".png"))    
  #   } else if (user == "dhourani"){
  #     ggsave(plot , filename = paste0("/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/CityDandelions/MigrantCityDandelion",region,year,"Year", yearsInAustralia,".png"))
  #   }
  # }
  
}

#Call function (don't change this)
for (j in migrantRegionList) {
  MigrantCityDandelions(yr = year , region = j , yearsInAustralia = 1)
}

for (j in migrantRegionList) {
  MigrantCityDandelions(yr = year , region = j , yearsInAustralia = 3)
}

for (j in migrantRegionList) {
  MigrantCityDandelions(yr = year , region = j , yearsInAustralia = 5)
}

for (j in migrantRegionList) {
  MigrantCityDandelions(yr = year , region = j , yearsInAustralia = 10)
}

