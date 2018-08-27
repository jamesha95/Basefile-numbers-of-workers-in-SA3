library(data.table)
library(tidyverse)
library(Hmisc)
library(hutils)
library(magrittr)
#Choose between "dhourani" and "jaha" (make sure it's in quotation marks)
user <- "jaha" 

if (user == "dhourani"){
setwd("/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure") 
basefileSA2 <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/basefile_workSA2.csv")
basefileDZN <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/basefile_workDZN.csv")
workplaceDataDZN <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/workplaceDataDZN.csv")
workplaceDataSA2 <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/workplaceDataSA2.csv")
workplaceDataDZNwithCorro <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/workplaceDataDZNwithCorrespondences.csv")
workplaceDataSA2withCorro <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/workplaceDataSA2withCorrespondences.csv")
migrantBasefileSA2 <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/migrantBasefile.csv")
migrantWorkplaceDataSA2 <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/migrantWorkplaceDataSA2.csv")
}else if (user == "jaha"){
setwd("/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure")
basefileSA2 <- fread("/Users/jaha/Documents/Spatial structure of cities/Basefile/basefile_workSA2.csv")
basefileDZN <- fread("/Users/jaha/Documents/Spatial structure of cities/Basefile/basefile_workDZN.csv")
workplaceDataDZN <- fread("/Users/jaha/Documents/Spatial structure of cities/Basefile/workplaceDataDZN.csv")
workplaceDataSA2 <- fread("/Users/jaha/Documents/Spatial structure of cities/Basefile/workplaceDataSA2.csv")
workplaceDataDZNwithCorro <- fread("/Users/jaha/Documents/Spatial structure of cities/Basefile/workplaceDataDZNwithCorrespondences.csv")
workplaceDataSA2withCorro <- fread("/Users/jaha/Documents/Spatial structure of cities/Basefile/workplaceDataSA2withCorrespondences.csv")
migrantBasefileSA2 <- fread("/Users/jaha/Documents/Spatial structure of cities/Basefile/migrantBasefile.csv")
migrantWorkplaceDataSA2 <- fread("/Users/jaha/Documents/Spatial structure of cities/Basefile/migrantWorkplaceDataSA2.csv")
}

#Percentage of monocentric / polycentric / constrained dispersion jobs (the classification of SA2 areas that fall within each spatial structure correspond to those in the 'Dandelions.R' code)
spatial_structure_classification <- workplaceDataSA2withCorro %>% copy
#Monocentric jobs
work.listCBD <- c("Sydney - Haymarket - The Rocks" , "Pyrmont - Ultimo" , "Surry Hills")
spatial_structure_classification[work %ein% work.listCBD  , monocentric := 1]
#Polycentric jobs
work.listSubcentres <- c("North Sydney - Lavender Bay" , "Parramatta - Rosehill" , "Macquarie Park - Marsfield" , "St Leonards - Naremburn" , "Homebush Bay - Silverwater" , "Chatswood (East) - Artarmon" , "Baulkham Hills (West) - Bella Vista" , "Mascot - Eastlakes")
spatial_structure_classification[work %ein% work.listSubcentres  , polycentric := 1]
#Constrained dispersion jobs
remove <- c(work.listCBD , work.listSubcentres )
work.listEverywhereElse <- spatial_structure_classification[work_region == "Greater Sydney" & core_city == 1][[1]] %>% unique %>% .[!  . %in% remove]
spatial_structure_classification[work %ein% work.listEverywhereElse  , constrainedDispersion := 1] 
#Calculate spatial structure type percentages
spatial_structure_classification[work_region == "Greater Sydney" & core_city == 1 , .(total = sum(workers)) , by = .(year , monocentric , polycentric , constrainedDispersion)] %>% 
  .[ , perc := total/sum(total) , by = year] %>% print


#Average beeline commute to work in major cities
basefileDZN[work_region == live_region][, `:=`(weighted_distance_by_city = distance_to_work * workers / sum(workers)) , by = .(year,work_region)][
  , .(average_distance = sum(weighted_distance_by_city) , total_workers = sum(workers)) , by = .(year,work_region)] 

#Percentile distance to work in major cities - just use DZN to avoid getting zeroes on the smallest percentile
# for(city in c("Greater Sydney")){
#   city %>% print
#   for (year_i in c("2011" , "2016")){
#     analysisSet <- basefileSA2[work_region == live_region & year == year_i & work_region==city & core_city_work == 1]
#     year_i %>% print
#     for (i in 1:10){
#       wtd.quantile (x=analysisSet$distance_to_work , probs= i/10 , na.rm = FALSE , weight =analysisSet$workers) %>% print
#     }
#     "99th percentile" %>% print
#     wtd.quantile (x=analysisSet$distance_to_work , probs= 99/100 , na.rm = FALSE , weight =analysisSet$workers) %>% print
#   }
# }

#

#WARNING WARNING - if this differs to what you've seen before it's because i've taken out the core city work indicator
citylist <-c("Greater Sydney", "Greater Melbourne","Greater Brisbane", "Greater Perth","Greater Adelaide")
for(city in citylist){
  #city %>% print
  for (year_i in c("2011","2016")){
    #for people who live and work in the same city, use the first line. To include people who commute in from out of town, use the second line.
    # analysisSet <- basefileDZN[work_region == live_region & year == year_i & work_region==city ]
    analysisSet <- basefileDZN[year == year_i & work_region==city ]
   # year_i %>% print
    for (i in 1:9){
      q <- wtd.quantile (x=analysisSet$distance_to_work , probs= i/10 , na.rm = FALSE , weight =analysisSet$workers) # %>% print
      if (i == 1) {
        assign("x", q ,envir = globalenv())
      } else {
        q <- cbind(x,q)
      assign("x", q ,globalenv())
      }
     if (i==9) { assign(paste0(city, year_i, "file"), x , globalenv())}
    }
    if (year_i == '2016'){
     rowa <- get(paste0(city,"2011","file"))
     rowb <- get(paste0(city,"2016","file"))
     assign(paste0(city,"combinedfile"),rbind(rowa,rowb))
     #fwrite(get(paste0(city,"combinedfile")) , file = paste0("/Users/jaha/Documents/Spatial structure of cities/",city,"Haversine Commutes.csv"))
    }
    # "99th percentile" %>% print
    # wtd.quantile (x=analysisSet$distance_to_work , probs= 99/100 , na.rm = FALSE , weight =analysisSet$workers) %>% print
  }
}



#Average beeline commute to work for CBD workers - they are fairly close (including the totals) so use DZN here 
basefileSA2[work_region == live_region][work == "Sydney - Haymarket - The Rocks"][
  , `:=`(weighted_distance = distance_to_work * workers / sum(workers) , total = sum(workers) ) , by = .(year)][
    , .(total_distance = sum(weighted_distance) , total_workers = sum(workers)) , by = .(year)] 

basefileDZN[work_region == live_region][work_SA2 == "Sydney - Haymarket - The Rocks"][
  , `:=`(weighted_distance = distance_to_work * workers / sum(workers) , total = sum(workers) ) , by = .(year)][
    , .(total_distance = sum(weighted_distance) , total_workers = sum(workers)) , by = .(year)] 



#Change in workers by top 10 suburbs - use SA2 workplace file as it is different to DZN, where the destination zones do not sum to SA2s properly in 2011
cityList2 <- c("Greater Sydney" , "Greater Melbourne" , "Greater Brisbane")
for (city in cityList2){
workplaceDataSA2withCorro[work_region == city][
  , .(sumWorkers = sum(workers)) , by = .(year, work)][
    , percWorkers := (sumWorkers / sum(sumWorkers)) * 100 , by = .(year)] %>% 
  reshape(., idvar = "work", timevar = "year", direction = "wide") %>% 
  .[ , percChange := ((sumWorkers.2016 / sumWorkers.2011) - 1)*100] %>%
  .[order(-sumWorkers.2016)] %>% 
  .[1:20] %>% print
  #%>% write.csv( . , "/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/Tables/SA2 Sydney top 20 change work.csv")
}

workplaceDataDZN[work_region == "Greater Sydney"][
  , .(sumWorkers = sum(workers)) , by = .(year, work_SA2)][
    , percWorkers := (sumWorkers / sum(sumWorkers)) * 100 , by = .(year)] %>% 
  reshape(., idvar = "work_SA2", timevar = "year", direction = "wide") %>% 
  .[ , aveAnnualPercChange := ((sumWorkers.2016 / sumWorkers.2011)  - 1)*100] %>%
  .[order(-sumWorkers.2016)] %>% 
  .[1:20] 


#Average workplace distance from CBD - probably use DZN for this one for greater precision.
#SA2
for(city in c("Greater Sydney")){
  city %>% print
      analysisSet <- workplaceDataSA2[work_region==city & core_city == 1]
    if (city == "Greater Sydney"){
      analysisSet$main_city_lat <- workplaceDataSA2[work == "Sydney - Haymarket - The Rocks" & year == "2016", .SD , .SDcols = "dest_lat"][1,1]
      analysisSet$main_city_lon <- workplaceDataSA2[work == "Sydney - Haymarket - The Rocks" & year == "2016", .SD , .SDcols = "dest_lon"][1,1]
      }
    analysisSet[ , workplaceDistanceFromCBD := haversine_distance(dest_lat , dest_lon , main_city_lat , main_city_lon)][
      , .(sumWorkers = sum(workers) , workplaceDistanceFromCBD = mean(workplaceDistanceFromCBD)) , by = .(year, work)][
        , percWorkers := (sumWorkers / sum(sumWorkers)) , by = .(year)][
          , weightedWorkplaceDistanceFromCBD := percWorkers * workplaceDistanceFromCBD][
            , .(aveWorkplaceDistanceFromCBD = sum(weightedWorkplaceDistanceFromCBD)) , by = year] %>% print
}

#DZN
for(city in c("Greater Sydney" , "Greater Melbourne")){
  city %>% print
  analysisSet <- workplaceDataDZN[work_region==city ] #warning - if this looks different to previous stats its because i got rid of the core city indicator
  if (city == "Greater Sydney"){
    analysisSet$main_city_lat <- workplaceDataDZN[work == "113371053" & year == "2016", .SD , .SDcols = "dest_lat"][1,1]
    analysisSet$main_city_lon <- workplaceDataDZN[work == "113371053" & year == "2016", .SD , .SDcols = "dest_lon"][1,1]
  } else if (city == "Greater Melbourne"){
    analysisSet$main_city_lat <- workplaceDataDZN[work == "211221039" & year == "2016", .SD , .SDcols = "dest_lat"][1,1]
    analysisSet$main_city_lon <- workplaceDataDZN[work == "211221039" & year == "2016", .SD , .SDcols = "dest_lon"][1,1]
  }
  analysisSet[ , workplaceDistanceFromCBD := haversine_distance(dest_lat , dest_lon , main_city_lat , main_city_lon)][
    , .(sumWorkers = sum(workers) , workplaceDistanceFromCBD = mean(workplaceDistanceFromCBD)) , by = .(year, work)][
      , percWorkers := (sumWorkers / sum(sumWorkers)) , by = .(year)][
        , weightedWorkplaceDistanceFromCBD := percWorkers * workplaceDistanceFromCBD][
          , .(aveWorkplaceDistanceFromCBD = sum(weightedWorkplaceDistanceFromCBD)) , by = year] %>% print
}

#Average beeline distance to work for top 20 cities
basefileDZN <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/basefile_workDZN.csv") %>% .[ , c("work" , "work_SA2" , "work_region" , "workers" , "distance_to_work" , "year" , "live_region")]
SA2_to_SUA <- fread("C:/Users/dhourani/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Data/SA2_SUA_2016_AUST.csv") %>% .[ , c("SA2_NAME_2016" , "SUA_NAME_2016")]
setkey(SA2_to_SUA , SA2_NAME_2016)
setkey(basefileDZN , work_SA2)
basefileDZN <- basefileDZN[SA2_to_SUA] %>% setnames(. , "SUA_NAME_2016" , "work_SUA")

large_cities_list <- c("Greater Sydney" , "Greater Melbourne" , "Greater Brisbane" , "Greater Perth" , "Greater Darwin", "Greater Adelaide" , "Australian Capital Territory" , "Greater Hobart")
small_cities_list <- c("Newcastle - Maitland" , "Gold Coast - Tweed Heads" , "Sunshine Coast" , "Wollongong" , "Geelong" , "Townsville" , "Cairns" , "Toowoomba" , "Ballarat" , "Bendigo" , "Albury - Wodonga" , "Mackay")
top_20_cities_list <- c(small_cities_list , large_cities_list)

basefileDZN[work_region %ein% large_cities_list , top_20_city := work_region]
basefileDZN[work_SUA %in% small_cities_list , top_20_city := work_SUA]

basefileDZN[work_region == live_region & !is.na (top_20_city) & distance_to_work < 250][, `:=`(weighted_distance_by_city = distance_to_work * workers / sum(workers)) , by = .(year,top_20_city)][
  , .(average_distance = sum(weighted_distance_by_city) ) , by = .(year,top_20_city)] %>% 
  fwrite (. , "C:/Users/dhourani/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/Tables/Top 20 cities distance to work change.csv")



############################################################################################################################################################################################################
#MIGRANT ANALYSIS
############################################################################################################################################################################################################
migrantAnalysis <- migrantWorkplaceDataSA2 %>% copy
#Monocentric jobs
work.listCBD <- c("Sydney - Haymarket - The Rocks" , "Pyrmont - Ultimo" , "Surry Hills")
migrantAnalysis[work %ein% work.listCBD  , monocentric := 1]

#Workplaces of first year migrants
migrantAnalysis[work_region == "Greater Sydney" & yearsFromArrival == 1 & core_city == 1 ][
  , .(workersNonStuds = sum(workersNonStuds)) , by = .(year, work , yearsFromArrival)][
    , percWorkers := (workersNonStuds / sum(workersNonStuds)) * 100 , by = .(year , yearsFromArrival)] %>% 
  .[order(-workersNonStuds)] %>%
  .[1:30] #%>% fwrite( . , "/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/Tables/SA2 Sydney migrant workplaces first year.csv")

#Workplaces of fifth year migrants
migrantAnalysis[work_region == "Greater Sydney" & yearsFromArrival == 5 & core_city == 1 ][
  , .(workersNonStuds = sum(workersNonStuds)) , by = .(year, work , yearsFromArrival)][
    , percWorkers := (workersNonStuds / sum(workersNonStuds)) * 100 , by = .(year , yearsFromArrival)] %>% 
  .[order(-workersNonStuds)] %>% 
  .[1:30] #%>% fwrite( . , "/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/Tables/SA2 Sydney migrant workplaces fifth year.csv")

#Workplaces of tenth year migrants
migrantAnalysis[work_region == "Greater Sydney" & yearsFromArrival == 10 & core_city == 1 ][
  , .(workersNonStuds = sum(workersNonStuds)) , by = .(year, work , yearsFromArrival)][
    , percWorkers := (workersNonStuds / sum(workersNonStuds)) * 100 , by = .(year , yearsFromArrival)] %>% 
  .[order(-workersNonStuds)] %>% 
  .[1:30] #%>% fwrite( . , "/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/Tables/SA2 Sydney migrant workplaces tenth year.csv")

#Per cent monocentric
migrantAnalysis[work_region == "Greater Sydney" & core_city == 1 , .(total = sum(workersNonStuds)) , by = .(year , monocentric , yearsFromArrival)] %>% 
  .[ , perc := total/sum(total) , by = .(year , yearsFromArrival)] %>% print


