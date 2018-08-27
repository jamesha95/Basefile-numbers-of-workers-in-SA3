library(data.table)
library(Hmisc)
library(hutils)
library(ASGS)
library(magrittr)
library(grattanCharts)
library(tmap)

#Choose between "dhourani" and "jaha" (make sure it's in quotation marks)
user <- "dhourani" 

if (user == "dhourani"){
setwd("/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure") 
workplaceDataDZNwithCorrespondences <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/workplaceDataDZNwithCorrespondences.csv")
}else if (user == "jaha"){
setwd("/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure")
workplaceDataDZNwithCorrespondences <- fread("/Users/jaha/Documents/Spatial structure of cities/Basefile/workplaceDataDZNwithCorrespondences.csv")
}

city_list <-c("Greater Sydney" , "Greater Melbourne", "Greater Brisbane" , "Greater Perth" , "Greater Adelaide" ,"Greater Hobart" , "Australian Capital Territory" , "Greater Darwin") 

analysisSet <- workplaceDataDZNwithCorrespondences %>% copy
  
main_city_lat_Sydney <- workplaceDataDZNwithCorrespondences[work == "113371053" , .SD , .SDcols = "dest_lat"][1,1] 
main_city_lon_Sydney <- workplaceDataDZNwithCorrespondences[work == "113371053" , .SD , .SDcols = "dest_lon"][1,1] 
main_city_lat_Melbourne <- workplaceDataDZNwithCorrespondences[work == "211221039" , .SD , .SDcols = "dest_lat"][1,1]
main_city_lon_Melbourne <- workplaceDataDZNwithCorrespondences[work == "211221039" , .SD , .SDcols = "dest_lon"][1,1]
main_city_lat_Adelaide <- workplaceDataDZNwithCorrespondences[work == "410011006" , .SD , .SDcols = "dest_lat"][1,1]
main_city_lon_Adelaide <- workplaceDataDZNwithCorrespondences[work == "410011006" , .SD , .SDcols = "dest_lon"][1,1]
main_city_lat_Brisbane <- workplaceDataDZNwithCorrespondences[work == "311051684" , .SD , .SDcols = "dest_lat"][1,1]
main_city_lon_Brisbane <- workplaceDataDZNwithCorrespondences[work == "311051684" , .SD , .SDcols = "dest_lon"][1,1]
main_city_lat_Perth <- workplaceDataDZNwithCorrespondences[work == "510411094" , .SD , .SDcols = "dest_lat"][1,1]
main_city_lon_Perth <- workplaceDataDZNwithCorrespondences[work == "510411094" , .SD , .SDcols = "dest_lon"][1,1]
main_city_lat_Hobart <- workplaceDataDZNwithCorrespondences[work == "610270001" , .SD , .SDcols = "dest_lat"][1,1]
main_city_lon_Hobart <- workplaceDataDZNwithCorrespondences[work == "610270001" , .SD , .SDcols = "dest_lon"][1,1]
main_city_lat_Canberra <- workplaceDataDZNwithCorrespondences[work == "810531112" , .SD , .SDcols = "dest_lat"][1,1]
main_city_lon_Canberra <- workplaceDataDZNwithCorrespondences[work == "810531112" , .SD , .SDcols = "dest_lon"][1,1]
main_city_lat_Darwin <- workplaceDataDZNwithCorrespondences[work == "710021194" , .SD , .SDcols = "dest_lat"][1,1]
main_city_lon_Darwin <- workplaceDataDZNwithCorrespondences[work == "710021194" , .SD , .SDcols = "dest_lon"][1,1]




analysisSet[work_region == "Greater Sydney" , main_city_lat := main_city_lat_Sydney]
analysisSet[work_region == "Greater Sydney" , main_city_lon := main_city_lon_Sydney]
analysisSet[work_region == "Greater Melbourne" , main_city_lat := main_city_lat_Melbourne]
analysisSet[work_region == "Greater Melbourne" , main_city_lon := main_city_lon_Melbourne]
analysisSet[work_region == "Greater Adelaide" , main_city_lat := main_city_lat_Adelaide]
analysisSet[work_region == "Greater Adelaide" , main_city_lon := main_city_lon_Adelaide]
analysisSet[work_region == "Greater Brisbane" , main_city_lat := main_city_lat_Brisbane]
analysisSet[work_region == "Greater Brisbane" , main_city_lon := main_city_lon_Brisbane]
analysisSet[work_region == "Greater Perth" , main_city_lat := main_city_lat_Perth]
analysisSet[work_region == "Greater Perth" , main_city_lon := main_city_lon_Perth]
analysisSet[work_region == "Greater Hobart" , main_city_lat := main_city_lat_Hobart]
analysisSet[work_region == "Greater Hobart" , main_city_lon := main_city_lon_Hobart]
analysisSet[work_region == "Australian Capital Territory" , main_city_lat := main_city_lat_Canberra]
analysisSet[work_region == "Australian Capital Territory" , main_city_lon := main_city_lon_Canberra]
analysisSet[work_region == "Greater Darwin" , main_city_lat := main_city_lat_Darwin]
analysisSet[work_region == "Greater Darwin" , main_city_lon := main_city_lon_Darwin]

analysisSet[ , workplaceDistanceFromCBD := haversine_distance(dest_lat , dest_lon , main_city_lat , main_city_lon)] 

#Hobart is a special case because the city centre DZN changed code between 2011 and 2016 so manually adjust workplace distance from CBD for CBD to 0
analysisSet[work == "610270001" , workplaceDistanceFromCBD := 0]

analysisSet2 <- analysisSet %>% copy
#Percentile distance to work in major cities - just use DZN because no real difference between DZN and SA2 for this table
for(city in city_list){
  for (year_i in c("2011")){
    for (i in 1:5){
      analysisSet2 <- analysisSet %>% copy
      analysisSet2 <- analysisSet2[work_region == city & year == year_i]
      analysisSet[work_region == city ,':=' (quintile1 = wtd.quantile (x=analysisSet2$workplaceDistanceFromCBD , probs= 1/5 , na.rm = FALSE , weight =analysisSet2$workers) ,
                                                                      quintile2 = wtd.quantile (x=analysisSet2$workplaceDistanceFromCBD , probs= 2/5 , na.rm = FALSE , weight =analysisSet2$workers) ,
                                                                      quintile3 = wtd.quantile (x=analysisSet2$workplaceDistanceFromCBD , probs= 3/5 , na.rm = FALSE , weight =analysisSet2$workers) ,
                                                                      quintile4 = wtd.quantile (x=analysisSet2$workplaceDistanceFromCBD , probs= 4/5 , na.rm = FALSE , weight =analysisSet2$workers) ,
                                                                      quintile5 = wtd.quantile (x=analysisSet2$workplaceDistanceFromCBD , probs= 5/5 , na.rm = FALSE , weight =analysisSet2$workers) 
      )]
    }
  }
}


quintile_change <- analysisSet[workplaceDistanceFromCBD <= quintile1 , quintile := 1][
  workplaceDistanceFromCBD > quintile1 & workplaceDistanceFromCBD <= quintile2 , quintile := 2][
    workplaceDistanceFromCBD > quintile2 & workplaceDistanceFromCBD <= quintile3 , quintile := 3][
      workplaceDistanceFromCBD > quintile3 & workplaceDistanceFromCBD <= quintile4 , quintile := 4][
        workplaceDistanceFromCBD > quintile4 & workplaceDistanceFromCBD <= quintile5 , quintile := 5]
      

quintile_change[quintile %in% c(1,2,3,4,5), .(workers = sum(workers)) , by = .(year, work_region , quintile)] %>% 
  reshape(., idvar = c("quintile" , "work_region") , timevar = "year", direction = "wide") %>% fwrite(. ,"/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/IMO suburbs growth/IMO_quintiles.csv")


for(city in city_list){
  IMO_shapefile5 <- DZN_2011 %>% copy
  IMO_shapefile5@data <- merge(IMO_shapefile5@data , quintile_change[year =="2011"] , by.x= "DZN_CODE11" , by.y = "work") %>% as.data.table
  IMO_shapefile5 <- IMO_shapefile5[IMO_shapefile5$"work_region" == city,]
  
  Growth_quintiles <- tm_shape(IMO_shapefile5) +
    tm_fill ("quintile", 
             title= "" , 
             palette = gpal(7, reverse = TRUE), 
             title.text.size=2, 
             colorNA = "white" ) +
    tm_borders("grey20") +
    tm_view (alpha = 0.7, 
             basemaps.alpha = 2, 
             basemaps = "Stamen.TonerLite") +
    tm_layout(legend.position = c("left","top"),
              legend.text.size = 0.6 ,
              legend.width = 1)+
    tm_scale_bar(position=c("right", "bottom"), 
                 color.dark = "#D4582A", 
                 color.light = "#FFE07F") 
  
  
  assign(paste0("Growth_quintiles", city) ,Growth_quintiles , globalenv())
  save_tmap(Growth_quintiles, paste0("C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/IMO suburbs growth/Maps/",city,"quintiles.html"))
}