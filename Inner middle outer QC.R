library(data.table)
library(Hmisc)
library(hutils)
library(ASGS)
library(magrittr)
library(grattanCharts)
library(tmap)

#Choose between "dhourani" and "jaha" (make sure it's in quotation marks)
user <- "jaha" 

if (user == "dhourani"){
setwd("/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure") 
workplaceDataDZNwithCorrespondences <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/workplaceDataDZNwithCorrespondences.csv")
}else if (user == "jaha"){
setwd("/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure")
workplaceDataDZNwithCorrespondences <- fread("/Users/jaha/Documents/Spatial structure of cities/Basefile/workplaceDataDZNwithCorrespondences.csv")
}

city_list <-c("Greater Sydney" , "Greater Melbourne", "Greater Brisbane" , "Greater Perth" , "Greater Adelaide" , "Australian Capital Territory") #,"Greater Hobart", "Greater Darwin") 

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



#Hobart is a special case because the city centre DZN changed code between 2011 and 2016 so manually adjust workplace distance from CBD for CBD to 0
analysisSet[work == "610270001" , workplaceDistanceFromCBD := 0]

#Using 4 specifications - CBD and surrounds: 0-2km; Inner 2-10km ; Middle 10-20 km ; Outer >20km
IMO4_category_0 <- 2
IMO4_category_1 <- 10
IMO4_category_2 <- 20

jtw_IMO4 <- analysisSet[workplaceDistanceFromCBD < IMO4_category_0 , IMO := "CBD_surrounds"][
  workplaceDistanceFromCBD > IMO4_category_0 & workplaceDistanceFromCBD < IMO4_category_1 , IMO := "Inner"][
  workplaceDistanceFromCBD > IMO4_category_1 & workplaceDistanceFromCBD < IMO4_category_2 , IMO := "Middle"][
    workplaceDistanceFromCBD > IMO4_category_2 , IMO := "Outer"]

jtw_IMO4_2016 <- jtw_IMO4[year == "2016"]

jtw_IMO4[IMO %in% c("CBD_surrounds", "Inner", "Middle" ,"Outer"), .(workers = sum(workers)) , by = .(year, work_region , IMO)] %>% 
  reshape(., idvar = c("IMO" , "work_region") , timevar = "year", direction = "wide") %>% fwrite(. ,"/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/IMO suburbs growth/IMO4QC.csv")

#Using 6 specifications - CBD and surrounds: 0-2km; Inner 2-5km ; Inner-Middle 5-10km ; Middle 10-15km ; Middle-Outer 15-20km ; Outer >20km
IMO6_category_0 <- 2
IMO6_category_1 <- 5
IMO6_category_2 <- 10
IMO6_category_3 <- 15
IMO6_category_4 <- 20

jtw_IMO6 <- analysisSet[workplaceDistanceFromCBD < IMO6_category_0 , IMO := "CBD_surrounds"][
  workplaceDistanceFromCBD > IMO6_category_0 & workplaceDistanceFromCBD < IMO6_category_1 , IMO := "Inner"][
    workplaceDistanceFromCBD > IMO6_category_1 & workplaceDistanceFromCBD < IMO6_category_2 , IMO := "Inner-Middle"][
    workplaceDistanceFromCBD > IMO6_category_2 & workplaceDistanceFromCBD < IMO6_category_3 , IMO := "Middle"][
      workplaceDistanceFromCBD > IMO6_category_3 & workplaceDistanceFromCBD < IMO6_category_4 , IMO := "Middle-Outer"][
    workplaceDistanceFromCBD > IMO6_category_4 , IMO := "Outer"]

jtw_IMO6_2016 <- jtw_IMO6[year == "2016"]

jtw_IMO6[IMO %in% c("CBD_surrounds" , "Inner", "Inner-Middle", "Middle", "Middle-Outer", "Outer"), .(workers = sum(workers)) , by = .(year, work_region , IMO)] %>% 
  reshape(., idvar = c("IMO" , "work_region") , timevar = "year", direction = "wide") %>% fwrite(. ,"C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/IMO suburbs growth/IMO6.csv")



#Draw maps to visualise IMO boundaries

  for(city in city_list){
    city %>% print
  
  IMO_shapefile4 <- DZN_2016 %>% copy
  IMO_shapefile4@data <- merge(IMO_shapefile4@data , jtw_IMO4_2016 , by.x= "DZN_CODE16" , by.y = "work") %>% as.data.table
  IMO_shapefile4 <- IMO_shapefile4[IMO_shapefile4$"work_region" == city,]
  
  IMO_map <- tm_shape(IMO_shapefile4) +
    tm_fill ("IMO", 
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
  

   assign(paste0("IMO4_map", city) ,IMO_map , globalenv())
   save_tmap(IMO_map, paste0("/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/IMO suburbs growth/",city,"4specsQC.html"))
   
   IMO_shapefile6 <- DZN_2016 %>% copy
   IMO_shapefile6@data <- merge(IMO_shapefile6@data , jtw_IMO6_2016 , by.x= "DZN_CODE16" , by.y = "work") %>% as.data.table
   IMO_shapefile6 <- IMO_shapefile6[IMO_shapefile6$"work_region" == city,]
   
   IMO_map <- tm_shape(IMO_shapefile6) +
     tm_fill ("IMO", 
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
 
    assign(paste0("IMO6_map", city) ,IMO_map , globalenv())  
    
    save_tmap(IMO_map, paste0("/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/IMO suburbs growth/",city,"6specsQC.html"))
}

###############################################################################################################################################################
# Quintile Analysis
###############################################################################################################################################################
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


jtw_IMO_2 <- analysisSet[workplaceDistanceFromCBD <= quintile1 , IMO2 := 1][
  workplaceDistanceFromCBD > quintile1 & workplaceDistanceFromCBD <= quintile2 , IMO2 := 2][
    workplaceDistanceFromCBD > quintile2 & workplaceDistanceFromCBD <= quintile3 , IMO2 := 3][
      workplaceDistanceFromCBD > quintile3 & workplaceDistanceFromCBD <= quintile4 , IMO2 := 4][
        workplaceDistanceFromCBD > quintile4 & workplaceDistanceFromCBD <= quintile5 , IMO2 := 5]
      

jtw_IMO_2[IMO2 %in% c(1,2,3,4,5), .(workers = sum(workers)) , by = .(year, work_region , IMO2)] %>% 
  reshape(., idvar = c("IMO2" , "work_region") , timevar = "year", direction = "wide") %>% fwrite(. ,"/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/IMO suburbs growth/IMO_quintilesQC.csv")


for(city in city_list){
  IMO_shapefile5 <- DZN_2011 %>% copy
  IMO_shapefile5@data <- merge(IMO_shapefile5@data , jtw_IMO_2[year =="2011"] , by.x= "DZN_CODE11" , by.y = "work") %>% as.data.table
  IMO_shapefile5 <- IMO_shapefile5[IMO_shapefile5$"work_region" == city,]
  
  IMO_map_quintiles <- tm_shape(IMO_shapefile5) +
    tm_fill ("IMO2", 
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
  
  
  assign(paste0("IMO_map_quintiles", city) ,IMO_map_quintiles , globalenv())
  save_tmap(IMO_map_quintiles, paste0("/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/IMO suburbs growth/Maps/",city,"quintilesQC.html"))
}