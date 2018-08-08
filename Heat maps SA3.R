##INDEX
#1. Heat maps of % of city workers in various suburbs
#2. Heat maps of distance travelled to work
#3. Heat maps of mode of transport
#4. Heat maps of % workers who drive to work by workplace SA2 and income bracket
#5a. Heat maps of city growth

user <- "jamesha" #choose from hbatrouney or dhourani or jamesha
save <- "yes" #choose from save "yes" or "no" - only pick yes if you want to save dandelions

library(data.table)
library(grattanCharts)
library(maptools)
library(tmap)
library(ASGS)
library(hutils)
library(magrittr)

#############################################################################################################################################
#Heat maps of % of city workers in various suburbs
#############################################################################################################################################
# if (user == "hbatrouney")
# { basefile <- fread("C:/Users/hbatrouney/Documents/Spatial structure of cities/Basefile/basefile_workSA2.csv")
# } else if (user == "dhourani"){basefile <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/basefile_workSA2.csv")
# 
# 
# #cityList <- c("Sydney - Haymarket - The Rocks")
# cityList <- c("Sydney - Haymarket - The Rocks" , "Pyrmont - Ultimo" , "Surry Hills")
# #cityList <- c("Sydney - Haymarket - The Rocks" , "Surry Hills" , "Pyrmont - Ultimo" , "Potts Point - Woolloomooloo" , "Glebe - Forest Lodge" , "Newtown - Camperdown - Darlington" , "Redfern - Chippendale")
# 
# Perc_city_workers_data <- basefile[live_region == "Greater Sydney" & year == 2016][
#     ,`:=`(perc_city_workers = workers / sum(workers)), by =  live][
#       work %ein%  cityList, .(perc_city_workers = sum(perc_city_workers)), by =  live][
#       order(-perc_city_workers)][
#         , .(live, perc_city_workers)
#       ][perc_city_workers %>% is.na , perc_city_workers := 0]
# 
# #Read in shapefile for map
# liveShapefile_CBDworkers <- SA2_2016 
# liveShapefile_CBDworkers@data <- liveShapefile_CBDworkers@data %>% as.data.table
# liveShapefile_CBDworkers <- liveShapefile_CBDworkers[liveShapefile_CBDworkers$"GCC_NAME16" == "Greater Sydney",]
# liveShapefile_CBDworkers@data <- Perc_city_workers_data[liveShapefile_CBDworkers@data, on = "live==SA2_NAME16"]
# 
# perc_cat_CBDworker <- function(x, lower , upper, by, sep , above.char) {
#   labs <- c(paste(paste(seq(lower, upper - by, by = by)*100,"%"),
#                   paste(seq(lower + by, upper, by = by)*100,"%"),
#                   sep = sep),
#             paste(upper*100,"%", above.char, sep = ""))
#   breaks <- c(seq(lower, upper, by = by), Inf)
#   liveShapefile_CBDworkers$perc_city_workers_range <- cut(x, breaks = breaks,
#                                                right = FALSE, labels = labs)
#   assign("liveShapefile_CBDworkers" , liveShapefile_CBDworkers, envir = globalenv())
#   assign("breaks" , breaks, envir = globalenv())
# }
# 
# perc_cat_CBDworker(liveShapefile_CBDworkers$perc_city_workers , lower = 0, upper = 0.7, by = 0.1, sep = "-", above.char = "+")
# 
# cityWorkersHeatMapGreaterSydney <- tm_shape(liveShapefile_CBDworkers) +
#   tm_fill ("perc_city_workers_range", 
#            title = "Per cent CBD workers", 
#            palette = gpal(7, reverse = TRUE), 
#            title.text.size=2, 
#            colorNA = "white" ,
#            breaks = breaks ) +
#   tm_borders("grey20") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite") +
#   tm_layout(legend.position = c("left","top"),
#             legend.text.size = 0.6 ,
#             legend.width = 1)+
#   tm_scale_bar(position=c("right", "bottom"), 
#                color.dark = "#D4582A", 
#                color.light = "#FFE07F")
# 
# #Get core city indicator from basefile
# core_city_merger <- basefile[live_region == "Greater Sydney" , .(core_city = mean(core_city_live)) , by = live]
# liveShapefile_CBDworkers@data <- core_city_merger[liveShapefile_CBDworkers@data, on = "live==live"]
# liveShapefile_CBDworkers_core <- liveShapefile_CBDworkers[liveShapefile_CBDworkers$"core_city" == 1,]
# 
# cityWorkersHeatMapCoreSydney <- tm_shape(liveShapefile_CBDworkers_core) +
#   tm_fill ("perc_city_workers_range", 
#            title = "", 
#            palette = gpal(7, reverse = TRUE), 
#            title.text.size=2, 
#            colorNA = "white" ,
#            breaks = breaks ) +
#   tm_borders("grey20") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite")  +
#   tm_layout(legend.position = c("left","top"),
#             legend.text.size = 0.4 ,
#             legend.width = 1)+
#   tm_scale_bar(position=c("right", "bottom"), 
#                color.dark = "#D4582A", 
#                color.light = "#FFE07F")
# 
# if (save == "yes"){
#   if (user == "dhourani"){
#   save_tmap(cityWorkersHeatMapGreaterSydney, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/SydneyHeatMapCBDworkersAllSydneytest3.png")
#   save_tmap(cityWorkersHeatMapCoreSydney, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/SydneyHeatMapCBDworkersCoreSydney.png")
#   } else if (user == "hbatrouney"){
#   save_tmap(cityWorkersHeatMapGreaterSydney, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/SydneyHeatMapCBDworkersAllSydney.png")
#   save_tmap(cityWorkersHeatMapCoreSydney, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/SydneyHeatMapCBDworkersCoreSydney.png")  
#   }
# }
# #############################################################################################################################################
# #Heat maps of distance travelled to work
# #############################################################################################################################################
# if (user == "hbatrouney")
# { basefile <- fread("C:/Users/hbatrouney/Documents/Spatial structure of cities/Basefile/basefile_workDZN.csv")
# } else if (user == "dhourani"){basefile <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/basefile_workSA2.csv")}
# distance_data <- basefile[live_region == "Greater Sydney" & year == 2016 & live_region == work_region][
#   , `:=`(dist.workers = distance_to_work * workers)][
#   ,.(average_distance = sum(dist.workers) / sum(workers)), by =  live][
#     average_distance %>% is.na , average_distance := 0][
#       order(-average_distance)]
# 
# #Read in shapefile for map
# liveShapefileDistToWork <- SA2_2016 
# liveShapefileDistToWork@data <- liveShapefileDistToWork@data %>% as.data.table
# liveShapefileDistToWork <- liveShapefileDistToWork[liveShapefileDistToWork$"GCC_NAME16" == "Greater Sydney",]
# liveShapefileDistToWork@data <- distance_data[liveShapefileDistToWork@data, on = "live==SA2_NAME16"]
# 
# distance_cat <- function(x, lower = 0, upper = 25, by = 5,
#                             sep = "-", above.char = "+") {
#   labs2 <- c(paste(seq(lower, upper - by, by = by),
#                   seq(lower + by, upper, by = by),
#                   sep = sep),
#             paste(upper, above.char, sep = ""))
#   breaks2 <- c(seq(lower, upper, by = by), Inf)
#   
#   liveShapefileDistToWork$average_distance <- cut(x, breaks = breaks2 , right = FALSE, labels = labs2)
#   assign("liveShapefileDistToWork" , liveShapefileDistToWork, envir = globalenv())
#   assign("breaks2" , breaks2, envir = globalenv())
# }
# 
# distance_cat(liveShapefileDistToWork$average_distance)
# 
# distToWorkHeatMapGreaterSydney <-tm_shape(liveShapefileDistToWork) +
#   tm_fill ("average_distance", 
#            title = "kms to work", 
#            palette = gpal(7, reverse = TRUE), 
#            title.text.size=2, 
#            colorNA = "white" ,
#            breaks = breaks2) +
#   tm_borders("grey20") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite")  +
#   tm_layout(legend.position = c("left","top"),
#             legend.title.size = 0.8,
#             legend.text.size = 0.53 ,
#             legend.width = 1 )+
#   tm_scale_bar(position=c("right", "bottom"), 
#                color.dark = "#D4582A", 
#                color.light = "#FFE07F")
# 
# 
# #Get core city indicator from basefile
# core_city_merger <- basefile[live_region == "Greater Sydney" , .(core_city = mean(core_city_live)) , by = live]
# liveShapefileDistToWork@data <- core_city_merger[liveShapefileDistToWork@data, on = "live==live"]
# liveShapefileDistToWork_core <- liveShapefileDistToWork[liveShapefileDistToWork$"core_city" == 1,]
# 
# 
# distToWorkHeatMapCoreSydney <-tm_shape(liveShapefileDistToWork_core) +
#   tm_fill ("average_distance", 
#            title = "kms to work", 
#            palette = gpal(7, reverse = TRUE), 
#            title.text.size=2, 
#            colorNA = "white" ,
#            breaks = breaks2) +
#   tm_borders("grey20") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite")  +
#   tm_layout(legend.position = c("left","top"),
#             legend.title.size = 0.8,
#             legend.text.size = 0.53 ,
#             legend.width = 1 )+
#   tm_scale_bar(position=c("right", "bottom"), 
#                color.dark = "#D4582A", 
#                color.light = "#FFE07F")
# 
# 
# if (save == "yes"){
#   if (user == "dhourani"){
#     save_tmap(distToWorkHeatMapGreaterSydney, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/SydneyHeatMapAverageDistToWorkAllSydney.png")
#     save_tmap(distToWorkHeatMapCoreSydney, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/SydneyHeatMapAverageDistToWorkCoreSydney.png")
#   } else if (user == "hbatrouney"){
#     save_tmap(distToWorkHeatMapGreaterSydney, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/SydneyHeatMapAverageDistToWorkAllSydney.png")
#     save_tmap(distToWorkHeatMapCoreSydney, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/SydneyHeatMapAverageDistToWorkCoreSydney.png")  
#   }
# }
# 
# 
# #############################################################################################################################################
# #Heat maps of mode of transport
# #############################################################################################################################################
# if (user == "hbatrouney")
# {basefile <- fread("C:/Users/hbatrouney/Documents/Spatial structure of cities/Basefile/basefile_workSA2.csv")
# } else if (user == "dhourani"){basefile <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/basefile_workSA2.csv")}
# region_list_drivers <- c("Greater Sydney" , "Greater Melbourne")
# for (city in region_list_drivers) {
# #Heat map of vehicle users 
# transport_mode_data <- basefile[live_region == city & year == 2016 & live_region == work_region][
#   , `:=`(vehiclePerc.workers = vehiclePerc * workers)] %>% 
#   na.omit(. , invert = FALSE) %>% 
#   .[, .(average_vehicle_perc = sum(vehiclePerc.workers) / sum(workers)), by =  live] 
# 
# #Read in shapefile for map
# liveShapefileModeVehicle <- SA2_2016 
# liveShapefileModeVehicle@data <- liveShapefileModeVehicle@data %>% as.data.table
# liveShapefileModeVehicle <- liveShapefileModeVehicle[liveShapefileModeVehicle$"GCC_NAME16" == city,]
# liveShapefileModeVehicle@data <- transport_mode_data[liveShapefileModeVehicle@data, on = "live==SA2_NAME16"] %>% 
#   .[average_vehicle_perc %>% is.na , average_vehicle_perc:= 0] #turn to zero places where no one lives (ie zero denominator) - these include places like Rookwood Cemetery or Airport in Sydney
# 
# mode_perc_cat <- function(x, lower , upper , by , sep , above.char ) {
#   labs3 <-c(paste(paste(seq(lower, upper - by, by = by)*100,"%"),
#                   paste(seq(lower + by, upper, by = by)*100,"%"),
#                   sep = sep),
#             paste(upper*100,"%", above.char, sep = ""))
#   breaks3 <- c(seq(lower, upper, by = by), Inf)
#   
#   liveShapefileModeVehicle$average_vehicle_perc <- cut(x, breaks = breaks3 , right = FALSE, labels = labs3)
#   assign("liveShapefileModeVehicle" , liveShapefileModeVehicle, envir = globalenv())
#   assign("breaks3" , breaks3, envir = globalenv())
# }
# 
# mode_perc_cat(liveShapefileModeVehicle$average_vehicle_perc ,  lower = 0, upper = 0.8 , by = 0.2 , sep = "-", above.char = "+")
# 
# vehiclePercentageHeatMapGreaterCity <- tm_shape(liveShapefileModeVehicle) +
#   tm_fill ("average_vehicle_perc", 
#            title = "", 
#            palette = gpal(7, reverse = TRUE), 
#            title.text.size=2, 
#            colorNA = "white" ,
#            breaks = breaks3 ) +
#   tm_borders("grey20") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite") +
#   if (city == "Greater Melbourne"){
#   tm_layout(legend.outside.position = "left",
#             legend.outside = TRUE ,
#             legend.title.size = 0.8,
#             legend.text.size = 0.53 ,
#             legend.width = 1 )} else if (city == "Greater Sydney"){
#               tm_layout(legend.outside.position = "left",
#                         legend.outside = TRUE ,
#                         legend.title.size = 0.8,
#                         legend.text.size = 0.53 ,
#                         legend.width = 1 )
#             } #+
# #  tm_scale_bar(position=c("left", "bottom"), 
# #               color.dark = "#D4582A", 
# #               color.light = "#FFE07F") 
# 
# 
# #Get core city indicator from basefile
# core_city_merger <- basefile[live_region == city , .(core_city = mean(core_city_live)) , by = live]
# liveShapefileModeVehicle@data <- core_city_merger[liveShapefileModeVehicle@data, on = "live==live"]
# liveShapefileModeVehicle_core <- liveShapefileModeVehicle[liveShapefileModeVehicle$"core_city" == 1,]
# 
# vehiclePercentageHeatMapCoreCity <- tm_shape(liveShapefileModeVehicle_core) +
#   tm_fill ("average_vehicle_perc", 
#            title = "", 
#            palette = gpal(7, reverse = TRUE), 
#            title.text.size=2, 
#            colorNA = "white" ,
#            breaks = breaks3 ) +
#   tm_borders("grey20") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite") +
#   if (city == "Greater Melbourne"){
#   tm_layout(legend.outside.position = "left",
#             legend.outside = TRUE ,
#             legend.title.size = 0.8,
#             legend.text.size = 0.53 ,
#             legend.width = 1 )} else if (city == "Greater Sydney"){
#               tm_layout(legend.outside.position = "left",
#                         legend.outside = TRUE ,
#                         legend.title.size = 0.8,
#                         legend.text.size = 0.53 ,
#                         legend.width = 1 )
#             } #+
# #  tm_scale_bar(position=c("left", "bottom"), 
# #               color.dark = "#D4582A", 
# #               color.light = "#FFE07F") 
# 
# 
# if (save == "yes"){
#     save_tmap(vehiclePercentageHeatMapGreaterCity, paste0("C:/Users/",user,"/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/For report/HeatMapVehiclePercentage",city,".png"))
#     save_tmap(vehiclePercentageHeatMapCoreCity, paste0("C:/Users/",user,"/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/For report/HeatMapVehiclePercentage",city,"- core.png"))
# }
# 
# #Heat map of public transport users 
# # if (user == "hbatrouney")
# # {basefile <- fread("C:/Users/hbatrouney/Documents/Spatial structure of cities/Basefile/basefile_workSA2.csv")
# # } else if (user == "dhourani"){basefile <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/basefile_workSA2.csv")}
# #Heat map of PT users 
# transport_mode_data <- basefile[live_region == city & year == 2016 & live_region == work_region][
#   , `:=`(publicTransportPerc.workers = publicTransportPerc * workers)] %>% na.omit(. , invert = FALSE) %>% 
#   .[, .(average_public_transport_perc = sum(publicTransportPerc.workers) / sum(workers)), by =  live]
# 
# #Read in shapefile for map
# liveShapefileModePublicTransport <- SA2_2016 
# liveShapefileModePublicTransport@data <- liveShapefileModePublicTransport@data %>% as.data.table
# liveShapefileModePublicTransport <- liveShapefileModePublicTransport[liveShapefileModePublicTransport$"GCC_NAME16" == city,]
# liveShapefileModePublicTransport@data <- transport_mode_data[liveShapefileModePublicTransport@data, on = "live==SA2_NAME16"]
# 
# mode_perc_cat <- function(x, lower , upper , by , sep , above.char ) {
#   labs4 <-c(paste(paste(seq(lower, upper - by, by = by)*100,"%"),
#                   paste(seq(lower + by, upper, by = by)*100,"%"),
#                   sep = sep),
#             paste(upper*100,"%", above.char, sep = ""))
#   breaks4 <- c(seq(lower, upper, by = by), Inf)
#   
#   liveShapefileModePublicTransport$average_public_transport_perc <- cut(x, breaks = breaks4 , right = FALSE, labels = labs4)
#   assign("liveShapefileModePublicTransport" , liveShapefileModePublicTransport, envir = globalenv())
#   assign("breaks4" , breaks4, envir = globalenv())
# }
# 
# mode_perc_cat(liveShapefileModePublicTransport$average_public_transport_perc ,  lower = 0, upper = 0.8 , by = 0.2 , sep = "-", above.char = "+")
# 
# publicTransportPercentageHeatMapGreaterCity <- tm_shape(liveShapefileModePublicTransport) +
#   tm_fill ("average_public_transport_perc", 
#            title = "Proportion PT users by SA2 of residence", 
#            palette = gpal(7, reverse = TRUE), 
#            title.text.size=2, 
#            colorNA = "white" ,
#            breaks = breaks4 ) +
#   tm_borders("grey20") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite") +
#   tm_layout(legend.outside.position = "left",
#             legend.outside = TRUE ,
#             legend.text.size = 0.6 ,
#             legend.width = 1)#+
#   # tm_scale_bar(position=c("right", "bottom"), 
#   #              color.dark = "#D4582A", 
#   #              color.light = "#FFE07F")
# 
# 
# #Get core city indicator from basefile
# core_city_merger <- basefile[live_region == city , .(core_city = mean(core_city_live)) , by = live]
# liveShapefileModePublicTransport@data <- core_city_merger[liveShapefileModePublicTransport@data, on = "live==live"]
# liveShapefileModePublicTransport_core <- liveShapefileModePublicTransport[liveShapefileModePublicTransport$"core_city" == 1,]
# 
# publicTransportPercentageHeatMapCoreCity <- tm_shape(liveShapefileModePublicTransport_core) +
#   tm_fill ("average_public_transport_perc", 
#            title = "Proportion PT users by SA2 of residence", 
#            palette = gpal(7, reverse = TRUE), 
#            title.text.size=2, 
#            colorNA = "white" ,
#            breaks = breaks4 ) +
#   tm_borders("grey20") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite") +
#   tm_layout(legend.outside.position = "left",
#             legend.outside = TRUE ,
#             legend.text.size = 0.6 ,
#             legend.width = 1)#+
# # tm_scale_bar(position=c("right", "bottom"), 
# #              color.dark = "#D4582A", 
# #              color.light = "#FFE07F")
# 
# 
# if (save == "yes"){
#   save_tmap(publicTransportPercentageHeatMapGreaterCity, paste0("C:/Users/",user,"/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/For report/HeatMapPTPercentage",city,".png"))
#   save_tmap(publicTransportPercentageHeatMapCoreCity, paste0("C:/Users/",user,"/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/For report/HeatMapPTPercentage",city,"- core.png"))
# }
# }
# 
# #############################################################################################################################################
# #Heat maps of % workers who drive to work by workplace SA2 and income bracket
# #############################################################################################################################################
# driversFile <- fread("C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Data/Drive to work by income 2016/Drive by income and SA2 work 2016.csv")
# incomeDistFile <- fread("C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Data/Drive to work by income 2016/Income and SA2 work 2016.csv")
# 
# driversFile[ , ':=' (lowIncDrivers = driversFile$"$1-$149 ($1-$7,799)" 
#                      + driversFile$"$150-$299 ($7,800-$15,599)"  
#                      + driversFile$"$300-$399 ($15,600-$20,799)" 
#                      + driversFile$"$400-$499 ($20,800-$25,999)" 
#                      + driversFile$"$500-$649 ($26,000-$33,799)" 
#                      + driversFile$"$650-$799 ($33,800-$41,599)" , 
#                      highIncDrivers = driversFile$"$3,000 or more ($156,000 or more)") ] 
# 
# driversFile <- driversFile[ , c("work" , "lowIncDrivers" , "highIncDrivers" , "total")]
# 
# incomeDistFile[ , ':=' (lowInc = incomeDistFile$"$1-$149 ($1-$7,799)" 
#                      + incomeDistFile$"$150-$299 ($7,800-$15,599)"  
#                      + incomeDistFile$"$300-$399 ($15,600-$20,799)" 
#                      + incomeDistFile$"$400-$499 ($20,800-$25,999)" 
#                      + incomeDistFile$"$500-$649 ($26,000-$33,799)" 
#                      + incomeDistFile$"$650-$799 ($33,800-$41,599)" , 
#                      highInc = incomeDistFile$"$3,000 or more ($156,000 or more)") ] 
# 
# incomeDistFile <- incomeDistFile[ , c("work" , "lowInc" , "highInc" , "total")]
# setnames(incomeDistFile, "total", "totalIncEarners")
# 
# driversShapefile <- SA2_2016
# driversShapefile@data <- driversFile[SA2_2016@data , on = "work==SA2_NAME16" , nomatch = 0L]
# driversShapefile@data <- incomeDistFile[driversShapefile@data , on = "work==work" , nomatch = 0L] 
# 
# # driversShapefile@data <- driversShapefile@data[ , perclowIncDrivers := lowIncDrivers / sum(lowIncDrivers) , by = GCC_NAME16]
# # driversShapefile@data <- driversShapefile@data[ , perchighIncDrivers := highIncDrivers / sum(highIncDrivers) , by = GCC_NAME16]
# 
# driversShapefile@data <- driversShapefile@data[ , perclowIncDrivers := lowIncDrivers / lowInc ]
# driversShapefile@data <- driversShapefile@data[ , perchighIncDrivers := highIncDrivers / highInc]
# 
# 
# perc_cat_drivers <- function(x, lower , upper, by, sep , above.char) {
#   labs <- c(paste(paste(seq(lower, upper - by, by = by)*100,"%"),
#                   paste(seq(lower + by, upper, by = by)*100,"%"),
#                   sep = sep),
#             paste(upper*100,"%", above.char, sep = ""))
#   breaks <- c(seq(lower, upper, by = by), Inf)
#   driversShapefile$perclowIncDriversRange <- cut(x, breaks = breaks,
#                                           right = FALSE, labels = labs)
#   assign("driversShapefile" , driversShapefile, envir = globalenv())
#   assign("breaks" , breaks, envir = globalenv())
# }
# 
# #perc_cat_drivers(driversShapefile$perclowIncDrivers , lower = 0 , upper = 0.1 , by = 0.02 , sep = "-", above.char = "+")
# perc_cat_drivers(driversShapefile$perclowIncDrivers , lower = 0 , upper = 0.7 , by = 0.1 , sep = "-", above.char = "+")
# 
# perc_cat_drivers <- function(x, lower , upper, by, sep , above.char) {
#   labs <- c(paste(paste(seq(lower, upper - by, by = by)*100,"%"),
#                   paste(seq(lower + by, upper, by = by)*100,"%"),
#                   sep = sep),
#             paste(upper*100,"%", above.char, sep = ""))
#   breaks <- c(seq(lower, upper, by = by), Inf)
#   driversShapefile$perchighIncDriversRange <- cut(x, breaks = breaks,
#                                            right = FALSE, labels = labs)
#   assign("driversShapefile" , driversShapefile, envir = globalenv())
#   assign("breaks" , breaks, envir = globalenv())
# }
# 
# #perc_cat_drivers(driversShapefile$perchighIncDrivers , lower = 0 , upper = 0.1 , by = 0.02 , sep = "-", above.char = "+")
# perc_cat_drivers(driversShapefile$perchighIncDrivers , lower = 0 , upper = 0.7 , by = 0.1 , sep = "-", above.char = "+")
# 
# 
# driversShapefile <- driversShapefile[driversShapefile$"GCC_NAME16" == "Greater Sydney", ]
# 
# lowIncDriversSydney <- tm_shape(driversShapefile) +
#   tm_fill ("perclowIncDriversRange", 
#            title= "" , 
#            palette = gpal(7, reverse = TRUE), 
#            title.text.size=2, 
#            colorNA = "white" ,
#            breaks = breaks ) +
#   tm_borders("grey20") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite") +
#   tm_layout(legend.position = c("left","top"),
#             legend.text.size = 0.6 ,
#             legend.width = 1)+
#   tm_scale_bar(position=c("right", "bottom"), 
#                color.dark = "#D4582A", 
#                color.light = "#FFE07F") 
# 
# #Get core city indicator from basefile
# core_city_merger <- basefile[live_region == "Greater Sydney" , .(core_city = mean(core_city_work)) , by = work]
# driversShapefile@data <- core_city_merger[driversShapefile@data, on = "work==work"]
# lowIncDriversSydney_core <- driversShapefile[driversShapefile$"core_city" == 1,]
# 
# lowIncDriversCoreSydney <- tm_shape(lowIncDriversSydney_core) +
#   tm_fill ("perclowIncDriversRange", 
#            title= "" , 
#            palette = gpal(7, reverse = TRUE), 
#            title.text.size=2, 
#            colorNA = "white" ,
#            breaks = breaks ) +
#   tm_borders("grey20") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite") +
#   tm_layout(legend.position = c("left","top"),
#             legend.text.size = 0.6 ,
#             legend.width = 1)+
#   tm_scale_bar(position=c("right", "bottom"), 
#                color.dark = "#D4582A", 
#                color.light = "#FFE07F") 
# 
# 
# 
# if (save == "yes"){
#   if (user == "dhourani"){
#     save_tmap(lowIncDriversSydney, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/lowIncDriversGreaterSydney.png")
#     save_tmap(lowIncDriversCoreSydney, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/lowIncDriversCoreSydney.png")
#   } else if (user == "hbatrouney"){
#     save_tmap(lowIncDriversSydney, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/lowIncDriversGreaterSydney.png")
#     save_tmap(lowIncDriversCoreSydney, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/lowIncDriversCoreSydney.png")  
#   }
# }
# 
# 
# highIncDriversSydney <- tm_shape(driversShapefile) +
#   tm_fill ("perchighIncDriversRange", 
#            title= "" , 
#            palette = gpal(7, reverse = TRUE), 
#            title.text.size=2, 
#            colorNA = "white" ,
#            breaks = breaks ) +
#   tm_borders("grey20") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite") +
#   tm_layout(legend.position = c("left","top"),
#             legend.text.size = 0.6 ,
#             legend.width = 1)+
#   tm_scale_bar(position=c("right", "bottom"), 
#                color.dark = "#D4582A", 
#                color.light = "#FFE07F")
# 
# #Get core city indicator from basefile
# core_city_merger <- basefile[live_region == "Greater Sydney" , .(core_city = mean(core_city_work)) , by = work]
# driversShapefile@data <- core_city_merger[driversShapefile@data, on = "work==work"]
# highIncDriversSydney_core <- driversShapefile[driversShapefile$"core_city" == 1,]
# 
# highIncDriversCoreSydney <- tm_shape(highIncDriversSydney_core) +
#   tm_fill ("perchighIncDriversRange", 
#            title= "" , 
#            palette = gpal(7, reverse = TRUE), 
#            title.text.size=2, 
#            colorNA = "white" ,
#            breaks = breaks ) +
#   tm_borders("grey20") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite") +
#   tm_layout(legend.position = c("left","top"),
#             legend.text.size = 0.6 ,
#             legend.width = 1)+
#   tm_scale_bar(position=c("right", "bottom"), 
#                color.dark = "#D4582A", 
#                color.light = "#FFE07F")
# 
# 
# if (save == "yes"){
#   if (user == "dhourani"){
#     save_tmap(highIncDriversSydney, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/highIncDriversGreaterSydney.png")
#     save_tmap(highIncDriversCoreSydney, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/highIncDriversCoreSydney.png")
#   } else if (user == "hbatrouney"){
#     save_tmap(highIncDriversSydney, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/highIncDriversGreaterSydney.png")
#     save_tmap(highIncDriversCoreSydney, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/highIncDriversCoreSydney.png")  
#   }
# }
#############################################################################################################################################
#5a. Heat maps of city growth
#############################################################################################################################################
#Read in workplace data
if (user == "dhourani"){
workplaceDataSA3 <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/workplaceDataSA3.csv")
} else if (user == "hbatrouney") {
  workplaceDataSA3 <-  fread("C:/Users/hbatrouney/Documents/Spatial structure of cities/Basefile/workplaceDataSA3.csv")
} else if (user == "jamesha"){
  workplaceDataSA3 <- fread("/Users/jamesha/Documents/Spatial structure of cities/Basefile/workplaceDataSA3.csv")}

growthpercentmap <- function(city){

 growthShapefile <- SA3_2016
 SA3_temp <- SA3_2016@data %>% as.data.table
 growthShapefile@data <- SA3_temp[ , SA3_NAME16 := SA3_NAME16 %>% as.character()]

threshold <- 1000

growth <- workplaceDataSA3[`2016workers`>threshold]
growth <- growth[,GrowthRate:=GrowthRate*100]
growth <- growth[ , SA3 := SA3 %>% as.character()]

growthShapefile@data <- growth[growthShapefile@data , on = "SA3==SA3_NAME16"]

perc_cat_growth <- function(x, lower , upper, by, sep , above.char) {
  labs <- c(
            paste(paste(seq(lower, upper - by, by = by),"%"),
                  paste(seq(lower + by, upper, by = by),"%"),
                  sep = sep),
            paste(upper,"%", above.char, sep = ""))
  breaks <- c(seq(lower, upper, by = by), Inf)
 growthShapefile$GrowthRate <- cut(x, breaks = breaks,
  #growth$GrowthRate <- cut(x,breaks = breaks,
                                              right = FALSE, labels = labs)
  assign("growthShapefile" , growthShapefile, envir = globalenv())
  assign("breaks" , breaks, envir = globalenv())
}

perc_cat_growth(growthShapefile$GrowthRate , lower = 0 , upper = 5 , by = 1 , sep = "-", above.char = "+")

growthShapefile <- growthShapefile[!is.na(growthShapefile$"GrowthRate"), ]
growthShapefile <- growthShapefile[growthShapefile$"city" == city, ] 

for(i in 1:length(growthShapefile$GrowthRate)){
  if(growthShapefile$GrowthRate[i]<0){
    growthShapefile$GrowthRate = na_if(growthShapefile$GrowthRate,growthShapefile$GrowthRate[i])
  }
}
growthcity <- tm_shape(growthShapefile) +
  tm_fill ("GrowthRate", 
           title= "" , 
           palette = gpal(6, reverse = TRUE), 
           title.text.size=2, 
           colorNA = "#828282" ,
           breaks = breaks) +
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
assign(paste0("growthpercentages",city) , growthcity , envir = globalenv())
}
growthpercentmap("Sydney")
growthpercentmap("Melbourne")
growthpercentmap("Brisbane")
# #Get core city indicator from basefile
# core_city_merger <- workplaceDataDZN_with_corro[work_region == "Greater Sydney" & year == "2016", .(core_city = mean(core_city)) , by = work]
# core_city_merger <- core_city_merger[ , work_merger := work %>% as.character()]
# densityShapefile@data <- core_city_merger[densityShapefile@data, on = "work_merger==work" , nomatch = 0L]
# densityShapefileCoreSydney <- densityShapefile[densityShapefile$"core_city" == 1,]
# 
# 
# densitySydneyCore <- tm_shape(densityShapefileCoreSydney) +
#   tm_fill ("perc_density_change", 
#            title= "" , 
#            palette = gpal(7, reverse = TRUE), 
#            title.text.size=2, 
#            colorNA = "white" ,
#            breaks = breaks ,
#            showNA =FALSE) +
#   tm_borders("grey80") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite") +
#   tm_layout(legend.position = c("left","top"),
#             legend.text.size = 0.6 ,
#             legend.width = 1)

if (save == "yes"){
  if (user == "dhourani"){
    save_tmap(growthpercentagesSydney, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthSydneytest3.png")
  } else if (user == "hbatrouney"){
    save_tmap(growthpercentagesSydney, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthSydneytest3.png")
  } else if (user == "jamesha"){
    save_tmap(growthpercentagesSydney, "/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthSydneytest3.png")  
  }
}
if (save == "yes"){
  if (user == "dhourani"){
    save_tmap(growthpercentagesMelbourne, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthMelbournetest3.png")
  } else if (user == "hbatrouney"){
    save_tmap(growthpercentagesMelbourne, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthMelbournetest3.png")
  } else if (user == "jamesha"){
    save_tmap(growthpercentagesMelbourne, "/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthMelbournetest3.png")  
  }
}
if (save == "yes"){
  if (user == "dhourani"){
    save_tmap(growthpercentagesBrisbane, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthBrisbanetest3.png")
  } else if (user == "hbatrouney"){
    save_tmap(growthpercentagesBrisbane, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthBrisbanetest3.png")
  } else if (user == "jamesha"){
    save_tmap(growthpercentagesBrisbane, "/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthBrisbanetest3.png")  
  }
}
# 
# 
# densityShapefileCBD <- densityShapefile[densityShapefile$"SA3_NAME16" == "Sydney Inner City"  ,] 
# 
# densitySydneyCBD <- tm_shape(densityShapefileCBD) +
#   tm_fill ("perc_density_change", 
#            title= "" , 
#            palette = gpal(7, reverse = TRUE), 
#            title.text.size=2, 
#            colorNA = "white" ,
#            breaks = breaks ,
#            showNA =FALSE) +
#   tm_borders("grey80") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite") +
#   tm_layout(legend.show = FALSE)
# 
# if (save == "yes"){
#   if (user == "dhourani"){
#     save_tmap(densitySydneyCBD, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/densitySydneyCBD.png")
#   } else if (user == "hbatrouney"){
#     save_tmap(densitySydneyCBD, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/densitySydneyCBD.png")
#   }
# }


#############################################################################################################################################
#5b. Heat maps of city growth relative to city average
#############################################################################################################################################
#Read in workplace data
if (user == "dhourani"){
  workplaceDataSA3 <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/workplaceDataSA3.csv")
} else if (user == "hbatrouney") {
  workplaceDataSA3 <-  fread("C:/Users/hbatrouney/Documents/Spatial structure of cities/Basefile/workplaceDataSA3.csv")
} else if (user == "jamesha"){
  workplaceDataSA3 <- fread("/Users/jamesha/Documents/Spatial structure of cities/Basefile/workplaceDataSA3.csv")}

#This function spits out a heat map where SA3s are shaded depending on whether they're growing faster or slower than the city average
growthmap <- function(averagegrowth, city){

  #this code is needed to make a shape file
growthShapefile <- SA3_2016
SA3_temp <- SA3_2016@data %>% as.data.table
growthShapefile@data <- SA3_temp[ , SA3_NAME16 := SA3_NAME16 %>% as.character()]

 #this is to get rid of the outlier "Blue Mountains South", which has a 25% growth rate bc population went from 5 people to 16 people.
threshold <- 1000

growth <- workplaceDataSA3[`2016workers`>threshold]
growth <- growth[,GrowthRate:=GrowthRate*100]
growth <- growth[ , SA3 := SA3 %>% as.character()]



growthShapefile@data <- growth[growthShapefile@data , on = "SA3==SA3_NAME16"]

#This code could do with a closer inspection - it's just borrowed from Diana's.
perc_cat_growth <- function(x, lower , upper, by, sep , below.char, above.char) {
  labs <- c("Below average", "About average", "Above average")
  breaks <- c(-Inf,seq(lower, upper, by = by), Inf)
  growthShapefile$GrowthRate <- cut(x, breaks = breaks, right = FALSE, labels = labs)
  assign("growthShapefile" , growthShapefile, envir = globalenv())
  assign("breaks" , breaks, envir = globalenv())
}

#Here we can specify how close to the city average is "close", e.g. +/- 0.2 percentage points.
perc_cat_growth(growthShapefile$GrowthRate , lower = averagegrowth - 0.2 , upper = averagegrowth + 0.2 , by = 0.4 , sep = "-", below.char = "-", above.char = "+")

growthShapefile <- growthShapefile[!is.na(growthShapefile$"GrowthRate"), ]
growthShapefile <- growthShapefile[growthShapefile$"city" == city, ] 


growthcity <- tm_shape(growthShapefile) +
  tm_fill ("GrowthRate", 
           title= "" , 
           #palette = gpal(6, reverse = TRUE), 
           palette = c("#FFC35A","#F68B33","#D4582A"),
           title.text.size=2, 
           colorNA = "white" ,
           breaks = breaks ) +
  tm_borders("grey20") +
  tm_view (alpha = 0.7, 
           basemaps.alpha = 2, 
           basemaps = "Stamen.TonerLite") +
  tm_layout(legend.position = c("left","top"),
            legend.text.size = 0.6 ,
            legend.width = 1)
  #tm_scale_bar(position=c("right", "bottom"), 
               #color.dark = "#D4582A", 
               #color.light = "#FFE07F") 

assign(paste0("growth",city) , growthcity , envir = globalenv())
}

#These average growth rates come from "Jobs Growth by SA2 and SA3.xlsx" in the dropbox 
growthmap(2.19, "Sydney")
growthmap(2.16, "Melbourne")
growthmap(1.16, "Brisbane")

if (save == "yes"){
  if (user == "dhourani"){
    save_tmap(growthSydney, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthSydney.png")
  } else if (user == "hbatrouney"){
    save_tmap(growthSydney, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthSydney.png")
  } else if (user == "jamesha"){
    save_tmap(growthSydney, "/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthSydneytest2.png")  
  }
}

if (save == "yes"){
  if (user == "dhourani"){
    save_tmap(growthMelbourne, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthMelbourne.png")
  } else if (user == "hbatrouney"){
    save_tmap(growthMelbourne, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthMelbourne.png")
  } else if (user == "jamesha"){
    save_tmap(growthMelbourne, "/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthMelbournetest2.png")  
  }
}
if (save == "yes"){
  if (user == "dhourani"){
    save_tmap(growthBrisbane, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthBrisbane.png")
  } else if (user == "hbatrouney"){
    save_tmap(growthBrisbane, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthBrisbane.png")
  } else if (user == "jamesha"){
    save_tmap(growthBrisbane, "/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthBrisbanetest2.png")  
  }
}