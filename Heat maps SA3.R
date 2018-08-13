##INDEX

#5a. SA3 Heat maps of city growth
#5b. SA3 Heat maps of growth relative to city average
#5c. SA2 Heat maps of city growth
#5d. SA2 Heat maps of growth relative to city average

#Inputs for SA3 level maps are already scaled to ABS LFS data to get around the issue of imputation in the 2016 census.
#Inputs for SA2 level maps need to have their job numbers scaled in 2011, as they come from a different, unscaled file.

user <- "jaha" #choose from hbatrouney or dhourani or jamesha or jaha
save <- "yes" #choose from save "yes" or "no" - only pick yes if you want to save dandelions

library(data.table)
library(grattanCharts)
library(maptools)
library(tmap)
library(ASGS)
library(hutils)
library(magrittr)


#############################################################################################################################################
#5a. Heat maps of city growth
#############################################################################################################################################
#Read in workplace data
if (user == "dhourani"){
workplaceDataSA3 <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/workplaceDataSA3.csv")
} else if (user == "hbatrouney") {
  workplaceDataSA3 <-  fread("C:/Users/hbatrouney/Documents/Spatial structure of cities/Basefile/workplaceDataSA3.csv")
} else if (user == "jamesha"){
  workplaceDataSA3 <- fread("/Users/jamesha/Documents/Spatial structure of cities/Basefile/workplaceDataSA3.csv")
} else if (user == "jaha"){
  workplaceDataSA3 <- fread("/Users/jaha/Documents/Spatial structure of cities/Basefile/workplaceDataSA3.csv")}

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
  labs <- c(paste("Negative growth"),
            paste(paste(seq(lower, upper - by, by = by),"%"),
                  paste(seq(lower + by, upper, by = by),"%"),
                  sep = sep),
            paste(upper,"%", above.char, sep = ""))
  breaks <- c(-Inf,seq(lower, upper, by = by), Inf)
 growthShapefile$GrowthRate <- cut(x, breaks = breaks,
  #growth$GrowthRate <- cut(x,breaks = breaks,
                                              right = FALSE, labels = labs)
  assign("growthShapefile" , growthShapefile, envir = globalenv())
  assign("breaks" , breaks, envir = globalenv())
  assign("labs", labs, envir = globalenv())
}

perc_cat_growth(growthShapefile$GrowthRate , lower = 0 , upper = 8 , by = 2, sep = "-", above.char = "+")

growthShapefile <- growthShapefile[!is.na(growthShapefile$"GrowthRate"), ]
growthShapefile <- growthShapefile[growthShapefile$"city" == city, ] 

# for(i in 1:length(growthShapefile$GrowthRate)){
#   if(growthShapefile$GrowthRate[i]<0){
#     growthShapefile$GrowthRate = na_if(growthShapefile$GrowthRate,growthShapefile$GrowthRate[i])
#  }
#}
growthcity <- tm_shape(growthShapefile) +
  tm_fill ("GrowthRate", 
           title= "" , 
           palette = gpal(6, reverse = TRUE), 
           title.text.size=2, 
           colorNA = "#828282" ,
           breaks = breaks,
           labels = labs) +
  #tm_borders("grey20") +
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
  } else if (user == "jaha"){
    save_tmap(growthpercentagesSydney, "/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthSydneytest3.png")  
  }
}
if (save == "yes"){
  if (user == "dhourani"){
    save_tmap(growthpercentagesMelbourne, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthMelbournetest3.png")
  } else if (user == "hbatrouney"){
    save_tmap(growthpercentagesMelbourne, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthMelbournetest3.png")
  } else if (user == "jamesha"){
    save_tmap(growthpercentagesMelbourne, "/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthMelbournetest3.png")  
  } else if (user == "jaha"){
    save_tmap(growthpercentagesMelbourne, "/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthMelbournetest3.png")  
  }
}
if (save == "yes"){
  if (user == "dhourani"){
    save_tmap(growthpercentagesBrisbane, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthBrisbanetest3.png")
  } else if (user == "hbatrouney"){
    save_tmap(growthpercentagesBrisbane, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthBrisbanetest3.png")
  } else if (user == "jamesha"){
    save_tmap(growthpercentagesBrisbane, "/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthBrisbanetest3.png")  
  } else if (user == "jaha"){
    save_tmap(growthpercentagesBrisbane, "/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/growthBrisbanetest3.png")  
  }
}



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
  assign("labs", labs, envir = globalenv())
}

#Here we can specify how close to the city average is "close", e.g. +/- 0.2 percentage points.
perc_cat_growth(growthShapefile$GrowthRate , lower = averagegrowth - 0.2 , upper = averagegrowth + 0.2 , by = 0.4 , sep = "-", below.char = "-", above.char = "+")

growthShapefile <- growthShapefile[!is.na(growthShapefile$"GrowthRate"), ]
growthShapefile <- growthShapefile[growthShapefile$"city" == city, ] 


growthcity <- tm_shape(growthShapefile) +
  tm_fill ("GrowthRate", 
           title= "" , 
           #palette = gpal(6, reverse = TRUE), 
           palette = gpal(4)[2:4],
           title.text.size=2, 
           colorNA = "white" ,
           breaks = breaks,
           labels = labs,
           legend.show = FALSE) +
  tm_borders("grey20") +
  tm_view (alpha = 0.7, 
           basemaps.alpha = 2, 
           basemaps = "Stamen.TonerLite")
   #+
  #tm_layout(legend.position = c("left","top"),
           # legend.text.size = 0.6 ,
           # legend.width = 1)
  #tm_scale_bar(position=c("right", "bottom"), 
               #color.dark = "#D4582A", 
               #color.light = "#FFE07F") 

assign(paste0("growth",city) , growthcity , envir = globalenv())
}

#These average growth rates come from "Jobs Growth by SA2 and SA3.xlsx" in the dropbox 
growthmap(2.04, "Sydney")
growthmap(2.30, "Melbourne")
growthmap(1.21, "Brisbane")

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


##########################################################################################################################################
# 5c. Trying to repeat the analysis but more granular (SA2)
##########################################################################################################################################
if (user == "dhourani"){
  workplaceDataSA2_with_corro <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/workplaceDataSA2withCorrespondences.csv")
} else if (user == "hbatrouney") {
  workplaceDataSA2_with_corro <-  fread("C:/Users/hbatrouney/Documents/Spatial structure of cities/Basefile/workplaceDataSA2withCorrespondences.csv")
} else if (user == "jamesha") {
  workplaceDataSA2_with_corro <-  fread("/Users/jamesha/Documents/Spatial structure of cities/Basefile/workplaceDataSA2withCorrespondences.csv")
}

densityShapefile <- SA2_2016
SA2_temp <- SA2_2016@data %>% as.data.table
SA3_temp <- SA3_2016@data %>% as.data.table
SA2_temp <- SA2_temp[ , SA2_CODE16 := SA2_CODE16 %>% as.character()]
densityShapefile@data <- SA3_temp[SA2_temp , on = "SA3_NAME16==SA3_NAME16"]

threshold <- 1000

density_change <- workplaceDataSA2_with_corro[, .(density = sum(density) , workers = sum(workers)) , by = .(year, work)] %>% 
  reshape(., idvar = "work", timevar = "year", direction = "wide") %>% 
  .[workers.2016 > threshold , perc_density_change := ((workers.2016 / workers.2011)^0.2 - 1)*100] %>% 
  .[density.2016==0 & workers.2016==0 , perc_density_change := 0] %>%  #Calculate density change for areas with a non-insignificant number of workers
  .[!is.na(density.2016)] #get rid of entries that existed in 2011 but not in 2016 (these are just NAs)

density_change <- density_change[ , work := work %>% as.character()]

densityShapefile@data <- density_change[densityShapefile@data , on = "work==SA2_NAME16"]

perc_cat_density <- function(x, lower , upper, by, sep , above.char) {
  labs <- c(paste(paste(seq(lower, upper - by, by = by),"%"),
                  paste(seq(lower + by, upper, by = by),"%"),
                  sep = sep),
            paste(upper,"%", above.char, sep = ""))
  breaks <- c(seq(lower, upper, by = by), Inf)
  densityShapefile$perc_density_change <- cut(x, breaks = breaks,
                                              right = FALSE, labels = labs)
  assign("densityShapefile" , densityShapefile, envir = globalenv())
  assign("breaks" , breaks, envir = globalenv())
}

perc_cat_density(densityShapefile$perc_density_change , lower = -5 , upper = 10 , by = 2.5 , sep = "-", above.char = "+")

densityShapefile <- densityShapefile[densityShapefile$"GCC_NAME16" == "Greater Sydney", ] 
densityShapefile <- densityShapefile[!is.na(densityShapefile$"density.2016"), ] 

densitySydney <- tm_shape(densityShapefile) +
  tm_fill ("perc_density_change", 
           title= "" , 
           palette = gpal(7, reverse = TRUE), 
           title.text.size=2, 
           colorNA = "white" ,
           breaks = breaks ) +
  #tm_borders("grey20") +
  tm_view (alpha = 0.7, 
           basemaps.alpha = 2, 
           basemaps = "Stamen.TonerLite") +
  tm_layout(legend.position = c("left","top"),
            legend.text.size = 0.6 ,
            legend.width = 1) +
  tm_scale_bar(position=c("right", "bottom"), 
               color.dark = "#D4582A", 
               color.light = "#FFE07F") 

#Get core city indicator from basefile
core_city_merger <- workplaceDataSA2_with_corro[work_region == "Greater Sydney" & year == "2016", .(core_city = mean(core_city)) , by = work]
core_city_merger <- core_city_merger[ , work_merger := work %>% as.character()]
densityShapefile@data <- core_city_merger[densityShapefile@data, on = "work_merger==work" , nomatch = 0L]
densityShapefileCoreSydney <- densityShapefile[densityShapefile$"core_city" == 1,]


densitySydneyCore <- tm_shape(densityShapefileCoreSydney) +
  tm_fill ("perc_density_change", 
           title= "" , 
           palette = gpal(7, reverse = TRUE), 
           title.text.size=2, 
           colorNA = "#AEAEAE" ,
           breaks = breaks ,
           showNA =FALSE) +
 # tm_borders("grey80") +
  tm_view (alpha = 0.7, 
           basemaps.alpha = 2, 
           basemaps = "Stamen.TonerLite") +
  tm_layout(legend.position = c("left","top"),
            legend.text.size = 0.6 ,
            legend.width = 1)

if (save == "yes"){
  if (user == "dhourani"){
    save_tmap(densitySydneyCore, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/densitySydneyCore.png")
  } else if (user == "hbatrouney"){
    save_tmap(densitySydneyCore, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/densitySydneyCore.png")
  } else if (user == "jamesha"){
    save_tmap(densitySydneyCore, "/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/densitySydneyCoretestonly.png")
  }
}


densityShapefileCBD <- densityShapefile[densityShapefile$"SA3_NAME16" == "Sydney Inner City"  ,] 

densitySydneyCBD <- tm_shape(densityShapefileCBD) +
  tm_fill ("perc_density_change", 
           title= "" , 
           palette = gpal(7, reverse = TRUE), 
           title.text.size=2, 
           colorNA = "#AEAEAE" ,
           breaks = breaks ,
           showNA =FALSE) +
#  tm_borders("grey80") +
  tm_view (alpha = 0.7, 
           basemaps.alpha = 2, 
           basemaps = "Stamen.TonerLite") +
  tm_layout(legend.position = c("left","top"),
            legend.text.size = 0.6 ,
            legend.width = 1)

if (save == "yes"){
  if (user == "dhourani"){
    save_tmap(densitySydneyCBD, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/densitySydneyCBD.png")
  } else if (user == "hbatrouney"){
    save_tmap(densitySydneyCBD, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/densitySydneyCBD.png")
  }else if (user == "jamesha"){
    save_tmap(densitySydneyCBD, "/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/densitySydneyCBDtestonly.png")
  }
}
##########################################################################################################################################
# 5d. SA2 level relative to city average
##########################################################################################################################################
if (user == "dhourani"){
  workplaceDataSA2_with_corro <- fread("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/workplaceDataSA2withCorrespondences.csv")
} else if (user == "hbatrouney") {
  workplaceDataSA2_with_corro <-  fread("C:/Users/hbatrouney/Documents/Spatial structure of cities/Basefile/workplaceDataSA2withCorrespondences.csv")
} else if (user == "jamesha") {
  workplaceDataSA2_with_corro <-  fread("/Users/jamesha/Documents/Spatial structure of cities/Basefile/workplaceDataSA2withCorrespondences.csv")
}

makeshape <- function(averagegrowth,city,scalefactor){

densityShapefile <- SA2_2016
SA2_temp <- SA2_2016@data %>% as.data.table
SA3_temp <- SA3_2016@data %>% as.data.table
SA2_temp <- SA2_temp[ , SA2_NAME16 := SA2_NAME16 %>% as.character()]
densityShapefile@data <- SA3_temp[SA2_temp , on = "SA3_NAME16==SA3_NAME16"]

threshold <- 1000

density_change <- workplaceDataSA2_with_corro[, .(density = sum(density) , workers = sum(workers)) , by = .(year, work)] %>% 
  reshape(., idvar = "work", timevar = "year", direction = "wide") %>% 
  .[workers.2016 > threshold , perc_density_change := ((workers.2016 / (scalefactor*workers.2011))^0.2 - 1)*100] %>% 
  .[density.2016==0 & workers.2016==0 , perc_density_change := 0] %>%  #Calculate density change for areas with a non-insignificant number of workers
  .[!is.na(density.2016)] #get rid of entries that existed in 2011 but not in 2016 (these are just NAs)

density_change <- density_change[ , work := work %>% as.character()]

densityShapefile@data <- density_change[densityShapefile@data , on = "work==SA2_NAME16"]



perc_cat_density <- function(x, lower , upper, by, sep , above.char) {
  labs <- c(paste("below average"),
                  paste("about average"),
                  paste("above average"))
  breaks <- c(-Inf,seq(lower, upper, by = by), Inf)
  densityShapefile$perc_density_change <- cut(x, breaks = breaks,
                                              right = FALSE, labels = labs)
  assign("densityShapefile" , densityShapefile, envir = globalenv())
  assign("breaks" , breaks, envir = globalenv(),
  assign("labs", labs, envir = globalenv()))
}



perc_cat_density(densityShapefile$perc_density_change , 
                 lower = averagegrowth -0.2 , upper = averagegrowth + 0.2 , by = 0.4 , 
                 sep = "-", above.char = "+")

densityShapefile <- densityShapefile[densityShapefile$"GCC_NAME16" == paste0("Greater ",city),] 
densityShapefile <- densityShapefile[!is.na(densityShapefile$"density.2016"), ] 

output <- tm_shape(densityShapefile) +
  tm_fill ("perc_density_change", 
           title= "" , 
           palette = c("#FFC35A","#F68B33","#D4582A"), 
           title.text.size=2, 
           colorNA = "#AEAEAE" ,
           breaks = breaks ,
           labels = labs) +
  #tm_borders("grey20") +
  tm_view (alpha = 0.7, 
           basemaps.alpha = 2, 
           basemaps = "Stamen.TonerLite") 
  # +
  # tm_layout(
  # legend.position = c("left","top"),
  # legend.text.size = 0.6 ,
  # legend.width = 1)
  #+
  # tm_scale_bar(position=c("right", "bottom"), 
  #              color.dark = "#D4582A", 
  #              color.light = "#FFE07F") 
assign(paste0("density",city),output,envir = globalenv())
}

  makeshape(2.04,"Sydney",1.0868)
  makeshape(2.30,"Melbourne",1.0739)
  makeshape(1.21,"Brisbane",1.0777)

save_tmap(densitySydney, "/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/GrowthSydneySA2.png")
save_tmap(densityMelbourne, "/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/GrowthMelbourneSA2.png")
save_tmap(densityBrisbane, "/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/GrowthBrisbaneSA2.png")

# #Get core city indicator from basefile
# core_city_merger <- workplaceDataSA2_with_corro[work_region == "Greater Sydney" & year == "2016", .(core_city = mean(core_city)) , by = work]
# core_city_merger <- core_city_merger[ , work_merger := work %>% as.character()]
# densityShapefile@data <- core_city_merger[densityShapefile@data, on = "work_merger==work" , nomatch = 0L]
# densityShapefileCoreSydney <- densityShapefile[densityShapefile$"core_city" == 1,]
# 
# 
# densitySydneyCore <- tm_shape(densityShapefileCoreSydney) +
#   tm_fill ("perc_density_change", 
#            title= "" , 
#            palette = c("#FFC35A","#F68B33","#D4582A"),
#            colorNA = "#AEAEAE" ,
#            breaks = breaks ,
#            showNA =FALSE) +
#   #tm_borders("grey80") +
#   tm_view (alpha = 0.7, 
#            basemaps.alpha = 2, 
#            basemaps = "Stamen.TonerLite") +
#   tm_layout(legend.position = c("left","top"),
#             legend.text.size = 0.6 ,
#             legend.width = 1)
# 
# if (save == "yes"){
#   if (user == "dhourani"){
#     save_tmap(densitySydneyCore, "C:/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/densitySydneyCore.png")
#   } else if (user == "hbatrouney"){
#     save_tmap(densitySydneyCore, "C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/densitySydneyCore.png")
#   } else if (user == "jamesha"){
#     save_tmap(densitySydneyCore, "/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/HeatMaps/densitySydneyCoretestonlyrelative.png")
#   }
# }

