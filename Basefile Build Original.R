user <- "jamesha" #choose from hbatrouney or dhourani
save <- "yes" #choose from save "yes" or "no" - only pick yes if you want to save dandelions

if (user == "hbatrouney")
{setwd("C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure")
}else if (user == "dhourani"){
  setwd("/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure")
}else if (user == "jamesha"){
  setwd("/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure")
}

library(ASGS)
library(readxl)
library(viridis)
library(rgeos)
library(dplyr)
library(data.table)
library(hutils)

#Read in Destination Zone correspondences and delete first row which is blank in the ABS spreadsheet
include_correspondence <-  "yes" #switch for generating basefile with and without conrrespondences

DZN_correspondence <- readxl::read_excel("./Data/Destination zone correspondence 2011 2016.xls" , sheet = "Table 3" , range = "A6:D9562" , col_names = TRUE )  %>% as.data.table %>% .[!1]
SA2_correspondence <- readxl::read_excel("./Data/SA2 correspondence 2011 2016.xls" , sheet = "Table 3" , range = "A6:F2433" , col_names = TRUE )  %>% as.data.table %>% .[!1] 

#Set global parameters 

work.asgs <- "SA2" #Select main statistical area of work from "DZN" and "SA2" (needs to be in "") )
live.asgs <- "SA2" #Select main statistical area of residence from "DZN" and "SA2" (needs to be in "") )

#Create baesefile - loop over census years
BuildWorkplaceDataset <- function(year) {
  
  sheetOD <- paste0("Live (",live.asgs,") v Work (",work.asgs,") ",year)
  rangeOD <- if(work.asgs == "DZN"){
    if(year == "2011"){
      "A13:CGF10346" 
    }else if (year == "2016"){
      "A13:CJX9170" 
    } 
  }else if (work.asgs == "SA2"){
    if(year == "2011"){
      "A13:CGF2248" 
    }else if (year == "2016"){
      "A13:CJX2325" 
    }
  }
  
  LiveWorkData <- read_excel("./Data/Origin-Destination Data.xlsx" , sheet = sheetOD , range = rangeOD , col_names = TRUE )  
  names(LiveWorkData)[1]<-"work"
  LiveWorkData <- LiveWorkData[ , c("work","Total")] %>% as.data.table
  setnames(LiveWorkData, "Total", "workers")
  
  LiveWorkData$work <- LiveWorkData$work %>% as.character
  
  workShapefile <- 
    switch(year,
           "2011" = {
             switch(work.asgs,
                    "SA2" = SA2_2011,
                    "DZN" = DZN_2011,
                    stop("The name of DT's first column must start with SA[1-4] to indicate the geography."))
           },
           "2016" = {
             switch(work.asgs,
                    "SA2" = SA2_2016,
                    "DZN" = DZN_2016,
                    stop("The name of DT's first column must start with SA[1-4] to indicate the geography, ",
                         "or CED (for Commonwealth Electoral Divisions)."))
           })
  
  
  
  #Calculate centroids and attach to work shapefiles
  centroidsdt <- as.data.table(gCentroid(workShapefile, byid = TRUE))
  names(centroidsdt) <- c("CentroidLon", "CentroidLat" )
  workShapefile@data <- as.data.table (cbind(workShapefile@data, centroidsdt))
  
  
  #Rename shapefile variables
  if (work.asgs == "SA2"){
    setnames(workShapefile@data, 
             old = switch(year,
                          "2016" = c("SA2_NAME16", "AREASQKM16"),
                          "2011" = c("SA2_NAME11", "ALBERS_SQM"),
                          stop("Invalid year.")),
             new = c("work_asgs", "area"))
  } else if (work.asgs == "DZN"){
    setnames(workShapefile@data, 
             old = switch(year,
                          "2016" = c("DZN_CODE16", "AREASQKM16"),
                          "2011" = c("DZN_CODE11", "AREA_SQKM"),
                          stop("Invalid year.")),
             new = c("work_asgs", "area"))
  }
  
  #Merge region data onto work shapefile if using DZN (this is because ASGS shapefile data slot doesn't contain this info)
  if (work.asgs == "DZN"){
    if (year == "2016"){mergefile <- SA2_2016@data %>% as.data.table} else if (year == "2011"){mergefile <- SA2_2011@data %>% as.data.table}
    setnames(mergefile,
             old = switch(year,
                          "2016" = c("GCC_NAME16", "SA2_NAME16"),
                          "2011" = c("GCC_NAME11", "SA2_NAME11"),
                          stop("Invalid year.")),
             new = c("work_region", "work_SA2"))
    setnames(workShapefile@data,
             old = switch(year,
                          "2016" = c("SA2_NAME16"),
                          "2011" = c("SA2_NAME11"),
                          stop("Invalid year.")),
             new = c("work_SA2"))
    mergefile <- mergefile[ , c("work_SA2" , "work_region")]
    workShapefile@data <- mergefile[workShapefile@data , on = "work_SA2==work_SA2", nomatch=0L]
  }
  
  ##Create core city indicator on work shapefile
  # DIH - only did this for Sydney and Melbourne so far - need to replicate for other major cities. 
  #Note - for each, first line is Sydney areas and second line is Melbourne areas. 
  inner_city_SA4s <- c("Sydney - Eastern Suburbs" , "Sydney - City and Inner South", "Sydney - Inner South West" , "Sydney - Inner West", "Sydney - Parramatta" ,"Sydney - Northern Beaches" , "Sydney - Ryde" , "Sydney - South West" , "Sydney - Blacktown" ,
                       "Melbourne - Inner" , "Melbourne - Inner East" , "Melbourne - Inner South")
  inner_city_SA3s <- c("Baulkham Hills" , "Hornsby" , "Penrith" ,  "Camden" ,  "Bringelly - Green Valley" , "North Sydney - Mosman" , "Chatswood - Lane Cove" , "Ku-ring-gai" , "St Marys" , "Cronulla - Miranda - Caringbah"  ,
                       "Monash" , "Dandenong" , "Casey - North" , "Casey - South" , "Frankston" , "Whitehorse - East" , "Manningham - East" , "Maroondah" , "Knox" , "Banyule" , "Darebin - North" , "Moreland - North" , "Keilor" , "Maribyrnong" , "Hobsons Bay" , "Brimbank" , "Wyndham")
  if (year == "2016"){
    inner_city_SA2s <- c("Normanhurst - Thornleigh - Westleigh", "Waitara - Wahroonga (West)" , "Hornsby - East" , "Hornsby - West" , "Rouse Hill - Beaumont Hills", "Sutherland - Kirrawee" , "Oyster Bay - Como - Jannali" , "Illawong - Alfords Point" ,  "Woronora Heights" , "Loftus - Yarrawarrah" , "Engadine" , "Leumeah - Minto Heights" , "Campbelltown - Woodbine" , "Minto - St Andrews" , "Claymore - Eagle Vale - Raby" , "Ingleburn - Denham Court" , "Macquarie Fields - Glenfield" ,"Menai - Lucas Heights - Woronora",
                         "Melton South" , "Melton West" , "Melton" , "Hillside" , "Rockbank - Mount Cottrell" , "Taylors Hill" , "Caroline Springs" , "Burnside Heights" , "Burnside" , "Melbourne Airport" , "Tullamarine" , "Broadmeadows" , "Campbellfield - Coolaroo" , "Meadow Heights" , "Roxburgh Park - Somerton" , "Craigieburn - South" , "Craigieburn - Central" , "Craigieburn - North" , "Craigieburn - West" , "Thomastown" , "Bundoora - West" , "Bundoora - North" , "Lalor" , "Mill Park - South" , "Mill Park - North" , "Epping - South" , "South Morang (South)" , "South Morang (North)" , "Mernda" , "Doreen" , "Plenty - Yarrambat" , "Wattle Glen - Diamond Creek" , "Research - North Warrandyte" , "Eltham" , "Hurstbridge" , "Chirnside Park", "Mooroolbark" , "Kilsyth" , "Montrose" , "Mount Evelyn" , "Mount Dandenong - Olinda" , "Upwey - Tecoma" , "Belgrave - Selby")
  } else if (year == "2011"){
    inner_city_SA2s <- c("Normanhurst - Thornleigh - Westleigh", "Hornsby - Waitara", "Rouse Hill - Beaumont Hills" , "Sutherland - Kirrawee" , "Oyster Bay - Como - Jannali" , "Illawong - Alfords Point" ,  "Engadine - Loftus" , "Leumeah - Minto Heights" , "Campbelltown - Woodbine" , "Minto - St Andrews" , "Claymore - Eagle Vale - Raby" , "Ingleburn - Denham Court" , "Macquarie Fields - Glenfield" ,"Menai - Lucas Heights - Woronora",
                         "Melton South" , "Melton West" , "Melton" , "Hillside" , "Rockbank - Mount Cottrell" , "Taylors Hill" , "Caroline Springs", "Melbourne Airport" , "Tullamarine" , "Broadmeadows" , "Campbellfield - Coolaroo" , "Meadow Heights" , "Roxburgh Park - Somerton" , "Craigieburn - Mickleham" , "Thomastown" , "Bundoora - West" , "Bundoora - North" , "Lalor" , "Mill Park - South" , "Mill Park - North" , "South Morang" , "Plenty - Yarrambat" , "Wattle Glen - Diamond Creek" , "Research - North Warrandyte" , "Eltham" , "Hurstbridge" , "Chirnside Park", "Mooroolbark" , "Kilsyth" , "Montrose" , "Mount Evelyn" , "Mount Dandenong - Olinda" , "Upwey - Tecoma" , "Belgrave - Selby")
  }
  
  if (year == "2016"){
    core_city_merger <- SA2_2016@data %>% as.data.table
    core_city_merger[((core_city_merger$"SA4_NAME16"  %ein% inner_city_SA4s) | 
                        (core_city_merger$"SA3_NAME16" %ein% inner_city_SA3s) | 
                        (core_city_merger$"SA2_NAME16" %ein% inner_city_SA2s)) , core_city := 1 ][
                          is.na(core_city) , core_city := 0] 
  }else if(year == "2011"){
    core_city_merger <- SA2_2011@data %>% as.data.table
    core_city_merger[((core_city_merger$"SA4_NAME11"  %ein% inner_city_SA4s) | 
                        (core_city_merger$"SA3_NAME11" %ein% inner_city_SA3s) | 
                        (core_city_merger$"SA2_NAME11" %ein% inner_city_SA2s)) , core_city := 1 ][
                          is.na(core_city) , core_city := 0] 
  }
  
  setnames(core_city_merger,
           old = switch(year,
                        "2016" = c("SA2_NAME16"),
                        "2011" = c("SA2_NAME11"),
                        stop("Invalid year.")),
           new = c("work_SA2"))
  core_city_merger <- core_city_merger[ , .SD , .SDcols = c("work_SA2" , "core_city")]
  
  Work_Decoder <- 
    switch(year,
           "2011" = {
             switch(work.asgs,
                    "SA2" = SA2_2011@data %>% as.data.table ,
                    "DZN" = DZN_2011@data %>% as.data.table ,
                    stop("The name of DT must be either SA2 or DZN "))
           },
           "2016" = {
             switch(work.asgs,
                    "SA2" = SA2_2016@data %>% as.data.table,
                    "DZN" = DZN_2016@data %>% as.data.table ,
                    stop("The name of DT must be either SA2 or DZN "))
           }
    )
  
  
  #Subset the work data to make sure all the suburbs on the file exist in the shapefils)
  #Note need to use different decode for different cities (bigger cities can be decoded using GCC, but smaller cities are decoded using GCC)
  
  Work_Decoder <- if (work.asgs == "SA2") {
    setnames(Work_Decoder,
             old = switch(year,
                          "2016" = c("SA2_NAME16" , "GCC_NAME16"),
                          "2011" = c("SA2_NAME11" , "GCC_NAME11"),
                          stop("Invalid year.")),
             new = c("decode" , "work_region"))
  } else if (work.asgs == "DZN") {
    setnames(Work_Decoder,
             old = switch(year,
                          "2016" = c("DZN_CODE16"),
                          "2011" = c("DZN_CODE11"),
                          stop("Invalid year.")),
             new = c("decode"))}
  
  if (work.asgs == "DZN") {Work_Decoder[, decode := as.character(decode)]}
  
  if (work.asgs == "DZN") {
    LiveWorkData <-
      Work_Decoder[LiveWorkData, on = "decode==work", nomatch=0L] %>% #This joins the work data onto the "decoder" table (to make sure the regions on the live / work file are on the live shapefile)
      .[, .(work = decode, workers )] #This again subsets the columns for the 4 variables of interest (work, live, workers, work_region)
  }  else if (work.asgs == "SA2") {
    LiveWorkData <-
      Work_Decoder[LiveWorkData, on = "decode==work", nomatch=0L] %>% #This joins the work data onto the "decoder" table (to make sure the regions on the live / work file are on the live shapefile)
      .[, .(work = decode, workers , work_region)] #This again subsets the columns for the variables of interest (work,  workers, work_region)
  }
  
  basefile <-
    LiveWorkData[workShapefile@data, on = "work==work_asgs", nomatch=0L] %>%
    setnames(c("CentroidLon","CentroidLat"), c("dest_lon", "dest_lat")) %>% 
    .[, .SD, .SDcols = c(grep("lat|lon|work|area", names(.), value = TRUE))] 
  
  if (work.asgs == "SA2"){
    basefile <- core_city_merger[basefile, on = "work_SA2==work", nomatch=0L] 
    setnames(basefile , "work_SA2" , "work")
  } else if (work.asgs == "DZN"){
    basefile <- core_city_merger[basefile, on = "work_SA2==work_SA2", nomatch=0L] }
  
  #Density calculations - note that 2011 SA2 densitiy is in square meters, all others are in square km
  if (year == "2011" & work.asgs == "SA2"){basefile[ , area :=  (area/1000000)]} 
  basefile[ , density := workers / area]
  
  #Adding a year variable
  basefile$year <- year
  
  if(include_correspondence == "yes"){
    #This basefile is intended to be used to compare results over time. Because ASGS regions can change over time, need to amend the data slighty to ensure they are comparable over time periods.
    #In this section we change the 2011 entry for ASGS regions that have split between time periods. This amendment pro-rates the working population across the new 'split out' ASGS regions
    if (year =="2011" & work.asgs == "SA2"){
      #Places whose names and boundaries are unchanged
      same_everything <- SA2_correspondence[PERCENTAGE == 100 & SA2_NAME_2011 == SA2_NAME_2016]
      basefile1 <- same_everything[basefile, on="SA2_NAME_2011==work" , nomatch = 0L] %>% 
        .[ ,  ':='(work_2016_in_2011 = SA2_NAME_2011 , name_change_2011_2016_type = "no change") ] %>% 
        setnames(. ,"SA2_NAME_2011" , "work") %>% 
        .[ , .SD, .SDcols =c(names(basefile),"work_2016_in_2011" ,"name_change_2011_2016_type")]
      #Places whose names have changed but boundaries are unchanged
      new_names <- SA2_correspondence[PERCENTAGE == 100 & SA2_NAME_2011 != SA2_NAME_2016]
      basefile2 <- new_names[basefile, on="SA2_NAME_2011==work", nomatch = 0L] %>% 
        .[ ,  ':='(work_2016_in_2011 = SA2_NAME_2011 , name_change_2011_2016_type = "new name") ] %>% 
        setnames(. ,"SA2_NAME_2016" , "work") %>% 
        .[ , .SD, .SDcols =c(names(basefile),"work_2016_in_2011" ,"name_change_2011_2016_type")]
      #Places whose boundaries are changed
      new_boundaries <- SA2_correspondence[PERCENTAGE != 100]
      basefile3_temp <- basefile[new_boundaries, on="work==SA2_NAME_2011", nomatch = 0L] %>% 
        .[ ,  ':='(work_2016_in_2011 = work, work = SA2_NAME_2016 , name_change_2011_2016_type = "new boundary" , workers = workers * RATIO) ] %>% 
        .[ , .SD, .SDcols =c(names(basefile),"work_2016_in_2011" ,"name_change_2011_2016_type")]
      #Adjust density for asgs areas that have changed boundaries - do this by looking up the density from asgs package
      basefile3 <- SA2_2016@data %>% as.data.table %>% 
        .[basefile3_temp , on = "SA2_NAME16==work" , nomatch = 0L] %>% 
        .[ , area:= AREASQKM16] %>% 
        .[ , density := workers / area]  
      basefile3 <- setnames(basefile3, "SA2_NAME16", "work") %>% 
        .[ , .SD, .SDcols=names(basefile3_temp) ]
    } else if (year =="2011" & work.asgs == "DZN"){
      #Change variables to characters to they merge on with other characters
      DZN_correspondence$DZN_CODE_2011 <- as.character(DZN_correspondence$DZN_CODE_2011)
      DZN_correspondence$DZN_CODE_2016 <- as.character(DZN_correspondence$DZN_CODE_2016)
      
      #Places whose names and boundaries are unchanged
      same_everything <- DZN_correspondence[PERCENTAGE == 100 & DZN_CODE_2011 == DZN_CODE_2016]
      basefile1 <- same_everything[basefile, on="DZN_CODE_2011==work" , nomatch = 0L] %>% 
        .[ ,  ':='(work_2016_in_2011 = DZN_CODE_2011 , name_change_2011_2016_type = "no change") ] %>% 
        setnames(. ,"DZN_CODE_2011" , "work") %>% 
        .[ , .SD, .SDcols =c(names(basefile),"work_2016_in_2011" ,"name_change_2011_2016_type")]
      #Places whose names have changed but boundaries are unchanged
      new_names <- DZN_correspondence[PERCENTAGE == 100 & DZN_CODE_2011 != DZN_CODE_2016]
      basefile2 <- new_names[basefile, on="DZN_CODE_2011==work", nomatch = 0L] %>% 
        .[ ,  ':='(work_2016_in_2011 = DZN_CODE_2011 , name_change_2011_2016_type = "new name") ] %>% 
        setnames(. ,"DZN_CODE_2016" , "work") %>% 
        .[ , .SD, .SDcols =c(names(basefile),"work_2016_in_2011" ,"name_change_2011_2016_type")]
      #Places whose boundaries are changed NB here merge onto the new boundaries file as it is a one to many merge instead of a 1 to 1
      new_boundaries <- DZN_correspondence[PERCENTAGE != 100]
      basefile3_temp <- basefile[new_boundaries, on="work==DZN_CODE_2011", nomatch = 0L] %>% 
        .[ ,  ':='(work_2016_in_2011 = work , work = DZN_CODE_2016, name_change_2011_2016_type = "new boundary" , workers = workers * RATIO) ] %>% 
        .[ , .SD, .SDcols =c(names(basefile),"work_2016_in_2011" ,"name_change_2011_2016_type")]
      #Adjust density for asgs areas that have changed boundaries - do this by looking up the density from asgs package 
      basefile3 <- DZN_2016@data %>% as.data.table %>% 
        .[basefile3_temp , on = "DZN_CODE16==work" , nomatch = 0L] %>% 
        .[ , area:= AREASQKM16] %>% 
        .[ , density := workers / area] 
      basefile3 <- setnames(basefile3, "DZN_CODE16", "work") %>% 
        .[ , .SD, .SDcols=names(basefile3_temp) ]
    }
    
    assign("basefile1", basefile1 , envir = globalenv())
    assign("basefile2", basefile2 , envir = globalenv())
    assign("basefile3", basefile3 , envir = globalenv())
    
    if (year == "2011"){
      #Create a file which includes all asgs areas which existed in 2011 and exist now in 2016 (although they may be split up)
      #This does not include asgs areas which existed in 2011 but do NOT exist in 2016
      basefile <- rbind(basefile1, basefile2, basefile3)
      # #Add on asgs areas which existed in 2011 but not in 2016
      # add_rows <- merge(basefile,basefile_temp, all=TRUE)
      # add_rows <- add_rows[is.na(work_2011_in_2016)]
      # basefile <- dplyr::bind_rows(basefile_temp, add_rows)
    }
  }
  
  #Assign basefile created here to the global environment called basefile with suffix year - this is so later we can just merge all the years in one big basefile
  assign(paste0("workplaceData",year) , basefile , envir = globalenv())
}

for (i in c( "2011", "2016")){BuildWorkplaceDataset (i)}
workplaceData <- dplyr::bind_rows(workplaceData2011 , workplaceData2016)


#Save basefile
if (save == "yes"){
  if (include_correspondence == "yes"){
    if (user == "dhourani"){
      fwrite(workplaceData , paste0("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/workplaceData",work.asgs,"withCorrespondences.csv"))
    } else if (user == "hbatrouney"){
      fwrite(workplaceData , paste0("C:/Users/hbatrouney/Documents/Spatial structure of cities/Basefile/workplaceData",work.asgs,"withCorrespondences.csv"))  
    } else if (user == "jamesha"){
      fwrite(workplaceData , paste0("/Users/jamesha/Documents/Spatial structure of cities/Basefile/workplaceData",work.asgs,"withCorrespondences.csv"))  
    }
  } else if (include_correspondence == "no"){
    if (user == "dhourani"){
      fwrite(workplaceData , paste0("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/workplaceData",work.asgs,".csv"))
    } else if (user == "hbatrouney"){
      fwrite(workplaceData , paste0("C:/Users/hbatrouney/Documents/Spatial structure of cities/Basefile/workplaceData",work.asgs,".csv"))  
    } else if (user == "jamesha"){
      fwrite(workplaceData , paste0("/Users/jamesha/Documents/Spatial structure of cities/Basefile/workplaceData",work.asgs,".csv"))  
    }
  } 
}