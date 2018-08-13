user <- "jaha" #choose from hbatrouney or dhourani or jamesha or jaha
save <- "yes" #choose from save "yes" or "no" - only pick yes if you want to save dandelions

#STEPS
#1. Set up working directory and global parameters
#2. Create the function that yields numbers of workers in each destination for both 2016 and 2011 (the "BuildWorkplaceDataset" function)
#   2.1 #workers: Extract relevant data from the "Jobs Growth by SA3 Major Cities" spreadsheet as "WorkData"
#   2.2 Map file: Extract relevant data from the ASGS file as "workShapefile"
#       2.2.1 Add in longitude and latitude of centroids of locations
#   2.4 Check that the locations in WorkData match the locations in workShapefile
#   2.5 basefile: Merge WorkData with workShapefile to create our "basefile"
#       2.5.2 Add a year variable
#3. Run the function for 2011 and 2016
#4. Save the output


#Set the working directory according to which user is running the script
if (user == "hbatrouney")
{setwd("C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure")
}else if (user == "dhourani"){
  setwd("/Users/dhourani/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure")
}else if (user == "jamesha"){
  setwd("/Users/jamesha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure")
}else if (user == "jaha"){
  setwd("/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure")
}
#Load these libraries
library(ASGS)
library(readxl)
library(viridis)
library(rgeos)
library(dplyr)
library(data.table)
library(hutils)


#Note that %>% is piping (i.e. takes the preceeding as input and applies the function that follows).
#Also note that .[!j] in this context is a function that says extract all the data from the data table preceeding, except for row j

#Set global parameters 

work.asgs <- "SA3" #Select main statistical area of work from "DZN" and "SA2" (needs to be in "") )

###live.asgs <- "SA2" #Select main statistical area of residence from "DZN" and "SA2" (needs to be in "") )

#Create basefile - loop over census years
BuildWorkplaceDataset <- function(city) {
#city <- "Melbourne"   ##This line is just for testing parts of the function

  #the following lines just determine what sheet and cells to extract from the Jobs Growth by SA3 Major Cities.xlsx file
  sheetOD <- paste0(city)
  rangeOD <-if(city == "Melbourne"){
      "A1:E42" 
    }else if (city == "Sydney"){
      "A1:E49" 
  }else if (city == "Brisbane"){
      "A1:E41" 
    }
  
  #This WorkData data table is the table that contains the output from the jobs Growth by SA3 major cities spreadsheet.
  #Hence WorkData is five columns
  WorkData <- read_excel("./Data/Jobs Growth by SA3 major cities.xlsx" , sheet = sheetOD , range = rangeOD , col_names = TRUE )%>% as.data.table 
  
  WorkData$SA3 <- WorkData$SA3 %>% as.character
  
  #switch sort of works like an "if", in so far as it's a way of sorting between multiple cases
  #the switch-switch structure belosAw enables enumeration according to a two-step decision process
  #with two options at each stage (like a tree-diagram)
  #the workShapefile is set as a pre-existing SpatialPolygonDataFrame that exists within the ASGS package
  workShapefile <- SA3_2016
  
  
  
  #Calculate centroids and attach to work shapefiles
  centroidsdt <- as.data.table(gCentroid(workShapefile, byid = TRUE))
  #gCentroid is a function that calculates the centroid of a polygon. 
  #'byid" means that it acts on each polygon in the file, rather than the whole polygon of Australia.
  names(centroidsdt) <- c("CentroidLon", "CentroidLat" )
  workShapefile@data <- as.data.table (cbind(workShapefile@data, centroidsdt))
  #We've added the longitude and latitude of each centroid of each SA2/DZN to the workShapefile
  
  #Rename shapefile variables

setnames(workShapefile@data, old = c("SA3_NAME16", "AREASQKM16"), new = c("work_asgs","area"))

  
 
 #  #takes the SA2_20xx data as a data.table
 #  Work_Decoder <-  SA3_2016@data %>% as.data.table
 #  
 #  
 #  #Subset the work data to make sure all the suburbs on the file exist in the shapefiles)
 #  Work_Decoder <- setnames(Work_Decoder,old = c("SA3_NAME16" , "GCC_NAME16"), new = c("decode" , "work_region"))
 #    #i.e. we've changed the names of Work_Decoder so that the SA3 column has been called "decode" and the GCC column is "work_region"
 # WorkData <-
 #      Work_Decoder[LiveWorkData, on = "decode==work", nomatch=0L] %>% #This joins the work data onto the "decoder" table (to make sure the regions on the live / work file are on the live shapefile)
 #      .[, .(work = decode, workers , work_region)] #This again subsets the columns for the variables of interest (work,  workers, work_region)
 #  }
 

  basefile <-
    WorkData[workShapefile@data, on = "SA3==work_asgs", nomatch=0L] %>%
    setnames(c("CentroidLon","CentroidLat"), c("dest_lon", "dest_lat")) %>% 
    .[, .SD, .SDcols = c(grep("lat|lon|SA3|work|area|2011|2016|Growth", names(.), value = TRUE))] 
 
  
  #Adding a city variable (this is relevant because at the end we row_bind the data from each city)
  basefile$city <- city

  setnames(basefile, old = c("2011 raw","2011 scaled to ABS LFS","2016 workers","Growth Rate"),
           new = c("2011raw","2011scaled","2016workers","GrowthRate"))
 
  
  #Assign basefile created here to the global environment called basefile with suffix year - this is so later we can just merge all the years in one big basefile
  assign(paste0("workplaceData",city) , basefile , envir = globalenv())
}

for (i in c( "Melbourne", "Sydney","Brisbane")){BuildWorkplaceDataset (i)}
workplaceData <- dplyr::bind_rows(workplaceDataMelbourne , workplaceDataSydney, workplaceDataBrisbane)


#Save basefile
if (save == "yes"){
    if (user == "dhourani"){
      fwrite(workplaceData , paste0("C:/Users/dhourani/Documents/Spatial structure of cities/Basefile/workplaceData",work.asgs,".csv"))
    } else if (user == "hbatrouney"){
      fwrite(workplaceData , paste0("C:/Users/hbatrouney/Documents/Spatial structure of cities/Basefile/workplaceData",work.asgs,".csv"))  
    } else if (user == "jamesha"){
      fwrite(workplaceData , paste0("/Users/jamesha/Documents/Spatial structure of cities/Basefile/workplaceData",work.asgs,".csv"))
    }else if (user == "jaha"){
      fwrite(workplaceData , paste0("/Users/jaha/Documents/Spatial structure of cities/Basefile/workplaceData",work.asgs,".csv"))
    }
}


