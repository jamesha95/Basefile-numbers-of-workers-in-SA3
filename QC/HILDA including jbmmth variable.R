user <- "jaha" #choose from hbatrouney or dhourani or jaha

if (user == "hbatrouney")
{setwd("C:/Users/hbatrouney/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/Tables/HILDA")
}else if (user == "dhourani")
{setwd("/Users/dhourani/Dropbox/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/Tables/HILDA")
}else if (user == "jaha"){
  setwd("/Users/jaha/Dropbox (Grattan Institute)/Transport Program/Project - Spatial structure of cities/Spatial structure/Output/Tables/HILDAqc")}

library(survey)
library(haven)
library(data.table)
library(hutils)
library(Weighted.Desc.Stat)
library(speedglm)
library(tidyr)
library(grattan)
library(viridis)
library(lme4)
library(testthat)
library(fastmatch)
library(dplyr)
library(dtplyr)
library(data.table)
library(magrittr)
library(rstanarm)
library(ggrepel)
library(ggplot2)
library(bit64)
library(hutils)

letters <- c("a", "b" , "c", "d", "e", "f" , "g" , "h" , "i", "j" , "k" , "l" , "m", "n" , "o" , "p")

for (i in 1:16) {
  wave <- letters[i]
  HILDAtemp <- read_sas(paste0("/Users/jaha/Documents/Spatial structure of cities/HILDA data/2. SAS 160c (Zip File 1 of 2 - Combined Data Files)/Combined_", wave, "160c.sas7bdat"))
  assign(paste0("Combined_", wave, "160c") , HILDAtemp, globalenv())
  
  HILDAtemp <- NULL
}
########################################################################################################################################################################################################
# The above code takes a VERY long time to run. Once the data is loaded, just run calculations from this point onwards.
########################################################################################################################################################################################################

commute_data <-
  lapply(1:16, function(y) {
    wave <- letters[y]
    yr <- y + 2000
    
    relevant_columns <- 
      c("xwaveid",
        paste0(wave, c("hhrhid",   # household id
                       "hgage",    # age
                       "hhmove",   # moved house?
                       "pjmsemp" ,  #moved job?
                       "lshrcom",   # Hours per week - Travelling to and from a place of paid employment
                       "lsmncom",   # Minutes per week - Travelling to and from a place of paid employment
                       "mhreawp" ,  #Main reason for moving - To be nearner place of work
                       "pjljrea" ,  #Main reason for changing job 
                       "esdtl" ,   #Labour force status -detail
                       "jbmday1" , #Worked Monday
                       "jbmday2" , #Worked Tuesday  
                       "jbmday3" , #Worked Wednesday
                       "jbmday4" , #Worked Thursday
                       "jbmday5" , #Worked Friday
                       "jbmday6" , #Worked Saturday
                       "jbmday7"  ,#Worked Sunday
                       "jbmday" , #Type of work schedule
                       "jbmmth" , #Number of days worked in the last four weeks
                       "pjsemp" , #Working for same employer as last year
                       "hhmsr" , #City
                       "hhwtrp" #weight
        ))) 
    
    new_names <- c("xwaveid",
                   "hh_id",
                   "age",
                   "moved_house",
                   "moved_job" ,
                   "hours_travelling",
                   "minutes_travelling", 
                   "move_reason", 
                   "job_change_reason", 
                   "lf_status" ,   #Labour force status -detail
                   "work_Monday" ,
                   "work_Tuesday" ,
                   "work_Wednesday" ,
                   "work_Thursday" ,
                   "work_Friday" ,
                   "work_Saturday" ,
                   "work_Sunday" ,
                   "work_schedule" ,
                   "work_last_month",
                   "same_employer_as_ly" , 
                   "region" ,
                   "weight"
    )
    
    renamer <- data.table(relevant_columns = relevant_columns, new_names = new_names)
    
    select_relevant_columns <- function(DT) {
      noms <- names(DT)
      
      intersection <- intersect(noms, relevant_columns)
      if (length(intersection) > 0) {
        out <-
          DT %>%
          .[, .SD, .SDcols = intersection] %>%
          setnames(old = renamer[relevant_columns %in% intersection][["relevant_columns"]],
                   new = renamer[relevant_columns %in% intersection][["new_names"]])
      } else {
        out <- data.table:::null.data.table()
      }
      return(out)
    }
    
    get(paste0("Combined_", wave, "160c")) %>%
      setDT %>%
      select_relevant_columns %>%
      .[, year := yr]
  }) %>%
  rbindlist(use.names = TRUE, fill = TRUE) %>%
  .[move_reason < 0 , move_reason := NA] %>% 
  .[weight == -10 , weight := NA] %>%  # non-responding people
  
  setDT(commute_data)
for(j in grep("work_|travelling" , names(commute_data))){
  set(commute_data, i= which(commute_data[[j]]<0), j= j, value=NA)
}

#Tidy up moved work variable (recoded from data dictionary https://www.online.fbe.unimelb.edu.au/HILDAodd/VariableDetails.aspx?varn=pjmsemp&&varw=3)
commute_data[moved_job < 0 , moved_job := NA][ 
  moved_job == 2 , moved_job := 0][ #If moved_job = 2 that means they didn't move job
    moved_job == 1 , moved_job := 1] #If moved_job = 1 that means they moved job

#Tidy up labour force status (recoded from data dictionary https://www.online.fbe.unimelb.edu.au/HILDAodd/VariableDetails.aspx?varn=esdtl&&varw=4)
commute_data[lf_status %ein% c(5,6) , lf_status_text := "NILF"][ 
  lf_status %ein% c(3,4) , lf_status_text := "UNEMP"][ 
    lf_status %ein% c(1,2) , lf_status_text := "EMP"][
      lf_status < 0 , lf_status_text := NA][ 
        , lf_status := NULL] 

#Tidy up labour force status (recoded from data dictionary https://www.online.fbe.unimelb.edu.au/HILDAodd/VariableDetails.aspx?varn=esdtl&&varw=4)
commute_data[same_employer_as_ly < 0 , same_employer_as_ly := NA][
  same_employer_as_ly == 2 , same_employer_as_ly := 0
  ]

setnames(commute_data , "lf_status_text" , "lf_status")

#Tidy up days worked data (recoded from data dictionary)
commute_data[work_schedule == 1 , days_worked := 5][ #If work_schedule = 1 that means they worked 5 days a week
  work_schedule == 2 , days_worked := 4.5 ][ #If work_schedule = 2 that means they worked a nine day fortnight
    work_schedule == 8, days_worked := work_Monday +  work_Tuesday + work_Wednesday +  work_Thursday + work_Friday + work_Saturday + work_Sunday][ #If work_schedule = 8 that means other and they were asked which days they worked
      work_schedule == 3 | work_schedule == 4, days_worked := work_last_month/4] #If work_schedule = 3 or 4, that means their schedule varies a lot - they were then asked how many days in a 4 week period they work
#commute_data[, which(grepl("^work_", colnames(commute_data))):=NULL]
commute_data[is.na(minutes_travelling) , minutes_travelling := 0] 

#I'm going to check whether you can have an NA for mins travelled but still have a valid hours travelled, and vice-versa. Ans: Yes, then no. So that's why NA mins are set to 0.
#commute_data[is.na(minutes_travelling) & hours_travelling != 0] 
#commute_data[is.na(hours_travelling) & minutes_travelling != 0]

commute_data[days_worked > 0  , commute.time := ((hours_travelling *60 + minutes_travelling) / days_worked) / 2] #Multiply by 60 to get minutes, divide by 2 to get each way
#commute_data[, which(grepl("travelling", colnames(commute_data))):=NULL]

#Tidy up region data (recoded from data dictionary)
commute_data[region == 11 , region_text := "Sydney"] %>% 
  .[region == 19 , region_text := "Balance of NSW"] %>%
  .[region == 21 , region_text := "Melbourne"] %>%
  .[region == 29 , region_text := "Balance of VIC"] %>%
  .[region == 31 , region_text := "Brisbane"] %>% 
  .[region == 39 , region_text := "Balance of QLD"] %>% 
  .[region == 41 , region_text := "Adelaide"] %>% 
  .[region == 49 , region_text := "Balance of SA"] %>% 
  .[region == 51 , region_text := "Perth"] %>% 
  .[region == 59 , region_text := "Balance of WA"] %>% 
  .[region == 61 , region_text := "Tasmania"] %>% 
  .[region == 71 , region_text := "Northern Territory"] %>% 
  .[region == 81 , region_text := "ACT"] %>%
  .[region == -7 , region_text := NA] 

region_list <- c(commute_data[!(region_text %>% is.na) , .N , by = region_text] %>% .[[1]])
#Get rid of unreasonable looking commute times e.g. more than 3 hours each way
time.threshold <- 3*60
commute_data <- commute_data[commute.time < time.threshold] 

#######################################################################################################################################
#Average commute times all Australia
#######################################################################################################################################

travel <- svydesign(id=~xwaveid , weights=~weight,data=commute_data[!is.na(weight) & commute.time >0 ] )
commute_means <- svyby(~commute.time , by =~year , design=subset(travel, commute.time>0) , FUN = svymean , na.rm = TRUE ) %>% as.data.table 
commute_means %>% fwrite( . , "Commute Means - HILDA.csv")

#######################################################################################################################################
#Average commute times by city
#######################################################################################################################################

for (j in region_list){
  travel2 <- svydesign(id=~xwaveid , weights=~weight,data=commute_data[!is.na(weight) & commute.time >0 & region_text == j] )
  commute_means <- svyby(~commute.time , by =~year , design=subset(travel, commute.time>0) , FUN = svymean , na.rm = TRUE ) %>% as.data.table 
  commute_means %>% fwrite( . , paste0("Commute Means",j," - HILDA.csv"))
}

#qantiles <- c(0 , 0.1 , 0.2 , 0.3 , 0.4 , 0.5 , 0.6 , 0.7 , 0.8 , 0.9 , 0.99)
qantiles <- c(0.25 , 0.5, 0.75 , 0.9 , 0.99)

quantiles_list <- NULL
start_year <- 2004
end_year <- 2016
by_year <- 4
sequence <- c(seq(from=start_year, to=end_year , by= by_year))

#######################################################################################################################################
#Travel quantiles for all Australia
#######################################################################################################################################

travel <- svydesign(id=~xwaveid , weights=~weight,data=commute_data[!is.na(weight) & commute.time >0 ] )
for (yr in  sequence){
  quantiles_data <- svyquantile (~commute.time , design=subset(travel, commute.time>0 & year == yr) , qantiles) %>% 
    as.data.table %>% 
    .[, year := yr] 
  assign(paste0("qantiles_data",yr) ,quantiles_data , envir = globalenv())
  list_ref_temp <- (yr - start_year) / by_year + 1
  quantiles_list[[list_ref_temp]] <- paste0("qantiles_data",yr)
}

quantiles_list <- lapply(quantiles_list,get)
qantiles_data <- do.call(rbind, quantiles_list)
qantiles_data %>% write.csv( . , "Commute Quantiles - HILDA.csv")

#######################################################################################################################################
#Travel quantiles by city
#######################################################################################################################################
travel <- svydesign(id=~xwaveid , weights=~weight,data=commute_data[!is.na(weight) & commute.time >0 ] )
for (city in region_list){
  for (yr in  sequence){
    quantiles_data <- svyquantile (~commute.time , design=subset(travel, commute.time > 0 & year == yr & region_text == city) , qantiles) %>% 
      as.data.table %>% 
      .[, year := yr] 
    assign(paste0("qantiles_data",yr) ,quantiles_data , envir = globalenv())
    list_ref_temp <- (yr - start_year) / by_year + 1
    quantiles_list[[list_ref_temp]] <- paste0("qantiles_data",yr)
  }
  
  quantiles_list <- lapply(quantiles_list,get)
  qantiles_data <- do.call(rbind, quantiles_list) 
  qantiles_data %>% write.csv( . , paste0("Commute Quantiles - HILDA",city,".csv"))
}

#######################################################################################################################################
#Travel histograms by city
#######################################################################################################################################
travel <- svydesign(id=~xwaveid , weights=~weight,data=commute_data[!is.na(weight) & commute.time >0 ] )
for (city in c("Sydney","Melbourne","Brisbane","Perth","Adelaide","ACT")){
  for (yr in  sequence){
    svyhist (~commute.time , design=subset(travel, commute.time > 0 & year == yr & region_text == city), breaks = seq(0,time.threshold,15),probability = TRUE,main = paste(city,yr)) 
  }
}

#######################################################################################################################################
#Analysis of persistence rates in long commutes 
#######################################################################################################################################
commute_data_temp <- commute_data %>% copy %>% .[ , c("xwaveid" , "commute.time" , "year")]
#Long to wide
commute_data_wide <- dcast(commute_data_temp, xwaveid ~ year, value.var = c("commute.time" ))
commute_data_temp <- NULL
#Shift years by one year to create variable "commute time last year"
commute_data_wide_lagged  <-  setnames(commute_data_wide , names(commute_data_wide) , c("xwaveid"  , names(commute_data_wide)[3:17] , "2017")) 
commute_data_wide_lagged[ , "2017" := NULL]
setnames(commute_data_wide_lagged , names( commute_data_wide_lagged)[2:16] , paste0("commute.time.last.year_",names(commute_data_wide)[2:16]))  
#Wide back to long
commute_data_long2 <-  reshape(commute_data_wide_lagged, varying = grep("commute.time.last.year" , names(commute_data_wide_lagged) , value = TRUE), timevar = "year", idvar = "xwaveid", direction = "long", sep = "_")
#Merge new variable (commute time last year) onto existing dataset
commute_data_long2[ , xwaveid := as.character(xwaveid)]  
commute_data[ , xwaveid := as.character(xwaveid)]
commute_data <- commute_data_long2[commute_data, on = c("xwaveid==xwaveid" , "year==year")]

long_commute_threshold <- 90
#Analysis - number of people who moved home or changed jobs since last year if they had a long commute time last year 
commute_data[commute.time.last.year >= long_commute_threshold , sum(weight) , by = commute.time >= long_commute_threshold]
commute_data[commute.time.last.year >= long_commute_threshold & commute.time < long_commute_threshold , sum(weight) , by = moved_job]
commute_data[commute.time.last.year >= long_commute_threshold & commute.time < long_commute_threshold , sum(weight) , by = moved_house]

#######################################################################################################################################
#Analysis of percentages of people making certain commutes
#######################################################################################################################################
commutes_of_interest <- c(15,30,45,60,75,90)
for (time in commutes_of_interest){
  time %>% print
  commute_percs <- commute_data %>% copy %>% 
    .[ , sumweight := .(sum(weight)), by = .(year, region_text)] %>%
    .[commute.time <  time , less_than_time:= .(sum(weight))  , by = .(year, region_text)] %>% 
    .[is.na(less_than_time) , less_than_time:= 0 ] %>% 
    .[ , .(total = min(sumweight) , less_than_time = max(less_than_time)), by = .(year, region_text)] %>% 
    .[ , `:=` (less_than_time = less_than_time/total , total = NULL) ]
  setnames(commute_percs , "less_than_time" , paste0("perc_less_than_time_" , time) )
  assign(paste0("commute_percs_" , time) , commute_percs , env = globalenv())
}

data_list <- list()
for (n in commutes_of_interest){
  data_list[[n/15]] = get(paste0("commute_percs_" , n))
}

mymerge <- function(x,y){}
mymerge <- function(x,y){
  setkey(x, year, region_text)
  setkey(y, year, region_text)
  x[y]
}

commute_percs <- Reduce(mymerge,(data_list)) %>% fwrite( . , paste0("Percent journeys by commute times - HILDA.csv"))

#######################################################################################################################################
#Analysis of percentages of people making ZERO TIME commutes
#######################################################################################################################################
commute_perc <- commute_data %>% copy %>% 
  .[ , sumweight := .(sum(weight)), by = .(year, region_text)] %>%
  .[commute.time ==  0 , less_than_time:= .(sum(weight))  , by = .(year, region_text)] %>% 
  .[is.na(less_than_time) , less_than_time:= 0 ] %>% 
  .[ , .(total = min(sumweight) , less_than_time = max(less_than_time)), by = .(year, region_text)] %>% 
  .[ , `:=` (less_than_time = less_than_time/total , total = NULL) ]
setnames(commute_perc , "less_than_time" , paste0("perc_less_than_time_" ,0) )
assign(paste0("commute_perc_" ,0) , commute_perc , env = globalenv())

data_list1 <- list()
data_list1[[1]] = get(paste0("commute_perc_" ,0))

mymerge <- function(x,y){}
mymerge <- function(x,y){
  setkey(x, year, region_text)
  setkey(y, year, region_text)
  x[y]
}

commute_perc <- Reduce(mymerge,(data_list1)) %>% fwrite( . , paste0("Percent journeys of zero commute time - HILDA.csv"))

