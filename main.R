
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(data.table)) install.packages("data.table") 
if(!require(ggmap)) install.packages("ggmap")
if(!require(lubridate)) install.packages("lubridate")

dat <- fread("trafficdatav8.csv", header = T, sep = ",", colClasses = 'character')

###Cleaning data

colnames(dat) <- gsub(" ", "_", colnames(dat)) #better column format
colnames(dat) <- tolower(colnames(dat)) 

## date_reported appears to be in a broken ISO 8601 format
## some values are YYYY-MM-DDTHH:MM:SS
## Dirty date appears to be in the following format
## MDDYYYY-MM-01-01THH:MM:SS

substrFromEnd <- function(str, n) {
  #We will take last n characters from the end of the tring
  substr(str, nchar(str) - n+1, nchar(str))
}

cleandate_reported <- function(x) { #O(n) performance
  
  #ISO 8601 is mandated to have 19 characters, we will check if string has 19 characters

  # If extract the MDD portion, we will be able to identify the month and day from data
  # DD will always be the last 2 characters in the MDD format, M will be the remaining
  
  output <- character(length(x)) # initalising output
  dirty.condition <- nchar(x) != 19 #check for condition prior to loop
  
  for (i in 1:length(x)) {
    
    if(dirty.condition[i]){ #check only elements that are dirty
      cleanDate <- substrFromEnd(x[i], 19) #extract datetime with correct year
      lostMDD <- substr(x[i], 1, nchar(x[i])- 19) #extract the remaining MDD

      substr(cleanDate,9,10) = substrFromEnd(lostMDD, 2) 
      substr(cleanDate,6,7) = ifelse(nchar(lostMDD) == 3, 
                                      paste0("0",substr(lostMDD, 1, nchar(lostMDD) - 2)),
                                      substr(lostMDD, 1, nchar(lostMDD) - 2))
      
      output[i] <- cleanDate #character will be coerced into POSIXct
    }
    else output[i] <- x[i] 
  }
  return(output) #output will be a character vector
}

## TODO: Add time back into date_occurred column (separate column)
# Similar to the date_reported issue, date_occurred is presented with only the date for which an accident occrred
# time_occured is presented in HHmm format

fix_date_time <- function(date,time) { #O(n) performance
  
  #Setting conditions for input variables
  
  if(length(date) != length(time)) stop("date and time vector are not the same length")
  
  output <- character(length(date)) # initalising output
  
  for (i in 1:length(date)) {
      
      substr(date[i],12,13) = substr(time[i], 1, 2) # hours in HH
      substr(date[i],15,16) =  substrFromEnd(time[i], 2) # minutes in MM  
      output[i] <- date[i] #character will be coerced into POSIXct
      
    }
  return(output) #output will be a character vector
}


## Finish cleaning

dat <- dat %>% mutate(date_reported_clean = cleandate_reported(date_reported),
                      datetime_occured = fix_date_time(date = date_occurred, time = time_occurred))

dat$date_reported_clean <- as.POSIXct(dat$date_reported_clean, "America/Los_Angeles", format = "%Y-%m-%dT%H:%M:%S")
dat$datetime_occured <- as.POSIXct(dat$datetime_occured, "America/Los_Angeles", format = "%Y-%m-%dT%H:%M:%S")

dat$address <- gsub(' +', ' ', dat$address) #remove unncessary whitespace
dat$cross_street <- gsub(' +', ' ', dat$cross_street) #remove unncessary whitespace

## TODO: Extract Lat and Long coordinates from location column

accidents.bymonth <- dat %>% filter(datetime_occured < '2019-01-01') %>% group_by(MonthYear = floor_date(datetime_occured, "month")) %>% summarise(Accidents = n())
accidents.byday <- dat %>% group_by(DayOfWeek = wday(datetime_occured)) %>%  summarise(Accidents = n())
accidents.hour <- dat  %>% group_by(HourOfDay = hour(datetime_occured)) %>%  summarise(Accidents = n())
#accidents. <- dat %>% group_by(DayOfWeek = wday(datetime_occured)) %>%  summarise(Accidents = n())

accidents.byarea <- dat %>% filter(datetime_occured < '2019-01-01') %>% group_by(area_name, YearOccurred = year(datetime_occured)) %>% summarise(Accidents = n())
accidents.byarea <- accidents.byarea %>% mutate(YoYGrowth = (Accidents - lag(Accidents)) / Accidents)

victim.gender <- dat %>% group_by(area_name, victim_sex) %>% summarise(Accidents = n())

ggplot(accidents.byarea, aes(x = YearOccurred, y= Accidents)) + geom_line() + facet_wrap(~area_name)
ggplot(accidents.byarea, aes(x = YearOccurred, y= Accidents, color = area_name)) + 
  geom_line() + geom_vline(xintercept = 2014)

ggplot(accidents.byarea, aes(x = YearOccurred, y= YoYGrowth, color = area_name)) + 
  geom_smooth() + geom_vline(xintercept = 2014)

ggplot(accidents.bymonth, aes(x = MonthYear, y = Accidents)) + geom_line(stat = "identity") +geom_smooth()
ggplot(accidents.byday, aes(x = DayOfWeek, y = Accidents)) + geom_bar(stat = "identity")
ggplot(accidents.hour, aes(x = HourOfDay, y = Accidents)) + geom_bar(stat = "identity")
=======

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(data.table)) install.packages("data.table") 
if(!require(ggmap)) install.packages("ggmap")
if(!require(lubridate)) install.packages("lubridate")

dat <- fread("trafficdatav8.csv", header = T, sep = ",", colClasses = 'character')

###Cleaning data

colnames(dat) <- gsub(" ", "_", colnames(dat)) #better column format
colnames(dat) <- tolower(colnames(dat)) 

## date_reported appears to be in a broken ISO 8601 format
## some values are YYYY-MM-DDTHH:MM:SS
## Dirty date appears to be in the following format
## MDDYYYY-MM-01-01THH:MM:SS

substrFromEnd <- function(str, n) {
  #We will take last n characters from the end of the tring
  substr(str, nchar(str) - n+1, nchar(str))
}

cleandate_reported <- function(x) { #O(n) performance
  
  #ISO 8601 is mandated to have 19 characters, we will check if string has 19 characters

  # If extract the MDD portion, we will be able to identify the month and day from data
  # DD will always be the last 2 characters in the MDD format, M will be the remaining
  
  output <- character(length(x)) # initalising output
  dirty.condition <- nchar(x) != 19 #check for condition prior to loop
  
  for (i in 1:length(x)) {
    
    if(dirty.condition[i]){ #check only elements that are dirty
      cleanDate <- substrFromEnd(x[i], 19) #extract datetime with correct year
      lostMDD <- substr(x[i], 1, nchar(x[i])- 19) #extract the remaining MDD

      substr(cleanDate,9,10) = substrFromEnd(lostMDD, 2) 
      substr(cleanDate,6,7) = ifelse(nchar(lostMDD) == 3, 
                                      paste0("0",substr(lostMDD, 1, nchar(lostMDD) - 2)),
                                      substr(lostMDD, 1, nchar(lostMDD) - 2))
      
      output[i] <- cleanDate #character will be coerced into POSIXct
    }
    else output[i] <- x[i] 
  }
  return(output) #output will be a character vector
}

## TODO: Add time back into date_occurred column (separate column)
# Similar to the date_reported issue, date_occurred is presented with only the date for which an accident occrred
# time_occured is presented in HHmm format

fix_date_time <- function(date,time) { #O(n) performance
  
  #Setting conditions for input variables
  
  if(length(date) != length(time)) stop("date and time vector are not the same length")
  
  output <- character(length(date)) # initalising output
  
  for (i in 1:length(date)) {
      
      substr(date[i],12,13) = substr(time[i], 1, 2) # hours in HH
      substr(date[i],15,16) =  substrFromEnd(time[i], 2) # minutes in MM  
      output[i] <- date[i] #character will be coerced into POSIXct
      
    }
  return(output) #output will be a character vector
}


## Finish cleaning

dat <- dat %>% mutate(date_reported_clean = cleandate_reported(date_reported),
                      datetime_occured = fix_date_time(date = date_occurred, time = time_occurred))

dat$date_reported_clean <- as.POSIXct(dat$date_reported_clean, "America/Los_Angeles", format = "%Y-%m-%dT%H:%M:%S")
dat$datetime_occured <- as.POSIXct(dat$datetime_occured, "America/Los_Angeles", format = "%Y-%m-%dT%H:%M:%S")

dat$address <- gsub(' +', ' ', dat$address) #remove unncessary whitespace
dat$cross_street <- gsub(' +', ' ', dat$cross_street) #remove unncessary whitespace

## TODO: Extract Lat and Long coordinates from location column

accidents.bymonth <- dat %>% filter(datetime_occured < '2019-01-01') %>% group_by(MonthYear = floor_date(datetime_occured, "month")) %>% summarise(Accidents = n())
accidents.byday <- dat %>% group_by(DayOfWeek = wday(datetime_occured)) %>%  summarise(Accidents = n())
accidents.hour <- dat  %>% group_by(HourOfDay = hour(datetime_occured)) %>%  summarise(Accidents = n())
#accidents. <- dat %>% group_by(DayOfWeek = wday(datetime_occured)) %>%  summarise(Accidents = n())

accidents.byarea <- dat %>% filter(datetime_occured < '2019-01-01') %>% group_by(area_name, YearOccurred = year(datetime_occured)) %>% summarise(Accidents = n())
accidents.byarea <- accidents.byarea %>% mutate(YoYGrowth = (Accidents - lag(Accidents)) / Accidents)

victim.gender <- dat %>% group_by(area_name, victim_sex) %>% summarise(Accidents = n())

ggplot(accidents.byarea, aes(x = YearOccurred, y= Accidents)) + geom_line() + facet_wrap(~area_name)
ggplot(accidents.byarea, aes(x = YearOccurred, y= Accidents, color = area_name)) + 
  geom_line() + geom_vline(xintercept = 2014)

ggplot(accidents.byarea, aes(x = YearOccurred, y= YoYGrowth, color = area_name)) + 
  geom_smooth() + geom_vline(xintercept = 2014)

ggplot(accidents.bymonth, aes(x = MonthYear, y = Accidents)) + geom_line(stat = "identity") +geom_smooth()
ggplot(accidents.byday, aes(x = DayOfWeek, y = Accidents)) + geom_bar(stat = "identity")
ggplot(accidents.hour, aes(x = HourOfDay, y = Accidents)) + geom_bar(stat = "identity")
>>>>>>> efc6cb8c05cdb07d193f9091188bbb8c4b24eb25
