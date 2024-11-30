# dates in R


#timestamp includes both date and time
#base R dates and times ####
today <- "2024-11-14"
class(today)
a <-  as.POSIXct(today)
b <-  as.POSIXlt(today)
str(as.POSIXlt(today))
c <-  as.Date(today)
a - b
b - a
today_eu <- "14-11-2024"
as.POSIXct(today_eu)
as.POSIXlt(today_eu)
as.Date(today_eu)
as.POSIXct(today_eu, format = "%d-%m-%Y")

as.POSIXlt(today_eu, format = "%d-%m-%Y")


right_now <-  "2024-11-14 11:20:00"
as.POSIXct(right_now)
library(tidyverse)

#dates and times with lubridate ####

#turn character date-time into a date-time object
#multiple functions depends on the format of input
#returns same format always
date1 <-  "2024-11-14"
date2 <-  "14-11-2024"
date3 <- "14-11-24"
ymd(date1)
dmy(date2)
dmy(date3)
class(right_now)    
class(dmy(date3))

ymd_hms(right_now)
right_now2 <-  "2024-11-14 11:20"
ymd_hm(right_now2)


#time zones ####

right_now_pst <-  ymd_hms(right_now, tz = "America/Los_Angeles")
ymd_hms("2024-06-14 11:20:00", tz = "America/Los_Angeles")
ymd_hms("2024-06-14 11:20:00", tz = "PST8PDT")


OlsonNames()#returns names of time zones
class(right_now_pst)
#force_tz and with_tz work with objects that already have a time zone
#Forces time zone to be Rome, keep same time
force_tz(right_now_pst, tz = "Europe/Rome")

#returns the equivalent time in Rome, returns military time
with_tz(right_now_pst, tz = "Europe/Rome")

today <- "2024-11-14"
today_pst <-  ymd(today, tz = "America/Los_Angeles")
with_tz(today_pst, tz= "Europe/Rome")#assumes if no time stamp component it is midnight
class(today_pst)

#Doing math on date time objects ####
#assumes it is midnight
today_pst + hours(3)
today_pst - hours(1)
today_pst + seconds(10)

with_tz(right_now_pst, tz = "Europe/Rome") + hours(5)


# set and get components out of an object ####
hour(right_now_pst)
month(right_now_pst)
day(right_now_pst)
year(right_now_pst)

glimpse(planets)


library(DBI)
library(tidyverse)
# when making a connection do not use ;!!
db_conn <-  dbConnect(drv = RSQLite::SQLite(),
                      "databases_WLF/dragons.db")


deployments <- dbGetQuery(db_conn,
                          "SELECT * FROM deployments;")

dragons <- dbGetQuery(db_conn,
                      "SELECT * FROM dragons;")

head(deployments)
class(deployments$start_deployment)
class(deployments$end_deployment)

#turn start and end dates into date-time object ####
#select returns a data frame and pull returns column as a vector
deployments %>% 
  mutate(start_deployment = ymd(start_deployment)) %>% 
  pull(start_deployment) %>% 
  class()

class(ymd("2024-11-14"))
class(ymd("2024-11-14", tz = "America/Los_Angeles"))#assume a time component


head(deployments)
deployments %>% 
  mutate(start_deployment = ymd(start_deployment),
         end_deployment = ymd(end_deployment)) %>% 
  mutate(deployment_duration = difftime(end_deployment, start_deployment))
                                      

deployments %>% 
  mutate(start_deployment = ymd(start_deployment),
         end_deployment = ymd(end_deployment)) %>% 
  mutate(deployment_duration = difftime(end_deployment, start_deployment,
                                        units = "hours")) %>% 
  ggplot(aes(x = dragon_id, y = deployment_duration))+
  geom_bar(stat = "identity")
#stat = "identity" tells R to plot the given values without modification

head(deployments)
deployments %>% 
  mutate(start_deployment = ymd(start_deployment),
         end_deployment = ymd(end_deployment)) %>% 
  mutate(deployment_duration = difftime(end_deployment, start_deployment,
                                        units = "hours")) %>% 
  left_join(dragons) %>% 
  group_by(species) %>% 
  summarise(mean_duration = mean(deployment_duration)) %>% 
  ggplot(aes(x= species, y = mean_duration, fill = species)) +
  geom_bar(stat = "identity") +
  labs(x = " ", y = "Deployment duration (hours)",
       fill = "species")+
  theme_light()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
         
         
ibm <-  read.csv("Raw_data/ibm.csv")         
head(ibm)
glimpse(ibm)

ibm %>% 
  mutate(Date = ymd(Date)) %>% 
  glimpse()
pok <-  read.csv("Raw_data/pokemon.csv")
glimpse(pok)
goog <-  read.csv("Raw_data/google_stock_price.csv")
head(goog)
glimpse(goog)
goog %>% 
  mutate(date = ymd(Date)) %>% 
  glimpse()
Food <- read.csv("Raw_data/foods.csv")
glimpse(Food)
