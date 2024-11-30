#set working directory ####
#setwd("~/553_WLF)
list.files()#list all files in the directory where you are located
#Load packages ####
library(tidyverse)
library(DBI)
library(rlang)
#load data
dragons_db <- dbConnect(drv = RSQLite::SQLite(),
                         "databases_WLF/dragons.db")#this is the path, not SQL, no ;

#list all tables in the database
dbListTables(dragons_db)




#load all tables ####
dragons <-dbGetQuery(conn = dragons_db,
                     statement = "SELECT * FROM dragons;")
dim(dragons)
glimpse(dragons)
capture_sites <-  dbGetQuery(dragons_db,
                        "SELECT * FROM capture_sites;")
glimpse(capture_sites)

captures <- dbGetQuery(dragons_db, "SELECT * FROM captures;")
glimpse(captures)
morphometrics <-  dbGetQuery(dragons_db, "SELECT * FROM morphometrics;")


diet_contents <-  dbGetQuery(dragons_db,
                             "SELECT * FROM diet_samples")
glimpse(diet_contents)

deployments <- dbGetQuery(dragons_db,
                          "SELECT * FROM deployments;")

gps <-  dbGetQuery(dragons_db,
                   "SELECT * FROM gps_data;")

mortalities <-  dbGetQuery(dragons_db,
                    "SELECT * FROM mortalities;")
glimpse(mortalities)
tags <-  dbGetQuery(dragons_db,
                           "SELECT * FROM tags;")

glimpse(tags)
morphometrics %>% 
  left_join(captures, by = "capture_event") %>% 
  view()

captures %>% 
  left_join(diet_contents, by="dragon_id") %>% 
  view()

capture_sites %>% 
  left_join(captures, by="site") %>% 
  left_join(morphometrics, by = "capture_event") %>% 
  view()

morphometrics %>% 
  left_join(captures, by="capture_event") %>% 
  left_join(capture_sites, by = "site") %>% 
  view()
  
 
dbListTables(dragons_db)


#select columns from a data frame ####
head(morphometrics)
select(morphometrics,
       capture_event,
       total_body_length_cm,
       wingspan_cm)
glimpse(morphometrics)


#filter rows in a data frame ####
#conditional sub-setting
step1 <- filter(morphometrics, total_body_length_cm > 1000)

#base R morphometrics[morphometrics$total_body_length_cm > 1000, ]

select(step1,
       capture_event,
       total_body_length_cm,
       wingspan_cm)

# the pipe operator ####
morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  select(capture_event,
         total_body_length_cm,
         wingspan_cm)


# create new columns with mutate ####
morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  select(capture_event,
         total_body_length_cm,
         wingspan_cm) %>% 
  mutate(total_body_length_m = total_body_length_cm/100,
         wingspan_m = wingspan_cm/100) %>% 
  select(-total_body_length_cm,
         -wingspan_cm) %>% 
  slice(1:5)#only print first 5 rows

#alternate method for creating new columns
morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  mutate(total_body_length_m = total_body_length_cm/100,
         wingspan_m = wingspan_cm/100) %>% 
  select(capture_event, total_body_length_m,
         wingspan_m) %>% 
  slice(1:5)#only print first 5 rows

obj1 <- morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  mutate(total_body_length_m = total_body_length_cm/100,
         wingspan_m = wingspan_cm/100) %>% 
  select(capture_event, total_body_length_m,
         wingspan_m) %>% 
  view()
#could also use glimpse() instead of view()
#base R could use str() equivalent to glimpse()

str(obj1)
class(obj1)
# tibbles ####
obj1 <- as.tibble(obj1)
class(obj1)
# joining tables ####
?left_join
glimpse(obj1)
glimpse(captures)
glimpse(dragons)
#below the left table is already defined so only need the right table and how
#the tables are being joined, by= column that links the two tables
obj1 %>% 
  left_join(captures, by = "capture_event") %>%
  left_join(dragons, by = "dragon_id") %>% 
  as.tibble()

obj2 <- obj1 %>% 
  left_join(captures, by = "capture_event") %>% 
  view()

obj2 %>% 
  left_join(dragons, by = "dragon_id") %>% 
  as.tibble() %>% 
  view()






#calculate summary statistics ####

#count number of dragons by species
morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  mutate(total_body_length_m = total_body_length_cm/100,
         wingspan_m = wingspan_cm/100) %>% 
  select(capture_event, total_body_length_m,
         wingspan_m) %>% 
  left_join(captures, by = "capture_event") %>%
  left_join(dragons, by = "dragon_id") %>% 
  as.tibble() %>% 
  group_by(species) %>% 
  tally() %>% 
  arrange(desc(n))


#mean wingspan by sex

morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  mutate(total_body_length_m = total_body_length_cm/100,
         wingspan_m = wingspan_cm/100) %>% 
  select(capture_event, total_body_length_m,
         wingspan_m) %>% 
  left_join(captures, by = "capture_event") %>%
  left_join(dragons, by = "dragon_id") %>% 
  as.tibble() %>%
  group_by(sex) %>% 
  summarize(min_wingspan_m = min(wingspan_m),
            mean_wingspan_m = mean(wingspan_m),
            max_wingspan_m = max(wingspan_m),
            sd_wingspan_m = sd(wingspan_m))
#group_by automatically converts a df into a tibble

#----------------------------------------------------------------
library(tidyverse)
data()

msleep %>% 
  drop_na(sleep_rem, vore) %>% 
  group_by(vore) %>% 
  summarise('Average total sleep' = mean(sleep_total),
            'Maximun rem sleep' = max(sleep_rem)) %>% 
  view()


#more left join commands
#state abbr has a different name in each of the two data frames
state_geog <- tibble(state = state.abb,
                  area = state.area,
                  center_long = state.center$x,
                  center_lat = state.center$y)
state_reg <- tibble(name = state.name,
                    abbr = state.abb,
                    region = state.region)
state_geog
state_reg
states <- state_geog %>% 
  left_join(state_reg, by = c("state" = "abbr")) %>% 
  select(name,
         abbr = state,
         region,
         everything())
head(states)#now reorder the columns

  