# Load packages ####
library(tidyverse)
library(DBI)
library(viridis)# color palette
#Load data, establish connection with dragons database
#do NOT put ; 
db_conn <- dbConnect(drv = RSQLite::SQLite(),
                     "databases_WLF/dragons.db")

#Load tables
# put ; at the end
dragons <- dbGetQuery(db_conn, "SELECT * FROM dragons;")
morphometrics <- dbGetQuery(db_conn, "SELECT * FROM morphometrics;")
captures <- dbGetQuery(db_conn, "SELECT * FROM captures;")
head(dragons)

#ggplot2 ####
## scatterplot ####
#inside ggplot function need
#data and aes(), then add layers with + signs
ggplot(data = morphometrics,
       mapping = aes(x=total_body_length_cm,
                     y = wingspan_cm))+
  geom_point() + #geometry
  labs(x = "Total Body Length (cm)",
       y = "Wingspan (cm)") +
  #theme_bw()
  #theme_minimal()
  theme_light()

#Two cases of using colors
#colors encoded by hex codes
#If colors not encoding a variable, goes into geometry, DOES NOT go in aes()
ggplot(data = morphometrics,
       mapping = aes(x=total_body_length_cm,
                     y = wingspan_cm))+
  geom_point(col = "darkseagreen") +
  labs(x = "Total Body Length (cm)",
       y = "Wingspan (cm)") +
  #theme_bw()
  #theme_minimal()
  theme_light()

#If color encoding a specific variable, coloring variable goes in aes()
#left join captures
head(captures)
#every time has an aes that is not x or y legend automatically added
#comes with column label, in labs change legend label
#
morphometrics %>% 
  left_join(captures, by = "capture_event") %>% 
  ggplot(mapping = aes(x=total_body_length_cm,
                     y = wingspan_cm,
                    color = age_at_capture))+
  geom_point() +
  labs(x = "Total Body Length (cm)",
       y = "Wingspan (cm)",
       color= "Age") +
  #theme_bw()
  #theme_minimal()
  theme_light()+
  scale_color_viridis_d(option = "plasma",
                        begin = 0.2,
                        end = 0.8)
#d is discrete, c is continuous
#begin and end btw [0, 1], refers to gradient
#make size of point proportional to body weight
morphometrics %>% 
  left_join(captures, by = "capture_event") %>% 
  ggplot(mapping = aes(x=total_body_length_cm,
                       y = wingspan_cm,
                       color = age_at_capture,
         size = tarsus_length_cm)) +
  geom_point() +
  labs(x = "Total Body Length (cm)",
       y = "Wingspan (cm)",
       color= "Age",
       size = "Tarsus length (cm)") +
  #theme_bw()
  #theme_minimal()
  theme_light()+
  scale_color_viridis_d(option = "plasma",
                        begin = 0.2,
                        end = 0.8)

#use different shape for male and female
morphometrics %>% 
  left_join(captures, by = "capture_event") %>%
  left_join(dragons, by = "dragon_id") %>% 
  mutate(sex = case_when(
    is.na(sex) ~ "Unknown",#replace NA with unknown
    TRUE ~ sex
  )) %>% 
  ggplot(mapping = aes(x=total_body_length_cm,
                       y = wingspan_cm,
                       color = age_at_capture,
                       size = tarsus_length_cm,
                      shape = sex)) +
  geom_point() +
  #facet_wrap(~species)+
  labs(x = "Total Body Length (cm)",
       y = "Wingspan (cm)",
       color= "Age",
       size = "Tarsus length (cm)",
       shape = "Sex") 
  #theme_bw()
  #theme_minimal()
  # theme_light()+
  # scale_color_viridis_d(option = "plasma",
  #                       begin = 0.2,
  #                       end = 0.8)+
  
#create multiple panels, each species get own panel
#use facet wrap, place after geometry
morphometrics %>% 
  left_join(captures, by = "capture_event") %>%
  left_join(dragons, by = "dragon_id") %>% 
  mutate(sex = case_when(
    is.na(sex) ~ "Unknown",#replace NA with unknown
    TRUE ~ sex
  )) %>% 
  ggplot(mapping = aes(x=total_body_length_cm,
                       y = wingspan_cm,
                       color = age_at_capture,
                       size = tarsus_length_cm),
                      shape = sex) +
  geom_point() +
  facet_wrap(~species)+
  labs(x = "Total Body Length (cm)",
       y = "Wingspan (cm)",
       color= "Age",
       shape = "Sex") +
  #theme_bw()
  #theme_minimal()
  theme_light()+
  scale_color_viridis_d(option = "plasma",
                        begin = 0.2,
                        end = 0.8) +
  scale_shape_manual(values = c(7,8,9))




# Histogram ####

ggplot(data = morphometrics,
       aes(x = wingspan_cm))+
  geom_histogram()+
  labs(x= "Wingspan (cm)",
       y = "Frequency")+
       theme_bw()

# Density ####
ggplot(data = morphometrics,
       aes(x = wingspan_cm))+
  geom_density(size = 3)+
  labs(x= "Wingspan (cm)",
       y = "Frequency")+
  theme_light()

# boxplots ####
#only pass y variable
ggplot(data = morphometrics, aes(y = wingspan_cm)) +
  geom_boxplot(fill = "darkseagreen") +
  labs(y = "Wingspan (cm)") +
  theme_light() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000))
  
  
        
#scale_y_continuous(breaks = seq(0, max(morphometrics$wingspan_cm,
#                                      na.rm = TRUE),
#                              length.out = 10))+
       