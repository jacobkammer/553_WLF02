# Load packages ####

library(tidyverse)
library(DBI)
library(skimr)
library(gtExtras)
library(scales)
library(wakefield)
# Load data ####

dragons_db <- dbConnect(RSQLite::SQLite(),
                        "databases_WLF/dragons.db")

dragons <- dbGetQuery(dragons_db,
                      "SELECT * FROM dragons;")
skimr::skim(dragons)
is.na(dragons)

capture_sites <- dbGetQuery(dragons_db,
                      "SELECT * FROM capture_sites;")
captures <- dbGetQuery(dragons_db,
                      "SELECT * FROM captures;")
morphometrics <- dbGetQuery(dragons_db,
                      "SELECT * FROM morphometrics;")
diet_contents <- dbGetQuery(dragons_db,
                      "SELECT * FROM diet_contents;")
diet_samples <- dbGetQuery(dragons_db,
                      "SELECT * FROM diet_samples;")
deployments <- dbGetQuery(dragons_db,
                           "SELECT * FROM deployments;")
gps <- dbGetQuery(dragons_db,
                           "SELECT * FROM gps_data;")
mortalities <- dbGetQuery(dragons_db,
                           "SELECT * FROM mortalities;")
tags <- dbGetQuery(dragons_db,
                           "SELECT * FROM tags;")

dbListTables(dragons_db)

# Select columns from a data frame ####

select(morphometrics, 
       capture_event, 
       total_body_length_cm,
       wingspan_cm) %>% 
  head()

# Filter rows in a data frame ####

step1 <- filter(morphometrics,
       total_body_length_cm > 1000)

morphometrics[morphometrics$total_body_length_cm > 1000, ]

select(step1, 
       capture_event, 
       total_body_length_cm,
       wingspan_cm)

# Concatenate functions with the pipe ####

morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  select(capture_event, 
         total_body_length_cm, 
         wingspan_cm)

class(morphometrics %>% 
  filter(tail_length_cm>200) %>% 
  pull(wingspan_cm))

morphometrics %>% 
  filter(tail_length_cm> 1000 | total_body_length_cm<500) %>% 
  head()
# Create new columns ####
morphometrics %>% 
  mutate(claw_meter = claw_length_cm/100) %>% 
  relocate(claw_meter, .before = total_body_length_cm) %>% 
  head()
morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  select(capture_event, 
         total_body_length_cm, 
         wingspan_cm) %>% 
  mutate(total_body_length_m = total_body_length_cm/100,
         wingspan_m = wingspan_cm/100) %>% 
  select(-total_body_length_cm, 
         -wingspan_cm)

morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  mutate(total_body_length_m = total_body_length_cm/100,
         wingspan_m = wingspan_cm/100) %>% 
  select(capture_event,
         total_body_length_m, 
         wingspan_m) %>% 
  slice(1:5)

morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  mutate(total_body_length_m = total_body_length_cm/100,
         wingspan_m = wingspan_cm/100) %>% 
  select(capture_event,
         total_body_length_m, 
         wingspan_m) 

morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  mutate(total_body_length_m = total_body_length_cm/100,
         wingspan_m = wingspan_cm/100) %>% 
  select(capture_event,
         total_body_length_m, 
         wingspan_m) %>%
  slice(c(1, 10, 100))

obj1 <- morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  mutate(total_body_length_m = total_body_length_cm/100,
         wingspan_m = wingspan_cm/100) %>% 
  select(capture_event,
         total_body_length_m, 
         wingspan_m)

obj1[c(1, 10, 100), ]

glimpse(obj1)

# Tibbles ####

obj1 <- as_tibble(obj1)
class(obj1)

# Join tables ####

morph_join <- morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  mutate(total_body_length_m = total_body_length_cm/100,
         wingspan_m = wingspan_cm/100) %>% 
  select(capture_event,
         total_body_length_m, 
         wingspan_m) %>% 
  left_join(captures, by = "capture_event") %>% 
  left_join(dragons, by = "dragon_id") %>% 
  as_tibble()

# Calculate summary statistics by group ####

# Count number of dragons by species 
morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  mutate(total_body_length_m = total_body_length_cm/100,
         wingspan_m = wingspan_cm/100) %>% 
  select(capture_event,
         total_body_length_m, 
         wingspan_m) %>% 
  left_join(captures, by = "capture_event") %>% 
  left_join(dragons, by = "dragon_id") %>% 
  as_tibble() %>% 
  group_by(species) %>% 
  tally() %>% 
  arrange(desc(n))

# Mean wingspan by sex
output <- morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  mutate(total_body_length_m = total_body_length_cm/100,
         wingspan_m = wingspan_cm/100) %>% 
  select(capture_event,
         total_body_length_m, 
         wingspan_m) %>% 
  left_join(captures, by = "capture_event") %>% 
  left_join(dragons, by = "dragon_id") %>% 
#  as_tibble() %>% 
  group_by(sex) %>% 
  summarize(min_wingspan_m = min(wingspan_m),
            mean_wingspan_m = mean(wingspan_m),
            max_wingspan_m = max(wingspan_m),
            sd_wingspan_m = sd(wingspan_m)) 

#up to this point is the same content as class_10_24_2024.R file
#What follows is from class_10_29_2024
# Change the order of columns ####

morph_join %>% 
  relocate(dragon_id, .before = capture_event) %>% 
  relocate(date:species, .after = capture_event)

morph_join %>% 
  select(dragon_id, capture_event, date, site, 
         age_at_capture, sex, species,
         total_body_length_m, wingspan_m)

# Rename columns ####

morph_join %>% 
  relocate(dragon_id, .before = capture_event) %>% 
  relocate(date:species, .after = capture_event) %>% 
  rename(dragon = dragon_id)

morph_join %>% 
  select(dragon = dragon_id, capture_event, date, site, 
         age_at_capture, sex, species,
         total_body_length_m, wingspan_m)

# Extract columns as vectors ####

class(morphometrics[, 1]) # returns a vector
class(morphometrics[1]) # returns a data frame
class(morphometrics$measurement_id) # returns a vector

class(morphometrics[, c(1:2)]) # returns a data frame

morphometrics %>% 
  select(measurement_id) %>% 
  class()

morphometrics %>% 
  pull(measurement_id) %>% 
  class()

keepers <- morphometrics %>% 
  filter(total_body_length_cm > 1000) %>% 
  left_join(captures) %>% 
  left_join(dragons) %>% 
  pull(dragon_id)

for (i in 1:length(keepers)) {
  #...
}

# Switch between long and wide format ####

aza <- read.csv("raw_data/aza_example.csv")

target <- data.frame(common_name = NA,
                     latin_name = NA, 
                     class = NA,
                     mle = NA,
                     lwr_ci = NA, 
                     upr_ci = NA,
                     sex = NA)

?pivot_longer

mle <- pivot_longer(aza,
             cols = mle_m:mle_f,
             values_to = "mle",
             names_to = "sex") %>% 
  select(common_name, latin_name, class, sex, mle)
  
lwr <- pivot_longer(aza, cols = lwr_ci_m:lwr_ci_f,
               values_to = "lwr_ci",
               names_to = "lwr_sex") %>% 
  select(common_name, latin_name, class, lwr_sex, lwr_ci)

upr <- pivot_longer(aza, cols = upr_ci_m:upr_ci_f,
                    values_to = "upr_ci",
                    names_to = "upr_sex") %>% 
  select(common_name, latin_name, class, upr_sex, upr_ci)

# Conditional value assignment ####
aza <- read.csv("Raw_data/aza_example.csv")
library(tidyverse)
glimpse(aza)
aza %>% 
  mutate(upr_ci_f = case_when(
    upr_ci_f %in% c("-", "") ~ NA_character_,
    TRUE ~ upr_ci_f
  )) %>% 
  mutate(upr_ci_f = as.numeric(upr_ci_f))

aza %>% 
  mutate(upr_ci_f = as.numeric(case_when(
    upr_ci_f %in% c("-", "") ~ NA_character_,
    TRUE ~ upr_ci_f)
  ))

aza %>% 
  mutate(upr_ci_f = case_when(
    upr_ci_f %in% c("-", "") ~ NA_real_,
    TRUE ~ as.numeric(upr_ci_f)
  ))

upr <- aza %>% 
  mutate(upr_ci_f = case_when(
    upr_ci_f %in% c("-", "") ~ NA_character_,
    TRUE ~ upr_ci_f
  ),
  upr_ci_f = as.numeric(upr_ci_f)) %>% 
  pivot_longer(cols = upr_ci_m:upr_ci_f,
               values_to = "upr_ci",
               names_to = "upr_sex") %>% 
  select(common_name, latin_name, class, upr_sex, upr_ci)

mle <- mle %>% 
  mutate(sex = case_when(
    sex == "mle_m" ~ "M",
    sex == "mle_f" ~ "F"
  ))

lwr <- lwr %>% 
  mutate(sex = case_when(
    lwr_sex == "lwr_ci_m" ~ "M",
    lwr_sex == "lwr_ci_f" ~ "F"
  )) %>% 
  select(-lwr_sex)

upr <- upr %>% 
  mutate(sex = case_when(
    upr_sex == "upr_ci_m" ~ "M",
    upr_sex == "upr_ci_f" ~ "F"
  )) %>% 
  select(-upr_sex)

new_aza <- mle %>% 
  left_join(lwr) %>% 
  left_join(upr)








# More case value assignment
# case_when(
#   conditional1 ~ value1,
#   conditional2 ~ value2,
#   conditional3 ~ value3,
#   TRUE ~ value_for_everything_else_not_covered_by_ conditions
# )

# Create data for patient ages
library(wakefield)
wakefield::age(10)#returns 10 different ages
patients <- dob(5000,
                random = TRUE,
                start = Sys.Date() - 365 * 120,
                k = 365 * 120,
                by = "1 days")
patients <-  patients %>% 
  as_tibble()
names(patients) <- c("DOB")
head(patients)
patients <-  patients %>% 
  mutate(AgeYears = floor((Sys.Date() - DOB)/365.25)) %>% 
  mutate(AgeYears = as.integer(AgeYears))

# convert age into age groups
patients01 <- patients %>% 
  arrange(AgeYears) %>% 
  mutate(AgeGroup = case_when(between(AgeYears, 0, 10) ~ "0-10 Years",
         between(AgeYears, 11, 20) ~ "11 - 20 Years",
         between(AgeYears, 21, 30) ~ "21 - 30 Years",
         between(AgeYears, 31, 40) ~ "31 - 40 Years",
         between(AgeYears, 41, 50) ~ "41 - 50 Years",
         between(AgeYears, 51, 70) ~ "51 - 70 Years",
         TRUE ~ "unknown"))
print(n = 500, patients01 )
multiple_rows <-  slice(patients, c(1625, 1786))
print(multiple_rows)
#aggregate the data
data02 <-  patients01 %>% 
  group_by(AgeGroup) %>% 
  tally() %>% 
  mutate(pct = n/sum(n))
data02
## plot histogram
patients01 %>% 
  ggplot(mapping = aes(x = AgeYears))+
  geom_histogram(fill = "blue", alpha = 0.2)+
  theme_dark()
#plot density
patients01 %>% 
  ggplot(mapping = aes(x= AgeYears))+
  geom_density()+
  theme_classic()
#bar chart
data02 %>% 
  ggplot(mapping = aes(x= AgeGroup, y = n))+
  geom_bar(stat = "identity")+
  theme_light()

# barchart showing percentages
data02 %>% 
  ggplot(mapping = aes(x= AgeGroup, y = pct))+
  geom_bar(stat = "identity")+
  theme_bw()
# using scales package to make percentages
data02 %>% 
  ggplot(mapping = aes(x= AgeGroup, y = pct))+
  geom_bar(stat = "identity")+
  theme_bw()+
  scale_y_continuous(labels = percent)


#Rifformas youtube
library(tidyverse)
read_tsv("Raw_data/baxter.subsample.shared",
         col_types = cols(Group = col_character(),
                          .default = col_double())) %>% 
rename_all(tolower) %>% 
  select(group, starts_with("otu")) %>% 
  pivot_longer(-group, names_to = "otu", values_to = "count")


# Creating a sample dataset
sample_data <- tibble(
  ID = c(1:10),
  Age = c(25, 30, 35, 28, 22, 40, 33, 26, 38, 29),
  Gender = c("Male", "Female", "Male", "Female", "Male", "Male", "Female", "Male", 
             "Female", "Male"),
  Score1 = c(85, 70, 60, 75, 90, 80, 92, 78, 65, 88),
  Score2 = c(75, 82, 88, 95, 70, 68, 80, 85, 77, 93),
  Status = c("Active", "Inactive", "Active", "Active", "Inactive", "Active", "Inactive", 
             "Active", "Inactive", "Active"),
  Income = c(50000, 60000, 75000, 55000, 80000, 90000, 72000, 65000, 82000, 70000)
)
# Display the sample dataset
print("Original dataset:")
print(sample_data)