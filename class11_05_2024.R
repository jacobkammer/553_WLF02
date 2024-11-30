library(tidyverse)
wq <-  readxl::read_xlsx("Raw_data/2024-11-02_case-study-WLF553.xlsx", col_types = "text")
glimpse(wq)
class(wq$BOD)
unique(wq$BOD)
?readxl::read_xlsx#col_types argument
as.numeric("-")
str(wq)
as.numeric(wq$Sample_ID)
unique(wq$Sample_ID)
#need to figure out if all uniques values
length(unique(wq$Sample_ID)) == length(wq$Sample_ID)
wq <-  wq %>% 
  mutate(Sample_ID = as.numeric(Sample_ID))

#column 2
wq$water_source_name
any(is.na(wq$water_source_name))
unique(wq$water_source_name)

wq %>% 
  filter(water_source_name == "\r\nОткрытая вода\r\n")
wq %>% 
  mutate(water_source_name = case_when(
    grepl("\r\n", water_source_name) ~ gsub("\r\n", "", water_source_name),
    TRUE ~ water_source_name
  )) %>% 
  pull(water_source_name) %>% 
  unique() %>% 
  sort() %>% 
  length()

wq %>% 
  mutate(water_source_name = case_when(
    grepl("\r\n", water_source_name) ~ gsub("\r\n", "", water_source_name),
    TRUE ~ water_source_name
  ))
 
#column 3 

unique(wq$smell)
#case 1: normal number
#case 2: NA
#case 3: "0балл"
#case 4: "o instead of Orange"
#Case 5: "бал"
#Case 6:  "бал"
#Case 7: "1баал"
#Case 8: " балл"

#column 4

unique(wq$colouring)


#column 6

unique(wq$turbidity)
#only keep 2 decimal places (truncate)
#remove ",,"
#make numeric

wq %>% 
  mutate(turbidity = case_when(
    grepl(",,", turbidity) ~ gsub(",,",  ".", turbidity),
    TRUE ~ turbidity
 )) %>% 
  mutate(turbidity = round(as.numeric(turbidity), 2))
    
    
#column   
unique(wq$fluorine)
#commas instead of periods
#commas or periods at the end
#too many digits
#three possible ways to encode non-detect: 0 , -, "отс", 0TC, *OK



# Create a sample dataset
data <- data.frame(
  text = c("apple123", "BANANA!", "cherry_pie", "Date.fruit", "elderberry"),
  value = c(10, 25, 15, 30, 20)
)

# Load dplyr for case_when
library(dplyr)
#This example demonstrates:
  
#  case_when(): Creates conditional categories based on patterns

#Works similar to nested if-else statements but more readable
#Returns the value after ~ when the condition is TRUE


#grepl(): Searches for pattern matches

#^[A-Z] checks if text starts with uppercase
#[0-9] checks for any numbers
#[._] checks for periods or underscores
#Returns TRUE/FALSE


#gsub(): Substitutes patterns with replacement text

#[0-9._!] matches any digit, period, underscore, or exclamation mark
#Replaces matches with empty string ""



# Apply all three functions in an example
data %>%
  mutate(
    # Use case_when to categorize based on patterns
    category = case_when(
      grepl("^[A-Z]", text) ~ "Uppercase Start",
      grepl("[0-9]", text) ~ "Contains Numbers",
      grepl("[._]", text) ~ "Contains Special Chars",
      TRUE ~ "Other"
    ),
    # Use gsub to clean the text
    clean_text = gsub("[0-9._!]", "", text)
  )