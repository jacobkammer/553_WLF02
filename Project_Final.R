library(tidyverse)
library(DBI)
library(RSQLite)
library(viridis)
costR <-  read.csv("Raw_data/costR.csv", stringsAsFactors = FALSE)
glimpse(costR)
colnames(costR)



#What are the ranges of the numeric variables in the data set? ####
##fitness ####
costR %>% 
  select(fitness) %>% 
  ggplot(mapping = aes(x = fitness))+
  geom_histogram()+
  theme()
##SE ####
ggplot(data = costR, mapping = aes(x = SE))+
  geom_histogram()+
  theme_classic()

# evaluate distribution of selection_coefficient values
costR %>% 
  select(Selection_coefficient) %>% 
  ggplot(mapping = aes(Selection_coefficient))+
  geom_histogram(bins = 15)+
  scale_fill_viridis_d(option = "magma")

##MIC ####
costR %>% 
  ggplot(mapping = aes(x = MIC))+
  geom_histogram()+
  theme()

#Examine relationship between the numeric variables of 1)fitness, 2)SE and
#3)Selection_coefficient 

#Below are 4 different ways of examining relationships

#1. Scatter Plot with Color or Size Encoding

#You can use a scatter plot to visualize the relationship between two variables
#and use color or size to represent the third variable.
ggplot(data = costR, mapping = aes(x = fitness, y = Selection_coefficient, color = SE))+
  geom_point()+labs(title = "Scatter Plot with color encoding",
                    x="Fitness",
                    y = "Selection Coefficient",
                    color = "SE")

#or can use size encoding instead of color for SE
ggplot(data = costR, mapping = aes(x = fitness, y = Selection_coefficient, size = SE))+
  geom_point()+
  labs(title = "Scatter plot with size encoding",
       x = "Fitness",
       y = "Selection Coefficient",
       size = "SE")

# 3. Pair Plot (Scatterplot Matrix)
#
# A pair plot (scatterplot matrix) allows you to visualize pairwise
# relationships between multiple variables.
library(GGally)
#prepare the data
numericonly <- costR %>% 
  select(fitness, Selection_coefficient, SE)
#Use the data
ggpairs(numericonly, title = 'Pair Plot')

#lattice package
library(lattice)

splom(numericonly,
      main = "Scatterplot Matrix")




#fitness values greater than one, indicating increase in fitness
costR %>% 
  select(fitness) %>% 
  filter(fitness >= 1.0) %>% 
  tally()
#fitness values less than one, indicating decrease in fitness
costR %>% 
  select(fitness) %>% 
  filter(fitness< 1.0) %>% 
  tally()
#summary: More mutations showed a decrease in fitness, instead of increase of fitness









costR %>% 
  mutate(GroupFIT = case_when(fitness > 1.35 ~ "Most Fit of All",
                              between(fitness, 1.25, 1.35)~ "Highly Fit",
                              between(fitness, 1.0, 1.25)~ "Fit",
                              between(fitness, .75, 1.0) ~ "Less Fit",
                              between(fitness, .35, .75)~ "Much Less Fit",
                              fitness<.35 ~ "Least Fit of All",
                              TRUE ~ "Unknown"))

#What is the mean fitness (other measures) each type of bacteria
costR %>% 
  select(bacteria, fitness) %>% 
  group_by(bacteria) %>% 
  summarise(FITmean = mean(fitness),
            FITIQR= IQR(fitness),
            FITmin = min(fitness),
            FITmax = max(fitness),
            FITsd = sd(fitness))



#The following two scripts show the distribution if fitness values using two different ways 
#to arrange the bins
#distribution of fitness values setting number of bins to 20
costR %>% 
  select(fitness) %>% 
  ggplot(mapping = aes(fitness))+
  geom_histogram(fill = "aquamarine", bins = 20)+
  theme_light()

# evaluate distribution of fitness, setting bin width instead of setting the  
# number of bins
costR %>% 
  select(fitness) %>% 
  ggplot(mapping = aes(fitness))+
  geom_histogram(fill = "deepskyblue", binwidth = 0.05)+
  theme_get()


GramFIT <-  as_tibble(GramFIT)
?gt()

costR %>% 
  select(fitness, bacteria) %>% 
  mutate(GroupFIT = case_when(fitness > 1.35 ~ "Most Fit of All",
                              between(fitness, 1.25, 1.35)~ "Highly Fit",
                              between(fitness, 1.0, 1.25)~ "Fit",
                              between(fitness, .75, 1.0) ~ "Less Fit",
                              between(fitness, .35, .75)~ "Much Less Fit",
                              fitness<.35 ~ "Least Fit of All",
                              TRUE ~ "Unknown")) %>% 
  select(GroupFIT, bacteria) %>% 
  group_by(GroupFIT) %>% 
  tally() %>% 
  mutate(pct = round(100 *(n/sum(n)), digits =3)) %>%
  arrange(factor(GroupFIT, levels = c("Most Fit of All", "Highly Fit",
                                      "Fit", "Less Fit",
                                      "Much Less Fit"),
                 "Least Fit of All")) %>% 
  gt() %>% 
  tab_header(title = "Fitness Groups") %>% 
  cols_label(GroupFIT = "Group",
             n = "Number of organisms",
             pct = "Percentage"
             )



#Gram-positive bacteria had a significantly
# greater fitness costs associated with resistance mutations
# (mean fitness = 0.822) when compared with Gram-negative
# bacteria (mean fitness = 0.973). Caution must be used in interpreting
# this result because there were a greater number of Gram-negative
# than Gram-positive bacteria in the data set.




#boxplot of fitness between gram + and gram - organisms does not show
#much of a difference between the two groups.







costR %>%
  select(gene_mutation, fitness) %>%
  group_by(gene_mutation) %>% 
  tally()
  summarise(MeanValueMut = mean(fitness))


  BacterialessFIT <- costR %>% 
    select(bacteria, gene_mutation, fitness) %>% 
    filter(bacteria == "S. aureus" & fitness < 1.0) %>% 
    distinct(gene_mutation)
  colnames(BacterialessFIT) <- "Gene Mutation"





# select the 3 most frequent gene mutations
costR %>%
  select(gene_mutation, fitness) %>% 
  filter(gene_mutation %in% c("23S A2074C", "rpoB H489Y","23S A2075G")) %>% 
  group_by(gene_mutation) %>% 
  summarise(MeanValueMut = mean(fitness))














# Analysis ####
#Question: Are there costs to Resistance?
costR %>% 
  select(fitness, SE) %>% 
  ggplot(mapping = aes(x= fitness, y= SE))+
  geom_point()








costR %>% 
  select(bacteria, fitness) %>% 
  group_by(bacteria) %>% 
  summarise(mean_cost = mean(fitness),
            stddev = sd(fitness, na.rm = TRUE),
            meansd_l = mean_cost - stddev,
            meansd_u = mean_cost + stddev) %>% 
  ggplot(mapping = aes(x = mean_cost, y =bacteria))+
  geom_point()+
  geom_vline(xintercept = 1)+
  geom_errorbar(mapping = aes(xmin = meansd_l, xmax = meansd_u))+
  labs(x = "Mean Fitness Cost", y = "Bacteria")+
  ggtitle("Fitness Cost per Bacterial Species")


#The average fitness of each species
# and antibiotic comparison yielded no clear patterns in
# costs of resistance.
summary_data <- costR %>%
  drop_na() %>% 
  group_by(bacteria, antibioticType, fitness) %>%
  summarize(MeanFITNESS = mean(fitness))
summary_data



## RESISTANCE database ####
conn01 <-  dbConnect(drv = RSQLite::SQLite(),
                     "databases_WLF/Resistance")

dbWriteTable(conn01, "antibiotics", atbdf, overwrite = TRUE)
dbWriteTable(conn01, "Bacteria", distinctBac, overwrite = TRUE)
dbWriteTable(conn01, "Fitness", FIT, overwrite = TRUE)  
dbWriteTable(conn01, "Reference", REF, overwrite = TRUE)
#list all tables
DBI::dbListTables(conn01)
#list fields within each table
dbListFields(conn01, "Bacteria;")
dbListFields(conn01, "antibiotics")


dbGetQuery(conn01, "SELECT * FROM antibiotics;")
dbGetQuery(conn01, "SELECT * FROM Bacteria;")
dbGetQuery(conn01, "SELECT * FROM fitness;")




## ATB DATABASE ####
connatb <- dbConnect(drv = RSQLite::SQLite(),
                     "databases_WLF/ATB.db")

dbWriteTable(connatb, "antibiotics", atbdf, overwrite = TRUE)
dbWriteTable(connatb, "Bacteria", distinctBac, overwrite = TRUE)
dbWriteTable(connatb, "Fitness", FIT, overwrite = TRUE)  
dbWriteTable(connatb, "Reference", REF, overwrite = TRUE)

dbRemoveTable(connatb, "Laboratory_standard")
dbRemoveTable(connatb, "Antibiotic")
dbRemoveTable(connatb, "microbial_fitness")

DBI::dbListTables(connatb)#returns a list of all the table in the dragons database


dbListFields(atb_db, "antibiotics")
dbReadTable(atb_db, "antibiotics")



antibiotic <-  dbGetQuery(conn01, "SELECT * FROM antibiotics;")
head(antibiotic)

fitnessLevel <-  dbGetQuery(conn01, "SELECT * FROM Fitness;")
head(fitnessLevel)

bacteria <-  dbGetQuery(conn01, "SELECT * FROM Bacteria;")
head(bacteria)

references <-  dbGetQuery(conn01, "SELECT * FROM Reference;")
head(references)





## Analysis ####

#list the gram positive and gram negative organisms
costR %>% 
  group_by(gram_stain) %>% 
  tally()

costR %>% 
  group_by(antibioticClass) %>% 
  tally() %>% 
  arrange(desc(n))



         
