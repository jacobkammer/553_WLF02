
library(tidyverse)
StarsData <- read.csv("Raw_data/class6_stars.csv")
glimpse(StarsData)

#using group_by and summarise together
StarsData %>% 
  group_by(Spectral.Class) %>% 
  summarise(mean_magnitude = mean(Absolute.magnitude.Mv., na.rm = TRUE))
StarsData %>% 
  group_by(Star.type) %>% 
  summarise(mean_radius = mean(Radius.R.Ro.), mean_mag = mean(Absolute.magnitude.Mv.))


StarsData %>% 
  filter(Spectral.Class == "M") %>% 
  tally()
names(StarsData)
StarsData %>% 
  group_by(Spectral.Class)
class(StarsData)
as.tibble(StarsData)
StarsData <-  as_tibble(StarsData)
class(StarsData)

StarsData %>% 
  select(Temperature..K., Absolute.magnitude.Mv.,Star.type, Star.color,Radius.R.Ro.) %>% 
  mutate(Temp_C = Temperature..K.-273) %>% 
  select(-Temperature..K.) %>% 
  relocate(Temp_C, .before = Absolute.magnitude.Mv.) %>% 
  rename(Ab_Magnitude = Absolute.magnitude.Mv., Type_star = Star.type, Color = Star.color, Radius = Radius.R.Ro.) %>% 
  summarise(Max = max(Ab_Magnitude),
            Min = min(Ab_Magnitude))
 
hist(StarsData$Absolute.magnitude.Mv.)

data01 %>% 
  select(full_name, e, diameter, data_arc, om) %>% 
  summarize(count = n_distinct(full_name))
  

New_stars <- StarsData %>% 
  rename(Magnitude = Absolute.magnitude.Mv., 
         Type = Star.type,
         Color = Star.color,
         Spectral_class = Spectral.Class,
         Luminosity = Luminosity.L.Lo.,
         Radius = Radius.R.Ro.,
         Temperature = Temperature..K. ) %>% 
  filter(Color == "Red" | Color == "Yellow") 
  
         
exoplanets <-  read.csv("Raw_data/exoplanets.csv")
library(tidyverse)
glimpse(exoplanets)
vectjor <- exoplanets %>% 
  pull(Radius..RJ.)
class(vectjor)
str(vectjor)
length(vectjor)

exoplanets02 <-  exoplanets %>%
  select(Name, Mass..MJ., Radius..RJ., Period..days., Semi.major.axis..AU., Temp...K., Disc..Year,
         Distance..ly.) %>% 
  mutate(Radius..RJ. = as.numeric(Radius..RJ.),
         Period..days.= as.numeric(Period..days.),
         Semi.major.axis..AU.= as.numeric(Semi.major.axis..AU.),
         Temp...K. = as.numeric(Temp...K.),
         Distance..ly. = as.numeric(Distance..ly.),
         Mass..MJ. = as.numeric(Mass..MJ.)) %>% 
  rename(radius = Radius..RJ.,
         period = Period..days.,
         MajorAxis = Semi.major.axis..AU.,
         temperature= Temp...K.,
         distance = Distance..ly.,
         mass = Mass..MJ.,
         year_discovery = Disc..Year)


glimpse(exoplanets02)
sum(is.na(exoplanets02$radius))
sum(is.na(exoplanets02$temperature))
sum(is.na(exoplanets02$MajorAxis))
sum(is.na(exoplanets02))

# Definition. An expression of a body's mass in units of the product of the 
#gravitational constant (G) and the #mass of the sun (M). GM is also known as the heliocentric gravitational #constant. Units are typically km3/s2 or #au3/d2.

moons <-  read.csv("Raw_data/satellites.csv")
glimpse(moons)

moons$albedo <-  as.numeric(moons$albedo)
sum(is.na(moons$albedo))
sapply(moons, class)
  
moons %>% 
  mutate(gm = as.numeric(gm),
         radius = as.numeric(radius),
         density= as.numeric(density),
         magnitude = as.numeric(magnitude)) %>% 
  glimpse()







planets <-  read.csv("Raw_data/planets.csv")
names(planets)
glimpse(planets)
class(planets$surface_pressure)
planets %>% 
  group_by(has_global_magnetic_field) %>% 
  summarise(dayL = mean(length_of_day))
planets %>% 
  group_by(has_ring_system) %>% 
  summarise(Vel = mean(orbital_velocity), mean(aphelion))
planets %>% 
  select(orbital_velocity, orbital_period, orbital_inclination, planet) %>% 
  arrange(desc(orbital_period))
  
planets %>% 
  select(mass, mean_temperature, diameter, orbital_period, orbital_velocity) %>% 
  mutate(temp_kelvin = 273 + mean_temperature) %>% 
  relocate(temp_kelvin, .before = "diameter") %>% 
  rename(temp_Celsius = mean_temperature) %>% 
  filter(orbital_velocity < 13)



DM <-  read.csv("Raw_data/diabetes.csv")
glimpse(DM)
DM %>% 
  group_by(Pregnancies) %>% 
  summarise(mean_insulin = mean(Insulin))
DM %>% 
  group_by(Pregnancies) %>% 
  summarise(SBP = mean(BloodPressure), BMI_mean = mean(BMI))
