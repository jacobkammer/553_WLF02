library(tidyverse)
library(ggridges)

# chapter 3 ####
library(nycflights13)
nycflights13::flights
head(flights)
class(flights)
glimpse(flights)

flights %>% 
  filter(dest == "IAH") %>% 
  group_by(year, month, day) %>% 
  summarise(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  head()

flights %>% 
  filter(dep_delay > 800 & dep_delay < 1200)

flights %>% 
  filter(month== 1 & day == 1)
flights %>% 
  filter(month==1 | month==2)
flights %>% 
  filter(month %in% c(1,2))
flights %>% 
  filter(day %in% c(3,5,8)) %>% 
  tail()
flights %>% 
  filter(month %in% c(3,4) & day %in% c(4,6)) %>% 
  tail()

flights %>% 
  arrange(year, month, day, dep_time)
flights %>% 
  arrange(desc(dep_delay))
flights %>% 
  filter(month %in% c(3,5,8)) %>% 
  arrange(desc(dep_delay)) %>% 
  tail()

flights %>% 
  distinct()

flights %>% 
  distinct(origin, dest)

flights %>% 
  distinct(origin, dest, .keep_all = TRUE)

flights %>% 
  count(origin, dest, sort = TRUE)
satdata <- read.csv("Raw_data/planets.csv")
head(satdata)
class(satdata)
planetdata <-  as_tibble(satdata)
head(planetdata)
planetdata %>% 
  filter(density > 1500)
names(planetdata)

planetdata %>%
  select(planet, escape_velocity, perihelion) %>% 
  filter(escape_velocity > 60 | escape_velocity< 5)
# chapter 9 ####
mpg
glimpse(mpg)
ggplot(mpg, mapping = aes(x=displ, y = hwy, color = cyl))+
  geom_point()


ggplot(data= mpg, mapping = aes(x= displ, y = hwy, shape= cyl))+
  geom_point()


ggplot(data = mpg, mapping = aes(x = displ, y = hwy, size = cyl))+
         geom_point()
glimpse(mpg)       
mpg$cyl


ggplot(data = mpg, mapping = aes(x=displ, y = hwy))+
  geom_point(color = "blue")


ggplot(data = mpg, mapping = aes(x=hwy, y = displ))+
  geom_point(shape=24,
             fill = "pink",
             color="black")
?geom_point
ggplot(data = mpg, mapping = aes(x=hwy, y = displ)) +
  geom_point(shape=24,
             fill = "pink",
             color = "black",
             stroke = 2)  # Makes the border thicker
# Some key points about stroke:
# The default value is 0.5
# Larger values make the border thicker
# Only works with shapes 21-25 (the fillable shapes)
# Measured in millimeters


ggplot(data = mpg, mapping = aes(x= displ, y = hwy))+
  geom_point()
#Every geom function in ggplot2 takes a mapping argument
ggplot(data = mpg, mapping = aes(x= displ, y = hwy))+
  geom_smooth(se = TRUE)

ggplot(data = mpg, mapping = aes(x= displ, y = hwy))+
  geom_smooth(se = FALSE)
 #SE in geom_smooth displays confidence intervals             



ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_smooth(aes(color = drv))




ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(size = 5, shape = 5, fill= "green")

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, shape = drv))+
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, linetype = drv))+
  geom_smooth()
?geom_smooth

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv))+
  geom_point()+
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = hwy))+
  geom_histogram(binwidth = 2)

ggplot(data = mpg, mapping = aes(x = hwy))+
  geom_density()

ggplot(data = mpg, mapping = aes(x = hwy))+
  geom_boxplot()

#plot  below use ggridges package
ggplot(data = mpg, mapping = aes(x = hwy, y = drv, fill = drv, color = drv))+
  geom_density_ridges(alpha = 0.5, show.legend = FALSE)
#making ridgeline plots, which can be useful for visualizing the density of a #numerical variable for different levels of a categorical variable.

glimpse(diamonds)
diamonds %>% 
  select(color) %>% 
  distinct()
diamonds %>% 
  select(cut) %>% 
  distinct()
ggplot(data = diamonds, mapping = aes(x = price, y = cut, fill = cut))+
  geom_density_ridges(alpha = 0.5)


ggplot(data = diamonds, mapping = aes(x = cut, y = price))+
  geom_boxplot()

ggplot(data = diamonds, mapping = aes(x = color, y = price))+
  geom_boxplot()
ggplot(data = diamonds, mapping = aes(x = clarity, y = price))+
  geom_boxplot()
ggplot(data = diamonds, mapping = aes(x=z))+
  geom_density()
    
# Chapter 10.3 Variation ####

ggplot(data = diamonds, mapping = aes(x = carat))+
  geom_histogram(binwidth = 0.5)

diamonds %>% 
  filter(carat<3) %>% 
  ggplot(mapping = aes(x = carat))+
  geom_histogram(binwidth = 0.01)


ggplot(data = diamonds, mapping = aes(x = y))+
  geom_histogram(binwidth = 0.5)


ggplot(data = diamonds, mapping = aes(x = y))+
  geom_histogram(binwidth = 0.5)+
  coord_cartesian(ylim = c(0, 50))


unusual <-  diamonds %>% 
  filter(y<3 | y>20) %>% 
  arrange(y)
unusual
diamonds2 <-  diamonds %>% 
  mutate(y = if_else(y < 3 | y >20, NA, y))

diamonds %>% 
  ggplot(mapping = aes(x = y))+
  geom_histogram()+
  coord_cartesian(y = c(0, 30))
diamonds %>% 
  ggplot(mapping = aes(x = z))+
  geom_histogram()+
  theme_classic()+
  coord_cartesian(y = c(0, 30))

diamonds %>% 
  ggplot(mapping = aes(x = x))+
  geom_histogram()+
  theme_ridges()
glimpse(mpg)
mpg %>% 
  ggplot(mapping = aes(x = cty))+
  geom_histogram()+
  theme_get()+
  coord_cartesian(x = c(0, 25))

# if_else(condition, true, false)
# `if_else()` is useful for creating new columns inside of `mutate()`
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point(na.rm =  TRUE)




library(gt)

# Define the start and end dates for the data range
start_date <- "2010-06-07"
end_date <- "2010-06-14"

# Create a gt table based on preprocessed
# `sp500` table data
sp500 |>
  dplyr::filter(date >= start_date & date <= end_date) |>
  dplyr::select(-adj_close) |>
  gt() |>
  tab_header(
    title = "S&P 500",
    subtitle = glue::glue("{start_date} to {end_date}")
  ) |>
  fmt_currency() |>
  fmt_date(columns = date, date_style = "wd_m_day_year") |>
  fmt_number(columns = volume, suffixing = TRUE)


# First, load the required libraries
library(tibble)  # for creating tibbles
library(gt)      # for creating tables

# Create a sample tibble
my_tibble <- tibble(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(25, 30, 35),
  Score = c(92.5, 88.3, 95.7)
)

# Convert the tibble to a gt table
my_gt_table <- gt(my_tibble)

# Customize the table (optional)
my_gt_table <- my_gt_table %>%
  tab_header(
    title = "Sample Student Information",
    subtitle = "Names, Ages, and Scores"
  ) %>%
  fmt_number(
    columns = c(Score),
    decimals = 1
  )

# Print or render the table
my_gt_table



# Chpater 12 ####

flights %>% 
  filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20)
?abs

flights %>%
  mutate(
  daytime = dep_time > 600 & dep_time < 2000,
approx_ontime = abs(arr_delay) < 20,
.keep = "used"
) %>% 
  filter(daytime & approx_ontime)

x <-  c(1/49 * 49, sqrt(2)^2)
x
x ==c(1,2)
print(x, digits = 16)
near(x, c(1,2))
is.na(x)
is.na(c(1, NA, 3, NA, NA))
flights %>% 
  filter(is.na(dep_time))


flights %>% 
  filter(month == 1, day == 1) %>% 
  arrange(dep_time)
flights %>% 
  filter(month ==1, day ==1) %>% 
  arrange(desc(is.na(dep_time)), dep_time)
?arrange
near
arrange
is.na
flights %>% 
  mutate(desc(is.na(sched_dep_time)), sched_dep_time)
screen <-  is.na(flights$sched_dep_time)
flights$sched_dep_time[screen]
