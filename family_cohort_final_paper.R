#====================================
# Final Research Project Analysis 
#====================================

#-----------------------------------
# Setting up workspace
#-----------------------------------

# This snippet of code is a little loop that makes my code work on your computer
root <- getwd()
while(basename(root) != "family-cohort") {
  root <- dirname(root)
}

# This line runs the script in your data.R file so that each person can have
# their data in a different place because everyone's file structure will be 
# a little different
source(file.path(root, "data.R"))

# Loading the packages we want
library(tidyverse)
library(haven) # for reading stata data
library(lfe) # for fixed effect regression
library(stargazer) # for pretty regression tables
library(ggplot2)

#-----------------------------------
# Loading In the Data
#-----------------------------------

#df <- read_dta(file.path(ddir, "Joneses", "ipums.dta.gz"))

#save(small_df, file = file.path(ddir, "Joneses", "small_df.Rda"))

load(file.path(ddir, "df.Rda"))

head(df)
#-----------------------------------
# Cleaning Our Data
#-----------------------------------

#Here is where we will clean our data

# Sunday, April 28th
NYmetro_df <- df %>%
  filter(PWMETRO == 5600) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9)

NYinter_df <- NYmetro_df %>%
  filter(RACE != RACE_SP)

summary(NYmetro_df)
  
LAmetro_df <- df %>%
  filter(PWMETRO == 4480) 

#-----------------------------------
# Figures
#-----------------------------------

#Here is where we will make our figures 

# Sunday, May 5th

# We decide to work on interracial marriages in Los Angeles-Long Beach this time

# part 1: data cleaning

LAmetro_df <- df %>%
  filter(PWMETRO == 4480) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9)

# clean the dataset for interracial marriages in LA in the 1980s

LAmetro_1980_df <- df %>%
  filter(PWMETRO == 4480) %>%
  filter(YEAR == 1980) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

LAinter_1980_df <- LAmetro_1980_df %>%
  filter(RACE != RACE_SP)

# clean the dataset for interracial marriages in LA in the 1990s

LAmetro_1990_df <- df %>%
  filter(PWMETRO == 4480) %>%
  filter(YEAR == 1990) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

LAinter_1990_df <- LAmetro_1990_df %>%
  filter(RACE != RACE_SP)

# clean the dataset for interracial marriages in LA in the 2000s

LAmetro_2000_df <- df %>%
  filter(PWMETRO == 4480) %>%
  filter(YEAR == 2000) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

LAinter_2000_df <- LAmetro_2000_df %>%
  filter(RACE != RACE_SP)

# part 2: creating a new dataset for the figure

LAtable <- LAmetro_df %>%
  group_by(YEAR) %>%
  mutate(interracial = as.numeric(RACE != RACE_SP)) %>%
  summarise(prop_interracial = mean(interracial))

# part 3: creating a figure

ggplot(data = LAtable, aes(x = YEAR, y = prop_interracial)) +
  geom_point() +
  geom_line()

# part 4: incorporate new cities and create a similar figure

Citiesmetro_df <- df %>%
  filter(PWMETRO == 4480 | PWMETRO == 5600) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

Citiestable <- Citiesmetro_df %>%
  group_by(YEAR, PWMETRO) %>%
  mutate(interracial = as.numeric(RACE != RACE_SP)) %>%
  summarise(prop_interracial = mean(interracial))

# convert PWMETRO from a numeric variable into a factor variable
Citiestable$PWMETRO = as.factor(Citiestable$PWMETRO)

ggplot(data = Citiestable, aes(x = YEAR, y = prop_interracial, color = PWMETRO)) +
  geom_point() +
  geom_line()

# Sunday, May 19th

SmallCitiesMetro_df <- df %>%
  filter(PWMETRO == 5080 | PWMETRO == 7240 | PWMETRO == 1520) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

SmallCitiesNametable <- tibble(
  PWMETRO = c(5080, 7240, 1520), 
  Cities = c("Milwaukee", "San Antonio", "Charlotte")
)

SmallCitiesMetro_df <- SmallCitiesMetro_df %>%
  left_join(SmallCitiesNametable)

SmallCitiestable <- SmallCitiesMetro_df %>%
  group_by(YEAR, Cities) %>%
  mutate(interracial = as.numeric(RACE != RACE_SP)) %>%
  summarise(prop_interracial = mean(interracial))

SmallCitiestable$Cities = as.factor(SmallCitiestable$Cities)

ggplot(data = SmallCitiestable, aes(x = YEAR, y = prop_interracial, color = Cities)) +
  geom_point() +
  geom_line() +
  labs(title = "Interracial Marriage Rates in Small Cities during 1980 to 2000")

BigCitiesMetro_df <- df %>%
  filter(PWMETRO == 1600 | PWMETRO == 3360 | PWMETRO == 520) %>%
  filter(MARST == 1) %>%
  filter(RACE != 7) %>%
  filter(RACE != 8) %>%
  filter(RACE != 9) %>%
  filter(RACE_SP != 7) %>%
  filter(RACE_SP != 8) %>%
  filter(RACE_SP != 9) 

BigCitiesNametable <- tibble(
  PWMETRO = c(1600, 3360, 520), 
  Cities = c("Chicago", "Houston", "Atlanta")
)

BigCitiesMetro_df <- BigCitiesMetro_df %>%
  left_join(BigCitiesNametable)

BigCitiestable <- BigCitiesMetro_df %>%
  group_by(YEAR, Cities) %>%
  mutate(interracial = as.numeric(RACE != RACE_SP)) %>%
  summarise(prop_interracial = mean(interracial))

BigCitiestable$Cities = as.factor(BigCitiestable$Cities)

ggplot(data = BigCitiestable, aes(x = YEAR, y = prop_interracial, color = Cities)) +
  geom_point() +
  geom_line() +
  labs(title = "Interracial Marriage Rates in Big Cities during 1980 to 2000")


# May 26th

df_regression <- df %>% 
  filter(PWMETRO == 1520 | PWMETRO == 7240 | PWMETRO == 5080 | PWMETRO == 4480 | PWMETRO == 5600 | PWMETRO == 1600) %>%
  mutate(race_non_white = if_else(RACE != 1, 1, 0, missing = NULL)) 

prop_non_white = df_regression %>% 
  group_by(PWMETRO, YEAR) %>%
  summarise(diversity_prop = mean(race_non_white))

df_regression <- left_join(df_regression, prop_non_white, by = c("PWMETRO", "YEAR"))

#adding dummy variable for married or not 
df_regression <- df_regression %>%
  filter(MARST == 1) %>%
  mutate(interracial_marriage = if_else(RACE != RACE_SP, 1, 0, missing = NULL))

reg1 <-
  felm(interracial_marriage~diversity_prop, data = df_regression)
stargazer(reg1)
