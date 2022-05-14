library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(haven)
library(knitr)
library(broom)

rm(list = ls())
setwd("C:/Users/aliso/Downloads/Project Insect Carnivore main/Project-Insect-Carnivore-main")

df_Ladybug <- read_excel("data/Ladybug Data.xlsx", sheet = 1)
df_ScanLadyb
  group_by(df_Ladybug$Species, df_Ladybug$plot) %>%
  summarise(number = n()) 

  
##Creating a pivot table that counts how many of a certain type of ladybug 
##appears in each plot
df_PivotTable1 <- df_Ladybug %>%
  group_by(df_ScanLadybug$scientificName, df_ScanLadybug$stateProvince) %>%
  summarise(number = n())
  

Chart1 <- ggplot(df_PivotTable1, aes(x=df_PivotTable1$number, y=df_PivotTable1$`df_Ladybug$plot`)) +
  xlab("Number of Ladybugs") +
  ylab("Plot") +
  ggtitle("Number of Species of Ladybugs per Plot") 
plot(Chart1)

##Creating a pivot table that counts how many of a certain type of ladybug 
##appears in each state
df_PivotTable2 <- df_ScanLadybug %>%ug <- read_excel("data/Scan Ladybug Data.xlsx", sheet = 1) %>%
  #dplyr::mutate(stateProvince = str_replace(stateProvince, pattern = "Il", replacement = "Illinois")) %>%
  dplyr::mutate(stateProvince = str_replace(stateProvince, pattern =  "IL", replacement = "Illinois")) %>%
  dplyr::mutate(stateProvince = str_replace(stateProvince, pattern =  "Il", replacement = "Illinois")) %>%
  dplyr::mutate(stateProvince = str_replace(stateProvince, pattern = "IA", replacement = "Iowa")) %>%
  dplyr::mutate(stateProvince = str_replace(stateProvince, pattern = "Ia", replacement = "Iowa"))

##Creating a pivot table that counts how many of a certain type of ladybug 
##appears in each plot
df_PivotTable1 <- df_Ladybug %>%
  group_by(df_ScanLadybug$scientificName, df_ScanLadybug$stateProvince) %>%
  summarise(number = n())

Chart2 <- ggplot(df_PivotTable2, aes(y=df_PivotTable2$number, x=df_PivotTable2$`df_ScanLadybug$stateProvince`)) +
  xlab("States") +
  ylab("Number of Ladybugs") +
  ggtitle("Number of Species of Ladybugs per State") 
plot(Chart2)

hist(df_PivotTable2$number)

##Creating a pivot table that counts how many of a certain type of ladybug
##appears in the year 2021
df_PivotTable3 <- df_ScanLadybug %>%
  dplyr::select("catalogNumber","scientificName", "year") %>%
  dplyr::filter(df_ScanLadybug$year >= 2021) %>%
  dplyr::group_by(scientificName, year) %>%
  summarise(number = n())

hist(df_PivotTable3$number)

##Putting the years into their respective decades   
df_ScanLadybug$year <- as.integer(df_ScanLadybug$year)
df_ScanLadybug$decade <- df_ScanLadybug$year
df_ScanLadybug$first_three <- substr(df_ScanLadybug$year, 1, 3)
df_ScanLadybug$digit <- paste(0)
df_ScanLadybug$decade <- paste0(df_ScanLadybug$first_three, df_ScanLadybug$digit)
df_ScanLadybug$decade <- substr(df_ScanLadybug$decade)

df_StatisitcsCount <- df_ScanLadybug %>%
 dplyr::group_by(df_ScanLadybug$scientificName, df_ScanLadybug$decade) %>%
  summarize(number = n()) 

#Filtering amount of ladybugs found in the 1950s
df_decade1950 <- df_ScanLadybug %>%
  dplyr::select("catalogNumber", "scientificName", "decade") %>%
  dplyr::filter(df_ScanLadybug$decade == 1950) %>%
  dplyr::group_by(scientificName, decade) %>%
  summarise((number = n()))
#Finding the total number of ladybugs in the 1950s
df_Sum1950 <-df_decade1950 %>%
  dplyr::mutate(sumOfLadybugs = sum(df_decade1950$`(number = n())`))

#Filtering amount of ladybugs found in the 1960s
df_decade1960 <- df_ScanLadybug %>%
  dplyr::select("catalogNumber", "scientificName", "decade") %>%
  dplyr::filter(df_ScanLadybug$decade == 1960) %>%
  dplyr::group_by(scientificName, decade) %>%
  summarise((number = n()))
#Finding the total number of ladybugs in the 1960s
df_Sum1960 <-df_decade1960 %>%
  dplyr::mutate(sumOfLadybugs = sum(df_decade1960$`(number = n())`))

#Filtering amount of ladybugs found in the 1970s
df_decade1970 <- df_ScanLadybug %>%
  dplyr::select("catalogNumber", "scientificName", "decade") %>%
  dplyr::filter(df_ScanLadybug$decade == 1970) %>%
  dplyr::group_by(scientificName, decade) %>%
  summarise((number = n()))
#Finding the total number of ladybugs in the 1970s
df_Sum1970 <-df_decade1970 %>%
  dplyr::mutate(sumOfLadybugs = sum(df_decade1970$`(number = n())`))

#Filtering amount of ladybugs found in the 1980s
df_decade1980 <- df_ScanLadybug %>%
  dplyr::select("catalogNumber", "scientificName", "decade") %>%
  dplyr::filter(df_ScanLadybug$decade == 1980) %>%
  dplyr::group_by(scientificName, decade) %>%
  summarise((number = n()))
#Finding the total number of ladybugs in the 1980s
df_Sum1980 <-df_decade1980 %>%
  dplyr::mutate(sumOfLadybugs = sum(df_decade1980$`(number = n())`))

#Filtering amount of ladybugs found in the 1990s
df_decade1990 <- df_ScanLadybug %>%
  dplyr::select("catalogNumber", "scientificName", "decade") %>%
  dplyr::filter(df_ScanLadybug$decade == 1990) %>%
  dplyr::group_by(scientificName, decade) %>%
  summarise((number = n()))
#Finding the total number of ladybugs in the 1990s
df_Sum1990 <-df_decade1990 %>%
  dplyr::mutate(sumOfLadybugs = sum(df_decade1990$`(number = n())`))

#Filtering amount of ladybugs found in the 2000s
df_decade2000 <- df_ScanLadybug %>%
  dplyr::select("catalogNumber", "scientificName", "decade") %>%
  dplyr::filter(df_ScanLadybug$decade == 2000) %>%
  dplyr::group_by(scientificName, decade) %>%
  summarise((number = n()))
#Finding the total number of ladybugs in the 2000s
df_Sum2000 <-df_decade2000 %>%
  dplyr::mutate(sumOfLadybugs = sum(df_decade2000$`(number = n())`))

#Filtering amount of ladybugs found in the 2010s
df_decade2010 <- df_ScanLadybug %>%
  dplyr::select("catalogNumber", "scientificName", "decade") %>%
  dplyr::filter(df_ScanLadybug$decade == 2010) %>%
  dplyr::group_by(scientificName, decade) %>%
  summarise((number = n()))
#Finding the total number of ladybugs in the 2010s
df_Sum2010 <-df_decade2010 %>%
  dplyr::mutate(sumOfLadybugs = sum(df_decade2010$`(number = n())`))

#Filtering amount of ladybugs found in the 2020s
df_decade2020 <- df_ScanLadybug %>%
  dplyr::select("catalogNumber", "scientificName", "decade") %>%
  dplyr::filter(df_ScanLadybug$decade == 2020) %>%
  dplyr::group_by(scientificName, decade) %>%
  summarise((number = n()))
#Finding the total number of ladybugs in the 2020s
df_Sum2020 <-df_decade2020 %>%
  dplyr::mutate(sumOfLadybugs = sum(df_decade2020$`(number = n())`))

#creating a data frame with the totals from each decade created above
df_DecadeTotal <- rbind(df_Sum1950, df_Sum1960, df_Sum1970, df_Sum1980, df_Sum1990, df_Sum2000, df_Sum2010, df_Sum2020) %>%
  dplyr::select("scientificName","decade", "sumOfLadybugs")


decade_max <- as.integer(max(df_DecadeTotal$sumOfLadybugs))
decade_min <- as.integer(min(df_DecadeTotal$sumOfLadybugs))
decade_avg <- as.integer(mean(df_DecadeTotal$sumOfLadybugs))
decade_med <- as.integer(median(df_DecadeTotal$sumOfLadybugs))

AmtStats <- data.frame(decade_max, decade_min, decade_avg, decade_med)

mean1990 <- as.integer(mean(df_Sum1990$`(number = n())`))
mean1980 <- as.integer(mean(df_Sum1980$`(number = n())`))

tTestData <- rbind(mean1990, mean1980)

t.test(tTestData, mu=10) %>%
  tidy()


