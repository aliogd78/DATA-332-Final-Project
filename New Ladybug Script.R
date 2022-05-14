library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(haven)
library(knitr)
library(broom)

#Set the Working Directory
rm(list = ls())
setwd("C:/Users/aliso/Downloads/Project Insect Carnivore main/Project-Insect-Carnivore-main")



#Open the files to read
df_Ladybug <- read_excel("data/Ladybug Data.xlsx", sheet = 1)
df_ScanLadybug <- read.csv("data/Scan Ladybug Data.csv") %>%
  #Clean data so that values are consistent
  dplyr::mutate(stateProvince = ifelse(stateProvince == "IL", "Illinois", stateProvince)) %>%
  dplyr::mutate(stateProvince = ifelse(stateProvince == "Il", "Illinois", stateProvince)) %>%
  dplyr::mutate(stateProvince = ifelse(stateProvince == "IA", "Iowa", stateProvince)) %>%
  dplyr::mutate(stateProvince = ifelse(stateProvince == "Ia", "Iowa", stateProvince)) %>%
  dplyr::mutate(scientificName = ifelse(scientificName == "harmonia axyridis", "Harmonia axyridis", scientificName))
 # dplyr::mutate(scientificName = ifelse(is.na(scientificName), "UNKNOWN", scientificName))
# dfg <- df_ScanLadybug %>%
#   group_by(stateProvince) %>%
#   summarise(n())
  
##Putting the years into their respective decades   
df_ScanLadybug$year <- as.integer(df_ScanLadybug$year)
df_ScanLadybug$decade <- df_ScanLadybug$year
df_ScanLadybug$first_three <- substr(df_ScanLadybug$year, 1, 3)
df_ScanLadybug$digit <- paste(0)
df_ScanLadybug$decade <- paste0(df_ScanLadybug$first_three, df_ScanLadybug$digit) 
#df_ScanLadybug$decade <- substr(df_ScanLadybug$decade)

#Selecting the columns that we want to work with 
df <- df_ScanLadybug %>%
  dplyr::select(decade, scientificName, stateProvince)


#Create the pivot table
df <- df %>%
  dplyr::group_by(decade, scientificName, stateProvince) %>%
  dplyr::summarise(number_of_species = n()) %>%
  #dplyr::summarise(average_number = mean(number_of_species)) %>%
  dplyr::filter(decade == "2020" | decade == "2010" | decade == "2000") %>%
  dplyr::filter(!is.na(scientificName)) %>%
  dplyr::filter(!is.na(decade))


#Number of Specific Species caught 2000-2020
#Create a chart
ggplot(df, aes(x=number_of_species, y=scientificName, fill=scientificName)) + 
  geom_bar(stat = "identity") +
  xlab("Number Of Insects Found") +
  ylab("Species") +
  ggtitle("Species Found during the decades of 2000-2020") +
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))

#Species caught in Illinois and Iowa in 2000-2020
#Create a chart
ggplot(df, aes(y=number_of_species, x=stateProvince, fill=stateProvince)) + 
  geom_bar(stat = "identity") +
  xlab("State") +
  ylab("Number of Insects Found") +
  ggtitle("Total Number of Insects Found in Illinois & Iowa during 2000-2020") +
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))

#Number of total species caught in 2000-2020
ggplot(df, aes(y=number_of_species, x=decade, fill=decade)) + 
  geom_bar(stat = "identity") +
  xlab("Decade") +
  ylab("Number of Insects Found") +
  ggtitle("Number of Insects Found by Decade") +
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))

#Selecting the columns that will be used in calculating the average
dfAvg <- df_ScanLadybug %>%
  dplyr::select("decade", "scientificName")

#find the total amount of species caught per decade
df_pivot <- dfAvg %>%
  count(decade, sort = TRUE)

#Find the average
df_pivot$average <- df_pivot$n/10

#Create the T Test of the average amount of ladybugs found per decade
#mu = 7 because we believe that the average number of ladybugs found per decade
#would be around 7 because of the total found (638/8) 
#we are not including the value of "NA0", we can't get that value to filter, but it is so miniscule we don't think it will matter
t.test(df_pivot$average, mu=7) %>%
  tidy()

