####################################################################
# PHACR Training
#                                               
# Purpose: A data-viz scenario involving COVID-19 Wastewater data
# from multiple sites across Canada. 
#
# To be completed after day 1 of TDU R-Dataviz
#
# Data Used: datasets available in covid.ww course folder
#
#                                               
# Author: Benjamin Hetman
# Created: 2023-03-10
#
# Modified By: J. Stares
# Date Last Modified: 2025-01-28
####################################################################
# install.packages("pacman") # Install the pacman package if you haven't already
pacman::p_load(tidyverse,
               rio, 
               here) 
               

# Example: Starting with MB and QC
#
# Part 1.1: compile the data from daily and weekly measurements into a cohesive dataset
#
# Daily COVID-19 Wastewater measurements are in the daily-viral-load folder
MB.daily <- rio::import(here("IndependentActivity1", "data", "daily_viral_load", 
                             "MB_daily_viral_load.csv"), 
                        header = TRUE)
QC.daily <- rio::import(here("IndependentActivity1", "data", "daily_viral_load",
                             "QC_daily_viral_load.csv"), 
                        header = TRUE)

# Have a look at the format of each of these data sources

str(MB.daily)
str(QC.daily) # It looks like each of these datasets have a *lot* of variables in wide format

# Before we do anything with these datasets, lets look at the weekly averages 
MB.weekly <- rio::import("IndependentActivity1/data/weekly_mavg/MB_7DayAvg.csv",
                         header = TRUE)
QC.weekly <- rio::import("IndependentActivity1/data/weekly_mavg/QC_7DayAvg.csv",
                         header = TRUE)

str(MB.weekly)
str(QC.weekly)

# From the looks of things, the weekly data are in an easier format to work with, so let's convert our daily data to a similar format
MB.daily.long <- MB.daily %>% pivot_longer(!Location:fractionid, 
                                           names_to = "Date", 
                                           values_to = "viral_load")
QC.daily.long <- QC.daily %>% pivot_longer(!Location:fractionid, 
                                           names_to = "Date", 
                                           values_to = "viral_load")

# Combine the daily measurements into a single data.frame 
daily.viral.load <- bind_rows(MB.daily.long, QC.daily.long)

# We will need to perform the same treatments to our weekly average datasets as well, though these are already in a long-format
#
weekly.viral.load <- bind_rows(MB.weekly, QC.weekly)
#
#
combined.viral.load <- left_join(daily.viral.load, weekly.viral.load) # doesn't work, need to reformat "Date"
#
weekly.viral.load$Date <- as.character(weekly.viral.load$Date)
# 
# Try again:
combined.viral.load <- left_join(daily.viral.load, weekly.viral.load)
# This time it works! 


###########################################################################
#                                                                         #
# Part II: Create your master Canada-wide data set                        #
#                                                                         #
###########################################################################

# Okay - so we've seen how to combine 2 provinces into a useable dataset: let's
# go ahead at create our master data set using the same approach but on all of 
# the available provincial data.

# Let's clear our work space so we're starting clean:

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.



# Import all of the daily COVID19 Datasets from the daily_viral_load folder
# Note, you can absolutely do this one-by-one, or as we'll see - you can import
# them more quickly all at the same time. This only works because they have the
# same format.



# First, we'll create a list of all the data frames we want to load: 
ls.daily.canada <- list.files(path = "IndependentActivity1/data/daily_viral_load/", full.names = TRUE)
# note: full.names argument is required to list the path to the files

# Now we use import_list to load the data frames into a *list*
daily.canada <- import_list(ls.daily.canada, header = TRUE)

# Have a look at the structure of this list now: (large output)
str(daily.canada)

# Recall that all these daily covid readings are in a wide-format. We'll need to 
# transform this into long format to work with them more efficiently.

# We'll combine our pivot_longer() function with the map() function from tidyverse
# in order to do these two things at once. 


# Note: this is a complex, but very useful operation. Try to make sure you 
# understand what is happening here by reading the function help entry for map().
daily.canada.long <- map(daily.canada, 
                         ~ pivot_longer(.x,
                                        cols = !Location:fractionid,
                                        names_to = "Date",
                                        values_to = "viral_load")
                         )


# Now lets create a single data frame from our list of long daily data frames. 
daily.canada.long.final <-  bind_rows(daily.canada.long)


###
### Weekly COVID19 waste water readings for Canada
###


# Similar to above, we'll create a list of all the data frames we want to load: 
ls.weekly.canada <- list.files(path = "IndependentActivity1/data/weekly_mavg/", full.names = TRUE)
# note: full.names argument is required to list the path to the files

# Now we use import_list to load the data frames into a *list*
weekly.canada <- import_list(ls.weekly.canada, header = TRUE)

# Have a look at the structure of this list now: (large output)
str(weekly.canada)

# These readings are already in long-format, so we can proceed to bind all the 
# data frames into a single dataset. 

weekly.canada.long.final <-  bind_rows(weekly.canada)


# Now that we have both our daily and weekly COVID19 wastewater measurements 
# in the same format, we can combine them into a single, long-format dataframe

covid.ww <- left_join(weekly.canada.long.final,
                      daily.canada.long.final)


# Looks like we can't combine these because of incompatible "Date" variables. 
# Let's fix that! 

weekly.canada.long.final$Date <- as.character(weekly.canada.long.final$Date)

# Now, re-run the left-join
covid.ww <- left_join(weekly.canada.long.final,
                      daily.canada.long.final)


################################################################
## Import additional information with our COVID wastewater data
################################################################

# Now that we have our covid.ww master dataset, we can bring in some additional 
# data to help us decide how to build our visualization(s). In the data folder, 
# there are two files under demographics/ . Let's have a look at these and see 
# how we can incorporate them. 

pruid <- rio::import(here("IndependentActivity1", "data", "demographics", "pruid.csv"), 
                     header = TRUE)
str(pruid)



region.pop <- rio::import(here("IndependentActivity1", "data", "demographics", 
                               "region.pop.2021.csv"), header = TRUE)
str(region.pop)

# First we'll combine the region.pop information with our covid.ww using the 
# 'region' variable as the key. 

covid.ww.region <- left_join(covid.ww, region.pop)

# Now that we have the regional population data imported, we can deal with the 
# provincial population data. Notice that there are no consistent unique ID's 
# between the these datasets... (gasp!). We'll have to create one, the easiest 
# way of doing this is probably adding a "PROV" shorthand variable to the pruid 
# dataset. 

# Take note of the order of provinces and territories in the pruid table, and create a vector 
# of the abbreviations in the same order. 

PROV <- c("NL", "PEI", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YK", "NWT", "NU") 
pruid$prov <- PROV

# Now we can combine the pruid table with our master dataset using PROV as the key. 

covid.ww.final <- left_join(covid.ww.region, pruid)

# Let's take a second now to clean up the variable names in our final dataset: 

var.names <- c("date", 
               "seven_day_rolling_avg",
               "location",
               "region",
               "measureID",
               "fractionID",
               "viral_load",
               "province_abbr",
               "region_pop",
               "pruid",
               "province",
               "province_pop")

colnames(covid.ww.final) <- var.names

# And we can write our final dataset to disk so we don't have to redo the above 
# steps again. 

write_csv(covid.ww.final, here("IndependentActivity1", "data", "covid_ww.csv"))
# Hurray! 




































