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

# Note: the following isn't the best practice, but does work to clear your 
# environment when you want to start fresh. It is generally considered much 
# safer and purposive to click the clear-button (broomstick icon) above your 
# environment pane. 

 rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
 gc() #free up memory and report the memory usage.


# Load required libraries: 
# install.packages("pacman") # Install the pacman package if you haven't already
pacman::p_load(tidyverse,
               rio, 
               here,
               ggridges,
               RColorBrewer) 


# Import dataset from Part I. 

covid.ww <- rio::import(here("IndependentActivity1", "data", "covid_ww.csv"), 
                        header = TRUE)

# While rio is super useful, it imports Dates as an integer-date format. (IDate)
# This can cause headaches later, so let's update this now. 

covid.ww$date <- as.Date(covid.ww$date, 
                         format = "%Y-%m-%d")


# Lets have a look at the variables in the dataset: 

# Which variables might you want to visualize to get a sense of the dataset? 

# Let's look at the following: 
# 1) Date ranges
# 2) Locations 
# 3) viral_load ranges 
# 4) regional and provincial populations 

##########################################
#
# 1) Date Ranges and 2) Locations:
#
##########################################
# Let's see how many observations we have over the range of dates present in the 
# dataset. We can quickly do this using the hist() function in base R. 

hist(covid.ww$date, freq = TRUE, 
     breaks = "months", 
     xlab = "Date", 
     main = "Observations of viral load from COVID-19 Wastewater sites
     in Canada, 2021-2023")

# It looks like the majority of the covid wastewater data ramps up after 2021. 
# Unfortunately, using this visualization, we can't tell which sites were operational 
# at which times. We'll need to plot sites individually to tell this. 

#e.g., for sites in Winnipeg, MB
par(mfrow=c(3, 1)) # set up the multi-panel plot

hist(covid.ww[covid.ww$location == "Winnipeg North End",]$date,
     freq = TRUE, breaks = "months",
     main = "Winnipeg North", xlab = "Date")
hist(covid.ww[covid.ww$location == "Winnipeg South End",]$date,
     freq = TRUE, breaks = "months",
     main = "Winnipeg South", xlab = "Date")
hist(covid.ww[covid.ww$location == "Winnipeg West End",]$date,
     freq = TRUE, breaks = "months",
     main = "Winnipeg West", xlab = "Date")


# What about if we wanted to do this for all sites? This sounds like a lot of extra lines of code. 
# How many locations are there in the dataset? 

length(unique(covid.ww$location)) # 36 locations! Too many to type by hand easily


# Lets switch to ggplot to make a quick comparison of all sites. 
ggplot(covid.ww, aes(x=date)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(~province_abbr + location) + 
  theme_minimal() #click Zoom to view in fullscreen. 

# OK - not the prettiest graphic ever, but informative. 
# We can see for example, that some sites in Atlantic Canada and Yukon 
# were established quite a bit later on. 


##########################################
#
# 3) Viral load ranges:
#
##########################################

# So we've seen frequency of observations, but it may be useful to get an idea 
# of the actual range of measurements for viral load when making decisions about 
# analysis later. Let's assess these visually using our data viz skills! 


# The simplest way to look at the spread of data is likely using a scatter plot. 
# Let's start with base R plotting 

# Clear parameter changes from previous plot. 
dev.off()

plot(viral_load ~ date, 
     data = covid.ww[covid.ww$location == "Saskatoon",],
     main = "Wastewater viral load in Saskatoon, SK")

lines(viral_load ~ date, 
      data = covid.ww[covid.ww$location == "Saskatoon",],
      lty = "dotted")


# Quickly looking at this, we can see that viral load ranges for Saskatoon lie 
# between 0 and approximately 250 viral particles / sample volume. However, these 
# ranges occur over a long timeframe. It may be useful to have a better idea of 
# the median distribution of viral load across the sample time. To do this, let's 
# try incorporating a box-and whisker plot. 

boxplot(covid.ww[covid.ww$location == "Saskatoon",]$viral_load)


# Multi-panel plots in base R: 

# We can use layout to add custom plot widths for multiple panels in baseR:
layout(matrix(1:2, ncol=2), 
       widths=c(3, 1))
# layout() can mess up the default margins, so we'll set these individually: 
par(mar=c(5, 4, 3, 0), oma = c(0, 0, 3, 0)) #oma = outer margins
# start plotting: 
plot(viral_load ~ date, data = covid.ww[covid.ww$location == "Saskatoon",], ylab = "Viral Load", xlab = "Year")
lines(viral_load ~ date, data = covid.ww[covid.ww$location == "Saskatoon",],
      lty = "dotted")

# add second plot: 
par(mar=c(5, 0, 3, 0))
boxplot(covid.ww[covid.ww$location == "Saskatoon",]$viral_load, axes = FALSE)
mtext("COVID-19 viral load in wastewater, Saskatoon, SK. 2021-2023.",
      side = 3, line = 0, outer = TRUE, font = 2)


# Ok - so we've plotted one site, but there are 36 overall, and that's too much 
# code to type out individually. Why don't we try switching to ggplot again and 
# see if we can get some ranges of data for each site? 

# Say we wanted to be able to see multiple data distributions on the same plot, 
# while keeping it attractive and being able to spot any major differences 
# easily. Ridgeline plots are a nice way of doing this - and can be made using 
# the ggridges package. 


ggplot(covid.ww, aes(x= viral_load, 
                     fill = fct_reorder(province_abbr, pruid))) + 
  #we use fct_reorder to plot the locations in the same order as the 
  #provinces/territories based on their unique ID (PRUID)
  geom_density_ridges(aes(y = fct_reorder(location, -pruid))) +
  # here we'll change the x-axis limit to 300 to better fit the majority of the
  # data onto the screen. 
  scale_x_continuous(limits = c(0, 300)) + 
  theme_minimal() + 
  labs(title = "Density distribution of viral load for COVID-19 
       wastewater sites in Canada, 2021-2023", 
       fill = "Province / \n Territory",
       x = "Viral Load", y = "Location") + 
  # Adjust the positioning of the plot title to better fit the visualization:
  theme(plot.title = element_text(hjust = 1))


# There are lots of ways you could apply ridgeline plots to your data. Feel free 
# to explore! Maybe try plotting viral load over time for all the sites in a 
# single province.

###############################################################################
#
#  4) Regional and Provincial Population data
#
###############################################################################

# Obviously the provinces (and territory) in our dataset represent very 
# different sized populations. Let's take a minute to explore these population 
# sizes, as it may help us in defining our question for our final data 
# visualization. 

# Starting with base R: let's create a simple bar plot to compare sizes of the 
# provincial populations for each site. For base R plotting, we'll want to 
# subset the data first so we don't have repeat observations


pop.table <- unique(covid.ww[c("province_abbr", "location", 
                               "region_pop", "province_pop")])

# Let's create a table that we can use to assign color profiles to provinces and 
# territories. 
prov.color <- data.frame(province_abbr = c("AB","BC","MB","NB",
                                           "NL","NS","ON","PEI",
                                           "QC","SK","YK"),
                         color = RColorBrewer::brewer.pal(11, "Set3"))

pop.table <- left_join(pop.table, prov.color)                         


# Restart the graphics device
dev.off()


par(mar = c(5, 10, 3, 4)) # Set margins


barplot(height = pop.table$region_pop, 
        names =pop.table$location,
        cex.names = 0.75,
        col = pop.table$color,
        xlab = "Regional Population (2021)",
        horiz = TRUE,
        axes = TRUE,
        las = 1,
        main = "Regional Population of Covid-19 Wastewater 
        sites in Canada, 2021-2023")

legend("bottomright", title = "Province", 
       legend = unique(pop.table$province_abbr), 
       fill = unique(pop.table$color),
       cex = 0.6)


# Kind of boring, what can you do to improve it at this point?
# Why not try something different, like a bubbleplot? 

# Bubbleplots are a special case of a scatterplot - where the size of the points
# is directly proportional to a value you're trying to show. 
# In this instance we can use the population size of covid wastewater sites to 
# indicate the size of the bubbles! 

pop.table %>% 
  ggplot(aes(x = province_abbr, 
             y = region_pop, 
             fill = province_abbr)) + 
  geom_jitter(aes(size = region_pop), alpha = 0.6, 
              shape = 21, color = "black") +
  scale_size(range = c(.75, 24), 
             name="Sample Site Population") + 
  scale_color_brewer(palette = "Spectral") + 
  theme_minimal() + 
  labs(title = "Regional population at COVID-19 wastewater sites",
       subtitle = "Canadian provinces and territories, 2021-2023", 
       y = "Municipal/Regional population size",
       x = "Province",
       fill = "Province")
  
# So there you have it. Something a little different that lets you compare sizes 
# and incorporates x/y comparisons with color and size. Lots of options! 


############################################################################
# Extra resources on grouped bar charts
############################################################################

### Modify for grouped bar chart by province instead of by site

pop.matrix<- 
  pop.table %>% 
  select(province_abbr, region_pop) %>%
  group_by(province_abbr) %>% 
  mutate(site = 1:n()) %>%
  pivot_wider(names_from = province_abbr, 
              values_from = region_pop) %>%
  column_to_rownames("site") %>%
  as.matrix()

options(scipen=5)

barplot(pop.matrix, 
        beside = TRUE,
        horiz = TRUE,
        space = c(0, 1), 
        col = brewer.pal(7, "Set3"), 
        las = 1, 
        main = "Regional/municipal population size at COVID19 wastewater sample sites, Canada 2021-2023")

legend("bottomright", title = "Site", 
       legend = c("1", "2", "3", "4", "5", "6", "7"), 
       fill = brewer.pal(7, "Set3"))


# Not bad, but you'll notice that there are gaps within each of the groups.
# This is caused by the fact that we're comparing unequal group sizes. In other 
# words, provinces and territories have different numbers of sites. 
# These gaps are very difficult (maybe impossible) to remove entirely within the 
# base R plotting system. Let's switch gears to ggplot and see what we can do. 


pop.table %>% 
  select(province_abbr, region_pop) %>%
  group_by(province_abbr) %>% 
  mutate(site = 1:n()) %>% 
  ggplot() + 
  geom_bar(aes(x = province_abbr, y = region_pop, fill = as.character(site)),
           stat = "identity", position = "dodge", col = "black") + 
  #facet_wrap(.~province_abbr, scales = "free", ncol = 3) + 
  theme_classic() +
  coord_flip() + 
  labs(title = "Regional population of COVID-19 Wastewater test site locations in provinces and territories in Canada, 2021",
       y = "Regional or Municipal Population",
       x = "Province or Territory",
       fill = "Site")






















































