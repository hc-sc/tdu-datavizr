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
# Created: 2023-04-21
#
# Modified By: 
# Date Last Modified: 2023-04-21
####################################################################


# Data viz task: use the COVID-19 Wastewater dataset to create a visualization
# that describes: 
# 1) daily viral load
# 2) 7-day moving average
# 3) multiple plot panels
# 4) compares data from multiple provinces 


# Example: 


# Setup
pacman::p_load(tidyverse,
               rio, 
               here,
               ggridges,
               RColorBrewer) 



## To get you started: baseR graphics system

# Import dataset 
covid.ww <- rio::import(here("data", "covid_ww.csv"), header = TRUE)

# Choose the locations that you want to visualize
c19.subset_AB <- covid.ww[covid.ww$province_abbr == "AB",]
c19.subset_NL <- covid.ww[covid.ww$province_abbr == "NL",]

# Identify the date ranges for each subset so you can adjust the x-axis
min.date.ab <- min(c19.subset_AB$date)
max.date.ab <- max(c19.subset_AB$date)

min.date.nl <- min(c19.subset_NL$date)
max.date.nl <- max(c19.subset_NL$date)

# Reset graphic parameters for fresh plotting: 
dev.off()

# Set up your plotting canvas
#par(mfrow = c(2, 1), mar = c(4, 1, 2, 4))
layout(matrix(c(1,2,3), ncol=1, byrow=TRUE), heights = c(3, 3, 1))
# layout() can mess up the default margins, so we'll set these individually: 
par(mar=c(1, 4, 3, 0), oma = c(0, 0, 3, 0)) #oma = outer margins


# Print the first plot
plot(viral_load ~ date, data = c19.subset_AB[c19.subset_AB$date >= min.date.nl,], main = "Edmonton, AB", ylab = "Viral Load")
lines(viral_load ~ date, data = c19.subset_AB)
lines(seven_day_rolling_avg ~ date, data = c19.subset_AB, lty = "solid", col = "darkred")


# Print the second plot 
par(mar=c(4, 4, 3, 0))

plot(viral_load ~ date, data = c19.subset_NL, main = "St. John's, NL", ylab = "Viral Load")
lines(viral_load ~ date, data = c19.subset_NL)
lines(seven_day_rolling_avg ~ date, data = c19.subset_NL, lty = "dashed", col = "darkred")

par(mai=c(0,0,0,0))
plot.new()
legend("center", ncol = 2, legend = c("Daily", "Weekly"), lty = c("solid", "dashed"), col = c("black", "darkred"), title = "Viral Load Measurements")



































