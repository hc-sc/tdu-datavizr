# ***************************************************************************
# Title: Misleading data visualization
# Purpose: Demonstrates the use of scale and theme elements, as well as non-standard data format
# Author: Penelope Gorton
# Created: 2025-01-24
# Last Updated: 2025-02-03
# ***************************************************************************

# Let's start by recreating the 2015 National Review graphic regarding changes in global temperatures over the last 144 years.

# ***********************************
#     LOAD PACKAGES AND DATA ----
# ***********************************

## Load packages ---------

pacman::p_load(
  tidyverse,  # for data management and visualization 
  plotly      # for interactive graphs
  ) 

## Get data -----------

# As there aren't any publicly available global temperature datasets (that I could find), we will use the temperature anomaly data from the NASA figure - the change in global surface temperature (C), as compared to the average from 1951 to 1980. 

# We'll read in the .txt data from the URL using base R's read.table function ---
url <- "https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt"
temp <- read.table(url, skip = 5, header = FALSE, sep = "")
# This function reads in space-delimited files and converts them to a table. We are skipping the first 5 rows as these contain super headers (not needed) or row separators (----), which would result in all columns being combined. Because the column names are interspersed within row separators, we are saying we have no headers, and R will assign them (V1 to V3).

# Now we'll assign our column names ---
## Read in just the column names from row 3, and only import that row
col_names <- read.table(url, header = FALSE, sep = "", skip = 3, nrows = 1)
## Print them
col_names
## Assign our own versions of these names manually
colnames(temp) <- c("year", "diff_avg_c", "loess_c")

# Now let's double-check our data matches the online .txt file
View(temp)  
str(temp)
# looks good!


# ***********************************
#     NATIONAL REVIEW FIGURE ----
# ***********************************

## Manipulate data to recreate original ---------------

# First, we'll need to take our anomaly dataset, and try to back-calculate to the temperature data presented in the original National Review graphic. 
# According to NOAA, the average temperature for the 1951 to 1980 period was 14C. So let's add that to our anomaly variable (this is sketchy, but will serve our purposes). Then, we'll convert to Fahrenheit.

temp <- temp %>%
   mutate(avg_c = diff_avg_c + 14,
          avg_f = (avg_c*(9/5)) + 32)


## Now, let's reproduce the original crime! ----------------

ggplot(temp, aes(x = year, y = avg_f)) +
  geom_line(colour = "darkorange", linewidth = 1) +  # plot the data as a line
  scale_y_continuous(                      # use the scale function (hello again!) to alter the y-axis to match the graphic 
    limits = c(0, 110),                        # begin at 0 and end at 110
    breaks = seq(0,110, by = 5),               # breaks every 5 degrees
    expand = c(0,0)) +                         # remove the padding at the start and end of y-axis
  scale_x_continuous(                      # do the same for the x-axis
    limits = c(1880, 2015),                    # truncate at 2015 (note this will drop 9 data points)
    breaks = seq(1880, 2010, by = 10),         # match the 10 year breaks
    expand = c(0,0)) +                         # remove the padding at the start and end of x-axis
  labs(                                    # alter the labels to match
      x = "",                                  # remove the x-axis label  
      y = "",                                  # same for y
      title = "Average Annual Global Temperature in Fahrenheit",        # add the title
      subtitle = "1880-2015"                                            # and subtitle below
  ) +
  theme_classic() +                        # Use minimal theme for a clean base (quickly remove all grids)
  theme(                                   # and modify it with our custom theme
    panel.background = element_rect(fill = "grey95"),                    # light grey background
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),  # horizontal grey grid lines
    axis.text.x = element_text(size = 7, face = "bold", margin = margin(t = 5)),  # shrink and bold x-axis labels; increase gap
    axis.text.y = element_text(size = 7, margin = margin(r = 5)),        # reduce the font size of y-axis labels; increase gap
    axis.line.y = element_line(linewidth = 1.25),                        # thicken y-axis line
    axis.ticks.length.x = unit(-0.05, "cm"),                             # flip x axis ticks inwards 
    axis.ticks.length.y = unit(-0.1, "cm"),                              # flip y axis ticks inwards and increase length
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),     # centre the title and shrink font size
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold"),  # centre the subtitle and shrink font size
    panel.border = element_rect(color = "black", size = 0.5, fill = NA), # add border around all sides of the panel space
    plot.background = element_rect(colour = "black")                     # add border around plot
  ) 
# Looks pretty close!
# Of course we should add a note explaining how we recreated this data, but we are trying to do 'bad' after all!

ggsave(height = 4.5, width = 8, here("nr_plot.png"))


# ***********************************
#     NASA'S FIGURE ----
# ***********************************

# Now, let's reproduce NASA's version of it, which you can see here: https://climate.nasa.gov/vital-signs/global-temperature/?intent=121 

## Answer --------------
# find our min and max y
min(temp$diff_avg_c) 
max(temp$diff_avg_c)

ggplot(temp, aes(x = year)) +                  # keep only x within aes given we're plotting two y variables
  geom_line(aes(y = loess_c, colour = "Lowess smoothing"), linewidth = 0.75) +  # plot the smoothed data as a black line. this goes first as it's underneath the 'raw' data. note that colour is assigned to what we would like our legend name for it to be
  geom_line(aes(y = diff_avg_c, colour = "Annual mean"), linewidth = 0.75) +    # do the same for the annual mean. this goes next as it's below the filled points
  geom_point(aes(y = diff_avg_c, colour = "Annual mean"),                       # plot the same data again as points on top
    shape = 21,                                    # circle we can define outline and fill for                 
    fill = "white",                                # white fill
    size = 1.75,                                   # increase circle size
    stroke = 1.5                                   # increase outline width
  ) + 
  scale_y_continuous(                      # use the scale function to alter the y-axis to match the graphic 
    limits = c(-0.55, 1.3),                        # set limits to match range/NASA figure
    breaks = seq(0, 1, by = 0.5),                  # label breaks every 0.5, for 0 to 1
    expand = c(0,0.1)) +                           # remove the padding at the start and end of y-axis
  scale_x_continuous(                      # do the same for the x-axis
    limits = c(1880, 2024),                        # set limits to match year range
    breaks = seq(1880, 2020, by = 20),             # breaks from 1880 to 2020, every 20 yrs 
    expand = c(0,0.9)) +                           # remove the padding at the start (0), keep some at end (0.8) to allow full data pt
  labs(                                    # alter the labels to match NASA figure
    x = "YEAR",                                    # x-axis
    y = "Temperature Anomaly (C)",                 # y-axis
    title = "GLOBAL LAND-OCEAN TEMPERATURE INDEX", # title
    subtitle = "Data source: NASA's Goddard Institute for Space Studies (GISS)",  # subtitle
    colour = ""                                    # no title for legend
  ) +
  theme_minimal() +                        # Use minimal theme for a clean base
  theme(                                   # and modify it with our custom theme
    panel.grid.minor = element_blank(),                           # remove minor grid lines
    axis.text = element_text(size = 12, colour = "#646363"),      # set size and colour of numbers on both axes
    axis.title.y = element_text(size = 12, colour = "#646363", margin = margin(r = 10)), # set size and colour of axis title, increase gap between numbers and the title
    axis.title.x = element_text(size = 12, colour = "#646363", margin = margin(t = 10)), # same for x
    plot.title = element_text(size = 13, colour = "#BF5449", face = "bold"),             # styling of title
    plot.subtitle = element_text(size = 12, colour = "#7C7E7F"),                         # styling of subtitle
    plot.title.position = "plot",                                     # make title and subtitle left-justified to the edge of the plot rather than panel
    legend.position = "inside",                                       # place legend inside the panel
    legend.position.inside = c(0.14, 0.83),                           # position it exactly inside the panel (x,y coords)
    legend.background = element_rect(fill = "white", colour = NA),    # make legend background white
    legend.text = element_text(size = 11)                             # resize legend text
  ) +
  scale_colour_manual(values = c("Lowess smoothing" = "black", "Annual mean" = "lightgrey"))  # map colours to your line names. Make sure the names match exactly what you typed in the geoms!

ggsave(height = 5.75, width = 8, here("nasa_plot.png"))


# EXTRA: See if you can make the NASA plot interactive using the plotly package! Is there anything you need to change?
# First assign your plot as an object (here, p)
# Then nest it within ggplotly
ggplotly(p)


# ***********************************
#     MAKE YOUR OWN FIGURE! ----
# ***********************************

# Both these graphics are misleading in their own ways. How would you present climate data? You can consider aspects such as the time scale, how the data is described, how variability is shown, the use of a reference period, etc. 

