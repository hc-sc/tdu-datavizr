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

# See this infographic for a handy visual of how to change some common components of a plot: https://isabella-b.com/blog/ggplot2-theme-elements-reference/

## Hints -----------
# We will need to make a number of changes to our graph above. These include:
  # change the variable being plotted to show anomaly data  
  # find the max and min of this variable to determine our y-axis range
  # plot this variable as a light grey line, that is thinner than the line in the plot above
  # use geom_point to add the same variable as as point layer, using the shape, colour, fill, size, and stroke arguments to produce points that are largish circles outlined in the same grey, filled with white
  # plot the smoothed data as a black line
  # be careful of your layer order! rearrange until it matches the NASA plot.
  # use the scale functions we used above to modify the x and y axes to match the NASA figure 
  # use the lab function we used above to change the axis labels, title and subtitle to match the NASA figure
  # use some of the same theme functions we used above to match the NASA figure, with the addition of:
    # remove all minor grid lines using panel.grid.minor
    # use legend.position, legend.position.inside, legend.background to modify the legend
    # tip - if you would like to match NASA's colours exactly, I use the eyedropper in powerpoint :)
  # Note: You may find you're having issues with the legend. To get a legend, you need to place your colour argument *within* the aes call. And since our data is wide (in that our two line colours are separate variables, rather than levels within a variable), we need to be a bit sneaky. In this situation, the fix involves assigning what we want our legend item names to be as the colour, and then assigning those names to colour in colour_scale_manual(). See the day2 colours demo code for how to do this!


# EXTRA: See if you can make the NASA plot interactive using the plotly package! Is there anything you need to change?


# ***********************************
#     MAKE YOUR OWN FIGURE! ----
# ***********************************

# Both these graphics are misleading in their own ways. How would you present climate data? 
