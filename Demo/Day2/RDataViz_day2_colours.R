# ***************************************************************************
# Title: Changing colours using scale_()
# Purpose: Walk through some ways to change colours of your plotted data
# Author: Penelope Gorton
# Last Updated: 2025-01-28
# ***************************************************************************


# ***********************************
#     PREP ENVIRONMENT & LOAD DATA ----
# ***********************************

## Install and load packages ------
pacman::p_load(
  rio,            # import/export
  tidyverse,      # data management, summary, and visualization
  RColorBrewer,   # colour palettes
  wesanderson,    # colour palettes
  paletteer,      # all of the colour palettes 
  colorBlindness  # test palettes through simulation 
)


## Download and import dataset in one step -------
linelist <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/case_linelists/linelist_cleaned.rds")


# ***********************************
#     CREATE PLOTS TO PLAY WITH ----
# ***********************************

# We will be working with plots where we are using *scaled* aesthetics - that is, where aesthetics (in this case colour) are mapped to a variable. 

# So, let's first create some named plots so that we can easily modify plot colours without repeating the plotting code each time.  

## For discrete colour aesthetics -----
d <- ggplot(data = linelist, aes(x = age, y = wt_kg, colour = gender)) +
  geom_point()

d  # call (print) the plot

## For continuous colour aesthetics -------
c <- ggplot(data = linelist, aes(x = age, y = wt_kg, colour = age)) +
  geom_point()

c

# We can control the colour of scaled aesthetics using the scale_() functions. Scales allow you to change plot defaults such as colours, but also things like axis labels. 

# Scales follow this generic formula: scale_x_y(), where x is the aesthetic, and y is the method. 

# Use scale_colour_y() for colouring lines and points, and scale_fill_y() for the inside colour (e.g. bar plots, violin plots, box plots, etc.). Note that given our plot types, this demo focuses on scale_colour_y(), but these methods can all be changed to scale_fill_y()! 

# y (method) can take many forms, including discrete, continuous, gradient, manual, etc. 

# Ok, now it's colour time!


# ***********************************
#     MANUAL ASSIGNMENT ----
# ***********************************

# see R's colour names and some hex codes here: https://derekogle.com/NCGraphing/resources/colors 

# I love this site for creating palettes and finding hex codes: coolors.co 

## Discrete ------

d + 
  scale_colour_manual(values = c("darkcyan", "goldenrod", "grey28")) # don't forget to specify the colour for NAs! 
# This will assign colours to values based on the order the values occur in your dataset. However, you can specify your specific values without needing to know the order (and here we're using hex codes):

d + 
  scale_colour_manual(values = c("m" = "#008B8B", "f" = "#DAA520"), na.value = "#555555")

## Continuous ------

c + scale_colour_gradient(low = "orange", high = "purple")

# Explore _gradient2 (diverging) and _gradientn (n-colours) on your own! 


# ***********************************
#     PALETTES ----
# ***********************************

# We can also use pre-defined palettes (collections of colours). Palettes can be discrete or continuous. Here are a few examples:

## RColorBrewer ---------

display.brewer.all() # view all palettes
display.brewer.all(colorblindFriendly = TRUE) # view just colourblind friendly palettes 

d + scale_colour_brewer(palette = "Dark2", na.value = "grey28") # discrete
c + scale_colour_distiller(palette = "YlOrRd") # continuous

  
## The famous viridis -------------
  
d + scale_colour_viridis_d(na.value = "grey28")  # discrete
c + scale_colour_viridis_c(na.value = "grey28")  # continuous
c + scale_colour_viridis_c(na.value = "grey28", option = "plasma")  # 8 different options available


## The paletteer collection ----------

# The colour package to end all packages. A collection of colour palettes from a number of packages (including all those above!). Visit https://pmassicotte.github.io/paletteer_gallery/ to view a gallery of the *many* palette options.

# You must ensure that the palette you select is appropriate to your data (discrete or continuous). You may have to answer Y to download the package containing the palette.

d + scale_colour_paletteer_d("ghibli::MononokeMedium", na.value = "grey28", direction = -1) # direction reverses the palette
c + scale_colour_paletteer_c("harrypotter::ronweasley2")  # format is always "package::palette"


# ***********************************
#     INDEPENDENT EXPLORATION ----
# ***********************************

## Colour blind-friendly testing ---------------
# There are a number of ways that you can explore the colour-blind-friendliness of your chosen palettes. One option is to use the colorBlindness package: 

# Select your favourite palette from those explored above, and see a simulation of it's appearance for the some types of colorBlindness:
displayAllColors(viridis::viridis(6)) # provide package name, palette name, and # of colours
# Don't worry about the warnings :)

## More RColorBrewer --------
# If you'd like individual hex codes or to change just one colour within a palette, you can explore this usage:
brewer.pal(n = 8, name = "Dark2") # generate palette and get individual hex codes 
col_pal <- brewer.pal(8, "Dark2") # assign it, can then use 'col_pal' within your plot's colour call  

## More paletteer -----------
# If you'd like to pick and choose some of the colours from a palette and then assign them manually, you can view hex codes like this (format is "package::palette"): 
paletteer_d("trekcolors::romulan")                  # for discrete palettes (fixed n)
paletteer_c("harrypotter::dracomalfoy", n = 7)      # for continuous palettes
paletteer_dynamic("cartography::grey.pal", n = 7)   # for dynamic palettes (discrete palettes that do not have a fixed # of colours)

# And to get list of palette names and types (qualitative, sequential, diverging):
palettes_dynamic_names  
palettes_c_names
palettes_d_names

## Another fun package is wesanderson ----------
d + scale_colour_manual(values = wes_palette("FantasticFox1", n = 3, type = "discrete")) 
c + scale_colour_gradientn(colours = wes_palette("Zissou1", type = "continuous")) 
# try using it with paletteer! 
