# ***************************************************************************
# Purpose: ggplot demo 
# Based on EpiRHandbook (Ch30, ggplot basics) 
# Last modified: 2025-01-23 by P. Gorton 
# Modifications: formatting, explanations; added questions, breakdown, etc.
# ***************************************************************************

# PREPARE WORKSPACE AND IMPORT DATA ------------

# Install pacman if you don't have it already
# install.packages("pacman")

# Load packages (this will install AND load)
pacman::p_load(
  rio,            # import/export files
  here,           # file pathways
  tidyverse      # data management packages, visualization (incl. ggplot2!)
  )

# There are two ways to import the data:

# 1. Download and import in one step (no need for RProject or here package)
linelist <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/case_linelists/linelist_cleaned.rds") 
  # You can ignore the warning, it's about only loading from trusted sources and changes to rio2.0

# 2. Alternatively, download the cleaned data file to your computer by copying the link into your browser. Move the downloaded file into your home directory, or into the folder containing an Rproject and this script.

# Printing here() will tell you your current working/root directory if you're unsure. 
# here()  
# Import this data using rio and here packages 
linelist <- import(here("linelist_cleaned.xlsx")) # change to your directory


# STEP BY STEP BREAKDOWN OF A SIMPLE PLOT ----------

# Initialize the canvas and specify the data
ggplot(data = linelist)

# Now let's use 'aesthetics' to map your variables to the x- and y-axes
ggplot(data = linelist, mapping = aes(x = age, y = wt_kg)) 

  # note this is equivalent to:
  ggplot(linelist, aes(x = age, y = wt_kg)) 
  
  # and to:
  linelist %>% ggplot(aes(x = age, y = wt_kg))

# FINALLY, add a geometric layer to tell R HOW to visualize the relationship - here, telling it to plot each observation as a point (scatterplot)
ggplot(data = linelist, aes(x = age, y = wt_kg)) + # use the + sign to add a layer 
  geom_point()  # display data as points
  # This geom inherits the aesthetic mappings (here, axes assignments) from the ggplot() command 

# What do you think the warning is about? Look at your data :)


# Note: Data and aesthetics can be mapped either within ggplot (applies to whole plot) and/or within a specific geom. So, the above code is also equivalent to:
ggplot(data = linelist) +
  geom_point(aes(x = age, y = wt_kg))

# and to:
ggplot() +
  geom_point(data = linelist, aes(x = age, y = wt_kg))

# This functionality allows us to use the same aesthetic arguments in different layers, and even to plot different datasets!




# What other variables might modify the relationship between age and weight? How would we plot those? 









# Answer - how about we investigate gender? Here we will do so using colour in our existing plot

# MODIFYING AESTHETICS: SCALED  ------------
# Display of data depends on observation's value = *within* aes()

ggplot(data = linelist,   # set data
       aes(               # map aesthetics to column values
         x = age,           # map x-axis to age            
         y = wt_kg,         # map y-axis to weight
         colour = gender))+ # map color to gender (outline colour)
  geom_point()            # display data as points


# So far, everything we have plotted has been related to the values of an observation. However, sometimes we may want to change a visual aspect of how each point is displayed, regardless of value. 
# For example, let's say we would like all the data points to be slightly transparent, and a diamond shape. How would we do that?

# MODIFYING AESTHETICS: STATIC  ------------
# Applied to all data points = *outside* aes()

ggplot(data = linelist,   # set data
       aes(               # map aesthetics to column values
         x = age,                    
         y = wt_kg,         
         colour = gender))+   
  geom_point(             # display data as points
    shape = "diamond",      # change shape 
    alpha = 0.5)            # point transparency at 50% 

# Now, let's say we would like to see a regression line for this data. How might we do that?









# MULTIPLE GEOMETRIC LAYERS ---------------
# Let's try adding a linear trend line now
ggplot(data = linelist,
       mapping = aes(      # map aesthetics to columns
         x = age,
         y = wt_kg,
         colour = gender)) + 
  geom_point(              # display data as points
    shape = "diamond",       
    alpha = 0.5) +             
  geom_smooth(             # add a trend line (smoothed cond. mean)
    method = "lm",           # with linear method
    linewidth = 1.5)         # widen line width

# Why do we have three lines?? 

# How might we create a single trend line for all the data, but maintain different coloured dots for each gender? 









# Answer - move the gender colour specification to within the geom_point argument
ggplot(data = linelist,
       mapping = aes(         # map aesthetics to columns
         x = age,
         y = wt_kg)) + 
  geom_point(                 # add points for each row of data
    aes(colour = gender),        # map colour to gender (no longer global, ie., within ggplot call)
    alpha = 0.5,
    size = 1) +  
  geom_smooth(                # add a trend line (smoothed cond. mean)
    method = "lm",               
    linewidth = 1.5,              
    colour = "grey28")            # change colour of line (static)            


# FACET PLOTS AND LABELS ------------------------------
# Split one plot into many by subgroup
# New plot! Epidemic curves using histograms

## One variable: facet_wrap ----------
# Note: You may need to make your RSTudio window bigger, especially the bottom right window, to see the figures

linelist %>% 
  filter(hospital %in% c("Central Hospital", "Military Hospital", "Port Hospital","St. Mark's Maternity Hospital (SMMH)")) %>%    # filter out 'missing' and 'other'  hospitals first
  ggplot(aes(x = date_onset))+       # onset date on x-axis. note you do not need to specify the y as we will be using histogram (y = # observations)
  geom_histogram(                    # histogram
    binwidth = 7,                      # weekly
    fill = "darkcyan",                # all one fill colour
    colour = "white") +                # all one outline colour for all bars
  labs(                              # add labels
    x = "Date of symptom onset",       # x-axis       
    y = "Number of cases",             # y-axis
    title = "Epidemic curves by hospital",  # plot title
    subtitle = "Fictional Ebola outbreak, 2014") + # plot subtitle
  facet_wrap(~ hospital)              # facet data based on values in the hospital variable 


# What do you think 'facet_wrap(~ hospital, scales = "free")' might do? Hint: what appears to be the default behaviour for the y-axis in facet_wrap? 

# What do you think 'facet_grid(hospital ~ outcome)' will produce?










# Answers

# default is to match them all, free allows each axis to scale according to the data within that facet. Note that you can also do "free_y", and "free_x" and "free_all"


## Two variables -----------
## Note: drop grid if do group activity (unless want extra time!)

linelist %>% 
  filter(hospital %in% c("Central Hospital", "Military Hospital", "Port Hospital","St. Mark's Maternity Hospital (SMMH)")) %>%    
  ggplot(aes(x = date_onset, fill = hospital))+       # fill based on hospital instead
  geom_histogram(                    
    binwidth = 7,                      
    colour = "white") +                
  labs(                              
    x = "Date of symptom onset",           
    y = "Number of cases",             
    title = "Epidemic curves by hospital",  
    subtitle = "Fictional Ebola outbreak, 2014") +
  facet_grid(hospital ~ outcome, scales = "free")  # facet by hospital AND outcome, and allow both axes to be sized based on data ranges within those intersections                  


# EXPORTING PLOTS ------------

# We will use ggsave() from ggplot2, and the here() package

# Just like anything in R, plots can be saved as objects. You can then save the plot using the named object, OR, you can save it without specifying, in which case it will save the last plot open in your plots window. 

# 1. Save last plot that was printed ---
ggsave(here("output", "plots", "epicurve_facet.png"))  # change to your home location
# You can change the file type by changing the file extension. 

## You can also modify the dimensions and resolution:
  ggsave(here("output", "plots", "epicurve_facet.png"), height = 5, width = 5, dpi = 300)

# 2. Save a named plot object ---

## Create named object
  p1 <- ggplot(data = linelist, aes(x = age, y = wt_kg)) +
    geom_point()  

# print
  p1

## Add another layer
  p2 <- p1 + geom_vline(xintercept = mean(linelist$age, na.rm = T), colour = "blue")  # plot mean age as a vertical blue line
  p2

## Save
  ggsave(p2, here("output", "age_wt_scatter.png")) 

# Go take a look at your saved plots! Note that they will look slightly different than within R due to scaling and resolution. 

