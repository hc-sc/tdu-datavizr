#####################################################
##
##  Title: Data Visualization Beautification (answer key)
##  Author: Nevena Veljanovic
##  Created: Apri 14, 2023
##  Last Edited: April 24, 2023
##  Last edited by: Benjamin Hetman
##
##  Description: This answer key provides options
##  to modify the data visualization resulting from
##  the data visualization beautification exercise to 
##  follow best practices.
##
#####################################################

##############################################################################
##
## Initial Setup
##
##############################################################################

# Clear environment variables (note that its better to do this with the button)
rm(list = ls())

### Uses the package manager to load required packages.
pacman::p_load(
  rio,  # for importing data
  here, # for file paths
  janitor, # for data cleaning
  lubridate, # because dates are messy
  tidyverse, # to make life so much easier
  ggplot2,# the graphics of visualization.
  gghighlight # to highlight data in ggplot
)


### Import the raw dataset into the object 'linelist'
linelist <- rio::import(here("De-Ugly", "data", "bchips_dataset_2022.csv"))

### Remove unnecessary variables

  ## Delete a variable with a specific name
  linelist <- linelist %>% select(-obs_risk)

  ## Delete multiples columns whose names start with "fc"
  linelist <- linelist %>% select(-starts_with('fc'))
  view(linelist)

  
###############################################################################
### Creating new data visualizations that align with data visualizations best practices
###############################################################################  

  
### Example 1: Create an individual visualization for each zone (example here for zone = 1)   
    
  colors <- c("Minimum" = "darkblue", "Maximum" = "darkred") #Sets colors in a vector for legend creation
  
  linelist %>% filter(zone_id==1) %>%   #Selects the zone of interest
    ggplot() +  #Invokes GGPlot, passing the linelist object to it as the data
    
    
    theme_classic() + #Start with the "classic" built-in theme
    
    theme(plot.title = element_text(size=14), #Customize the theme to improve accessibility
          
          legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 11)) +
    
    guides(colour = guide_legend(override.aes = list(size=2))) + #Thickens the lines in the legend
    
    aes(x = Date) + #Sets the Date column as the X axis
    
    #Adds obs_min and obs_max as the response variable on the y-axis and modify the default color and size
    geom_line(aes(y = obs_min, colour = "Minimum"), size= 0.8) + 
    geom_line(aes(y = obs_max, colour = "Maximum"), size= 0.8) + 
    
    #Sets the text of all the different text elements currently on screen
    labs(
      title = "Temperature variation from January 2022 to January 2023 in Abbotsford, British Columbia", 
      x = "Date", y = "Temperature (in degrees Celsius)",
      subtitle = unique(paste0('Published ',as.Date(Sys.Date()))),
      caption = "Data Source: BCCDC Health Impact Prediction System
                 Information presented is as-is and for training purposes only.
                  Do not distribute.",
      color = "Observed temperature") +
    
    scale_color_manual(values = colors) #Adds a legend manually
  

  
  
### Example 2: Create a multi-panel data visualization showing zones of interest
  linelist %>% filter(zone_location_name %in% c(	
    'Abbotsford','Blue River','Burns Lake','Clinton')) %>%   #Selects the zones of interest using location names instead of ID numbers
    ggplot() +  #Invokes GGPlot, passing it the linelist
    
    theme_minimal() + #Selects the "minimal" pre-existing theme
    
    theme(strip.text = element_text(size = 12), #Customizes the theme to improve accessibility
          
          legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 11)) +
    
    guides(colour = guide_legend(override.aes = list(size=2))) + #Thickens the lines in the legend
    
    aes(x = Date) + #Sets the Date column as the X axis
    
    #Adds obs_min and obs_max as the response variable on the y-axis and modify the default color and size
    geom_line(aes(y = obs_min, colour = "Minimum"), size= 0.7) + 
    geom_line(aes(y = obs_max, colour = "Maximum"), size = 0.7) + 
    
    #Sets the text of all the different text elements currently on screen
    labs(
      title = "Temperature variation from January 2022 to January 2023 in Abbotsford, Blue River, Burns Lake, and Clinton, British Columbia", 
      x = "Date", y = "Temperature (in degrees Celsius)",
      subtitle = unique(paste0('Published ',as.Date(Sys.Date()))),
      caption = "Data Source: BCCDC Health Impact Prediction System
                 Information presented is as-is and for training purposes only.
                  Do not distribute.",
      color = "Observed temperature") +
    
    scale_color_manual(values = colors) + #Adds a legend manually
    
    facet_wrap(~zone_location_name)  #Creates a multi-panel figure
  
  

### Example 3: Create one data visualization including all zones and highlight single zone of interest
  linelist %>% filter(zone_id %in% c(1,2,3,4)) %>%   #Selects the zones of interest
    ggplot() +  #Invokes GGPlot, passing it linelist data
    
    #Uses a pre-existing, accessible theme if you do not need intricate fonts and colors
    theme_classic() + 
    aes(x = Date) + #Sets the Date column as the X axis
    
    #Adds obs_min and obs_max as the response variable on the y-axis and modify the default color and size
    geom_line(aes(y = obs_min), colour= "darkblue", size= 1) + 
    geom_line(aes(y = obs_max), colour = "darkred", size= 1) + 

    gghighlight(zone_id == 1, label_params = list(linewidth=10)) + #Highlights zone 1
    
    #Sets the text of all the different text elements currently on screen.
    labs(
      title = "Temperature variation from January 2022 to January 2023 highlighted in Abbotsford compared to Blue River, Burns Lake, and Clinton, British Columbia", 
      x = "Date", y = "Temperature (in degrees Celsius)",
      subtitle = unique(paste0('Published ',as.Date(Sys.Date()))),
      caption = "Data Source: BCCDC Health Impact Prediction System
                 Information presented is as-is and for training purposes only.
                  Do not distribute.",
      color = "Legend") +
    
    #Adds customized labels
    geom_label(
      label= "Maximum observed temperature",
      x= as.Date("2022-11-26"),
      y=32,
      label.padding = unit(0.30, "lines"),
      color= "darkred",
      size= 4,
      fontface= "bold",
      fill= "white") +
    
    geom_label(
      label= "Minimum observed temperature",
      x= as.Date("2022-11-26"),
      y=-17,
      label.padding = unit(0.30, "lines"),
      color= "darkblue",
      size= 4,
      fontface= "bold",
      fill= "white")
    
    

        