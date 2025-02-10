#####################################################
##
##  Title: Data Visualization Beautification
##  Author: Marc-André Bélair (I'm so sorry!)
##  Created: March 27, 2023
##  Revised by: J. Stares
##  Last Edited: 2025-01-28 
##
##  Description: This exercise reads in a datafile
##    cleans it up for the purposes of the exercise
##    and then outputs a truly heinous visualization.
##
##  Instructions: After the section indicated 
##    INSERT CODE HERE, use your data visualization
##    skills to create a graph that is more pleasing.
##
#####################################################
#install pacman if you haven't already 
#install.packages("pacman")

##Uses the package manager to load required packages.
pacman::p_load(
  rio,  # for importing data
  here, # for file paths
  janitor, # for data cleaning
  lubridate, # because dates are messy
  tidyverse, # to make life so much easier
  ggplot2 # the graphics of visualization.
)

### These commands import fonts from Windows.
windowsFonts(ComicSans=windowsFont("Comic Sans MS"))
windowsFonts(Papyrus=windowsFont("Papyrus"))
windowsFonts(Joker=windowsFont("Jokerman"))
windowsFonts(Times=windowsFont("TT Times New Roman"))

### Imports the raw dataset into linelist
linelist <- import(here("IndependentActivity2_Pt1", "data", "bchips_dataset_2022.csv"), header = TRUE)


# Create a custom theme for the visualization. 
# Refer to Day 2 slides for the basics on theme() usage, and 
# refer to the help documentation on theme() for additional details. 
theme_goodbye_retinas <- function(){
theme(plot.title  = element_text(family = "ComicSans", color = "red", size = 40),
      plot.subtitle = element_text(family = "Papyrus", color = "red"),
      plot.caption = element_text(family = "Joker", color = "darkblue"),
      panel.background = element_rect(fill = "purple"),
      plot.background = element_rect(fill = "green"))
}


###The Data Visualization
linelist %>% 
  ggplot() +  # Invokes GGPlot, passing it linelist

    aes(x = Date) + #Sets the Date column as the X axis.
    
    #adds obs_min and obs_max as the response variable on the y-axis and factors colours by zoneid
    geom_line(aes(y = obs_min, colour=factor(zone_id))) + 
    geom_line(aes(y = obs_max, colour=factor(zone_id))) + 
    
    #Sets the text of all the different text elements currently on screen.
    labs(
      title = "min and max temps in bc in zones", 
      x = "date", y = "temp",
      subtitle = unique(paste0('Published ',as.Date(Sys.Date()))),
      caption = "Data Source: BCCDC Health Impact Prediction System
                 Information presented is as-is and for training purposes only.
                  DO NOT DISTRIBUTE."
    ) + 
  
  theme_goodbye_retinas()

##############################
## NOTE: SEE PARTICIPANTS GUIDE FOR FULL INSTRUCTIONS 
## 1 - Think about what this data is trying to show. How can you manipulate the data to 
##     only show what is necessary.  (Hint: try picking only one zone!)
## 2 - Clean the data set to remove any unnecessary variables. A data dictionary is included in your data folder.
##     If you want to integrate additional data, by all means! Be creative, but don't overdo it.
## 3 - Create your own ggplot statement to generate a beautiful creation that applies data visualization best practices.
##
## If the coding is too challenging, feel free to write pseudocode to outline what your process will be!
#############################

###
### INSERT CODE HERE

###
###

