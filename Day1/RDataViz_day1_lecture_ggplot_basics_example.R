########################################################################################
#Purpose: ggplot example to use in lecture for day 1 of data visualization in R training.
#         Shared here for course participants to follow along (optional).
#Author: Applied Epi
#Source: Epidemiologist R Handbook, Ch. 30 - ggplot basics
#        https://www.epirhandbook.com/en/ggplot-basics.html
#Last modified: 2023-04-11
#Last modified by: J. Stares
#Modifications: Very minor, only to highlight examples from Epi R Handbook for TDU training.
########################################################################################


# load library packages

pacman::p_load(
  rio,            # import/export
  here,           # file pathways
  flextable,      # make HTML tables 
  officer,        # helper functions for tables
  stringr,        # working with characters  
  tidyverse      # data management, summary, and visualization
)

# import the data
# importing data directly from where it is stored online
# alternatively, download using the link in the statement below and import from local computer 
x <- "https://github.com/epirhandbook/Epi_R_handbook/raw/master/data/case_linelists/linelist_cleaned.rds"

linelist <- import(x)

# prepare the data
# make display version of columns with more friendly names
linelist <- linelist %>%
  mutate(
    gender_disp = case_when(gender == "m" ~ "Male",        # m to Male 
                            gender == "f" ~ "Female",      # f to Female,
                            is.na(gender) ~ "Unknown"),    # NA to Unknown
    outcome_disp = replace_na(outcome, "Unknown")          # replace NA outcome with "unknown"
  )

#in the ggplot() statement, the data are set as the case linelist
#in the mapping = aes() argument the column age is mapped to the x-axis, and the column wt_kg is mapped to the y-axis
#a shape is created with the “geom” function geom_point(). 
#This geom inherits the mappings from the ggplot() command 
#It knows the axis-column assignments and proceeds to visualize those relationships as points on the canvas.

# working with a point plot
ggplot(data = linelist, aes(x = age, y = wt_kg)) +
  geom_point()

# aesthetics - scaled column values and static values
# this example maps aethetics (point shape and transparency) to static values outside aes()
# it is helpful to keep this capacity in mind particularly for plots multiple geometric layers 
# e.g., do you want everything in the figure to be set to 30% transparency, or just one specific component?
ggplot(data = linelist,   # set data
       mapping = aes(     # map aesthetics to column values
         x = age,           # map x-axis to age            
         y = wt_kg,         # map y-axis to weight
         color = age,       # map color to age
         size = age))+      # map size to age
  
  geom_point(             # display data as points
    shape = "diamond",      # points display as diamonds 
    alpha = 0.5)            # point transparency at 50% 

# figures with multiple geometric layers
# note some aesthetic mappings (size) have moved from aes() to geom 
ggplot(data = linelist,
       mapping = aes(           # map aesthetics to columns
         x = age,
         y = wt_kg,
         color = age)) + 
  
  geom_point(                   # add points for each row of data
    shape = "diamond",
    alpha = 0.5,
    size = 1) +  
  
  geom_smooth(                  # add a trend line 
    method = "lm",              # with linear method
    size = 2)                   # size (width of line) of 2

# plot with facets - epidemic curves by hospital (written to match the data and code provided)
hospitals <- c("Central Hospital", "Military Hospital", "Port Hospital","St. Mark's Maternity Hospital (SMMH)")
hospital_data <- linelist %>% filter(hospital %in% hospitals)


ggplot(hospital_data, aes(x = date_onset)) +
  geom_histogram(binwidth = 7, 
                 colour = "black",
                 fill = "darkred")+
  theme_minimal()+                              # simplify the background panels
  labs(                                         # add plot x- and y-axis labels, title, etc.
    x = "Date of symptom onset",
    y = "Number of cases",
    title = "Epidemic curves by hospital") +
  facet_wrap(~hospital)                       # the facets are created on the values in the hospital variable
