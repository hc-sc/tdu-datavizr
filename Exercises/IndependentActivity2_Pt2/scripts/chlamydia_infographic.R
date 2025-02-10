#####################################################
##
##  Title: Do no harm data visualization
##  Author: Nevena Veljanovic
##  Created: April 26, 2023
##  Last Edited: January 28, 2025
##  Last edited by: J. Stares
##
##  Description: This exercise provides code required
##  to create data visualizations in the chlamydia 
##  outbreak infographic.
##
##  Instructions: Analyze and visualize the associated
##  dataset to confirm recommendations provided by 
##  junior analyst. Provide alternative recommendations
##  if necessary.
##
#####################################################

##############################################################################
##
## Initial Setup
##
##############################################################################

### Uses the package manager to load required packages.
pacman::p_load(
  rio,  # for importing data
  here, # for file paths
  janitor, # for data cleaning
  lubridate, # because dates are messy
  tidyverse, # to make life so much easier
  ggplot2,# the graphics of visualization.
  gghighlight, # to highlight data in ggplot
  janitor, # to clean variable names
  incidence, # for epi curve
  dplyr
)



### Import the raw dataset into the object 'linelist'
mockdata <- rio::import(here("IndependentActivity2_Pt2", "data", 
                             "chlamydia_linelist.csv"), 
                        header = TRUE)

### Remove unnecessary variables
### MABNOTE:  NONE of these variables exist in the data provided to participants, this code is extraneous.
### Commented out, but suggest removing.

#mockdata <- mockdata %>% select(-(prev_chlamydia:partners),
#                                -notes, 
#                                -dupid, 
#                                prev_chlamydia_coded, 
#                                -coinfection_coded, 
#                                -asymptomatic, 
#                                -(msm:clusters))

str(mockdata)

### Data cleaning

mockdata <- mockdata %>%
  mutate(reporteddate_new=  as.Date(reporteddate_new, format = "%Y/%m/%d")) %>% # Ensures reported date is properly coded
  mutate(partners_coded = as.numeric(partners_coded)) # Ensures you can group cases by the number of reported partners

mockdata <- mockdata %>%
  janitor::clean_names() # Cleans variable names

### Create new grouped variables

## Group cases by age categories

mockdata <- mockdata %>%
  mutate(
    age_group = dplyr::case_when(
      age <= 14 ~ "0-14",
      age > 14 & age <= 24 ~ "15-24",
      age > 24 & age <= 34 ~ "25-34",
      age > 34 & age <= 44 ~ "35-44",
      age > 44 & age <= 54 ~ "45-54"),
    
    age_group = factor(age_group, level = c("0-14", "15-24", "25-34", "35-44", "45-54"))
  )

cases_age <- mockdata %>% # Creates frequency table to check if the grouping was done properly
  group_by(age_group) %>%
  count()

## Group cases by number of sexual partners

mockdata <- mockdata %>% 
  mutate(
    partner_group = dplyr::case_when(
      partners_coded <= 1 ~ "0-1",
      partners_coded > 1 & partners_coded <= 7 ~ "2-7",
      partners_coded > 7 ~ "8+"),
    partner_group = factor(partner_group, level = c("0-1", "2-7", "8+"))
  )

###############################################################################
### Creating original data visualizations for infographic
###############################################################################  

### First data visualization: epi curve 

# 7-day incidence:
i.7 <- incidence(mockdata$reporteddate_new, interval = 7, groups = mockdata$age_group) # Creates incidence object

plot(i.7, show_cases = TRUE, border = "white", labels_week = TRUE) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16, colour ="#1B304A", hjust = 0.5)) + # Customizes theme
  scale_fill_manual(values = c("#00CFFF", "#007C99", "#969696", "#31445C", "1B304A")) + 
  scale_y_continuous(breaks = seq(0, 20 , 2)) + # Specifies x axis
  
  labs( 
    title = "Cases of chlamydia by week of case report",
    x = "ISO week of case report", y = "Case count", fill = "Age groups") +
  coord_fixed(ratio = 7)


### Second data visualization: multiple partners pie charts

## For female cases

# Create a subset of data for female cases grouped by how many sexual partners they've reported having
pie_female <- mockdata %>% 
  filter(sex== "F")%>% 
  group_by(partner_group) %>%
  count()

# Create pie chart
ggplot(data = subset(pie_female, !is.na(partner_group)), # Uses a subset of data without NAs
       
       aes(x = "", y = n, fill = partner_group)) + # Uses counts of cases in groups of reported sexual partners
  
       geom_bar(stat= "identity", width = 1, color = "white") + 
       geom_text(aes(label = n), color = "white", size = 6, # Adds and customizes text in the pie
            position = position_stack(vjust = 0.5)) +
  
       coord_polar("y", start = 0) +
  
       labs(
         title = "Females cases grouped by number of sexual partners",
         fill = "Number of sexual partners"
       ) +
  
       theme_void() + 
       theme(plot.title = element_text(face = "bold", size = 16, colour ="#1B304A", hjust = 0.5), # Customizes theme
             legend.position = "bottom",
             legend.title = element_text(size = 12),
             legend.text = element_text(size = 11))  + 
       scale_fill_manual(values = c("#969696", "#1B304A")) 



## For male cases
# Create a subset of data for male cases grouped by how many sexual partners they've reported having
pie_male <- mockdata %>% 
  filter(sex== "M")%>% 
  group_by(partner_group) %>%
  count()


# Create pie chart
ggplot(data = subset(pie_male, !is.na(partner_group)),  # Uses a subset of data without NAs
       
       aes(x = "", y = n, fill = partner_group)) + # Uses counts of cases in groups of reported sexual partners
  
  geom_bar(stat= "identity", width = 1, color = "white") + 
  geom_text(aes(label = n), color = "white", size = 6, # Adds and customizes text in the pie
            position = position_stack(vjust = 0.5)) +
  
  coord_polar("y", start = 0) +
  
  labs(
    title = "Males cases grouped by number of sexual partners",
    fill = "Number of sexual partners"
  ) +
  
  theme_void() + 
  theme(plot.title = element_text(face = "bold", size = 16, colour ="#1B304A", hjust = 0.5), # Customizes theme
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))  + 
  
  scale_fill_manual(values = c("#969696", "#1B304A" , "#007C99")) 


  
