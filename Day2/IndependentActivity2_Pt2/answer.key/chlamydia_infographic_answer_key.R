#####################################################
##
##  Title: Do no harm data visualization
##  Author: Nevena Veljanovic
##  Created: April 28, 2023
##  Last Edited: April 28, 2023
##  Last edited by: 
##
##  Description: This answer key provides code to create
##  new data visualizations to investigate some
##  of the relationships and conclusions presented in the 
##  chlamydia infographic.
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
mockdata <- rio::import(here("Infographic", "data", "chlamydia_linelist.csv"), 
                        header = TRUE)

str(mockdata)

### Data cleaning

mockdata <- mockdata %>%
  mutate(reporteddate_new=  as.Date(reporteddate_new, format = "%Y/%m/%d")) %>% # Ensures reported date is properly coded
  mutate(partners_coded = as.numeric(partners_coded)) # Ensures you can group cases by the number of reported partners

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

##############################################################################
### Creating new data visualizations for infographic
###############################################################################

### First data visualization: epi curve
## Create two different epi curves based on the variable sex

# 7-day incidence for female cases

mockdata_female <- mockdata %>% filter(sex == "F") # Filters out male cases

i.7.f <- incidence(mockdata_female$reporteddate_new, interval = 7, groups = mockdata_female$age_group, standard = FALSE) # Creates incidence object

plot(i.7.f, show_cases = TRUE, border = "white", labels_week = TRUE) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 25, colour ="#1B304A", hjust = 0.5),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20)) + # Customizes theme
  scale_fill_manual(values = c("#00CFFF", "#007C99", "#969696", "#31445C", "1B304A")) + 
  scale_y_continuous(breaks = seq(0, 20 , 2)) + # Specifies x axis
  
  labs( 
    title = "Female cases of chlamydia by week of case report",
    x = "Week of case report", y = "Case count", fill = "Age groups") +
  coord_fixed(ratio = 7) 

ggsave(here("Infographic", "output", "epi_curve_female.png"), # Saves figure in output folder
       width = 11.5, height = 5) # in following dimensions

# 7-day incidence for male cases
mockdata_male <- mockdata %>% filter(sex == "M") # Filters out female cases

i.7.m <- incidence(mockdata_male$reporteddate_new, interval = 7, groups = mockdata_male$age_group, standard = FALSE) # Creates incidence object

plot(i.7.m, show_cases = TRUE, border = "white", labels_week = TRUE) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 25, colour ="#1B304A", hjust = 0.5),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25)) + # Customizes theme
  scale_fill_manual(values = c("#007C99", "#969696", "#31445C", "1B304A")) + 
  scale_y_continuous(breaks = seq(0, 20 , 2)) + # Specifies x axis
  
  labs( 
    title = "Male cases of chlamydia by week of case report",
    x = "Week of case report", y = "Case count", fill = "Age groups") +
  coord_fixed(ratio = 7) 

ggsave(here("Infographic", "output", "epi_curve_male.png"), # Saves figure in output folder
       width =11.5, height = 5) # Specifies size


### Second data visualization: multiple partners pie charts

# Manipulate data to include NAs as "Missing" values in pie charts

str(mockdata$partner_group)

mockdata$partner_group <- as.character(mockdata$partner_group) # Factors don't store NAs as a unique level so recode variable as character
mockdata$partner_group[is.na(mockdata$partner_group)] <- "Missing" # Add "Missing" value to NAs

## For female cases
# Create a subset of data for female cases grouped by how many sexual partners they've reported having
pie_female <- mockdata %>% 
  filter(sex== "F")%>% 
  group_by(partner_group) %>%
  count()

# Create pie chart
ggplot(pie_female,
       
       aes(x = "", y = n, fill = partner_group)) + # Uses counts of cases grouped by number of reported sexual partners
  
  geom_bar(stat= "identity", width = 1, color = "white") + 
  geom_text(aes(label = n), color = "white", size = 10,
            position = position_stack(vjust = 0.5)) +
  
  coord_polar("y", start = 0) +
  
  labs(
    title = "Female cases grouped by number of sexual partners",
    fill = "Number of sexual partners"
  ) +
  
  theme_void() + 
  theme(plot.title = element_text(face = "bold", size = 30, colour ="#1B304A", hjust = 0.5), # Customizes theme
        legend.position = "bottom",
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30))  + 
  scale_fill_manual(values = c( "#969696", "#1B304A", "#005266")) 


ggsave(here("Infographic", "output", "female_cases_partners_NAs.png"), # Saves figure in output folder
       width = 11.5, height = 13) # Specifies size

## For male cases
# Create a subset of data for male cases grouped by how many sexual partners they've reported having
pie_male <- mockdata %>% 
  filter(sex== "M")%>% 
  group_by(partner_group) %>%
  count()

# Create pie chart
ggplot(pie_male,
       
       aes(x = "", y = n, fill = partner_group)) + # Uses counts of cases grouped by number of reported sexual partners
  
  geom_bar(stat= "identity", width = 1, color = "white") + 
  geom_text(aes(label = n), color = "white", size = 10,
            position = position_stack(vjust = 0.5)) + 
  
  coord_polar("y", start = 0) +
  
  labs(
    title = "Male cases grouped by number of sexual partners",
    fill = "Number of sexual partners"
  ) +
  
  theme_void() + 
  theme(plot.title = element_text(face = "bold", size = 30, colour ="#1B304A", hjust = 0.5), # Customizes theme
        legend.position = "bottom",
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30))  + 
  
  scale_fill_manual(values = c("#969696", "#1B304A" , "#007C99", "#005266" ))


ggsave(here("Infographic", "output", "male_cases_partners_NAs.png"), # Saves figure in output folder
       width = 11.5, height = 13) # Specifies size

