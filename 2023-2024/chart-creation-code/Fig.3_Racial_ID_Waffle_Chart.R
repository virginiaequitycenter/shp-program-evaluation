##############################################################
# Program Evaluation- Fig 3. Race/Ethnicity Student Demographic Waffle Chart                    
# Authors: Asha Muralidharan     
# GitHub: asha-ec                           
# Last revised: 2024-01-27  
# Summary: A document that analyzes the data
#          of SHP students' breakdown by districts to create
#          Fig 3. Racial Self Identification
##############################################################

##############################################################
# Library Intros and Importing Excel Sheet                               
##############################################################

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(waffle)
library(ggvenn)

shpdemo <- read_excel("SHP 2023-24 student demographics.xlsx")

# If self-identified Race or Ethnicity was not given, replace with VDOE category that was indicated by student or caregiver

shpdemo <- shpdemo %>% mutate(`Race/Ethnicity` = ifelse(`VDOE Race`=="Black" & is.na(`Race/Ethnicity`),"Black",`Race/Ethnicity`))
shpdemo <- shpdemo %>% mutate(`Race/Ethnicity` = ifelse(`VDOE Race`=="Latinx" & is.na(`Race/Ethnicity`),"Latinx",`Race/Ethnicity`))
shpdemo <- shpdemo %>% mutate(`Race/Ethnicity` = ifelse(`VDOE Race`=="White" & is.na(`Race/Ethnicity`),"White",`Race/Ethnicity`))

##############################################################
# School Demographics Chart                    
##############################################################

# Summarize SHP demographics by Race and Ethnicity 
shpdemo %>% count(`Race/Ethnicity`, `VDOE Race`) -> shp_race_count

  
# Create Chart
ggplot(
  data = shp_race_count, 
  aes(fill = `Race/Ethnicity`, values = n)
) +
  geom_waffle(
    color = "white", 
    n_rows = 5, 
    flip = TRUE
  ) +
  facet_wrap( ~`VDOE Race`,nrow = 1,strip.position = "bottom"
  )+
  theme_void()+
  theme(legend.position = "bottom")+
  labs(title = "Starr Hill Pathways Summer 2023 Students:
Self-Reported Race and Ethnicity by VDOE Ethnicity Categories")

# Chart is cleaned up in Illustrator, where different colors are chosen to create
# gradients within each VDOE Race category

