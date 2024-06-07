##############################################################
# Program Evaluation- Fig 4. Focus of Pathway Curriculum                   
# Authors: Asha Muralidharan     
# GitHub: asha-ec                           
# Last revised: 2024-01-28 
# Summary: A document that analyzes the focus of SHP pathways and creates
#          Fig. 4. Focus of Pathway Curricula of Program Evaluation
##############################################################

##############################################################
# Library Intros and Importing Excel Sheet                               
##############################################################

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)

##############################################################
# Build Focus Chart                             
##############################################################

# Count values read in from program evaluation spreadsheet on pathway focus 
focus <- c("Career or Post-Secondary\nEducation Opportunities","Grade-level\nAcademics",
           "Social-Emotional\nWellness","Fitness/Recreation","Volunteerism","On-Campus\nExperiences",
           "Play")
count <- c(14,8,6,2,2,2,1)

pathway_focus <- data.frame(focus,count)

# Create bar chart 
ggplot(pathway_focus, aes(y=count, x=reorder(`focus`,-count))) + 
  geom_bar(position="dodge", stat="identity", fill="#0C9ED9") +
  labs(x="Focus of Pathway",y="Number of Pathways", title="Starr Hill Pathways Summer 2023 - What did Pathways focus on?") +
  geom_text(aes(label = count), position = position_dodge(.9), vjust=-.5) +
  theme_minimal()+
  scale_y_continuous(name="Number of Pathways", breaks=seq(0,15,2),limits=c(0,15))
