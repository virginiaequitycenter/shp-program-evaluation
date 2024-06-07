##############################################################
# 2023-24 Program Evaluation: Fig. 5- Wellness Surveys Cleaning and Analyzing 
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2024-02-19     
# Summary: Cleaning Wellness Surveys given to Starr-Hill 
#          Pathways' students in Check-ins in Fall of 2023
##############################################################

##############################################################
# Library Intros and Excel Imports                                
##############################################################

library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(labelled)
library(ggplot2)

##############################################################
# Call in Week 1 and Week 2 Check-In Data                            
##############################################################

week1 <- read_csv("Summer SHP Week 1 Check-in.csv")

colnames(week1) <- week1[1,]
week1 <- week1[-1,]

week1$`Your Name` <- week1$`Your Name` %>% replace_na('Anonymous')
week1$`What was the best part of this past week at Starr Hill Pathways for you?` <- week1$`What was the best part of this past week at Starr Hill Pathways for you?` %>% replace_na('No Comment')


week2 <- read_csv("Summer SHP Week 2 Check-in.csv")

colnames(week2) <- week2[1,]
week2 <- week2[-1,]

##############################################################
# Save Clean Versions                         
##############################################################

write_excel_csv(week1,"Summer SHP Week 1 Check-in- Clean_No NAs.csv")
write_excel_csv(week2,"Summer SHP Week 2 Check-in- Clean_No NAs.csv")

##############################################################
# Create Summary Data Frame                      
##############################################################

# Reading in clean versions of the data sets
week1 <- read_csv("Summer SHP Week 1 Check-in- Clean_No NAs.csv")
week2 <- read_csv("Summer SHP Week 2 Check-in- Clean_No NAs.csv")

# Change the names of columns
colnames(week1) <- c("Name","Feeling","Emotion","Excited","Angry","Happy","Best")
colnames(week2) <- c("Name","Feeling","Emotion","Worried","Safe","Best")

# Choose specific questions that we are interested in 
week1 <- select(week1, c("Excited","Angry","Happy"))
week2 <- select(week2, c("Worried","Safe"))

week1 <- week1 %>% mutate_all(as.factor)
week2 <- week2 %>% mutate_all(as.factor)

# Wrangle data into frame that works for chart
summary <- data.frame(Emotion = c("Excited","Angry","Happy","Worried","Safe"),
                      students = c(length(which(week1$Excited=="Almost Always"|week1$Excited=="Frequently")),
                                 length(which(week1$Angry=="Almost Always"|week1$Angry=="Frequently")),
                                 length(which(week1$Happy=="Almost Always"|week1$Happy=="Frequently")),
                                 length(which(week2$Worried=="Almost Always"|week2$Worried=="Frequently")),
                                 length(which(week2$Safe=="Almost Always"|week2$Safe=="Frequently"))))

# Create Summary Data Frame for Percentages 
mean_summary <- summary %>% add_column(perc_stu=0)
mean_summary <- mean_summary %>% mutate(perc_stu= ifelse(Emotion=="Worried"|Emotion=="Safe",students / 45, perc_stu))
mean_summary <- mean_summary %>% mutate(perc_stu= ifelse(Emotion=="Happy"|Emotion=="Angry"|Emotion=="Excited",students / 76, perc_stu))


##############################################################
# Create Plot(s)                  
##############################################################

# Plot for number of students 
ggplot(summary, aes(x=reorder(Emotion,-students), y=students)) + 
  geom_bar(position="dodge", stat="identity", fill="#F8BE3D") +
  labs(y="Number of Students",x="Emotion", title="Student Wellness Check-ins - Summer 2023",
       subtitle= "While at Starr Hill Pathways, did you frequently feel ______?") +
  theme_minimal()+
  theme(legend.position="none")+
  geom_text(aes(label = students), position = position_dodge(.9), vjust=-.5) +
  scale_y_continuous(name="Number of Students", breaks=seq(0, 60,5),limits=c(0,60))

# Plot for percentage of students 
ggplot(mean_summary, aes(x=reorder(Emotion,-perc_stu), y=perc_stu)) + 
  geom_bar(position="dodge", stat="identity", fill="#F8BE3D") +
  labs(y="Percetage of Students",x="Emotion", title="Student Wellness Check-ins - Summer 2023",
       subtitle= "While at Starr Hill Pathways, did you frequently feel ______?") +
  theme_minimal()+
  theme(legend.position="none")+
  scale_y_continuous(name="Percentage of Student Responses", breaks=seq(0, 1,.1),limits=c(0,1))
