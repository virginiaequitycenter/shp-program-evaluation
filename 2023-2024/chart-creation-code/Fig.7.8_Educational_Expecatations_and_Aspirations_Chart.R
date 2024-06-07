##############################################################
# Program Evaluation- Fig 7 and 8 - Educational Aspirations Graphs                    
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2024-01-13    
# Summary: A document that analyzes the data collected in Fall 2023
#          from survey of SHP students on their educational aspirations-
#          how far they WANT to go in school vs how far they
#          EXPECT to go in school: 
#          Creates Figure 7- Educational Aspirations and 
#          Figure 8- Educational Expectations
##############################################################

##############################################################
# Library Intros and Importing Excel Sheet                               
##############################################################

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)


edasp <- read_csv("Educational Aspirations Fall 2023- Clean_No NAs.csv")

##############################################################
# Building Summary Data Frames for Aspirations and Expectations                              
##############################################################

ed_asp_like <- edasp %>% group_by(Ed_Aspirations_Like) %>% summarise(count = n(),
                                                                     mean_Diff = mean(Difference))

ed_asp_think <- edasp %>% group_by(Ed_Aspirations_Think) %>% summarise(count = n(),
                                                                       mean_Diff = mean(Difference))

# Refactorize Aspiration and Expectations for Summarized Chart
ed_asp_like$Ed_Aspirations_Like <- factor(ed_asp_like$Ed_Aspirations_Like, levels=c("Less than High School Graduation","High School Graduation Only",
                                                                                     "Less Than 2 years of college,vocational, or business school",
                                                                                     "Two or more years of college including a 2-year degree",
                                                                                     "Graduate College (4- or 5-year degree)",
                                                                                     "Master’s Degree or Equivalent",
                                                                                     "Ph.D., M.D., or another Professional Degree"))

ed_asp_think$Ed_Aspirations_Think <- factor(ed_asp_think$Ed_Aspirations_Think, levels=c("Less than High School Graduation","High School Graduation Only",
                                                                                        "Less Than 2 years of college,vocational, or business school",
                                                                                        "Two or more years of college including a 2-year degree",
                                                                                        "Graduate College (4- or 5-year degree)",
                                                                                        "Master’s Degree or Equivalent",
                                                                                        "Ph.D., M.D., or another Professional Degree"))


##############################################################
# Building Summary Charts for Aspirations and Expectations in GGPlot                              
##############################################################

ggplot(ed_asp_like, aes(y=count, x=Ed_Aspirations_Like)) + 
  geom_bar(position="dodge", stat="identity", fill="#0C9ED9") +
  labs(y="Number of Students",x="Educational Attainment Level", title="Educational Aspirations of Starr Hill Pathways Students",
       subtitle="How far would you LIKE to go in school?") +
  theme_minimal()+
  theme(legend.position="none")+
  coord_flip()+
  geom_text(aes(label = count), position = position_dodge(.9), hjust=-.5) +
  scale_x_discrete(limits = rev(levels(ed_asp_like$Ed_Aspirations_Like)))+
  scale_y_continuous(name="Number of Students", breaks=seq(0, 35,5),limits=c(0,30))



ggplot(ed_asp_think, aes(y=count, x=Ed_Aspirations_Think)) + 
  geom_bar(position="dodge", stat="identity", fill="#F8BE3D") +
  labs(y="Number of Students",x="Educational Attainment Level", title="Educational Expectations of Starr Hill Pathways Students",
       subtitle= "Realistically speaking, how far do you THINK you will get in school?") +
  theme_minimal()+
  theme(legend.position="none")+
  coord_flip()+
  geom_text(aes(label = count), position = position_dodge(.9), hjust=-.5) +
  scale_x_discrete(limits = rev(levels(ed_asp_think$Ed_Aspirations_Think)))+
  scale_y_continuous(name="Number of Students", breaks=seq(0, 35,5),limits=c(0,30))



