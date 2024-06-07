##############################################################
# Program Evaluation- Fig 7/8. Educational Aspirations Cleaning Document                      
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2024-01-13    
# Summary: A document that cleans the data collected in Fall 2023
#          from survey of SHP students on their educational aspirations-
#          how far they WANT to go in school vs how far they
#          EXPECT to go in school
##############################################################

##############################################################
# Library Intros and Importing Excel Sheet                               
##############################################################

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)

edasp <- read_excel("SHP Ed Aspirations 23-24 .xlsx")

#############################################################
# Removing Students who did not Complete Survey                            
#############################################################

edasp_complete <- edasp %>% drop_na("Educational Aspirations - LIKE")

#############################################################
# Factorize Educational Aspirations Variables and Change Column Names                         
#############################################################

edasp_complete <- edasp_complete %>% mutate(`Educational Aspirations - LIKE` = ifelse(`First Name`=="Déyard",4,`Educational Aspirations - LIKE`))
edasp_complete <- edasp_complete %>% mutate(`Educational Aspirations - LIKE` = ifelse(`First Name`=="Cipriano",5,`Educational Aspirations - LIKE`))

edasp_complete <- edasp_complete %>% mutate(as.numeric(`Educational Aspirations - LIKE`))
edasp_complete <- edasp_complete %>% mutate(as.numeric(`Educational Aspirations - THINK`))


edasp_complete$`Educational Aspirations - LIKE` = factor(edasp_complete$`Educational Aspirations - LIKE`, 
                        labels= c("High School Graduation Only",
                                  "Less Than 2 years of college,vocational, or business school",
                                   "Two or more years of college including a 2-year degree",
                                   "Graduate College (4- or 5-year degree)",
                                   "Master’s Degree or Equivalent",
                                   "Ph.D., M.D., or another Professional Degree"))

edasp_complete$`Educational Aspirations - THINK` = factor(edasp_complete$`Educational Aspirations - THINK`, 
                                                         labels= c("Less than High School Graduation","High School Graduation Only",
                                                                   "Less Than 2 years of college,vocational, or business school",
                                                                   "Two or more years of college including a 2-year degree",
                                                                   "Graduate College (4- or 5-year degree)",
                                                                   "Master’s Degree or Equivalent",
                                                                   "Ph.D., M.D., or another Professional Degree"))

colnames(edasp_complete) <- c("First_Name","Last_Name", "Ed_Aspirations_Like","Ed_Aspirations_Think","Influence_1","Influence_2","Ed_Asp_Like_Num","Ed_Asp_Think_Num")

#############################################################
# Add Difference in Expectations Column:
#   Positive: Student Expects to complete more Schooling than they would like to
#   Zero: Student Expects to complete as much schooling as they would like to 
#   Negative: Students Expects to complete less schooling than they would like to 
#############################################################

edasp_complete <- edasp_complete %>% add_column(Difference = edasp_complete$Ed_Asp_Think_Num - edasp_complete$Ed_Asp_Like_Num)


write_excel_csv(edasp_complete,"Educational Aspirations Fall 2023- Clean_No NAs.csv")

