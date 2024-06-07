##############################################################
# Program Evaluation- Fig 2. School Dsitrict Breakdown                    
# Authors: Asha Muralidharan     
# GitHub: asha-ec                           
# Last revised: 2024-01-27  
# Summary: A document that analyzes the data
#          of SHP students' breakdown by districts to create
#          Fig 2. School District Breakdown
##############################################################

##############################################################
# Library Intros and Importing Excel Sheet                               
##############################################################

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)

shpdemo <- read_excel("SHP 2023-24 student demographics.xlsx")

##############################################################
# School District Data Wrangling                             
##############################################################

# The following code recategorized the counties of students going to schools outside of Charlottesville and Albemarle
# into one large 'Other' category. The following code has been scrubbed of county and school names. 
shpdemo_other <- shpdemo %>% mutate(`School District`= ifelse(`School District`=="County A","Other",`School District`))
shpdemo_other <- shpdemo_other %>% mutate(`School District`= ifelse(`School District`=="County B","Other",`School District`))
shpdemo_other <- shpdemo_other %>% mutate(`School District`= ifelse(`School District`=="School C","Other",`School District`))
shpdemo_other <- shpdemo_other %>% mutate(`School District`= ifelse(`School District`=="County D","Other",`School District`))
shpdemo_other <- shpdemo_other %>% mutate(`School District`= ifelse(`School District`=="County E","Other",`School District`))
shpdemo_other <- shpdemo_other %>% mutate(`School District`= ifelse(`School District`=="County F","Other",`School District`))

# Replace Acronyms with Full Names of Schools
shpdemo_other <- shpdemo_other %>% mutate(`School District`= ifelse(`School District`=="ACPS","Albemarle County Public Schools",`School District`))
shpdemo_other <- shpdemo_other %>% mutate(`School District`= ifelse(`School District`=="CCS","Charlottesville City Schools",`School District`))

# Create summary with counts for School District Categories to input into chart 
district_summ <- shpdemo_other %>% group_by(`School District`) %>% summarise(count = n())

##############################################################
# School District Chart                         
##############################################################

#Make bar plot of district plot 
ggplot(district_summ, aes(y=count, x=reorder(`School District`,-count))) + 
  geom_bar(position="dodge", stat="identity", fill="#0C9ED9") +
  labs(y="Number of Students",x="School District", title="School District Breakdown of Starr Hill Pathways Students",
       subtitle="2023-24 Academic Year") +
  theme_minimal()+
  geom_text(aes(label = count), position = position_dodge(.9), vjust=-.5) +
  scale_y_continuous(name="Number of Students", breaks=seq(0,150,20),limits=c(0,140))

