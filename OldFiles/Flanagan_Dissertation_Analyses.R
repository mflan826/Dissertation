#load packages
library(lavaan)
library(readxl)
library(tidyverse)

#get the TDQI data
d1 <- read_excel("Allentown.xlsx")
d2 <- read_excel("Avon_Grove.xlsx")
d3 <- read_excel("Butler.xlsx")
d4 <- read_excel("Canon_McMillan.xlsx")
d5 <- read_excel("East_Lycoming.xlsx")
d6 <- read_excel("Lebanon.xlsx")
d7 <- read_excel("Mountain_View.xlsx")
d8 <- read_excel("PA_Virtual_Charter.xlsx")
d9 <- read_excel("Plum_Borough.xlsx")
d10 <- read_excel("Southwestern.xlsx")
d11 <- read_excel("Union_Area.xlsx")

#make a column index for reference, change df as needed to update / modify the index list
column_index <- data.frame(colnames(d1Y))

#Allentown

#whole sheet clean
#Rename role, age, gender, school, race
d1 <- d1 %>% rename(Role = Q1) %>% 
  rename(Age = Q2) %>% rename(Gender = Q3) %>% 
  rename(School = Q5) %>% rename(Race_Eth = Q4)

#remove first row acting as question name which was added by Qualtrics out-dated export functionality.
d1 <- d1[-c(1),]


#Clean by role - Youth
d1Y <- d1 %>% filter(Role =="I'm a Young Person") %>% 
  filter(Finished=="True") %>% select(-c(77:698)) %>%
  select(-c(85:95)) %>% select(-c(81:84)) %>% 
  select(-c(75:76)) %>% select (-c(1:10)) %>% 
  mutate(Q773 = str_replace(Q773,"Some\r\n","Some")) %>% 
  mutate(Q773 = str_replace(Q773,"A lot\r\n","A lot")) %>%
  mutate(Q773 = str_replace(Q773,"Not at al\r\n","Not at all")) %>%
  select(-c(6:10)) %>% 
  mutate_at(vars(6:59),
            ~as.numeric(recode(.,
                               "A lot"=3,
                               "Some"=2,
                               "Not at all"=1,
            ))) %>% 
  #RENAME SQIs
  colnames(d1Y)[6:59] <- c(paste0("SQI",1:54))



d1F <- d1 %>% filter(Q1 =="I'm a Parent / Legal Guardian")
d1S <- d1 %>% filter(Q1 =="I'm a Transition Stakeholder")
