# Cohort Analysis

### This is a brief application of cohort analysis using the 'cohorts' package
### in R, created by (Christensen, 2022).

## Loading the data base into R

library(readxl)
Online_Retail <- read_excel("Online Retail.xlsx")
View(Online_Retail)

##Installing and loading packages to be used in this example

install.packages("cohorts")
install.packages("ggplot2")
library(ggplot2)
library(stringr)
library(tidyverse)
library(tidyr)
library(cohorts)
library(dplyr)

View(online_cohorts)

## Recreating the 'online_cohorts' table with code.

keeps <- c("CustomerID", "InvoiceDate")
clean_data_cohorts <- Online_Retail[keeps]

head(clean_data_cohorts) %>% 
  mutate(CustomerID=abbreviate(CustomerID))

## Creating a cohorts table with the 'cohort_table_month' function.

c_table1 <- clean_data_cohorts %>% 
  cohort_table_month(id_var = CustomerID, date = InvoiceDate)
View(c_table1)

## Creating a cohorts table with percentages.

c_table_percentages <- clean_data_cohorts %>% 
  cohort_table_month(id_var = CustomerID, date = InvoiceDate) %>%
  cohort_table_pct()

View(c_table_percentages)

## Making the data plot friendly

long_data_cohorts <- clean_data_cohorts %>% 
  cohort_table_day(id_var = CustomerID, date = InvoiceDate) %>%
  pivot_longer(-cohort)%>%
  mutate(time = as.numeric(str_remove(name,"t")))
  
long_data_cohorts[long_data_cohorts == 0] <- NA

## Plotting cohorts

long_data_cohorts %>%
  filter(cohort <= 7, value > 0) %>%
  ggplot(aes(time, value, colour = factor(cohort), group = cohort)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  theme_minimal()