#################################################################
# Title: Data Tidying - O2 Project
# Purpose: Creating master dataset for O2 project from Sitka NSF Master
# Written by : L. Pandori & S. Lira
# Created: 4/9/2020
# Last edited: 4/9/2020
################################################################\

##### Packages and set-up #####

# Clear workspace
rm(lists=ls())

# Libraries
library(tidyverse) # tidy, visualize, model
library(RColorBrewer) # nice colors
library(lubridate) # dealing with dates

##### Data import and tidying #####

##### Pool characteristics #####
pooldata <- read_csv("SitkaNSF_PoolData_04092020.csv")

  # Get rid of extra rows (37-999) and columns (10-27)
  pooldata <- pooldata[-c(37:999),-c(10:27)]
    
  # Filter to contorl pools only
  pooldata <- filter(pooldata, Treatment == 'Control')
##### -- #####
  
# Productivity data (light, dark, recovery experiment results)
prodata <- read_csv("SitkaNSF_ProductivityLightDark_04092020.csv", 
          col_types = cols(Date = col_date(format = "%m/%d/%Y")))
  
  # Get rid of strange column X32
  prodata <- prodata[,-c(32)]

  # Select pools of interest
    # Get list of control pools from pool data (7,10,25,30,31)
    unique(pooldata$Pool)
    # Filter dataset for control pools
    prodata <- filter(prodata, Pool %in% unique(pooldata$Pool))
  
  # Select dates of interest (2019 only)
    # make year column
    prodata$year <- year(prodata$Date)
    # Filter only 2019 data
    prodata <- filter(prodata, year == '2019')
    
# Water chemsitry data ()
chemdata <- read_csv("SitkaNSF_WaterChemData_04092020.csv")