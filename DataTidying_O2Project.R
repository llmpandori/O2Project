#################################################################
# Title: Data Tidying - O2 Project
# Purpose: Creating master dataset for O2 project from Sitka NSF Master
# Written by : L. Pandori & S. Lira
# Created: 4/9/2020
# Last edited: 4/9/2020
################################################################

##### Packages and set-up #####

# Clear workspace 
rm(lists=ls())

# Libraries
library(tidyverse) # tidy, visualize, model
library(RColorBrewer) # nice colors
library(lubridate) # dealing with dates

##### Data import and tidying #####

# Pool characteristics
pooldata <- read_csv("SitkaNSF_PoolData_04092020.csv")

  # Get rid of extra rows (37-999) and columns (10-27)
  pooldata <- pooldata[-c(37:999),-c(10:27)]

  # Filter to contorl pools only
  pooldata <- filter(pooldata, # data frame
                     Treatment == 'Control') # column & observations
  
# Productivity data (light, dark, recovery experiment results)
prodata <- read_csv("SitkaNSF_ProductivityLightDark_04092020_longformat.csv", 
          col_types = cols(Date = col_date(format = "%m/%d/%Y")))

  # Select pools of interest
    # Get list of control pools from pool data (7,10,25,30,31)
    unique(pooldata$Pool)
    # Filter dataset for control pools
    prodata <- filter(prodata, Pool %in% unique(pooldata$Pool))
  
  # Select dates of interest (2019 only)
    # make year column
    prodata$Year <- year(prodata$Date)
    # Filter only 2019 data
    prodata <- filter(prodata, Year == '2019')
    
# Water chemsitry data (T1, T2, T3 Day and Night data)
chemdata <- read_csv("SitkaNSF_WaterChemData_04092020.csv")
View(chemdata)
  # Comment from SL - chemdata <- chemdata [-c(,9:26)] didn't work
  # Resolve from LP - see changes in format below
    # I wouldn't get rid of column 9 - there is data farther down
    unique(chemdata$`Light /Apogee(PAR)`) # lots of non-NA values
    # To get rid of columns 11-26
    chemdata <- chemdata[,-c(11:26)] # note location of comma

  # Code from SL - 
      # chemdata <- filter(chemdata, Pool == 7, 10, 25, 30, 31)
          # This won't work because each == can only refer to one thing
          # Unless... you use %in% instead to specify a list (see productivity data above)
      # chemdata <- filter(chemdata, Pool = 7, Pool = 10 ...)
          # This won't work because you have to repeat the "filter" argument over and over again
          # this is a good start, becuase the following will work: 
    
      chemdata2 <- filter(chemdata, Pool == 7 | Pool == 10 | Pool == 25 | Pool == 30 | Pool == 31) # the | means "or"
      
          # if you want to be more concise, you can use %in%
      
      chemdata3 <- filter(chemdata, Pool %in% c(7,10,25,30,31))
      
          # if you want to be even more concise, you can have the unique list of pools specified by the unique() function so that you don't have to type the pool numbers by hand
      
      chemdata4 <- filter(chemdata, Pool %in% unique(pooldata$Pool))
      
      # If you look in the environment, all of the chemdatas we created (chemdata2, chemdata3, chemdata4) are all the same
      
      # we can use the unique() function to double check the code did what we wanted it to do
      
      unique(chemdata2$Pool)
      unique(chemdata3$Pool)
      unique(chemdata4$Pool)
      
      # We can confirm that these have all done the same thing and select one to use for our final analysis
      
      # You're on the right track :) keep going Stephany :)
