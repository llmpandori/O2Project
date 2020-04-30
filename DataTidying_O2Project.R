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
prodata <- read_csv("SitkaNSF_ProductivityLightDark_04092020.csv", 
                       col_types = cols(`Dark_Time_hh:mm` = col_time(format = "%H:%M"), 
                                        Date = col_date(format = "%m/%d/%Y"), 
                                        `Init_time_hh:mm` = col_time(format = "%H:%M"), 
                                        `Rec_Time_hh:mm` = col_time(format = "%H:%M")))

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
    
    # get rid of weird calc columns
    prodata <- prodata[,-c(25:35)]
# Water chemsitry data (T1, T2, T3 Day and Night data)
chemdata <- read_csv("SitkaNSF_WaterChemData_04092020.csv", 
                     col_types = cols(Date = col_date(format = "%m/%d/%Y")))
View(chemdata)
    # get rid of weird columns
    chemdata <- chemdata[,-c(11:26)] # note location of comma
    # filter for pools
    chemdata <- filter(chemdata, Pool %in% unique(pooldata$Pool))
    
    chemdata <- filter(chemdata, year(Date) == '2019')
      
##### Calculate NPP #####

# NPP formula = change in O2 / change in time
  # Producitivity data (Initial --> Dark) and (Dark --> Recovery)
  # Chem Data (Night T1 --> T2) & (Day T1 --> T2)
    
prodata <- prodata %>%
  # mutate(new colum = math)
  mutate(ID_delta_DO = Dark_DO_mg_L - Init_DO_mg_L) %>%
  mutate(ID_delta_Time = time_length(`Dark_Time_hh:mm` - `Init_time_hh:mm`, unit = 'hour')) %>%
  mutate(ID_NPP = ID_delta_DO/ID_delta_Time)

##### Let's plot something! #####

ggplot() + 
  geom_col(data = prodata,
            mapping = aes(x = Pool, y = ID_NPP, fill = Pool)) + 
  xlab('Pool') + 
  # y label 
  ylab('NPP gO2/L/hr') + 
  # gives title
  ggtitle('Initial --> Dark NPP in Control Pools') +
  # massive improvement w one line of code
  theme_bw()

# Water chem data
View(chemdata)


chemdata <- chemdata %>%
  rename('timept' = 'Time point') %>%
  # structure: rename('newname' = 'oldname')
  filter(timept == 1 | timept == 2)
  # filter time points 1 or 2



  