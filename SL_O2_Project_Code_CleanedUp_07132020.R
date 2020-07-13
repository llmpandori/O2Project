#################################################################
# Title: O2 Project - Cleaned Up Code
# Created by : Written by S. Lira & Cleaned by L. Pandori
# Created on : 3/?/2020
# Last Edited : 7/13/2020
#################################################################

##### Load Libraries #####
library(tidyverse) # tidy, visualize, model
library(lubridate) # dealing with dates

##### Load and Tidy Productivity (IDR experiment) Data #####

# productivity data (light, dark, recovery experiment results)
prodata <- read_csv("SitkaNSF_ProductivityLightDark_04092020.csv",
                    # specify date and time column formats 
                    col_types = cols(`Dark_Time_hh:mm` = 
                                       col_time(format = "%H:%M"),                        Date = col_date(format = "%m/%d/%Y"),
                    `Init_time_hh:mm` = col_time(format = "%H:%M"),
                    `Rec_Time_hh:mm` = col_time(format = "%H:%M")))


# get rid of calc columns
  prodata <- prodata[,-c(25:35)]
  
# Filter only 2019 data
# make year column
prodata$year <- year(prodata$Date)
prodata$month2 <- month(prodata$Date)

# 
prodata <- prodata %>%
  # only year 2019
  filter(year == '2019') %>%
  # only control pools 
  filter(Pool==7|Pool== 10|Pool==25| Pool== 30|Pool==31) %>%
  # no April
  filter(Month != 'April') %>%
  # create a moth 9a and 9b column for Sept ON Sept OFF
  mutate(month2 = ifelse(Date=='2019-09-21', '9A', month2))%>%
  mutate(month2 = ifelse(Date=='2019-09-23', '9B', month2))

##### Calculate NPP from Productivity Data #####

# NPP formula = delta O2 / delta time

# Approach:
# Productivity Data (Initial --> Dark) and (Dark --> Recovery)
# Compared to...
# Water Chemistry Data (Night T1 --> T2) & (Day T1 --> T2)

# Calculate NPP I --> D
prodata <- prodata %>%
  # calculate change in DO I --> D (in mg/L)
  mutate(ID_delta_DO = Dark_DO_mg_L - Init_DO_mg_L) %>%
  # calculate change in time I --> D (in hrs)
  mutate(ID_delta_Time = time_length(`Dark_Time_hh:mm` - `Init_time_hh:mm`, unit = 'hour')) %>%
  # divide delta DO by delta Time (result in mg O2/L/hr)
  mutate(ID_NPP = ID_delta_DO/ID_delta_Time)

# Calculate NPP D --> R (same formula and methods as above)
prodata <- prodata %>%
  mutate(DR_delta_DO = Rec_DO_mg_L - Dark_DO_mg_L) %>%
  mutate(DR_delta_Time = time_length(`Rec_Time_hh:mm` - `Dark_Time_hh:mm`, unit = 'hour')) %>%
  mutate(DR_NPP = DR_delta_DO/DR_delta_Time)

# save csv of productivity dataset with calculations 
write_csv(prodata, 'productivity_data_calculations.csv')

##### Load and Tidy Water Chemistry (T1-T3 Day and Night) Data #####
chemdata <- read_csv("SitkaNSF_WaterChemData_04092020.csv", 
                     col_types = cols(`DO (mg/L)` = col_number(),
                     Date = col_date(format = "%m/%d/%Y"), 
                     `Time water collected` = 
                      col_time(format = "%H:%M")))

# remove columns not needed 
chemdata <- chemdata[,-c(11:26)]

# tidy/filter relevant data
chemdata$Year <- year(chemdata$Date)

chemdata <- chemdata %>%
  # only 2019
  filter(Year == "2019") %>%
  # only control pools
  filter(Pool==7|Pool== 10|Pool==25| Pool== 30|Pool==31) %>%
  # rename columns for easier R reference
  rename('timept' = 'Time point',
         'timecollected' = 'Time water collected',
         'DO'='DO (mg/L)') %>%
  # get only T1 and T2
  filter(timept == 1 | timept == 2) 


# organize into long format (separate by time point, add identifiers, join)
chemdata1 <- filter(chemdata, timept == 1)
chemdata2 <- filter(chemdata, timept == 2)
colnames(chemdata1) <- paste('one',colnames(chemdata1), sep = '.')
colnames(chemdata2) <- paste('two', colnames(chemdata2), sep = '.')
chemdata3 <- cbind(chemdata1, chemdata2)
View(chemdata3)

#change the month into number
chemdata3<- chemdata3 %>%
  mutate(Month = month(one.Date))
#remove April and May months
chemdata3<- chemdata3[-c(21:30),]
unique(chemdata3$Month)

# Calculate NPP/GPP (see productivity data, T1 --> T2 for day and night experiments)
chemdata3<- chemdata3 %>%
  mutate(DN12 = two.DO - one.DO) %>%
  mutate(DN_delta_Time = time_length(two.timecollected - one.timecollected, unit = 'hour')) %>%
  mutate(DN_NPP = DN12/DN_delta_Time)

# add column to differentiate day and night
chemdata3<- chemdata3 %>%
  # if PAR > 0, daytime
  mutate(daynight=ifelse(`one.Light /Apogee(PAR)`>0, 'Day',"Night"))

# create pool column so you don't have to write 'one.' plenty 
chemdata3$Pool<-chemdata3$one.Pool


# chem data 4 = night data from water chemistry experiment
chemdata4<- chemdata3 %>%
  filter(daynight == 'Night') 

# chem data 5 = day data from water chemistry experiment
chemdata5<- chemdata3 %>%
  filter(daynight == 'Day')%>%
  mutate(daynight2= daynight)

# separate september ON/OFF by A/B deliniation for both datasets
chemdata4<- chemdata4 %>%
  mutate(Month=ifelse(one.Date=='2019-09-21', '9A',Month))%>%
  mutate(Month=ifelse(one.Date=='2019-09-22', '9B',Month))

chemdata5<- chemdata5 %>%
  mutate(Month=ifelse(one.Date=='2019-09-19', '9A',Month))%>%
  mutate(Month=ifelse(one.Date=='2019-09-25', '9B',Month))

# remove intermediate dataframes
remove(chemdata1, chemdata2, chemdata3)

##### Merge water chemistry (day and night T1 - T2) and productivity (IDR experiment) datasets #####

# merging chem and pro data 
prodata$Month <- prodata$month2

ultimate2<-prodata%>% full_join(chemdata4, by=c( 'Month','Pool'))%>%
  full_join(chemdata5, by=c( 'Month','Pool'))


#Now we remove unecessary columns
ultimate2 <- ultimate2[,-c(2,5,7,8,11,13,14,25,26,33,34,35,37,38,39,43,44,45,46,48,49,50,54,59,60,61,63,64,65,69,70,71,72,74,75,76,80,84)]
ultimate2 <- ultimate2[,-c(10,12,13)]

###### Calculate difference between Day(T1-->T2) and Light (D --> R) and Night (T1 --> T2) and Dark (I --> D)

ultimate2<- ultimate2 %>%
  # get difference between Night (T1 --> T2) and Dark (I --> D) and Day (T1 --> T2) and Light (D --> R)
  mutate(ND_NPP =ID_NPP-DN_NPP.x,
         DL_NPP =DR_NPP-DN_NPP.y) 

# save dataset
write_csv(ultimate2, 'merged_waterchem_prodata.csv')

# Convert from wide to long format 
d1 <- ultimate2[,c(1:2,45)] #differences in dark  
d2 <- ultimate2[,c(1:2,9,35,39,46)]#differences in light

# make cols consistent (pool, month, category, NPP, light)
d1$Category <- 'ND'
d1$Light <- '0'
d1$NPP <- d1$ND_NPP
d1$ID_NPP <- NULL
d1 <- select(d1, Pool, Month, Category, NPP, Light)

# d2 
View(d2)
d2$Category <- 'DL'
d2$Light <-(d2$Rec_Light_umol_m2_hr+d2$`one.Light /Apogee(PAR).y`+d2$`two.Light /Apogee(PAR).y`)/3
d2$NPP <- d2$DL_NPP
d2$Rec_Light_umol_m2_hr <- NULL
d2$`one.Light /Apogee(PAR).y`<-NULL
d2$`two.Light /Apogee(PAR).y`<-NULL
d2$DL_NPP <- NULL
d2 <- select(d2, Pool, Month, Category, NPP, Light)
#now we check
colnames(d1) == colnames(d2)
#now we merge
diffnpp <- rbind(d1,d2)
View(diffnpp)

# export difference only data
write_csv(difnpp, 'experiment_difference.csv')

##### One-sided t-tests to determine if differences between methodology (Day/Light vs Night/Dark) are significant #####

t.test(filter(diffnpp, diffnpp$Category=='DL')$NPP, mu=0, alternative = 'two.sided')
  # NSD for Day and Light (t = 1.67, df  = 34, p = 0.10)

t.test(filter(diffnpp, diffnpp$Category=='ND')$NPP, mu=0, alternative = 'two.sided')
  # significant difference for Night and Dark (t = -7.63, df = 34, p < 0.01)

# Summarize differences to plot
diffnpp_summary_nomonth <- diffnpp %>%
  group_by(Category) %>%
  # calculate mean, SD, N and SE
  summarize(Mean_NPP = mean(NPP),
            SD_NPP = sd(NPP),
            N_NPP = length(NPP))%>%
  mutate(SE_NPP = SD_NPP/(N_NPP^0.5))

# add nice method column
diffnpp_summary_nomonth$Method <- c('Light-Day','Dark-Night')

# add order so that compared methods are plotted side-by-side
diffnpp_summary_nomonth$Method <- ordered(diffnpp_summary_nomonth$Method,levels = unique(diffnpp_summary_nomonth$Method))

# export summary data
write_csv(diffnpp_summary_nomonth, 'difference_summary_no_month.csv')

#make a graph
ggplot(data = diffnpp_summary_nomonth) + 
  geom_col(mapping = aes(x = Method, y = Mean_NPP, fill = Method)) + 
  geom_errorbar(mapping = aes(x = Method, ymax = (Mean_NPP + SE_NPP), ymin = (Mean_NPP - SE_NPP)), width = 0.25) + 
  scale_fill_manual("Method", values=c("cadetblue1","cadetblue")) +
  xlab('Method')+
  geom_hline(yintercept = 0) + 
  ylab('NPP (mgO2/L/hr)') + 
  scale_y_continuous(sec.axis = sec_axis(~.*-6, name = "NCR (mgO2/L/hr)")) +
  theme_bw()


##### Convert from wide to long format #####

# make 3 separate dfs to pivot longer
ultimate3 <- ultimate2 %>%
  # selected cols of interest
  select(Pool, Month, ID_NPP, Rec_Light_umol_m2_hr, DR_NPP, DN_NPP.x, DN_NPP.y, `one.Light /Apogee(PAR).y`, `two.Light /Apogee(PAR).y`) 
d1 <- ultimate3[,1:3] #ID Npp
d2 <- ultimate3[,c(1:2,4:5)] #DR NPp
d3 <- ultimate3[,c(1:2,6)] #Nigth NPP
d4 <- ultimate3[,c(1:2,7:9)]#Day NPP

# make cols consistent (pool, month, category, NPP, light)
d1$NPP <- d1$ID_NPP
d1$Category <- 'ID'
d1$Light <- '0'
d1$ID_NPP <- NULL
d1 <- select(d1, Pool, Month, Category, NPP, Light)

# d2 
View(d2)
d2$Category <- 'DR'
d2$Light <- d2$Rec_Light_umol_m2_hr
d2$NPP <- d2$DR_NPP
d2$Rec_Light_umol_m2_hr <- NULL
d2$DR_NPP <- NULL

d2 <- select(d2, Pool, Month, Category, NPP, Light)

# d3
d3$Category <- 'NightT'
d3$Light <- '0'
d3$NPP <- d3$DN_NPP.x
d3$DN_NPP.x <- NULL

d3 <- select(d3, Pool, Month, Category, NPP, Light)


# d4 
d4$Category <- 'DayT'
d4$Light <- (d4$`one.Light /Apogee(PAR).y`+ d4$`two.Light /Apogee(PAR).y`)/2
d4$NPP <- d4$DN_NPP.y
d4$DN_NPP.y <- NULL
d4$`one.Light /Apogee(PAR).y` <- NULL
d4$`two.Light /Apogee(PAR).y` <- NULL

d4 <- select(d4, Pool, Month, Category, NPP, Light)

# check our work
colnames(d1) == colnames(d2)
colnames(d2) == colnames(d3)
colnames(d4) == colnames(d3)

# rbind 
prettynpp <- rbind(d1,d2,d3,d4)

# Plot before summary functions
ggplot(data = prettynpp) + 
  geom_boxplot(mapping = aes(x = Category, y = NPP)) 

# save long-format data
write_csv(prettynpp, 'long_format_difference_data.csv')


###### Exploratory Data Analysis #####
# This section gets more disorganized -- will re-format once data analysis direction is chosen 

# 1-way ANOVAs on each category to determine if there is a difference b/w months

ID_aov <- aov(NPP ~ Month,data = filter(prettynpp, prettynpp$Category == 'ID'))
DR_aov <- aov(NPP ~ Month,data = filter(prettynpp, prettynpp$Category == 'DR'))
# New summary table with things to plot
prettynpp_summary_nomonth <- prettynpp %>%
  group_by(Category) %>%
  summarize(Mean_NPP = mean(NPP),
            SD_NPP = sd(NPP),
            N_NPP = length(NPP))%>%
  mutate(SE_NPP = SD_NPP/(N_NPP^0.5))

prettynpp_summary_nomonth$Method <- c('Day T1-T2', 'Prod. D-R', 'Night T1-T2', 'Prod. I-D')

prettynpp_summary_nomonth$Wrap <- c(1,1,2,2)

prettynpp_summary_nomonth$Method <- ordered(
  prettynpp_summary_nomonth$Method, 
  levels = unique(prettynpp_summary_nomonth$Method))

# re-order Method to be not in alphabetical
p3<-ggplot(data = prettynpp_summary_nomonth) + 
  geom_col(mapping = aes(x = Method, y = Mean_NPP, fill = Method)) + 
  geom_errorbar(mapping = aes(x = Method, ymax = (Mean_NPP + SE_NPP), ymin = (Mean_NPP - SE_NPP)), width = 0.25) +
  scale_fill_manual("Method", values=c("cadetblue1","cadetblue","cyan3","cyan4"))+
  geom_vline(xintercept = 2.5, linetype = 'longdash') +
  geom_hline(yintercept = 0) + 
  ylab('NPP (mgO2/L/hr)') + 
  scale_y_continuous(sec.axis = sec_axis(~.*-6,name = "NCR (mgO2/L/hr)"))  +
  theme_bw()

#filter the data on the prettynpp to do a linear regression model
lightnpp <- prettynpp %>%
  filter(Light > 0) 

lightnpp$Light <- as.numeric(lightnpp$Light)

# DR and DayT Together
DR_DayT_linreg <- lm(NPP ~ Light, data = lightnpp)
summary(DR_DayT_linreg)

# Important values: 
# R2 = 0.03 (r2*100 tells you about the % of variance in Y that can be accounted for by the variance in X)
# P-value = 0.09 (tells if slope is significantly different from 0 -- here, it's not)

p1<-ggplot(data = lightnpp, mapping = aes(x = Light, y = NPP)) + 
  geom_point() + 
  geom_smooth(method = 'lm', linetype = 'dashed') 

p2<-ggplot(data = lightnpp, mapping = aes(x = Light, y = NPP, fill=Category)) + 
  geom_point() + 
  geom_smooth(method = 'lm', linetype = 'dashed') 

p1+p2

# Ancova prep
  
 interaction <- lm(NPP ~ Light*Category, data = lightnpp)
  summary(interaction)
  # there is a significant difference in slopes

  summary(lm(NPP ~ Light, data = filter(lightnpp, Category == 'DR')))
  
  summary(lm(NPP ~ Light, data = filter(lightnpp, Category == 'DayT')))
  
  library(nlme)
  # dayt model
  m1 <- lme(NPP ~ Light + Pool + Category, random = ~1|Month, data= filter(lightnpp))
  
  m2 <- lme(NPP ~ , random = ~1|Pool, data= filter(lightnpp, Category == 'DR'))
  
  
  # test assumptions----------------------
  # test fit of the residuals.  There should be no patterns
  par(mfrow=c(1,1)) # make the subplots
  qqnorm(resid(m1))
  qqnorm(resid(m2))
  
  E2<-resid(m1, type = "normalized")
  E22 <- resid(m2, type = 'normalized')# extract normalized residuals
  F2<-fitted(m1)
  F22 <- fitted (m2)# extract the fitted data
  
  plot(F2, E2, xlab = "fitted values", ylab = "residuals") # plot the relationship
  abline(h = 0, lty = 2)
  
  plot(F22, E22, xlab = 'fitted values', ylab = 'residuals')
  abline(h = 0, lty = 2)# add a flat line at zerp
  
  # test for homogeneity of variances
  boxplot(E2~filter(lightnpp, Category == 'DR')$Month, ylab = "residuals")
  
  boxplot(E22~filter(lightnpp, Category == 'DayT')$Month, ylab = "residuals")
  
  # check for independence. There should be no pattern
  plot(E2~filter(lightnpp, Category == 'DR')$NPP, ylab = 'residuals', xlab = "NPP")
  
  plot(E22 ~ filter(lightnpp, Category == 'DayT')$NPP, ylab = 'residuals', xlab = 'NPP')
  
  # anova(m)
  summary(m1)
  anova(m1)
  
  summary(m2)
  anova(m2)
  

  # dayt model
  m3 <- lme(NPP ~ Light + Pool + Category, random = ~1|Month, data= filter(lightnpp))

  qqnorm(resid(m3))
  
  
  E3<-resid(m3, type = "normalized")
  F3<-fitted(m3)
  
  plot(F3, E3, xlab = "fitted values", ylab = "residuals") # plot the relationship
  abline(h = 0, lty = 2)
  
  
  # test for homogeneity of variances
  boxplot(E3~lightnpp$Month, ylab = "residuals")
  boxplot(E3 ~lightnpp$Pool, ylab = 'residuals')
  boxplot(E3 ~ lightnpp$Category, ylab = 'residuals')
  
  # check for independence. There should be no pattern
  plot(E3~lightnpp$NPP, ylab = 'residuals', xlab = "NPP")

  
  # anova(m)
  summary(m3)
  anova(m3)
  
lightnpp1 <- lightnpp %>% 
  group_by(Pool, Category) %>%
  summarize(lightmean = mean(Light),
            NPPmean = mean(NPP))

ggplot (data = lightnpp1,
        mapping = aes(x = lightmean, y = NPPmean)) + 
  geom_point(mapping = aes(color = Category, shape = Pool))

# make figs for NPP/NCR explanation

# get example month and all pools (Jan 2019)
o2figs <- chemdata %>%
  filter(month(Date) == 1) %>%
  select(Date, timept, Pool, DO) %>%
  mutate(timept2 = ifelse(timept ==1, 'T1', 'T2'))

# Day (O2 increase)

ggplot(data = filter(o2figs, day(Date) < 18),
      mapping = aes(x = timept2, y = DO)) + 
  geom_point(mapping = aes(color = Pool)) + 
  geom_line(mapping = aes(x = timept2, y = DO, group = Pool, color = Pool)) +
  xlab('Time point') + 
  ylab('Dissolved oxygen (mg O2/L)') +
  coord_cartesian(xlim = c(1.5,1.6)) +
  theme_bw()

ggsave('Day_o2_increase.png', width = 3, height = 3, unit = 'in')

# Night (o2 decrease)
ggplot(data = filter(o2figs, day(Date) > 18),
       mapping = aes(x = timept2, y = DO)) + 
  geom_point(mapping = aes(color = Pool)) + 
  geom_line(mapping = aes(x = timept2, y = DO, group = Pool, color = Pool)) +
  xlab('Time point') + 
  ylab('Dissolved oxygen (mg O2/L)') +
  coord_cartesian(xlim = c(1.5,1.6)) +
  theme_bw()
  
ggsave('Night_o2_decrease.png', width = 3, height = 3, unit = 'in')
