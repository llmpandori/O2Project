library(tidyverse) # tidy, visualize, model
library(lubridate) # dealing with dates
library(dplyr)
library(ggplot)
#####
# Productivity data (light, dark, recovery experiment results)
prodata <- read_csv("SitkaNSF_ProductivityLightDark_04092020.csv", col_types = cols(`Dark_Time_hh:mm` = col_time(format = "%H:%M"), 
                                                                                    Date = col_date(format = "%m/%d/%Y"),`Init_time_hh:mm` = col_time(format = "%H:%M"),`Rec_Time_hh:mm` = col_time(format = "%H:%M")))
View(prodata)
# get rid of weird calc columns
prodata <- prodata[,-c(25:35)]
# Filter only 2019 data
# make year column
prodata$year <- year(prodata$Date)
prodata$month2<-month(prodata$Date)
# Filter only 2019 data
prodata <- filter(prodata, year == '2019')
unique(prodata$year)
prodata<- filter(prodata, Pool==7|Pool== 10|Pool==25| Pool== 30|Pool==31)
unique(prodata$Pool)
# Filter dataset for control pools
prodata <- filter(prodata, Pool %in% unique(prodata$Pool))
#creating a month number column
prodata<- prodata %>%
  mutate(Month=month(Date))
#get rid of April
prodata <- prodata[-c(11:15),]
unique(prodata$Month)
#create a moth 9a and 9b column do the same for chemdata
prodata<- prodata %>%
  mutate(Month=ifelse(Date=='2019-09-21', '9A', Month))%>%
  mutate(Month=ifelse(Date=='2019-09-23', '9B', Month))
unique(prodata$Month)

#####
##### Calculate NPP #####

# NPP formula = change in O2 / change in time
# Producitivity data (Initial --> Dark) and (Dark --> Recovery)
# Chem Data (Night T1 --> T2) & (Day T1 --> T2)
#####
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
#####
# NPP formula = change in O2 / change in time
# Producitivity data (Dark --> Recovery)
# Chem Data (Night T1 --> T2) & (Day T1 --> T2)

prodata <- prodata %>%
  # mutate(new colum = math)
  mutate(DR_delta_DO = Rec_DO_mg_L - Dark_DO_mg_L) %>%
  mutate(DR_delta_Time = time_length(`Rec_Time_hh:mm` - `Dark_Time_hh:mm`, unit = 'hour')) %>%
  mutate(DR_NPP = DR_delta_DO/DR_delta_Time)

##### Let's plot something! #####

ggplot() + 
  geom_col(data = prodata,
           mapping = aes(x = Pool, y = DR_NPP, fill = Pool)) + 
  xlab('Pool') + 
  # y label 
  ylab('NPP gO2/L/hr') + 
  # gives title
  ggtitle('Dark --> Recovery NPP in Control Pools') +
  # massive improvement w one line of code
  theme_bw()

#####
chemdata <- read_csv("SitkaNSF_WaterChemData_04092020.csv",col_types = cols(`DO (mg/L)` = col_number(),Date = col_date(format = "%m/%d/%Y"), `Time water collected` = col_time(format = "%H:%M")))
View(chemdata)
#remove the columns:
chemdata <- chemdata[,-c(11:26)]
#filter the dates and year
chemdata$Year <- year(chemdata$Date)
chemdata <- filter(chemdata,Year=="2019")
chemdata <- filter(chemdata, Pool==7|Pool== 10|Pool==25| Pool== 30|Pool==31)
unique(chemdata$Pool)

#####
chemdata <- chemdata %>%
  rename('timept' = 'Time point',
         'timecollected' = 'Time water collected','DO'='DO (mg/L)') %>%
  # structure: rename('newname' = 'oldname')
  filter(timept == 1 | timept == 2)
# filter time points 1 or 2 
chemdata1 <- filter(chemdata, timept == 1)
chemdata2 <- filter(chemdata, timept == 2)
colnames(chemdata1) <- paste('one',colnames(chemdata1), sep = '.')
#View(chemdata1)
colnames(chemdata2) <- paste('two', colnames(chemdata2), sep = '.')
#View(chemdata2)
chemdata3 <- cbind(chemdata1, chemdata2)
View(chemdata3)
#filter the pools and check
unique(chemdata1$one.Date==chemdata2$two.Date)
unique(chemdata1$one.Pool==chemdata2$two.Pool)
#change the month into number
chemdata3<- chemdata3 %>%
  mutate(Month=month(one.Date))
#remove April and May months
chemdata3<- chemdata3[-c(21:30),]
unique(chemdata3$Month)

#####
#Now we graph at this 
#Checking between time 1 & 2 analysis for NPP
# Know Producitivity data (ID_12 = one.DO - two.DO)) 
#Chem Data (day->night)
chemdata3<- chemdata3 %>%
  # mutate(new colum = math)
  mutate(DN12 = two.DO - one.DO) %>%
  mutate(DN_delta_Time = time_length(two.timecollected - one.timecollected, unit = 'hour')) %>%
  mutate(DN_NPP = DN12/DN_delta_Time)

#####
#graph
##if x is left as just Pool, Error in FUN(X[[i]], ...) : object 'Pool' not found
ggplot() + 
  geom_col(data = chemdata3,
           mapping = aes(x = one.Pool, y = ID_NPP)) + 
  xlab('Pool') + 
  # y label 
  ylab('NPP gO2/L/hr') + 
  # gives title
  ggtitle('Initial --> Final NPP in Control Pools') +
  # massive improvement w one line of code
  theme_bw()
ggplot() + 
  geom_col(data = chemdata3,
           mapping = aes(x = two.Pool, y = ID_NPP)) + 
  xlab('Pool') + 
  # y label 
  ylab('NPP gO2/L/hr') + 
  # gives title
  ggtitle('Initial --> Final NPP in Control Pools') +
  # massive improvement w one line of code
  theme_bw()

#now we seperate in regards to day and night
chemdata3<- chemdata3 %>%
  mutate(daynight=ifelse(`one.Light /Apogee(PAR)`>0, 'Day',"Night"))
chemdata3$Pool<-chemdata3$one.Pool
chemdata4<- chemdata3 %>%
  filter(daynight == 'Night') 
chemdata5<- chemdata3 %>%
  filter(daynight == 'Day')%>%
mutate(daynight2= daynight)
#Now we check for the daynight to see what they are unique
unique(chemdata4$daynight)
unique(chemdata5$daynight)
#now we make september the 9A and 9B
chemdata4<- chemdata4 %>%
  mutate(Month=ifelse(one.Date=='2019-09-21', '9A',Month))%>%
  mutate(Month=ifelse(one.Date=='2019-09-22', '9B',Month))
unique(chemdata4$Month)
#now we make september the 9A and 9B
chemdata5<- chemdata5 %>%
  mutate(Month=ifelse(one.Date=='2019-09-19', '9A',Month))%>%
  mutate(Month=ifelse(one.Date=='2019-09-25', '9B',Month))
unique(chemdata5$Month)

##### ultimate<-merge(prodata,chemdata4, by=c('Pool', 'Month'))ultimate<-merge(ultimate,chemdata5, by=c('Pool', 'Month'))view(ulitmate)
# merging chem and pro data 
ultimate2<-prodata%>% full_join(chemdata4, by=c( 'Month','Pool'))%>%
  full_join(chemdata5, by=c( 'Month','Pool'))
View(ultimate2)

#Now we remove unecessary columns
ultimate2 <- ultimate2[,-c(2,5,7,8,11,13,14,25,26,33,34,35,37,38,39,43,44,45,46,48,49,50,54,59,60,61,63,64,65,69,70,71,72,74,75,76,80,84)]
ultimate2 <- ultimate2[,-c(10,12,13)]

#go back and change the names(day and night) and (light par available)

#analyze the difference between ID and Dx nigth difference
#analyze the light difference 
#plot the differences summary data instead of pool
#what is the difference between the two methods, two day and two light

# Let's look at ultimate2
View(ultimate2)
#we want to be able to plot the differences between the methods so we need to create a new column and table with this informations. above 0 if tarp 1 -other, if its positive, then yield another result
#statitstical test on difference of averges and differences
#analyze the difference between ID and night.Know Producitivity data (Diff_NPP = ID_NPP-DN_NPP.x)) 
ultimate2<- ultimate2 %>%
  # mutate(new colum = math)
  mutate(ND_NPP =ID_NPP-DN_NPP.x ) 
#analyze the difference between DR and Day.Know Producitivity data (Diff_NPP = DR_NPP-DN_NPP.y)) 
ultimate2<- ultimate2 %>%
  # mutate(new colum = math)
  mutate(DL_NPP =DR_NPP-DN_NPP.y ) 
#now we make a pivot table:
# factors: 
# Pool
# Month
# Dark_Difference = IDNPP - DN_NPP.x
# Light_Difference = DR_NPP - DN_NPP.y
# make 2 separate dfs to pivot longer
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
#take a summary 
diffnpp_summary <- diffnpp %>%
  group_by(Category, Month) %>%
  summarize(Mean_NPP = mean(NPP),
            Mean_light = mean(Light),
            SD_NPP = sd(NPP),
            SD_Light = sd(Light))
diffnpp_summary2 <- diffnpp %>%
  group_by(Category, Pool, Month) %>%
  summarize(Mean_NPP = mean(NPP),
            Mean_light = mean(Light),
            SD_NPP = sd(NPP),
            SD_Light = sd(Light))

#we make graphs now 
ggplot(data = diffnpp_summary) + 
  geom_col(mapping = aes(x = Category, y = Mean_NPP)) + scale_fill_manual("Category", values=c("red","yellow"))+
  facet_wrap(~Month)
#running an anova test
lm(Mean_NPP~ Month + Category, data=diffnpp_summary) %>% 
  anova

ggplot(diffnpp_summary2, aes(x = Category,  y = Mean_NPP, fill=Pool)) + 
  labs(title = "Difference Between Methodology", x = "Category", y = "Light") +geom_boxplot() + scale_fill_manual("Pool", values=c("red","yellow","blue","green","orange"))
#running a ANOVA
lm(Mean_NPP~ Pool + Category, data=diffnpp_summary2) %>% 
  anova

ggplot(diffnpp, aes(x = Pool, fill=Category, y = Light)) + 
  labs(title = "Difference Between Methodology", x = "Category", y = "NPP") +
  geom_boxplot() + 
  scale_fill_manual("Pool", values=c("red","yellow","blue","green","orange"))+
  facet_wrap(~Month)
#runing anova
lm(Light~ Pool + Category, data=diffnpp) %>% 
  anova

ggplot(diffnpp, aes(x = Category,  y = Light)) + 
  labs(title = "Difference Between Methodology", x = "Category", y = "NPP") +
  geom_point() + 
  geom_smooth(method="lm", se = F) + 
  scale_color_manual("Category", values=c("red","yellow","blue","green","orange"))+
  facet_wrap(~Month)

# response variables: 
#  ID_NPP = Initial --> Dark (daytime tarp)
#  DR_NPP = Dark --> Recovery (daytime tarp)
#  DN_NPP.x = Night T1 --> Night T2 (night natural)
#  DN_NPP.y = Day T1 --> T2 (day natural)
#  Make light columns that // names

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
View(prettynpp)
# Plot before summary functions
ggplot(data = prettynpp) + 
  geom_boxplot(mapping = aes(x = Category, y = NPP)) 

# Start with 1-way ANOVAs on each category to determine if there is a difference b/w months

ID_aov <- aov(NPP ~ Month, 
          data = filter(prettynpp, prettynpp$Category == 'ID'))

# Pretending month isn't a big deal -- use one-sided t-tests to determine if samples overlap 0

t.test(filter(diffnpp, diffnpp$Category == 'ND')$NPP, mu=0, alternative = 'two.sided')

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
ggplot(data = prettynpp_summary_nomonth) + 
  geom_col(mapping = aes(x = Method, y = Mean_NPP, fill = Method)) + 
  geom_errorbar(mapping = aes(x = Method, ymax = (Mean_NPP + SE_NPP), ymin = (Mean_NPP - SE_NPP)), width = 0.25) + 
  geom_vline(xintercept = 2.5, linetype = 'longdash') +
  geom_hline(yintercept = 0) + 
  ylab('NPP or NCR (mgO2/L/hr)') + 
  scale_y_continuous(sec.axis = dup_axis()) +
  theme_bw()



#running anova
lm(Mean_NPP ~ Category+ Month, data=prettynpp_summary) %>% 
  anova

ggplot(data = prettynpp_summary) + 
  geom_col(mapping = aes(x = Category, y = Mean_light)) + 
  facet_wrap(~Month)


ggplot(data=prettynpp) +
  labs(title="NPP vs Category", x = "Category", y = "NPP")+
  geom_col( aes(x = Category, fill = Pool, y = NPP)) +
 +scale_fill_manual("Pool", values=c("red","yellow","blue","green","orange"))
  
ggplot(prettynpp, aes(x = Category, fill = Pool, y = NPP)) + 
  labs(title="NPP vs Category", x = "Category", y = "NPP") +
  geom_boxplot() + 
  scale_fill_manual("Pool", values=c("red","yellow","blue","green","orange")) +scale_y_continuous(limits=c(0,NA))+facet_wrap(~Month)
#running anova
lm(NPP ~ Category+ Pool, data=prettynpp) %>% 
  anova

ggplot(prettynpp, aes(x = Category, fill = Pool, y = NPP)) + 
  labs(title="NPP vs Category", x = "Category", y = "NPP") +
  geom_boxplot() + 
  scale_fill_manual("Pool", values=c("red","yellow","blue","green","orange")) +scale_y_continuous(limits=c(0,NA))+facet_wrap(~Month)
#running anova
lm(NPP ~ Category+ Month+Pool, data=prettynpp) %>% 
  anova

ggplot(prettynpp, aes(x = Category, fill = Pool, y = Light)) + 
  labs(title="NPP vs Category", x = "Category", y = "NPP") +
  geom_boxplot() + 
  scale_fill_manual("Pool", values=c("red","yellow","blue","green","orange"))+facet_wrap(~Month)
#running anova
lm(Light~ Category+ Pool, data=prettynpp) %>% 
  anova












#now make plots
ggplot(ultimate2, aes(x = Month, y = DR_NPP, fill=Pool)) + 
  labs(title = "Seasonal Initial-> Dark", x = "Seasons", y = "ID_NPP")  +
  geom_point()  +scale_color_manual("Month", values=c("Red","Yellow", "Blue", "Green", "Orange"))
ggplot(ultimate2, aes(x = Month, y = DR_NPP, fill=Pool)) + 
  labs(title = "TITLE", x = "X AXIS LABEL", y = "Y AXIS LABEL") +
  geom_boxplot()
plot_1_bymonth <- ggplot(ultimate2, aes(x=Pool, y=ID_NPP, color=Month))+
  geom_point()+
  facet_wrap(~Month)
plot_1_bymonth
plot_2_bymonth <- ggplot(ultimate2, aes(x=DN_NPP.x, y=ID_NPP, color=Pool))+
  geom_point()+
  facet_wrap(~Pool)
plot_2_bymonth

#now make plots
 ggplot(ultimate2, aes(x = Month, y = DR_NPP, fill=Pool)) + 
labs(title = "Seasonal Initial-> Dark", x = "Seasons", y = "ID_NPP")  +
  geom_point()  +scale_color_manual("Month", values=c("Red","Yellow", "Blue", "Green", "Orange"))
ggplot(ultimate2, aes(x = Month, y = DR_NPP, fill=Pool)) + 
labs(title = "TITLE", x = "X AXIS LABEL", y = "Y AXIS LABEL") +
geom_boxplot()
plot_1_bymonth <- ggplot(ultimate2, aes(x=Pool, y=ID_NPP, color=Month))+
  geom_point()+
  facet_wrap(~Month)
plot_1_bymonth
plot_2_bymonth <- ggplot(ultimate2, aes(x=DN_NPP.x, y=ID_NPP, color=Pool))+
  geom_point()+
  facet_wrap(~Pool)
plot_2_bymonth

# lauren