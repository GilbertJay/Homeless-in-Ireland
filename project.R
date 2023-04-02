
################# HOMELESS ACCESSING EMERGENCY ACCOMMODATION IN IRELAND (2016 - 2022) ###############


#IMPORTING THE DATA 

setwd("C:/Users/aline/OneDrive/Documentos/Dundalk Institute/Real Time Data Analytics/PROJECT")
mydata= read.csv("data1.csv", header=T)
mydata2= read.csv("data2.csv", header=T)           
head(mydata)
head(mydata2)

#LIBRARIES 
library(tidyverse)
library(lubridate)
library(scales)
library(dplyr)
library(ggplot2)
library(leaflet)
library(fpp2)

install.packages("tidyverse")
# DATA  #FORMATTING   #TRANSFORMING IN DATA FRAME
mydata= as.data.frame(mydata)
mydata2= as.data.frame(mydata2)

#CHECKING AND FORMATTING DATA AS NUMERIC AND AS DATE                                     
sapply(mydata,class)
mydata[,3:17]= sapply(mydata[ ,3:17], as.numeric) # setting as numeric 
mydata$Reference.Date=noquote(mydata$Reference.Date)  # setting as date
mydata$Reference.Date= my(as.character(mydata$Reference.Date,label = TRUE, abbr = TRUE))


sapply(mydata2,class)
mydata2$Homesslessness.by.county = as.numeric(mydata2$Homesslessness.by.county) # setting as numeric 
mydata2$Reference.Date= my(as.character(mydata2$Reference.Date,label = TRUE, abbr = TRUE))  # setting as date

#CHECKING DOR MISSING VALUES
sapply(mydata, function(x) sum(is.na(x)))

sapply(mydata2, function(x) sum(is.na(x)))

# THE DATA on which "total families of each single parents was not available for 54 observations"
# OTHER THAN THAT THERE ARE NO MISSING VALUES 

############## DATA EXPLORATION 

#Number of homelessness over time
mydata = mutate(mydata, Year=year(Reference.Date), Month= month(Reference.Date))

homeless_by_year = mydata%>%
  group_by(Reference.Date) %>%
  summarise(total_homeless = sum(Homeless.adults))

  homeless_by_year

ggplot(homeless_by_year, aes(x = Reference.Date,Month, y=total_homeless))+
  geom_line()+
  geom_point()+
  theme_classic()+
  ggtitle("Total Homeless over time") +
  xlab("year") + ylab("Homeless")+
  (scale_x_date(breaks=date_breaks("1 month"),
                labels=date_format("%b %y")))+
  theme(axis.text.x = element_text(angle = 90))

#It seems like the number of homeless flow an increase trend
# expect from March 2020 the numbers drop and increase again from June 2021
#probably because of the pandemic

#Number of homeless by month
homeless_by_month = mydata%>%
  group_by(Month) %>%
  summarise(avg_homeless = mean(Homeless.adults))

homeless_by_month

ggplot(homeless_by_month, aes(x = Month, y=avg_homeless))+
  geom_line()+
  geom_point()+
  theme_classic()+
  ggtitle("Average Homeless by month") +
  xlab("year") + ylab("Homeless")+
  scale_x_continuous(breaks=seq(1,12,1))

#Is it possible the summer (April to August) affects the number of homeless? 

# Homeless by County 
homeless_by_region = mydata2%>%
  group_by(Region)%>%
  summarise(Homeless = mean(Homesslessness.by.county))

homeless_by_region %>% 
  ggplot(aes(fct_rev(fct_reorder(Region,Homeless)), Homeless))+
  geom_col(aes(fill=Region))+
  theme_classic()+
  labs(x="Region", title="Average Homeless by Region")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="none")

# Due to the high concentration of homeless in Dublin, in this dataset it is considered as a region
# Other than Dublin, the highest average of homeless concentrate in the South-West 

#Homeless by county 
homeless_by_county = mydata2%>%
  group_by(Counties)%>%
  summarise(Homeless = mean(Homesslessness.by.county))%>%
  arrange(desc(Homeless))



homeless_by_county %>% 
  ggplot(aes(fct_rev(fct_reorder(Counties,Homeless)), Homeless))+
  geom_col(aes(fill=Counties))+
  theme_classic()+
  labs(x="Counties", title="Average Homeless by County")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="none")
 
#Dublin is the county with highest average of homeless, followed by counties that have cities such as Cork, Galway and Limerick 

#Homeless Profile       #by gender 
gender_table = mydata %>%
  group_by(Year)%>%
  summarise(male= sum(Male)/sum(Homeless.adults),  female= sum(Female)/sum(Homeless.adults))

gender_table 

# Generally, men represent the majority of homeless, but since 2020 this number increased to 2/3 of the total number of homeless

age_table = mydata %>%
  group_by(Year)%>%
  summarise(Ages.18.24= sum(Ages.18.24)/sum(Homeless.adults), 
            Ages.25.44= sum(Ages.25.44)/sum(Homeless.adults),
            Ages.45.64= sum(Ages.45.64)/sum(Homeless.adults),
            Ages.65.= sum(Ages.65.)/sum(Homeless.adults))

age_table

# The majority of homeless people are between 25 and 44. 
# The number of people living in the streets age between 45 and 64 have been increasing, it increased over 6% since 2017
# Young adults living on the streets represent 14% 
# Elderlies are minority, they represent about 2%

#Accommodation Type 
accommodation_table = mydata %>%
  group_by(Year)%>%
  summarise(PEA= sum(PEA)/sum(Homeless.adults), 
            STA= sum(STA)/sum(Homeless.adults),
            TEA= sum(TEA)/sum(Homeless.adults),
            Other= sum(Other)/sum(Homeless.adults))

accommodation_table
#  Supported Temporary Accommodation was majority in 2017 and 2018.
# In 2019, Private Emergency Accommodation became majority while STA decreased to around 45% 
# Temporary Emergency Accommodation represent only 2%  and Others less than 1%


#PREDICTIVE ANALYSIS - TIME SERIES


#TIME SERIES WAS CHOSEN IN ORDER TO ANALYSE DATA OVER TIME AND TRY TO PREDICT THE NUMBER OF HOMELESS ONE YEAR AHEAD 
# STEP 1: PERFORM FEATURE ENGINEERING TO GROUP THE SUM OF HOEMELESS IN IRELAND BY MONTH 
#STEP 2: CHECK FOR STATIOARITY ASSUMPTION
# STEP3: IF NOT STATIONARY, TRANFORMING THE DATA INTO STATIONARY BY APPLYING DIFF, LOG OR BOTH 
# STEP 4: FINDING D, P, Q VALUES BY PLOTTING ACF AND PACF PLOTS
#STEP 5: APPLYING THOSE VALUES IN THE ARIMA MODELS, COMPARE THE MODELS AND CHOOSING THE ONE WITH LOWEST AIC
# STEP 6: MAKE A PREDICTION FOR 12 MONTHS AHEAD 

mydata_modified = mydata%>%
  group_by(Reference.Date)%>%
  summarise(Homeless = sum(Homeless.adults),
            Male = sum(Male),
            Female = sum(Female),
            Ages.18.24=sum(Ages.18.24),
            Ages.25.44=sum(Ages.25.44),
            Ages.45.64=sum(Ages.45.64),
            Ages.65=sum(Ages.65.),
            PEA=sum(PEA),
            STA=sum(STA),
            TEA=sum(TEA),
            Other=sum(Other),
            Total.Families = sum(Total.Families),
            single_parents = sum(X.of.which..single.parent.families),
            Total.Adults=sum(Total.Adults),
            Total.Adults=sum(Total.Adults))
            
mydata_modified

mydata2_modified = mydata2%>%
  group_by(Reference.Date)%>%
  summarise(Homeless = sum(Homesslessness.by.county))
mydata2_modified


#TRANSFORMING DATASET IN TIME SERIES FORMAT
mydata_modified_ts = ts(mydata_modified, start = c(2016,1), frequency = 12)
mydata2_modified_ts  = ts(mydata2_modified, start = c(2016,1), frequency = 12)

is.ts(mydata_modified_ts)
is.ts(mydata2_modified_ts)

# is the data stationary?

autoplot(mydata_modified_ts[,2]) # TOTAL NUMBER OF HOMELESS
autoplot(mydata2_modified_ts[ ,2])

ggseasonplot(mydata_modified_ts[,2])
ggseasonplot(mydata_modified_ts[,2], polar = TRUE)
ggsubseriesplot(mydata_modified_ts[,2])

#Non-Stationary       
# There is a upper trend, except from 2020 to middle 2021
# The mean is not constant 
# It seems like there might be seasonality, the numbers usually get slightly higher from August to November and lower from March to June


ggAcf(mydata_modified[,2])

ggPacf(mydata_modified[,2])

dev.off()
#TRANSFORMING THE DATA INTO STATIONARY 
stationary_log = log(mydata_modified_ts[,2])
plot(stationary_log)            # The mean inconstancy and upper trend was not fixed in this model

stationary_diff = diff(mydata_modified_ts[,2])
plot(stationary_diff)                           #It seems like diff and diff log show the same results and fix the means inconsistency 

stationary_diff_log = diff(log(mydata_modified_ts[,2]))
plot(stationary_diff_log)

stationary_diff2_log = diff(diff(log(mydata_modified_ts[,2]))) ##It seems like diff diff log also fix the means inconsistency, except by a spinke in 2020 
plot(stationary_diff2_log)

### ARIMA      

                                        #DIFF
#d=1                       

#p= 0 
pacf(diff(mydata_modified_ts[,2]))

#q = 1
acf(diff(mydata_modified_ts[,2]))

# MODEL 1

model_1 = arima(mydata_modified_ts[,2], c(1, 0, 1),seasonal = list(order = c(1, 0, 1), period = 12)) #order: d,p,q
model_1

                                        #DIFF LOG
#d=1

#p=0
pacf(diff(log(mydata_modified_ts[,2])))

#q = 1
acf(diff(log(mydata_modified_ts[,2])))

# MODEL 2

model_2 = arima(log(mydata_modified_ts[,2]), c(1, 0, 1),seasonal = list(order = c(1, 0, 1), period = 12)) #order: d,p,q
model_2
                                        #DIFF DIFF LOG

#d=2 

#p=0
pacf(diff(diff(log(mydata_modified_ts[,2]))))

#q =  1
acf(diff(diff(log(mydata_modified_ts[,2]))))

# MODEL 3
model_3 = arima(log(mydata_modified_ts[,2]), c(2, 0, 1),seasonal = list(order = c(2, 0, 1), period = 12))
model_3

#Seems like model 3 is the best because it has the smallest AIC (-347.46)

#FORECASTING 

prediction <- predict(model_3, n.ahead = 12)
ts.plot(mydata_modified_ts[,2],2.718^prediction$pred, log = "y", lty = c(1,3))


# THE MODEL PREDICT THAT THE NUMBER OF HOMELESS IN IRELAND IN MARCH 2023 WILL BE AROUND 8.900 PEOPLE 



# DID THE PANDEMIC AFFECTED THE NUMBER OF HOMELESS ACCESSING EMERGENCY ACCOMMODATIONS IN IRELAND? 

mydata_compare = mutate(mydata, Year = year(Reference.Date), Month=month(Reference.Date))%>%
  filter(Year == 2019|Year == 2020|Year == 2021)

boxplot(mydata_compare$Homeless.adults~mydata_compare$Year, main = "Homeless in Ireland (2019-2021)", xlab="Year", ylab="Homeless")
#The boxplot don't show much difference between the years 

par(mfrow=c(2,1))
plot(aov(mydata_compare$Homeless.adults~mydata_compare$Year), which=1:2)
#The variance assumptions does seem to be about the same for the three groups
# The normality assumption is being violated 

#KRUSKAL TEST WAS CHOSEN TO TEST GROUPS INTEAD OF ANOVA BECAUSE DON'T NEED THE NORMALITY ASSUMPTION
kruskal.test(Homeless.adults ~ Year, data = mydata_compare) 
# P-value 0.1093 > 0.05 so we fail to reject the H0 that there is no difference among the years
# We conclude there is no significant difference between the number of homeless over the the period pre-pandemic(2019), during the pandemic(2020) and the "end" of pandemic (2021)


#The number of homeless accessing temporary accommodation change over the seasons?
mydata_seasons = mydata %>%
  mutate(Year=year(Reference.Date),Month=month(Reference.Date))
mydata_seasons = mutate(mydata_seasons, Seasons=case_when(Month %in% 3:5 ~ "Spring",
                                    Month %in% 6:8 ~ "Summer",
                                    Month %in% 9:11 ~ "Fall",
                                    Month %in% c(1,2,12) ~ "Winter"))

boxplot(Homeless.adults ~ Seasons, data = mydata_seasons)
#The boxplot do not show any evident difference over the seasons

par(mfrow=c(2,1))
plot(aov(Homeless.adults ~ Seasons, data = mydata_seasons), which=1:2)
kruskal.test(Homeless.adults ~ Seasons, data = mydata_seasons) 
# # P-value 0.9757 > 0.05 so we fail to reject the H0 that there is no difference among the seasons
# We conclude there is no significant difference on the number of homeless among seasons 

# PRESCRIPTIVE ANALYSIS 

#THE HOMELESSNESS IN IRELAND IS A COMPLEX SOCIAL ISSUE THAT NEED SEVERAL FACTORS TO BE UNTANGLED 
# THE DATASET USED IN THIS PROJECT DOES NOT PROVIDE ANY CONTEXT TO ANALYSED IN ORDER TO UNDERSTAND WHAT INFLUENCES THE FIGURES
# FOR THIS REASON, EXTERNAL DATA ON HOUSE INDEX PRICES AND UNEMPLYMENT WAS TAKEN FROM CSO.IE FROM THE SAME PERIOD
# THE MAIN GOAL OF THIS PRESCREPTIVE ANALYSIS IS TO UNDERSTAND HOW THOSE VARIABLES BEHAVE IN THE SAME PERIOD 
# IT IS POSSIBLE THERE MIGHT BE CORRELATION, BUT IT DOES NOT IMPLY CAUSATION 

#CORRELATION WITH EXTERNAL VARIABLES

# HOUSE INDEX PRICE DATASET 
mydata3= read.csv("data3.csv", header=T)
mydata3= as.data.frame(mydata3)

sapply(mydata3,class)
mydata3[,2:5]= sapply(mydata3[ ,2:5], as.numeric) # setting as numeric 
mydata3$Reference.Date=noquote(mydata3$Reference.Date)  # setting as date
mydata3$Reference.Date= my(as.character(mydata3$Reference.Date,label = TRUE, abbr = TRUE))

dev.off()
plot(mydata3$Base.Jan.2005...100~mydata2_modified$Homeless, xlab="House Price Index", ylab="Homeless", main = "House Price vs Homeless")
cor.test(mydata3$Base.Jan.2005...100,mydata2_modified$Homeless)

#It seems like there is a linear positive relationship between House Price Index and Homeless people
#In the same period, as the House price index increase, the number of hoemless also increase


# UNEMPLOYMENT RATE
mydata4= read.csv("data4.csv", header=T)
mydata4= as.data.frame(mydata4)

sapply(mydata4,class)
mydata4[,3:4]= sapply(mydata4[ ,3:4], as.numeric) # setting as numeric 
mydata4$Reference.Date=noquote(mydata4$Reference.Date)  # setting as date
mydata4$Reference.Date= my(as.character(mydata4$Reference.Date,label = TRUE, abbr = TRUE))


mydata4_grouped = mydata4 %>%
  group_by(Reference.Date) %>%
  summarise(Unemplyment_thousand = sum(Unemplyment_thousand),Unemplyment_percentage_rate=sum(Unemplyment_percentage_rate))

plot(mydata4_grouped$Unemplyment_thousand~mydata2_modified$Homeless, xlab="Unemployment in Thousand", ylab="Homeless", main = "Unemployment  vs Homeless")
plot(mydata4_grouped$Unemplyment_percentage_rate~mydata2_modified$Homeless, xlab="Unemplyment percentage rate", ylab="Homeless", main = "Unemployment Rate vs Homeless")

cor.test(mydata4_grouped$Unemplyment_thousand,mydata2_modified$Homeless)
cor.test(mydata4_grouped$Unemplyment_percentage_rate,mydata2_modified$Homeless)

#IN THE UNEMPLYMENT RATE, THE SCATTERPLOT SHOWS THAT THERE IS A NEGATIVE STRONG CORRELATION
# AS THE NUMBER OF UNEMPLOYMENT INCREASE, THE NUMBER OF HOMELESS DECREASE
# THAT'S BECAUSE THE UNEMPLYMENT RATE DECREASE OVER THE YEARS, WHILE THE NUMBER OF HOMELESS INCREASE
# AGAIN CORRELATION DOES NOT MEAN CAUSATION

#FINAL PRESCRIPTIVE ADVICE
#IN ORDER TO UNDERSTAND WHAT INFLUENCES THE FIGURES ON HOMELESSNESS IN IRELAND IT IS NECESSARY TO INVESTIGATE EXTERNAL FACTORS AND TO COLLECT MORE DATA
#PERHARBS A GOOD WAY TO DO THAT IS TO PERFORM A SURVEY WITH PEOPLE ACCESSING THE EMERGENCIAL ACCOMMODATION IN ORDER TO COLLECT INFORMATION AND TRY TO TRACE A PATTERN 

#VISUALIZATION
head(mydata2)
mydata2= mutate(mydata2, Year=year(Reference.Date), Month=month(Reference.Date))

mydata2_map = mydata2%>%
  group_by(Counties, Year)%>%
  summarise(Homeless=mean(Homesslessness.by.county), Latitude = unique(Latitude), Longitude=unique(Longitude))

  
  hover <- paste0(mydata2_map$Counties, " : ", 
                  as.character(mydata2_map$Homeless))
  
  pal = colorFactor(brewer.pal(4, 'Set2'), mydata2_map$Counties)

  
  leaflet() %>%
    addProviderTiles("OpenStreetMap")%>%
    addCircleMarkers(mydata2_map$Longitude,mydata2_map$Latitude, label = hover, group=mydata2_map$Year, col=pal(mydata2_map$Counties), weight = (mydata2_map$Homeless)/100)
  


  
  