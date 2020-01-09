library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(tseries)
library(lubridate)

traffic_violation <- read_csv('/Users/Rina/Documents/Personal projects/Traffic_Violations.csv')
traffic_df <- data.frame(traffic_violation)

#Extract/Transform Data and Time of Stop

#Based on rounak041993 (2017) Traffic Violations Analysis (Version 17)[Source Code].https://www.kaggle.com/rounak041993/traffic-violations-analysis

traffic_df[,"Date.Of.Stop"]<-as.Date(traffic_df[,"Date.Of.Stop"],format="%m/%d/%Y")
traffic_df$month<-format(traffic_df[,"Date.Of.Stop"],"%B")
traffic_df$month_code<-as.numeric(format(traffic_df[,"Date.Of.Stop"],"%m"))

traffic_df$year<-as.numeric(format(traffic_df[,"Date.Of.Stop"],"%Y"))

traffic_df$hour<-hour(hms(as.character(traffic_df[,"Time.Of.Stop"])))+1

traffic_df$weekday<-as.numeric(format(traffic_df[,"Date.Of.Stop"],"%u"))

traffic_df$weekday_full<-format(traffic_df[,"Date.Of.Stop"],"%a")

traffic_df


#Let's explore traffic violations that Contributed To Accidents:

AccidentDF <- filter(traffic_df, Contributed.To.Accident %in% c("Yes"))
AccidentDF

accident_month_yr<-data.frame(table(AccidentDF$month_code,AccidentDF$year))
names(accident_month_yr)<-c("month","year","count")
accident_month_yr
timeser_accidents_MY=ts(accident_month_yr[which(accident_month_yr$count!=0),3],frequency=12,start=c(2012,1))
print(accident_month_yr)

#Grouped barplot first

AMY_df <- as.data.frame(accident_month_yr)

ggplot(data = AMY_df, aes(x=month, y = count, fill=year)) + 
  geom_bar(stat="identity", position = "dodge")+
  scale_x_discrete("", labels = c("Jan", "Feb", "March", "Apr", 
                                  "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  theme(axis.text.x=element_text(color = "black", size=8, angle=45, vjust=.8, hjust=0.8))+
  scale_fill_brewer(palette = "Set1")+
  labs(title = "Total Traffic Accidents \n from 2012-2018")

#What does the distribution look like over hour?

ggplot(AccidentDF, aes(x = AccidentDF$hour)) + 
  geom_density(fill = "orange", alpha = 0.5)+
  xlim(0,24)+
  xlab(label = "Hour")+
  labs(title = "Total Traffic Accidents")

#timeseries plot:

#Timeseries let's look out the hour of the accidents occuring each year

tab_hr_accidents<-data.frame(table(AccidentDF$year,AccidentDF$hour))
names(tab_hr_accidents)<-c("year","hour","count")

tab_hr_accidents<-tab_hr_accidents[order(tab_hr_accidents$year),]


time_hr_accidents=ts(tab_hr_accidents[which(tab_hr_accidents$count!=0),3],frequency=24,start=c(2012,1),end=c(2018,24))

names(tab_hr_accidents)<-c("year","hour","count")

tab_hr_accidents<-tab_hr_accidents[order(tab_hr_accidents$year),]


time_hr_accidents=ts(tab_hr_accidents[which(tab_hr_accidents$count!=0),3],frequency=24,start=c(2012,1),end=c(2018,24))
print(time_hr_accidents)


plot(aggregate(time_hr_accidents,FUN=mean),ylab="# of Traffic Accidents", 
     xlab = "Year", main = "Average Traffic Accidents per Year", ylim = c(50,300),
     lty=2,lwd=2,col="red")
grid()

boxplot(time_hr_accidents~cycle(time_hr_accidents),xlab="Hours",ylab="Traffic Accidents hourly",col="#00AFBB")

str(traffic_df)

#PCA analysis

#Going to look at 2017 only bc it has the highest number of accidents

#Pivoting Tables:
#The row key in a pivot table is group_by
#Summarize/summarise is the equivalent to value - adding numeric fields
#count = n() count the number of occurences

traffic_2017_df <- traffic_df %>%
  filter(Year == "2017")

Accidents_2017DF <- traffic_2017_df %>%
  filter(Contributed.To.Accident == "Yes")

F_TAccidents17_hour <- Accidents_2017DF %>%
  filter(Gender == "F") %>%
  group_by(hour) %>%
  summarize(count = n())

F_TAccidents17_hour

M_TAccidents17_hour <- Accidents_2017DF %>%
  filter(Gender == "M") %>%
  group_by(hour) %>%
  summarize(count = n())

M_TAccidents17_hour

names(F_TAccidents17_hour) <- c("hour", "F_accidents")
F_TAccidents17_hour

names(M_TAccidents17_hour)
names(M_TAccidents17_hour) <- c("hour", "M_accidents")
M_TAccidents17_hour

Gender_Accidents17 <- F_TAccidents17_hour %>% inner_join(M_TAccidents17_hour)
Gender_Accidents17df <- as.data.frame(Gender_Accidents17)
Gender_Accidents17df

#dplyr. Join two tbls together. https://dplyr.tidyverse.org/reference/join.html


#now need it to be a matrix for PCA
#Based on Carr, D. Principal Components 2D.

mat_Gender_accident <- as.matrix(sapply(Gender_Accidents17df[,c(2,3)], as.numeric))  
mat_Gender_accident

#Got rid of hour but will add it as a row
rownames(mat_Gender_accident) <- c(Gender_Accidents17df$hour)
mat_Gender_accident

#calculate mean

male_accidentM <- mean(Gender_Accidents17df$M_accidents)
female_accidentM <- mean(Gender_Accidents17df$F_accidents)

male_accidentM
female_accidentM

G_accidentmatCen <- scale(mat_Gender_accident,scale=FALSE)
G_accidentdfCen <- as.data.frame(G_accidentmatCen)
head(G_accidentdfCen)

ggplot(Gender_Accidents17df,aes(x=M_accidents,y=F_accidents))+
  geom_hline(yintercept=female_accidentM,color='blue')+ 
  geom_vline(xintercept=male_accidentM,color='blue')+
  geom_point(fill="red",shape=21)


ggplot(G_accidentdfCen,aes(x=M_accidents,y=F_accidents))+
  geom_hline(yintercept=0,color='blue')+ 
  geom_vline(xintercept=0,color='blue')+
  geom_point(fill="red",shape=21)+
  labs(x="Male - Mean(Male)",
       y="Female - mean(Female)")

pc <- prcomp(mat_Gender_accident,scale=FALSE)
pcDat <- pc$x # principal components
pcRotate <- pc$rotation  #rotation matrix

det(pcRotate)

matRot <- G_accidentmatCen %*% pcRotate
head(pcDat)
head(matRot)
all.equal(pcDat,matRot)

round(pcRotate,2)

a <- -pcRotate[1,1]
asin(a)*180/pi

big <- max(abs(pcDat))
big

ggplot(G_accidentdfCen,aes(x,y))+
  geom_hline(yintercept=0,color='blue')+ 
  geom_vline(xintercept=0,color='blue')+
  geom_point(fill="red",shape=21)+
  ylim(-big,big)+
  xlim(-big,big)+
  labs(x="Male - Mean(Male)",
       y="Female - mean(Female)")


biplot(pc,scale=0,las=1,
       main= "Covariance PC Loadings Top and Right Axes")
head(pc$x)
pc$rot

pcCor <- prcomp(mat_Gender_accident,scale=TRUE)
biplot(pcCor,scale=1,las=1, 
       main="Correlation PC Loadings Top and Right Axes")
#shows with scale


pc$sdev
screeplot(pc)
#screeplot to see the variance of the 2 principal components


#Personal Injury

personalinjury_df <- filter(traffic_df, Personal.Injury %in% c("Yes"))
personalinjury_df

PI_MY<-data.frame(table(personalinjury_df$month_code,personalinjury_df$year))
names(PI_MY)<-c("month","year","count")
PI_MY

PI_MY_df <- as.data.frame(PI_MY)
PI_MY_df 

ggplot(data = PI_MY_df, aes(x=month, y = count, fill=year)) + 
  geom_bar(stat="identity")

ggplot(data = PI_MY_df, aes(x=month, y = count, fill=year)) + 
  geom_bar(stat="identity", position = "dodge")+
  scale_x_discrete("", labels = c("Jan", "Feb", "March", "Apr", 
                                  "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  theme(axis.text.x=element_text(color = "black", size=8, angle=45, vjust=.8, hjust=0.8))+
  scale_fill_brewer(palette = "Set1")+
  labs(title = "Total Traffic incidences Resulting in Personal Injuries \n from 2012-2018")

time_ser_PIMY=ts(PI_MY[which(PI_MY$count!=0),3],frequency=12,start=c(2012,1))
print(time_ser_PIMY)

plot(time_ser_PIMY,ylab="Total Traffic Injuries",xlab = "Year", pch=5,lwd=2,col="#00AFBB")
abline(reg=lm(time_ser_PIMY~time(time_ser_PIMY)),col="red",lty=2, lwd=3)

plot(aggregate(time_ser_PIMY,FUN=mean),ylab="Traffic Personal Injury Trend ",xlab = "Year", lty=2,lwd=2,col="red")
boxplot(time_ser_PIMY~cycle(time_ser_PIMY),ylab="Traffic Personal Injury Seasonality")

ggplot(personalinjury_df, aes(x = personalinjury_df$hour)) + 
  geom_density(fill = "red", alpha = 0.5)+
  xlim(0,24)+
  xlab(label = "Hour")+
  labs(title = "Personal Injuries throughout the hour")

library(tidyverse)

Injury_df <- traffic_df %>%
  dplyr::select(Personal.Injury, Belts, Contributed.To.Accident, Alcohol, Work.Zone, Property.Damage)


#logit model for pI

#Based on Rawat A. (2017) Binary Logistic Regression. https://towardsdatascience.com/implementing-binary-logistic-regression-in-r-7d802a9d98fe

Injury_df$Personal.Injury <- as.factor(Injury_df$Personal.Injury)
Injury_df$Belts <- as.factor(Injury_df$Belts)
Injury_df$Contributed.To.Accident <- as.factor(Injury_df$Contributed.To.Accident)
Injury_df$Alcohol <- as.factor(Injury_df$Alcohol)
Injury_df$Work.Zone <- as.factor(Injury_df$Work.Zone)
Injury_df$Property.Damage <- as.factor(Injury_df$Property.Damage)


Injury_logit <- glm(Personal.Injury ~., data = Injury_df, family = "binomial")
summary(Injury_logit)

#Based on atmatthew (2015) Evaluating Logistic Regression Models. https://www.r-bloggers.com/evaluating-logistic-regression-models/
library(caret)
varImp(Injury_logit)
coef(Injury_logit)

library(rpart)
InjuryModel1 = rpart(Personal.Injury~. , data=Injury_df, cp=.001)

library(rpart.plot)
rpart.plot(InjuryModel1, varlen=12, extra=2, main="Personal Injury Classification Tree")

#Cluster Analysis on Geolocation

#Based on Carr. D. Cluster Methods, Data and Graphics

Accident2017DF <- AccidentDF %>%
  filter(year == "2017")

lat_long_accident17 <- Accident2017DF %>%
  drop_na (Latitude, Longitude)

lat_long_accident17

accidentlatlong17_only <- lat_long_accident17 %>%
  dplyr::select(Latitude, Longitude)

library("factoextra")
library(useful)

set.seed(123)

km2 <- kmeans(accidentlatlong17_only, centers=2, nstart = 20)
km3 <- kmeans(accidentlatlong17_only, centers=3, nstart = 20)
km4 <- kmeans(accidentlatlong17_only, centers=4, nstart = 20)
km5 <- kmeans(accidentlatlong17_only, centers=5, nstart = 20)
km6 <- kmeans(accidentlatlong17_only, centers=6, nstart = 20)
km7 <- kmeans(accidentlatlong17_only, centers=7, nstart = 20)
km8 <- kmeans(accidentlatlong17_only, centers=8, nstart = 20)
km9 <- kmeans(accidentlatlong17_only, centers=9, nstart = 20)


# Look at the result for three clusters
names(km3)

km2$centers

km5$centers

km3$centers
km3$cluster # a vector give case cluster membership cluster
km3$cluster

# Within Sum of Squares calculation
km3C2 <- accidentlatlong17_only[km3$cluster==2,]
km3C2Cen <- km3$centers[2,]
km3C2Within <- sum(
  scale(km3C2,center=km3C2Cen,scale=FALSE)^2)
km3$withinss

# Total Sum of squares calculation
sum( scale(accidentlatlong17_only,scale=FALSE)^2)
km3$totss


# Comparing within cluster total within sum of squares
# as the number of clusters increases

plot(2:9, c(km2[5], km3[5], km4[5], km5[5],
            km6[5], km7[5], km8[5],km9[5]),
     main = "K Means:  How many clusters?",
     las=1, xlab="Number of Clusters", 
     ylab="Within Clusters Total Sums of Squares")

library(useful)

GeoBest <- FitKMeans(accidentlatlong17_only, max.clusters=20,nstart=25,seed=43)
GeoBest 
PlotHartigan(GeoBest)

fviz_cluster(km5, accidentlatlong17_only, stand = FALSE, ellipse = FALSE, geom = "point",
             main = "Cluster plot of Traffic Accidents in 2017")

devtools::install_github("UrbanInstitute/urbnmapr")
library(tidyverse)
library(urbnmapr)

state_counties <- get_urbn_map(map = "counties", sf = FALSE)
MD <- state_counties %>%
  filter(state_abbv == "MD")

MDC <- MD %>%
  filter(county_name == "Montgomery County")

County_Map <- MDC %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

County_Map

k5cords <- as.data.frame(km5$centers)
k4cords <- as.data.frame(km4$centers)

cluster4map <- County_Map + geom_point(data = k4cords,
                                       aes(x = Longitude, y = Latitude), inherit.aes = FALSE)+
  labs(title = "Centers of 4 Clusters of Accidents in 2017 \n in Montgomery County MD")

cluster4map


cluster5map <- County_Map + geom_point(data = k5cords,
                        aes(x = Longitude, y = Latitude), inherit.aes = FALSE)+
  labs(title = "Centers of 5 Clusters of Accidents in 2017 \n in Montgomery County MD")

cluster5map

library(plotly)
ggplotly(cluster5map)

PI17 <- personalinjury_df %>%
  filter(Year == "2017")

injury_latlong17 <- PI17 %>%
  drop_na(Latitude, Longitude)

injurylatlong17_only <- injury_latlong17 %>%
  dplyr::select(Latitude, Longitude)


km2_injury <- kmeans(injurylatlong17_only, centers=2, nstart = 20)
km3_injury <- kmeans(injurylatlong17_only, centers=3, nstart = 20)
km4_injury <- kmeans(injurylatlong17_only, centers=4, nstart = 20)
km5_injury <- kmeans(injurylatlong17_only, centers=5, nstart = 20)
km6_injury <- kmeans(injurylatlong17_only, centers=6, nstart = 20)
km7_injury <- kmeans(injurylatlong17_only, centers=7, nstart = 20)
km8_injury <- kmeans(injurylatlong17_only, centers=8, nstart = 20)
km9_injury <- kmeans(injurylatlong17_only, centers=9, nstart = 20)


plot(2:9, c(km2_injury[5], km3_injury[5], km4_injury[5], km5_injury[5],
            km6_injury[5], km7_injury[5], km8_injury[5],km9_injury[5]),
     main = "K Means:  How many clusters?",
     las=1, xlab="Number of Clusters", 
     ylab="Within Clusters Total Sums of Squares")

InjuryBest <- FitKMeans(injurylatlong17_only, max.clusters=20,nstart=25,seed=43)
InjuryBest 
PlotHartigan(InjuryBest)

km4_injury$centers
k4_injurycords <- as.data.frame(km4_injury$centers)


cluster4map + geom_point(data = k4_injurycords,
                         aes(x = Longitude, y = Latitude), inherit.aes = FALSE, col = "blue")+
  labs(title = "Center of 4 Clusters of Accidents and Personal Injuries\n from 2017",
       caption = "Black displaying Traffic Accidents \nBlue displaying Personal Injury")

km5_injury$centers
k5_injurycords <- as.data.frame(km5_injury$centers)


Cluster5_injury_accident <- cluster5map + geom_point(data = k5_injurycords,
                         aes(x = Longitude, y = Latitude), inherit.aes = FALSE, col = "blue")+
  labs(title = "Center of 5 Clusters of Accidents and Personal Injuries\n from 2017",
       caption = "Black displaying Traffic Accidents\n Blue displaying Personal Injury")

ggplotly(Cluster5_injury_accident)

citation("plotly")
