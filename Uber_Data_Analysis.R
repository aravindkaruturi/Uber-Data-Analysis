# Uber Data Analysis 
#____________________#

#Bases (Classifying Uber trips as small,medium and large)
#B02512 - Small
#B02598 - Medium
#B02617 - Medium
#B02682 - Medium
#B02764 - Large
#refer above

# Importing the Essential packages
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("dplyr")
install.packages("lubridate")
install.packages("tidyr")
install.packages("DT")
install.packages("scales")

library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

#Creating vector of colors to be implemented in our plots
colors <- c("red","brown","green","yellow","grey","steelblue","purple")
head(colors)

# Reading the data into their designated variables
apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

data_2014 <- rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)
str(data_2014)
 
# Formatting Date.Time variable of the data and create factors of time objects like day.,month,year etc.
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time,format= "%m/%d/%Y %H:%M:%S")
str(data_2014)

data_2014$Time <- format(data_2014$Date.Time,format = "%H:%M:%S")
str(data_2014)

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)
str(data_2014)

data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time,label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time,label = TRUE))
str(data_2014)

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))
str(data_2014)

# Plotting the trips by the hours in a day (To identify peak hours of the day)
hour_data <- data_2014 %>% group_by(hour) %>% summarise(Total = n())
datatable(hour_data)

ggplot(hour_data,aes(x=hour,y=Total)) + 
        geom_bar(stat = "identity",fill="steelblue",color="red") +
        ggtitle("Trips Every Hour") +
        theme(legend.position = "none") +
        scale_y_continuous(labels = comma) +
       geom_hline(yintercept = quantile(hour_data$Total,0.75))

# Plotting the trips by hour and month 
month_hour <- data_2014 %>% group_by(month,hour) %>% summarise(Total = n())
datatable(month_hour)

ggplot(month_hour,aes(x=hour,y=Total,fill=month)) +
        geom_bar(stat = "identity") +
        ggtitle("Trips by hour and month") +
        scale_y_continuous(labels = comma)

# Plotting trips during every day of the month
day_group <- data_2014 %>% group_by(day) %>% summarise(Total = n())
datatable(day_group)

ggplot(day_group,aes(x=day,y=Total)) +
        geom_bar(stat = "identity",fill="steelblue",color="red") +
        ggtitle("Trips Every Day") + 
        theme(legend.position = "none") +
        scale_y_continuous(labels = comma)

# Plotting trips for every day of week 
weekday_group <- data_2014 %>% group_by(dayofweek) %>% summarise(Total=n())
datatable(weekday_group)

ggplot(weekday_group,aes(x=dayofweek,y=Total)) +
        geom_bar(stat = "identity",fill="steelblue",color="red") +
        ggtitle("Trips Every Day of week") + 
        theme(legend.position = "none") +
        scale_y_continuous(labels = comma)

# Plotting trips during every day for each month
month_day <- data_2014 %>% group_by(month,day) %>% summarise(Total = n())
datatable(month_day)

ggplot(month_day,aes(x=day,y=Total,fill=month)) +
        geom_bar(stat = "identity") +
        ggtitle("Trips on each day month wise") +
        scale_y_continuous(labels = comma)

# Plotting trips taken during every month
month_group <- data_2014 %>% group_by(month) %>% summarise(Total = n())
datatable(month_group)

ggplot(month_group,aes(x=month,y=Total,fill=month)) +
        geom_bar(stat = "identity") +
        ggtitle("Trips by month") +
        theme(legend.position = "none") +
        scale_y_continuous(labels = comma) + 
        scale_fill_manual(values = colors)
       
# Plotting trips on every day of week in each month
weekday_month_group <- data_2014 %>% group_by(month,dayofweek) %>% summarise(Total = n())
datatable(weekday_month_group)

ggplot(weekday_month_group,aes(x=month,y=Total,fill=dayofweek)) +
        geom_bar(stat = "identity",position = "dodge") +
        ggtitle("Trips day wise for every month ") +
        scale_y_continuous(labels = comma) +
        scale_fill_manual(values = colors)

# Plotting trips by bases
base_group <- data_2014 %>% group_by(Base) %>% summarise(Total=n())
datatable(base_group)

ggplot(base_group,aes(x=Base,y=Total)) +
        geom_bar(stat = "identity",fill="steelblue") +
        ggtitle("Trips by bases") +
        scale_y_continuous(labels = comma)

# Plotting trips per base month-wise
month_base_group <- data_2014 %>% group_by(Base,month) %>% summarise(Total=n())
datatable(month_base_group)

ggplot(month_base_group,aes(x=Base,y=Total,fill=month)) +
        geom_bar(stat = "identity",position = "dodge") +
        ggtitle("Trips month wise on each base") +
        scale_y_continuous(labels = comma) +
        scale_fill_manual(values = colors)

# Plotting trips per base on each day of week
weekday_base_group <- data_2014 %>% group_by(Base,dayofweek) %>% summarise(Total=n())
datatable(weekday_base_group)

ggplot(weekday_base_group,aes(x=Base,y=Total,fill=dayofweek)) +
        geom_bar(stat = "identity",position = "dodge") +
        ggtitle("Trips per base on each day of week") +
        scale_y_continuous(labels = comma) +
        scale_fill_manual(values = colors)

##   Heat Maps

# Plotting heat map by hour and day
day_and_hour <- data_2014 %>% group_by(day,hour) %>% summarise(Total=n())
datatable(day_and_hour)

ggplot(day_and_hour,aes(x=day,y=hour,fill=Total)) +
        geom_tile(color = "white") + 
        ggtitle("Heat map by hour and day")

# Plotting heat map by month and day
ggplot(month_day,aes(x=day,y=month,fill=Total)) +
        geom_tile(color="white") +
        ggtitle("Hear map by month and day")

# Plotting heat map by month and day of week
ggplot(weekday_month_group,aes(x=dayofweek,y=month,fill=Total)) +
        geom_tile(color="white") +
        ggtitle("Heat map by month and day of week")

# Plotting heat map by month and base
ggplot(month_base_group,aes(x=Base,y=month,fill=Total)) +
        geom_tile(color="white") +
        ggtitle("Heat map by month and base") 

# Plotting heat map by Bases and days of week
ggplot(weekday_base_group,aes(x=Base,y=dayofweek,fill=Total)) +
        geom_tile(color = "white") +
        ggtitle("Heat map by bases and days of week")

##  Map Visualization of Uber rides in New York

# New York map based on Uber rides (Apr-Sep)
#min_lat <- min(data_2014$Lat)
#max_lat <- max(data_2014$Lat)
#min_lon <- min(data_2014$Lon)
#max_lon <- max(data_2014$Lon)

#As data is large, we are taking arbitrary values for limits of lattitudes and longitudes
min_lat <- 40.5774
max_lat <- 40.65
min_lon <- -74.15
max_lon <- -73.90

ggplot(data_2014,aes(x=Lon,y=Lat)) +
        geom_point(size=1,color="blue") +
        scale_x_continuous(limits = c(min_lon,max_lon)) +
        scale_y_continuous(limits = c(min_lat,max_lat)) +
        theme_map() +
        ggtitle("New York map based on uber rides (Apr-Sep,2014) ")
ggplot(data_2014,aes(x=Lon,y=Lat,color=Base)) +
        geom_point(size=1) +
        scale_x_continuous(limits = c(min_lon,max_lon)) +
        scale_y_continuous(limits = c(min_lat,max_lat)) +
        theme_map() +
        ggtitle("New York map based on uber rides (Apr-Sep,2014) by Base ")



#______________________________#
#Inferences ...................#

# 1. Evening 4:00 to 6:00 PM are the peak hours
# 2. 30th of every month has high trips compared to other days
# 3. September month has highest number of trips
# 4. Base: B02617 has highest number of trips 
# 5. Medium type of bases have more number of trips compared to small and large
# 6. Thursday closely followed by friday have more trips than other days
# 7. With the help of map visualization of trips, we can identify locations which have high density of trips (base wise)





        



     