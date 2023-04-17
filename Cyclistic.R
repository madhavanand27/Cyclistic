#loading the libraries 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)

#Step1: Collecting the data 
#loading .csv files into the dataframe, 
mar_23 <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Data Analytics\\Capstone project\\case 1\\source\\202303-divvy-tripdata.csv")
feb_23 <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Data Analytics\\Capstone project\\case 1\\source\\202302-divvy-tripdata.csv")
jan_23 <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Data Analytics\\Capstone project\\case 1\\source\\202301-divvy-tripdata.csv")
dec_22 <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Data Analytics\\Capstone project\\case 1\\source\\202212-divvy-tripdata.csv")
nov_22 <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Data Analytics\\Capstone project\\case 1\\source\\202211-divvy-tripdata.csv")
oct_22 <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Data Analytics\\Capstone project\\case 1\\source\\202210-divvy-tripdata.csv")
sep_22 <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Data Analytics\\Capstone project\\case 1\\source\\202209-divvy-publictripdata.csv")
aug_22 <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Data Analytics\\Capstone project\\case 1\\source\\202208-divvy-tripdata.csv")
jul_22 <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Data Analytics\\Capstone project\\case 1\\source\\202207-divvy-tripdata.csv")
jun_22 <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Data Analytics\\Capstone project\\case 1\\source\\202206-divvy-tripdata.csv")
may_22 <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Data Analytics\\Capstone project\\case 1\\source\\202205-divvy-tripdata.csv")
apr_22 <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Data Analytics\\Capstone project\\case 1\\source\\202204-divvy-tripdata.csv")

#Step 2: Data wrangling and stacking the files into a new data frame 
cyclistic_df <- rbind(mar_23, feb_23, jan_23, dec_22, nov_22, oct_22, sep_22,
                      aug_22, jul_22, jun_22, may_22, apr_22)

str(cyclistic_df)
colnames(cyclistic_df)

#removing start_lat, start_lng, end_lat,end_lng
cyclistic_df <- cyclistic_df %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))
colnames(cyclistic_df)#verification 4 columns are removed

#Step3: Data cleaning stage 

#Data exploration
str(cyclistic_df)
head(cyclistic_df,3)
summary(cyclistic_df)# to get summary statistics
unique(cyclistic_df$member_casual)
unique(cyclistic_df$rideable_type)
table(cyclistic_df$member_casual)#to know how many observation fall under each user type
 
cyclistic_df$member_casual <- ifelse(cyclistic_df$member_casual == 
                                       'casual', 'Customer', 'Subscriber')
#Casual user type are now named as customers and members are now named as Subscribers
table(cyclistic_df$member_casual)# verification, there are similar observations 

#adding columns that lists the date, month, day, year, and weekdays

cyclistic_df$Date <- as.Date(cyclistic_df$started_at)
cyclistic_df$Day <- format(cyclistic_df$Date, "%d") #day column
cyclistic_df$Month <- format(cyclistic_df$Date, "%m")#month column
cyclistic_df$Year <- format(cyclistic_df$Date, "%y")#Year column 
cyclistic_df$weekdays <- weekdays(cyclistic_df$Date)#weekdays column

#adding a new metric ride_length into data frame
cyclistic_df <- cyclistic_df %>% 
  mutate(ride_length = ended_at - started_at)
str(cyclistic_df)
colnames(cyclistic_df)
cyclistic_df_2 <- cyclistic_df[!(cyclistic_df$start_station_name == "HQ RQ"|
                               cyclistic_df$ride_length < 0),]

#converting ride_length to numeric variable 

cyclistic_df_2$ride_length <- as.numeric(cyclistic_df_2$ride_length)
str(cyclistic_df_2)

cyclistic_df_2 <- separate(cyclistic_df_2, ride_length, 
                           into = c('ride_length_in_sec', 'sec'), sep = " ")
cyclistic_df_2$ride_length_in_sec <- 
  as.numeric(cyclistic_df_2$ride_length_in_sec)
cyclistic_df_2 <- cyclistic_df_2 %>% select(-c(sec))#removing the temporary column
#converting weekdays into factor
cyclistic_df_2$weekdays <- as.factor(cyclistic_df_2$weekdays)
#STEP 4: Conducting descriptive analysis
summary(cyclistic_df_2$ride_length_in_sec)

aggregate(cyclistic_df_2$ride_length_in_sec ~ cyclistic_df_2$member_casual, FUN = mean)
aggregate(cyclistic_df_2$ride_length_in_sec ~ cyclistic_df_2$member_casual, FUN = median)
aggregate(cyclistic_df_2$ride_length_in_sec ~ cyclistic_df_2$member_casual, FUN = max)
aggregate(cyclistic_df_2$ride_length_in_sec ~ cyclistic_df_2$member_casual, FUN = min)

#average ride time by each day for customers vs subscribers
#first ordering the values in weekdays column 
cyclistic_df_2$weekdays <- 
  ordered(cyclistic_df_2$weekdays, levels = c("Sunday","Monday", "Tuesday", 
                                              "Wednesday", "Thursday", "Friday", 
                                              "Saturday")) 
aggregate (cyclistic_df_2$ride_length_in_sec ~ cyclistic_df_2$member_casual + 
             cyclistic_df_2$weekdays, FUN = mean)

#analyzing data set by type and weekdays
final_analysis <- cyclistic_df_2 %>% 
  group_by(member_casual, weekdays) %>% 
  summarize(number_of_rides = n(), average_duration =  mean(ride_length_in_sec)) %>% 
  arrange(member_casual, weekdays) %>%
  drop_na()
View(final_analysis)


#visualizing number of rides by rider type
ggplot(final_analysis, aes(x = weekdays, y = number_of_rides, fill = member_casual))+
  geom_col(position = "dodge", alpha = 0.6)+
  theme(axis.text.x = element_text(angle = 45), legend.title = element_blank())+
  labs(title = "Number of Rides by Rider Type",
       subtitle = "The chart shows the number of riders using bikes by days in a whole year",
      caption = "From April 2022 to March 2023" , x = "Days", y = "Number of Rides")+
  scale_y_continuous(label = scales::comma)



ggplot(final_analysis, aes(x = weekdays, y = number_of_rides, fill = member_casual))+
  geom_col(position = "dodge", alpha = 0.6)+
  facet_wrap(~member_casual)+
  theme(axis.text.x = element_text(angle = 90), legend.title = element_blank(),
        legend.position = "none")+
  labs(title = "Number of Rides by Rider Type",
       subtitle = "The chart shows the number of riders using bikes by days in a whole year",
       caption = "From April 2022 to March 2023" , x = "Days", y = "Number of Rides")+
  scale_y_continuous(label = scales::comma)# to convert from scientific notation to readable format


#visualization for average duration

ggplot(final_analysis, aes(x = weekdays, y = average_duration, fill = member_casual))+
  geom_col(position = "dodge", alpha = 0.5)+
  labs(title = "Average Duration Spent by Rider Type ", 
       subtitle = "The chart demonstrates average duration spent by riders by days in a whole year",
       caption = "From April 2022 to March 2023", x = "Days", y = "Average Duration in Seconds")+
  theme(axis.text.x = element_text(angle = 45), legend.title  = element_blank())+
  scale_fill_manual(values = c("blue", "red"))


ggplot(final_analysis, aes(x = weekdays, y = average_duration, fill = member_casual))+
  geom_col(position = "dodge", alpha = 0.6)+
  facet_wrap(~member_casual)+
  labs(title = "Average Duration Spent by Rider Type ", 
       subtitle = "The chart demonstrates average duration spent by riders by days in a whole year",
       caption = "From April 2022 to March 2023", x = "Days", y = "Average Duration in Seconds")+
  theme(axis.text.x = element_text(angle = 90), legend.title  = element_blank(), legend.position = "none")+
  scale_fill_manual(values = c("blue", "red"))

#Step 5: exporting final_analysis to the system location for further analysis or exploration 
write_csv(final_analysis,"C:\\Users\\User\\OneDrive\\Desktop\\Data Analytics\\Capstone project\\case 1\\final_data set\\Final_Analysis.csv")
write_csv(cyclistic_df_2, "C:\\Users\\User\\OneDrive\\Desktop\\Data Analytics\\Capstone project\\case 1\\final_data set.Cyclistic_Final.csv")








