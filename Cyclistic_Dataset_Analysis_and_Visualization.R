#Install packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("skimr")
install.packages("janitor")
install.packages("lubridate")


#Call packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(skimr)
library(janitor)
library(lubridate)

# Check the location of dataset and if location of dataset is outside the project data, change it using setwd() then load dataset
getwd()
setwd("C:/Users/Nisha/OneDrive/Documents/Data Analyst/Case Study_Google_Data_Analytics/Case Study-1/Cyclistic Project/Cyclistic Dataset")
df1 <- read_csv("cyclistic_2021_07.csv")
df2 <- read_csv("cyclistic_2021_08.csv")
df3 <- read_csv("cyclistic_2021_09.csv")
df4 <- read_csv("cyclistic_2021_10.csv")
df5 <- read_csv("cyclistic_2021_11.csv")
df6 <- read_csv("cyclistic_2021_12.csv")
df7 <- read_csv("cyclistic_2022_01.csv")
df8 <- read_csv("cyclistic_2022_02.csv")
df9 <- read_csv("cyclistic_2022_03.csv")
df10 <- read_csv("cyclistic_2022_04.csv")
df11 <- read_csv("cyclistic_2022_05.csv")
df12 <- read_csv("cyclistic_2022_06.csv")

#We are analysing data of july 2021 to june 2022 and all months have separate files, we have to merge all file. Before merging, I compared the data files to each other to check if they can be merged
compare_df_cols(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

#I Merged the files because we found that data's column and it's unit is matching.
Data <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

#Checked the structure of data
str(Data)

#We found that data in started_at and ended_at column is in POSXct and POSXIt so used strptime to update the column value to YMD, HMS
Data$started_at = strptime(Data$started_at, "%Y-%m-%d %H:%M:%S")
Data$ended_at = strptime(Data$ended_at, "%Y-%m-%d %H:%M:%S")

#Runned str() function to check if the data types converted correctly
str(Data)

#Now, it's time to check the accuracy of data. For this we calculate duration of ride and if duration is negative, that means that data row is not accurate.
Data <- mutate(Data, duration = difftime(ended_at, started_at, unit="secs"))

#Removed inaccurate data row
Data <- filter(Data, duration>0)
str(Data)

# Created Weekday column
Data <- mutate(Data, Weekday=weekdays(Data$started_at))

#Data_NA <- Data %>%
# na.omit(Data)

#Removed rows which have blank cell
Data_NA <- na.omit(Data)

#Added ridetime in minute column in the data file
Data_NA <- mutate(Data_NA, ride_time_min = as.numeric(Data_NA$ended_at -Data_NA$started_at)/60)

#summary of ride_time_min
summary(Data_NA$ride_time_min)


#Added month of the year column. 
Data_NA <- mutate(Data_NA, year_month = paste(strftime(Data_NA$started_at, "%Y"), "-", strftime(Data_NA$started_at, "%m"), paste("(",strftime(Data_NA$started_at, "%b"), ")", sep = " ")))

unique(Data_NA$year_month)

# Created a starting hour column. The starting hour will be useful to determine patterns of travels in the week.
Data_clean <- mutate(Data_NA, start_hour = strftime(Data_NA$ended_at, "%H"))
unique(Data_clean$start_hour)

#Clean up weird ride_time in minute data.
ventiles = quantile(Data_NA$ride_time_min, seq(0,1, by=0.05))
ventiles

Cyclistic <- Data_clean %>%
  filter(ride_time_min >as.numeric(ventiles['5%'])) %>%
  filter(ride_time_min < as.numeric(ventiles['95%']))

#Data_NA dataset doesn't have starting hour of ride column
#Data_clean contains min and max ride time in min. It has one column extra than Data_NA. i.e.starting hour column
#Cyclistic dastaset doesn't have max and min ride time in min
print(paste("Removed", nrow(Data_NA) - nrow(Cyclistic), "rows as outliers"))

Cyclistic %>%
  write.csv("Cyclistic.csv")

#To resize the kernel
fig <- function(width, height) {options (repr.plot.width = width, repr.plot.height = height)}

#to view first some rows of the data
head(Cyclistic)

#Comparison of member/annual riders versus casual riders
Cyclistic %>%
  group_by(member_casual) %>%
  summarise(count = length(ride_id), "%" = (length(ride_id)/nrow(Cyclistic)) *100)


#Graph for showing casual rider versus  annual/member riders
ggplot(data = Cyclistic)+
  geom_bar(mapping = aes(x=member_casual, color = member_casual, fill = member_casual))+
  labs(title ="Member vs. Casual Distribution") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))
 # annotate("text", member = "41.3%", casual = "58.7%")
 
#View the monthly distribution of members and casual riders
Cyclistic %>%
  group_by(year_month) %>%
  summarise(count = length(ride_id), '%' = (length(ride_id)/nrow(Cyclistic)) * 100, 'member_percent' = (sum(member_casual == "member")/length(ride_id)) * 100, 'casual_percent' = (sum(member_casual == "casual")/length(ride_id)) * 100, 'Member_vs_Casual_Percent' = member_percent - casual_percent)


#plot to view monthly distribution of annual and casual riders
ggplot(data = Cyclistic)+
  geom_bar(mapping = aes(x = year_month, fill = member_casual))+
  labs(x= "Month", title = "Distribution by Month")+
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1))

#Weekday Distribution of annual and casual riders
Cyclistic %>%
  group_by(Weekday) %>%
  summarise(count = length(ride_id), '%'=(length(ride_id) / nrow(Cyclistic)) * 100, 'member_percent' = (sum(member_casual == "member") / length(ride_id)) * 100, 'casual_percent' = (sum(member_casual == "casual") / length(ride_id)) * 100, 'Member_v_Casual_Percent' = member_percent - casual_percent) %>%
  arrange(Weekday)

#plot for Weekday Distribution of annual and casual riders
Cyclistic$Weekday <- factor(Cyclistic$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(data = Cyclistic) +
  geom_bar(mapping = aes(x = Weekday, fill = member_casual)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Distibution by Weekday") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))

#Hourly Distribution of annual and casual riders
Cyclistic %>%
  group_by(start_hour) %>%
  summarise(count = length(ride_id), '%' = (length(ride_id) / nrow(Cyclistic)) * 100, 'members_percent' = (sum(member_casual == "member") / length(ride_id))*100, 'casual_percent' = (sum(member_casual == "casual") / length(ride_id)) * 100, 'Member_v_Casual_Percent' = members_percent - casual_percent)

#Plot for hourly distribution of annual and casual riders
ggplot(data = Cyclistic) +
  geom_bar(mapping = aes(x= start_hour, fill = member_casual, )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5)) +
  labs(x = "Hour Of the Day", title = "Distribution by Hour") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))

#Plot for Hourly distribution by day
ggplot(data = Cyclistic) +
  geom_bar(mapping = aes(x = start_hour, fill = member_casual)) +
  facet_wrap(~ Weekday) +
  theme_gray(base_size = 5)
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Distribution by hour of the day divided by weekday", x = "Hour of the Day")


#Data distribution by bike type
Cyclistic %>%
  group_by(rideable_type) %>%
  summarise(count = length(ride_id), '%' = (length(ride_id) / nrow(Cyclistic))*100, 'members_percent' = sum(member_casual == "member") / length(ride_id) * 100, 'casual_percent' = sum(member_casual == "casual") / length(ride_id) * 100, 'Member_v_Casual_Percent' = members_percent - casual_percent)

#plot for data distribution by bike type
ggplot(data = Cyclistic) +
  geom_bar(mapping = aes(x = rideable_type, fill = member_casual)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Bike Type", title = "Distribution of Bike types") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))


#Rider type data distribution by ride time-->need to write analysis code
Cyclistic %>%
  group_by(member_casual) %>%
  summarise(mean = mean(ride_time_min), 'first_quarter' = as.numeric(quantile(ride_time_min, 0.25)), 'median' = median(ride_time_min), 'third_quarter' = as.numeric(ride_time_min, 0.75), 'IQR' = third_quarter - first_quarter)

#Plot for rider type data distribution by ride time 
ggplot(data = Cyclistic) +
  geom_boxplot(mapping = aes(x = member_casual, y = ride_time_min, fill =member_casual)) +
  labs(x ="Members vs Casual", y = "Ride Time (min)", title = "Distribution of Ride Time by Rider Type")


#Plot for ride time compare by weekday
ggplot(data = Cyclistic) +
  geom_boxplot(mapping = aes(x = ride_time_min, y = Weekday, fill = member_casual)) +
  facet_wrap(~member_casual) +
  labs(x = "Ride Time (min)", y = "Weekday", title = "Distribution of Ride Time by Weekday") 


#Plot for ride time compared by bike type
ggplot(data = Cyclistic) +
  geom_boxplot(mapping = aes(x = ride_time_min, y = rideable_type, fill = member_casual)) +
  facet_wrap(~member_casual) +
  labs(x = "Ride Time(min)", y = "Bike Type", title = "Distribution of Ride Time by Bike Type")


q()

