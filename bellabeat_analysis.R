# Install and run packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("magrittr")
library(tidyverse)
library(lubridate)
library(magrittr)
library(dplyr)
library(janitor)


# Creating data tables and getting their secifications
activities <- read_csv("/cloud/project/bellabeat_1/dailyActivity_merged.csv");

# sleep
sleep <- read_csv("/cloud/project/bellabeat_1/sleepDay_merged.csv");

# Calories
calories <- read_csv("/cloud/project/bellabeat_1/hourlyCalories_merged.csv");

# weigth
weight <- read_csv("/cloud/project/bellabeat_1/weightLogInfo_merged.csv");

# intensities
intensities <- read_csv("/cloud/project/bellabeat_1/hourlyIntensities_merged.csv")



# Specs
spec(activities);
spec(sleep);
spec(calories);
spec(weight);
spec(intensities);

# Check to ensure how many clients you have 
# From the data we are told we have about 30 customers who willing gave out their information

n_distinct(sleep$Id)
n_distinct(activities$Id)
n_distinct(calories$Id)
n_distinct(weight$Id)
n_distinct(intensities$Id)


# View the table structures
head(sleep)
head(activities)
head(weight)
head(calories)
head(intensities)

# Data Cleaning
# Date Formating
sleep$SleepDay <- parse_date_time(sleep$SleepDay, orders = "%m/%d/%Y %H:%M:%S %p")
sleep$date <- format(sleep$SleepDay, format("%m/%d/%Y"))

calories$ActivityHour <- parse_date_time(calories$ActivityHour, orders = "%m/%d/%Y %H:%M:%S %p")

activities$ActivityDate <- parse_date_time(activities$ActivityDate, orders = "%m/%d/%Y")
activities$date <- format(activities$ActivityDate, format("%m/%d/%Y"))

intensities$ActivityHour <- parse_date_time(intensities$ActivityHour, orders = "%m/%d/%Y %H:%M:%S %p")
intensities$date <- format(intensities$ActivityHour, format("%m/%d/%Y"))
intensities$time <- format(intensities$ActivityHour, format("%H:%M:%S"))

weight$Date <- parse_date_time(weight$Date, orders = "%m/%d/%Y %H:%M:%S %p")


# Clean column names
sleep <- sleep %>%
  clean_names()
calories <- calories %>%
  clean_names()
intensities <- intensities %>%
  clean_names()
activities <- activities %>%
  clean_names()
weight <- weight %>%
  clean_names()


# Sorting and organizing the data
sleep <- sleep %>% 
  arrange(sleep_day)
activities <- activities %>% 
  arrange(activity_date)
intensities <- intensities %>% 
  arrange(activity_hour)
weight <- weight %>% 
  arrange(date)
calories <- calories %>% 
  arrange(activity_hour)

# Get duplicates will be very important while aggregating the data
activities %>%
  get_dupes()
sleep %>%
  get_dupes() # Has duplicates
weight %>%
  get_dupes()
intensities %>%
  get_dupes()
calories %>%
  get_dupes()


# Viewing summaries
activities %>% 
  summarise(mean(very_active_minutes), mean(fairly_active_minutes), mean(lightly_active_minutes), mean(sedentary_minutes))

activities %>% 
  select(very_active_minutes, fairly_active_minutes, lightly_active_minutes, sedentary_minutes) %>% 
  summary()
activities %>% 
  select(total_distance, very_active_distance, moderately_active_distance, light_active_distance, sedentary_active_distance) %>% 
  summary()

activities %>% 
  select(total_steps, total_distance, tracker_distance, calories) %>% 
  summary()
weight %>%
  select(weight_kg, weight_pounds, bmi) %>% 
  summary()
intensities %>% 
  select(total_intensity, average_intensity) %>% 
  summary()
sleep %>% 
  select(total_sleep_records, total_minutes_asleep, total_time_in_bed) %>% 
  summary()

# Group By
# Average weight for each client
avg_weight <- weight %>% 
  group_by(Client_id <- id) %>% 
  summarise(weight_in_kg <- mean(weight_kg), weight_in_pounds <- mean(weight_pounds))

activities %>% 
  group_by(id) %>% 
  #reframe(mean(tracker_distance)) %>% 
  summarise(mean(tracker_distance), mean(total_distance))
  
activities %>% 
  group_by(id) %>% 
  summarise(mean(total_distance), mean(total_steps))
  #arrange(desc(total_distance))


#merging data
activity_sleep <- merge(sleep, activities, by=c('id', 'date'))

activity_sleep %>% 
  ggplot(mapping = aes(x=total_minutes_asleep, y=sedentary_minutes)) + geom_point(stat = "identity", fill="green") +
  geom_smooth() + labs(title = "Sedentary Minutes VS Asleep Minutes")

activities %>% 
  ggplot(mapping = aes(x = total_distance, y = moderately_active_distance, color = "blue")) +
  geom_line()
activities %>% 
  ggplot(mapping = aes(x = total_distance, y = light_active_distance, color ="green")) +
  geom_line()
activities %>% 
  ggplot(mapping = aes(x = total_distance, y = very_active_distance, color = "purple")) +
  geom_line()
activities %>% 
  ggplot(mapping = aes(x = total_distance, y = sedentary_active_distance, color = "orange")) + 
  geom_line()
activities %>% 
  ggplot(mapping = aes(x = total_steps)) +
  geom_histogram()

# Establishing a relationship between steps and calories
activities %>% 
  ggplot(mapping = aes(x = total_steps, y = calories)) +
  geom_point() + geom_smooth() +
  labs(title = "Total Steps VS Calories", x = "Total Steps", y = "Calories")

activities %>% 
  ggplot(mapping = aes(x=tracker_distance, y=calories)) + geom_point(stat = "identity", fill="skyblue") +
  geom_smooth(stat = "smooth", position = "identity", inherit.aes = TRUE) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Tracker Distance Vs Calories")


# Establishing the total minutes in bed and the total minutes asleep
sleep %>% 
  ggplot(mapping = aes(x = total_time_in_bed, y = total_minutes_asleep)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Total minutes asleep vs Total minutes in bed", x = "Minutes asleep", y = "Minutes in bed")

# Getting insights for the average intensity over the one month period
avg_int <- intensities %>% 
  group_by(date) %>% 
  drop_na() %>% 
  summarise(avg_intensity = mean(total_intensity))

avg_int %>% 
  ggplot(mapping = aes(x = date , y = avg_intensity)) + geom_histogram(stat = "identity", fill="darkgreen") +
  theme(axis.text.x = element_text(angle=90)) + 
  labs(title = "Daily Average Intensities")


# Hourly average intensity
hourly_int <- intensities %>% 
  group_by(time) %>% 
  drop_na() %>% 
  summarise(avg_inte = mean(total_intensity))

hourly_int %>% 
  ggplot(mapping = aes(x=time, y = avg_inte)) + geom_histogram(stat = "identity", fill = "brown") + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Hourly Intensity")



# Find out the average sleep minutes on a daily basis
daily_sleep <- sleep %>% 
  group_by(date) %>% 
  drop_na() %>% 
  summarise(mins_asleep = mean(total_minutes_asleep), mins_in_bed = mean(total_time_in_bed), sleep_record = mean(total_sleep_records))

daily_sleep %>% 
  ggplot(mapping = aes(x = date, y = mins_asleep)) + geom_histogram(stat = "identity", fill="blue") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Daily Average Minutes Asleep", x = "Date", y = "Average minutes asleep")

daily_sleep %>% 
  ggplot(mapping = aes(x = date, y = mins_in_bed)) + geom_histogram(stat = "identity", fill = "darkblue") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Daily Average Minutes Asleep", x = "Date", y = "Average minutes asleep")

daily_sleep %>% 
  ggplot(mapping = aes(x = date, y = sleep_record)) + geom_histogram(stat = "identity", fill = "darkblue") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Daily Average Minutes Asleep", x = "Date", y = "Sleep record")

