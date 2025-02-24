## Bellebeat Case Study Data Analysis

## Introduction
Bellabeat is a _Smart Device_ manufacturer for women's smart devices. These are health wellness wearable used to track physical activity, sleep, stress, menstrual cycle and mindful habits. Bellabeat is a successful start up with the ability to growing to becoming a global company. The Chief Creative Officer and co-founder of Bellabeat Urška Sršen, believes that analyzing the available data on smart devices will help identify usage trends and patterns that will help the company identify expansion and growth opportunities and design a better marketing strategy.

## Analysis Questions
1. What are the trends and patterns in usage of smart devices?
2. How do these trends and patterns correlate with Bellabeat's smart devices?
3. How will these trends impact Bellabeats' marketing strategy?

## Business Task
Identify potential growth opportunities and recommendations for the  marketing strategy for the Bellabeat company.

## Installing and Loading Packages

During the analysis process, I used the following standardized R package libraries for the data cleaning and processing and visualization:

a. tidyverse
b. lubridate
c. dplyr
d. magritrr
To install and load them:

```{r}
# Install
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("magrittr")

# Load
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(magrittr)
library(janitor)
```

## Loading Datasets
I imported the following datasets from the data given since they were most in alignment with the goals of Bellabeats' technology:
i. activities - Imported from dailyActivity_merged.csv file
ii. calories - imported from the hourlyCalories_merged.csv" file.
iii. intensities - imported from the hourlyIntensities_merged.csv file
iv. weight - imported from weightLogInfo_merged.csv file
v. sleep - imported from sleepDay_merged.csv file

_*Note:* All the files are in the bellabeat 1 folder which are the most current_
```{r}
# Creating data tables and getting their secifications
# ~/Desktop/projects_data_analysis/Bellabeat-Case-Study---Capstone-Project/bellabeat_1
# /cloud/project/bellabeat_1
activities <- read_csv("~/Desktop/projects_data_analysis/Bellabeat-Case-Study---Capstone-Project/bellabeat_1/dailyActivity_merged.csv")
# Calories
calories <- read_csv("~/Desktop/projects_data_analysis/Bellabeat-Case-Study---Capstone-Project/bellabeat_1/hourlyCalories_merged.csv")
# intensities
intensities <- read_csv("~/Desktop/projects_data_analysis/Bellabeat-Case-Study---Capstone-Project/bellabeat_1/hourlyIntensities_merged.csv")
# sleep
sleep <- read_csv("~/Desktop/projects_data_analysis/Bellabeat-Case-Study---Capstone-Project/bellabeat_1/sleepDay_merged.csv")
# weigth
weight <- read_csv("~/Desktop/projects_data_analysis/Bellabeat-Case-Study---Capstone-Project/bellabeat_1/weightLogInfo_merged.csv")
```

I used the spec(), and head functions to check on the dataset specifications and viewing its structure.

```{r}
# Specs, then do for each table
spec(activities)
```
```{r}
# Sample, then do for each table
head(activities)
```

## Data Cleaning

#### Date Time 
I realised there was an error in the date-time columns since date was stored in character format. I changed the dates into date time fields. I also had to split the datetime to separate fields in like date and time to display the day and time respectively.

```{r}
sleep$SleepDay <- parse_date_time(sleep$SleepDay, orders = "%m/%d/%Y %H:%M:%S %p")
sleep$date <- format(sleep$SleepDay, format("%m/%d/%Y"))
calories$ActivityHour <- parse_date_time(calories$ActivityHour, orders = "%m/%d/%Y %H:%M:%S %p")
activities$ActivityDate <- parse_date_time(activities$ActivityDate, orders = "%m/%d/%Y")
activities$date <- format(activities$ActivityDate, format("%m/%d/%Y"))
# Splitted the ActivityHour column to form the date and time columns in the intensities dataset
intensities$ActivityHour <- parse_date_time(intensities$ActivityHour, orders = "%m/%d/%Y %H:%M:%S %p")
intensities$date <- format(intensities$ActivityHour, format("%m/%d/%Y"))
intensities$time <- format(intensities$ActivityHour, format("%H:%M:%S"))
weight$Date <- parse_date_time(weight$Date, orders = "%m/%d/%Y %H:%M:%S %p")
```


#### Cleaning Names

I changed all the column names from mixed case to snake case for my preferences and ease of readability.

```{r}
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
```

#### Sorting and Organizing
I sorted and organized the datasets in ascending order.

```{r}
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
```

#### Duplicates
I checked for the duplicates and I found duplicates in the _sleep_ dataset

```{r}
sleep %>%
  get_dupes()
```

This will be helpful more especially while aggregating data.

#### Exploring and Confirming Datasets
1. Confirmed the number of clients who submitted data for each category:

```{r}
n_distinct(sleep$id)
n_distinct(activities$id)
n_distinct(calories$id)
n_distinct(weight$id)
n_distinct(intensities$id)
```
**_Observations_**
From the data we were told that 30 customers willingly submitted the data. However, this was not the case for other datasets had only 8 customers for the weight submitting the data. This is a very low figure to use and draw conclusions from. The rest of the data met atlest the minimum number of customers submitting the data for analysis example 24 and 33 from a majority of datasets.

2. View the structure of the newly cleaned datasets. I used the _head()_ function.
```{r}
head(sleep)
head(activities)
head(weight)
head(calories)
head(intensities)
```

## Processing and Analysis

#### Summary Statitics
This is the summary statistics from some of the datasets which will be giving us some insights while doing analysis.


```{r}
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
```

**_Observations from the summaries_**
These were some of the observations from the summaries:

1. The following were the observations from the activities dataset:

    a. The average sedentary active time was 991.2 minutes which is a very large one stays without any activity. It needs to be reduced and be supplemented with other activities including physical activity. The mean of very active time is 21.16 minutes which is a third of a hour.
  
    b. The average very active distance is 1.503 and it is less compared to the light active distance whose average is 3.341. The sedentary active distance average stand at 0.001606 which is very low and it implies atleast people exercise by walking as the physical activity.
  
    c. The tracker and total distance mean vary awhich implies either the customers do not walk to complete all the distance or some faults somewhere.
    d. The average total steps is 7638 steps made per each person for a whole day.
  
2. The average of the total intensity hourly is 12.04 and the avaerage of the average intensity is 0.2006. This needs to be increased inorder to realise the effectiveness of each activity.

3. The average time in bed is 458.6 minutes and the average time asleed is 419.5 minutes and atleast each person  must sleep at least once in a day for 7 hours.

#### Merging Data
Merged the _activities_ and _sleep_ datasets on the common field  *id* and *date*. The _date_ field is the one I formed during the data cleaning process. We form a new dataset _activity_sleep_ which we use for visualizing our data. This is achievd by an inner join where all values which merge the criteria set are merged together

```{r}
activity_sleep <- merge(sleep, activities, by = c('id', 'date'))
```

View the new dataset structure
```{r}
head(activity_sleep)
```

## Visualization
**_Comparisons between the time asleep and sedentary active minutes_**
```{r}
activity_sleep %>% 
  ggplot(mapping = aes(x=total_minutes_asleep, y=sedentary_minutes)) + geom_point(stat = "identity") +
  geom_smooth() + labs(title = "Sedentary Minutes VS Asleep Minutes")

```
![Sedentary Mins VS Time Asleep](imgs/sedentary_mins_vs_asleep_mins.png)

**_Observation_** 

- As sedentary active minutes increase the total minutes asleep decrease and the majority of the People have almost a balanced time for being asleep and time for being sedentary active except for a few cases. This portrays a negative correlation and since the more people are sedentary active the less time they are asleep. 
- Too much sitting without any activity is in itself an health risk.

- Therefore there is a need to monitor the time spent on sedentary activities like sleeping, sitting, reclining which need reduction. Bellabeat products should be able to monitor this accurately and give reliable data to the use ensuring the well-being of the user and efficient management of time in other activities.

- However, this is a challenge to the women who are employed and in jobs which require too much sitting.

- Bellabeat should make their products flexible in recording such activities and be able to recommend the best time for any other physical activity before or beyond the working hours.

**_Comparisons between the total steps and calories_**

```{r}
activity_sleep %>% 
  ggplot(mapping = aes(x = total_steps, y = calories)) + geom_point(stat = "identity") + geom_smooth() +
  labs(title = "Calories vs Total Steps")
```
![calories vs total steps](imgs/calories_vs_total_steps.png)

**_Observation:_** As the number of steps increase the number of calories burnt increases. This is true since the more we walk the more number of calories are oxidized to release energy.

**_Comparison between total minutes in bed and total minutes asleep_**
```{r}
sleep %>% 
  ggplot(mapping = aes(x = total_time_in_bed, y = total_minutes_asleep)) + geom_point(stat = 'identity') + geom_smooth() +
  labs(title = "Total Time in Bed vs Total Minutes Asleep", x = "Time in bed (minutes)", y = "Time asleep (minutes)")
```

**_Observation:_** We observe that the more time one stays on bed the more likelihood it is for him to be asleep. This a positive correlation.

![time in bed vs time asleep](imgs/mins_in_bed_vs_asleep_mins.png)

**_Hourly Intensity_**
First we create a table to hold the data that we will use for this visualization;

```{r}

# Hourly average intensity
hourly_int <- intensities %>% 
  group_by(time) %>% 
  drop_na() %>% 
  summarise(avg_inte = mean(total_intensity))
```

Then we visualize the data using a histogram to view the hourly intensity as it compares through each hour of the day and know the time when the customers are most active.
```{r}
hourly_int %>% 
  ggplot(mapping = aes(x=time, y = avg_inte)) + geom_histogram(stat = "identity", fill = "darkgreen") + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Hourly Intensity", x="Time (24 hour)", y="Average intensity")
```
![hourly intensity](imgs/hourly_int.png)

**_Observation:_** From the histogram we find out that majority of the customers are very active as from 0500hrs to 2300 hrs. Intensity increases as time goes by through the day with its peak being between 1700hrs - 1900hrs before it begins to go down. Between 0000hrs - 0400hrs there is minimal activity, and therefore, reduced intensity.

## Target Audience
The target audience for the high-tech products from Bellabeat is Women who are working. The products are use to track activities, sleep, stress, mindfulness ensuring the health well-being of these women. The women are always those working in offices and they sit for long hours and time. It is good for them to keep track of their activities both at work and out of work to be able to manage time efficiently in doing other physical activities including walking and even at the gym. They will be able to track the intensity and time for each activity.

## Products
Bellabeats products are the Bellabeat app which is connected with every product Bellabeat and is used to receive data collected from the other wearbles. We have the Leaf which is an innovative product that can either be a necklace, bracelet, or clip. It is fixed with a tracker which is used for tracking user activity, sleep, and stress. We have smart watch used to track all these mentioned activities and relay the data to the APP. Spring which is a water bottle that is used to track daily water intake to ensure that the user is well hydrated. 

## Conclusion and Recommendations

#### Conclusion
Bellabeat being a high-tech global company has a potential for growth to becoming a big global company.Since, its inception in 2013 it has grown rapidly to get to where it is and it feel the need for expansion a good sign for growth. Therefore, from the insights gained through analysis of the data below are the recommendations:

#### Recommendations
Bellabeat APP is more than receiving data from all the other products. It can be used to achieve more than it was intended to.
1. The app can be used to set reminders to remind users when they become inactive, or when to undertake a certain activity since from the observations above we found out that the average sedentary active time is 991.2 minute which is almost 17 hrs more than two-thirds of a day. 
2. Can set notifications which will be part of the reminders more especially after identifying your trends and when you are most active to keep you on toes. Forexample a notification when your most active time approaches. Also the notifications can be used to alert you when something is wrong or your have by-passed an activity.
3. In addition to the produscts Bellabeat should consider having their watches be used to track down the heart rate. This is very important in keeping an eye on the heartrate of the user.

## Referrence

1. Healy, G. N., Clark, B. K., Winkler, E. A., Gardiner, P. A., Brown, W. J., & Matthews, C. E. (2011). Measurement of adults' sedentary time in population-based studies. American journal of preventive medicine, 41(2), 216-227.