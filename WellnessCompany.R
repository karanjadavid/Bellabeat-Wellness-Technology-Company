                        #BELLABEAT CASE STUDY

#Loading libraries (set up the environment) 
library(tidyverse)
library(here)
library(janitor)
library(skimr)
library(hms)
library(lubridate)




#Import data to use in the project.

daily_activity <- read_csv("dailyActivity_merged.csv")
hourly_cal <- read_csv("hourlyCalories_merged.csv")
hourly_intensity <- read_csv("hourlyIntensities_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")
sleep_day <- read_csv("sleepDay_merged.csv")
weight_log_info <- read.csv("weightLogInfo_merged.csv")


###########################################################################
                        #EXPLORE THE DATA
###########################################################################daily
#Explore the data frame content Use either(Summary, str, glimpse, head.
#skim_without_charts)

str(daily_activity)
str(hourly_cal)
str(hourly_intensity)
str(hourly_steps)
glimpse(sleep_day)
summary(weight_log_info)

#Find the number of unique people identified by id represented in the data.
length(unique(daily_activity$Id))

unique_people <- data.frame(
  name = c("daily_activity", "hourly_cal", "hourly_intensity",
           "hourly_steps", "sleep_day", "weight_log_info"), 
  id_count = c(n_distinct(daily_activity$Id), n_distinct(hourly_cal$Id), 
               n_distinct(hourly_intensity$Id), n_distinct(hourly_steps$Id), 
               n_distinct(sleep_day$Id), n_distinct(weight_log_info$Id)))
unique_people

#How long is the data collected
min(daily_activity$ActivityDate)
max(daily_activity$ActivityDate)

#For how many days is each user's data collected?
collection_period <- group_by(daily_activity, Id) %>%
  summarise(total_day = n_distinct(ActivityDate))
collection_period

#Find the least and maximum number of days individual data is collected.
arrange(collection_period, total_day)
arrange(collection_period, -total_day)


#QUICK TAKE AWAYS
#1. Names of columns are in Pascal case.
#2. Date columns are in character data type.
#3. Weight log info data frame has 65 NULLs.
#4. The data covers one month period. 
#5. Number of unique people represented in the data is as follows:
  #Data frames: daily_activity, hourly_cal, hourly_intensity, hourly_steps all 
  #have 33 users. Sleep_day has 24 while weight_log_info has 8 unique users.
#6. The data was collected from 12th April 2016 to 9th May 2016. Highest 
#number of days recorded is 31 while the least is 4 days.





#############################################################################
                  #CLEAN AND TRANSFORM THE DATA
############################################################################

#Clean all column names for better readability and uniformity. 
#Change column name format from pascal_case to snake_case.
daily_activity <- clean_names(daily_activity)
hourly_cal <- clean_names(hourly_cal)
hourly_intensity <- clean_names(hourly_intensity)
hourly_steps <- clean_names(hourly_steps)
sleep_day <- clean_names(sleep_day)
weight_log_info <- clean_names(weight_log_info)


#TRANSFORM DAILY DATA
str(daily_activity)
#daily_activity. Create new data frames to avoid tampering with existing ones.
#daily_activity Has its date in character data type
#Tracker distance column is unnecessary - delete it.
#Ensure the date format is mdy
#Link each date to a week day. Eg. Monday, Tuesday ...Sunday
#Filter out the user whose data is collected for only 4 days the entire month.
#If not filtered out, it might skew the analysis results.
#name the transformed data frames. ******_df


daily_df <- daily_activity %>%
  select(-tracker_distance) %>% 
  mutate(activity_date = mdy(daily_activity$activity_date))
daily_df$day_of_week <- wday(daily_df$activity_date, label = TRUE)
daily_df$week_no <- week(daily_df$activity_date)
daily_df <- filter(daily_df, id != 4057192912)
         




#TRASFORM HOURLY DATA 

#use hourly cal, hourly intensity and hourly steps data frames.
#merge the three data frames into one using inner joins.
#merged data is easily accessible and compatible for analysis.
#the three data frames have id and activity hour in common.
#format the activity hour column to mdy_hms date format. Remove AM/PM.
#add new columns of date and time derived from activity_ hour column.


hourly_df <- inner_join(inner_join(hourly_cal, hourly_intensity, by = c(
  "id", "activity_hour")), hourly_steps, by = c(
    "id", "activity_hour"))
hourly_df$activity_hour <- mdy_hms(hourly_df$activity_hour)
hourly_df$date <- as_date(hourly_df$activity_hour)
hourly_df$time <- as_hms(hourly_df$activity_hour)
hourly_df$weekday <- wday(hourly_df$activity_hour, label = TRUE)
hourly_df <- filter(hourly_df, id != 4057192912)




#TRANSFORM SLEEP DATA 
#add a new column showing time people spend resting in bed
#format the date. split the day and time. 
#The time is not necessary since it is 12:00:00 AM all through.

sleep_day_df <- mutate(sleep_day, 
                       rest_bed_time = total_time_in_bed - total_minutes_asleep)
sleep_day_df$sleep_day = mdy_hms(sleep_day_df$sleep_day)
sleep_day_df$date <- as_date(sleep_day_df$sleep_day)
sleep_day_df <- filter(sleep_day_df, id != 4057192912, total_time_in_bed > 100)



#TRANSFORM WEIGHT DATA 
#remove unnecessary columns
#format the date 
#filter out id 4057192912

weight_df <- weight_log_info %>%
  select(-weight_pounds, -log_id, -fat)
weight_df$date <- mdy_hms(weight_df$date)
weight_df$dates <- as_date(weight_df$date)
weight_df <- filter(weight_df, id != 4057192912)





###############################################################
                #ANALYSIS AND VISUALIZATION
###############################################################
#1) Find out users' walking trends. Which days do they walk the most?
#Average daily steps grouped by by day of week.
average_daily_steps <- group_by(daily_df, day_of_week) %>% 
  summarise(average_steps = mean(total_steps))

glimpse(average_daily_steps)
#1b)Visualize the weekly average daily steps.
#Plot a bar graph with the data from Sunday to Saturday.
ggplot(average_daily_steps)+
  geom_bar(mapping = aes(day_of_week, average_steps), stat = "identity", 
           fill = "blue")+
  labs(title = "Steps People Walk per Day?", 
       subtitle = "Average daily steps from Monday to Sunday",
       caption = "steps data barplot")+
  xlab("day of the week")+
  ylab("average steps")+
  ylim(0, 10000)+
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(margin = margin(b = 20)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(10,10,10,10),
        legend.title = element_text(face = "bold"))

#TAKEAWAYS
#People walk most on Saturdays closely followed by Tuesdays.
#people walk least on Sundays.
#the average number of steps is 7645
#the number of steps are likely to vary based on ones age, occupation,
#sex, height and stride. 






#2a) Average daily active minutes
#How active did people spend their minutes on average?
#were people very active, fairly active, lightly active or sedentary?

#############steps#####################
#convert the daily df data frame into a long format to have 
#very active, fairly active, lightly active, and sedentary minutes in one
#column called "active_type".
#transfer the values of active_types into a column named active_minutes. 


# Pivot daily_df
pivot_daily_df <- daily_df %>%
  pivot_longer(c(very_active_minutes, fairly_active_minutes,
                 lightly_active_minutes, sedentary_minutes), 
               names_to = "active_type", values_to = "active_minutes")
# Summarize data
daily_active_df <- select(pivot_daily_df, id, day_of_week,
                          active_type, active_minutes)
average_daily_active_df <- summarise(group_by(daily_active_df, day_of_week,
                                              active_type), 
                                     average_active_minutes = mean(
                                       active_minutes))
#2b) Visualization

average_daily_active_df %>%
  drop_na() %>% 
  ggplot()+
  geom_bar(aes(day_of_week, average_active_minutes, fill = active_type), 
           stat = "identity", position = "stack")+
  labs(title = "How Active Are People In A Day")+
  xlab(NULL)+
  ylab("average active minutes")+
  ylim(0, 1500)+
  scale_color_discrete(name = "Active")+
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(margin = margin(b = 20)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(10,10,10,10),
        legend.title = element_text(face = "bold"))




#2c) visualization comparison
#fairly active vs very active
filter(average_daily_active_df, active_type == "very_active_minutes" 
       |active_type == "fairly_active_minutes") %>% 
  drop_na() %>% 
  ggplot()+
  geom_bar(aes(day_of_week, average_active_minutes, fill = active_type), 
           stat = "identity", position = "stack")+
  labs(title = "How Active Are People In A Day")+
  xlab(NULL)+
  ylab("average active minutes")+
  ylim(0, 70)+
  scale_color_discrete(name = "Active")+
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(margin = margin(b = 20)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(10,10,10,10),
        legend.title = element_text(face = "bold"))



#Hourly Intensities of a day analysis
##############steps##########################
#3a) How is the intensity during each hour of the day?
#create a new data frame with intensity data grouped by id and date
#create a new column showing days of the week
#regroup the new data frame into weeks
#find the average intensity grouped in each day of the week.


intensity_data <- hourly_df %>% 
  group_by(id,date) %>% 
  drop_na() %>% 
  summarise(sum_total_intensity = sum(total_intensity))
intensity_data$weekday <- weekdays(intensity_data$date)
intensity_weekdays <- intensity_data %>%
  group_by(weekday) %>%
  drop_na() %>%
  summarise(average_intensity = mean(sum_total_intensity))

## Cleaning the format: change data type from character to factor.
intensity_weekdays$weekday <- factor(intensity_weekdays$weekday,
                                     levels = c(
  "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))


## Get the start date and end date of hourly data
mindate <- min(hourly_df$date)
mindate
maxdate <- max(hourly_df$date)
maxdate
totalid <- n_distinct(hourly_df$id)
totalid

## Get the start date and end date of hourly data
mindate <- min(hourly_df$date)
mindate
maxdate <- max(hourly_df$date)
maxdate
totalid <- n_distinct(hourly_df$id)
totalid


#TAKEAWAY
#most intensity on Saturday.
#least intensity on Sunday.



#4a)Intensity during the 24 hours analysis
intensity_hours <- hourly_df %>%
  group_by(weekday, time) %>%
  drop_na() %>%
  summarise(mean_total_intensity = mean(total_intensity))

# Grouping to by Day of the weeks:
intensity_week_hour <- intensity_hours %>%
  group_by(weekday) %>%
  drop_na() %>%
  summarise(mean_total_intensity = mean(mean_total_intensity))

#4b) visualization of intensity during the 24 hours of a day
ggplot(data=intensity_hours) +
  geom_col(mapping = aes(x = time, y=mean_total_intensity), fill = "blue") +
  labs(title="Hourly Intensity of a Day",
       caption=paste0("Date from ", mindate," to ",maxdate, ";
                      ", totalid, " users"),
       x="Time(hour)",
       y="Average intensity")+
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(margin = margin(b = 20)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(10,10,10,10),
        legend.title = element_text(face = "bold"))


#4c) visualization intensity of each hour of each day of the week. 
ggplot(data = intensity_hours) +
  geom_col(mapping = aes(x = time, y=mean_total_intensity, 
                         fill=mean_total_intensity)) +
  facet_wrap(~weekday) +
  labs(title="Intensity of Each Hour of Each Day of the Week",
       caption=paste0("Date from ", mindate," to ",maxdate, "; ", 
                      totalid, " users"),
       x="Time(hour)",
       y="Average intensity",
       fill="Intensity")



#5a)What was the effect of more steps on calories?
#total steps vs calories burnt analysis. 
steps_vs_calories_df <- daily_df %>% 
  select(id, total_steps, calories) %>% 
  group_by(id) %>% 
  summarise(average_steps = mean(total_steps),
            average_calories = mean(calories))

#5b)steps vs calories Visualization.
ggplot(data = steps_vs_calories_df)+
  geom_point(mapping = aes(x = average_steps, y = average_calories),
             color = "blue")+
  labs(title = "Do users who walk more burn more calories?",
       subtitle = "User average steps vs average calories")+
  ylab("average steps")+
   xlab("average calories")+
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(margin = margin(b = 20)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(10,10,10,10),
        legend.title = element_text(face = "bold"))






#6)How fit are the users?
#6a)Find out what type of users use the fitness devices - Target market
#According to Centers for Disease Control and prevention
#You are under weight if your BMI(Body Mass Index) < 18.5
#Normal weight - B.M.1 is between (18.5 - 24.9)
#Overweight / Obese B.M.I > 30 

#Average B.M.I, Max B.M.I, Min B.M.I 
mean(weight_df$bmi)
max(weight_df$bmi)
min(weight_df$bmi)


Weight_category <- weight_df %>% 
  select(id, bmi) %>% 
  mutate(category = case_when(
    .$bmi <30 ~ "normal weight",
    .$bmi >30 ~ "over weight",
    .$bmi <18.5 ~ "underweight"
  ))



#A bar plot showing different average user B.M.I quantities
ggplot(data = Weight_category, mapping = aes(x = factor(id), y = bmi,
                                             fill = category))+
  geom_bar(stat = "summary", fun = "mean")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  ggtitle("Users average bmi")+
  ylab("Average B.M.I")+
  xlab("User Id")+
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(margin = margin(b = 20)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(10,10,10,10),
        legend.title = element_text(face = "bold"))


#KEY FINDINGS
#People walk most on Saturdays closely followed by Tuesdays.
#people walk least on Sundays.
#the average number of steps is 7645
#the number of steps are likely to vary based on ones age, occupation,
#sex, height and stride. The data should be added fo furhur analysis
#most time is sedentary which we presume they are a working class who do 
#most of their work sitted in an office.


#on average, people are more active between 5ppm and 7pm in the evening.
#peoples are least active at night, which we assume they are asleep.
#there is a high intensity at noon meaning people might be exercise
#while going out to look for lunch.

#trends throughout the working weekdays are almost similar
#Monday, Tuesday has people active from 4 to 7 pm
#Saturday has many people active at around 1pm.

#there is a linear correlation between steps and calories
#people who walk for burn more calories hence are healthier

#1 out of 9 people is overweight.
#the rest of the 8 are healthy and have a normal weight



