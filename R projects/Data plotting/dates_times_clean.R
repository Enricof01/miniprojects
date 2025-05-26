# Nonprofit Data Analysis in R----
# Section 3: Data Transformation----

#1.0 Load packages----

library(janitor)
library(lubridate)
library(tidyverse)
library(readxl)
library(tidyquant)

#2.0 Load Data from Excel----
?readxl
# Identify names of sheets in an excel file
excel_sheets("00_data/excel/ds_1.xlsx")

clients <- read_excel("00_data/excel/ds_1.xlsx",
                      sheet = "Clients")

reports <- read_excel("00_data/excel/ds_1.xlsx",
                      sheet = "Reports")

locations <- read_excel("00_data/excel/ds_1.xlsx",
                        sheet = "Locations")

employees <- read_excel("00_data/excel/ds_1.xlsx",
                        sheet = "Employees")


#3.0 Join tables----
cw_df <- reports %>% 
  left_join(clients, by= "client_id") %>% 
  left_join(locations, by= c("location" = "location_id")) %>% 
  left_join(employees, by= c("employee_reporting" = "employee_id"))


# 4.0 Mutate----
cw_df <- cw_df %>% 
  group_by(client_id) %>%
  mutate(report_count = n()) %>% 
  ungroup() %>% 
  mutate(report_count_binned = ntile(report_count, 3)) %>%
  mutate(report_count_cat = case_when(
    report_count > quantile(report_count, .66) ~ "High",
    report_count > quantile(report_count, .33) ~ "Medium",
    report_count <= quantile(report_count, .33) ~ "Low",
    TRUE ~ "NA") %>% as.factor()) %>%
  mutate(foster_care = if_else(program_admitted %in% c("foster", "ther. foster"), "Yes",
                               "No")) %>%
  mutate(foster_care = case_when(
    program_admitted %in% c("ther. foster", "foster") ~ "Yes",
    program_admitted == "residential" ~ "No",
    TRUE ~ "Other") %>% as.factor()) %>% 
  mutate(incident_cat = case_when(
    incident_type %>%  str_to_lower() %>% str_detect("behavior") ~ "Behavior",
    incident_type %>%  str_to_lower() %>% str_detect("abuse") ~ "Abuse",
    TRUE ~ "Other") %>% as.factor()) 

# 5.0 Date-time components----

cw_df %>% 
  glimpse()

today() + days(1)
today() + months(3)
now() - hours(4)

time_1 <- "2019-07-22"
time_2 <- "2019-07-22 14:10:23"
time_3 <- "2019-07-22 14:10"
time_4 <- 1722861541 #Unix timestamp 
# Unix epoch is midnight UTZ on January 1, 1970. Time increases by 1 for each non-leap second that passes. https://www.epochconverter.com/ 

class(time_1)
ymd(time_1)
time_5 <- ymd_hms(time_2, tz= "UTC")
?tz
ymd_hms(time_1, tz= "America/New_York")
time_6 <- with_tz(time_4, tzone = "America/New_York")
time_4 - hours(4)

Sys.timezone()
# UTC= Coordinated Universal Time (Zulu in US military)
#https://www.surfertoday.com/images/stories/time-zone-map.png 

ymd_hm(time_3)
as_datetime(time_4)

cw_df <- cw_df %>% 
  mutate(report_month = month(report_date, label= T),
         report_year  = year(report_date))

cw_df %>%
  select(client_id, report_year, report_month) %>% 
  filter(report_year == "2020") %>% 
  group_by(report_month) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(avg_reports   = mean(n),
         reports_dev   = n-avg_reports) %>% 
  ggplot(aes(x= report_month, y=reports_dev)) +
  geom_col()

#Floor and ceiling date----
#First day of month
cw_df %>% 
  select(client_id, report_date) %>% 
  mutate( report_date = ymd(report_date),
          report_my = floor_date(report_date, unit= "month")) #floor_date reduces a date to the first day of the month

#Last day of month
cw_df %>% 
  select(client_id, report_date) %>% 
  mutate( report_date = ymd(report_date),
          report_my = ceiling_date(report_date, unit= "month")- days(1)) 


#Change over time----
# Compare to previous month
cw_df_dt <- cw_df %>%
  select(client_id, report_year, report_month) %>% 
  filter(report_year == "2020") %>% 
  group_by(report_month) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(report_lag_1 = lag(n, n=1),
         report_lag_1 = case_when(
           is.na(report_lag_1) ~ n,
           TRUE ~ report_lag_1
         ),
         report_lag_diff = n-report_lag_1,
         report_lag_diff_pct = report_lag_diff/report_lag_1,
         report_lag_lbl = scales::percent(report_lag_diff_pct)) 

cw_df_dt %>% 
  ggplot(aes(x= report_month, y=report_lag_diff_pct))+
  geom_hline(yintercept = 0, color= "red", linetype= "solid", linewidth= .5)+
  geom_line(aes(group= 1), color = "blue")+
  geom_point() +
  geom_text(aes(label= report_lag_lbl), vjust= -0.5)+
  labs(title= "Report Trends: Comp. to Prev. Month",
       subtitle = "2020",
       x= "Report Month",
       y= "% change from previous month") +
  theme_tq()+
  scale_y_continuous(limits = c(-.75,1.5 )) #alt based on values: max(cw_df_dt$report_lag_diff_pct)*1.2))

#Rolling and cumulative calculations----
# How many reports year-to-date?
cw_df %>%
  select(client_id, report_year, report_month) %>% 
  filter(report_year == "2020") %>% 
  group_by(report_month) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(cum_reports     = cumsum(n),
         cum_reports_pct = cum_reports/sum(n)*100) 

# How can we obtain a rolling avg?
cw_df %>%
  select(client_id, report_year, report_month) %>% 
  filter(report_year == "2020") %>% 
  group_by(report_month) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(rm_reports = rollmean(n, k= 3, 
                               align = "right", 
                               fill= 0))

?rollmean

# Avg report count by quarter?
cw_df %>%
  select(report_year, report_date) %>% 
  filter(report_year == "2020") %>% 
  mutate(report_quarter = quarter(report_date)) %>% 
  group_by(report_quarter) %>% 
  count() %>% 
  ungroup()

#Obtain a count between specific dates
cw_df %>% 
  filter(between(report_date, as.Date("2020-01-01"), as.Date("2020-03-31"))) %>% 
  arrange(desc(report_date))
#Alt. approach:
cw_df %>% 
  filter(report_date >= as.Date("2020-01-01") & report_date <= as.Date("2020-03-31"))
















         