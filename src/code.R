## ---- load
library(tidyverse)
library(lubridate)
library(sugrrants)

## ---- theme-remark
theme_remark <- function() {
  theme_bw() +
  theme(
    axis.text = element_text(size = 14), 
    strip.text = element_text(size = 16), 
    axis.title = element_text(size = 16)
  )
}

## ---- read
# the csv downloaded on 27 May, 2017
ped <- read_csv("data/Pedestrian_traffic_-_hourly_count.csv")
sx <- ped %>% 
  filter(
    Sensor_Name == "Southern Cross Station",
    Year > 2010,
  ) %>% 
  mutate(Date_Time = dmy_hm(Date_Time))
time_breaks <- seq.int(0, 23, by = 4)
time_label <- paste(time_breaks, "00", sep = ":")
clock <- tibble(
  x = 2,
  y = 1700,
  label = paste(seq.int(0, 23), "00", sep = ":")
)

## ---- sx4
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 5, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-04")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[5, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx5
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 6, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[6, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx6
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 7, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[7, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx7
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 8, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[8, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx8
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 9, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[9, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx9
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 10, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[10, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx10
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 11, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[11, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx11
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 12, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[12, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx12
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 13, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[13, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx13
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 14, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[14, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx14
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 15, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[15, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx15
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 16, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[16, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx16
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 17, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[17, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx17
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 18, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[18, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx18
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  mutate(Hourly_Counts = if_else(Time < 19, Hourly_Counts, NA_integer_)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[19, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx19
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  filter(
    Date == as_date("2011-01-10")
  ) %>% 
  ggplot(aes(x = Time, y = Hourly_Counts)) +
  geom_line() +
  geom_label(aes(x, y, label = label), data = clock[24, ], size = 10) +
  scale_x_continuous(
    breaks = time_breaks,
    label = time_label,
    limits = c(0, 23)
  ) +
  ylab("Pedestrian Counts") +
  ylim(0, 1850) +
  theme_remark()

## ---- sx-week
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  filter(between(Date, as_date("2011-01-10"), as_date("2011-01-16"))) %>% 
  ggplot(aes(x = Date_Time, y = Hourly_Counts)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
  xlab("Weekday") +
  ylab("Pedestrian Counts") +
  theme_remark()

## ---- sx-month
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  filter(between(Date, as_date("2011-01-01"), as_date("2011-01-31"))) %>% 
  ggplot(aes(x = Date_Time, y = Hourly_Counts)) +
  geom_line() +
  xlab("Date") +
  ylab("Pedestrian Counts") +
  theme_remark()

## ---- sx-year
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  filter(Year == 2011) %>% 
  ggplot(aes(x = Date_Time, y = Hourly_Counts, group = Date)) +
  geom_line() +
  xlab("Date") +
  ylab("Pedestrian Counts") +
  theme_remark()

## ---- sx-2years
sx %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  filter(Year <= 2012) %>% 
  ggplot(aes(x = Date_Time, y = Hourly_Counts, group = Date)) +
  geom_line() +
  xlab("Date") +
  ylab("Pedestrian Counts") +
  theme_remark()

## ---- sx-6years
sx %>% 
  ggplot(aes(x = Date_Time, y = Hourly_Counts)) +
  geom_line() +
  xlab("Date") +
  ylab("Pedestrian Counts") +
  theme_remark()

## ---- sx-facet-year
sx %>% 
  mutate(Date_Time = ymd_h(paste( # hacking to prepare date time for ggplot2
    paste(2016, Month, Mdate, sep = "-"),
    Time)) 
  ) %>% 
  ggplot(aes(x = Date_Time, y = Hourly_Counts)) +
  geom_line() +
  facet_grid(Year ~ .) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  xlab("Date") +
  ylab("Pedestrian Counts") +
  theme_remark()

## ---- sx-feb
p <- sx %>% 
  filter(Year == 2017, Month == "February") %>% 
  frame_calendar(Time, Hourly_Counts, Date_Time, ncol = 1, nrow = 1) %>% 
  ggplot(aes(.x, .y, group = .group_id)) +
  geom_line() +
  theme_remark()
prettify(p)

## ---- sx-2016
# calendar-plot
p2016 <- sx %>% 
  filter(Year >= 2016) %>% 
  frame_calendar(Time, Hourly_Counts, Date_Time, ncol = 4, nrow = 4) %>% 
  ggplot(aes(.x, .y, group = .group_id)) +
  geom_line() +
  theme_remark()
prettify(p2016)

## ---- sx-calendar
# calendar-plot
sx_cal <- sx %>% 
  frame_calendar(Time, Hourly_Counts, Date_Time, ncol = 12, nrow = 7) %>% 
  ggplot(aes(.x, .y, group = .group_id)) +
  geom_line() +
  theme_void()
sx_cal
