# Plot 3 of Course Project 1

# importing libraries
library(tidyverse)
library(data.table)

filename <- "household_power_consumption.txt"
df <- fread(filename, sep = ";", header = TRUE, na.strings = "?")

# converting Date and Time to Date and POSc from character
# filter for 2007-02-01 and 2007-02-02 only
# converting columns to numeric from character for columns that are
# meant to be numeric

df <- df %>% 
	mutate(datetime = dmy(df$Date) + hms(df$Time)) %>%
	relocate(datetime) %>% 
	select(-Date, -Time) %>% 
	filter(between(datetime,
				   ymd_hms("2007-02-01 00:00:00"),
				   ymd_hms("2007-02-02 23:59:59"))) %>% 
	mutate(across(
		c(Global_active_power, Global_reactive_power,
		  Voltage, Global_intensity, Sub_metering_1,
		  Sub_metering_2, Sub_metering_3),
		as.numeric))

# checking for NaNs for all available columns
df %>% 
	summarise(across(everything(), ~ sum(is.na(.x))))

# check for summary of df
summary(df)

