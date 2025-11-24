# Plot 4 of Course Project 1

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

png("plot4.png", width = 480, height = 480)

par(mfrow = c(2,2), mar = c(4,4,2,1))

ticks <- seq(from = floor_date(min(df$datetime), "day"),
			 to   = ceiling_date(max(df$datetime), "day"),
			 by   = "day")

with(df, {
	plot(datetime, Global_active_power,
		 type = "l",
		 xlab = "",
		 ylab = "Global Active Power",
		 xaxt = "n")
	
	axis(1, at = ticks, labels = format(ticks, "%a"))
	
	plot(datetime, Voltage,
		 type = "l",
		 xlab = "datetime",
		 ylab = "Voltage",
		 xaxt = "n")
	
	axis(1, at = ticks, labels = format(ticks, "%a"))
	
	plot(datetime, Sub_metering_1,
		 type = "l",
		 xlab = "",
		 ylab = "Energy sub metering",
		 xaxt = "n")

	with(df, lines(datetime, Sub_metering_2, col = "red"))
	with(df, lines(datetime, Sub_metering_3, col = "blue"))
	
	axis(1, at = ticks, labels = format(ticks, "%a"))
	
	legend("topright",
		   legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
		   col = c("black","red","blue"),
		   lty = 1)
	
	plot(datetime, Global_reactive_power,
		 type = "l",
		 xlab = "datetime",
		 ylab = "Global_reactive_power",
		 xaxt = "n")
	
	axis(1, at = ticks, labels = format(ticks, "%a"))
	
})

dev.off()



