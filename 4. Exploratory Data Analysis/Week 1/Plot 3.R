# PLOT 3 - Individual household electric power consumption Data Set

# Dataset are taken from the UCI web site. Descriptions of 9 variables in the dataset:
# Global_active_power: household global minute-averaged active power (in kilowatt)
# Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
# Voltage: minute-averaged voltage (in volt)
# Global_intensity: household global minute-averaged current intensity (in ampere)
# Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy).
#                 It corresponds to the kitchen, containing mainly a dishwasher, an oven
#                 and a microwave (hot plates are not electric but gas powered).
# Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy).
#                 It corresponds to the laundry room, containing a washing-machine, a
#                 tumble-drier, a refrigerator and a light.
# Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy).
#                 It corresponds to an electric water-heater and an air-conditioner.
# DateTime:       Date in format yyyy-mm-dd hh:mm:ss (=derived column). Original Date and Time
#                 column are left out.

library(dplyr)

# Clear memory
rm(list=ls())

# Load data from Feb 1st and 2nd 2007 into memory.
startrow <- min(grep("1/2/2007",readLines("household_power_consumption.txt")))
fdata1 <- read.table("household_power_consumption.txt", skip=startrow-1, nrows=2880, sep=";")
colnames(fdata1) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")

# Combine Date and Time column. Create new dataframe leaving out original Date and Time column.
fdata1$DateTime <- strptime(paste(fdata1$Date, fdata1$Time),"%d/%m/%Y %H:%M:%S")
fdata2 <- subset(fdata1, select = 3:10)

# Make plot 3.
png("plot3.png")
plot(fdata2$DateTime, fdata2$Sub_metering_1, xlab="", ylab="Energy sub metering", type="n")
lines(fdata2$DateTime, fdata2$Sub_metering_1, type="l")
lines(fdata2$DateTime, fdata2$Sub_metering_2, type="l", col="red")
lines(fdata2$DateTime, fdata2$Sub_metering_3, type="l", col="blue")
legend("topright",c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd=2,col=c("black","red","blue"), text.col=c("black","red","blue"))
dev.off()