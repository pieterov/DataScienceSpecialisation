

# Clear memory
rm(list=ls())

# Load data from Feb 1st and 2nd 2007 into memory.
startrow <- min(grep("1/2/2007",readLines("household_power_consumption.txt")))
fdata1 <- read.table("household_power_consumption.txt", skip=startrow-1, nrows=2880, sep=";")
colnames(fdata1) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")

# Combine Date and Time column. Create new dataframe leaving out original Date and Time column.
fdata1$DateTime <- strptime(paste(fdata1$Date, fdata1$Time),"%d/%m/%Y %H:%M:%S")
fdata2 <- subset(fdata1, select = 3:10)

# Make plot 1.
png("plot1.png")
hist(fdata2$Global_active_power, col="red", xlab="Global Active Power (kilowatt)",main="Global Active Power")
dev.off()