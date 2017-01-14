# Clear the memory
rm(list=ls())

# Set working directory
setwd('~/Documents/Github Repos/DataScienceSpecialisation/4. Exploratory Data Analysis/week 4')

SCC <- readRDS("Source_Classification_Code.rds")

# =========================================================================
# PLOT1 - PM2.5 emissions in US 1999-2008
# =========================================================================

# Reading the data into NEI and SCC
NEI <- readRDS("summarySCC_PM25.rds")

# Calculate total emissions per year (megatons per year)
EpY <- with(NEI,tapply(Emissions, year, sum, na.rm=T))/1e6

# Plot chart to png file
png(filename="plot1.png")

plot(names(EpY),EpY,
     pch=19, 
     col=2,
     cex=2,
     xlab="Year",ylab="Total PM2.5 Emission (megatons/y)",
     main="USA 1999-2008: Decrease in PM2.5 Emission",
     xlim=c(1998,2009),
     ylim=c(0,8))

lines(names(EpY),EpY,
      col=2,
      lwd=3)

# Close the graphical device
dev.off()

# =========================================================================
# PLOT2 - PM2.5 emissions in US 1999-2008 in Baltimore City, Maryland
# =========================================================================

# Reading the data into NEI and SCC
NEI <- readRDS("summarySCC_PM25.rds")

NEI_Baltimore <- subset(NEI,fips=="24510")

# Calculate total emissions per year (megatons per year)
EpY <- with(NEI_Baltimore, tapply(Emissions, year, sum, na.rm=T))/1e6

# Plot chart to png file
png(filename="plot2.png")

plot(names(EpY),EpY,
     pch=19, 
     col=2,
     cex=2,
     xlab="Year",ylab="Total PM2.5 Emission (megatons/y)",
     main="Baltimore 1999-2008: Downward trend in PM.2.5 Emission with peak in 2005",
     xlim=c(1998,2009),
     ylim=c(0,0.005))

lines(names(EpY),EpY,
      col=2,
      lwd=3)

# Close the graphical device
dev.off()

# ===========================================================================
# PLOT3 - PM2.5 emissions in US 1999-2008 in Baltimore City, Maryland by TYPE
# ===========================================================================

library(ggplot2)

# Reading the data into NEI and SCC
NEI <- readRDS("summarySCC_PM25.rds")

NEI_Baltimore <- subset(NEI,fips=="24510")

# Plot chart to png file
png(filename="plot3.png")

g <- ggplot(NEI_Baltimore, aes(factor(year), Emissions))
 
g + geom_boxplot() +
    facet_wrap(~ type, nrow = 1, ncol = 4) +
    scale_y_log10() + labs(title = "Baltimore 1999-2008: Non-Road, On-Road and Point see a decrease in PM.2.5 Emission, where NonPoint stays same") +
    labs(x = "Year", y ="Total PM2.5 Emission (tons/y) - note, log10 scale")

# Close the graphical device
dev.off()

# Calculate total PM2.5 per type
EpY_NONROAD <- with(subset(NEI,fips=="24510" & type=="NON-ROAD"), tapply(Emissions, year, sum, na.rm=T))/1e6
EpY_NONPOINT <- with(subset(NEI,fips=="24510" & type=="NONPOINT"), tapply(Emissions, year, sum, na.rm=T))/1e6
EpY_ONROAD <- with(subset(NEI,fips=="24510" & type=="ON-ROAD"), tapply(Emissions, year, sum, na.rm=T))/1e6
EpY_POINT <- with(subset(NEI,fips=="24510" & type=="POINT"), tapply(Emissions, year, sum, na.rm=T))/1e6

# Table of number of measurements per year and type
with(NEI_Baltimore, table(type,year))
