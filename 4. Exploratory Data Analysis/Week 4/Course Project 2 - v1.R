# Clear the memory
rm(list=ls())
rm(list=setdiff(ls(),c("NEI")))

# Set working directory
setwd('~/Documents/Github Repos/DataScienceSpecialisation/4. Exploratory Data Analysis/week 4')

# =========================================================================
# PLOT1 - Total PM2.5 emissions 1999-2008 in US
# =========================================================================

# Reading the data into NEI and SCC
NEI <- readRDS("summarySCC_PM25.rds")

# Calculate total emissions per year (tons per year)
NEI_TOT <- aggregate(Emissions ~ year, NEI, FUN=sum, na.rm=TRUE)
# Alternatively,NEI_TOT <- with(NEI,tapply(Emissions, year, sum, na.rm=T))/1e6

# Plot chart to png file
png(filename="plot1.png", width = 1000, height = 700, units = "px")

# Show emissions in barplot in megatons
barplot(NEI_TOT$Emissions/1e6,
        names.arg=NEI_TOT$year,
        xlab="Year",ylab="Total PM2.5 Emission (megatons per year)",
        main="USA PM2.5 Emissions 1999-2008: An overall decrease",
        ylim=c(0,8))

# Close the graphical device
dev.off()

# =========================================================================
# PLOT2 - Total PM2.5 emissions 1999-2008 in Baltimore City, Maryland
# =========================================================================

# Reading the data into NEI and SCC
NEI <- readRDS("summarySCC_PM25.rds")

NEI_Baltimore <- subset(NEI,fips=="24510")

# Calculate total emissions per year (megatons per year)
NEI_Baltimore_TOT <- aggregate(Emissions ~ year, NEI_Baltimore, FUN=sum, na.rm=TRUE)
# Alternatively, NEI_Baltimore_TOT <- with(NEI_Baltimore, tapply(Emissions, year, sum, na.rm=T))/1e6

# Plot chart to png file
png(filename="plot2.png", width = 1000, height = 700, units = "px")

# Show emissions in barplot in kilotons
barplot(NEI_Baltimore_TOT$Emissions/1e3,
        names.arg=NEI_Baltimore_TOT$year,
        xlab="Year",ylab="Total PM2.5 Emission (kilotons per year)",
        main="Baltimore PM.2.5 Emissions 1999-2008: A downward trend with peak in 2005",
        ylim=c(0,4))

# Close the graphical device
dev.off()

# ===========================================================================
# PLOT3 - PM2.5 emissions 1999-2008 in Baltimore City, Maryland by TYPE
# ===========================================================================

library(ggplot2)
library(gridExtra)
library(scales)

# Reading the data into NEI and SCC
NEI <- readRDS("summarySCC_PM25.rds")

NEI_Baltimore <- subset(NEI,fips=="24510")

# Plot chart to png file
png(filename="plot3.png", width = 1000, height = 700, units = "px")

g <-  ggplot(NEI_Baltimore, aes(factor(year), Emissions)) +
      geom_boxplot(aes(color = type)) + scale_y_log10(labels=comma) +
      facet_wrap(~ type, nrow = 1, ncol = 4) +
      labs(title = "Source Emissions: Individual data") +
      labs(x = "Year", y ="PM2.5 Emission (tons per year) on log10 scale")

h <-  ggplot(NEI_Baltimore, aes(factor(year), Emissions, fill=type)) +
      geom_bar(stat="identity") +
      facet_wrap(~ type, nrow = 1, ncol = 4) +
      labs(title = "Total Emissions: 3 types show decreasing trend, only 'POINT' shows a peak ") +
      labs(x = "Year", y ="Total PM2.5 Emission (tons per year)")

grid.arrange(g, h, nrow=1, ncol=2, top="Baltimore PM2.5 Emissions 1999-2008 from all sources")

# Close the graphical device
dev.off()

# Alternative to the geom_boxplot, use geom_point and geom_smooth. Benefit of boxplot is to see the distribution of data.
# h <-  ggplot(NEI_Baltimore, aes(year, Emissions)) +
#       geom_point(aes(color = type), size = 4, alpha = 1/2) + scale_y_log10(labels=comma) + 
#       facet_wrap(~ type, nrow = 1, ncol = 4) +
#       geom_smooth(method = "lm", se = FALSE) +
#       labs(title = "Baltimore 1999-2008: Non-Road, On-Road and Point see a decrease in PM.2.5 Emission, where NonPoint stays same") +
#       labs(x = "Year", y ="PM2.5 Emission per source (tons per year) on log10 scale")


# ===========================================================================
# PLOT4 - PM2.5 emissions 1999-2008 in USA from Coal Combustion
# ===========================================================================

library(ggplot2)
library(gridExtra)
library(scales)

# Reading the data into NEI and SCC
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")[,c("SCC", "EI.Sector")]

# Determine 'fuel combustion coal' sources, subset the concerned emission data, and assign the source descriptions.
SCC_FCC <- SCC$SCC[grep("Fuel.*Comb.*Coal", SCC$EI.Sector, value = FALSE, ignore.case = TRUE)]
NEI_FCC <- subset(NEI, SCC %in% SCC_FCC)
NEI_FCC_Merged <- merge(NEI_FCC, SCC, by.x = "SCC", by.y = "SCC")

# Plot chart to png file
png(filename="plot4.png", width = 1000, height = 700, units = "px")

g <-  ggplot(NEI_FCC_Merged, aes(factor(year), Emissions)) + 
      geom_boxplot() + scale_y_log10(labels=comma) + geom_jitter(width = 0.1) +
      labs(title = "Source Emissions: Individual data") +
      labs(x = "Year", y ="PM2.5 Emission (tons per year) on log10 scale")

h <-  ggplot(NEI_FCC_Merged, aes(factor(year), Emissions/1e3)) +
      geom_bar(stat="identity") +
      labs(title = "Total Emissions: Decreasing trend over time (note, medians go up, left)") +
      labs(x = "Year", y ="Total PM2.5 Emission (kilotons per year)")

grid.arrange(g, h, nrow=1, ncol=2, top="USA PM2.5 Emissions 1999-2008 from coal fuel combustion")

# Close the graphical device
dev.off()


# ====================================================================================
# PLOT5 - PM2.5 emissions in 1999-2008 in Baltimore City, Maryland from Motor Vehicles
# ====================================================================================

library(ggplot2)
library(gridExtra)
library(scales)

# Reading the data into NEI and SCC
NEI <- readRDS("summarySCC_PM25.rds")
NEI_Baltimore <- subset(NEI,fips=="24510")

# Read source classification code
SCC <- readRDS("Source_Classification_Code.rds")[,c("SCC", "SCC.Level.Two")]

# Determine 'motor vehicle' sources, subset the concerned emission data, and assign the source descriptions.
SCC_MV <- SCC$SCC[grep("vehicle", SCC$SCC.Level.Two, value = FALSE, ignore.case = TRUE)]
NEI_MV <- subset(NEI_Baltimore, SCC %in% SCC_MV)
NEI_MV_Merged <- merge(NEI_MV, SCC, by.x = "SCC", by.y = "SCC")

# Plot chart to png file
png(filename="plot5.png", width = 1000, height = 700, units = "px")

g <-  ggplot(NEI_MV_Merged, aes(factor(year), Emissions)) +
      geom_boxplot() + scale_y_log10(labels=comma) +
      labs(title = "Source Emissions: Individual data") +
      labs(x = "Year", y ="PM2.5 Emission (tons per year) on log10 scale")

h <-  ggplot(NEI_MV_Merged, aes(factor(year), Emissions)) +
      geom_bar(stat="identity") +
      labs(title = "Total Emissions: Big drop 1999 -> 2002, continued by decreasing trend") +
      labs(x = "Year", y ="Total PM2.5 Emission (tons per year)")

grid.arrange(g, h, nrow=1, ncol=2, top="Baltimore PM2.5 Emissions 1999-2008 from motor vehicles")

# Close the graphical device
dev.off()







