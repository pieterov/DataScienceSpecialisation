# Clear the memory
rm(list=ls())
rm(list=setdiff(ls(),c("NEI")))

# Set working directory
setwd('~/Documents/Github Repos/DataScienceSpecialisation/5. Reproducible Research/week 1')

# Use only the base R graphics system (not ggplot2 or lattice) to make your figure.

# Original name of csv file: "Inpatient_Prospective_Payment_System__IPPS__Provider_Summary_for_the_Top_100_Diagnosis-Related_Groups__DRG__-_FY2011.csv"
# Renamed to: "IPPS.csv"

# =================================================================================
# PLOT1 - Compare 'Average Covered Charges' to 'Average Total Payments' in New York
# =================================================================================

# Question:
# Make a plot that answers the question: what is the relationship between
# mean covered charges (Average.Covered.Charges) and mean total payments 
# (Average.Total.Payments) in New York?

# Load data, and read NY data into ACC_NY and ATP_NY.
ippsdata <- read.csv("IPPS.csv")
ippsdata_NY <- subset(ippsdata, Provider.State=="NY")
ACC_NY <- ippsdata_NY$Average.Covered.Charges
ATP_NY <- ippsdata_NY$Average.Total.Payments

# Remove '$' from data and convert to numeric data.
ACC_NY_num <- as.numeric(sub('\\$','',as.character(ACC_NY)))
ATP_NY_num <- as.numeric(sub('\\$','',as.character(ATP_NY)))

# Plot chart to png file
pdf("plot1.pdf")
plot(ACC_NY_num, ATP_NY_num,
     log='xy',
     ylim=c(2e3, 5e5),
     xlim=c(2e3, 5e5),
     col="grey", 
     main='ATP increase with ACC - where ACC > ATP for most cases.',
     xlab='Average Covered Charges',
     ylab='Average Total Payments')
abline( h = seq(0, 7e5, 1e4), lty = 3, col = 'black' )
abline( v = seq(0, 7e5, 1e4), lty = 3, col = 'black' )
points(ACC_NY_num, ATP_NY_num, pch=16, col=rgb(1, 0, 0, 0.3))

# Close the graphical device
dev.off()

# Alternative for gridlines, not used here
# grid (NULL,NULL, lty = 6, col = "black")

# =================================================================================
# PLOT2 - Compare 'Average Covered Charges' to 'Average Total Payments' in New York
# =================================================================================

# Question:
# Make a plot (possibly multi-panel) that answers the question: how does the
# relationship between mean covered charges (Average.Covered.Charges) and
# mean total payments (Average.Total.Payments) vary by medical condition
# (DRG.Definition) and the state in which care was received (Provider.State)?

# Load data, and read NY data into ACC_NY and ATP_NY.
ippsdata <- read.csv("IPPS.csv")
ippsdata_NY <- subset(ippsdata, Provider.State=="NY")
ACC_NY <- ippsdata_NY$Average.Covered.Charges
ATP_NY <- ippsdata_NY$Average.Total.Payments

# Remove '$' from data and convert to numeric data.
ACC_NY_num <- as.numeric(sub('\\$','',as.character(ACC_NY)))
ATP_NY_num <- as.numeric(sub('\\$','',as.character(ATP_NY)))

# Plot chart to png file
pdf("plot1.pdf")
plot(ACC_NY_num, ATP_NY_num,
     log='xy',
     ylim=c(2e3, 5e5),
     xlim=c(2e3, 5e5),
     col="grey", 
     main='ATP increase with ACC - where ACC > ATP for most cases.',
     xlab='Average Covered Charges',
     ylab='Average Total Payments')
abline( h = seq(0, 7e5, 1e4), lty = 3, col = 'black' )
abline( v = seq(0, 7e5, 1e4), lty = 3, col = 'black' )
points(ACC_NY_num, ATP_NY_num, pch=16, col=rgb(1, 0, 0, 0.3))

# Close the graphical device
dev.off()