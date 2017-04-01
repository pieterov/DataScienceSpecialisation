# Set working directory
setwd('~/Documents/Github Repos/DataScienceSpecialisation/5. Reproducible Research/week 4'); dir()

# Clear memory
rm(list=ls())

# Does analysis include description and justification for any data transformations?
# Do all the results of the analysis (i.e. figures, tables, numerical summaries) appear to be reproducible?
# Do the figure(s) have descriptive captions (i.e. there is a description near the figure of what is happening in the figure)?


# TITLE: Exploratory Data Analysis of Storm Data in the US 1950-2011

# SYNOPSIS: Describe and summarize the data analysis in less than 10 sentences

library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)

# DATA PROCESSING - how data were loaded into R and processed for analysis
sourcedata <- read.csv("repdata-data-StormData.csv.bz2")

# sourcedata$YEAR <- as.numeric(format(as.Date(sourcedata$BGN_DATE, format = "%m/%d/%Y"),"%Y"))

sourcedata$PROPDMG_DOLLAR <- with(sourcedata,
                             ifelse(PROPDMGEXP=="B", PROPDMG,
                             ifelse(PROPDMGEXP=="M", PROPDMG/1e3,
                             ifelse(PROPDMGEXP=="K", PROPDMG/1e6, PROPDMG/1e9))))

sourcedata$CROPDMG_DOLLAR <- with(sourcedata,
                             ifelse(CROPDMGEXP=="B", CROPDMG,
                             ifelse(CROPDMGEXP=="M", CROPDMG/1e3,
                             ifelse(CROPDMGEXP=="K", CROPDMG/1e6, CROPDMG/1e9))))

fdata <- subset(sourcedata, select=c(EVTYPE,
                                     FATALITIES, INJURIES, 
                                     PROPDMG_DOLLAR, CROPDMG_DOLLAR))
rm(list=setdiff(ls(), "fdata"))

#fdata_health1 <- aggregate(cbind(fdata$FATALITIES, fdata$INJURIES),
#                by=list(fdata$EVTYPE), FUN=sum, na.rm=TRUE)
#fdata_damage1 <- aggregate(cbind(fdata$PROPDMG_DOLLAR, fdata$CROPDMG_DOLLAR),
#                by=list(fdata$EVTYPE), FUN=sum, na.rm=TRUE)

fdata_fatalities1 <- setNames(aggregate(fdata$FATALITIES, by=list(fdata$EVTYPE),
                        FUN=sum, na.rm=TRUE), c("EVTYPE", "FATALITIES"))
fdata_injuries1 <- setNames(aggregate(fdata$INJURIES, by=list(fdata$EVTYPE),
                        FUN=sum, na.rm=TRUE), c("EVTYPE", "INJURIES"))
fdata_propdmg1 <- setNames(aggregate(fdata$PROPDMG_DOLLAR, by=list(fdata$EVTYPE),
                        FUN=sum, na.rm=TRUE), c("EVTYPE", "PROPDMG"))
fdata_cropdmg1 <- setNames(aggregate(fdata$CROPDMG_DOLLAR, by=list(fdata$EVTYPE),
                        FUN=sum, na.rm=TRUE), c("EVTYPE", "CROPDMG"))

fdata_fatalities2 <- subset(fdata_fatalities1, FATALITIES>220)
fdata_injuries2 <- subset(fdata_injuries1, INJURIES>1350)
fdata_propdmg2 <- subset(fdata_propdmg1, PROPDMG>5.2)
fdata_cropdmg2 <- subset(fdata_cropdmg1, CROPDMG>1)

#fdata_health3 <- melt(fdata_health2,id.var=c("EVTYPE"), 
#                      variable.name = "HEALTHIMPACT", value.name="MEASUREMENT")
#fdata_damage3 <- melt(fdata_damage2,id.var=c("EVTYPE"), 
#                      variable.name = "DAMAGEIMPACT", value.name="MEASUREMENT")

# RESEARCH QUESTIONS
# Which types of events are most harmful to population health?

f <-  ggplot(data=fdata_fatalities2, aes(x=reorder(EVTYPE,FATALITIES), y=FATALITIES)) +
        geom_bar(stat="identity", fill="lightblue") + coord_flip() +
        geom_text(aes(label=FATALITIES), hjust=0.6, size=3.5) +
        labs(title = "FATALITIES") +
        labs(x = "Type of Event", y ="Total Number")

g <-  ggplot(data=fdata_injuries2, aes(x=reorder(EVTYPE,INJURIES), y=INJURIES)) +
        geom_bar(stat="identity", fill="lightblue") + coord_flip() +
        geom_text(aes(label=INJURIES), hjust=0.6, size=3.5) +
        labs(title = "INJURIES") +
        labs(x = "Type of Event", y ="Total Number")

grid.arrange(f, g, nrow=1, ncol=2, top="Top-10 Event Types most harmful to population health (1950-2011)")
        
# Which types of events have the greatest economic consequences?

k <-  ggplot(data=fdata_propdmg2, aes(x=reorder(EVTYPE,PROPDMG), y=PROPDMG)) +
        geom_bar(stat="identity", fill="lightblue") + coord_flip() +
        geom_text(aes(label=round(PROPDMG)), hjust=0.6, size=3.5) +
        labs(title = "PROPERTY DAMAGE") +
        labs(x = "Type of Event", y ="Total Cost (billion $)")

l <-  ggplot(data=fdata_cropdmg2, aes(x=reorder(EVTYPE,CROPDMG), y=CROPDMG)) +
        geom_bar(stat="identity", fill="lightblue") + coord_flip() +
        geom_text(aes(label=round(CROPDMG)), hjust=0.6, size=3.5) +
        labs(title = "CROP DAMAGE") +
        labs(x = "Type of Event", y ="Total Cost (billion $)")

grid.arrange(k, l, nrow=1, ncol=2, top="Top-10 Event Types greatest economic consequences (1950-2011)")

# RESULTS - Present main results. At least one plot. At most 3 figures
