# Reproducible Research
# Project #2
# Rich Huebner
# January 25, 2016

# Set WD... Set this to whatever working directory you have.
setwd("D:\\data\\RProjects\\RResearch_Project2")

# Load the data
if (!exists(data)) {
     data <- read.csv("repdata-data-StormData.csv", header=TRUE, sep=",")       
}

# Data Processing

### Load the required libraries
library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)

### First, I will only use the variables I need in this analysis. So, create a subset.
scols = c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
subdata <- data[scols]

### Fill in 0s for anything that's missing in the dataset
subdata[subdata$FATALITIES == ""] <- 0
subdata[subdata$INJURIES == ""] <- 0
subdata[subdata$PROPDMG == ""] <- 0
subdata[subdata$CROPDMG == ""] <- 0

head(subdata)

### Clean the data by normalizing the data set.

subdata$PROPDMGEXP[subdata$PROPDMGEXP == ""] <- 0

### Update event names -- essentially normalizing them, "wind, WiNd" = "WIND"
subdata$EVTYPE <- gsub("^HEAT$", "EXCESSIVE HEAT", subdata$EVTYPE )
subdata$EVTYPE <- gsub("^TSTM WIND$", "THUNDERSTORM WIND", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^THUNDERSTORM WIND$", "THUNDERSTORM WIND", subdata$EVTYPE)

### Get aggregated data on fatalities.

f <- aggregate(subdata$FATALITIES, by=list(subdata$EVTYPE), sum, na.rm = TRUE)
names(f) <- c("Category", "Total")
fsort <- f[order(-f$Total), ]
topf <- fsort[1:10, ]
topf$Category <- factor(topf$Category, levels=topf$Category, ordered=TRUE)

### Get aggregated data on economic consequences
e <- aggregate(subdata$PROPDMG, by=list(subdata$EVTYPE), sum, na.rm=TRUE)
names(e) <- c("Category", "Total")
esort <- e[order(-e$Total), ]
tope <- esort[1:10, ]
tope$Category <- factor(tope$Category, levels=tope$Category, ordered=TRUE)


# Results


# Q 1. Across the U.S., which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

gplot <- ggplot(topf, aes(x=Category, y=Total, fill=Category)) +
          geom_bar(stat="identity") +
          labs(x="Category", y="Total Fatalities", title="Fatalities by Event Type") +
          theme(axis.text.x = element_text(angle=45, hjust=1))
print(gplot)

### Based on the graph, the MOST harmful with respect to population health is the TORNADO, followed by
### EXCESSIVE HEAT and FLASH FLOODING.

# Q 2. Across the U.S., which types of events have the greatest economic consequences? 

gplot <- ggplot(tope, aes(x=Category, y=Total, fill=Category)) +
     geom_bar(stat="identity") +
     coord_flip() +
     labs(x="Category", y="$$$", title="Total Economic Consequences per Event Type") +
     theme(axis.text.x = element_text(angle=45, hjust=1)) +
     scale_y_continuous(labels = scales::dollar)
print(gplot)

### In terms of greatest economic impact, the TORNADO is the highest, followed by THUNDERSTORM WINDS
### and FLASH FLOODING.
