## Julie Macon
## Coursera, Exploratory Data Analysis, Week 1 Project


## This code creates the plot1.png file
## This function makes use of the lubridate and dplyr packages


plot1 <- function() {
        ## read the text file in the local directory
        hpc <- read.csv2("household_power_consumption.txt", na.strings = 
                c("\\?")) ## replace ? with NA
        
        ## convert Date to date class
        library(lubridate)
        library(dplyr)
        hpc$Date <- dmy(hpc$Date)

        ## subset data by entries from 2007-02-01 and 2007-02-02
        hpc_subset <- filter(hpc, hpc$Date == ymd("2007-02-01") | hpc$Date == 
                ymd("2007-02-02"))
        
        ## plot hist of Global Active Power and send to png
        hpc_subset$Global_active_power <- as.numeric(as.character(
                hpc_subset$Global_active_power)) ## convert factor to numeric
        png("plot1.png", width = 480, height = 480, units = "px", bg = 
                "transparent")
        par(mar= c(4, 4, 2, 1))
        hist(hpc_subset$Global_active_power, breaks = 12, xlab = 
                "Global Active Power (kilowatts)", ylab = "Frequency", main = 
                "Global Active Power", col = "red")
        dev.off()
}