## Julie Macon
## Coursera, Exploratory Data Analysis, Week 1 Project


## This code creates the plot2.png file
## This function makes use of the lubridate and dplyr packages


plot2 <- function() {
        ## read the text file in the local directory
        hpc <- read.csv2("household_power_consumption.txt", na.strings = 
                c("\\?")) ## replace ? with NA
        
        ## convert Date to date class
        library(lubridate)
        library(dplyr)
        hpc$Date <- dmy(hpc$Date)
        hpc_subset <- mutate(hpc, Time = paste(Date, Time))
        hpc_subset$Time <- ymd_hms(hpc_subset$Time)
        
        ## subset data by entries from 2007-02-01 and 2007-02-02
        hpc_subset <- filter(hpc_subset, hpc_subset$Date == ymd("2007-02-01") | 
                hpc_subset$Date == ymd("2007-02-02"))
        
        ## plot xy graph of Day vs Global Active Power and send to png
        hpc_subset$Global_active_power <- as.numeric(as.character(
                hpc_subset$Global_active_power)) ## convert factor to numeric
        
        png("plot2.png", width = 480, height = 480, units = "px", bg = 
                "transparent")
        par(mar= c(4, 4, 2, 1))
        plot(hpc_subset$Time, hpc_subset$Global_active_power, type = "l", 
                xlab = "", ylab = "Global Active Power (kilowatts)")
        dev.off()
}