## Julie Macon
## Coursera, Exploratory Data Analysis, Week 1 Project


## This code creates the plot4.png file
## This function makes use of the lubridate and dplyr packages


plot4 <- function() {
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
        
        ## Plot 4 graphs and send to png
        png("plot4.png", width = 480, height = 480, units = "px", bg = 
                    "transparent")
        par(mar= c(4, 4, 2, 1), mfrow = c(2,2))
        
        ## TOP LEFT
        ## plot xy graph of Day vs Global active power
        hpc_subset$Global_active_power <- as.numeric(as.character(
                hpc_subset$Global_active_power)) ## convert factor to numeric
        plot(hpc_subset$Time, hpc_subset$Global_active_power, type = "l", 
             xlab = "", ylab = "Global Active Power")
        
        ## TOP RIGHT
        ## plot xy graph of Day vs voltage
        hpc_subset$Voltage <- as.numeric(as.character(
                hpc_subset$Voltage)) ## convert factor to numeric
        plot(hpc_subset$Time, hpc_subset$Voltage, type = "l", 
             xlab = "datetime", ylab = "Voltage")
        
        ## BOTTOM LEFT 
        ## plot xy graph of Day vs energy sub metering
        hpc_subset$Sub_metering_1 <- as.numeric(as.character(
                hpc_subset$Sub_metering_1)) ## convert factor to numeric
        hpc_subset$Sub_metering_2 <- as.numeric(as.character(
                hpc_subset$Sub_metering_2)) ## convert factor to numeric
        hpc_subset$Sub_metering_3 <- as.numeric(as.character(
                hpc_subset$Sub_metering_3)) ## convert factor to numeric
        plot(hpc_subset$Time, hpc_subset$Sub_metering_1, type = "l", 
             xlab = "", ylab = "Energy sub metering")
        lines(hpc_subset$Time, hpc_subset$Sub_metering_2, col = "red")
        lines(hpc_subset$Time, hpc_subset$Sub_metering_3, col = "blue")
        legend("topright", names(hpc_subset[,7:9]), col = c("black", "red", 
                "blue"), lty = 1, bty = "n")
        
        ## BOTTOM RIGHT
        ## plot xy graph of Day vs Global reactive power
        hpc_subset$Global_reactive_power <- as.numeric(as.character(
                hpc_subset$Global_reactive_power)) ## convert factor to numeric
        plot(hpc_subset$Time, hpc_subset$Global_reactive_power, type = "l", 
             xlab = "datetime", ylab = "Global_reactive_power")
        
        dev.off()
}