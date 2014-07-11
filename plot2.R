# Filename:    plot2.R
#
# Description: plots Global Active Power vs weekdays from data in the UC Urvine Machine
#              learning repository (http://archive.ics.uci.edu/ml) as a line graph
#
#              Only data from 2007-02-01 through 2007-02-02 inclusive will be plotted
#
#              Missing data in this dataset is denoted by '?'
#       
#              The data dictionary/schema for this dataset is as follows: 
#
#                   Date: Date in format dd/mm/yyyy
#                   Time: time in format hh:mm:ss
#                   Global_active_power: household global minute-averaged active power (in kilowatt)
#                   Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
#                   Voltage: minute-averaged voltage (in volt)
#                   Global_intensity: household global minute-averaged current intensity (in ampere)
#                   Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
#                   Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
#                   Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.
#
# History:     2014-July-09 - Dwight Kruger - Original Version

plot2 <- function()
{
    # Get the raw datafile, and unZip it
    url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    destFile.zip <- "./household_power_consumption.zip"
    download.file(url, destFile.zip)
    unzip(destFile.zip, overwrite = TRUE, exdir = ".")
    
    # We should have a file called 'household_power_consumption.txt' in the cwd. 
    # Load it into a dataframe with the appropriate data types
    colClasses <- c("character", "character", rep("numeric", 7) )
    hpc <- read.csv("./household_power_consumption.txt", header=TRUE, sep = ";", na.strings = "?", colClasses = colClasses, stringsAsFactors=FALSE)
    hpc$Date <- as.Date(hpc$Date, format="%d/%m/%Y")
    
    # Select a subset of the dataframe for the daterange specified below
    startDate <- as.Date("2007-02-01")    
    endDate <- as.Date("2007-02-02")     
    
    trimData <- hpc[hpc$Date >= startDate,] 
    trimData <- trimData[trimData$Date <= endDate,] 
    
    # Calculate the X axis values for the graph
    xData <- strftime(paste(trimData$Date, trimData$Time))
    xData <- as.numeric(difftime(xData, startDate, tz="UTC", units="days"))
    
    # Start building our graph
    library(lattice)
    xTickLabels <- c(strftime(startDate, format="%a", tz="UTC"), strftime(endDate, format="%a", tz="UTC"), strftime(endDate+1, format="%a", tz="UTC"))
    xRange <- range(xData)
    xTicks <- c(xRange[1], (xRange[2]+xRange[1])/2, xRange[2])
    
    yTicks <- seq(from=floor(min(trimData$Global_active_power)), to=ceiling(max(trimData$Global_active_power)), by=2)
    yLabel <- "Global Active Power (kilowatts)"

    # Open output device and save the plot to a PNG file
    png(filename="./figure/plot2.png", width=504, height=504)
    
    # Plot the line and label the axes 
    plot(trimData$Global_active_power ~ xData, type="l", xlab="", ylab=yLabel, axes=FALSE)
    axis(1, at=xTicks, labels=xTickLabels)
    axis(2, at=yTicks, labels=yTicks)
    box()
    
    dev.off()  # close output device
}