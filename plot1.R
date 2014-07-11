# Filename:    plot1.R
#
# Description: plots Global Active Power vs Frequency from data in the UC Urvine Machine
#              learning repository (http://archive.ics.uci.edu/ml) as a bar chart
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

plot1 <- function()
{
    # Get the raw datafile, and unZip it
    url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    destFile.zip <- "./household_power_consumption.zip"
    download.file(url, destFile.zip)
    unzip(destFile.zip, overwrite = TRUE, exdir = ".")
    
    # We should have a file called 'household_power_consumption.txt' in the cwd. 
    # Load it into a dataframe with the appropriate data types
    colClasses <- c("character", "character", rep("numeric", 7) )
    hpc = read.csv("./household_power_consumption.txt", header=TRUE, sep = ";", na.strings = "?", colClasses = colClasses, stringsAsFactors=FALSE)
    hpc$Date <- as.Date(hpc$Date, format="%d/%m/%Y")
    
    # Select a subset of the dataframe for the daterange specified below
    startDate <- as.Date("2007-02-01")    
    endDate <- as.Date("2007-02-02")     
    
    trimData <- hpc[hpc$Date >= startDate,] 
    trimData <- trimData[trimData$Date <= endDate,] 
    
    # Save the plot to a PNG file
    png(filename="./plot1.png", width=504, height=504)

    # Build the histogram
    main <- "Global Active Power"
    xlab <- "Global Active Power (kilowatts)"
    hist(trimData$Global_active_power, col="red", ylim=c(0,1200), main=main, xlab=xlab)
    
    dev.off()
}