## plot4MultiPlot: create a 2x2 matrix of plots from the power consumption data
## over the days Feb 1-2 2007

## Drew upon several internet sources for info on how to make a line plot,
## combining columns with paste, and the fact that data.table does not support
## POSIX dates

plot4MultiPlot <- function() {
    # load the data.table library, which is faster
    # for loading this type of data
    library(data.table)
    
    # load the raw data; we know it to be separated by semicolons so we
    # indicate that. Initial read of the data indicated that there are
    # some values as '?', so we indicate that as NA; Furthermore, set 
    # data.table=FALSE (thus ending up with a data frame) because data.table 
    # does not support posix dates, hence later date conversion will not work 
    # with a data.table
    rawdata <- fread("household_power_consumption.txt", 
                     sep=';', 
                     na.strings="?",
                     data.table=FALSE)
    
    # subset the dates of interest - Feb 1-2 2007
    # date formats are (d)d/(m)m/yyyy, e.g.
    # for a single digit day, no leading zeroes
    daysdata <- rawdata[rawdata$Date=="1/2/2007"|rawdata$Date=="2/2/2007",]
    
    # convert date and time columns to a POSIX date object
    # This is accomplished by creating a new column that pastes the
    # Date and Time columns together as a single string, then
    # uses strptime based on that pasted format.
    daysdata$datetime <- strptime(paste(daysdata$Date, daysdata$Time), 
                                  format="%d/%m/%Y %H:%M:%S")
    
    # open graphics device for png. As plots are to be 480 x 480 pixels,
    # which is the default, we are ok
    png(filename="plot4.png")
    
    # Draw the plots by building up one by one
    # All four are line plots, so we use type="l" in each case
    # xlab and ylab are set as appropriate for x and y axis labels
    
    with (daysdata, {
        # First set up for a 2-by-2 plot with mfrow
        par(mfrow = c(2,2))
        
        # Top left is first, and is the Global_active_power plot
        plot(datetime, 
             Global_active_power, 
             type="l", 
             xlab="", 
             ylab="Global Active Power")
        
        # Top right is second, and is the Voltage plot
        plot(datetime,
             Voltage,
             type="l",
             xlab="datetime",
             ylab="Voltage")

        # Bottom left is third, and is the sub metering plot
        
        # begin with Sub_Metering_1, which is black (default). Add x and y 
        # labels with this call
        plot(daysdata$datetime, 
             daysdata$Sub_metering_1, 
             type="l", 
             xlab="", 
             ylab="Energy sub metering")
        
        # then add the points for Sub_Metering_2 in red. Add the points in as
        # a line with type="l"
        points(daysdata$datetime, daysdata$Sub_metering_2, type="l", col="red")
        
        # finally add points for Sub_Metering_3 in blue. Add the points in as
        # a line with type="l"
        points(daysdata$datetime, daysdata$Sub_metering_3, type="l", col="blue")
        
        # Add the desired legend - top right, colors matching the data series
        # and use of lty="solid" to indicate lines in the legend
        legend("topright", 
               col=c("black", "red", "blue"), 
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
               lty="solid")
        
        # Bottom right is last, and is the Global Reactive Power plot
        plot(datetime, 
             Global_reactive_power, 
             type="l", 
             xlab="datetime", 
             ylab="Global_reactive_power")
    })

    # Close the graphics device
    dev.off();
}