## plot1GlobalActivePowerHist: create a histogram of global active power
## in kilowatts for the days Feb 1-2 2007

plot1GlobalActivePowerHist <- function() {
    # load the data.table library, which is faster
    # for loading this type of data
    library(data.table)
    
    # load the raw data; we know it to be separated by semicolons so we
    # indicate that. Initial read of the data indicated that there are
    # some values as '?', so we indicate that as NA; finally, since we
    # are only plotting the Global_active_power column by date, use
    # select to read only those columns
    rawdata <- fread("household_power_consumption.txt", 
                     sep=';', 
                     na.strings="?", 
                     select=c("Date", "Global_active_power"))

    # subset the dates of interest - Feb 1-2 2007
    # date formats are (d)d/(m)m/yyyy, e.g.
    # for a single digit day, no leading zeroes
    daysdata <- rawdata[rawdata$Date=="1/2/2007"|rawdata$Date=="2/2/2007"]
    
    # now subset out the global active power column, and
    # make it numeric (as it is a string in the raw data)
    globalActivePowerData <- as.numeric(daysdata$Global_active_power);
    
    # open graphics device for png. As plots are to be 480 x 480 pixels,
    # which is the default, we are ok
    png(filename="plot1.png")
    
    # Draw the histogram for global active power, setting, in order,
    # color red, x and y labels, and the title.
    hist(globalActivePowerData, 
         col="red", 
         xlab="Global Active Power (kilowatts)", 
         ylab="Frequency",
         main="Global Active Power");

    # Close the graphics device
    dev.off();
}