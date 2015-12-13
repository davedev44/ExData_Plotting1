## plot2GlobalActivePowerLine: create a line plot of global active power
## over the days Feb 1-2 2007

## Drew upon several internet sources for info on how to make a line plot,
## combining columns with paste, and the fact that data.table does not support
## POSIX dates

plot2GlobalActivePowerLine <- function() {
    # load the data.table library, which is faster
    # for loading this type of data
    library(data.table)
    
    # load the raw data; we know it to be separated by semicolons so we
    # indicate that. Initial read of the data indicated that there are
    # some values as '?', so we indicate that as NA; finally, since we
    # are only plotting the Global_active_power column by date and time, use
    # select to read only those columns. Furthermore, set data.table=FALSE
    # (thus ending up with a data frame) because data.table does not support
    # posix dates, hence later date conversion will not work with a data.table
    rawdata <- fread("household_power_consumption.txt", 
                     sep=';', 
                     na.strings="?", 
                     select=c("Date", "Time", "Global_active_power"),
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
    png(filename="plot2.png")

    # draw the line plot - since it has no points, we use type="l" to
    # generate a line plot. Set x and y axis labels as indicated.
    plot(daysdata$datetime, 
         daysdata$Global_active_power, 
         type="l",
         xlab="", 
         ylab="Global Active Power (kilowatts)")

    # Close the graphics device
    dev.off();
}