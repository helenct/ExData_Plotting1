plot2 <- function() {
        
        ## DATA PROCESSING
        ## Read in the data
        data <- read.table("household_power_consumption.txt", sep = ";",
                           colClasses = "character", header = TRUE)
        
        ## Cut data to include only relevant dates
        day1 <- "1/2/2007"
        day2 <- "2/2/2007"
        cut_data <- data[data$Date==day1 | data$Date==day2, ]
        
        ## Create new datetime column in POSIXct format, add it to data frame
        ## and remove date and time columns
        datetime <- paste(cut_data[,1],cut_data[,2])
        datetime <- as.POSIXct(datetime, format="%d/%m/%Y %H:%M:%S")
        cut_data <- data.frame(datetime, cut_data[,3:ncol(cut_data)])
        
        
        ## Convert remaining columns to numeric
        cut_data[,2:ncol(cut_data)] <- apply(cut_data[,2:ncol(cut_data)], 2, as.numeric)
        
        ## PLOTTING
        ## Set up axis labels
        ticks = c(min(cut_data$datetime), mean(cut_data$datetime), 
                  max(cut_data$datetime))
        tick_labels = c("Thurs", "Fri", "Sat")
        
        ## Plot graph
        png("plot2.png")
        with(cut_data, plot(datetime, Global_active_power, type="l", xaxt="n",
                            xlab="", ylab = "Global Active Power (kilowatts)"))
        axis(side=1, at=ticks, labels=tick_labels)
        dev.off()
}