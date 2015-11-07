
#========================================================================================
# plot4.R - uses Electric Power Consumption data from:
# https://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption
# builds a reduced sized dataset (1st and 2nd of Feb 2007 only)
#
# and constructs 4 graphs:
#   1) plot showing Global Active Power over POSIX DateTime
#   2) plot showing Voltage over POSIX DateTime
#   3) plot showing the 3 energy sub-metering profiles over POSIX DateTime
#   4) plot showing Global Reactive Power over POSIX DateTime
# 
#========================================================================================


#========================================================================================
# prepare.EPC - reads the Electric Power Consumption File from current working directory
# Filters for reduced date range (1st and 2nd of Feb 2007 only) and adds useful additional
# columns for further analysis.
#
# Assumes household_power_consumption.txt unzipped and present in current working directory
#
#========================================================================================

prepare.EPC <- function(){
  #Dirty read of 1st 2 lines to grab native header
  df.head<-read.table(file="household_power_consumption.txt",nrows=1,sep=";",header=TRUE)
  #Grab only 2880 minutes (2 days) from file
  suppressWarnings(df<-read.table(file="household_power_consumption.txt"
                 ,skip=grep("1/2/2007", readLines("household_power_consumption.txt"))-1
                 ,nrows=2880,sep=";",col.names = colnames(df.head), na.strings = "?"))
  
  #add useful cols, cast to new data types
  df$DateTime = paste(df$Date, df$Time)
  df$POSIXDateTime=strptime(df$DateTime,format="%d/%m/%Y %H:%M:%S", tz="")
  df
}

#Prepare analysis dataset
EPC <- prepare.EPC()

#Open PNG device, add plot and close
png(file="plot4.png", width = 480, height = 480)

#Configure the graphic params
par(mfrow=c(2,2),mar=c(5, 4, 4, 2))

#Add plot1
plot(EPC$POSIXDateTime,EPC$Global_active_power,type = "l",main = "", xlab = "", ylab = "Global Active Power (kilowatts)")

#Add plot2
plot(EPC$POSIXDateTime,EPC$Voltage,type = "l",col="black",main = "", xlab = "datetime", ylab = "Voltage")

#Add plot3
#find the merged y-axis range
yrange<-range(c(EPC$Sub_metering_1,EPC$Sub_metering_2,EPC$Sub_metering_3))
#add the 3 profiles, overlaid using par(new=T)
plot(EPC$POSIXDateTime,EPC$Sub_metering_1,type = "l",col = "black", main = "", xlab = "", ylab = "Energy sub metering", ylim = yrange)
par(new=T)
plot(EPC$POSIXDateTime,EPC$Sub_metering_2,type = "l",col = "red", main = "", xlab = "", ylab = "Energy sub metering", ylim = yrange)
par(new=T)
plot(EPC$POSIXDateTime,EPC$Sub_metering_3,type = "l",col = "blue", main = "", xlab = "", ylab = "Energy sub metering", ylim = yrange)
#add legend - no border
legend("topright", col=c("black","red","blue"), lty=1, legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), bty = "n")
par(new=F)

#Add plot4
plot(EPC$POSIXDateTime,EPC$Global_reactive_power, col="black",type = "l", main = "", xlab = "datetime", ylab = "Global_reactive_power")

dev.off()
