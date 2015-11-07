
#========================================================================================
# plot2.R - uses Electric Power Consumption data from:
# https://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption
# builds a reduced sized dataset (1st and 2nd of Feb 2007 only)
#
# and plots Global Active Power over POSIX DateTime
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
png(file="plot2.png", width = 480, height = 480)
plot(EPC$POSIXDateTime,EPC$Global_active_power,type = "l",main = "", xlab = "", ylab = "Global Active Power (kilowatts)")
dev.off()
