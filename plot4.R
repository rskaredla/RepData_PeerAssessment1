# Project -1 : Base plotting
#  Plot-4: Multiplots
#
project1f <- "household_power_consumption.txt"
#
#number of minutes in two day = number of data points
nmd <- 24*60*2
#
#read data (subset)
dataheader <- read.table(project1f, header=FALSE, sep=";",nrows=1,stringsAsFactors=FALSE)
power_data <- read.table(project1f, header=FALSE, sep=";",nrows=nmd,
                         skip=grep("1/2/2007",readLines(project1f))[1]-1)
colnames(power_data) <- unlist(dataheader)
#
# initial checks
names(power_data)
head(power_data)
tail(power_data)
#
# date & time format conversion
dates <- power_data$Date
times <- power_data$Time
x <- paste(dates, times)
y <- strptime(x, format=("%d/%m/%Y %H:%M:%S"))
#
png(filename="plot4.png")
#
# global environment setup
par(mfrow=c(2,2),cex.lab=0.75)

##############   plot C(1,1)   ##############   
with(power_data, plot(y,Global_active_power,type="l",ann=FALSE))
title(ylab="Global Active Power")
##############   plot C(1,2)   ##############   
with(power_data, plot(y,Voltage,type="l",ann=FALSE))
title(xlab="datetime")
title(ylab="Voltage")

##############   plot C(2,1)   ##############   
g_range<- range(power_data$Sub_metering_1,power_data$Sub_metering_2,power_data$Sub_metering_3)
with(power_data, plot(Sub_metering_1,
                      col="black",type="l",axes=FALSE,ann=FALSE))
lines(power_data$Sub_metering_2,col="red")
lines(power_data$Sub_metering_3,col="blue")
legend("topright",lty=1,col=c("black","red","blue"),cex=0.5,box.col="white",
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
axis(1,at=c(0,nmd/2,nmd),lab=c("Thu","Fri","Sat"))
axis(2,at=10*0:g_range[2],cex.axis=0.6)
title(ylab="Energy Sub metering")
box()
##############   plot C(2,2)   ##############   
with(power_data, plot(Global_reactive_power,type="l",axes=FALSE,ann=FALSE))
#                      xlab="datetime",ylab="Global_reactive_power"))
title(xlab="datetime")
title(ylab="Global_reactive_power")
axis(1,at=c(0,nmd/2,nmd),lab=c("Thu","Fri","Sat"))
axis(2,at=seq(0.0,0.5,0.1),cex.axis=0.4)
box()
#
dev.off()
#
#end