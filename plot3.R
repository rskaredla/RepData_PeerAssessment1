# Project -1 : Base plotting
#  Plot-3: Line Plot with Legend
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
# plotting 
png(filename="plot3.png")
g_range<- range(power_data$Sub_metering_1,power_data$Sub_metering_2,power_data$Sub_metering_3)
with(power_data, plot(Sub_metering_1,
                      col="black",type="l",axes=FALSE,ann=FALSE))
lines(power_data$Sub_metering_2,col="red")
lines(power_data$Sub_metering_3,col="blue")
box()
legend("topright",lty=1,col=c("black","red","blue"),
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
axis(1,at=c(0,nmd/2,nmd),lab=c("Thu","Fri","Sat"))
axis(2,at=10*0:g_range[2])
title(ylab="Energy Sub metering")
dev.off()
#
#end