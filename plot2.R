# Project -1 : Base plotting
#  Plot-2: Line Plot
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
# plotting 
png(filename="plot2.png")
with(power_data, plot(y,Global_active_power,type="l",xlab="",ylab="Global Active Power (kilowatts)"))
dev.off()
#
#end