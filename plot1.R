# Project -1 : Base plotting
#  Plot-1: Histogram Plot
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
png(filename="plot1.png")
hist(power_data$Global_active_power,col="red",main="Global Active Power",
     xlab="Global Active Power (kilowatts)")
dev.off()
#
#end
