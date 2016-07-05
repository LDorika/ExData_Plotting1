if(!file.exists("exdata-data-household_power_consumption.zip")) {
  temp <- tempfile()
  download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
  file <- unzip(temp)
  unlink(temp)
}
power <- read.table(file, header=T, sep=";")
power$Date <- as.Date(power$Date, format="%d/%m/%Y")
dat <- power[(power$Date=="2007-02-01") | (power$Date=="2007-02-02"),]
dat$Global_active_power <- as.numeric(as.character(dat$Global_active_power))
dat$Global_reactive_power <- as.numeric(as.character(dat$Global_reactive_power))
dat$Voltage <- as.numeric(as.character(dat$Voltage))
dat <- transform(dat, timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
dat$Sub_metering_1 <- as.numeric(as.character(dat$Sub_metering_1))
dat$Sub_metering_2 <- as.numeric(as.character(dat$Sub_metering_2))
dat$Sub_metering_3 <- as.numeric(as.character(dat$Sub_metering_3))

#Plot 1
plot1 <- function() {
  hist(dat$Global_active_power, main = paste("Global Active Power"), col="red", xlab="Global Active Power (kilowatts)")
  dev.copy(png, file="plot1.png", width=480, height=480)
  dev.off()
  cat("Plot1.png has been saved in", getwd())
}
plot1()

#plot2
plot2 <- function() {
  plot(dat$timestamp,dat$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
  dev.copy(png, file="plot2.png", width=480, height=480)
  dev.off()
  cat("plot2.png has been saved in", getwd())
}
plot2()

#plot3
plot3 <- function() {
  plot(dat$timestamp,dat$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
  lines(dat$timestamp,dat$Sub_metering_2,col="red")
  lines(dat$timestamp,dat$Sub_metering_3,col="blue")
  legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), lwd=c(1,1))
  dev.copy(png, file="plot3.png", width=480, height=480)
  dev.off()
  cat("plot3.png has been saved in", getwd())
}
plot3

#plot4
plot4 <- function() {
  par(mfrow=c(2,2))
  
  ##PLOT 1
  plot(dat$timestamp,dat$Global_active_power, type="l", xlab="", ylab="Global Active Power")
  ##PLOT 2
  plot(dat$timestamp,dat$Voltage, type="l", xlab="datetime", ylab="Voltage")
  
  ##PLOT 3
  plot(dat$timestamp,dat$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
  lines(dat$timestamp,dat$Sub_metering_2,col="red")
  lines(dat$timestamp,dat$Sub_metering_3,col="blue")
  legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), bty="n", cex=.5) #bty removes the box, cex shrinks the text, spacing added after labels so it renders correctly
  
  #PLOT 4
  plot(dat$timestamp,dat$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
  
  #OUTPUT
  dev.copy(png, file="plot4.png", width=480, height=480)
  dev.off()
  cat("plot4.png has been saved in", getwd())
}
plot4()