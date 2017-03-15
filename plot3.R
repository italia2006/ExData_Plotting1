
require(data.table)
require(lubridate)

#Read data
data <- read.table("household_power_consumption.txt",header = TRUE,sep=";")

#CleanUp and set Tidy

data <- cbind(strptime(as.character(paste(data$Date,data$Time)),format= "%d/%m/%Y %H:%M:%S"),data)
colnames(data) <- tolower(gsub("_","",names(data)))

data <- data[-c(2,3)] #remove Date and Time columns
colnames(data)[1] <- "datetime"

DATE1 <- as.Date("2007-02-01")
DATE2 <- as.Date("2007-02-02")
data <- data[as.Date(data$datetime) %in% DATE1:DATE2,]

data$globalactivepower <- as.numeric(as.character(data$globalactivepower))
data$globalreactivepower <- as.numeric(as.character(data$globalreactivepower))
data$voltage <- as.numeric(as.character(data$voltage))
data$globalintensity <- as.numeric(as.character(data$globalintensity))
data$submetering1 <- as.numeric(as.character(data$submetering1))
data$submetering2 <- as.numeric(as.character(data$submetering2))
data$submetering3 <- as.numeric(as.character(data$submetering3))

data <- cbind(as.numeric(data$datetime),data)
colnames(data)[1] <- "ndate"

data <- cbind(wday(data$datetime, label=FALSE),data)
colnames(data)[1] <- "weekdays"

data <- cbind(wday(data$datetime, label=TRUE),data)
colnames(data)[1] <- "daylabel"
data$daylabel <- as.character(data$daylabel)


plot(data$ndate,data$submetering1,xaxt='n',pch="", xlab="", ylab="Energy sub metering")
lines(data$ndate,data$submetering1)
lines(data$ndate,data$submetering2,col="red")
lines(data$ndate,data$submetering3,col="blue")

xaxe <- c(min(which(data$weekdays %in% 5:7)),min(which(data$weekdays==6)), min(which(data$weekdays==7)))
labelx <- as.character(data[xaxe,1])

vals <- data[xaxe,3]
axis(1,at=vals, labels=labelx)

legend("topright", legend=c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"), 
       col=c("black","red", "blue"), lty=1, cex=0.8,y.intersp=2)

dev.copy(png,"plot3.png")







