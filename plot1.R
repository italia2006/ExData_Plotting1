
if(!file.exists("mydata.txt"))
{
  #Read data
  data <- read.table("household_power_consumption.txt",header = TRUE,sep=";")
  
  #CleanUp and set Tidy
  
  data <- cbind(strptime(as.character(paste(data$Date,data$Time)),format= "%d/%m/%Y %H:%M:%S"),data)
  data <- data[-c(2,3)]
  colnames(data)[1] <- "datetime"
  colnames(data) <- tolower(gsub("_","",names(data)))
  
  DATE1 <- as.Date("2007-02-01")
  DATE2 <- as.Date("2007-02-02")
  data <- data[as.Date(data$datetime) %in% DATE1:DATE2,]
  
  write.table(data,"mydata.txt",sep=";",row.names = FALSE)
} else
{
  data <- read.table("mydata.txt",header = TRUE,sep=";")  
}


hist(data$globalactivepower,xlab="Global Active Power (kilowatts)",main="Global Active Power",col="red")

dev.copy(png,"plot1.png")






