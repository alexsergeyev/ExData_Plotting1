getdata <- function(url, dest_file=basename(url)) {
  if (!file.exists(dest_file)) {
    download.file(url, destfile = dest_file, method = "curl")
  }
  dest_file
}

loaddata <- function(url) {
  # Use sed and command pipe to load only required set of data
  # sed 1p -> paste first line (header)
  # ^[1-2]/2/2007 -> paste everything mathing regex for 1/2/2007 and 2/2/2007
  # pipe -> paste shell output into R
  cmd <- paste0("unzip -p ", getdata(url)," | ","sed -n -e '1p;/^[1-2]\\/2\\/2007/p'")
  dataset <- read.csv(pipe(cmd), na.strings="?",sep=';', colClasses=c("factor","factor",rep('numeric',7)))
  dataset$datetime <- as.POSIXlt(strptime(format = '%d/%m/%Y %H:%M:%S', tz='UTC', paste(dataset$Date, dataset$Time)))
  dataset
}

plot4 <- function() {
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  dataset <- loaddata(url)
  
  par(mfrow = c(2, 2), cex=0.55)
  with(dataset, {
    plot(datetime, dataset$Global_active_power, xlab = '', ylab = 'Global Active Power', type='l')
    plot(datetime, Voltage, type='l')
    plot(dataset$datetime, dataset$Sub_metering_1, type='l', xlab = '', ylab = 'Energy sub metering')
    points(dataset$datetime,dataset$Sub_metering_2,col='red',type='l')
    points(dataset$datetime,dataset$Sub_metering_3,col='blue',type='l')
    legend("topright", bty = "n", lty=1, col = c("black","red", "blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    plot(datetime, Global_reactive_power, type='l')
  })
  
  dev.copy(png,'plot4.png')
  dev.off()
}
