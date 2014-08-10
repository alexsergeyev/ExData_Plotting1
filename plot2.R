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

plot2 <- function() {
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  dataset <- loaddata(url)
  plot(dataset$datetime, dataset$Global_active_power, type='l', xlab = '', ylab = 'Global Active Power (kilowatts)')
  dev.copy(png,'plot2.png')
  dev.off()
}
