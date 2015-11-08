# project 1 Exploring Data

# Reading the Data for Plotting
dataset_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(dataset_url, "Electric power consumption.zip")
unzip("Electric power consumption.zip", exdir = "electric_data")

base = read.table("./electric_data/household_power_consumption.txt",sep=";", header = TRUE)
#electric = read.table("./electric_data/household_power_consumption.txt",sep=";", header = TRUE)
electric=base
rm(base)


#head(electric[,1:2],50)

# I need to convert from Factor to Char! otherwise I get NAs
str(electric)
electric$Date = as.character(electric$Date)
electric$Time = as.character(paste(electric$Date,electric$Time))
electric$Date <- strptime(electric$Date, "%d/%m/%Y")
electric$Time <- strptime(electric$Time, "%d/%m/%Y %H:%M:%S")
str(electric)
# format(electric$Time, "%d/%m/%Y %H:%M:%S")[1:50] # Reformats from R's base to using / in dates
# format(electric$Time, "%H:%M:%S")[1:50] # REmoves the date portion from the POSXLT object
#electric

#summary(electric)
spl = electric[electric$Date>="2007-02-01" & electric$Date<"2007-02-03",]
str(spl)





# prepare data for Drawing
# Change to Characters columns 3-9

spl[,3:9]=sapply(spl[,3:9],as.character)
str(spl)
#Identify "?" in levels
#spl2[, 3:9][spl2[, 3:9] =="0.326"]
#which(spl2[, 3:9] =="NA")
spl[, 3:9][spl[, 3:9] =="?"]="NA"




# Convert Columns to Numeric as preparation
spl[,3:9]=sapply(spl[,3:9],as.numeric)
str(spl)
#head(spl2$Voltage,20)

# Create Time Series
par(mfrow=c(2,2),cex.axis=.75,cex.lab=.75)

# First Plot
plot(spl$Time,spl$Global_active_power,xlab="",ylab="Global Acitive Power", type="l")

# Second Plot
plot(spl$Time,spl$Voltage,xlab="datetime",ylab="Voltage", type="l")

# Third Plot
plot(spl$Time,spl$Sub_metering_1,xlab="",ylab="Energy sub metering", type="l")
lines(spl$Time,spl$Sub_metering_2,col="red")
lines(spl$Time,spl$Sub_metering_3,col="blue")
legend("topright",lty = c(1,1,1), cex=c(.75,.75,.75), bty="n",col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

# Fourth Plot
plot(spl$Time,spl$Global_reactive_power,xlab="datetime",ylab="Global_reactive_power", type="l")


# Create and save PNG File

dev.copy(png, file = "Plot 4.png") ## Copy my plot to a PNG file
dev.off() ## Don't forget to close the PNG device!


x <- 0:64/64
y <- sin(3*pi*x)
par(mfrow=c(1,1))
plot(x, y, type = "l", col = "blue",
     main = "points with bg & legend(*, pt.bg)")
points(x, y, pch = 21, bg = "white")
legend(.4,1, "sin(c x)", pch = 21, pt.bg = "white", lty = 1, col = "blue")

## legends with titles at different locations
plot(x, y, type = "n")
legend("bottomright", "(x,y)", pch = 1, title = "bottomright")
legend("bottom", "(x,y)", pch = 1, title = "bottom")
legend("bottomleft", "(x,y)", pch = 1, title = "bottomleft")
legend("left", "(x,y)", pch = 1, title = "left")
legend("topleft", "(x,y)", pch = 1, title = "topleft, inset = .05",
       inset = .05)
legend("top", "(x,y)", pch = 1, title = "top")
legend("topright", "(x,y)", pch = 1, title = "topright, inset = .02",
       inset = .02)
legend("right", "(x,y)", pch = 1, title = "right")
legend("center", "(x,y)", pch = 1, title = "center")

# using text.font (and text.col):
op <- par(mfrow = c(2, 2), mar = rep(2.1, 4))
c6 <- terrain.colors(10)[1:6]
for(i in 1:4) {
  plot(1, type = "n", axes = FALSE, ann = FALSE); title(paste("text.font =",i))
  legend("top", legend = LETTERS[1:6], col = c6,
         ncol = 2, cex = 2, lwd = 3, text.font = i, text.col = c6)
}
par(op)


