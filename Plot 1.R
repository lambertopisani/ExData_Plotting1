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

# Create Hist
hist(spl$Global_active_power, col="red",main = "Global Active Power",xlab="Global Acitive Power (kilowatts)")
dev.copy(png, file = "Plot 1.png") ## Copy my plot to a PNG file
dev.off() ## Don't forget to close the PNG device!




