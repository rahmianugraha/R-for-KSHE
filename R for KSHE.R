#install.packages(readxl)
setwd(".")

# Import dataset
library(readxl)
LeafData <- read_excel('Leaf morphology - (eWCC 2018).xlsx')

# Let's check the data
str(LeafData)
summary(LeafData)

unique(LeafData$Collector)
unique(LeafData$TrunkSize)
unique(LeafData$LeafShape)
unique(LeafData$Location)
unique(LeafData$country)


# DATA CLEANING
library(car)
# TrunkSize Cleaning
LeafData$TrunkSize <- recode(LeafData$TrunkSize, "'sapling'= 'Sapling'")
LeafData$TrunkSize <- recode(LeafData$TrunkSize, "'pole'= 'Pole'")
LeafData$TrunkSize <- recode(LeafData$TrunkSize, "'medium'= 'Medium'")
LeafData$TrunkSize <- recode(LeafData$TrunkSize, "'large'= 'Large'")
LeafData$TrunkSize <- recode(LeafData$TrunkSize, "'small'= 'Small'")

# LeafShape Cleaning
LeafData$LeafShape <- recode(LeafData$LeafShape, "'acicular'= 'Acicular'")
LeafData$LeafShape <- recode(LeafData$LeafShape, "'even-pinnate'= 'Even-pinnate'")
LeafData$LeafShape <- recode(LeafData$LeafShape, "'lobed'= 'Lobed'")
LeafData$LeafShape <- recode(LeafData$LeafShape, "'palmate'= 'Palmate'")
LeafData$LeafShape <- recode(LeafData$LeafShape, "'obovate'= 'Obovate'")
LeafData$LeafShape <- recode(LeafData$LeafShape, "'ovate'= 'Ovate'")
LeafData$LeafShape <- recode(LeafData$LeafShape, "'cordate'= 'Cordate'")
LeafData$LeafShape <- recode(LeafData$LeafShape, "'falcae'= 'Falcae'")
LeafData$LeafShape <- recode(LeafData$LeafShape, "c('odd-pinnate', 'odd pinnate', 'Odd Pinnate')= 'Odd-pinnate'")
LeafData$LeafShape <- recode(LeafData$LeafShape, "c('lanceolate', 'lanecolate')= 'Lanceolate'")

# Location Cleaning
LeafData$Location <- recode(LeafData$Location, "c('semi-urban', 'Semi-Urban')= 'Semi-urban'")



# DATA EXPLORATION
LeafData[1,] # Select first row of data.frame
LeafData[,1] # Select first column 

LeafData[c(3,4),] # Select rows 3 and 4 
LeafData[10:15,] # Select rows 10 to 15 inclusive 
LeafData[20:22, 1:3] # Select rows 20 to 22 and columns 1 to 3

LeafData[1:5,"TrunkSize"] # Select rows 1 to 5 in the TrunkSize column
LeafData[1:5,c("Latitude","Longitude")] # Rows 1-5 of Latitude and Longitude 

#the data collected by you personally
LeafData[LeafData$Collector == "Rahmia N",]
#extract leaves collected in the same country
LeafData[LeafData$country == "Singapore",]
#(NY)extract leaves collected at a similar latitude
LeafData[LeafData$Latitude == "LeafData$Latitude>1-0.5, LeafData$Latitude<1+0.5",]
# Leaves collected south of the equator
LeafData[LeafData$Latitude < 0,]
# Widths of Acicular leaves
LeafData[LeafData$LeafShape == "Acicular", "Width_mm"]
LeafData$LeafShape == "Acicular"

# Angiosperm records where leaves are <100mm long
LeafData[LeafData$Angio_Gymno == "Angiosperm" & LeafData$Length_mm < 100,]
# Width of non-Acicular leaves 
LeafData[LeafData$LeafShape != "Acicular", "Width_mm"]

#Calculate new columns
LeafIndex <- LeafData$Length_mm/LeafData$Width_mm
cbind(LeafData, LeafIndex)
LeafDataLeafIndex <- LeafData$Length_mm/LeafData$Width_mm

#Rename columns
names(LeafData)[names(LeafData) == "LeafIndex"]
names(LeafData)[names(LeafData) == "LeafIndex"] <- "LeafRatio"



# SUMMARY STATISTICS
summary(LeafData)
summary(LeafData$country)

table(LeafData$Collector)
table(LeafData$TrunkSize)
table(LeafData$LeafShape)
table(LeafData$Location)
table(LeafData$country)

table(LeafData$country, LeafData$LeafShape)
table(LeafData$country, LeafData$LeafShape, LeafData$Angio_Gymno) 
table(LeafData$LeafShape)
table(LeafData$LeafShape, LeafData$TrunkSize)


#Try: Central	tendency	and	dispersion
xdata <- c(1,3,4,5,6,6,6,8,9,12,NA)
hist(xdata)
as.numeric(names(which.max(table(xdata))))

median(xdata)
median(xdata, na.rm=TRUE)
mean(xdata, na.rm=TRUE)
var(xdata, na.rm=TRUE)
sd(xdata, na.rm=TRUE)
min(xdata, na.rm=TRUE)
max(xdata, na.rm=TRUE)
range(xdata, na.rm=TRUE)
quantile(xdata, probs=c(0.25, 0.75),na.rm=TRUE)
IQR(xdata, na.rm=TRUE)


# Summary Statistics for Leaf Length Across Locations
tapply(LeafData$Length_mm, LeafData$Location, mean, na.rm=TRUE)
tapply(LeafData$Length_mm, LeafData$Location, median, na.rm=TRUE)
#tapply(LeafData$Length_mm, LeafData$Location, mode, na.rm=TRUE)
tapply(LeafData$Length_mm, LeafData$Location, var, na.rm=TRUE)
tapply(LeafData$Length_mm, LeafData$Location, sd, na.rm=TRUE)
tapply(LeafData$Length_mm, LeafData$Location, min, na.rm=TRUE)
tapply(LeafData$Length_mm, LeafData$Location, max, na.rm=TRUE)
tapply(LeafData$Length_mm, LeafData$Location, IQR, na.rm=TRUE)

# Summary Statistics for Leaf Width Across Locations
#tapply(LeafData$Width_mm, list(LeafData$LeafShape, LeafData$country), mean, na.rm=TRUE)


# RESEARCH QUESTIONS
# Broad question: Does location affect tree morphology?
# Specific questions:
  #1.	Does location affect leaf length?
  #2.	Does location affect leaf width?
  #3. Does location affect trunk size?
  
# Use ANOVA to answer the questions
#1.	Does location affect leaf length?
fit <- aov(LeafData$Length_mm ~ LeafData$Location, data=LeafData)
summary(fit)
#2.	Does location affect leaf width?
fit <- aov(LeafData$Width_mm ~ LeafData$Location, data=LeafData)
summary(fit)

# Use Chi-square to answer the question
#3. Does location affect trunk size?
tbl <- table(LeafData$Location, LeafData$TrunkSize)
chisq <- chisq.test(tbl)
chisq.test(tbl)


#Create	a	frequency	histogram
hist(LeafData$Length_mm)
hist(LeafData$Length_mm, breaks = 20) # specifies 20 breaks
hist(LeafData$Width_mm, breaks = 50)


# DATA VISUALIZATION
# Import dataset
data_kukang <- read.csv("F:/PROJECT/R!/R for KSHE/Nycticebus coucang.csv")
str(data_kukang)
summary(data_kukang)
table(data_kukang$countryCode)
table(data_kukang$year)
names(data_kukang)

library(plyr)
# COUNT YEAR
year_kukang <- count(data_kukang, "year")
year_kukang
sum(year_kukang$freq)
# Remove NA
year_kukang <- data_kukang[year_kukang$year!='NA',]

# COUNT COUNTRY CODE
country_kukang <- count(data_kukang, "countryCode")
country_kukang
sum(country_kukang$freq)

#COUNT INSTITUTION CODE
institution_kukang <- count(data_kukang, "institutionCode")
institution_kukang
sum(institution_kukang$freq)

# CREATE PLOTS
library(ggplot2)
# LINE PLOT (Year & Frequency)
ggplot(year_kukang, aes(x=year_kukang$year, y=year_kukang$freq, fill=year_kukang$year, group=1)) +
  geom_line(stat="identity") + xlab("Year") + ylab("Frequency")
#plot(year_kukang$year, year_kukang$freq, type = "l", lty = 1)

# BAR PLOT (Country Code & Frequency)
ggplot(country_kukang, aes(x=country_kukang$countryCode, y=country_kukang$freq, fill=country_kukang$countryCode)) +
  geom_bar(stat = "identity") +
  xlab("Country Code") + ylab("Frequency") + theme(legend.position="right") +
  scale_fill_discrete(name = "Country", labels = c("Indonesia", "India", "Kenya", "Laos", "Myanmar", "Malaysia", "Philippines", "Singapore", "Thailand", "United states", "Vietnam"))
#geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25)

# BAR PLOT (Institution Code & Frequency)
ggplot(institution_kukang, aes(x=institution_kukang$institutionCode, y=institution_kukang$freq, fill=institution_kukang$institutionCode)) +
  geom_bar(stat = "identity") +
  xlab("Institution Code") + ylab("Frequency") + theme(legend.position="right")
  #scale_fill_discrete(name = "Country", labels = c("Indonesia", "India", "Kenya", "Laos", "Myanmar", "Malaysia", "Philippines", "Singapore", "Thailand", "United states", "Vietnam"))




#Types	of	plot	in	R
#Pie Charts
table(LeafData$LeafShape)
pie(table(data_kukang$institutionCode))

#Bar Charts
plot(LeafData$LeafShape)
#Stacked
barplot(table(LeafData$Angio_Gymno, LeafData$LeafShape), legend = TRUE) 
#Side by side
barplot(table(LeafData$Angio_Gymno, LeafData$LeafShape), legend = TRUE, beside= TRUE) 

#Bar chart of mean leaf length for different trunk sizes (TrunkSize), separating them by whether they’re angiosperms or gymnosperms (Angio_Gymno)
barplot(tapply(LeafData$Length_mm, # Variable to summarize
               list(LeafData$TrunkSize, # 1st grouping variable
                    LeafData$Angio_Gymno), # 2nd grouping column 
               mean, # Summary statistic 
               na.rm = TRUE), 
        legend = TRUE, 
        beside = TRUE)

#Scatter	plots	and	the	identify()	function	
plot(LeafData$Length_mm, LeafData$Width_mm)
identify(LeafData$Length_mm, LeafData$Width_mm) 
LeafData[2,]
identify(LeafData$Length_mm, LeafData$Width_mm, LeafData$LeafShape)


#Challenge 2f: Modify plots
#Section 1 Modify plot symbols and colours in R
#Use different point symbols
# create x and y values from 1 to 25, # draw symbols 1 to 25, # double the symbol size in all dimensions
plot(1:25, 1:25, pch = 1:25, cex= 0.5) 

#Structure code to help diagnose errors
plot(1:25,	# y values from 1 to 25
     1:25,	# x values from 1 to 25
     pch = 1:25,  # symbols 1 to 25
     cex = 2	# increase the plot size
)

#Work with colours
1:5	# Sequence of numbers from 1 to 5
1:5/5 # Rescale so sequence is between 0 and 1 
grey(1:5/5) # Generate colour codes
rainbow(6) # 6 colours from the rainbow
colors()[2:20]

pie(rep(1, 20), # create a vector of twenty 1's
    col = grey(seq(0, 2, 1/20))# create a sequence of 20 grey levels to shade the slices
)
pie(rep(1, 20),	# create a vector of twenty 1's
    col = rainbow(6) # colour the slices by splitting the rainbow into 20 colours
)

pie(rep(1, 20),	# create a vector of twenty 1'ss
    col = colours[1:20] # colour slices using the first 20 name colours
)

#Apply symbol and colour techniques to real data
hist(LeafData$Length_mm, col = "Light green")
scatterplot(LeafData$Length_mm, LeafData$Width_mm, pch = 17, # Use 	filled triangle
            col = "Dark red")

#Use unique symbology for different subsets of the data
length(levels(LeafData$Angio_Gymno))
rainbow(length(levels(LeafData$Angio_Gymno))[ # define the number of colours
  as.numeric(LeafData$Angio_Gymno)]) # choose colour 1 or 2 depending on whether the leaf is from a Rural or Urban or Semi-urban location
plot(LeafData$Width_mm, LeafData$Length_mm, pch = 1, # Use filled circle
     col = rainbow(
       length(levels(LeafData$Angio_Gymno)))[
         as.numeric(LeafData$Angio_Gymno)] # Create two rainbow colours, then choose which of them for each point to use depending on which category of Angio_Gymno that datapoint is
)

#Add a legend to a plot
legend("topleft",	# Where to place legend
       levels(LeafData$Angio_Gymno), # Labels for our legend categories 
       pch = 16, # Same point character as plot
       col = rainbow(length(levels(LeafData$Length_mm))) # Same colours as plot
)
#For a journal paper, we might want to draw Angiosperms as black crosses, and Gymnosperms as filled circles.
plot(LeafData$Length_mm, LeafData$Width_mm, type = "n") # Draw the data and the axes
points(LeafData$Length_mm[LeafData$Angio_Gymno == "Angiosperm"], # 	x data 
       LeafData$Width_mm[LeafData$Angio_Gymno == "Angiosperm"], # y data
       pch = 3 # specify a cross symbol
)
points(LeafData$Length_mm[LeafData$Angio_Gymno == "Gymnosperm"], # 	x	data 
       LeafData$Width_mm[LeafData$Angio_Gymno == "Gymnosperm"], # y data
       pch = 16 # specify a filled circle 	symbol )
)


#Section 2 Add text to plots
plot(LeafData$Length_mm, LeafData$Width_mm, 
     xlab = "Leaf length (mm)",	# x axis label
     ylab = "Leaf width (mm)" # y axis label
)
plot(LeafData$Length_mm, LeafData$Width_mm, main = "Leaf dimensions", # title
     sub = "Length is correlated with width" # subtitle 
)

#Add text outside and inside the data area of the plot
mtext("Cedric Tan, 13th October 2018", side = 3,
      line = 0.5,
      at = 100
)
text(140,	# y-coordinate of text
     250,	# x-coordinate of text
     "I can write here!", 
     col = "Green"	# Colour of text
)


#Section 3 Export figures and draw multiple plots in a single figure
#It’s possible to export plots from RStudio by clicking on the ‘Export…’ link above the plot, but let’s use a code for exporting plots since we are getting more comfortable with code hehe.

#Draw multiple plots in the same figure
backup.par <- par(no.readonly = T) # Store existing graphical parameters
par(mfrow = c(1,2)) # Plot layout with 2 rows and 1 column
hist(LeafData$Length_mm[LeafData$Angio_Gymno == "Angiosperm"])
hist(LeafData$Length_mm[LeafData$Angio_Gymno == "Gymnosperm"])

seq(0, 400, 20) # proposed breaks to insert into the hist() function
hist(LeafData$Length_mm[LeafData$Angio_Gymno == "Angiosperm"], xlim = c(0, 200),
     breaks = seq(0, 200, 20))
hist(LeafData$Length_mm[LeafData$Angio_Gymno == "Gymnosperm"], xlim = c(0, 400),
     breaks = seq(0, 200, 20))

