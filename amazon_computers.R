# this is data analysis of computer price/specs from amazon
# setwd([CURRENT DIRECTORY])
dirstr <- as.character(getwd())
filename <- "/AmazonComputers-02-Default.csv"

path <- paste0(dirstr, filename)

amazon <- read.csv(file = path, header = TRUE, stringsAsFactors = FALSE)

str(amazon)

# get rid of line items that have errors
amazon <- subset(amazon, Error == "")
# drop ItemID and Error column 
amazon <- amazon[,-c(1,16)]

# get rid of rows with blanks
amazon <- amazon[complete.cases(amazon), ]

# Hard part with these data:
# we need to see if we can narrow down the factors
# do each one by one

#************************* PROCESSORS *************************

# Start with intel
intel_index1 <- grep("intel", amazon$Processor, ignore.case = TRUE)

amazon$Processor[intel_index1]
# so it actually looks pretty good--
# just going to write a function which turns the long names
# into just i3, i5, etc

# name transformation function

# This function returns the index of the observations we wish to keep by default
# If we set remove_index to FALSE, it will return the newly formatted
# processors vector with uniform names
# we want to use this function with remove_index = TRUE first, and
# later use the vector it can give us

processor_name <- function(processor_vector, remove_index = TRUE) {
  # intel processors
  
  # create indexes for processors that contain these strings
  i3_index <- grep("i3", processor_vector, ignore.case = TRUE)
  i5_index <- grep("i5", processor_vector, ignore.case = TRUE)
  i7_index <- grep("i7", processor_vector, ignore.case = TRUE)
  i9_index <- grep("i9", processor_vector, ignore.case = TRUE)
  pent_index <- grep("pentium", processor_vector, ignore.case = TRUE)
  
  # create total intel index
  intel_index <- c(i3_index, i5_index, i7_index, i9_index, pent_index)
  
  # change names to be uniform in original vector
  processor_vector[i3_index] <- "i3"
  processor_vector[i5_index] <- "i5"
  processor_vector[i7_index] <- "i7"
  processor_vector[i9_index] <- "i9"
  processor_vector[pent_index] <- "Pentium"
  
  # amd processors
  
  # create indexes for processors that contain these strings
  FX_index <- grep("FX", processor_vector, ignore.case = TRUE)
  R3_index <- grep("Ryzen 3", processor_vector, ignore.case = TRUE)
  R5_index <- grep("Ryzen 5", processor_vector, ignore.case = TRUE)
  R7_index <- grep("Ryzen 7", processor_vector, ignore.case = TRUE)
  R_index <- grep("R Series", processor_vector, ignore.case = TRUE)
  A_index <- grep("A Series", processor_vector, ignore.case = TRUE)
  
  # create total amd index
  amd_index <- c(FX_index, R3_index, R5_index, R7_index, R_index, A_index)
  
  # change names to be uniform in original vector
  processor_vector[FX_index] <- "FX"
  processor_vector[R3_index] <- "Ryzen 3"
  processor_vector[R5_index] <- "Ryzen 5"
  processor_vector[R7_index] <- "Ryzen 7"
  processor_vector[R_index] <- "R Series"
  processor_vector[A_index] <- "A Series"
  
  # create final index of all found names
  final_index <- c(intel_index, amd_index)
  
  # create final vector with unused variables removed
  return_vector <- processor_vector[final_index]
  
  if (remove_index) {
    return(final_index)
  } else {
    return(return_vector)
  }
  
}

# get index of usable processors
index1 <- processor_name(amazon$Processor)

# subset data to only include usable processors
amazon_new1 <- amazon[index1,]

# subset processor vector in dataframe to only include formatted names
amazon_new1$Processor <- processor_name(amazon_new1$Processor, remove_index = FALSE)

# change processor from char to factor
amazon_new1$Processor <- as.factor(amazon_new1$Processor)

# check result
str(amazon_new1)

#************************* RAM ************************* 

# remove few observations where RAM equals 0 (for some reason)
amazon_new1 <- subset(amazon_new1, RAM.GB != 0)

# change RAM to factor
amazon_new1$RAM.GB <- as.factor(amazon_new1$RAM.GB)

#************************* HARD DRIVE STORAGE *************************
# obtain index of usable fields
GB_index <- grep("GB", amazon_new1$Hard.Drive)
TB_index <- grep("TB", amazon_new1$Hard.Drive)

HD_index <- c(GB_index, TB_index)

# subset by usable fields
amazon_new2 <- amazon_new1[HD_index,]

# find fields that have TB in them
hasTB_index <- grep("TB", amazon_new2$Hard.Drive)
amazon_new2$hasTB <- rep.int(0, dim(amazon_new2)[1])

amazon_new2$hasTB[hasTB_index] <- 1

# after checking data, realized that we don't need below functions, but are nice for future reference...
# looks like parse_number takes the first number in the string, which is convenient for our purposes
# get only the parts that have TB or GB in them
# amazon$Hard.Drive <- gsub("^([0-9]* ?GB).+", "\\1", amazon$Hard.Drive, perl = TRUE)
# amazon$Hard.Drive <- gsub("^([0-9]* ?TB).+", "\\1", amazon$Hard.Drive, perl = TRUE)

# attach readr package for extracting number
library(readr)
# extract numbers from Hard.Drive vector and put them in new HD variable
amazon_new2$HD <- parse_number(amazon_new2$Hard.Drive)

# now convert to GB
amazon_new2$HD <- ifelse(amazon_new2$hasTB == 0, amazon_new2$HD, amazon_new2$HD * 1000) 

# spot check data
# look at observations in the middle where GB turns into TB
amazon_new2$HD
# what's cookin good lookinnnn
# going to go ahead and keep all the columns until 
# I feel I absolutely need to drop one

# look at unique HD's to see if we should convert to factor
unique(amazon_new2$HD)
# no, don't change to factor

#************************* GRAPHICS *************************
# I'm going to go ahead and boil these down to brand,
# I don't think it will be productive here to do brand and type

# substitute long names for short ones, but in new vector
amazon_new2$Graphics_Brand <- amazon_new2$Graphics
# main brands
amazon_new2$Graphics_Brand <- gsub("^.*?(Intel).*", "\\1", amazon_new2$Graphics_Brand, ignore.case = TRUE, perl = TRUE)
amazon_new2$Graphics_Brand <- gsub("^(intel).*", "\\1", amazon_new2$Graphics_Brand, ignore.case = TRUE, perl = TRUE)
amazon_new2$Graphics_Brand <- gsub("^(AMD).*", "\\1", amazon_new2$Graphics_Brand, ignore.case = TRUE, perl = TRUE)
amazon_new2$Graphics_Brand <- gsub("^(NVIDIA).*", "\\1", amazon_new2$Graphics_Brand, ignore.case = TRUE, perl = TRUE)
# substitute the stragglers
# intel
amazon_new2$Graphics_Brand <- gsub("intel", "Intel", amazon_new2$Graphics_Brand, ignore.case = TRUE, perl = TRUE)
# nvidia
amazon_new2$Graphics_Brand <- gsub(".*GTX.*", "NVIDIA", amazon_new2$Graphics_Brand, ignore.case = TRUE, perl = TRUE)
amazon_new2$Graphics_Brand <- gsub(".*RTX.*", "NVIDIA", amazon_new2$Graphics_Brand, ignore.case = TRUE, perl = TRUE)
amazon_new2$Graphics_Brand <- gsub(".*Nvidia.*", "NVIDIA", amazon_new2$Graphics_Brand, ignore.case = TRUE, perl = TRUE)
# amd 
amazon_new2$Graphics_Brand <- gsub(".*radeon.*", "AMD", amazon_new2$Graphics_Brand, ignore.case = TRUE, perl = TRUE)
amazon_new2$Graphics_Brand <- gsub(".*rx.*", "AMD", amazon_new2$Graphics_Brand, ignore.case = TRUE, perl = TRUE)

# get index of usable observations
intel_index <- grep("Intel", amazon_new2$Graphics_Brand)
nv_index <- grep("NVIDIA", amazon_new2$Graphics_Brand)
amd_index <- grep("AMD", amazon_new2$Graphics_Brand)
graphics_index <- c(intel_index, nv_index, amd_index)

# create new dataframe
amazon_new3 <- amazon_new2[graphics_index,]

# change graphics vector to factor
amazon_new3$Graphics_Brand <- as.factor(amazon_new3$Graphics_Brand)

# check data
str(amazon_new3)

#************************* BRAND *************************
unique(amazon_new3$Brand)
# looking at these unique brands, I think I'm just not going
# to use this for analysis--it's kind of convoluted

#************************* ITEM WEIGHT *************************
amazon_new3$Item.Weight
# hmmm...I might also exclude this because there are too many missing fields
# well, it looks like it cuts it down to 222 observations, which isn't terrible
amazon_new3$Item.Weight <- parse_number(amazon_new3$Item.Weight)
amazon_new3$weight_index <- ifelse(!is.na(amazon_new3$Item.Weight), 1, 0)

# create new dataframe
amazon_new4 <- subset(amazon_new3, weight_index == 1)

str(amazon_new4)

#************************* PROCESSOR BRAND *************************
unique(amazon_new4$Processor.Brand)
# change the weird intel to just intel
amazon_new4$Processor.Brand <- gsub("^.*?(Intel).*", "\\1", amazon_new4$Processor.Brand, ignore.case = TRUE, perl = TRUE)

# change vector to factor 
amazon_new4$Processor.Brand <- as.factor(amazon_new4$Processor.Brand)

# drop empty ones
amazon_new4 <- subset(amazon_new4, Processor.Brand != "")
amazon_new4$Processor.Brand <- droplevels(amazon_new4$Processor.Brand)

# check data
str(amazon_new4)

#************************* MEMORY TYPE *************************
unique(amazon_new4$Memory.Type)
# ok, this one I might *actually* not use, because
# memory type will often be related to processor

#************************* HARD DRIVE INTERFACE *************************
unique(amazon_new4$Hard.Drive.Interface)

# start with the hybrids
amazon_new4$Hard.Drive.Interface <- gsub("Serial ATA, Solid State", "Hybrid", amazon_new4$Hard.Drive.Interface)
amazon_new4$Hard.Drive.Interface <- gsub("Solid State, Serial ATA", "Hybrid", amazon_new4$Hard.Drive.Interface)
amazon_new4$Hard.Drive.Interface <- gsub("Solid State, eSATA", "Hybrid", amazon_new4$Hard.Drive.Interface)

amazon_new4$Hard.Drive.Interface <- gsub("^.*?(ATA).*", "\\1", amazon_new4$Hard.Drive.Interface, ignore.case = TRUE, perl = TRUE)

# get index of empty fields
amazon_new4$hd_index <- ifelse(amazon_new4$Hard.Drive.Interface == "", 0, 1)

# create new dataframe
amazon_new5 <- subset(amazon_new4, hd_index == 1)

# change to factor
amazon_new5$Hard.Drive.Interface <- as.factor(amazon_new5$Hard.Drive.Interface)

# check data
str(amazon_new5)

#************************* SELLERS RANK *************************
# remove commas and turn into numeric vector
amazon_new5$Sellers.Rank <- gsub(",", "", amazon_new5$Sellers.Rank)
amazon_new5$Sellers.Rank <- as.numeric(amazon_new5$Sellers.Rank)

# create new dataframe where there are no null Sellers Rank
amazon_new6 <- subset(amazon_new5, !is.na(Sellers.Rank))

#************************* DATE FIRST AVAILABLE *************************
# treat date as numeric variable, where later dates are greater numbers
amazon_new6$Date.First.Available <- as.numeric(parse_date(amazon_new6$Date.First.Available, format = "%d-%b-%y"))

#************************* RATING *************************
# turn into numeric vector
amazon_new6$Rating <- as.numeric(amazon_new6$Rating)

# create new dataframe where there are no null ratings
amazon_new6 <- subset(amazon_new6, !is.na(Rating))

#************************* PRICE *************************
amazon_new6$Price <- gsub(",", "", amazon_new6$Price)
amazon_new6$Price <- as.numeric(amazon_new6$Price)

# create new dataframe where there are no null Sellers Rank
amazon_new7 <- subset(amazon_new6, !is.na(Sellers.Rank))

#************************* ANALYSIS *************************

#************************* RANDOM FOREST *************************
# Will start with random forest analysis of categorical variables

# make new dataframe that only includes categorical variables:
str(amazon_new7)
# Categorical Variables: Processor, RAM.GB, Processor.Brand, Hard.Drive.Interface,
#                        Graphics_Brand

# new dataframe
amazon_rf <- amazon_new7[,c(2,3,8,10,17,14)]

# check data
str(amazon_rf)

# create train and test datasets
# set test size
test_size <- 100

# create test index
test_index <- sample(1:dim(amazon_rf)[1], test_size)

# test dataset
rf_test <- amazon_rf[test_index,]

# train dataset
rf_train <- amazon_rf[-test_index,]

# compare train and test
summary(rf_test)
summary(rf_train)
# rf_train has a $100 greater median price

# have str(amazon_rf) handy to fit model
str(amazon_rf)

# attach randomForest library
library(randomForest)

# fit model
rf_out <- randomForest(x = rf_train[, -6], y = rf_train$Price, xtest = rf_test[, -6],
                         ytest = rf_test$Price, replace = TRUE, keep.forest = TRUE, ntrees = 50, mtry = 5, nodesize = 25)

#to understand what the model is doing:
round(importance(rf_out), 0)

#create importance plot that's a little easier to understand
varImpPlot(rf_out)
# Processor and Ram GB are most relevant: not surprising

#************************* MULTIPLE LINEAR REGRESSION *************************
str(amazon_new7)

# Quantitative variables: Item.Weight, Sellers.Rank, Date.First.Available, Rating, HD

# fit model
amazon_out <- lm(Price ~ Item.Weight + Sellers.Rank + Date.First.Available + Rating + HD, data = amazon_new7)

# look at regression coefficients
summary(amazon_out)

# look at crPlot 
library(car)
crPlots(amazon_out)
# hmmm...looking at these plots, we will want to see
# if there are outliers to eliminate from these variables:
# Sellers.Rank, Date.First.Available, HD

# r-studentized residuals
amazon_rstud <- rstudent(amazon_out)

# density overlay of r-studentized residuals
hist(amazon_rstud, freq = FALSE)
my_z <- seq(-3, 3, length = 50)
lines(my_z, dnorm(my_z, 0, 1), col = 'purple', lty = 1, lwd = 2)

# subset where r-studentized residuals are greater than 2
subset(amazon_new7, amazon_rstud > 2)

subset(amazon_new7, Processor == "i9")

# will later eliminate all of the outliers identified 
# except the i9 processors because they will inevitably
# be price outliers

subset(amazon_new7, amazon_rstud > 2 & Processor != "i9")

# test for normality
shapiro.test(amazon_rstud)
# data are *not* normal

# Regression Data Diagnostics

# LEVERAGE
amazon_leverage <- lm.influence(amazon_out)$hat

# Leverage *rule of thumb*: 2*(p+1)/n
# n=num rows, p+1=num parameters
subset(amazon_new7, amazon_leverage > 2 * (length(amazon_out$coefficients) - 1)  / dim(amazon_new7)[1])
# no extreme values by leverage measurement

# COOK'S DISTANCE
amazon_cookdistance <- cooks.distance(amazon_out)

#Cook's distance *rule of thumb*: 4/(n-(p+1))
#n = num rows, p+1=num parameters
subset(amazon_new7, amazon_cookdistance > 4 / (dim(amazon_new7)[1] - (length(amazon_out$coefficients) - 1)))

# nothing flagged by leverage or cook's distance rules of thumb

# remove outliers flagged by r-studentized residuals except i9's
rstud_removeindex <- as.numeric(rownames(subset(amazon_new7, amazon_rstud > 2 & Processor != "i9")))
amazon_new8 <- amazon_new7[-rstud_removeindex,]

# re-fit model now that observations have been removed

# NEW MODEL

amazon_out2 <- lm(Price ~ Item.Weight + Sellers.Rank + Date.First.Available + Rating + HD, data = amazon_new8)
summary(amazon_out2)
# Item Weight, Hard Drive, and Seller's rank are all significant
# Date First Available, and # of stars are not
