library(tidyverse)
library(lubridate)

data <- EQA_170517
head(data)
str(data)
dim(data)
names(data)

#select the columns i want to work with and assign it to data
data <- data %>% select(Test, Scheme, `Survey No`, `Combined Date`, `Barcode`, `Result`, `Results 2`, `Group Participants`, `Reagent Median`, `Z-Score`, `Overall Participants`, `Overall Median`,`Z-Score_1`, `IQ range`, `IQ result`, Range)

colnames(data) <- c("Assay", "Scheme", "Survey", "Date", "Barcode", "Result", "Result2", "Participants_Group", "Median_Reagent", "Z_Score", "Participants_Overall", "Median_Overall", "Z_Score2", "IQ_Range", "IQ_Result", "Range")

glimpse(data)

#change the data types of each variable
data$Assay <- as.factor(data$Assay)
data$Scheme <- as.factor(data$Scheme)
data$Survey <- as.factor(data$Survey)
data$Date <- dmy(data$Date)
data$Barcode <- as.factor(data$Barcode)
data$Result <- as.numeric(data$Result)
data$Result2 <- as.numeric(data$Result2)
data$Participants_Group <- as.numeric(data$Participants_Group)
data$Median_Reagent <- as.numeric(data$Median_Reagent)
data$Z_Score <- as.numeric(data$Z_Score)
data$Result <- as.numeric(data$Result)
data$Participants_Overall <- as.numeric(data$Participants_Overall)
data$Median_Overall <- as.numeric(data$Median_Overall)
data$Z_Score2 <- as.numeric(data$Z_Score2)
data$IQ_Result <- as.factor(data$IQ_Result)

#check what levels are present in the factor variables so they can be recoded
levels(data$Assay)
levels(data$Scheme)
levels(data$Survey)
levels(data$Barcode)
levels(data$IQ_Result)

summary(data)

data <- data[1:873,]

ggplot(data=data, aes(x=IQ_Result))  + geom_histogram(stat="count")

#do a missing map to see what NA values are present in the dataset
library(Amelia)
missmap(data, main = "Missing map", col = c("yellow", "black"), legend = FALSE)


