library(tidyverse)
library(lubridate)
library(ggplot2)
library(stringr)

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

# split dataset by survey

# check the levels of the scheme variable and clean
levels(data$Scheme)

#change ECAT, to ECAT
data$Scheme <- gsub("ECAT,", "ECAT", data$Scheme)

#clean assay names in df
data$Assay <- gsub("ADAMTS", "ADAMTS13 activity", data$Assay)
data$Assay <- gsub("ADAMTS13 activity-13 Inhibitor", "ADAMTS13 Inhibitor", data$Assay)
data$Assay <- gsub("ADAMTS13 Activity13 Inhibitor", "ADAMTS13 Inhibitor", data$Assay)
data$Assay <- gsub("ADAMTS13 Activity-13 Activity", "ADAMTS13 Activity", data$Assay)
data$Assay <- gsub("Anti-XA", "Anti-Xa", data$Assay)
data$Assay <- gsub("Heparin Anti-Xa", "Anti-Xa", data$Assay)
data$Assay <- gsub("APTTD", "APTT DADE", data$Assay)
data$Assay <- gsub("APTTSP", "APTT SP", data$Assay)
data$Assay <- gsub("Clauss Fibrinogen", "Fibrinogen", data$Assay)
data$Assay <- gsub("Dade Actin FS", "APTT DADE", data$Assay)
data$Assay <- gsub("EXTAM A10", "EXTEM A10", data$Assay)
data$Assay <- gsub("EXTEM A10 mm", "EXTEM A10", data$Assay)
data$Assay <- gsub("EXTEM A15 mm", "EXTEM A15", data$Assay)
data$Assay <- gsub("EXTEM A20 mm", "EXTEM A20", data$Assay)
data$Assay <- gsub("EXTEM A30 mm", "EXTEM A30", data$Assay)
data$Assay <- gsub("EXTEM angle", "EXTEM alpha", data$Assay)
data$Assay <- gsub("FVIII IA", "Factor VIII Inhibitor Assay", data$Assay)
data$Assay <- gsub("Factor VIII IA", "Factor VIII Inhibitor Assay", data$Assay)
data$Assay <- gsub("fibrinogen Antigen", "Fibrinogen Antigen", data$Assay)
data$Assay <- gsub("Protein S", "Free Protein S", data$Assay)
data$Assay <- gsub("Protein C Activity Activity", "Protein C Activity", data$Assay)
data$Assay <- gsub("HIT-IgG", "HIT IgG", data$Assay)
data$Assay <- gsub("Free Free Protein S", "Free Protein S", data$Assay)
data$Assay <- gsub("FVIII", "Factor VIII", data$Assay)
data$Assay <- gsub("Factor XIII Ag", "Factor XIII Antigen", data$Assay)
data$Assay <- gsub("FXIII AG", "Factor XIII Antigen", data$Assay)
data$Assay <- gsub("Heparin ratio", "Heparin Ratio", data$Assay)
data$Assay <- gsub("APTT - Heparin Dosage", "Heparin Ratio", data$Assay)
data$Assay <- gsub("ILVWF Ag", "vWF Antigen", data$Assay)
data$Assay <- gsub("ILVWF", "vWF Antigen", data$Assay)
data$Assay <- gsub("VWF Ag", "vWF Antigen", data$Assay)
data$Assay <- gsub("VWF Antigen", "vWF Antigen", data$Assay)
data$Assay <- gsub("VWACT", "vWF Activity", data$Assay)
data$Assay <- gsub("VWF ACT", "vWF Activity", data$Assay)
data$Assay <- gsub("VWF Activity", "vWF Activity", data$Assay)

data$Assay <- as.factor(data$Assay)
levels(data$Assay)


df.cqas <- filter(data, Scheme == "CQAS") %>%
  droplevels
dim(df.cqas)
df.neqas <- filter(data, Scheme =="NEQAS") %>% droplevels 
df.ecat <- filter(data, Scheme == "ECAT") %>% droplevels

#run missing map to see if there are variables needing that we dont need that are causing a problem

missmap(df.cqas, main = "CQAS",  col = c("yellow", "black"), legend = FALSE)
missmap(df.neqas, main = "NEQAS",  col = c("yellow", "black"), legend = FALSE)
missmap(df.ecat, main = "ECAT",  col = c("yellow", "black"), legend = FALSE)

# clean each df 

#cqas - not using as the required data is not present
summary(df.cqas)
df.cqas
df.cqas <- select(df.cqas, Date, Assay, Result)
levels(df.cqas$Assay)

#neqas clean
summary(df.neqas)
df.neqas <- select(df.neqas, Date, Assay, Result, Median_Reagent, Median_Overall, IQ_Result)

df.neqas %>% arrange(Date)
any(is.na(df.neqas))
sum(complete.cases(df.neqas))


#grade performance for all assays that have grades assigned
df.neqas.IQ <- filter(df.neqas, IQ_Result != "NA") %>% droplevels
summary(df.neqas.IQ)


#plot the graph of the grades for NEQAS
ggplot(data=df.neqas.IQ, aes(x=IQ_Result)) + geom_bar()
ggplot(data=df.neqas.IQ, aes(x=IQ_Result, fill=Assay)) + geom_bar()

summary(df.neqas.IQ)



# ecat data
summary(df.ecat)
df.ecat <- select(df.ecat, Date, Assay, Result, Median_Reagent, Z_Score)

Z_Score.no.na <- filter(df.ecat, Z_Score != "NA") %>% droplevels
summary(Z_Score.no.na)

ggplot(data=Z_Score.no.na, aes(x=Z_Score, fill=Assay)) + geom_histogram(bins=50)

# Z_Score for each assay against time i.e. by survey
ggplot(data=Z_Score.no.na, aes(x=Date, y=Z_Score, color=Assay, size=Result)) + geom_point()

summary(df.ecat)

df.tg.ecat <- filter(df.ecat, str_detect(Assay, "TG")) %>% droplevels
df.tg.ecat <- filter(df.tg.ecat, Result < 4000) %>% droplevels

df.tg.ecat <- df.tg.ecat %>% select(Assay, Result, Median_Reagent)

head(df.tg.ecat)
summary(df.tg.ecat)

df.diff.results <- df.tg.ecat %>%
  mutate(Difference = ((Result - Median_Reagent)/Median_Reagent)*100)
head(df.diff.results)

# relationship between result and group median by assay
ggplot(data=df.diff.results, aes(x=Result, y=Median_Reagent)) +
  geom_point() +
  facet_wrap(~Assay, scales="free")










