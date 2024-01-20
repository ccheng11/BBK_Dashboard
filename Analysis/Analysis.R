rm(list=ls())
options(scipen=9999)

library(stringr)
library(stringi)
library(readxl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(gtools)
library(modelsummary)
library(foreign)
library(tidyverse)

##### Set working directory #####
wd <- dirname(rstudioapi::getActiveDocumentContext()$path); wd
setwd(wd)

##### Load data #####
dta <- read_xlsx("SSS Research Methods Survey Clean.xlsx", 1)
colnames(dta) <- tolower(colnames(dta))

dta <- dta %>% drop_na(subject) # drop no identified subject area

table(duplicated(dta$name))
dta$name[duplicated(dta$name) == T]

##### Methods #####
subject_unique <- c("Qualitative", "Quantitative", "Computational")
for(i in 1:length(subject_unique)){
  dta$temp <- NA
  dta$temp[grep(subject_unique[i], dta$methods)] <- "Yes"
  colnames(dta)[colnames(dta) == "temp"] <- subject_unique[i]
}
rm(i)

colnames(dta) <- tolower(colnames(dta))

table(dta$qualitative)
table(dta$quantitative)
table(dta$computational)

dta$quali_quant <- ifelse(dta$qualitative == "Yes" & dta$quantitative == "Yes", 1, 0)
dta$quali_com <- ifelse(dta$qualitative == "Yes" & dta$computational == "Yes", 1, 0)
dta$quant_com <- ifelse(dta$quantitative == "Yes" & dta$computational == "Yes", 1, 0)
dta$method_all <- ifelse(dta$qualitative == "Yes" & dta$quantitative == "Yes" & dta$computational == "Yes", 1, 0)

table(dta$quali_quant)
table(dta$quali_com)
table(dta$quant_com)
table(dta$method_all)

##### Software #####
software_all <- list()
for(i in 1:nrow(dta)){
  software_temp <- unlist(str_split(dta$software[i], ";|,"))
  software_temp <- str_trim(software_temp)
  software_all[[i]] <- software_temp[software_temp != ""]
  rm(software_temp)
}
software_all <- na.omit(unlist(software_all))
rm(i)

software_count <- data.frame(software = sort(unique(software_all))[sort(unique(software_all)) != "C++"],
                             freq = NA)
for(i in 1:nrow(software_count)){
  software_count$freq[i] <- length(grep(software_count$software[i], software_all))
}
rm(i)

software_count <- rbind(software_count, c("C++", 1))
software_count$freq <- as.numeric(software_count$freq)
software_count <- software_count[order(software_count$freq, decreasing=T),]
rownames(software_count) <- NULL

library(openxlsx)
write.xlsx(software_count, "Analysis/software_count.xlsx")

##### Subject areas #####
subject_all <- list()
for(i in 1:nrow(dta)){
  subject_temp <- unlist(str_split(dta$subject[i], ";|,"))
  subject_temp <- str_trim(subject_temp)
  subject_all[[i]] <- subject_temp[subject_temp != ""]
  rm(subject_temp)
}
subject_all <- unlist(subject_all)
rm(i)

subject_unique <- sort(unique(subject_all))
for(i in 1:length(subject_unique)){
  dta$temp <- NA
  dta$temp[grep(subject_unique[i], dta$subject)] <- "Yes"
  colnames(dta)[colnames(dta) == "temp"] <- subject_unique[i]
}
rm(i)

colnames(dta)[colnames(dta) == "Gender & Sexuality studies"] = "gender_sexuality"
colnames(dta)[colnames(dta) == "International Development"] = "ind_dev"
colnames(dta)[colnames(dta) == "Socio-legal studies"] = "socio_legal"
colnames(dta)[colnames(dta) == "Urban Analytics"] = "urban_analytics"
colnames(dta) <- tolower(colnames(dta))

table(dta$politics)
table(dta$geography)
table(dta$psychosocial)
table(dta$criminology)
