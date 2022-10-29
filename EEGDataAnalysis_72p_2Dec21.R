EEGData <- read.csÅv("R_Data_Demo_EEG_72p.csv")

library(lmerTest)
library(tidyverse)
library(dplyr)
library(psych)
library(plyr) # packageVersion("plyr") 1.8.4
library(lmSupport)
# Change WD
setwd("/Users/joshuaupshaw/Documents/A_Projects/First Year Project/Data/R project")

# Remove blank rows 64-79
# EEGData <- EEGData[-c(64:79), ]
# Remove column 31 "Part_ID2"
EEGData <- within(EEGData, rm(Part_ID2))
EEGData <- within(EEGData, rm(X, X.1, X.2, X.3))
# Also works: EEGData <- EEGData[ -31 ]
# Remove problematic data point for Participant TECH4008 at P3_ComSq_Cz
# This data point caused the P3 for ComSq at Cz to be significantly different, 
# potentially skewing results
# EEGData[9, 43] <- NA
####### descriptives for conditions and channel sites (incomplete) #####
MeanP2AmpforConditions <- ddply(EEGData_NoMow_NAomit, .(SAPS_Med_Split), summarise,
                        M.Com_Ph_Cz = mean(P2_ComPh_Cz),
                        M.Com_Ph_Fz = mean(P2_ComPh_Fz),
                        M.Com_Ph_Pz = mean(P2_ComPh_Pz),
                        M.Rare_Ph_Cz = mean(P2_RarePh_Cz),
                        M.Rare_Ph_Fz = mean(P2_RarePh_Fz),
                        M.Rare_Ph_Pz = mean(P2_RarePh_Pz),
                        M.Com_Sq_Cz = mean(P2_ComSq_Cz),
                        M.Com_Sq_Fz = mean(P2_ComSq_Fz),
                        M.Com_Sq_Pz = mean(P2_ComSq_Pz),
                        M.Rare_Sq_Cz = mean(P2_RareSq_Cz),
                        M.Rare_Sq_Fz = mean(P2_RareSq_Fz),
                        M.Rare_Sq_Pz = mean(P2_RareSq_Pz),#
                        SD.Com_Ph_Cz = sd(P2_ComPh_Cz),
                        SD.Com_Ph_Fz = sd(P2_ComPh_Fz),
                        SD.Com_Ph_Pz = sd(P2_ComPh_Pz),
                        SD.Rare_Ph_Cz = sd(P2_RarePh_Cz),
                        SD.Rare_Ph_Fz = sd(P2_RarePh_Fz),
                        SD.Rare_Ph_Pz = sd(P2_RarePh_Pz),
                        SD.Com_Sq_Cz = sd(P2_ComSq_Cz),
                        SD.Com_Sq_Fz = sd(P2_ComSq_Fz),
                        SD.Com_Sq_Pz = sd(P2_ComSq_Pz),
                        SD.Rare_Sq_Cz = sd(P2_RareSq_Cz),
                        SD.Rare_Sq_Fz = sd(P2_RareSq_Fz),
                        SD.Rare_Sq_Pz = sd(P2_RareSq_Pz))

####### Creating new variables from original six conditions #######
# Main Oddballs #####
EEGData <- mutate(EEGData, phoddball_N2_Cz = N2_RarePh_Cz - N2_ComPh_Cz, 
                  phoddball_N2_Fz = N2_RarePh_Fz - N2_ComPh_Fz,
                  phoddball_N2_Pz = N2_RarePh_Pz - N2_ComPh_Pz,
                  sqoddball_N2_Cz = N2_RareSq_Cz - N2_ComSq_Cz,
                  sqoddball_N2_Fz = N2_RareSq_Fz - N2_ComSq_Fz,
                  sqoddball_N2_Pz = N2_RareSq_Pz - N2_ComSq_Pz,
                  mowoddball_N2_Cz = N2_RareMow_Cz - N2_ComMow_Cz,
                  mowoddball_N2_Fz = N2_RareMow_Fz - N2_ComMow_Fz,
                  mowoddball_N2_Pz = N2_RareMow_Pz - N2_ComMow_Pz,
                  phoddball_P2_Cz = P2_RarePh_Cz - P2_ComPh_Cz, 
                  phoddball_P2_Fz = P2_RarePh_Fz - P2_ComPh_Fz,
                  phoddball_P2_Pz = P2_RarePh_Pz - P2_ComPh_Pz,
                  sqoddball_P2_Cz = P2_RareSq_Cz - P2_ComSq_Cz,
                  sqoddball_P2_Fz = P2_RareSq_Fz - P2_ComSq_Fz,
                  sqoddball_P2_Pz = P2_RareSq_Pz - P2_ComSq_Pz,
                  mowoddball_P2_Cz = P2_RareMow_Cz - P2_ComMow_Cz,
                  mowoddball_P2_Fz = P2_RareMow_Fz - P2_ComMow_Fz,
                  mowoddball_P2_Pz = P2_RareMow_Pz - P2_ComMow_Pz,
                  phoddball_P3_Cz = P3_RarePh_Cz - P3_ComPh_Cz, 
                  phoddball_P3_Fz = P3_RarePh_Fz - P3_ComPh_Fz,
                  phoddball_P3_Pz = P3_RarePh_Pz - P3_ComPh_Pz,
                  sqoddball_P3_Cz = P3_RareSq_Cz - P3_ComSq_Cz,
                  sqoddball_P3_Fz = P3_RareSq_Fz - P3_ComSq_Fz,
                  sqoddball_P3_Pz = P3_RareSq_Pz - P3_ComSq_Pz,
                  mowoddball_P3_Cz = P3_RareMow_Cz - P3_ComMow_Cz,
                  mowoddball_P3_Fz = P3_RareMow_Fz - P3_ComMow_Fz,
                  mowoddball_P3_Pz = P3_RareMow_Pz - P3_ComMow_Pz,
                  phoddball_N2_FzCz = N2_RarePh_FzCz - N2_ComPh_FzCz, 
                  sqoddball_N2_FzCz = N2_RareSq_FzCz - N2_ComSq_FzCz,
                  mowoddball_N2_FzCz = N2_RareMow_FzCz - N2_ComMow_FzCz,
                  phoddball_N2_F3 = N2_RarePh_F3 - N2_ComPh_F3,
                  sqoddball_N2_F3 = N2_RareSq_F3 - N2_ComSq_F3)
# Total oddballs and averages for N2 #####      
EEGData <- mutate(EEGData, ComAve_PhSqMow_N2_Cz = (N2_ComPh_Cz + N2_ComSq_Cz + N2_ComMow_Cz)/3,
                  ComAve_PhSqMow_N2_Fz = (N2_ComPh_Fz + N2_ComSq_Fz + N2_ComMow_Fz)/3,
                  ComAve_PhSqMow_N2_Pz = (N2_ComPh_Pz + N2_ComSq_Pz + N2_ComMow_Pz)/3,
                  ComAve_PhSqMow_N2_FzCz = (ComAve_PhSqMow_N2_Fz + ComAve_PhSqMow_N2_Cz)/2,
                  RareAve_PhSqMow_N2_Cz = (N2_RarePh_Cz + N2_RareSq_Cz + N2_RareMow_Cz)/3,
                  RareAve_PhSqMow_N2_Fz = (N2_RarePh_Fz + N2_RareSq_Fz + N2_RareMow_Fz)/3,
                  RareAve_PhSqMow_N2_Pz = (N2_RarePh_Pz + N2_RareSq_Pz + N2_RareMow_Pz)/3,
                  RareAve_PhSqMow_N2_FzCz = (RareAve_PhSqMow_N2_Fz + RareAve_PhSqMow_N2_Cz)/2,
                  ComAve_PhSq_N2_Cz = (N2_ComPh_Cz + N2_ComSq_Cz)/2,
                  ComAve_PhSq_N2_Fz = (N2_ComPh_Fz + N2_ComSq_Fz)/2,
                  ComAve_PhSq_N2_F3 = (N2_ComPh_F3 + N2_ComSq_F3)/2,
                  ComAve_PhSq_N2_Pz = (N2_ComPh_Pz + N2_ComSq_Pz)/2,
                  ComAve_PhSq_N2_FzCz = (ComAve_PhSq_N2_Fz + ComAve_PhSq_N2_Cz)/2,
                  RareAve_PhSq_N2_Cz = (N2_RarePh_Cz + N2_RareSq_Cz)/2,
                  RareAve_PhSq_N2_Fz = (N2_RarePh_Fz + N2_RareSq_Fz)/2,
                  RareAve_PhSq_N2_F3 = (N2_RarePh_F3 + N2_RareSq_F3)/2,
                  RareAve_PhSq_N2_Pz = (N2_RarePh_Pz + N2_RareSq_Pz)/2,
                  RareAve_PhSq_N2_FzCz = (RareAve_PhSq_N2_Fz + RareAve_PhSq_N2_Cz)/2,
                  Total_Oddball_PhSqMow_N2_Cz = RareAve_PhSqMow_N2_Cz - ComAve_PhSqMow_N2_Cz,
                  Total_Oddball_PhSqMow_N2_Fz = RareAve_PhSqMow_N2_Fz - ComAve_PhSqMow_N2_Fz,
                  Total_Oddball_PhSqMow_N2_Pz = RareAve_PhSqMow_N2_Pz - ComAve_PhSqMow_N2_Pz,
                  Total_Oddball_PhSqMow_N2_FzCz = RareAve_PhSqMow_N2_FzCz - ComAve_PhSqMow_N2_FzCz,
                  Total_Oddball_PhSq_N2_Cz = RareAve_PhSq_N2_Cz - ComAve_PhSq_N2_Cz,
                  Total_Oddball_PhSq_N2_Fz = RareAve_PhSq_N2_Fz - ComAve_PhSq_N2_Fz,
                  Total_Oddball_PhSq_N2_Pz = RareAve_PhSq_N2_Pz - ComAve_PhSq_N2_Pz,
                  Total_Oddball_PhSq_N2_FzCz = RareAve_PhSq_N2_FzCz - ComAve_PhSq_N2_FzCz,
                  Total_Oddball_PhSq_N2_F3 = RareAve_PhSq_N2_F3 - ComAve_PhSq_N2_F3)

# Total oddballs and averages for P2 #####     
EEGData <- mutate(EEGData, ComAve_PhSqMow_P2_Cz = (P2_ComPh_Cz + P2_ComSq_Cz + P2_ComMow_Cz)/3,
                  ComAve_PhSqMow_P2_Fz = (P2_ComPh_Fz + P2_ComSq_Fz + P2_ComMow_Fz)/3,
                  ComAve_PhSqMow_P2_Pz = (P2_ComPh_Pz + P2_ComSq_Pz + P2_ComMow_Pz)/3,
                  RareAve_PhSqMow_P2_Cz = (P2_RarePh_Cz + P2_RareSq_Cz + P2_RareMow_Cz)/3,
                  RareAve_PhSqMow_P2_Fz = (P2_RarePh_Fz + P2_RareSq_Fz + P2_RareMow_Fz)/3,
                  RareAve_PhSqMow_P2_Pz = (P2_RarePh_Pz + P2_RareSq_Pz + P2_RareMow_Pz)/3,
                  ComAve_PhSq_P2_Cz = (P2_ComPh_Cz + P2_ComSq_Cz)/2,
                  ComAve_PhSq_P2_Fz = (P2_ComPh_Fz + P2_ComSq_Fz)/2,
                  ComAve_PhSq_P2_Pz = (P2_ComPh_Pz + P2_ComSq_Pz)/2,
                  RareAve_PhSq_P2_Cz = (P2_RarePh_Cz + P2_RareSq_Cz)/2,
                  RareAve_PhSq_P2_Fz = (P2_RarePh_Fz + P2_RareSq_Fz)/2,
                  RareAve_PhSq_P2_Pz = (P2_RarePh_Pz + P2_RareSq_Pz)/2,
                  Total_Oddball_PhSqMow_P2_Cz = RareAve_PhSqMow_P2_Cz - ComAve_PhSqMow_P2_Cz,
                  Total_Oddball_PhSqMow_P2_Fz = RareAve_PhSqMow_P2_Fz - ComAve_PhSqMow_P2_Fz,
                  Total_Oddball_PhSqMow_P2_Pz = RareAve_PhSqMow_P2_Pz - ComAve_PhSqMow_P2_Pz,
                  Total_Oddball_PhSq_P2_Cz = RareAve_PhSq_P2_Cz - ComAve_PhSq_P2_Cz,
                  Total_Oddball_PhSq_P2_Fz = RareAve_PhSq_P2_Fz - ComAve_PhSq_P2_Fz,
                  Total_Oddball_PhSq_P2_Pz = RareAve_PhSq_P2_Pz - ComAve_PhSq_P2_Pz)
   
# Total oddballs and averages for P3 ####      
EEGData <- mutate(EEGData, ComAve_PhSqMow_P3_Cz = (P3_ComPh_Cz + P3_ComSq_Cz + P3_ComMow_Cz)/3,
                  ComAve_PhSqMow_P3_Fz = (P3_ComPh_Fz + P3_ComSq_Fz + P3_ComMow_Fz)/3,
                  ComAve_PhSqMow_P3_Pz = (P3_ComPh_Pz + P3_ComSq_Pz + P3_ComMow_Pz)/3,
                  RareAve_PhSqMow_P3_Cz = (P3_RarePh_Cz + P3_RareSq_Cz + P3_RareMow_Cz)/3,
                  RareAve_PhSqMow_P3_Fz = (P3_RarePh_Fz + P3_RareSq_Fz + P3_RareMow_Fz)/3,
                  RareAve_PhSqMow_P3_Pz = (P3_RarePh_Pz + P3_RareSq_Pz + P3_RareMow_Pz)/3,
                  ComAve_PhSq_P3_Cz = (P3_ComPh_Cz + P3_ComSq_Cz)/2,
                  ComAve_PhSq_P3_Fz = (P3_ComPh_Fz + P3_ComSq_Fz)/2,
                  ComAve_PhSq_P3_Pz = (P3_ComPh_Pz + P3_ComSq_Pz)/2,
                  RareAve_PhSq_P3_Cz = (P3_RarePh_Cz + P3_RareSq_Cz)/2,
                  RareAve_PhSq_P3_Fz = (P3_RarePh_Fz + P3_RareSq_Fz)/2,
                  RareAve_PhSq_P3_Pz = (P3_RarePh_Pz + P3_RareSq_Pz)/2,
                  Total_Oddball_PhSqMow_P3_Cz = RareAve_PhSqMow_P3_Cz - ComAve_PhSqMow_P3_Cz,
                  Total_Oddball_PhSqMow_P3_Fz = RareAve_PhSqMow_P3_Fz - ComAve_PhSqMow_P3_Fz,
                  Total_Oddball_PhSqMow_P3_Pz = RareAve_PhSqMow_P3_Pz - ComAve_PhSqMow_P3_Pz,
                  Total_Oddball_PhSq_P3_Cz = RareAve_PhSq_P3_Cz - ComAve_PhSq_P3_Cz,
                  Total_Oddball_PhSq_P3_Fz = RareAve_PhSq_P3_Fz - ComAve_PhSq_P3_Fz,
                  Total_Oddball_PhSq_P3_Pz = RareAve_PhSq_P3_Pz - ComAve_PhSq_P3_Pz)               
#str(EEGData)
#EEGData <- within(EEGData, rm(na.rm))
# coerce all ERPs to numeric
# EEGData[c(31:180), ] <- as.numeric(EEGData[c(31:180), ])
# Save and wirte dataset to csv
# write.csv(EEGData, "EEGData_72p.csv")
# Basic oddball analyses for each channel site
# Sound averages ####
EEGData <- mutate(EEGData, ComRareAve_Ph_P3_Cz = (P3_ComPh_Cz + P3_RarePh_Cz)/2,
                  ComRareAve_Ph_P3_Fz = (P3_ComPh_Fz + P3_RarePh_Fz)/2,
                  ComRareAve_Ph_P3_Pz = (P3_ComPh_Pz + P3_RarePh_Pz)/2,
                  ComRareAve_Ph_N2_Cz = (N2_ComPh_Cz + N2_RarePh_Cz)/2,
                  ComRareAve_Ph_N2_Fz = (N2_ComPh_Fz + N2_RarePh_Fz)/2,
                  ComRareAve_Ph_N2_F3 = (N2_ComPh_F3 + N2_RarePh_F3)/2,
                  ComRareAve_Ph_N2_Pz = (N2_ComPh_Pz + N2_RarePh_Pz)/2,
                  ComRareAve_Ph_N2_FzCz = (N2_ComPh_FzCz + N2_RarePh_FzCz)/2,
                  ComRareAve_Ph_P2_Cz = (P2_ComPh_Cz + P2_RarePh_Cz)/2,
                  ComRareAve_Ph_P2_Fz = (P2_ComPh_Fz + P2_RarePh_Fz)/2,
                  ComRareAve_Ph_P2_Pz = (P2_ComPh_Pz + P2_RarePh_Pz)/2,
                  ComRareAve_Sq_P3_Cz = (P3_ComSq_Cz + P3_RareSq_Cz)/2,
                  ComRareAve_Sq_P3_Fz = (P3_ComSq_Fz + P3_RareSq_Fz)/2,
                  ComRareAve_Sq_P3_Pz = (P3_ComSq_Pz + P3_RareSq_Pz)/2,
                  ComRareAve_Sq_N2_Cz = (N2_ComSq_Cz + N2_RareSq_Cz)/2,
                  ComRareAve_Sq_N2_Fz = (N2_ComSq_Fz + N2_RareSq_Fz)/2,
                  ComRareAve_Sq_N2_F3 = (N2_ComSq_F3 + N2_RareSq_F3)/2,
                  ComRareAve_Sq_N2_Pz = (N2_ComSq_Pz + N2_RareSq_Pz)/2,
                  ComRareAve_Sq_N2_FzCz = (N2_ComSq_FzCz + N2_RareSq_FzCz)/2,
                  ComRareAve_Sq_P2_Cz = (P2_ComSq_Cz + P2_RareSq_Cz)/2,
                  ComRareAve_Sq_P2_Fz = (P2_ComSq_Fz + P2_RareSq_Fz)/2,
                  ComRareAve_Sq_P2_Pz = (P2_ComSq_Pz + P2_RareSq_Pz)/2)

# N2/P2/P3 total averages for Phone/Control sounds####
EEGData <- mutate(EEGData,
                  N2_Cz_PhConAve = (ComAve_PhSq_N2_Cz + RareAve_PhSq_N2_Cz)/2,
                  N2_Fz_PhConAve = (ComAve_PhSq_N2_Fz + RareAve_PhSq_N2_Fz)/2,
                  N2_F3_PhConAve = (ComAve_PhSq_N2_F3 + RareAve_PhSq_N2_F3)/2,
                  N2_Pz_PhConAve = (ComAve_PhSq_N2_Pz + RareAve_PhSq_N2_Pz)/2,
                  N2_FzCz_PhConAve = (ComAve_PhSq_N2_FzCz + RareAve_PhSq_N2_FzCz)/2,
                  P2_Cz_PhConAve = (ComAve_PhSq_P2_Cz + RareAve_PhSq_P2_Cz)/2,
                  P2_Fz_PhConAve = (ComAve_PhSq_P2_Fz + RareAve_PhSq_P2_Fz)/2,
                  P2_Pz_PhConAve = (ComAve_PhSq_P2_Pz + RareAve_PhSq_P2_Pz)/2,
                  P3_Cz_PhConAve = (ComAve_PhSq_P3_Cz + RareAve_PhSq_P3_Cz)/2,
                  P3_Fz_PhConAve = (ComAve_PhSq_P3_Fz + RareAve_PhSq_P3_Fz)/2,
                  P3_Pz_PhConAve = (ComAve_PhSq_P3_Pz + RareAve_PhSq_P3_Pz)/2)
#write.csv(EEGData, "EEGData_72p.csv")
#####read in data####
EEGData <- read.csv("EEGData_72p.csv")
# Adjusting df 
# Remove all variables with mower data
EEGData_NoMow <- select(EEGData, -contains("Mow"))
#write.csv(EEGData_NoMow, "EEGData_NoMow_72p_F3.csv")
#EEGData_NoMow <- read.csv("EEGData_NoMow_72p_F3.csv")
EEGData_NoMow_NAomit <- na.omit(EEGData_NoMow) # 
EEGData_NoMow_NAomit$SAPS.c <- EEGData_NoMow_NAomit$SAPS_Ave - mean(EEGData_NoMow_NAomit$SAPS_Ave)
#write.csv(EEGData_NoMow_NAomit, "EEGData_NoMow_NAomit_72p_F3.csv")
EEGData_NoMow_NAomit <- read.csv("EEGData_NoMow_NAomit_72p_F3.csv")

# demos for EEG Data ####
#unique()
describe(EEGData_NoMow_NAomit$Demo.Age) 
describe(EEGData_NoMow_NAomit$Coded.Race..0...White..1...Non.white.) 
describe(EEGData_NoMow_NAomit$Coded.Sex..0...male..1...female.) 
describe(EEGData_NoMow_NAomit$SAPS_MedSplit_Coded) 
aggregate(Part_ID ~ SAPS_Med_Split, data=EEGData_NoMow_NAomit,FUN=function(x){length(unique(x))})

EEGData_NoMow_NAomit %>% 
  select(SAPS_Ave, SAPS_MedSplit_Coded) %>% 
  MVN::mvn(univariatePlot = "histogram", univariateTest = "SW")

dev.off()
# Descriptives overall phone+control sound Rare and frequent trials ####
mean(EEGData_NoMow_NAomit$ComAve_PhSq_N2_FzCz)
sd(EEGData_NoMow_NAomit$ComAve_PhSq_N2_FzCz)
mean(EEGData_NoMow_NAomit$RareAve_PhSq_N2_FzCz)
sd(EEGData_NoMow_NAomit$RareAve_PhSq_N2_FzCz)
mean(EEGData_NoMow_NAomit$ComAve_PhSq_N2_F3)
sd(EEGData_NoMow_NAomit$ComAve_PhSq_N2_F3)
mean(EEGData_NoMow_NAomit$RareAve_PhSq_N2_F3)
sd(EEGData_NoMow_NAomit$RareAve_PhSq_N2_F3)

##### t-tests for rare vs frequent#####
# P2 all sounds #####
# # Oddball for all sounds at Cz latency will be 150-210 ms
# t.test(EEGData_NoMow_NAomit$RareAve_PhSqMow_P2_Cz, EEGData_NoMow_NAomit$ComAve_PhSqMow_P2_Cz, na.rm = T, paired = T)
# # t = 2.6269, df = 46, p-value = 0.01167 ### Significant
# # Oddball for all sounds at Fz latency will be 150-210 ms
# t.test(EEGData_NoMow_NAomit$RareAve_PhSqMow_P2_Fz, EEGData_NoMow_NAomit$ComAve_PhSqMow_P2_Fz, na.rm = T, paired = T) 
# # t = 2.419, df = 46, p-value = 0.01958 ### Significant
# # Oddball for all sounds at Pz latency will be 110-180 ms
# t.test(EEGData_NoMow_NAomit$RareAve_PhSqMow_P2_Pz, EEGData_NoMow_NAomit$ComAve_PhSqMow_P2_Pz, na.rm = T, paired = T) 
# # t = 1.2307, df = 46, p-value = 0.2247

# P2 phone and square sounds #####
# P2 Oddball for only phone and square sounds at Cz latency will be 150-210  
t.test(EEGData_NoMow_NAomit$RareAve_PhSq_P2_Cz, EEGData_NoMow_NAomit$ComAve_PhSq_P2_Cz, na.rm = T, paired = T)
# 0.124
# Oddball for only phone and square at Fz latency will be 150-210 
t.test(EEGData_NoMow_NAomit$RareAve_PhSq_P2_Fz, EEGData_NoMow_NAomit$ComAve_PhSq_P2_Fz, na.rm = T, paired = T) 
# 0.08745
mean(EEGData_NoMow_NAomit$RareAve_PhSq_P2_Fz)
mean(EEGData_NoMow_NAomit$ComAve_PhSq_P2_Fz)
sd(EEGData_NoMow_NAomit$RareAve_PhSq_P2_Fz)
sd(EEGData_NoMow_NAomit$ComAve_PhSq_P2_Fz)
# Oddball for only phone and square at Pz latency will be 110-180 
t.test(EEGData_NoMow_NAomit$RareAve_PhSq_P2_Pz, EEGData_NoMow_NAomit$ComAve_PhSq_P2_Pz, na.rm = T, paired = T) 
# 

# N2 all sounds #####
# # Oddball for all sounds at Cz latency window 220-380 ms
# t.test(EEGData_NoMow_NAomit$RareAve_PhSqMow_N2_Cz, EEGData_NoMow_NAomit$ComAve_PhSqMow_N2_Cz, na.rm = T, paired = T)
# # t = -3.5732, df = 46, p-value = 0.0008409 ### Significant
# # Oddball for all sounds at Fz latency window 220-380 ms
# t.test(EEGData_NoMow_NAomit$RareAve_PhSqMow_N2_Fz, EEGData_NoMow_NAomit$ComAve_PhSqMow_N2_Fz, na.rm = T, paired = T) 
# # t = -1.8464, df = 46, p-value = 0.07127
# # Oddball for all sounds at Pz latency window 190-250 ms
# t.test(EEGData_NoMow_NAomit$RareAve_PhSqMow_N2_Pz, EEGData_NoMow_NAomit$ComAve_PhSqMow_N2_Pz, na.rm = T, paired = T) 
# # t = 0.83035, df = 46, p-value = 0.4106
# # Oddball for all sounds at FzCz latency window 220-380 ms
# t.test(EEGData_NoMow_NAomit$RareAve_PhSqMow_N2_FzCz, EEGData_NoMow_NAomit$ComAve_PhSqMow_N2_FzCz, na.rm = T, paired = T) 
# # t = -2.8704, df = 46, p-value = 0.006176 ### Significant

# N2 phone and square sounds ######
# Oddball for only phone and square sounds at Cz latency window 220-380 ms
t.test(EEGData_NoMow_NAomit$RareAve_PhSq_N2_Cz, EEGData_NoMow_NAomit$ComAve_PhSq_N2_Cz, na.rm = T, paired = T)
mean(EEGData_NoMow_NAomit$RareAve_PhSq_N2_Cz)
mean(EEGData_NoMow_NAomit$ComAve_PhSq_N2_Cz)
sd(EEGData_NoMow_NAomit$RareAve_PhSq_N2_Cz)
sd(EEGData_NoMow_NAomit$ComAve_PhSq_N2_Cz)
# 
mean(EEGData_NoMow_NAomit$RareAve_PhSq_N2_Cz)-mean(EEGData_NoMow_NAomit$ComAve_PhSq_N2_Cz)
# Oddball for only phone and square at Fz latency window 220-380 ms
t.test(EEGData_NoMow_NAomit$RareAve_PhSq_N2_Fz, EEGData_NoMow_NAomit$ComAve_PhSq_N2_Fz, na.rm = T, paired = T) 
# 
# Oddball for only phone and square at F3 latency window 220-380 ms
t.test(EEGData_NoMow_NAomit$RareAve_PhSq_N2_F3, EEGData_NoMow_NAomit$ComAve_PhSq_N2_F3, na.rm = T, paired = T) 
mean(EEGData_NoMow_NAomit$RareAve_PhSq_N2_F3)
mean(EEGData_NoMow_NAomit$ComAve_PhSq_N2_F3)
sd(EEGData_NoMow_NAomit$RareAve_PhSq_N2_F3)
sd(EEGData_NoMow_NAomit$ComAve_PhSq_N2_F3)
# Oddball for only phone and square at Pz latency window 190-250 ms
t.test(EEGData_NoMow_NAomit$RareAve_PhSq_N2_Pz, EEGData_NoMow_NAomit$ComAve_PhSq_N2_Pz, na.rm = T, paired = T) 
# 
# Oddball for only phone and square at FzCz latency window 220-380 ms
t.test(EEGData_NoMow_NAomit$RareAve_PhSq_N2_FzCz, EEGData_NoMow_NAomit$ComAve_PhSq_N2_FzCz, na.rm = T, paired = T) 
# 

# P3 all sounds ####
# # P3a Oddball for all sounds at Cz
# t.test(EEGData_NoMow_NAomit$RareAve_PhSqMow_P3_Cz, EEGData_NoMow_NAomit$ComAve_PhSqMow_P3_Cz, na.rm = T, paired = T)
# # t = 3.5257, df = 45, p-value = 0.000984 ### Significant
# # P3a Oddball for all sounds at Fz
# t.test(EEGData_NoMow_NAomit$RareAve_PhSqMow_P3_Fz, EEGData_NoMow_NAomit$ComAve_PhSqMow_P3_Fz, na.rm = T, paired = T) 
# # t = 2.9811, df = 46, p-value = 0.004579 ### Significant
# # P3b Oddball for all sounds at Pz
# t.test(EEGData_NoMow_NAomit$RareAve_PhSqMow_P3_Pz, EEGData_NoMow_NAomit$ComAve_PhSqMow_P3_Pz, na.rm = T, paired = T) 
# # t = -0.00022105, df = 46, p-value = 0.9998

# P3 phone and square sounds #####
# P3a Oddball for only phone and square sounds at Cz latency window 400-380 ms 
t.test(EEGData_NoMow_NAomit$RareAve_PhSq_P3_Cz, EEGData_NoMow_NAomit$ComAve_PhSq_P3_Cz, na.rm = T, paired = T)
# 
# P3a Oddball for only phone and square at Fz latency window 220-380 ms
t.test(EEGData_NoMow_NAomit$RareAve_PhSq_P3_Fz, EEGData_NoMow_NAomit$ComAve_PhSq_P3_Fz, na.rm = T, paired = T) 
# 
# P3b Oddball for only phone and square at Pz latency window 260 – 600 ms
t.test(EEGData_NoMow_NAomit$RareAve_PhSq_P3_Pz, EEGData_NoMow_NAomit$ComAve_PhSq_P3_Pz, na.rm = T, paired = T) 
mean(EEGData_NoMow_NAomit$RareAve_PhSq_P3_Pz)
mean(EEGData_NoMow_NAomit$ComAve_PhSq_P3_Pz)
sd(EEGData_NoMow_NAomit$RareAve_PhSq_P3_Pz)
sd(EEGData_NoMow_NAomit$ComAve_PhSq_P3_Pz)
mean(EEGData_NoMow_NAomit$RareAve_PhSq_P3_Pz)-mean(EEGData_NoMow_NAomit$ComAve_PhSq_P3_Pz)



##### Overall Sound Comparisons ####
# P2 Fz
t.test(EEGData_NoMow_NAomit$ComRareAve_Ph_P2_Fz, EEGData_NoMow_NAomit$ComRareAve_Sq_P2_Fz, na.rm = T, paired = T)
mean(EEGData_NoMow_NAomit$ComRareAve_Ph_P2_Fz)
mean(EEGData_NoMow_NAomit$ComRareAve_Sq_P2_Fz)
sd(EEGData_NoMow_NAomit$ComRareAve_Ph_P2_Fz)
sd(EEGData_NoMow_NAomit$ComRareAve_Sq_P2_Fz)
# N2 F3
t.test(EEGData_NoMow_NAomit$ComRareAve_Ph_N2_F3, EEGData_NoMow_NAomit$ComRareAve_Sq_N2_F3, na.rm = T, paired = T)
mean(EEGData_NoMow_NAomit$ComRareAve_Ph_N2_F3)
mean(EEGData_NoMow_NAomit$ComRareAve_Sq_N2_F3)
sd(EEGData_NoMow_NAomit$ComRareAve_Ph_N2_F3)
sd(EEGData_NoMow_NAomit$ComRareAve_Sq_N2_F3)
length(EEGData_NoMow_NAomit$ComRareAve_Sq_N2_F3)
# P3 Pz
t.test(EEGData_NoMow_NAomit$ComRareAve_Ph_P3_Pz, EEGData_NoMow_NAomit$ComRareAve_Sq_P3_Pz, na.rm = T, paired = T)
mean(EEGData_NoMow_NAomit$ComRareAve_Ph_P3_Pz)
mean(EEGData_NoMow_NAomit$ComRareAve_Sq_P3_Pz)
sd(EEGData_NoMow_NAomit$ComRareAve_Ph_P3_Pz)
sd(EEGData_NoMow_NAomit$ComRareAve_Sq_P3_Pz)
##### Sound Oddball Compares ####
# P2 ####
t.test(EEGData_NoMow_NAomit$phoddball_P2_Cz, EEGData_NoMow_NAomit$sqoddball_P2_Cz, na.rm = T, paired = T)
t.test(EEGData_NoMow_NAomit$phoddball_P2_Fz, EEGData_NoMow_NAomit$sqoddball_P2_Fz, na.rm = T, paired = T)
mean(EEGData_NoMow_NAomit$phoddball_P2_Fz)
mean(EEGData_NoMow_NAomit$sqoddball_P2_Fz)
sd(EEGData_NoMow_NAomit$phoddball_P2_Fz)
sd(EEGData_NoMow_NAomit$sqoddball_P2_Fz)
cor(EEGData_NoMow_NAomit$phoddball_P2_Fz, EEGData_NoMow_NAomit$sqoddball_P2_Fz)
t.test(EEGData_NoMow_NAomit$phoddball_P2_Pz, EEGData_NoMow_NAomit$sqoddball_P2_Pz, na.rm = T, paired = T)
# N2 #####
t.test(EEGData_NoMow_NAomit$phoddball_N2_Cz, EEGData_NoMow_NAomit$sqoddball_N2_Cz, na.rm = T, paired = T)
t.test(EEGData_NoMow_NAomit$phoddball_N2_Fz, EEGData_NoMow_NAomit$sqoddball_N2_Fz, na.rm = T, paired = T)
t.test(EEGData_NoMow_NAomit$phoddball_N2_F3, EEGData_NoMow_NAomit$sqoddball_N2_F3, na.rm = T, paired = T)
mean(EEGData_NoMow_NAomit$phoddball_N2_F3)
mean(EEGData_NoMow_NAomit$sqoddball_N2_F3)
sd(EEGData_NoMow_NAomit$phoddball_N2_F3)
sd(EEGData_NoMow_NAomit$sqoddball_N2_F3)
cor(EEGData_NoMow_NAomit$phoddball_N2_F3, EEGData_NoMow_NAomit$sqoddball_N2_F3)
t.test(EEGData_NoMow_NAomit$phoddball_N2_FzCz, EEGData_NoMow_NAomit$sqoddball_N2_FzCz, na.rm = T, paired = T)
t.test(EEGData_NoMow_NAomit$phoddball_N2_Pz, EEGData_NoMow_NAomit$sqoddball_N2_Pz, na.rm = T, paired = T)
# P3 ####
t.test(EEGData_NoMow_NAomit$phoddball_P3_Cz, EEGData_NoMow_NAomit$sqoddball_P3_Cz, na.rm = T, paired = T)
t.test(EEGData_NoMow_NAomit$phoddball_P3_Fz, EEGData_NoMow_NAomit$sqoddball_P3_Fz, na.rm = T, paired = T)
t.test(EEGData_NoMow_NAomit$phoddball_P3_Pz, EEGData_NoMow_NAomit$sqoddball_P3_Pz, na.rm = T, paired = T)
mean(EEGData_NoMow_NAomit$phoddball_P3_Pz)
mean(EEGData_NoMow_NAomit$sqoddball_P3_Pz)
sd(EEGData_NoMow_NAomit$phoddball_P3_Pz)
sd(EEGData_NoMow_NAomit$sqoddball_P3_Pz)
cor(EEGData_NoMow_NAomit$phoddball_P3_Pz, EEGData_NoMow_NAomit$sqoddball_P3_Pz)

####### Overall oddball analyses######
# # P2 ####
summary(lm(data = EEGData_NoMow_NAomit, ComAve_PhSq_P2_Cz ~ RareAve_PhSq_P2_Cz))
summary(lm(data = EEGData_NoMow_NAomit, ComAve_PhSq_P2_Fz ~ RareAve_PhSq_P2_Fz))
summary(lm(data = EEGData_NoMow_NAomit, ComAve_PhSq_P2_Pz ~ RareAve_PhSq_P2_Pz))
# # N2 ####
summary(lm(data = EEGData_NoMow_NAomit, ComAve_PhSq_N2_Cz ~ RareAve_PhSq_N2_Cz))
summary(lm(data = EEGData_NoMow_NAomit, ComAve_PhSq_N2_Fz ~ RareAve_PhSq_N2_Fz))
summary(lm(data = EEGData_NoMow_NAomit, ComAve_PhSq_N2_F3 ~ RareAve_PhSq_N2_F3))
summary(lm(data = EEGData_NoMow_NAomit, ComAve_PhSq_N2_FzCz ~ RareAve_PhSq_N2_FzCz))
summary(lm(data = EEGData_NoMow_NAomit, ComAve_PhSq_N2_Pz ~ RareAve_PhSq_N2_Pz))
# # P3 ####
summary(lm(data = EEGData_NoMow_NAomit, ComAve_PhSq_P3_Cz ~ RareAve_PhSq_P3_Cz))
summary(lm(data = EEGData_NoMow_NAomit, ComAve_PhSq_P3_Fz ~ RareAve_PhSq_P3_Fz))
summary(lm(data = EEGData_NoMow_NAomit, ComAve_PhSq_P3_Pz ~ RareAve_PhSq_P3_Pz))
# 
# t.test(x, y, paired = TRUE, alternative = "two.sided")
##### Overall Sound Comparisons
##### Overall Sound Differences
# P2 Cz ns   ##### 
phoneP2_Cz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_P2_Cz, ComRareAve_Sq_P2_Cz, factor_key = T) 
phoneP2_Cz$Sound <- factor(phoneP2_Cz$Sound,levels=c("ComRareAve_Ph_P2_Cz","ComRareAve_Sq_P2_Cz"))
str(phoneP2_Cz$Sound)
contrasts(phoneP2_Cz$Sound) = contr.treatment(2)
contrasts(phoneP2_Cz$Sound)
mod.P2_Cz_Sound <- lmer(data = phoneP2_Cz, MeanAmp ~ Sound + (1|Part_ID), REML = F)
summary(mod.P2_Cz_Sound)  # p = 0.121 
t.test(EEGData_NoMow_NAomit$ComRareAve_Ph_P2_Cz, EEGData_NoMow_NAomit$ComRareAve_Sq_P2_Cz, na.rm = T, paired = T)
# P2 Fz ns   ##### 
phoneP2_Fz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_P2_Fz, ComRareAve_Sq_P2_Fz, factor_key = T) 
phoneP2_Fz$Sound <- factor(phoneP2_Fz$Sound,levels=c("ComRareAve_Ph_P2_Fz","ComRareAve_Sq_P2_Fz"))
str(phoneP2_Fz$Sound)
contrasts(phoneP2_Fz$Sound) = contr.treatment(2)
contrasts(phoneP2_Fz$Sound)
mod.P2_Fz_Sound <- lmer(data = phoneP2_Fz, MeanAmp ~ Sound + (Sound|Part_ID), REML = F)
summary(mod.P2_Fz_Sound)  # p = 0.0512 
t.test(EEGData_NoMow_NAomit$ComRareAve_Ph_P2_Fz, EEGData_NoMow_NAomit$ComRareAve_Sq_P2_Fz, na.rm = T, paired = T)
describeBy(phoneP2_Fz$MeanAmp, phoneP2_Fz$Sound)
describe(EEGData_NoMow_NAomit$ComRareAve_Ph_P2_Fz)
mean(EEGData_NoMow_NAomit$ComRareAve_Ph_P2_Fz)
mean(EEGData_NoMow_NAomit$ComRareAve_Sq_P2_Fz)
sd(EEGData_NoMow_NAomit$ComRareAve_Ph_P2_Fz)
sd(EEGData_NoMow_NAomit$ComRareAve_Sq_P2_Fz)
# P2 Pz ns  ##### 
phoneP2_Pz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_P2_Pz, ComRareAve_Sq_P2_Pz, factor_key = T) 
phoneP2_Pz$Sound <- factor(phoneP2_Pz$Sound,levels=c("ComRareAve_Ph_P2_Pz","ComRareAve_Sq_P2_Pz"))
str(phoneP2_Pz$Sound)
contrasts(phoneP2_Pz$Sound) = contr.treatment(2)
contrasts(phoneP2_Pz$Sound)
mod.P2_Pz_Sound <- lmer(data = phoneP2_Pz, MeanAmp ~ Sound + (1 + Sound|Part_ID), REML = F)
summary(mod.P2_Pz_Sound)  # p = 0.712
t.test(EEGData_NoMow_NAomit$ComRareAve_Ph_P2_Pz, EEGData_NoMow_NAomit$ComRareAve_Sq_P2_Pz, na.rm = T, paired = T)

# N2 Cz ns  ##### 
phoneN2_Cz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_N2_Cz, ComRareAve_Sq_N2_Cz, factor_key = T) 
phoneN2_Cz$Sound <- factor(phoneN2_Cz$Sound,levels=c("ComRareAve_Ph_N2_Cz","ComRareAve_Sq_N2_Cz"))
str(phoneN2_Cz$Sound)
contrasts(phoneN2_Cz$Sound) = contr.treatment(2)
contrasts(phoneN2_Cz$Sound)
mod.N2_Cz_Sound <- lmer(data = phoneN2_Cz, MeanAmp ~ Sound + (1|Part_ID), REML = F)
summary(mod.N2_Cz_Sound)  # p = 0.19
t.test(EEGData_NoMow_NAomit$ComRareAve_Ph_N2_Cz, EEGData_NoMow_NAomit$ComRareAve_Sq_N2_Cz, na.rm = T, paired = T)
mean(EEGData_NoMow_NAomit$ComRareAve_Ph_N2_Cz)
mean(EEGData_NoMow_NAomit$ComRareAve_Sq_N2_Cz)
sd(EEGData_NoMow_NAomit$ComRareAve_Ph_N2_Cz)
sd(EEGData_NoMow_NAomit$ComRareAve_Sq_N2_Cz)
# N2 Fz ns   ##### 
phoneN2_Fz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_N2_Fz, ComRareAve_Sq_N2_Fz, factor_key = T) 
phoneN2_Fz$Sound <- factor(phoneN2_Fz$Sound,levels=c("ComRareAve_Ph_N2_Fz","ComRareAve_Sq_N2_Fz"))
str(phoneN2_Fz$Sound)
contrasts(phoneN2_Fz$Sound) = contr.treatment(2)
contrasts(phoneN2_Fz$Sound)
mod.N2_Fz_Sound <- lmer(data = phoneN2_Fz, MeanAmp ~ Sound + (1|Part_ID), REML = F)
summary(mod.N2_Fz_Sound)  #  
t.test(EEGData_NoMow_NAomit$ComRareAve_Ph_N2_Fz, EEGData_NoMow_NAomit$ComRareAve_Sq_N2_Fz, na.rm = T, paired = T)

# N2 F3 ns   ##### 
phoneN2_F3 <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_N2_F3, ComRareAve_Sq_N2_F3, factor_key = T) 
phoneN2_F3$Sound <- factor(phoneN2_F3$Sound,levels=c("ComRareAve_Ph_N2_F3","ComRareAve_Sq_N2_F3"))
str(phoneN2_F3$Sound)
contrasts(phoneN2_F3$Sound) = contr.treatment(2)
contrasts(phoneN2_F3$Sound)
mod.N2_F3_Sound <- lmer(data = phoneN2_F3, MeanAmp ~ Sound + (1|Part_ID), REML = F)
summary(mod.N2_F3_Sound)  #  
t.test(EEGData_NoMow_NAomit$ComRareAve_Ph_N2_F3, EEGData_NoMow_NAomit$ComRareAve_Sq_N2_F3, na.rm = T, paired = T)
mean(EEGData_NoMow_NAomit$ComRareAve_Ph_N2_F3)
mean(EEGData_NoMow_NAomit$ComRareAve_Sq_N2_F3)
sd(EEGData_NoMow_NAomit$ComRareAve_Ph_N2_F3)
sd(EEGData_NoMow_NAomit$ComRareAve_Sq_N2_F3)
# N2 Pz ns   ##### 
unique(EEGData_NoMow_NAomit$Part_ID)
phoneN2_Pz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_N2_Pz, ComRareAve_Sq_N2_Pz, factor_key = T) 
phoneN2_Pz$Sound <- factor(phoneN2_Pz$Sound,levels=c("ComRareAve_Ph_N2_Pz","ComRareAve_Sq_N2_Pz"))
str(phoneN2_Pz$Sound)
contrasts(phoneN2_Pz$Sound) = contr.treatment(2)
contrasts(phoneN2_Pz$Sound)
mod.N2_Pz_Sound <- lmer(data = phoneN2_Pz, MeanAmp ~ Sound + (1|Part_ID), REML = F)
summary(mod.N2_Pz_Sound)  # 
t.test(EEGData_NoMow_NAomit$ComRareAve_Ph_N2_Pz, EEGData_NoMow_NAomit$ComRareAve_Sq_N2_Pz, na.rm = T, paired = T)

# N2 FzCz ns   ##### 
unique(EEGData_NoMow_NAomit$Part_ID)
phoneN2_FzCz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_N2_FzCz, ComRareAve_Sq_N2_FzCz, factor_key = T) 
phoneN2_FzCz$Sound <- factor(phoneN2_FzCz$Sound,levels=c("ComRareAve_Ph_N2_FzCz","ComRareAve_Sq_N2_FzCz"))
str(phoneN2_FzCz$Sound)
contrasts(phoneN2_FzCz$Sound) = contr.treatment(2)
contrasts(phoneN2_FzCz$Sound)
mod.N2_FzCz_Sound <- lmer(data = phoneN2_FzCz, MeanAmp ~ Sound + (1|Part_ID), REML = F)
summary(mod.N2_FzCz_Sound)  # p = 0.0982 . 
t.test(EEGData_NoMow_NAomit$ComRareAve_Ph_N2_FzCz, EEGData_NoMow_NAomit$ComRareAve_Sq_N2_FzCz, na.rm = T, paired = T)

# P3 Cz ns   ##### 
phoneP3_Cz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_P3_Cz, ComRareAve_Sq_P3_Cz, factor_key = T) 
phoneP3_Cz$Sound <- factor(phoneP3_Cz$Sound,levels=c("ComRareAve_Ph_P3_Cz","ComRareAve_Sq_P3_Cz"))
str(phoneP3_Cz$Sound)
contrasts(phoneP3_Cz$Sound) = contr.treatment(2)
contrasts(phoneP3_Cz$Sound)
mod.P3_Cz_Sound <- lmer(data = phoneP3_Cz, MeanAmp ~ Sound + (1|Part_ID), REML = F)
summary(mod.P3_Cz_Sound)  # p = 0.889
t.test(EEGData_NoMow_NAomit$ComRareAve_Ph_P3_Cz, EEGData_NoMow_NAomit$ComRareAve_Sq_P3_Cz, na.rm = T, paired = T)

# P3 Fz ns  ##### 
phoneP3_Fz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_P3_Fz, ComRareAve_Sq_P3_Fz, factor_key = T) 
phoneP3_Fz$Sound <- factor(phoneP3_Fz$Sound,levels=c("ComRareAve_Ph_P3_Fz","ComRareAve_Sq_P3_Fz"))
str(phoneP3_Fz$Sound)
contrasts(phoneP3_Fz$Sound) = contr.treatment(2)
contrasts(phoneP3_Fz$Sound)
mod.P3_Fz_Sound <- lmer(data = phoneP3_Fz, MeanAmp ~ Sound + (1|Part_ID), REML = F)
summary(mod.P3_Fz_Sound)  # p = 0.666
t.test(EEGData_NoMow_NAomit$ComRareAve_Ph_P3_Fz, EEGData_NoMow_NAomit$ComRareAve_Sq_P3_Fz, na.rm = T, paired = T)

# P3 Pz ns ##### 
unique(EEGData_NoMow_NAomit$Part_ID)
phoneP3_Pz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_P3_Pz, ComRareAve_Sq_P3_Pz, factor_key = T) 
phoneP3_Pz$Sound <- factor(phoneP3_Pz$Sound,levels=c("ComRareAve_Ph_P3_Pz","ComRareAve_Sq_P3_Pz"))
str(phoneP3_Pz$Sound)
contrasts(phoneP3_Pz$Sound) = contr.treatment(2)
contrasts(phoneP3_Pz$Sound)
mod.P3_Pz_Sound <- lmer(data = phoneP3_Pz, MeanAmp ~ Sound + (1|Part_ID), REML = F)
summary(mod.P3_Pz_Sound)  # p = 0.926
t.test(EEGData_NoMow_NAomit$ComRareAve_Ph_P3_Pz, EEGData_NoMow_NAomit$ComRareAve_Sq_P3_Pz, na.rm = T, paired = T)
mean(EEGData_NoMow_NAomit$ComRareAve_Ph_P3_Pz)
mean(EEGData_NoMow_NAomit$ComRareAve_Sq_P3_Pz)
sd(EEGData_NoMow_NAomit$ComRareAve_Ph_P3_Pz)
sd(EEGData_NoMow_NAomit$ComRareAve_Sq_P3_Pz)
#### Overall SAPS Comparisons ####
EEGData_NoMow_NAomit$SAPS.c <- EEGData_NoMow_NAomit$SAPS_Ave - mean(EEGData_NoMow_NAomit$SAPS_Ave)
# descriptives ####
ERPampsSAPS <- ddply(EEGData_NoMow_NAomit,
                .(SAPS_Med_Split),
                summarise,
                n = length(Part_ID),
                Mean.amp.P2_Fz = mean(P2_Fz_PhConAve),
                SD.amp.P2_Fz = sd(P2_Fz_PhConAve),
                Mean.amp.P2_Cz = mean(P2_Cz_PhConAve),
                SD.amp.P2_Cz = sd(P2_Cz_PhConAve),
                Mean.amp.P2_Pz = mean(P2_Pz_PhConAve),
                SD.amp.P2_Pz = sd(P2_Pz_PhConAve),
                Mean.amp.N2_Fz = mean(N2_Fz_PhConAve),
                SD.amp.N2_Fz = sd(N2_Fz_PhConAve),
                Mean.amp.N2_Cz = mean(N2_Cz_PhConAve),
                SD.amp.N2_Cz = sd(N2_Cz_PhConAve),
                Mean.amp.N2_Pz = mean(N2_Pz_PhConAve),
                SD.amp.N2_Pz = sd(N2_Pz_PhConAve),
                Mean.amp.N2_FzCz = mean(N2_FzCz_PhConAve),
                SD.amp.N2_FzCz = sd(N2_FzCz_PhConAve),
                Mean.amp.N2_F3 = mean(N2_F3_PhConAve),
                SD.amp.N2_F3 = sd(N2_F3_PhConAve),
                Mean.amp.P3_Fz = mean(P3_Fz_PhConAve),
                SD.amp.P3_Fz = sd(P3_Fz_PhConAve),
                Mean.amp.P3_Cz = mean(P3_Cz_PhConAve),
                SD.amp.P3_Cz = sd(P3_Cz_PhConAve),
                Mean.amp.P3_Pz = mean(P3_Pz_PhConAve),
                SD.amp.P3_Pz = sd(P3_Pz_PhConAve)) 
ERPampsSAPS
str
describeBy(EEGData_NoMow_NAomit)
# P2 ####
mod.P2_Fz <- lm(data = EEGData_NoMow_NAomit, P2_Fz_PhConAve ~ SAPS.c)
summary(mod.P2_Fz) # p = 0.00882
mod.P2_Fz <- lm(data = EEGData_NoMow_NAomit, P2_Fz_PhConAve ~ SAPS.c)
summary(mod.P2_Fz) # p = 0.0394
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   14.656      2.877   5.094 4.96e-06 ***
#   SAPS_Ave      -3.046      1.442  -2.113   0.0394 *  
contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split) 
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.P2_Fz))
confint(mod.P2_Fz)
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = F)
anova(mod.P2_Fz)
57.91/(57.91+674.49) # = 0.07906881
0.07906881/(1-0.07906881) # = 0.08585746
mod.P2_Cz <- lm(data = EEGData_NoMow_NAomit, P2_Cz_PhConAve ~ SAPS_Med_Split)
summary(mod.P2_Cz) # p = 0.0571
mod.P2_Pz <- lm(data = EEGData_NoMow_NAomit, P2_Pz_PhConAve ~ SAPS_Med_Split)
summary(mod.P2_Pz) # p = 0.843
# Plot ####
ggplot(EEGData_NoMow_NAomit, aes(x = SAPS_Ave, y = P2_Fz_PhConAve)) +
  geom_point() +
  geom_smooth(method = lm, se = T, color = "black", size = 2) +
  scale_color_brewer(palette = "Set1") +
  #geom_hline(yintercept = mean(EEGData_NoMow_NAomit$P2_Pz_PhConAve)) +
  labs(x = "Smartphone Addiction Proneness (SAPS)", y = "Overall P2 (mv)") +
  #scale_x_continuous(limits = c(0:4), breaks = seq(1,4)) +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.position = "bottom") + 
  theme_classic()+ 
  theme(axis.text = element_text(angle = 0, color="black",size=35, face = 3)) +
  theme(axis.title = element_text(angle = 0, color="black", size=40, face = "bold")) +
  theme(axis.title.x = element_text(vjust = -.6)) +
  theme(axis.title.y = element_text(vjust = 1.5))+
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))
  
# N2 ####
contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split)
EEGData_NoMow_NAomit$SAPS_Med_Split <- relevel(EEGData_NoMow_NAomit$SAPS_Med_Split, ref="Low")
contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split) = contr.treatment(2)
contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split)
# N2 Fz
mod.N2_Fz <- lm(data = EEGData_NoMow_NAomit, N2_F3_PhConAve ~ SAPS.c)
summary(mod.N2_Fz) # p = 0.07700 .
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)    9.025      2.735   3.300  0.00175 **
#   SAPS_Ave      -2.472      1.370  -1.804  0.07700 . 
mod.N2_Cz <- lm(data = EEGData_NoMow_NAomit, N2_Cz_PhConAve ~ SAPS_Med_Split)
summary(mod.N2_Cz) # p = 0.0713  
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.N2_Cz))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)
# N2 F3
mod.N2_F3 <- lm(data = EEGData_NoMow_NAomit, N2_F3_PhConAve ~ SAPS_Med_Split)
summary(mod.N2_F3) # 
mod.N2_F3 <- lm(data = EEGData_NoMow_NAomit, N2_F3_PhConAve ~ SAPS.c)
summary(mod.N2_F3) # 
anova(mod.N2_F3)
38.14/(38.14+609.28) 
# eta sqaured = 0.05891075
0.05891075/(1-0.05891075)

# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.N2_F3))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)
# N2 Pz
mod.N2_Pz <- lm(data = EEGData_NoMow_NAomit, N2_Pz_PhConAve ~ SAPS_Med_Split)
summary(mod.N2_Pz) # p = 0.293
mod.N2_FzCz <- lm(data = EEGData_NoMow_NAomit, N2_FzCz_PhConAve ~ SAPS_Med_Split)
summary(mod.N2_FzCz)
#0.128
# P3 ####
mod.P3_Fz <- lm(data = EEGData_NoMow_NAomit, P3_Fz_PhConAve ~ SAPS_Med_Split)
summary(mod.P3_Fz) # p = 0.215
contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split) = contr.treatment(2)
contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split)
mod.P3_Cz <- lm(data = EEGData_NoMow_NAomit, P3_Cz_PhConAve ~ SAPS_Med_Split)
summary(mod.P3_Cz) # p = 0.0834
mod.P3_Pz <- lm(data = EEGData_NoMow_NAomit, P3_Pz_PhConAve ~ SAPS.c)
summary(mod.P3_Pz) # p = 0.249542
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.P3_Pz))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)
anova(mod.P3_Pz)
31.75/(31.75+1217.66) # = 0.02541199
0.02541199/(1-0.02541199)
# Total oddball Descriptives #####
SAPSTotaloddballeffects <- ddply(EEGData_NoMow_NAomit,
                                 .(SAPS_Med_Split),
                                 summarise,
                                 n = length(Part_ID),
                                 P2_FzMean1 = mean(Total_Oddball_PhSq_P2_Fz),
                                 P2_FzSD1 = sd(Total_Oddball_PhSq_P2_Fz),
                                 P2_CzMean2 = mean(Total_Oddball_PhSq_P2_Cz),
                                 P2_CzSD2 = sd(Total_Oddball_PhSq_P2_Cz),
                                 P2_PzMean3 = mean(Total_Oddball_PhSq_P2_Pz),
                                 P2_PzSD3 = sd(Total_Oddball_PhSq_P2_Pz),
                                 N2_FzMean4 = mean(Total_Oddball_PhSq_N2_Fz),
                                 N2_FzSD4 = sd(Total_Oddball_PhSq_N2_Fz),
                                 N2_CzMean5 = mean(Total_Oddball_PhSq_N2_Cz),
                                 N2_CzSD5 = sd(Total_Oddball_PhSq_N2_Cz),
                                 N2_PzMean6 = mean(Total_Oddball_PhSq_N2_Pz),
                                 N2_PzSD6 = sd(Total_Oddball_PhSq_N2_Pz),
                                 N2_FzCzMean7 = mean(Total_Oddball_PhSq_N2_FzCz),
                                 N2_FzCzSD7 = sd(Total_Oddball_PhSq_N2_FzCz),
                                 N2_F3Mean11 = mean(Total_Oddball_PhSq_N2_F3),
                                 N2_F3SD11 = sd(Total_Oddball_PhSq_N2_F3),
                                 P3_FzMean8 = mean(Total_Oddball_PhSq_P3_Fz),
                                 P3_FzSD8 = sd(Total_Oddball_PhSq_P3_Fz),
                                 P3_CzMean9 = mean(Total_Oddball_PhSq_P3_Cz),
                                 P3_CzSD9 = sd(Total_Oddball_PhSq_P3_Cz),
                                 P3_PzMean10 = mean(Total_Oddball_PhSq_P3_Pz),
                                 P3_PzSD10 = sd(Total_Oddball_PhSq_P3_Pz))
SAPSTotaloddballeffects
# P2 #####
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_P2_Cz ~ SAPS.c)) #.1086
contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split)
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_P2_Fz ~ SAPS.c))
mod.TotOddballP2Fz <- lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_P2_Fz ~ SAPS.c)

EEGData_NoMow_NAomit$SAPS_Med_Split <- relevel(EEGData_NoMow_NAomit$SAPS_Med_Split, ref="Low")

# Cohen's d and conf intervals
fixef(mod.OverallOddball_SAPS_lmer) + c(-2,2)*arm::se.fixef(mod.OverallOddball_SAPS_lmer)
(param_tab <- parameters::parameters(mod.TotOddballP2Fz))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)
anova(mod.TotOddballP2Fz)
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_P2_Pz ~ SAPS_Med_Split))
# N2 #####
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_Cz ~ SAPS_Med_Split))
mod.TotOddballN2Cz <-lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_Cz ~ SAPS_Med_Split)
contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.TotOddballN2Cz))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)
# N2 F3
EEGData_NoMow_NAomit <- mutate(EEGData_NoMow_NAomit,Total_Oddball_PhSq_N2_F3 = RareAve_PhSq_N2_F3 - ComAve_PhSq_N2_F3)
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_F3 ~ SAPS.c))
mod.TotOddballN2F3 <-lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_F3 ~ SAPS.c)
contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.TotOddballN2F3))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_Fz ~ SAPS_Med_Split))
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_Pz ~ SAPS_Med_Split))
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_FzCz ~ SAPS_Med_Split))
anova1 <- ezANOVA(EEGData_NoMow_NAomit, dv = Total_Oddball_PhSq_N2_FzCz, wid = Part_ID, between = SAPS_Med_Split, type = 3, return_aov = TRUE)
anova1
emm <- emmeans(anova$aov, ~ SAPS_Med_Split)
N2_FzCz <- ddply(EEGData_NoMow_NAomit,
                 .(SAPS_Med_Split),
                 summarise,
                 Mean = mean(Total_Oddball_PhSq_N2_FzCz),
                 SD = sd(Total_Oddball_PhSq_N2_FzCz))
N2_FzCz
# P3 #####
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_P3_Cz ~ SAPS_Med_Split))
contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split)
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_P3_Fz ~ SAPS_Med_Split))
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_P3_Pz ~ SAPS.c))
mod.TotOddballP3Pz <- lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_P3_Pz ~ SAPS.c)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.TotOddballP3Pz))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)
#### Overall SAPS*Sounds Comparisons ####
describe(EEGData_NoMow_NAomit$N2_Pz_PhConAve)
describeBy(EEGData_NoMow_NAomit$P2_Fz_PhConAve, EEGData_NoMow_NAomit$SAPS_Med_Split)

# P2 Cz ns  p = 0.109 ##### 
phoneP2_Cz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_P2_Cz, ComRareAve_Sq_P2_Cz, factor_key = T) 
phoneP2_Cz$Sound <- factor(phoneP2_Cz$Sound,levels=c("ComRareAve_Ph_P2_Cz","ComRareAve_Sq_P2_Cz"))
str(phoneP2_Cz$Sound)
contrasts(BigTech_FinalExcludes_NoMow$Sound) <- matrix( c(.5,-.5),ncol = 1)

contrasts(phoneP2_Cz$Sound) = contr.treatment(2)
contrasts(phoneP2_Cz$Sound)
contrasts(phoneP2_Cz$SAPS_Med_Split) = contr.treatment(2)
contrasts(phoneP2_Cz$SAPS_Med_Split)
mod.P2_Cz_Sound <- lmer(data = phoneP2_Cz, MeanAmp ~ Sound * SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.P2_Cz_Sound)  # p = 0.0813
# P2 Fz ns  p = 0.0496 ##### 
phoneP2_Fz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_P2_Fz, ComRareAve_Sq_P2_Fz, factor_key = T) 
phoneP2_Fz$Sound <- factor(phoneP2_Fz$Sound, levels = c("ComRareAve_Ph_P2_Fz","ComRareAve_Sq_P2_Fz"), labels = c("Phone_P2_Pz", "Control_P2_Pz"))
contrasts(phoneP2_Fz$Sound)
phoneP2_Fz$Sound <- relevel(phoneP2_Fz$Sound, ref="ComRareAve_Ph_P2_Fz")
contrasts(phoneP2_Fz$Sound) <- matrix( c(.5,-.5),ncol = 1)
contrasts(phoneP2_Fz$Sound) 

contrasts(phoneP2_Fz$Sound) <- varContrasts(phoneP2_Fz$Sound, 
                                            Type = "POC",   
                                            POCList = list(c(.5, -.5)),
                                            Labels = c("Phone_P2_Pz", "Control_P2_Pz"))
 
mod.P2_Fz_Sound <- lmer(data = phoneP2_Fz, MeanAmp ~ Sound * SAPS.c + (1|Part_ID), REML = F, control = control)
summary(mod.P2_Fz_Sound)
#mod.P2_Fz_Sound2 <- lmer(data = phoneP2_Fz, MeanAmp ~ Sound * SAPS.c + (Sound|Part_ID), REML = F, control = control)
#anova(mod.P2_Fz_Sound,mod.P2_Fz_Sound2)
# Not including the sound condition as a random sloped is best model
confint(mod.P2_Fz_Sound)
phoneP2_Fz$Sound <- relevel(phoneP2_Fz$Sound, ref="ComRareAve_Sq_P2_Fz")
ddply(phoneP2_Fz,
      .(SAPS_Med_Split, Sound),
      summarise,
      n = length(Part_ID),
      M = mean(MeanAmp),
      SD = sd(MeanAmp))
# Cohen's d and conf intervals
fixef(mod.P2_Fz_Sound) + c(-4,4)*arm::se.fixef(mod.P2_Fz_Sound)
(param_tab <- parameters::parameters(mod.P2_Fz_Sound))
effectsize::t_to_d(param_tab$t[4], param_tab$df[4], pooled = FALSE)
# Int values
describeBy(phoneP2_Fz$Sound, phoneP2_Fz$SAPS_Med_Split)

str(phoneP2_Fz$Sound)
contrasts(phoneP2_Fz$Sound) = contr.treatment(2)
contrasts(phoneP2_Fz$Sound)
 # p = 0.0510
# P2 Pz ns  p = 0.376 ##### 
phoneP2_Pz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_P2_Pz, ComRareAve_Sq_P2_Pz, factor_key = T) 
phoneP2_Pz$Sound <- factor(phoneP2_Pz$Sound,levels=c("ComRareAve_Ph_P2_Pz","ComRareAve_Sq_P2_Pz"))
str(phoneP2_Pz$Sound)
contrasts(phoneP2_Pz$Sound) = contr.treatment(2)
contrasts(phoneP2_Pz$Sound)
mod.P2_Pz_Sound <- lmer(data = phoneP2_Pz, MeanAmp ~ Sound* SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.P2_Pz_Sound)  # p = 0.594
# N2 Cz ns  p = 0.227 ##### 
phoneN2_Cz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_N2_Cz, ComRareAve_Sq_N2_Cz, factor_key = T) 
phoneN2_Cz$Sound <- factor(phoneN2_Cz$Sound,levels=c("ComRareAve_Ph_N2_Cz","ComRareAve_Sq_N2_Cz"))
str(phoneN2_Cz$Sound)
contrasts(phoneN2_Cz$Sound) = contr.treatment(2)
contrasts(phoneN2_Cz$Sound)
mod.N2_Cz_Sound <- lmer(data = phoneN2_Cz, MeanAmp ~ Sound* SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.N2_Cz_Sound)  # p = 0.227
# N2 Fz ns  p = 0.266 ##### 
phoneN2_Fz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_N2_Fz, ComRareAve_Sq_N2_Fz, factor_key = T) 
phoneN2_Fz$Sound <- factor(phoneN2_Fz$Sound,levels=c("ComRareAve_Ph_N2_Fz","ComRareAve_Sq_N2_Fz"))
str(phoneN2_Fz$Sound)
contrasts(phoneN2_Fz$Sound) = contr.treatment(2)
contrasts(phoneN2_Fz$Sound)
mod.N2_Fz_Sound <- lmer(data = phoneN2_Fz, MeanAmp ~ Sound* SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.N2_Fz_Sound)  # p = 0.285
# N2 F3 ns  p =  ##### 
phoneN2_F3 <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_N2_F3, ComRareAve_Sq_N2_F3, factor_key = T) 
phoneN2_F3$Sound <- factor(phoneN2_F3$Sound,levels =c("ComRareAve_Ph_N2_F3","ComRareAve_Sq_N2_F3"), labels = c("Phone_N2_F3", "Control_N2_F3"))
str(phoneN2_F3$Sound)
contrasts(phoneN2_F3$Sound) = contr.treatment(2)
contrasts(phoneN2_F3$Sound)
contrasts(phoneN2_F3$Sound) <- varContrasts(phoneN2_F3$Sound, 
                                            Type = "POC", 
                                            POCList = list(c(.5, -.5)),
                                            Labels = c("Phone_N2_F3", "Control_N2_F3"))
mod.N2_F3_Sound <- lmer(data = phoneN2_F3, MeanAmp ~ Sound * SAPS.c + (1|Part_ID), REML = F, control = control)
summary(mod.N2_F3_Sound)  
confint(mod.N2_F3_Sound)
phoneN2_F3$Sound <- relevel(phoneN2_F3$Sound, ref="ComRareAve_Sq_N2_F3")
ddply(phoneN2_F3,
      .(SAPS_Med_Split, Sound),
      summarise,
      n = length(Part_ID),
      M = mean(MeanAmp),
      SD = sd(MeanAmp))
confint(mod.N2_F3_Sound)
# N2 Pz ns  p = 0.252 ##### 
unique(EEGData_NoMow_NAomit$Part_ID)
phoneN2_Pz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_N2_Pz, ComRareAve_Sq_N2_Pz, factor_key = T) 
phoneN2_Pz$Sound <- factor(phoneN2_Pz$Sound,levels=c("ComRareAve_Ph_N2_Pz","ComRareAve_Sq_N2_Pz"))
str(phoneN2_Pz$Sound)
contrasts(phoneN2_Pz$Sound) = contr.treatment(2)
contrasts(phoneN2_Pz$Sound)
mod.N2_Pz_Sound <- lmer(data = phoneN2_Pz, MeanAmp ~ Sound* SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.N2_Pz_Sound)  # p = 0.239
# N2 FzCz ns  p = 0.219 ##### 
unique(EEGData_NoMow_NAomit$Part_ID)
phoneN2_FzCz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_N2_FzCz, ComRareAve_Sq_N2_FzCz, factor_key = T) 
phoneN2_FzCz$Sound <- factor(phoneN2_FzCz$Sound,levels=c("ComRareAve_Ph_N2_FzCz","ComRareAve_Sq_N2_FzCz"))
str(phoneN2_FzCz$Sound)
contrasts(phoneN2_FzCz$Sound) = contr.treatment(2)
contrasts(phoneN2_FzCz$Sound)
mod.N2_FzCz_Sound <- lmer(data = phoneN2_FzCz, MeanAmp ~ Sound* SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.N2_FzCz_Sound)  # p = 0.228
# P3 Cz ns  p = 0.421 ##### 
phoneP3_Cz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_P3_Cz, ComRareAve_Sq_P3_Cz, factor_key = T) 
phoneP3_Cz$Sound <- factor(phoneP3_Cz$Sound,levels=c("ComRareAve_Ph_P3_Cz","ComRareAve_Sq_P3_Cz"))
str(phoneP3_Cz$Sound)
contrasts(phoneP3_Cz$Sound) = contr.treatment(2)
contrasts(phoneP3_Cz$Sound)
mod.P3_Cz_Sound <- lmer(data = phoneP3_Cz, MeanAmp ~ Sound* SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.P3_Cz_Sound)  # p = 0.302
# P3 Fz ns  p = 0.402 ##### 
phoneP3_Fz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_P3_Fz, ComRareAve_Sq_P3_Fz, factor_key = T) 
phoneP3_Fz$Sound <- factor(phoneP3_Fz$Sound,levels=c("ComRareAve_Ph_P3_Fz","ComRareAve_Sq_P3_Fz"))
str(phoneP3_Fz$Sound)
contrasts(phoneP3_Fz$Sound) = contr.treatment(2)
contrasts(phoneP3_Fz$Sound)
mod.P3_Fz_Sound <- lmer(data = phoneP3_Fz, MeanAmp ~ Sound* SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.P3_Fz_Sound)  # p = 0.402
# P3 Pz ns  p = 0.7846 ##### 
unique(EEGData_NoMow_NAomit$Part_ID)
phoneP3_Pz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_P3_Pz, ComRareAve_Sq_P3_Pz, factor_key = T) 
phoneP3_Pz$Sound <- factor(phoneP3_Pz$Sound,levels = c("ComRareAve_Ph_P3_Pz","ComRareAve_Sq_P3_Pz"),  labels = c("Phone_P3_Pz", "Control_P3_Pz"))
str(phoneP3_Pz$Sound)
contrasts(phoneP3_Pz$Sound) = contr.treatment(2)
contrasts(phoneP3_Pz$Sound)
contrasts(phoneP3_Pz$Sound)
contrasts(phoneP3_Pz$Sound) <- varContrasts(phoneP3_Pz$Sound, 
                                            Type = "POC", 
                                            POCList = list(c(.5, -.5)),
                                            Labels = c("Phone_P3_Pz", "Control_P3_Pz"))
mod.P3_Pz_Sound <- lmer(data = phoneP3_Pz, MeanAmp ~ Sound * SAPS.c + (1|Part_ID), REML = F, control = control)
summary(mod.P3_Pz_Sound)  
confint(mod.P3_Pz_Sound)
phoneP3_Pz$Sound <- relevel(phoneP3_Pz$Sound, ref="ComRareAve_Sq_P3_Pz")
ddply(phoneP3_Pz,
      .(SAPS_Med_Split, Sound),
      summarise,
      n = length(Part_ID),
      M = mean(MeanAmp),
      SD = sd(MeanAmp))
confint(mod.P3_Pz_Sound)
ez::ezANOVA(phoneP3_Pz, MeanAmp, Sound, within =SAPS_Med_Split)

10.26117 + 12.70400 
# Create SAPS thirds variable ####
EEGData_NoMow <- mutate(EEGData_NoMow, SAPS_thirds = as.factor(ifelse(SAPS_Ave < 1.67, "Low Third",
                                                                                                  ifelse(SAPS_Ave > 2.14, "High Third", "Mid Third"))))
# remove mid thirds crete new df
EEGData_NoMow_SAPS3 <- EEGData_NoMow[!EEGData_NoMow$SAPS_thirds == "Mid Third", ]

# Create long version of data #####
library(reshape2)
Long <-melt(EEGData_NoMow_NAomit,
             id.vars= c(2:30),
             measure.vars=c(32:151),
             variable.name="ERP",
             value.name="MeanAmp")
Long1 <-melt(EEGData_NoMow,
                     id.vars= c(2:30),
                     measure.vars=c(31:34),
                     variable.name="P3_Pz",
                     value.name="MeanAmpP3_Pz")
Long2 <-melt(EEGData_NoMow,
                           id.vars= c(2),
                           measure.vars=c(35:38),
                           variable.name="P3_Fz",
                           value.name="MeanAmpP3_Fz")
Long3 <-melt(EEGData_NoMow,
                           id.vars=2,
                           measure.vars=c(39:42),
                           variable.name="P3_Cz",
                           value.name="MeanAmpP3_Cz")
Long4 <-melt(EEGData_NoMow,
                           id.vars= c(2),
                           measure.vars=c(43:46),
                           variable.name="P2_Pz",
                           value.name="MeanAmpP2_Pz")
Long5 <-melt(EEGData_NoMow,
                           id.vars= c(2),
                           measure.vars=c(47:50),
                           variable.name="P2_Fz",
                           value.name="MeanAmpP2_Fz")
Long6 <-melt(EEGData_NoMow,
                           id.vars= c(2),
                           measure.vars=c(51:54),
                           variable.name="P2_Cz",
                           value.name="MeanAmpP2_Cz")
Long7 <-melt(EEGData_NoMow,
                           id.vars= c(2),
                           measure.vars=c(55:58),
                           variable.name="N2_Pz",
                           value.name="MeanAmpN2_Pz")
Long8 <-melt(EEGData_NoMow,
                           id.vars= c(2),
                           measure.vars=c(59:62),
                           variable.name="N2_Fz",
                           value.name="MeanAmpN2_Fz")
Long9 <-melt(EEGData_NoMow,
                           id.vars= c(2),
                           measure.vars=c(63:66),
                           variable.name="N2_Cz",
                           value.name="MeanAmpN2_Cz")
Long10 <-melt(EEGData_NoMow,
                           id.vars= c(2),
                           measure.vars=c(67:70),
                           variable.name="N2_FzCz",
                           value.name="MeanAmpN2_FzCz")
EEGData_NoMow_Long = data.frame(Long1, Long2, Long3, Long4, Long5, Long6, Long7, Long8, Long9, Long10)
EEGData_NoMow_Long <- EEGData_NoMow_Long[ -c(32,35, 38, 41, 44, 47, 50, 53, 56, 59) ]
# Plot #####
library(summarySE)
tgc <- summarySE(EEGData_NoMow_Long, measurevar="MeanAmpN2_Fz", groupvars=c("N2_Fz","SAPS_Med_Split"))
ggplot(EEGData_NoMow_Long, aes(x = N2_Fz, y = MeanAmpN2_Fz, color = SAPS_Med_Split, group = SAPS_Med_Split)) +
  geom_jitter(size=2, alpha=0.7, width = .3) +
  geom_errorbar(aes(ymin=mean(EEGData_NoMow_Long$MeanAmpN2_Fz)-sd(EEGData_NoMow_Long$MeanAmpN2_Fz), ymax=mean(EEGData_NoMow_Long$MeanAmpN2_Fz)+sd(EEGData_NoMow_Long$MeanAmpN2_Fz)), width=.1) +
  #stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="green", fill="green", alpha = .9) +
  # geom_stripchart(n.text.params=list(size = 5), location.scale.text.params=list(size =5), test.text.params=list(size = 5), 
  #                 jitter.params = list(size=0, width = 0, alpha = 0), line.params = list(), 
  #                 location.params = list(), point.params = list(color = "blank"), errorbar.params = list(),test.text = TRUE, color = "black") +
   #geom_boxplot(width= .7,weight= 1,alpha=0.8
               #position = position_dodge2(width = 20),
               # notch=TRUE
               # notchwidth = .5,
               # outlier.colour="red",
               # outlier.fill="red",
               # outlier.size=3,
               # outlier.alpha = .15
  #) +
  theme(legend.position="bottom") +
  geom_hline(yintercept = mean(EEGData_NoMow_Long$MeanAmpN2_Fz)) +
  labs(title = "ERP Mean Amplitude Predicted by Conditions for High vs Low Smartphone Addiction", x = "Condition", y = "Mean Amplitude (hz)", size = 12) +
  scale_y_continuous(breaks = seq(-5,15)) +
  theme(axis.text = element_text( 
    angle = 0, 
    color="blue", 
    size=10, 
    face=3)
  ) +
  theme(axis.title = element_text(angle = 0, color="black", size=15, face=3)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20))


# Anova ####
#####
# library(ez)
# install.packages("emmeans")
# library(emmeans)
# anova1 <- ezANOVA(EEGData_NoMow_NAomit, dv = Total_Oddball_PhSq_N2_Cz, wid = Part_ID, between = SAPS_Med_Split, type = 3, return_aov = TRUE)
# anova1
# emm <- emmeans(anova$aov, ~ SAPS_Med_Split)
# library(nlme)
# model = lme(Total_Oddball_PhSq_N2_Cz ~ SAPS_Med_Split, 
#             random = ~1|Part_ID,
#             data=EEGData_NoMow_NAomit,
#             method="REML")
# library(car)
# Anova(model)
# model.fixed = gls(Total_Oddball_PhSq_N2_Cz ~ SAPS_Med_Split,
#                   data=EEGData_NoMow_NAomit,
#                   method="REML")
# anova(model,
#       model.fixed)
Totaloddballeffects <- ddply(EEGData_NoMow_NAomit,
                                 summarise,
                                 Mean1 = mean(Total_Oddball_PhSq_P2_Fz),
                                 SD1 = sd(Total_Oddball_PhSq_P2_Fz),
                                 Mean2 = mean(Total_Oddball_PhSq_P2_Cz),
                                 SD2 = sd(Total_Oddball_PhSq_P2_Cz),
                                 Mean3 = mean(Total_Oddball_PhSq_P2_Pz),
                                 SD3 = sd(Total_Oddball_PhSq_P2_Pz),
                                 Mean4 = mean(Total_Oddball_PhSq_N2_Fz),
                                 SD4 = sd(Total_Oddball_PhSq_N2_Fz),
                                 Mean5 = mean(Total_Oddball_PhSq_N2_Cz),
                                 SD5 = sd(Total_Oddball_PhSq_N2_Cz),
                                 Mean6 = mean(Total_Oddball_PhSq_N2_Pz),
                                 SD6 = sd(Total_Oddball_PhSq_N2_Pz),
                                 Mean7 = mean(Total_Oddball_PhSq_N2_FzCz),
                                 SD7 = sd(Total_Oddball_PhSq_N2_FzCz),
                                 Mean8 = mean(Total_Oddball_PhSq_P3_Fz),
                                 SD8 = sd(Total_Oddball_PhSq_P3_Fz),
                                 Mean9 = mean(Total_Oddball_PhSq_P3_Cz),
                                 SD9 = sd(Total_Oddball_PhSq_P3_Cz),
                                 Mean10 = mean(Total_Oddball_PhSq_P3_Pz),
                                 SD10 = sd(Total_Oddball_PhSq_P3_Pz))
SAPSTotaloddballeffects

# library(lmerTest) ####
# summary(lmer(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_Cz ~ SAPS_Med_Split + (1|Part_ID), REML = F))
# ezANOVA(EEGData_NoMow_NAomit, Total_Oddball_PhSq_P2_Cz, Part_ID, between = SAPS_Med_Split)
# ezANOVA(EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_Fz, Part_ID, between = SAPS_Med_Split)
# ezANOVA(EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_Pz, Part_ID, between = SAPS_Med_Split)
# ezANOVA(EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_FzCz, Part_ID, between = SAPS_Med_Split)
# 
# ezANOVA(EEGData_NoMow_NAomit, ComAve_PhSq_N2_Cz, Part_ID, between = SAPS_Med_Split)
# ezANOVA(EEGData_NoMow_NAomit, RareAve_PhSq_N2_Cz, Part_ID, between = SAPS_Med_Split)
# 
# ezANOVA(EEGData_NoMow_NAomit, within = .(ComAve_PhSq_N2_Cz, RareAve_PhSq_N2_Cz), wid = Part_ID, between = SAPS_Med_Split)
# 
# contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split)
# ezANOVA(data = result,                # data frame being used
#         wid = Part_ID,                  # label for each subject
#         within = .(ERP),          # we can add within-subjects factors
#         dv = MeanAmp,                      # outcome variable
#         between = SAPS_Med_Split, type = 2)  
# result <- EEGData_NoMow %>% group_by(Part_ID) %>%
#   gather(ERP,MeanAmp, P3_ComSq_Pz,P3_RareSq_Pz,P3_ComPh_Pz,P3_RarePh_Pz)
# result$ERP <- factor(result$ERP,levels=c("P3_ComSq_Pz","P3_RareSq_Pz","P3_ComPh_Pz","P3_RarePh_Pz"))
# #options(contrasts="contr.treatment")
# modelAOV <- aov(MeanAmp ~ ERP * SAPS_Med_Split + Error(factor(Part_ID)),data = result)
# summary(modelAOV)
# result$ERP <- as.factor(result$ERP)
# contrasts(result$ERP)
# result$ERP <- relevel(result$ERP, ref="P3_ComPh_Pz")
# mod.P3_Pz <- lmer(data = result, MeanAmp ~ ERP * SAPS_Med_Split + (1|Part_ID), REML = F)
# summary(mod.P3_Pz)

# SoundOddball * SAPS ####
control <- lmerControl(check.nobs.vs.nRE="ignore", calc.derivs=FALSE)
# Descriptives ####
# Phone oddball
SAPSoddballeffectsPhone <- ddply(EEGData_NoMow_NAomit,
                 .(SAPS_Med_Split),
                 summarise,
                 P2_Fz_Mean1 = mean(phoddball_P2_Fz),
                 P2_Fz_SD1 = sd(phoddball_P2_Fz),
                 P2_Cz_Mean2 = mean(phoddball_P2_Cz),
                 P2_Cz_SD2 = sd(phoddball_P2_Cz),
                 P2_Pz_Mean3 = mean(phoddball_P2_Pz),
                 P2_Pz_SD3 = sd(phoddball_P2_Pz),
                 N2_Fz_Mean4 = mean(phoddball_N2_Fz),
                 N2_Fz_SD4 = sd(phoddball_N2_Fz),
                 N2_Cz_Mean5 = mean(phoddball_N2_Cz),
                 N2_Cz_SD5 = sd(phoddball_N2_Cz),
                 N2_Pz_Mean6 = mean(phoddball_N2_Pz),
                 N2_Pz_SD6 = sd(phoddball_N2_Pz),
                 N2_FzCz_Mean7 = mean(phoddball_N2_FzCz),
                 N2_FzCz_SD7 = sd(phoddball_N2_FzCz),
                 N2_F3_Mean11 = mean(phoddball_N2_F3),
                 N2_F3_SD11 = sd(phoddball_N2_F3),
                 P3_Fz_Mean8 = mean(phoddball_P3_Fz),
                 P3_Fz_SD8 = sd(phoddball_P3_Fz),
                 P3_Cz_Mean9 = mean(phoddball_P3_Cz),
                 P3_Cz_SD9 = sd(phoddball_P3_Cz),
                 P3_Pz_Mean10 = mean(phoddball_P3_Pz),
                 P3_Pz_SD10 = sd(phoddball_P3_Pz))
SAPSoddballeffectsPhone
# Control sound oddball
SAPSoddballeffectsControl <- ddply(EEGData_NoMow_NAomit,
                            .(SAPS_Med_Split),
                            summarise,
                            P2_Fz_Mean1 = mean(sqoddball_P2_Fz),
                            P2_Fz_SD1 = sd(sqoddball_P2_Fz),
                            P2_Cz_Mean2 = mean(sqoddball_P2_Cz),
                            P2_Cz_SD2 = sd(sqoddball_P2_Cz),
                            P2_Pz_Mean3 = mean(sqoddball_P2_Pz),
                            P2_Pz_SD3 = sd(sqoddball_P2_Pz),
                            N2_Fz_Mean4 = mean(sqoddball_N2_Fz),
                            N2_Fz_SD4 = sd(sqoddball_N2_Fz),
                            N2_Cz_Mean5 = mean(sqoddball_N2_Cz),
                            N2_Cz_SD5 = sd(sqoddball_N2_Cz),
                            N2_Pz_Mean6 = mean(sqoddball_N2_Pz),
                            N2_Pz_SD6 = sd(sqoddball_N2_Pz),
                            N2_FzCz_Mean7 = mean(sqoddball_N2_FzCz),
                            N2_FzCz_SD7 = sd(sqoddball_N2_FzCz),
                            N2_F3_Mean11 = mean(sqoddball_N2_F3),
                            N2_F3_SD11 = sd(sqoddball_N2_F3),
                            P3_Fz_Mean8 = mean(sqoddball_P3_Fz),
                            P3_Fz_SD8 = sd(sqoddball_P3_Fz),
                            P3_Cz_Mean9 = mean(sqoddball_P3_Cz),
                            P3_Cz_SD9 = sd(sqoddball_P3_Cz),
                            P3_Pz_Mean10 = mean(sqoddball_P3_Pz),
                            P3_Pz_SD10 = sd(sqoddball_P3_Pz))
SAPSoddballeffectsControl
# Phone-Control sound oddball effect diff
SoundOddballDiffSAPS <- SAPSoddballeffectsPhone-SAPSoddballeffectsControl
SoundOddballDiffSAPS$SAPS_Med_Split <- c("High", "Low")

# P2Cz SAPS med split ####
summary(lm(data = EEGData_NoMow, SAPS_MedSplit_Coded ~ phoddball_P2_Cz*sqoddball_P2_Cz, na.rm = T))

oddballP2_Cz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_P2_Cz, sqoddball_P2_Cz, factor_key = T) 
oddballP2_Cz$Oddball <- factor(oddballP2_Cz$Oddball,levels=c("phoddball_P2_Cz","sqoddball_P2_Cz"))
mod.P2_Cz_SAPS <- lmer(data = oddballP2_Cz, MeanAmp ~ Oddball * SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.P2_Cz_SAPS)
str(oddballP2_Cz$Oddball)
contrasts(oddballP2_Cz$Oddball) = contr.treatment(2)
contrasts(oddballP2_Cz$Oddball)
contrasts(oddballP2_Cz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballP2_Cz$SAPS_Med_Split)
mod.P2_Cz_Oddball_SAPS <- lm(data = oddballP2_Cz, MeanAmp ~ Oddball * SAPS_Med_Split)
summary(mod.P2_Cz_Oddball_SAPS)
oddballP2_Cz$SAPS_Med_Split <- relevel(oddballP2_Cz$SAPS_Med_Split, ref="Low")
oddballP2_Cz$Oddball <- relevel(oddballP2_Cz$Oddball, ref="phoddball_P2_Cz")
oddballP2_Cz$SAPS_Med_Split <- relevel(oddballP2_Cz$SAPS_Med_Split, ref="High")

mod.P2_Cz <- lmer(data = oddballP2_Cz, MeanAmp ~ Oddball + (1|Part_ID), REML = F)
summary(mod.P2_Cz)
contrasts(oddballP2_Cz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballP2_Cz$SAPS_Med_Split)
mod.P2_Cz_SAPS <- lmer(data = oddballP2_Cz, MeanAmp ~ SAPS_Med_Split  + (1|Part_ID), REML = F)
summary(mod.P2_Cz_SAPS)
describe(EEGData_NoMow$phoddball_P2_Cz)
describe(EEGData_NoMow$sqoddball_P2_Cz)
describeBy(EEGData_NoMow$phoddball_P2_Cz, EEGData_NoMow$SAPS_Med_Split)

# P2Cz SAP high low thirds #####

oddballP2_Cz <- EEGData_NoMow_SAPS3 %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_P2_Cz, sqoddball_P2_Cz, factor_key = T) 
oddballP2_Cz$Oddball <- factor(oddballP2_Cz$Oddball,levels=c("phoddball_P2_Cz","sqoddball_P2_Cz"))
str(oddballP2_Cz$Oddball)
contrasts(oddballP2_Cz$Oddball) = contr.treatment(2)
contrasts(oddballP2_Cz$Oddball)
contrasts(oddballP2_Cz$SAPS_thirds) = contr.treatment(3)
contrasts(oddballP2_Cz$SAPS_thirds)
levels(oddballP2_Cz$SAPS_thirds)
mod.P2_Cz_Oddball_SAPS <- lmer(data = oddballP2_Cz, MeanAmp ~ Oddball * SAPS_thirds + (1|Part_ID), REML = F)
summary(mod.P2_Cz_Oddball_SAPS)
#oddballP2_Cz$SAPS_thirds <- relevel(oddballP2_Cz$SAPS_thirds, ref="Low Third")
mod.P2_Cz <- lmer(data = oddballP2_Cz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.P2_Cz)
contrasts(oddballP2_Cz$SAPS_thirds) = contr.treatment(2)
contrasts(oddballP2_Cz$SAPS_thirds)
mod.P2_Cz_SAPS <- lmer(data = oddballP2_Cz, MeanAmp ~ SAPS_thirds  + (1|Part_ID), REML = F)
summary(mod.P2_Cz_SAPS)
describe(EEGData_NoMow_SAPS3$phoddball_P2_Cz)
describe(EEGData_NoMow_SAPS3$sqoddball_P2_Cz)
describeBy(EEGData_NoMow_SAPS3$phoddball_P2_Cz, EEGData_NoMow_SAPS3$SAPS_Med_Split)
# P2Fz SAPS med split #####
summary(lm(data = EEGData_NoMow_NAomit, SAPS.c ~ phoddball_P2_Fz*sqoddball_P2_Fz, na.rm = T))
control <- lmerControl(check.nobs.vs.nRE="ignore", calc.derivs=FALSE)

oddballP2_Fz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_P2_Fz, sqoddball_P2_Fz, factor_key = T) 
oddballP2_Fz$Oddball <- factor(oddballP2_Fz$Oddball,levels = c("phoddball_P2_Fz","sqoddball_P2_Fz"), labels = c("PhOddball_P2_Fz", "ContOddball_P2_Fz"))
contrasts(oddballP2_Fz$Oddball)
contrasts(oddballP2_Fz$Oddball) <- varContrasts(oddballP2_Fz$Oddball, 
                                            Type = "POC", 
                                            POCList = list(c(.5, -.5)),
                                            Labels = c("PhOddball_P2_Fz", "ContOddball_P2_Fz"))
mod.P2_Fz_Oddball_SAPS <- lmer(data = oddballP2_Fz, MeanAmp ~ Oddball * SAPS.c + (1|Part_ID), REML = F, control = control)
#mod.P2_Fz_Oddball_SAPS2 <- lmer(data = oddballP2_Fz, MeanAmp ~ Oddball * SAPS.c + (Oddball|Part_ID), REML = F, control = control)
#anova(mod.P2_Fz_Oddball_SAPS,mod.P2_Fz_Oddball_SAPS2)
summary(mod.P2_Fz_Oddball_SAPS) 
set.seed(69)
confint(mod.P2_Fz_Oddball_SAPS)
confint(mod.P2_Fz_Oddball_SAPS, method = "boot")
fixef(mod.P2_Fz_Oddball_SAPS)
describeBy(oddballP2_Fz$sqoddball_P2_Fz, oddballP2_Fz$SAPS_Med_Split)
cohen.d(oddballP2_Fz$MeanAmp, oddballP2_Fz$SAPS_Med_Split)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.P2_Fz_Oddball_SAPS))
effectsize::t_to_d(param_tab$t[4], param_tab$df[4], pooled = FALSE)
confint(mod.P2_Fz_Oddball_SAPS)
EEGData_NoMow_NAomit <- EEGData_NoMow_NAomit %>% 
  mutate(oddballDiff_P2_Fz = phoddball_P2_Fz - sqoddball_P2_Fz) 
 describeBy(EEGData_NoMow_NAomit$oddballDiff_P2_Fz, EEGData_NoMow_NAomit$SAPS_Med_Split)

str(oddballP2_Fz$Oddball)
contrasts(oddballP2_Fz$Oddball) = contr.treatment(2)
contrasts(oddballP2_Fz$Oddball)
contrasts(oddballP2_Fz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballP2_Fz$SAPS_Med_Split)
modelEffectSizes(mod.P2_Fz_Oddball_SAPS)
oddballP2_Fz$SAPS_Med_Split <- relevel(oddballP2_Fz$SAPS_Med_Split, ref="Low")
oddballP2_Fz$Oddball <- relevel(oddballP2_Fz$Oddball, ref="phoddball_P2_Fz")
mod.P2_Fz <- lmer(data = oddballP2_Fz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.P2_Fz)
contrasts(oddballP2_Fz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballP2_Fz$SAPS_Med_Split)
mod.P2_Fz_SAPS <- lmer(data = oddballP2_Fz, MeanAmp ~ SAPS_Med_Split  + (1|Part_ID), REML = F)
summary(mod.P2_Fz_SAPS)
describe(EEGData_NoMow$phoddball_P2_Fz)
describe(EEGData_NoMow$sqoddball_P2_Fz)
describeBy(oddballP2_Fz$phoddball_P2_Fz, oddballP2_Fz$SAPS_Med_Split)
describeBy(EEGData_NoMow$sqoddball_P2_Fz, EEGData_NoMow$SAPS_Med_Split)

# P2Fz SAP high low thirds #####
oddballP2_Fz <- EEGData_NoMow_SAPS3 %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_P2_Fz, sqoddball_P2_Fz, factor_key = T) 
oddballP2_Fz$Oddball <- factor(oddballP2_Fz$Oddball,levels=c("phoddball_P2_Fz","sqoddball_P2_Fz"))
str(oddballP2_Fz$Oddball)
contrasts(oddballP2_Fz$Oddball) = contr.treatment(2)
contrasts(oddballP2_Fz$Oddball)
contrasts(oddballP2_Fz$SAPS_thirds) = contr.treatment(3)
contrasts(oddballP2_Fz$SAPS_thirds)
levels(oddballP2_Fz$SAPS_thirds)
mod.P2_Fz_Oddball_SAPS <- lmer(data = oddballP2_Fz, MeanAmp ~ Oddball * SAPS_thirds + (1|Part_ID), REML = F)
summary(mod.P2_Fz_Oddball_SAPS)
#oddballP2_Fz$SAPS_thirds <- relevel(oddballP2_Fz$SAPS_thirds, ref="Low Third")
mod.P2_Fz <- lmer(data = oddballP2_Fz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.P2_Fz)
contrasts(oddballP2_Fz$SAPS_thirds) = contr.treatment(2)
contrasts(oddballP2_Fz$SAPS_thirds)
mod.P2_Fz_SAPS <- lmer(data = oddballP2_Fz, MeanAmp ~ SAPS_thirds  + (1|Part_ID), REML = F)
summary(mod.P2_Fz_SAPS)
describe(EEGData_NoMow$phoddball_P2_Fz)
describe(EEGData_NoMow$sqoddball_P2_Fz)
describeBy(EEGData_NoMow$phoddball_P2_Fz, EEGData_NoMow$SAPS_Med_Split)

# P2Pz SAPS med split ####
summary(lm(data = EEGData_NoMow, SAPS_MedSplit_Coded ~ phoddball_P2_Pz*sqoddball_P2_Pz, na.rm = T))

oddballP2_Pz  <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_P2_Pz, sqoddball_P2_Pz, factor_key = T) 
oddballP2_Pz$Oddball <- factor(oddballP2_Pz$Oddball,levels=c("phoddball_P2_Pz","sqoddball_P2_Pz"))
mod.P2_Pz_SAPS <- lmer(data = oddballP2_Pz, MeanAmp ~ Oddball * SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.P2_Pz_SAPS)
str(oddballP2_Pz$Oddball)
contrasts(oddballP2_Pz$Oddball) = contr.treatment(2)
contrasts(oddballP2_Pz$Oddball)
contrasts(oddballP2_Pz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballP2_Pz$SAPS_Med_Split)
mod.P2_Pz_Oddball_SAPS <- lm(data = oddballP2_Pz, MeanAmp ~ Oddball * SAPS_Med_Split)
summary(mod.P2_Pz_Oddball_SAPS)
oddballP2_Pz$SAPS_Med_Split <- relevel(oddballP2_Pz$SAPS_Med_Split, ref="High")
oddballP2_Pz$Oddball <- relevel(oddballP2_Pz$Oddball, ref="phoddball_P2_Pz")

mod.P2_Pz <- lmer(data = oddballP2_Pz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.P2_Pz)
contrasts(oddballP2_Pz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballP2_Pz$SAPS_Med_Split)

describe(EEGData_NoMow$phoddball_P2_Pz)
describe(EEGData_NoMow$sqoddball_P2_Pz)
describeBy(EEGData_NoMow$phoddball_P2_Pz, EEGData_NoMow$SAPS_Med_Split)

# N2Cz SAPS med split ####
summary(lm(data = EEGData_NoMow, SAPS_MedSplit_Coded ~ phoddball_N2_Cz*sqoddball_N2_Cz, na.rm = T))

oddballN2_Cz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_N2_Cz, sqoddball_N2_Cz, factor_key = T) 
oddballN2_Cz$Oddball <- factor(oddballN2_Cz$Oddball,levels=c("phoddball_N2_Cz","sqoddball_N2_Cz"))
mod.N2_Cz_Oddball_SAPS <- lmer(data = oddballN2_Cz, MeanAmp ~ Oddball * SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.N2_Cz_Oddball_SAPS)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.N2_Cz_Oddball_SAPS))
effectsize::t_to_d(param_tab$t[4], param_tab$df[4], pooled = FALSE)
str(oddballN2_Cz$Oddball)
contrasts(oddballN2_Cz$Oddball) = contr.treatment(2)
contrasts(oddballN2_Cz$Oddball)
contrasts(oddballN2_Cz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballN2_Cz$SAPS_Med_Split)

oddballN2_Cz$SAPS_Med_Split <- relevel(oddballN2_Cz$SAPS_Med_Split, ref="Low")
oddballN2_Cz$Oddball <- relevel(oddballN2_Cz$Oddball, ref="sqoddball_N2_Cz")
oddballN2_Cz$Oddball <- relevel(oddballN2_Cz$Oddball, ref="phoddball_N2_Cz")
mod.N2_Cz <- lmer(data = oddballN2_Cz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.N2_Cz)
contrasts(oddballN2_Cz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballN2_Cz$SAPS_Med_Split)
mod.N2_Cz_SAPS <- lmer(data = oddballN2_Cz, MeanAmp ~ SAPS_Med_Split  + (1|Part_ID), REML = F)
summary(mod.N2_Cz_SAPS)
describe(EEGData_NoMow$phoddball_N2_Cz)
describe(EEGData_NoMow$sqoddball_N2_Cz)
describeBy(EEGData_NoMow$phoddball_N2_Cz, EEGData_NoMow$SAPS_Med_Split)
describeBy(EEGData_NoMow$sqoddball_N2_Cz, EEGData_NoMow$SAPS_Med_Split)

# N2Cz SAP high low thirds #####
oddballN2_Cz <- EEGData_NoMow_SAPS3 %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_N2_Cz, sqoddball_N2_Cz, factor_key = T) 
oddballN2_Cz$Oddball <- factor(oddballN2_Cz$Oddball,levels=c("phoddball_N2_Cz","sqoddball_N2_Cz"))
str(oddballN2_Cz$Oddball)
contrasts(oddballN2_Cz$Oddball) = contr.treatment(2)
contrasts(oddballN2_Cz$Oddball)
contrasts(oddballN2_Cz$SAPS_thirds) = contr.treatment(3)
contrasts(oddballN2_Cz$SAPS_thirds)
levels(oddballN2_Cz$SAPS_thirds)
mod.N2_Cz_Oddball_SAPS <- lmer(data = oddballN2_Cz, MeanAmp ~ Oddball * SAPS_thirds + (1|Part_ID), REML = F)
summary(mod.N2_Cz_Oddball_SAPS)
#oddballN2_Cz$SAPS_thirds <- relevel(oddballN2_Cz$SAPS_thirds, ref="Low Third")
mod.N2_Cz <- lmer(data = oddballN2_Cz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.N2_Cz)
contrasts(oddballN2_Cz$SAPS_thirds) = contr.treatment(2)
contrasts(oddballN2_Cz$SAPS_thirds)
mod.N2_Cz_SAPS <- lmer(data = oddballN2_Cz, MeanAmp ~ SAPS_thirds  + (1|Part_ID), REML = F)
summary(mod.N2_Cz_SAPS)
describe(EEGData_NoMow_SAPS3$phoddball_N2_Cz)
describe(EEGData_NoMow_SAPS3$sqoddball_N2_Cz)
describeBy(EEGData_NoMow_SAPS3$phoddball_N2_Cz, EEGData_NoMow_SAPS3$SAPS_Med_Split)
# N2Fz SAPS med split #####
oddballN2_Fz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_N2_Fz, sqoddball_N2_Fz, factor_key = T) 
oddballN2_Fz$Oddball <- factor(oddballN2_Fz$Oddball,levels=c("phoddball_N2_Fz","sqoddball_N2_Fz"))
mod.N2_Fz_Oddball_SAPS <- lmer(data = oddballN2_Fz, MeanAmp ~ Oddball * SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.N2_Fz_Oddball_SAPS)
str(oddballN2_Fz$Oddball)
contrasts(oddballN2_Fz$Oddball) = contr.treatment(2)
contrasts(oddballN2_Fz$Oddball)
contrasts(oddballN2_Fz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballN2_Fz$SAPS_Med_Split)

modelEffectSizes(mod.N2_Fz_Oddball_SAPS)

oddballN2_Fz$SAPS_Med_Split <- relevel(oddballN2_Fz$SAPS_Med_Split, ref="Low")
mod.N2_Fz <- lmer(data = oddballN2_Fz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.N2_Fz)
oddballN2_Fz$Oddball <- relevel(oddballN2_Fz$Oddball, ref="phoddball_N2_Fz")
contrasts(oddballN2_Fz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballN2_Fz$SAPS_Med_Split)
mod.N2_Fz_SAPS <- lmer(data = oddballN2_Fz, MeanAmp ~ SAPS_Med_Split  + (1|Part_ID), REML = F)
summary(mod.N2_Fz_SAPS)
describe(EEGData_NoMow$phoddball_N2_Fz)
describe(EEGData_NoMow$sqoddball_N2_Fz)
describeBy(EEGData_NoMow$phoddball_N2_Fz, EEGData_NoMow$SAPS_Med_Split)

# N2Fz SAP high low thirds #####
oddballN2_Fz <- EEGData_NoMow_SAPS3 %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_N2_Fz, sqoddball_N2_Fz, factor_key = T) 
oddballN2_Fz$Oddball <- factor(oddballN2_Fz$Oddball,levels=c("phoddball_N2_Fz","sqoddball_N2_Fz"))
str(oddballN2_Fz$Oddball)
contrasts(oddballN2_Fz$Oddball) = contr.treatment(2)
contrasts(oddballN2_Fz$Oddball)
contrasts(oddballN2_Fz$SAPS_thirds) = contr.treatment(3)
contrasts(oddballN2_Fz$SAPS_thirds)
levels(oddballN2_Fz$SAPS_thirds)
mod.N2_Fz_Oddball_SAPS <- lm(data = oddballN2_Fz, MeanAmp ~ Oddball * SAPS_thirds)
summary(mod.N2_Fz_Oddball_SAPS)
#oddballN2_Fz$SAPS_thirds <- relevel(oddballN2_Fz$SAPS_thirds, ref="Low Third")
mod.N2_Fz <- lmer(data = oddballN2_Fz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.N2_Fz)
contrasts(oddballN2_Fz$SAPS_thirds) = contr.treatment(2)
contrasts(oddballN2_Fz$SAPS_thirds)
mod.N2_Fz_SAPS <- lmer(data = oddballN2_Fz, MeanAmp ~ SAPS_thirds  + (1|Part_ID), REML = F)
summary(mod.N2_Fz_SAPS)

# N2F3 SAPS med split #####
oddballN2_F3 <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_N2_F3, sqoddball_N2_F3, factor_key = T) 
oddballN2_F3$Oddball <- factor(oddballN2_F3$Oddball,levels=c("phoddball_N2_F3","sqoddball_N2_F3"), labels = c("PhOddball_N2_F3", "ContOddball_N2_F3"))
contrasts(oddballN2_F3$Oddball)
contrasts(oddballN2_F3$Oddball) <- varContrasts(oddballN2_F3$Oddball, 
                                                Type = "POC", 
                                                POCList = list(c(.5, -.5)),
                                                Labels = c("PhOddball_N2_F3", "ContOddball_N2_F3"))
mod.N2_F3_Oddball_SAPS <- lmer(data = oddballN2_F3, MeanAmp ~ Oddball * SAPS.c + (1|Part_ID), REML = F, control = control)
summary(mod.N2_F3_Oddball_SAPS)
set.seed(69)
confint(mod.N2_F3_Oddball_SAPS, method = "boot")
confint(mod.N2_F3_Oddball_SAPS, method = "boot")
str(oddballN2_F3$Oddball)
contrasts(oddballN2_F3$Oddball) = contr.treatment(2)
contrasts(oddballN2_F3$Oddball)
contrasts(oddballN2_F3$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballN2_F3$SAPS_Med_Split)
confint(mod.N2_F3_Oddball_SAPS)

EEGData_NoMow_NAomit <- EEGData_NoMow_NAomit %>% 
  mutate(oddballDiff_N2_F3 = phoddball_N2_F3 - sqoddball_N2_F3) 
describeBy(EEGData_NoMow_NAomit$oddballDiff_N2_F3, EEGData_NoMow_NAomit$SAPS_Med_Split)

oddballN2_F3$SAPS_Med_Split <- relevel(oddballN2_F3$SAPS_Med_Split, ref="Low")
mod.N2_F3 <- lmer(data = oddballN2_F3, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.N2_F3)
oddballN2_F3$Oddball <- relevel(oddballN2_F3$Oddball, ref="phoddball_N2_F3")
contrasts(oddballN2_F3$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballN2_F3$Oddball)
mod.N2_F3_SAPS <- lmer(data = oddballN2_F3, MeanAmp ~ SAPS_Med_Split  + (1|Part_ID), REML = F)
summary(mod.N2_F3_SAPS)

oddballN2_F3$SAPS_Med_Split <- relevel(oddballN2_F3$SAPS_Med_Split, ref="High")
mod.N2_F3 <- lmer(data = oddballN2_F3, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.N2_F3)
oddballN2_F3$Oddball <- relevel(oddballN2_F3$Oddball, ref="sqoddball_N2_F3")
contrasts(oddballN2_F3$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballN2_F3$SAPS_Med_Split)
mod.N2_F3_SAPS <- lmer(data = oddballN2_F3, MeanAmp ~ SAPS_Med_Split  + (1|Part_ID), REML = F)
summary(mod.N2_F3_SAPS)

describe(EEGData_NoMow$phoddball_N2_F3)
describe(EEGData_NoMow$sqoddball_N2_F3)
describeBy(EEGData_NoMow$phoddball_N2_F3, EEGData_NoMow$SAPS_Med_Split)
# N2FzCz SAPS med SPlit ####
summary(lm(data = EEGData_NoMow, SAPS_MedSplit_Coded ~ phoddball_N2_FzCz*sqoddball_N2_FzCz, na.rm = T))
contrasts(EEGData_NoMow$SAPS_MedSplit_Coded)

oddballN2_FzCz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_N2_FzCz, sqoddball_N2_FzCz) 
oddballN2_FzCz$Oddball <- factor(oddballN2_FzCz$Oddball,levels=c("phoddball_N2_FzCz","sqoddball_N2_FzCz"))
mod.N2_FzCz_Oddball_SAPS <- lmer(data = oddballN2_FzCz, MeanAmp ~ Oddball * SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.N2_FzCz_Oddball_SAPS)
str(oddballN2_FzCz$Oddball)
contrasts(oddballN2_FzCz$Oddball) = contr.treatment(2)
contrasts(oddballN2_FzCz$Oddball)
contrasts(oddballN2_FzCz$SAPS_Med_Split) = contr.sum(2)
contrasts(oddballN2_FzCz$SAPS_Med_Split)
contrasts(oddballN2_FzCz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballN2_FzCz$SAPS_Med_Split)

oddballN2_FzCz$Oddball <- relevel(oddballN2_FzCz$Oddball, ref="sqoddball_N2_FzCz")

oddballN2_FzCz$SAPS_Med_Split <- relevel(oddballN2_FzCz$SAPS_Med_Split, ref="Low")
mod.N2_FzCz <- lmer(data = oddballN2_FzCz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.N2_FzCz)
mod.N2_FzCz_SAPS <- lmer(data = oddballN2_FzCz, MeanAmp ~ SAPS_Med_Split  + (1|Part_ID), REML = F)
summary(mod.N2_FzCz_SAPS)
describe(EEGData_NoMow$phoddball_N2_FzCz)
describe(EEGData_NoMow$sqoddball_N2_FzCz)
describeBy(EEGData_NoMow$phoddball_N2_FzCz, EEGData_NoMow$SAPS_Med_Split)

# N2Pz SAPS med split ####
summary(lm(data = EEGData_NoMow, SAPS_MedSplit_Coded ~ phoddball_N2_Cz*sqoddball_N2_Cz, na.rm = T))

oddballN2_Pz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_N2_Pz, sqoddball_N2_Pz, factor_key = T) 
oddballN2_Pz$Oddball <- factor(oddballN2_Pz$Oddball,levels=c("phoddball_N2_Pz","sqoddball_N2_Pz"))
mod.N2_Pz_Oddball_SAPS <- lmer(data = oddballN2_Pz, MeanAmp ~ Oddball * SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.N2_Pz_Oddball_SAPS)
str(oddballN2_Pz$Oddball)
contrasts(oddballN2_Pz$Oddball) = contr.treatment(2)
contrasts(oddballN2_Pz$Oddball)
contrasts(oddballN2_Pz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballN2_Pz$SAPS_Med_Split)

oddballN2_Pz$SAPS_Med_Split <- relevel(oddballN2_Pz$SAPS_Med_Split, ref="Low")
oddballN2_Pz$Oddball <- relevel(oddballN2_Pz$Oddball, ref="phoddball_N2_Pz")

summary(mod.N2_Pz_Oddball_SAPS)$coefficients
# They are in the 4th column
summary(mod.N2_Pz_Oddball_SAPS)$coefficients[, 4]
ps <- summary(mod.N2_Pz_Oddball_SAPS)$coefficients[2:4, 4] # We don't need the p-value for the intercept
# Pass the p-values to p.adjust
p.adjust(ps, "holm")
p.adjust(ps, "bonferroni")

mod.N2_Pz <- lmer(data = oddballN2_Pz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.N2_Pz)
contrasts(oddballN2_Pz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballN2_Pz$SAPS_Med_Split)
mod.N2_Pz_SAPS <- lmer(data = oddballN2_Pz, MeanAmp ~ SAPS_Med_Split  + (1|Part_ID), REML = F)
summary(mod.N2_Pz_SAPS)
describe(EEGData_NoMow$phoddball_N2_Pz)
describe(EEGData_NoMow$sqoddball_N2_Pz)


# P3Cz SAPS med split ####
summary(lm(data = EEGData_NoMow, SAPS_MedSplit_Coded ~ phoddball_P3_Cz*sqoddball_P3_Cz, na.rm = T))

oddballP3_Cz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_P3_Cz, sqoddball_P3_Cz, factor_key = T) 
oddballP3_Cz$Oddball <- factor(oddballP3_Cz$Oddball,levels=c("phoddball_P3_Cz","sqoddball_P3_Cz"))
mod.P3_Cz_SAPS <- lmer(data = oddballP3_Cz, MeanAmp ~ Oddball * SAPS_Med_Split  + (1|Part_ID), REML = F)
summary(mod.P3_Cz_SAPS) 
str(oddballP3_Cz$Oddball)
contrasts(oddballP3_Cz$Oddball) = contr.treatment(2)
contrasts(oddballP3_Cz$Oddball)
contrasts(oddballP3_Cz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballP3_Cz$SAPS_Med_Split)
mod.P3_Cz_Oddball_SAPS <- lm(data = oddballP3_Cz, MeanAmp ~ Oddball * SAPS_Med_Split)
summary(mod.P3_Cz_Oddball_SAPS)
oddballP3_Cz$SAPS_Med_Split <- relevel(oddballP3_Cz$SAPS_Med_Split, ref="Low")
oddballP3_Cz$Oddball <- relevel(oddballP3_Cz$Oddball, ref="sqoddball_P3_Cz")
mod.P3_Cz <- lmer(data = oddballP3_Cz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.P3_Cz)
contrasts(oddballP3_Cz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballP3_Cz$SAPS_Med_Split)

describe(EEGData_NoMow$phoddball_P3_Cz)
describe(EEGData_NoMow$sqoddball_P3_Cz)
describeBy(EEGData_NoMow$phoddball_P3_Cz, EEGData_NoMow$SAPS_Med_Split)

# P3Cz SAP high low thirds #####

oddballP3_Cz <- EEGData_NoMow_SAPS3 %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_P3_Cz, sqoddball_P3_Cz, factor_key = T) 
oddballP3_Cz$Oddball <- factor(oddballP3_Cz$Oddball,levels=c("phoddball_P3_Cz","sqoddball_P3_Cz"))
mod.P3_Cz_Oddball_SAPS <- lmer(data = oddballP3_Cz, MeanAmp ~ Oddball * SAPS_thirds + (1|Part_ID), REML = F)
summary(mod.P3_Cz_Oddball_SAPS)
str(oddballP3_Cz$Oddball)
contrasts(oddballP3_Cz$Oddball) = contr.treatment(2)
contrasts(oddballP3_Cz$Oddball)
contrasts(oddballP3_Cz$SAPS_thirds) = contr.treatment(3)
contrasts(oddballP3_Cz$SAPS_thirds)
levels(oddballP3_Cz$SAPS_thirds)

#oddballP3_Cz$SAPS_thirds <- relevel(oddballP3_Cz$SAPS_thirds, ref="Low Third")
mod.P3_Cz <- lmer(data = oddballP3_Cz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.P3_Cz)
contrasts(oddballP3_Cz$SAPS_thirds) = contr.treatment(2)
contrasts(oddballP3_Cz$SAPS_thirds)
mod.P3_Cz_SAPS <- lmer(data = oddballP3_Cz, MeanAmp ~ SAPS_thirds  + (1|Part_ID), REML = F)
summary(mod.P3_Cz_SAPS)
describe(EEGData_NoMow_SAPS3$phoddball_P3_Cz)
describe(EEGData_NoMow_SAPS3$sqoddball_P3_Cz)
describeBy(EEGData_NoMow_SAPS3$phoddball_P3_Cz, EEGData_NoMow_SAPS3$SAPS_Med_Split)
# P3Fz SAPS med split #####
summary(lm(data = EEGData_NoMow, SAPS_MedSplit_Coded ~ phoddball_P3_Fz*sqoddball_P3_Fz, na.rm = T))

oddballP3_Fz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_P3_Fz, sqoddball_P3_Fz, factor_key = T) 
oddballP3_Fz$Oddball <- factor(oddballP3_Fz$Oddball,levels=c("phoddball_P3_Fz","sqoddball_P3_Fz"))
mod.P3_Fz_SAPS <- lmer(data = oddballP3_Fz, MeanAmp ~ Oddball * SAPS_Med_Split + (1|Part_ID), REML = F)
summary(mod.P3_Fz_SAPS)
str(oddballP3_Fz$Oddball)
contrasts(oddballP3_Fz$Oddball) = contr.treatment(2)
contrasts(oddballP3_Fz$Oddball)
contrasts(oddballP3_Fz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballP3_Fz$SAPS_Med_Split)
mod.P3_Fz_Oddball_SAPS <- lm(data = oddballP3_Fz, MeanAmp ~ Oddball * SAPS_Med_Split)
summary(mod.P3_Fz_Oddball_SAPS)
oddballP3_Fz$SAPS_Med_Split <- relevel(oddballP3_Fz$SAPS_Med_Split, ref="Low")
oddballP3_Fz$Oddball <- relevel(oddballP3_Fz$Oddball, ref="phoddball_P3_Fz")
mod.P3_Fz <- lmer(data = oddballP3_Fz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.P3_Fz)
contrasts(oddballP3_Fz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballP3_Fz$SAPS_Med_Split)

describe(EEGData_NoMow$phoddball_P3_Fz)
describe(EEGData_NoMow$sqoddball_P3_Fz)
describeBy(EEGData_NoMow$phoddball_P3_Fz, EEGData_NoMow$SAPS_Med_Split)
describeBy(EEGData_NoMow$sqoddball_P3_Fz, EEGData_NoMow$SAPS_Med_Split)

# P3Fz SAP high low thirds #####
oddballP3_Fz <- EEGData_NoMow_SAPS3 %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_P3_Fz, sqoddball_P3_Fz, factor_key = T) 
oddballP3_Fz$Oddball <- factor(oddballP3_Fz$Oddball,levels=c("phoddball_P3_Fz","sqoddball_P3_Fz"))
str(oddballP3_Fz$Oddball)
contrasts(oddballP3_Fz$Oddball) = contr.treatment(2)
contrasts(oddballP3_Fz$Oddball)
contrasts(oddballP3_Fz$SAPS_thirds) = contr.sum(3)
contrasts(oddballP3_Fz$SAPS_thirds)
levels(oddballP3_Fz$SAPS_thirds)
mod.P3_Fz_Oddball_SAPS <- lmer(data = oddballP3_Fz, MeanAmp ~ Oddball * SAPS_thirds + (1|Part_ID), REML = F)
summary(mod.P3_Fz_Oddball_SAPS)
#oddballP3_Fz$SAPS_thirds <- relevel(oddballP3_Fz$SAPS_thirds, ref="Low Third")
mod.P3_Fz <- lmer(data = oddballP3_Fz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.P3_Fz)
contrasts(oddballP3_Fz$SAPS_thirds) = contr.treatment(2)
contrasts(oddballP3_Fz$SAPS_thirds)
mod.P3_Fz_SAPS <- lmer(data = oddballP3_Fz, MeanAmp ~ SAPS_thirds  + (1|Part_ID), REML = F)
summary(mod.P3_Fz_SAPS)
describe(EEGData_NoMow$phoddball_P3_Fz)
describe(EEGData_NoMow$sqoddball_P3_Fz)
describeBy(EEGData_NoMow$phoddball_P3_Fz, EEGData_NoMow$SAPS_Med_Split)

# P3Pz SAPS med split ####
summary(lm(data = EEGData_NoMow, SAPS_MedSplit_Coded ~ phoddball_P3_Pz*sqoddball_P3_Pz, na.rm = T))

oddballP3_Pz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Oddball,MeanAmp, phoddball_P3_Pz, sqoddball_P3_Pz, factor_key = T) 
oddballP3_Pz$Oddball <- factor(oddballP3_Pz$Oddball,levels=c("phoddball_P3_Pz","sqoddball_P3_Pz"), labels = c("PhOddball_P3_Pz", "ContOddball_P3_Pz"))
contrasts(oddballP3_Pz$Oddball)
contrasts(oddballP3_Pz$Oddball) <- varContrasts(oddballP3_Pz$Oddball, 
                                                Type = "POC", 
                                                POCList = list(c(.5, -.5)),
                                                Labels = c("PhOddball_P3_Pz", "ContOddball_P3_Pz"))
mod.P3_Pz_Oddball_SAPS <- lmer(data = oddballP3_Pz, MeanAmp ~ Oddball * SAPS.c + (1|Part_ID), REML = F, control = control)
summary(mod.P3_Pz_Oddball_SAPS)
set.seed(69)
confint(mod.P3_Pz_Oddball_SAPS)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.P3_Pz_Oddball_SAPS))
effectsize::t_to_d(param_tab$t[4], param_tab$df[4], pooled = FALSE)
str(oddballP3_Pz$Oddball)
contrasts(oddballP3_Pz$Oddball) = contr.treatment(2)
contrasts(oddballP3_Pz$Oddball)
contrasts(oddballP3_Pz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballP3_Pz$SAPS_Med_Split)
confint(mod.P3_Pz_Oddball_SAPS)

EEGData_NoMow_NAomit <- EEGData_NoMow_NAomit %>% 
  mutate(oddballDiff_P3_Pz = phoddball_P3_Pz - sqoddball_P3_Pz) 
describeBy(EEGData_NoMow_NAomit$oddballDiff_P3_Pz, EEGData_NoMow_NAomit$SAPS_Med_Split)

oddballP3_Pz$SAPS_Med_Split <- relevel(oddballP3_Pz$SAPS_Med_Split, ref="High")
oddballP3_Pz$Oddball <- relevel(oddballP3_Pz$Oddball, ref="sqoddball_P3_Pz")

mod.P3_Pz <- lmer(data = oddballP3_Pz, MeanAmp ~ Oddball  + (1|Part_ID), REML = F)
summary(mod.P3_Pz)
contrasts(oddballP3_Pz$SAPS_Med_Split) = contr.treatment(2)
contrasts(oddballP3_Pz$SAPS_Med_Split)
mod.P3_Pz_SAPS <- lmer(data = oddballP3_Pz, MeanAmp ~ SAPS_Med_Split  + (1|Part_ID), REML = F)
summary(mod.P3_Pz_SAPS)
describe(EEGData_NoMow$phoddball_P3_Pz)
describe(EEGData_NoMow$sqoddball_P3_Pz)
describeBy(EEGData_NoMow$phoddball_P3_Pz, EEGData_NoMow$SAPS_Med_Split)

####### SAPS median split predicted by Phone oddball at Cz######
summary(lm(data = EEGData_NoMow_NAomit, SAPS_MedSplit_Coded ~ phoddball_N2_FzCz, na.rm = T))
contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split) = contr.treatment(2)



####### SAPS median split predicted by Phone oddball at Cz######
summary(lm(data = EEGData, SAPS_MedSplit_Coded ~ phoddball_N2_Cz, na.rm = T))
# Coefficients:
#               Estimate Std. Error t value      Pr(>|t|)    
# (Intercept)   0.55946    0.07613   7.349 0.00000000311 ***
# phoddball_N2_Cz  0.06411    0.02690   2.383        0.0214 *  
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4814 on 45 degrees of freedom
# (16 observations deleted due to missingness)
# Multiple R-squared:  0.1121,	Adjusted R-squared:  0.09235 
# F-statistic:  5.68 on 1 and 45 DF,  p-value: 0.02144




####### Predicting sound oddballs for P2 at Cz, Fz, and Pz from SAPS Median split ######
# Phone oddball predicted by SAPS median split
summary(lm(data = EEGData_NoMow_NAomit, phoddball_P2_Cz ~ SAPS.c, na.rm = T))
# First, fit your multivariate model predicting your three DVs with the IV of achieve.f

mod.mv <- lm(data = EEGData_NoMow_NAomit, cbind(phoddball_P2_Cz, sqoddball_P2_Fz) ~ SAPS.c)
summary(manova(mod.mv), test = "Wilks")


contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split)
contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split) = contr.treatment(2)
EEGData_NoMow$SAPS_Med_Split <- relevel(EEGData_NoMow$SAPS_Med_Split, ref="Low")
summary(lm(data = EEGData_NoMow_NAomit, phoddball_P2_Fz ~ SAPS.c))
mod.phoddball.P2Fz <- lm(data = EEGData_NoMow_NAomit, phoddball_P2_Fz ~ SAPS.c)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.phoddball.P2Fz))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)
summary(lm(data = EEGData_NoMow_NAomit, phoddball_P2_Pz ~ SAPS_Med_Split, na.rm = T))
# Control oddball predicted by SAPS median split
summary(lm(data = EEGData_NoMow_NAomit, sqoddball_P2_Cz ~ SAPS_Med_Split, na.rm = T))
summary(lm(data = EEGData_NoMow_NAomit, sqoddball_P2_Fz ~ SAPS.c))
mod.sqoddball.P2Fz <- lm(data = EEGData_NoMow_NAomit, sqoddball_P2_Fz ~ SAPS.c)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.sqoddball.P2Fz))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)
summary(lm(data = EEGData_NoMow_NAomit, sqoddball_P2_Pz ~ SAPS_Med_Split, na.rm = T))
####### Predicting sound oddballs for N2 at Cz, Fz, FzCz, and Pz from SAPS Median split ######
# Phone oddball predicted by SAPS median split
summary(lm(data = EEGData_NoMow_NAomit, phoddball_N2_Cz ~ SAPS_Med_Split, na.rm = T))
mod.phoddball.N2Cz <- lm(data = EEGData_NoMow_NAomit, phoddball_N2_Cz ~ SAPS_Med_Split, na.rm = T)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.phoddball.N2Cz))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)
contrasts(EEGData_NoMow_NAomit$SAPS_Med_Split) = contr.treatment(2)
EEGData_NoMow$SAPS_Med_Split <- relevel(EEGData_NoMow$SAPS_Med_Split, ref="High")
summary(lm(data = EEGData_NoMow_NAomit, phoddball_N2_Fz ~ SAPS_Med_Split, na.rm = T))
summary(lm(data = EEGData_NoMow_NAomit, phoddball_N2_Pz ~ SAPS_Med_Split, na.rm = T))
summary(lm(data = EEGData_NoMow_NAomit, phoddball_N2_FzCz ~ SAPS_Med_Split, na.rm = T))
# Control oddball predicted by SAPS median split
summary(lm(data = EEGData_NoMow_NAomit, sqoddball_N2_Cz ~ SAPS_Med_Split, na.rm = T))
mod.sqoddball.N2Cz <- lm(data = EEGData_NoMow_NAomit, sqoddball_N2_Cz ~ SAPS_Med_Split, na.rm = T)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.sqoddball.N2Cz))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)
summary(lm(data = EEGData_NoMow_NAomit, sqoddball_N2_Fz ~ SAPS_Med_Split, na.rm = T))
summary(lm(data = EEGData_NoMow_NAomit, sqoddball_N2_Pz ~ SAPS_Med_Split, na.rm = T))
summary(lm(data = EEGData_NoMow_NAomit, sqoddball_N2_FzCz ~ SAPS_Med_Split, na.rm = T))
# F3
# phone oddball
summary(lm(data = EEGData_NoMow_NAomit, phoddball_N2_F3 ~ SAPS_Med_Split, na.rm = T))
mod.phoddball.N2F3 <- lm(data = EEGData_NoMow_NAomit, phoddball_N2_F3 ~ SAPS.c)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.phoddball.N2F3))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)
# control oddball
summary(lm(data = EEGData_NoMow_NAomit, sqoddball_N2_F3 ~ SAPS_Med_Split, na.rm = T))
mod.sqoddball.N2F3 <- lm(data = EEGData_NoMow_NAomit, sqoddball_N2_F3 ~ SAPS.c)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.sqoddball.N2F3))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)
####### Predicting sound oddballs for P3 at Cz, Fz, and Pz from SAPS Median split ######
# Phone oddball predicted by SAPS median split
summary(lm(data = EEGData_NoMow_NAomit, phoddball_P3_Cz ~ SAPS_Med_Split, na.rm = T))
contrasts(EEGData_NoMow$SAPS_Med_Split) = contr.treatment(2)
EEGData_NoMow$SAPS_Med_Split <- relevel(EEGData_NoMow$SAPS_Med_Split, ref="Low")
summary(lm(data = EEGData_NoMow_NAomit, phoddball_P3_Fz ~ SAPS_Med_Split, na.rm = T))
summary(lm(data = EEGData_NoMow_NAomit, phoddball_P3_Pz ~ SAPS_Med_Split, na.rm = T))
mod.phoddball.P3Pz <- lm(data = EEGData_NoMow_NAomit, phoddball_P3_Pz ~ SAPS.c)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.phoddball.P3Pz))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)
# Control oddball predicted by SAPS median split
summary(lm(data = EEGData_NoMow_NAomit, sqoddball_P3_Cz ~ SAPS_Med_Split, na.rm = T))
summary(lm(data = EEGData_NoMow_NAomit, sqoddball_P3_Fz ~ SAPS_Med_Split, na.rm = T))
summary(lm(data = EEGData_NoMow_NAomit, sqoddball_P3_Pz ~ SAPS_Med_Split, na.rm = T))
mod.sqoddball.P3Pz <- lm(data = EEGData_NoMow_NAomit, sqoddball_P3_Pz ~ SAPS.c)
# Cohen's d and conf intervals
(param_tab <- parameters::parameters(mod.sqoddball.P3Pz))
effectsize::t_to_d(param_tab$t[2], param_tab$df[2], pooled = FALSE)

# ICA Compare #####
ICAComp <- read.csv("ICACompare2020.csv")
# P3 Pz all four conditions
t.test(ICAComp$P3_ComSq_Pz, ICAComp$ICA_P3_ComSq_Pz, na.rm = T)
t.test(ICAComp$P3_ComPh_Pz, ICAComp$ICA_P3_ComPh_Pz, na.rm = T)
t.test(ICAComp$P3_RareSq_Pz, ICAComp$ICA_P3_RareSq_Pz, na.rm = T)
t.test(ICAComp$P3_RarePh_Pz, ICAComp$ICA_P3_RarePh_Pz, na.rm = T)
# N2 Cz all four conditions
t.test(ICAComp$N2_ComSq_Cz, ICAComp$ICA_N2_ComSq_Cz, na.rm = T)
t.test(ICAComp$N2_ComPh_Cz, ICAComp$ICA_N2_ComPh_Cz, na.rm = T)
t.test(ICAComp$N2_RareSq_Cz, ICAComp$ICA_N2_RareSq_Cz, na.rm = T)
t.test(ICAComp$N2_RarePh_Cz, ICAComp$ICA_N2_RarePh_Cz, na.rm = T)
# N2 F3 all four conditions
t.test(ICAComp$N2_ComSq_F3, ICAComp$ICA_N2_ComSq_F3, na.rm = T)
t.test(ICAComp$N2_ComPh_F3, ICAComp$ICA_N2_ComPh_F3, na.rm = T)
t.test(ICAComp$N2_RareSq_F3, ICAComp$ICA_N2_RareSq_F3, na.rm = T)
t.test(ICAComp$N2_RarePh_F3, ICAComp$ICA_N2_RarePh_F3, na.rm = T)
# P2 Fz all four conditions
t.test(ICAComp$P2_ComSq_Fz, ICAComp$ICA_P2_ComSq_Fz, na.rm = T)
t.test(ICAComp$P2_ComPh_Fz, ICAComp$ICA_P2_ComPh_Fz, na.rm = T)
t.test(ICAComp$P2_RareSq_Fz, ICAComp$ICA_P2_RareSq_Fz, na.rm = T)
t.test(ICAComp$P2_RarePh_Fz, ICAComp$ICA_P2_RarePh_Fz, na.rm = T)



# older stuff####
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.9060 -2.1492 -0.0173  1.8420  5.3067 
# 
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -1.9490     0.5132  -3.798 0.000435 ***
# SAPS_MedSplit_Coded   1.7483     0.7336   2.383 0.021437 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.514 on 45 degrees of freedom
# (16 observations deleted due to missingness)
# Multiple R-squared:  0.1121,	Adjusted R-squared:  0.09235 
# F-statistic:  5.68 on 1 and 45 DF,  p-value: 0.02144

# Phone oddball at Fz predicted by SAPS median split
summary(lm(data = EEGData_NoMow, phoddball_N2_Fz ~ SAPS_MedSplit_Coded, na.rm = T)) # p = 0.0786 
# Phone oddball at Pz predicted by SAPS median split
summary(lm(data = EEGData_NoMow, phoddball_N2_Pz ~ SAPS_MedSplit_Coded, na.rm = T)) # p = 0.385 
# Square oddball at Cz predicted by SAPS median split
summary(lm(data = EEGData_NoMow, sqoddball_N2_Cz ~ SAPS_MedSplit_Coded, na.rm = T)) # p = 0.938
# Square oddball at Fz predicted by SAPS median split
summary(lm(data = EEGData_NoMow, sqoddball_N2_Fz ~ SAPS_MedSplit_Coded, na.rm = T)) # p = 0.729
# Square oddball at Pz predicted by SAPS median split
summary(lm(data = EEGData_NoMow, sqoddball_N2_Pz ~ SAPS_MedSplit_Coded, na.rm = T)) # p = 0.338
# Mower oddball at Cz predicted by SAPS median split
summary(lm(data = EEGData, mowoddball_N2_Cz ~ SAPS_MedSplit_Coded, na.rm = T)) # p = 0.5110
# Mower oddball at Fz predicted by SAPS median split
summary(lm(data = EEGData, mowoddball_N2_Fz ~ SAPS_MedSplit_Coded, na.rm = T)) # p = 0.721
# Mower oddball at Pz predicted by SAPS median split
summary(lm(data = EEGData, mowoddball_N2_Pz ~ SAPS_MedSplit_Coded, na.rm = T)) # p = 0.0521 Trending suggesting potential N2pc
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.9591 -1.5771  0.0839  1.2470  6.7609 
# 
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)  
# (Intercept)           0.6988     0.4540   1.539   0.1308  
# SAPS_MedSplit_Coded  -1.2947     0.6491  -1.995   0.0521 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.224 on 45 degrees of freedom
# (16 observations deleted due to missingness)
# Multiple R-squared:  0.08124,	Adjusted R-squared:  0.06082 
# F-statistic: 3.979 on 1 and 45 DF,  p-value: 0.05215



##### Overall Sound comparisons #####
# ERPamps <- ddply(EEGData_NoMow_NAomit,
#                  .(SAPS_Med_Split),
#                  summarise,
#                  n = length(Part_ID),
#                  Mean.amp.P2_Fz = mean(P2_Fz_PhConAve),
#                  SD.amp.P2_Fz = sd(P2_Fz_PhConAve),
#                  Mean.amp.P2_Cz = mean(P2_Cz_PhConAve),
#                  SD.amp.P2_Cz = sd(P2_Cz_PhConAve),
#                  Mean.amp.P2_Pz = mean(P2_Pz_PhConAve),
#                  SD.amp.P2_Pz = sd(P2_Pz_PhConAve),
#                  Mean.amp.N2_Fz = mean(N2_Fz_PhConAve),
#                  SD.amp.N2_Fz = sd(N2_Fz_PhConAve),
#                  Mean.amp.N2_Cz = mean(N2_Cz_PhConAve),
#                  SD.amp.N2_Cz = sd(N2_Cz_PhConAve),
#                  Mean.amp.N2_Pz = mean(N2_Pz_PhConAve),
#                  SD.amp.N2_Pz = sd(N2_Pz_PhConAve),
#                  Mean.amp.N2_FzCz = mean(N2_FzCz_PhConAve),
#                  SD.amp.N2_FzCz = sd(N2_FzCz_PhConAve),
#                  Mean.amp.P3_Fz = mean(P3_Fz_PhConAve),
#                  SD.amp.P3_Fz = sd(P3_Fz_PhConAve),
#                  Mean.amp.P3_Cz = mean(P3_Cz_PhConAve),
#                  SD.amp.P3_Cz = sd(P3_Cz_PhConAve),
#                  Mean.amp.P3_Pz = mean(P3_Pz_PhConAve),
#                  SD.amp.P3_Pz = sd(P3_Pz_PhConAve)) 
# ERPamps
###### Phone/control Total oddballs and from SAPS Median split #####
# N2
summary(lm(data = EEGData_NoMow, Total_Oddball_PhSq_N2_Cz ~ MAAS_Med_Split, na.rm = T))
summary(lm(data = EEGData_NoMow, Total_Oddball_PhSq_N2_Cz ~ SAPS_Med_Split, na.rm = T))
summary(lm(data = EEGData_NoMow, Total_Oddball_PhSq_N2_Fz ~ SAPS_Med_Split, na.rm = T))
contrasts(EEGData_NoMow$SAPS_Med_Split)
contrasts(EEGData_NoMow$MAAS_Med_Split)=contr.treatment(2)

summary(lm(data = EEGData_NoMow, Total_Oddball_PhSq_N2_Pz ~ SAPS_Med_Split, na.rm = T))

summary(lm(data = EEGData_NoMow, Total_Oddball_PhSq_N2_FzCz ~ SAPS_Med_Split, na.rm = T))
# P2
summary(lm(data = EEGData_NoMow, Total_Oddball_PhSq_P2_Cz ~ SAPS_Med_Split, na.rm = T))
summary(lm(data = EEGData_NoMow, Total_Oddball_PhSq_P2_Fz ~ SAPS_Med_Split, na.rm = T))
summary(lm(data = EEGData_NoMow, Total_Oddball_PhSq_P2_Pz ~ SAPS_Med_Split, na.rm = T))
# P3
summary(lm(data = EEGData_NoMow, Total_Oddball_PhSq_P3_Cz ~ SAPS_Med_Split, na.rm = T))
summary(lm(data = EEGData_NoMow, Total_Oddball_PhSq_P3_Fz ~ SAPS_Med_Split, na.rm = T))
summary(lm(data = EEGData_NoMow, Total_Oddball_PhSq_P3_Pz ~ SAPS_Med_Split, na.rm = T))





# MAAS ####
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_Cz ~ MAAS_Ave, na.rm = T))
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_FzCz ~ MAAS_Ave, na.rm = T))
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_Cz ~ MAAS_Ave, na.rm = T))
contrasts(EEGData_NoMow_NAomit$MAAS_Med_Split)=contr.treatment(2)
summary(lm(data = EEGData_NoMow_NAomit, phoddball_N2_Cz ~ MAAS_Ave, na.rm = T))
summary(lm(data = EEGData_NoMow_NAomit, SAPS_Ave ~ MAAS_Med_Split, na.rm = T))
describe(EEGData_NoMow_NAomit$MAAS_Ave)
#####
MAASPhoneP2_Cz <- EEGData_NoMow_NAomit %>% group_by(Part_ID) %>%
  gather(Sound, MeanAmp, ComRareAve_Ph_P2_Cz, ComRareAve_Sq_P2_Cz, factor_key = T) 
MAASPhoneP2_Cz$Sound <- factor(phoneP2_Cz$Sound,levels=c("ComRareAve_Ph_P2_Cz","ComRareAve_Sq_P2_Cz"))
str(MAASPhoneP2_Cz$Sound)
contrasts(MAASPhoneP2_Cz$Sound) = contr.treatment(2)
contrasts(MAASPhoneP2_Cz$Sound)
contrasts(phoneP2_Cz$MAAS_Ave) = contr.treatment(2)
contrasts(MAASPhoneP2_Cz$MAAS_Ave)
mod.P2_Cz_Sound <- lm(data = MAASPhoneP2_Cz, MeanAmp ~ Sound * MAAS_Ave)
summary(mod.P2_Cz_Sound)
######
summary(lm(data = EEGData_NoMow_NAomit, Total_Oddball_PhSq_N2_F3 ~ MAAS_Ave, na.rm = T))

####### Old analyses######

t.test(EEGData$Sq_Oddball_Cz, EEGData$Ph_Oddball_Cz)
t.test(EEGData$Sq_Oddball_Fz, EEGData$Ph_Oddball_Fz)

mod.1 <- summary(lm(data = EEGData, SAPS_MedSplit_Coded ~ Ph_Oddball_Cz, na.rm = T)) 

# Coefficients:
#                Estimate Std. Error t value      Pr(>|t|)    
# (Intercept)    0.55288    0.07587   7.287 0.00000000383 ***
# Ph_Oddball_Cz  0.06071    0.02665   2.278        0.0275 * 

summary(lm(data = EEGData, Ph_Oddball_Cz ~ SAPS_Med_Split, na.rm = T))
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)        -0.1768     0.5342  -0.331   0.7422  
# SAPS_Med_SplitLow  -1.7026     0.7475  -2.278   0.0275 *
contrasts(EEGData$SAPS_Med_Split); describe(EEGData$Ph_Oddball_Cz)
#       Low
# High   0
# Low    1
# vars  n  mean   sd median trimmed  mad   min max range skew kurtosis   se
# X1    1 47 -1.05 2.68  -1.09   -1.09 3.09 -6.56 5.3 11.85 0.15    -0.68 0.39

summary(lm(data = EEGData, SAPS_MedSplit_Coded ~ Sq_Oddball_Cz, na.rm = T))
summary(lm(data = EEGData, SAPS_MedSplit_Coded ~ Mow_Oddball_Cz, na.rm = T))
summary(lm(data = EEGData, SAPS_MedSplit_Coded ~ Ph_Oddball_Fz, na.rm = T))
summary(lm(data = EEGData, SAPS_MedSplit_Coded ~ Sq_Oddball_Fz, na.rm = T))
summary(lm(data = EEGData, SAPS_MedSplit_Coded ~ Mow_Oddball_Fz, na.rm = T))
summary(lm(data = EEGData, SAPS_MedSplit_Coded ~ Ph_Oddball_Pz, na.rm = T))
summary(lm(data = EEGData, SAPS_MedSplit_Coded ~ Sq_Oddball_Pz, na.rm = T))
summary(lm(data = EEGData, SAPS_MedSplit_Coded ~ Mow_Oddball_Pz, na.rm = T))

# Creating FZCZ oddballs
EEGData <- mutate(EEGData, Sq_Oddball_FzCz = Rare_Sq_FzCz - Com_Sq_FzCz)
EEGData <- mutate(EEGData, Ph_Oddball_FzCz = Rare_Ph_FzCz - Com_Ph_FzCz)

t.test(EEGData$Sq_Oddball_FzCz, EEGData$Ph_Oddball_FzCz)
# t = 10.374, df = 58.436, p-value = 7.158e-15

summary(lm(data = EEGData, SAPS_MedSplit_Coded ~ Sq_Oddball_FzCz))
summary(lm(data = EEGData, SAPS_MedSplit_Coded ~ Ph_Oddball_FzCz))
# Coefficients:
#                 Estimate Std. Error t value   Pr(>|t|)    
# (Intercept)      0.72854    0.13234   5.505 0.00000168 ***
# Ph_Oddball_FzCz  0.03942    0.01841   2.142     0.0377 *  

