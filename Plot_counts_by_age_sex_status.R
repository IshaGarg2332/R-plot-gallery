# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------

#Import libraries
library("readxl")
library("tidyr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("readxl")
library("boot")
library("table1")
library("Hmisc")
library("sjmisc")
library("ggplot2")


#The plot shows the number of patients with hypertension across different age groups, separated by sex (female and male), 
#and categorized by Parkinson’s disease status (Healthy Control, Prodromal, or Parkinson’s Disease).


# Import datasets
Participant <- read_csv("data/ParticipantStatus.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")
MedConditions <- read_csv("data/MedConditions.csv")
Demo <- read_csv("data/Demographics.csv")

# Define bar plot function 
barplott <- function(hldf,colname,titlename, titlename1, titlename2) {
  
  #Combine ParticipantStatus and DiagHistory
  cohToTest <- Participant
  cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
  cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
  cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)
  
  #Merge datasets
  Test1 <- merge(cohToTest,hldf,by="PATNO",all.x=T)
  
  #Filter hearing loss
  hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
  hloss <- hloss[!is.na(hloss$MHDIAGDT),]
  hloss$MHTERM <- tolower(hloss$MHTERM)
  hloss$MHTERM <- str_squish(hloss$MHTERM)
  hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
  hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
  hloss <- hloss[hloss$MHTERM != "hearing",]
  hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]
  
  #Merge the two together
  Tes1 <- merge(Test1,hloss,by="PATNO",all.x=T)
  
  #Sort hearing loss as true only when it was diagnosed at least a year prior to Parkinson's diagnosis
  Tes1$HLDate <- as.Date(paste0("01/",Tes1$MHDIAGDT.y),format="%d/%m/%Y")
  Tes1$PDdate <- as.Date(paste0("01/",Tes1$PDDXDT),format="%d/%m/%Y")
  Tes1$hlDiff <- ifelse(is.na(Tes1$PDdate),Tes1$EnrollDate - Tes1$HLDate,Tes1$PDdate - Tes1$HLDate)
  Tes1$hlBefore <- ifelse(!is.na(Tes1$MHTERM),ifelse(Tes1$hlDiff > (-365 * 1) ,T,F),F)
  
  #Select certain columns
  Tes1 <- select(Tes1, c(PATNO, COHORT_DEFINITION, EnrollDate, ENROLL_AGE, HLDate, PDdate, hlDiff, hlBefore, colname, MHTERM))
  
  # Create binary columns
  Tes2 <- Tes1 %>%
    mutate(colname=case_when(
      is.na(colname) ~ 0,
      !is.na(colname) ~ 1
    ))
  Tes2 <- Tes2 %>%
    mutate(hlBefore=case_when(
      hlBefore == FALSE ~ 0,
      hlBefore == TRUE ~ 1
    ))
  
  #Merge datasets
  Test1 <- merge(Demo,Tes2,by="PATNO",all.x=T)
  
  #Select certain columns
  Tes3 <- select(Test1, c(PATNO, COHORT_DEFINITION, EnrollDate, ENROLL_AGE, HLDate, PDdate, hlDiff, hlBefore, colname, MHTERM, SEX))
  
  #Make two different datasets, one for males and one for females
  Female <- Tes3 %>%
    mutate(SEX=case_when(
      SEX == 0 ~ NA,
      SEX == 1 ~ 1
    ))
  Male <- Tes3 %>%
    mutate(SEX=case_when(
      SEX == 0 ~ 0,
      SEX == 1 ~ NA
    ))
  
  #Remove temporary datasets
  rm("Tes3")
  
  #Remove NA values in sex, keep only the sex needed
  Female <- Female[!is.na(Female$SEX),]
  Male <- Male[!is.na(Male$SEX),]
  
  # Separate by age and remove NAs
  Female1 <- Female %>%
    mutate(agecat=case_when(
      ENROLL_AGE >=18 & ENROLL_AGE <25 ~ "18-25",
      ENROLL_AGE >=25 & ENROLL_AGE <35 ~ "25-35",
      ENROLL_AGE >=35 & ENROLL_AGE <45 ~ "35-45",
      ENROLL_AGE >=45 & ENROLL_AGE <55 ~ "45-55",
      ENROLL_AGE >=55 & ENROLL_AGE <65 ~ "55-65",
      ENROLL_AGE >=65 & ENROLL_AGE <75 ~ "65-75",
      ENROLL_AGE >=75 & ENROLL_AGE <85 ~ "75-85",
      ENROLL_AGE >=85 ~ ">85",
    ))
  Female1 <- Female1[!is.na(Female1$agecat),]
  
  # Separate by age and remove NAs
  Male1 <- Male %>%
    mutate(agecat=case_when(
      ENROLL_AGE >=18 & ENROLL_AGE <25 ~ "18-25",
      ENROLL_AGE >=25 & ENROLL_AGE <35 ~ "25-35",
      ENROLL_AGE >=35 & ENROLL_AGE <45 ~ "35-45",
      ENROLL_AGE >=45 & ENROLL_AGE <55 ~ "45-55",
      ENROLL_AGE >=55 & ENROLL_AGE <65 ~ "55-65",
      ENROLL_AGE >=65 & ENROLL_AGE <75 ~ "65-75",
      ENROLL_AGE >=75 & ENROLL_AGE <85 ~ "75-85",
      ENROLL_AGE >=85 ~ ">85",
    ))
  Male1 <- Male1[!is.na(Male1$agecat),]
  
  #Remove temporary datasets
  rm("Female", "Male")
  
  #Select only rows where the patient had hearing loss at least a year prior to Parkinson's diagnosis
  Female2 <- Female1 %>%
    mutate(hlBefore=case_when(
      hlBefore == 0 ~ NA,
      hlBefore == 1 ~ 1
    ))
  Male2 <- Male1 %>%
    mutate(hlBefore=case_when(
      hlBefore == 0 ~ NA,
      hlBefore == 1 ~ 1
    ))
  Female2 <- Female2[!is.na(Female2$hlBefore),]
  Male2 <- Male2[!is.na(Male2$hlBefore),]
  
  #Filter Male and Female datasets
  Female2 <- select(Female2, c(COHORT_DEFINITION, agecat, colname))
  Female2$COHORT_DEFINITION <- as.factor(Female2$COHORT_DEFINITION)
  Female2$agecat <- factor(Female2$agecat, levels = c("45-55", "55-65", "65-75", "75-85", ">85"))
  Female2$COHORT_DEFINITION <- factor(Female2$COHORT_DEFINITION, levels = c("Healthy Control", 
                                                                            "Prodromal", 
                                                                            "Parkinson's Disease"))
  names(Female2)[names(Female2) == 'COHORT_DEFINITION'] <- "Parkinson's Disease"
  
  Male2 <- select(Male2, c(COHORT_DEFINITION, agecat, colname))
  Male2$COHORT_DEFINITION <- as.factor(Male2$COHORT_DEFINITION)
  Male2$agecat <- factor(Male2$agecat, levels = c("35-45", "55-65", "65-75", "75-85"))
  Male2$COHORT_DEFINITION <- factor(Male2$COHORT_DEFINITION, levels = c("Healthy Control", 
                                                                        "Prodromal", 
                                                                        "Parkinson's Disease"))
  names(Male2)[names(Male2) == 'COHORT_DEFINITION'] <- "Parkinson's Disease"
  
  # Plot female data
  Female <- Female2 %>%
    ggplot(aes(x = agecat, fill = `Parkinson's Disease`)) +
    geom_bar(position = position_dodge(preserve = "single"), 
             width = 0.7) +
    scale_fill_manual(
      "Test", 
      guide = "legend"
    )+
    scale_fill_brewer(palette = "Pastel1", drop = FALSE) +
    labs(x="Age",y=titlename,
         title= titlename1) + 
    scale_y_continuous(breaks=c(0,2,4,6,8,10, 12, 14, 16, 18))
  
  # Plot male data
  Male <- Male2 %>%
    ggplot(aes(x = agecat, fill = `Parkinson's Disease`)) +
    geom_bar(position = position_dodge(preserve = "single"), 
             width = 0.7) +
    scale_fill_manual(
      "Test", 
      guide = "legend"
    )+
    scale_fill_brewer(palette = "Pastel1", drop = FALSE) +
    labs(x="Age",y=titlename,
         title= titlename2) + 
    scale_y_continuous(breaks=c(0,2,4,6,8,10, 12, 14, 16, 18))
  rm("Male1", "Male2", "Female1", "Female2")
  
  # Combine plots vertically
  library("ggpubr")
  figure <- ggarrange(Male, Female,
                      ncol = 1, nrow = 2)
  figure
}

# Prepare hypertension data for plotting
Hypertension <- read_csv("data/MedConditions.csv")
Hypertension <- Hypertension[grepl("(Hypertension|hypertension|High Blood Pressure|High blood pressure|high blood pressure|Elevated blood pressure)",Hypertension$MHTERM,ignore.case = F),]
tmp1 <- Hypertension
names(tmp1)[names(tmp1) == "MHTERM"] <- "hyper"
colnames(tmp1)[colnames(tmp1) == "patno"] <- "PATNO"

# Run plot function
barplott(tmp1,"hyper","Number of Patients with Hypertension", "Females with Hypertension", "Males with Hypertension")
