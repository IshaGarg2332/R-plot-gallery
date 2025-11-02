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


#The graph shows how the probability of having Parkinson’s Disease or Prodromal changes with age
#Each dot represents an individual participant (0 = Healthy Control, 1 = Parkinson’s/Prodromal)
#The green line represents the logistic regression model’s predicted probability of having Parkinson’s/Prodromal status based on age
#As age increases, the green line slopes upward, indicating that older participants have a higher predicted probability of Parkinson’s/Prodromal status


#Load datasets
Participant <- read_csv("data/ParticipantStatus.csv")
PDDiagHistory <- read_csv("data/PDDiagHistory.csv")

#Combine ParticipantStatus and DiagHistory
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

#Load and filter head injury data
HeadInjury <- read_csv("data/MedConditions.csv")
HeadInjury <- HeadInjury[grepl("(Concussion|concussion|concussions|skull fracture|Skull fracture|Head Injury|head injury|Head injury)",HeadInjury$MHTERM,ignore.case = F),]
tmp <- HeadInjury
names(tmp)[names(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

#Load and filter hypertension data
Hypertension <- read_csv("data/MedConditions.csv")
Hypertension <- Hypertension[grepl("(Hypertension|hypertension|High Blood Pressure|High blood pressure|high blood pressure|Elevated blood pressure)",Hypertension$MHTERM,ignore.case = F),]
tmp1 <- Hypertension
names(tmp1)[names(tmp1) == "MHTERM"] <- "hyper"
colnames(tmp1)[colnames(tmp1) == "patno"] <- "PATNO"

#Load and filter hypotension data
Hypotension <- read_csv("data/MedConditions.csv")
Hypotension <- Hypotension[grepl("(Orthostatis Hypotension|ORTHOSTATIC HYPOTENSION|orthostatic hypotension|Neurogenic Orthostatic Hypotension|Neurogenic orthostatic hypotension)",Hypotension$MHTERM,ignore.case = F),]
tmp2 <- Hypotension
names(tmp2)[names(tmp2) == "MHTERM"] <- "hypo"
colnames(tmp2)[colnames(tmp2) == "patno"] <- "PATNO"

#Combine datasets
Test1 <- merge(cohToTest,tmp1,by="PATNO",all.x=T)
Test2 <- merge(cohToTest,tmp1,by="PATNO",all.x=T)

#Load and filter hearing loss data
MedConditions <- read_csv("data/MedConditions.csv")
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

#Filter hearing loss by patients who have had hearing loss at least a year prior to Parkinson's diagnosis
Tes1$HLDate <- as.Date(paste0("01/",Tes1$MHDIAGDT.y),format="%d/%m/%Y")
Tes1$PDdate <- as.Date(paste0("01/",Tes1$PDDXDT),format="%d/%m/%Y")
Tes1$hlDiff <- ifelse(is.na(Tes1$PDdate),Tes1$EnrollDate - Tes1$HLDate,Tes1$PDdate - Tes1$HLDate)
Tes1$hlBefore <- ifelse(!is.na(Tes1$MHTERM),ifelse(Tes1$hlDiff > (-365 * 1) ,T,F),F)

#Select certain columns
Tes1 <- select(Tes1, c(PATNO, COHORT_DEFINITION, EnrollDate, ENROLL_AGE, HLDate, PDdate, hlDiff, hlBefore, hyper, MHTERM))

#Convert cohort definitions to binary values
Tes2 <- Tes1 %>%
  mutate(COHORT_DEFINITION=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ 1, 
    COHORT_DEFINITION == "Prodromal" ~ 1, 
    COHORT_DEFINITION == "Healthy Control" ~ 0, 
    COHORT_DEFINITION == "SWEDD" ~ NA
  ))

# Convert columns to binary
Tes2 <- Tes2 %>%
  mutate(hyper=case_when(
    is.na(hyper) ~ 0,
    !is.na(hyper) ~ 1
  ))
Tes2 <- Tes2 %>%
  mutate(hlBefore=case_when(
    hlBefore == FALSE ~ 0,
    hlBefore == TRUE ~ 1
  ))

#Gender into hypertension
#Logical regression model for hearing loss and another thing
#Y axis is 1 you have parkinson or prodromal and 0 is you dont
#Sinuodal curve
#X is hypertension or age or sex or hearing loss

# Load demographics data and merge with main dataset
Demo <- read_csv("data/Demographics.csv")
Test1 <- merge(Demo,Tes2,by="PATNO",all.x=T)

# Prepare data for logistic regression
Tes3 <- select(Test1, c(PATNO, COHORT_DEFINITION, EnrollDate, ENROLL_AGE, HLDate, PDdate, hlDiff, hlBefore, hyper, MHTERM, SEX))
Tes3 <- Tes3 %>% 
  mutate(interact = hyper*MHTERM)

# Fit logistic regression model predicting PD/prodromal status
test <- glm(COHORT_DEFINITION ~ hlBefore + ENROLL_AGE + SEX, data = Tes3, family = "binomial")
summary(test)
exp(coef(test))

# Prepare data for plotting predicted probabilities
pot <- data.frame(hyper = Tes3$hyper,
                  age = Tes3$ENROLL_AGE,
                  sex = Tes3$SEX,
                  hl = Tes3$MHTERM,
                  hasPPD = Tes3$COHORT_DEFINITION,
                  interaction = Tes3$interact,
                  fit = predict(test, Tes3))

# Calculate predicted probabilities from log-odds
pot$fit_prob <- exp(pot$fit)/(1+exp(pot$fit))
pot <- pot[!is.na(pot$hasPPD),]

#Import library
library(ggplot2)

# Plot observed data points and predicted probabilities by age
ggplot(pot, aes(x=age, y=hasPPD)) + 
  geom_point() +
  geom_line(aes(x=age, y=fit_prob))

# Plot with logistic regression smooth curve fit
ggplot(pot, aes(x=age, y=hasPPD)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE, 
              method.args = list(family=quasibinomial))

