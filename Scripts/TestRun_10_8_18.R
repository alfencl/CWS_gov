#Load Libraries
library(tidyverse)
library(pscl)

# Load data
Data <- read_csv("Data/10_8_19_TestRun.csv")

# Create binary public-private variable
Data$OWNERTYPE_KDB18 <- as.factor(Data$OWNERTYPE_KDB18)
summary(Data$OWNERTYPE_KDB18)
# create new variable
Data$OWNTERTYPE_binary <- NA 
#Use ifelse statements to aggregate ownership variable into a binary one
Data$OWNTERTYPE_binary <- as.factor(Data$OWNTERTYPE_binary)
Data$OWNTERTYPE_binary <- ifelse(Data$OWNERTYPE_KDB18 == "Private", "Private", NA)
Data$OWNTERTYPE_binary <- ifelse(Data$OWNERTYPE_KDB18 == "Federal Government", "Public", Data$OWNTERTYPE_binary)
Data$OWNTERTYPE_binary <- ifelse(Data$OWNERTYPE_KDB18 == "Local", "Public", Data$OWNTERTYPE_binary)
Data$OWNTERTYPE_binary <- ifelse(Data$OWNERTYPE_KDB18 == "State", "Public", Data$OWNTERTYPE_binary)
Data$OWNTERTYPE_binary <- ifelse(Data$OWNERTYPE_KDB18 == "State Government", "Public", Data$OWNTERTYPE_binary)
#correct variable type and check for accuracy/problems
Data$OWNTERTYPE_binary <- as.factor(Data$OWNTERTYPE_binary)
summary(Data$OWNTERTYPE_binary)

#Create binary compliance variable
Data$HRTW_Compliance_Status <- as.factor(Data$HRTW_Compliance_Status) #make factor
summary(Data$HRTW_Compliance_Status) 
#create new variable
Data$Compliance_binary <- NA
#ifelse statements
Data$Compliance_binary <- ifelse(Data$HRTW_Compliance_Status == "IC", 1, Data$Compliance_binary)
Data$Compliance_binary <- ifelse(Data$HRTW_Compliance_Status == "RTC", 1, Data$Compliance_binary)
Data$Compliance_binary <- ifelse(Data$HRTW_Compliance_Status == "OOC", 0, Data$Compliance_binary)
#check
Data$Compliance_binary <- as.factor(Data$Compliance_binary)
summary(Data$Compliance_binary)

#Check/correct variable type for violation count variable, it is an integeer so thats right
summary(Data$Violation_Count)

#check control variable types, source and size
summary(Data$POP_TOTAL) #integer which is good
Data$Source_coded_18 <- as.factor(Data$Source_coded_18)
summary(Data$Source_coded_18)# need to make into two

#make binary source variable
Data$Source_binary <- NA
Data$Source_binary <- ifelse(Data$Source_coded_18 == "GW", "GW", Data$Source_binary)
Data$Source_binary <- ifelse(Data$Source_coded_18 == "GWP", "GW", Data$Source_binary)
Data$Source_binary <- ifelse(Data$Source_coded_18 == "SW", "SW", Data$Source_binary)
Data$Source_binary <- ifelse(Data$Source_coded_18 == "SWP", "SW", Data$Source_binary)
Data$Source_binary <- as.factor(Data$Source_binary)
summary(Data$Source_binary)

# Try GLM for binary compliance against public private binary with controls
logit1 <- glm(Compliance_binary ~ OWNTERTYPE_binary + POP_TOTAL + Source_binary, data = Data, family = binomial(link="logit")) #HMMM saying there is perfect separation in a variable but I'm not sure where. 
summary(logit1)
pR2(logit1)
#Try for count
logit2 <- glm(Violation_Count ~ OWNTERTYPE_binary + POP_TOTAL + Source_binary, data = Data, family = "poisson") 
summary(logit2)

