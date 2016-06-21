# Course: CS 513 A
# Final Project Name: Data Mining Medicare Reimbursements to Physicians
# Group 5 (on Canvas), Team 7 (in presentation)
# Code for Evaluation phase

rm(list=ls())

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library('C50')

#read in csv file
medicare<-read.csv("MEDICARE PHYSICIAN DATA.csv")
attach(medicare)

max(Total.Medicare.Payment.Amount)
min(Total.Medicare.Payment.Amount)
median(Total.Medicare.Payment.Amount)

#categorize what region state is in
medicare$region <- factor(NPPES.Provider.State) 
levels(medicare$region) <- list(
  north = c("CT","ME","MA","NH","RI","VT","NJ","NY","PA"),
  midwest = c("IL","MI","IN","OH","WI","IA","KS","MN","MO","NE","ND","SD"),
  south =c("DE","FL","GA","MD","NC","SC","WV","VA","DC"),
  southwest=c("AL", "KY","MS", "TN","AR","LA","OK","TX"),
  west=c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA")
)

medicare$region<-as.factor(medicare$region)

#categorize what specialty is in
medicare$specialty<-factor(Provider.Type)
levels(medicare$specialty)<- list(
  surgery = c("Colorectal Surgery (formerly proctology)", "General Surgery", "Geriatric Medicine", "Maxillofacial Surgery", "Oral Surgery (dentists only)",
              "Orthopedic Surgery", "Plastic and Reconstructive Surgery", "Thoracic Surgery", "Hand Surgery", "Vascular Surgery"),
  radiation = c("Interventional Radiology", "Diagnostic Radiology", "Radiation", "Radiation Oncology"),
  cardiology = c("Cardiac Electrophysiology", "Cardiac Surgery", "Cardiology"),
  medicine = c("Addiction Medicine", "Nuclear Medicine", "Geriatric Medicine", "Ortheopedic Medicine", "Osteopathic Manipulative Medicine", "Pain Management", "Preventive Medicine","Sports Medicine", "Internal Medicine", "Interventional Pain Management"),
  therapy =  c("Physical Medicine and Rehabilitation", "Physical Therapist", "Occupational therapist"),
  ER =  c("Emergency Medicine", "Critical Care (Intensivists)"),
  psychology = c("Psychiatry", "Psychologist (billing independently)", "Clinical Psychologist", "Geriatric Psychiatry"),
  neurology = c("Neurology", "Neuropsychiatry", "Neurosurgery"),
  oncology = c("Gynecological/Oncology", "Medical Oncology", "Surgical Oncology"),
  hematology = c("Hematology", "Hematology/Oncology"),
  allergy_pathology = c("Pathology", "Allergy/Immunology", "Infectious Disease"),
  anesthesiology = "Anesthesiology",
  audiologist = "Audiologist (billing independently)",
  chiropractor = "Chiropractic",
  dermatology = "Dermatology",
  nephrology = "Nephrology",
  endocrinology = "Endocrinology",
  gastroenterology = "Gastroenterology",
  optometry = c("Optometry", "Ophthalmology"),
  podiatry = "Podiatry",
  urology = "Urology",
  rheumatology = "Rheumatology",
  otolaryngology="Otolaryngology",
  pulmonary = "Pulmonary Disease",
  general_practice = c("General Practice", "Family Practice")
)

#medicare$num<-ifelse(Number.of.Services >= 0 & Number.of.Services < 1000 ,'Low',ifelse(Number.of.Services >= 1000 & Number.of.Services < 400000 ,'Medium','High'))
#medicare$num<-as.factor(medicare$num)

#medicare$submitted<-ifelse(Total.Submitted.Charges >= 0 & Total.Submitted.Charges < 900000 ,'Low',ifelse(Total.Submitted.Charges >= 300000 & Total.Submitted.Charges < 1500000 ,'Medium','High'))
#medicare$submitted<-as.factor(medicare$submitted)

#medicare$allowed<-ifelse(Total.Medicare.Allowed.Amount >= 0 & Total.Medicare.Allowed.Amount < 385600 ,'Low',ifelse(Total.Medicare.Allowed.Amount >= 385600 & Total.Medicare.Allowed.Amount < 642674 ,'Medium','High'))
#medicare$allowed<-as.factor(medicare$allowed)

medicare$reimburse<-ifelse(Total.Medicare.Payment.Amount >= 0 & Total.Medicare.Payment.Amount < 300000 ,'Low',ifelse(Total.Medicare.Payment.Amount >= 300000 & Total.Medicare.Payment.Amount < 500000 ,'Medium','High'))
medicare$reimburse<-as.factor(medicare$reimburse)

###########################################
#define data frame or matrix of predictors
#model with gender, credentials, state, region, num and specialty as the predictors for the reimburse field
#predictor<-data.frame(medicare$NPPES.Provider.Gender, medicare$specialty, medicare$NPPES.Provider.State, medicare$NPPES.Credentials, medicare$num, medicare$region)
#predictor<-data.frame(medicare$NPPES.Provider.Gender, medicare$specialty, medicare$NPPES.Provider.State, medicare$num, medicare$region)
#predictor<-data.frame(medicare$NPPES.Provider.Gender, medicare$specialty, medicare$NPPES.Provider.State, medicare$num)
#predictor<-data.frame(medicare$NPPES.Provider.Gender, medicare$specialty, medicare$NPPES.Provider.State, medicare$region)
predictor<-data.frame(medicare$NPPES.Provider.Gender, medicare$specialty, medicare$NPPES.Provider.State)
#predictor<-data.frame(medicare$NPPES.Provider.Gender, medicare$specialty, medicare$region)
#predictor<-data.frame(medicare$NPPES.Provider.Gender, medicare$specialty, medicare$num)
#predictor<-data.frame(medicare$NPPES.Provider.Gender, medicare$NPPES.Provider.State)


#a target factor vector with 2 or more levels
target<- medicare$reimburse

#Divide the total dataset into testing dataset and training dataset
idx5=seq(from=1,to=60000,by=5)

#Store 20% data as testing dataset
test_predictor<-predictor[idx5,]
test_target<-target[idx5]
head(test_predictor,2)

#store the rest in the ¡°training¡± dataset
training_predictor<-predictor[-idx5,]
training_target<-target[-idx5]


#model <- C50(training_predictor, training_target )
#Boosting to increase the accuracy of the model with parameter trials
model <- C50::C5.0(training_predictor, training_target, trials=10)
summary(model)

#make predictions by inputting the model object into the predict() function
p <- predict(model, test_predictor, type="class")

#evaluate the accuracy of the model using the test data
sum( p == test_target ) / length( p )

detach(medicare)
