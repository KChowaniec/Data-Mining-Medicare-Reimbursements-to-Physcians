# Course: CS 513 A
# Final Project Name: Data Mining Medicare Reimbursements to Physicians
# Group 5 (on Canvas), Team 7 (in presentation)
#CART for Medicare Reimbursement Data

rm(list=ls())

#libraries
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

#read in csv file
medicare<-read.csv("MEDICARE PHYSICIAN DATA.csv")
attach(medicare)

#range of medicare payment information
max(Total.Medicare.Payment.Amount)
min(Total.Medicare.Payment.Amount)
median(Total.Medicare.Payment.Amount)

#categorize reimbursement as high, medium or low
medicare$reimburse<-ifelse(Total.Medicare.Payment.Amount >= 0 & Total.Medicare.Payment.Amount < 300000 ,'Low',ifelse(Total.Medicare.Payment.Amount >= 300000 & Total.Medicare.Payment.Amount < 500000 ,'Medium','High'))
medicare$reimburse<-as.factor(medicare$reimburse)


#calculate reimbursement rate (Total Payment Amount / Submitted Charges)
medicare$reimburse_rate<-(medicare$Total.Medicare.Payment.Amount/ medicare$Total.Submitted.Charges)

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

#create more general categories for provider type
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

#remove any rows that contain NA values
medicare<-na.omit(medicare)


#Gender analysis
females<-medicare[medicare$NPPES.Provider.Gender == "F",]
median(females$Total.Medicare.Payment.Amount) #median medicare payment for female doctors
table(females$specialty) #frequency table of specialties for females

males<-medicare[medicare$NPPES.Provider.Gender == "M",]
median(males$Total.Medicare.Payment.Amount) #median medicare payment for male doctors
table( males$specialty) #frequency table of specialties for males


table(medicare$NPPES.Provider.Gender, medicare$specialty) #frequency table of gender and specialty


#CART Plots


#REIMBURSEMENT RATE (RAW AMOUNT)
#reimbursement rate vs. life expectancy
life_expect<-data.frame(medicare$Average.Life.Expectancy)
expect_tree<-rpart(medicare$reimburse_rate~., data=life_expect)
expect_tree
fancyRpartPlot(expect_tree)

#reimbursement rate vs. specialty
specialty_rate<-data.frame(medicare$specialty)
specialty_tree<-rpart(medicare$reimburse_rate~., data=specialty_rate)
specialty_tree
fancyRpartPlot(specialty_tree)


#TOTAL PAYMENT (RAW AMOUNT)

#total payment vs num of services
services<-data.frame(medicare$Number.of.Services)
services_tree<-rpart(medicare$Total.Medicare.Payment.Amount~., data=services)
services_tree
fancyRpartPlot(services_tree)

#total payment vs num of beneficiaries
beneficiaries<-data.frame(medicare$Number.of.Unique.Beneficiaries)
beneficiary_tree<-rpart(medicare$Total.Medicare.Payment.Amount~., data=beneficiaries)
beneficiary_tree
fancyRpartPlot(beneficiary_tree)

#total payment vs gender
gender<-data.frame(medicare$NPPES.Provider.Gender)
gender_tree<-rpart(medicare$Total.Medicare.Payment.Amount~., data=gender)
gender_tree
fancyRpartPlot(gender_tree)


#TOTAL PAYMENT (CATEGORIZED)
#payment category vs. specialty
specialty_rate<-data.frame(medicare$specialty)
specialty_tree<-rpart(medicare$reimburse~., data=specialty_rate)
specialty_tree
fancyRpartPlot(specialty_tree)

#payment category vs num of services
services<-data.frame(medicare$Number.of.Services)
services_tree<-rpart(medicare$reimburse~., data=services)
services_tree
fancyRpartPlot(services_tree)


#max payment amount from dataset
max(medicare$Total.Medicare.Payment.Amount)

detach(medicare)
