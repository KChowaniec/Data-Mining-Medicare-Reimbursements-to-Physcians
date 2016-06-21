# Course: CS 513 A
# Final Project Name: Data Mining Medicare Reimbursements to Physicians
# Group 5 (on Canvas), Team 7 (in presentation)
#C5.0 Trees for Medicare Reimbursement Data

rm(list=ls())

#libraries
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library('C50')

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


#total payment category and gender
c5_tree<-data.frame(medicare$reimburse)
mytree<-C5.0(medicare$NPPES.Provider.Gender~., data=c5_tree)
summary(mytree)
plot(mytree, type="simple")

#total payment and region
c5_tree<-data.frame(medicare$reimburse)
mytree<-C5.0(medicare$region~., data=c5_tree)
summary(mytree)
plot(mytree, type="simple")

#total payment and region + gender
c5_tree<-data.frame(medicare$region, medicare$NPPES.Provider.Gender)
mytree<-C5.0(medicare$reimburse~., data=c5_tree)
summary(mytree)
plot(mytree, type="simple")

#total payment and state
c5_tree<-data.frame(medicare$NPPES.Provider.State)
mytree<-C5.0(medicare$reimburse~., data=c5_tree)
summary(mytree)

#total payment and specialty + gender
c5_tree<-data.frame(medicare$specialty, medicare$NPPES.Provider.Gender)
mytree<-C5.0(medicare$reimburse~., data=c5_tree)
summary(mytree)

detach(medicare)
