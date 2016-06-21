# Course: CS 513 A
# Final Project Name: Data Mining Medicare Reimbursements to Physicians
# Group 5 (on Canvas), Team 7 (in presentation)
#KMeans Clustering for Medicare Reimbursement Data

rm(list=ls())

mmnorm <-function(x,minx,maxx)
{
  z<-((x-minx)/(maxx-minx))
  return(z)                              
}

# install packages
#installed.packages()
#install.packages("ggplot2")

#load libraries
library(ggplot2)
library(class)

#Upload the dataset
medicare<-read.csv("MEDICARE PHYSICIAN DATA.csv")
attach(medicare)
View(medicare)

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

# Categorize the dataset into specialty to generalize all the provider type
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
  general_practice = c("General Practice", "Family Practice")
)

#Convert all the specialty type from categorical data to binary numerical data

medicare$surgery <- factor(medicare$specialty)
medicare$surgery <- ifelse(medicare$specialty == 'surgery',1 ,0)

medicare$radiation <- factor(medicare$specialty)
medicare$radiation <- ifelse(medicare$specialty == 'radiation',1 ,0)

medicare$cardiology <- factor(medicare$specialty)
medicare$cardiology <- ifelse(medicare$specialty == 'cardiology',1 ,0)

medicare$medicine <- factor(medicare$specialty)
medicare$medicine <- ifelse(medicare$specialty == 'medicine',1 ,0)

medicare$therapy <- factor(medicare$specialty)
medicare$therapy <- ifelse(medicare$specialty == 'therapy',1 ,0)

medicare$ER <- factor(medicare$specialty)
medicare$ER <- ifelse(medicare$specialty == 'ER',1 ,0)

medicare$psychology <- factor(medicare$specialty)
medicare$psychology <- ifelse(medicare$specialty == 'psychology',1 ,0)

medicare$neurology <- factor(medicare$specialty)
medicare$neurology <- ifelse(medicare$specialty == 'neurology',1 ,0)

medicare$oncology <- factor(medicare$specialty)
medicare$oncology <- ifelse(medicare$specialty == 'oncology',1 ,0)

medicare$hematology <- factor(medicare$specialty)
medicare$hematology <- ifelse(medicare$specialty == 'hematology',1 ,0)

medicare$allergy_pathology <- factor(medicare$specialty)
medicare$allergy_pathology <- ifelse(medicare$specialty == 'allergy_pathology',1 ,0)

medicare$anesthesiology <- factor(medicare$specialty)
medicare$anesthesiology <- ifelse(medicare$specialty == 'anesthesiology',1 ,0)

medicare$audiologist <- factor(medicare$specialty)
medicare$audiologist <- ifelse(medicare$specialty == 'audiologist',1 ,0)

medicare$chiropractor <- factor(medicare$specialty)
medicare$chiropractor <- ifelse(medicare$specialty == 'chiropractor',1 ,0)

medicare$dermatology <- factor(medicare$specialty)
medicare$dermatology <- ifelse(medicare$specialty == 'dermatology',1 ,0)

medicare$nephrology <- factor(medicare$specialty)
medicare$nephrology <- ifelse(medicare$specialty == 'nephrology',1 ,0)

medicare$endocrinology <- factor(medicare$specialty)
medicare$endocrinology <- ifelse(medicare$specialty == 'endocrinology',1 ,0)

medicare$gastroenterology <- factor(medicare$specialty)
medicare$gastroenterology <- ifelse(medicare$specialty == 'gastroenterology',1 ,0)

medicare$optometry <- factor(medicare$specialty)
medicare$optometry <- ifelse(medicare$specialty == 'optometry',1 ,0)

medicare$podiatry <- factor(medicare$specialty)
medicare$podiatry <- ifelse(medicare$specialty == 'podiatry',1 ,0)

medicare$urology <- factor(medicare$specialty)
medicare$urology <- ifelse(medicare$specialty == 'urology',1 ,0)

medicare$rheumatology <- factor(medicare$specialty)
medicare$rheumatology <- ifelse(medicare$specialty == 'rheumatology',1 ,0)

medicare$general_practice <- factor(medicare$specialty)
medicare$general_practice <- ifelse(medicare$specialty == 'general_practice',1 ,0)

medicare<-na.omit(medicare)
View(medicare)

# create dataframe to do knn analysis
medi<-data.frame(medicare$Number.of.Services, medicare$Number.of.Unique.Beneficiaries, medicare$Total.Medicare.Payment.Amount, 
                 medicare$surgery, medicare$radiation, medicare$cardiology, medicare$medicine, medicare$therapy, medicare$ER, 
                 medicare$psychology, medicare$neurology, medicare$oncology, medicare$hematology, medicare$allergy_pathology, 
                 medicare$anesthesiology, medicare$audiologist, medicare$chiropractor, medicare$dermatology, medicare$nephrology, 
                 medicare$endocrinology, medicare$gastroenterology, medicare$optometry, medicare$podiatry, medicare$urology, 
                 medicare$rheumatology, medicare$general_practice, medicare$NPPES.Provider.Gender)
View(medi)

# Read the data and:
# - Store every fifth record in a "test" dataset starting with the first record
every_fifth <- seq(from = 1, to = nrow(medi), by = 5)
test_data <- medi[every_fifth,]
View(test_data)

# - Store the rest in the "training" dataset
training_data <- medi[-every_fifth,]
View(training_data)

outcome<-training_data[,27]
predict<-knn(training_data[,-27],test_data[,-27],training_data[,27],k=1)
newresults<-cbind(test_data,as.character(predict))
table(newresults[,27],newresults[,28])
df<-as.data.frame(table(newresults[,27],newresults[,28]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_1 <- (good_predictions/total_predictions) * 100
successrate_K_1

outcome<-training_data[,27]
predict<-knn(training_data[,-27],test_data[,-27],training_data[,27],k=2)
newresults<-cbind(test_data,as.character(predict))
table(newresults[,27],newresults[,28])
df<-as.data.frame(table(newresults[,27],newresults[,28]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_2 <- (good_predictions/total_predictions) * 100
successrate_K_2

outcome<-training_data[,27]
predict<-knn(training_data[,-27],test_data[,-27],training_data[,27],k=3)
newresults<-cbind(test_data,as.character(predict))
table(newresults[,27],newresults[,28])
df<-as.data.frame(table(newresults[,27],newresults[,28]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_3 <- (good_predictions/total_predictions) * 100
successrate_K_3

outcome<-training_data[,27]
predict<-knn(training_data[,-27],test_data[,-27],training_data[,27],k=4)
newresults<-cbind(test_data,as.character(predict))
table(newresults[,27],newresults[,28])
df<-as.data.frame(table(newresults[,27],newresults[,28]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_4 <- (good_predictions/total_predictions) * 100
successrate_K_4

outcome<-training_data[,27]
predict<-knn(training_data[,-27],test_data[,-27],training_data[,27],k=5)
newresults<-cbind(test_data,as.character(predict))
table(newresults[,27],newresults[,28])
df<-as.data.frame(table(newresults[,27],newresults[,28]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_5 <- (good_predictions/total_predictions) * 100
successrate_K_5

outcome<-training_data[,27]
predict<-knn(training_data[,-27],test_data[,-27],training_data[,27],k=7)
newresults<-cbind(test_data,as.character(predict))
table(newresults[,27],newresults[,28])
df<-as.data.frame(table(newresults[,27],newresults[,28]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_7 <- (good_predictions/total_predictions) * 100
successrate_K_7

outcome<-training_data[,27]
predict<-knn(training_data[,-27],test_data[,-27],training_data[,27],k=9)
newresults<-cbind(test_data,as.character(predict))
table(newresults[,27],newresults[,28])
df<-as.data.frame(table(newresults[,27],newresults[,28]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_9 <- (good_predictions/total_predictions) * 100
successrate_K_9

outcome<-training_data[,27]
predict<-knn(training_data[,-27],test_data[,-27],training_data[,27],k=15)
newresults<-cbind(test_data,as.character(predict))
table(newresults[,27],newresults[,28])
df<-as.data.frame(table(newresults[,27],newresults[,28]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_15 <- (good_predictions/total_predictions) * 100
successrate_K_15

outcome<-training_data[,27]
predict<-knn(training_data[,-27],test_data[,-27],training_data[,27],k=50)
newresults<-cbind(test_data,as.character(predict))
table(newresults[,27],newresults[,28])
df<-as.data.frame(table(newresults[,27],newresults[,28]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_50 <- (good_predictions/total_predictions) * 100
successrate_K_50

outcome<-training_data[,27]
predict<-knn(training_data[,-27],test_data[,-27],training_data[,27],k=100)
newresults<-cbind(test_data,as.character(predict))
table(newresults[,27],newresults[,28])
df<-as.data.frame(table(newresults[,27],newresults[,28]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_100 <- (good_predictions/total_predictions) * 100
successrate_K_100

outcome<-training_data[,27]
predict<-knn(training_data[,-27],test_data[,-27],training_data[,27],k=150)
newresults<-cbind(test_data,as.character(predict))
table(newresults[,27],newresults[,28])
df<-as.data.frame(table(newresults[,27],newresults[,28]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_150 <- (good_predictions/total_predictions) * 100
successrate_K_150

# To visualize the accuracy of knn results

Value_of_k<-c(1,2,3,4,5,7,9,15,50,100)
Percentage_of_accuracy<-c(successrate_K_1,successrate_K_2,successrate_K_3,successrate_K_4,successrate_K_5,successrate_K_7,successrate_K_9,successrate_K_15,successrate_K_50,successrate_K_100)
ks<-cbind(Value_of_k,Percentage_of_accuracy)
plot(ks)