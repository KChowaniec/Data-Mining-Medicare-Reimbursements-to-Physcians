rm(list=ls())

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library('C50')

#normalization function
mnorm<-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx)) #give min and max of x to get z value
return(z)
}
#read in csv file
medicare<-read.csv("Random Sample MF Medicare.csv")
attach(medicare)

#categorize reimbursement as high, medium or low
medicare$reimburse<-ifelse(Total.Medicare.Payment.Amount >= 0 & Total.Medicare.Payment.Amount < 60000 ,'Low',ifelse(Total.Medicare.Payment.Amount >= 60000 & Total.Medicare.Payment.Amount < 120000 ,'Medium','High'))
medicare$reimburse<-as.factor(medicare$reimburse)

medicare$high_reimburse<-ifelse(Total.Medicare.Payment.Amount >= 1000000 ,'Yes', 'No')
medicare$high_reimburse<-as.factor(medicare$high_reimburse)

#categorize average state income (high, medium, low)
medicare$income_level<-ifelse(State.Average.Income >= 0 & State.Average.Income < 45000 ,'Low',ifelse(State.Average.Income >= 45000 & State.Average.Income < 60000 ,'Medium','High'))
medicare$income_level<-as.factor(medicare$income_level)

#categorize what region state is in
medicare$region <- factor(NPPES.Provider.State) 
levels(medicare$region) <- list(
  north = c("CT","ME","MA","NH","RI","VT","NJ","NY","PA"),
  midwest = c("IL","MI","IN","OH","WI","IA","KS","MN","MO","NE","ND","SD"),
  south =c("DE","FL","GA","MD","NC","SC","WV","VA","DC","AL","KY","MS","TN","AR","LA","OK","TX"),
  west=c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA")
)
medicare$region<-as.factor(medicare$region)

medicare$NPPES.Provider.Gender<-as.factor(medicare$NPPES.Provider.Gender)

#remove any rows that contain NA values
medicare<-na.omit(medicare)


#doctor2$medcare_level<-cut(doctor2$Total.Medicare.Payment.Amount,c(0.0,50000,100000,max(doctor2$Total.Medicare.Payment.Amount)),labels=c("Low","Medium","High"))


?rpart()
#outcome has to be 2 factor
my_data<-data.frame(medicare$Number.of.Unique.Beneficiaries)
mytree<-rpart(medicare$Total.Medicare.Payment.Amount~.,data= my_data) 
mytree
print(mytree)
fancyRpartPlot(mytree)

#C50
mytree<-C5.0(medicare$high_reimburse~medicare$region, data=medicare)
summary(mytree)
mytree
str(mytree)
attr(mytree, "class")
treeout<-mytree$output




