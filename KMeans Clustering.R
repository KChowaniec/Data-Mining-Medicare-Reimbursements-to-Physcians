# Course: CS 513 A
# Final Project Name: Data Mining Medicare Reimbursements to Physicians
# Group 5 (on Canvas), Team 7 (in presentation)
#KMeans Clustering for Medicare Reimbursement Data


rm(list=ls())

#normalization function
mnorm<-function(x,minx,max) {z<-((x-minx)/(max-minx)) #give min and max of x to get z value
return(z)
}
#read in csv file
medicare<-read.csv("MEDICARE PHYSICIAN DATA.csv")
attach(medicare)

#categorize what region state is in
medicare$region <- factor(NPPES.Provider.State) 
levels(medicare$region) <- list(
  north = c("CT","ME","MA","NH","RI","VT","NJ","NY","PA"),
  midwest = c("IL","MI","IN","OH","WI","IA","KS","MN","MO","NE","ND","SD"),
  south =c("DE","FL","GA","MD","NC","SC","WV","VA","DC"),
  southwest=c("AL", "KY","MS", "TN","AR","LA","OK","TX"),
  west=c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA")
)


#remove any rows that contain NA values
medicare<-na.omit(medicare)

#create new vector for region and normalized payment information
region.doc<-cbind( region=as.numeric(medicare$region),
                total_paid=mnorm(medicare$Total.Medicare.Payment.Amount,min(medicare$Total.Medicare.Payment.Amount, na.rm=TRUE),max(medicare$Total.Medicare.Payment.Amount, na.rm=TRUE)))

#conver to matrix
cluster<-as.matrix(region.doc)

#cluster by 5 regions
cl<-kmeans(cluster, 5)
cl
cl$cluster
cl$centers

#plot results
xlabels<-c("North", "Midwest", "South", "Southwest", "West")
plot(cluster,col=cl$cluster, main = "Region vs. Total Medicare Payment", xaxt = 'n')
axis(1, at = c(1:5), labels = xlabels, las = 1)

#convert gender to binary values
gender<-ifelse(medicare$NPPES.Provider.Gender=='M',1,0)

gender.doc<-cbind( gender,
                total_paid=mnorm(medicare$Total.Medicare.Payment.Amount,min(medicare$Total.Medicare.Payment.Amount, na.rm=TRUE),max(medicare$Total.Medicare.Payment.Amount, na.rm=TRUE)))

gender_cluster<-as.matrix(gender.doc)

#kmeans cluster for gender
kmeans_clust<-kmeans(gender_cluster, 2)
kmeans_clust
kmeans_clust$cluster
kmeans_clust$centers

#plot clusters
xlabels<-c("Female", "Male")
plot(gender_cluster, col=kmeans_clust$cluster, main = "Gender vs. Total Medicare Payment", xaxt = 'n')
axis(1, at = c(0:1), labels = xlabels, las = 1)

#compare total submitted charges to total medicare payment amounts
charges.doc<-cbind( submitted=mnorm(medicare$Total.Submitted.Charges, min(medicare$Total.Submitted.Charges), max(medicare$Total.Submitted.Charges)),
                   total_paid=mnorm(medicare$Total.Medicare.Payment.Amount,min(medicare$Total.Medicare.Payment.Amount, na.rm=TRUE),max(medicare$Total.Medicare.Payment.Amount, na.rm=TRUE)))

charges_cluster<-as.matrix(charges.doc)

kmeans_clust<-kmeans(charges_cluster, 3)
kmeans_clust
kmeans_clust$cluster
kmeans_clust$centers
#plot clusters
plot(charges_cluster,col=kmeans_clust$cluster, main="Total Submitted Charges vs Total Medicare Payment Amounts")

detach(medicare)
