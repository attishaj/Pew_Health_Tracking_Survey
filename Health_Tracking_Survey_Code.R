setwd("C:/datasets")
#the file below was pre-processed in Excel so that No=2 was transformed to No=0 (ie. yes=1 and no=0) for applicable variables
data <- read.csv("September_2012_health2_csv.csv", head=T)

#------------------------------------------------------
#DATA PRE-PROCESSING
#------------------------------------------------------

#select eligible variables
data<-data[,c(1:124)]
#select only those ppl who are internet users (2309 out of 3014 obs)
dataint<-data[which(data$intuse=='1'),]

#change all 8/9/98/99 values to NA because they mean "don't know" / "refuse to answer"
#[q23(vars 79-88),q29(vars 98-100),educ2(var 108),inc(var 120)] actually mean something for 8 and 9, so they're treated differently
q23<-dataint[,c(79:88)]
q29<-dataint[,c(98:100)]
educ2<-dataint[,108]
inc<-dataint[,120]


#change 8,9,98,99 to NA for these variables
dataint_no99<-dataint[,-c(79:88,98:100,108,120)]
dataint_no99[dataint_no99==8]<-NA
dataint_no99[dataint_no99==9]<-NA
dataint_no99[dataint_no99==98]<-NA
dataint_no99[dataint_no99==99]<-NA
dataint4<-dataint_no99

#change 98 and 99 to NA for these variables
good89vars<-cbind(q23,q29,educ2,inc)
good89vars[good89vars==98]<-NA
good89vars[good89vars==99]<-NA

#cbind all the variables back together
#this set will be used for association rules
dataint4<-cbind(dataint4,good89vars)

#-----
#create separate set for clustering
#to be used after the Data Analysis and Association Rules sections below
dataNumint4<-dataint4
#-----

#change all dataint4 variables (except age) from int to factor
for (i in c(1:90,92:124)) {   
  dataint4[,i]<-as.factor(dataint4[,i])
}
dataint5<-dataint4

#create a new age variable where age is binned
ageBin<-cut(dataint5$age, breaks=seq(9,99,by=10),labels=1:9)
ageBin[ageBin=="1"]<-"2" #combine 18 and 19 year olds with the 20 year olds
ageBin[ageBin=="8"]<-"7" #combine 70, 80, and 90 year olds together
ageBin[ageBin=="9"]<-"7"
#drop empty levels
ageBin<-droplevels(ageBin)
#change labels
levels(ageBin)<-c("18-29","30-39","40-49","50-59","60-69","70+")

#separate variables into 3 matrices 
demo<-dataint5[,c(9,10,13,15,92:99,103:104,123,124)] #numeric age variable not added
demo$ageBin<-ageBin #add the binned age variable to the "demo" matrix
health<-dataint5[,c(22:34,35,39,40,42,43)]
healthtech<-dataint5[,c(44:82,85:87,110:113,120)]

#------------------------------------------------------
#DATA ANALYSIS
#------------------------------------------------------

summary(demo$sex)
summary(demo$ageBin)
summary(dataint5$age)

library(psych)
describe(dataint5$age)

#use the original age variable to create a histogram for each decade
hist(dataint5$age,freq=TRUE,breaks=10, col=rainbow(10),
     xlim=c(0,100),main="Histogram of Age",xlab="Age (Years)")
summary(healthtech$q13)
summary(healthtech$q15)
summary(healthtech$q24)



prop.table(table(health$q2))
barplot(prop.table(table(health$q2)), 
        ylab="Percentage of Total Dataset (%)",xlab="Responses",col=rainbow(4), 
        main="In General, How Would You Rate Your Own Health?",
        names.arg=c("Excellent","Good","Fair","Poor"),ylim=c(0,1),yaxt="n")
axis(side=2,at=seq(0,1,by=.2),labels=FALSE)
text(par("usr")[1], seq(0,1,by=0.20), 
     labels = seq(0,100,by=20), pos=2, xpd = TRUE)



table2<-table(health$q2, demo$sex)
table2
barplot(prop.table(table2,2), 
        ylab="Percentage of Gender Group (%)",col=rainbow(4), 
        main="In General, How Would You Rate Your Own Health?", beside=TRUE, 
        names.arg=c("Male","Female"),ylim=c(0.00,1.00),yaxt="n")
axis(side=2,at=seq(0,1,by=.2),labels=FALSE)
text(par("usr")[1], seq(0,1,by=0.20), 
     labels = seq(0,100,by=20), pos=2, xpd = TRUE)
legend("topright",c("Excellent","Good", "Fair", "Poor"),cex=0.8,fill = rainbow(4))



table3<-table(health$q2, demo$inc)
table3
barplot(prop.table(table3,2), 
        ylab="Percentage of Income Group (%)",
        xlab="Income (U.S. Dollars)",
        col=rainbow(4), 
        main="In General, How Would You Rate Your Own Health?", 
        names.arg=c("","","","","","","","",""),
        beside=TRUE, 
        ylim=c(0.00,1.00),cex.names=0.55, yaxt="n")
text(x=seq(3.5,43.5,by=5),  par("usr")[3], 
     labels = c("  <$10K",
                "           $10K - 20K",
                "           $20K - 30K",
                "           $30K - 40K",
                "           $40K - 50K",
                "           $50K - 75K",
                "             $75K - 100K",
                "               $100K - 150K",
                "      >$150K"), 
     srt = 315, pos=1, xpd = TRUE, cex=0.7)
axis(side=2,at=seq(0,1,by=0.20),labels=FALSE)
text(par("usr")[1], seq(0,1,by=0.20), 
     labels = seq(0,100,by=20), pos=2, xpd = TRUE)
legend("topright",c("Excellent","Good", "Fair", "Poor"),cex=0.8,fill = rainbow(4))



table4<-table(health$q2, demo$ageBin)
table4
barplot(prop.table(table4,2),
        ylab="Percentage of Age Group (%)",
        xlab="Age Range (Years)",
        col=rainbow(4),
        main="In General, How Would You Rate Your Own Health?",
        beside=TRUE, ylim=c(0.00,1.00),yaxt="n")
axis(side=2,at=seq(0,1,by=0.20),labels=FALSE)
text(par("usr")[1], seq(0,1,by=0.20), 
     labels = seq(0,100,by=20), pos=2, xpd = TRUE)
legend("topright",c("Excellent","Good", "Fair", "Poor"),cex=0.8,fill = rainbow(4))



table5<-table(health$q2,demo$educ2)
table5
barplot(prop.table(table5,2),
        ylab="Percentage (%)",
        xlab="Education Level",
        col=rainbow(4),
        main="In General, How Would You Rate Your Own Health?",
        beside=TRUE,cex.names=0.8,ylim=c(0.00,1.00),yaxt="n")
axis(side=2,at=seq(0,1,by=0.20),labels=FALSE)
text(par("usr")[1], seq(0,1,by=0.20), 
     labels = seq(0,100,by=20), pos=2, xpd = TRUE)
legend("topright",c("Excellent","Good", "Only Fair", "Poor"),cex=0.7,fill = rainbow(4))



table6<-table(healthtech$q24,demo$ageBin)
table6
barplot(prop.table(table6,2),
        ylab="Percentage of Age Group (%)",
        xlab="Age Range (Years)",
        col=rainbow(2),
        main="Do you currently keep track of your own\nweight, diet, or exercise routine?",
        beside=TRUE, ylim=c(0.00,1.00),yaxt="n")
axis(side=2,at=seq(0,1,by=0.20),labels=FALSE)
text(par("usr")[1], seq(0,1,by=0.20), 
     labels = seq(0,100,by=20), pos=2, xpd = TRUE)
legend("topright",c("No", "Yes"),cex=0.8,fill = rainbow(2))



table7<-table(healthtech$q22,demo$ageBin)
prop.table(table7,2)
barplot(prop.table(table7,2),
        ylab="Percentage of Age Group (%)",
        xlab="Age Range (Years)",
        col=rainbow(2),
        main="On your cell phone, do you happen to have any software\napplications or apps that help you track or manage your health?",
        beside=TRUE, ylim=c(0.00,1.10),yaxt="n")
axis(side=2,at=seq(0,1,by=0.20),labels=FALSE)
text(par("usr")[1], seq(0,1,by=0.20), 
     labels = seq(0,100,by=20), pos=2, xpd = TRUE)
legend(1,1.09,c("No", "Yes"),cex=0.6,fill = rainbow(2))



summary(healthtech$q12)
tableq12<-table(healthtech$q12,demo$ageBin)
barplot(prop.table(tableq12,2),
        ylab="Percentage of Age Group (%)",
        xlab="Age Range (Years)",
        col=rainbow(2),
        main="Have you ever gone online to try to figure out\nwhat medical condition you or someone else might have?",
        beside=TRUE, ylim=c(0.00,1.00),yaxt="n")
axis(side=2,at=seq(0,1,by=0.20),labels=FALSE)
text(par("usr")[1], seq(0,1,by=0.20), 
     labels = seq(0,100,by=20), pos=2, xpd = TRUE)
legend("topright",c("No", "Yes"),cex=0.8,fill = rainbow(2))



plot(healthtech$q26m1, ylab="Frequency",xlab="Responses",col=rainbow(10),
     main="Thinking about the health indicator you pay the most attention to,\nhow do you keep track of changes? Do you use...")
legend(2,675,c("1: Paper (e.g. Notebook or Journal)",
               "2: Computer Program (e.g. Spreadsheet)",
               "3: Website or Other Online Tool",
               "4: App or Other Tool on Your Mobile Device",
               "5: Medical Device (e.g. Glucose Monitor)",
               "6: Keep Track in Your Head",
               "7: Other"), cex=0.7)


#------------------------------------------------
#ASSOCIATION RULES
#------------------------------------------------
install.packages("arules")
library(arules)


#1. assoc. rules for healthtech$q20a

#1a. demographic variables only
dataint20a<-cbind(demo,healthtech$q20a)
arules20a<-apriori(dataint20a,
                 parameter = list(minlen=2, maxlen=8, supp=0.01, conf=0.50),
                 appearance = list(rhs=c("healthtech$q20a=1"),
                                   default="lhs"),
                 control = list(verbose=TRUE))
arules20asorted<-sort(arules20a, by="lift")
inspect(arules20asorted)


#1b. health variables only
dataint20a3<-cbind(health,healthtech$q20a)
arules20a3<-apriori(dataint20a3,
                  parameter = list(minlen=2, maxlen=8, supp=0.01, conf=0.57),
                  appearance = list(rhs=c("healthtech$q20a=1"),
                                    default="lhs"),
                  control = list(verbose=TRUE))
arules20a3sorted<-sort(arules20a3, by="lift")
#inspect(arules20a3sorted)
subsetAR20a3<-is.subset(arules20a3sorted,arules20a3sorted)
subsetAR20a3[lower.tri(subsetAR20a3, diag=T)]<-NA
redundant<-colSums(subsetAR20a3, na.rm=T) >= 1
#which(redundant)
subsetAR20a3pruned<-arules20a3sorted[!redundant]
inspect(subsetAR20a3pruned)


#1c. both matrices combined
dataint20a4<-cbind(demo,health,healthtech$q20a)
arules20a4<-apriori(dataint20a4,
                   parameter = list(minlen=2, maxlen=7, supp=0.023, conf=0.505),
                   appearance = list(rhs=c("healthtech$q20a=1"),
                                     default="lhs"),
                   control = list(verbose=TRUE))
arules20a4sorted<-sort(arules20a4, by="lift")
#inspect(arules20a4sorted)
subsetAR20a4<-is.subset(arules20a4sorted,arules20a4sorted)
subsetAR20a4[lower.tri(subsetAR20a4, diag=T)]<-NA
redundant<-colSums(subsetAR20a4, na.rm=T) >= 1
#which(redundant)
subsetAR20a4pruned<-arules20a4sorted[!redundant]
inspect(subsetAR20a4pruned)



#-----
#2. assoc. rules for healthtech$q24

#2a. demographic variables only
#summary(healthtech$q24)
dataint24d<-cbind(demo,healthtech$q24)
arules24d<-apriori(dataint24d,
                 parameter = list(minlen=2, maxlen=7, supp=0.05, conf=0.75),
                 appearance = list(rhs=c("healthtech$q24=1","healthtech$q24=0"),
                                   default="lhs"),
                 control = list(verbose=TRUE))
arules24dsorted<-sort(arules24d, by="lift")
#inspect(arules24dsorted)
subsetAR24d<-is.subset(arules24dsorted,arules24dsorted)
subsetAR24d[lower.tri(subsetAR24d, diag=T)]<-NA
redundant<-colSums(subsetAR24d, na.rm=T) >= 1
#which(redundant)
subsetAR24dpruned<-arules24dsorted[!redundant]
inspect(subsetAR24dpruned)


#2b. health variables only
dataint24h<-cbind(health,healthtech$q24)
arules24h<-apriori(dataint24h,
                   parameter = list(minlen=2, maxlen=7, supp=0.029, conf=0.79),
                   appearance = list(rhs=c("healthtech$q24=1","healthtech$q24=0"),
                                     default="lhs"),
                   control = list(verbose=TRUE))
arules24hsorted<-sort(arules24h, by="lift")
#inspect(arules24hsorted)
subsetAR24h<-is.subset(arules24hsorted,arules24hsorted)
subsetAR24h[lower.tri(subsetAR24h, diag=T)]<-NA
redundant<-colSums(subsetAR24h, na.rm=T) >= 1
#which(redundant)
subsetAR24hpruned<-arules24hsorted[!redundant]
inspect(subsetAR24hpruned)


#2c. both matrices combined
dataint24b<-cbind(demo,health,healthtech$q24)
arules24b<-apriori(dataint24b,
                   parameter = list(minlen=2, maxlen=7, supp=0.048, conf=0.80),
                   appearance = list(rhs=c("healthtech$q24=1"),
                                     default="lhs"),
                   control = list(verbose=TRUE))
arules24bsorted<-sort(arules24b, by="lift")
#inspect(arules24bsorted)
subsetAR24b<-is.subset(arules24bsorted,arules24bsorted)
subsetAR24b[lower.tri(subsetAR24b, diag=T)]<-NA
redundant<-colSums(subsetAR24b, na.rm=T) >= 1
#which(redundant)
subsetAR24bpruned<-arules24bsorted[!redundant]
inspect(subsetAR24bpruned)




#-----
#3. assoc. rules for healthtech$q26m1

#3a. demographic variables only
#summary(healthtech$q26m1)
dataint26d<-cbind(demo,healthtech$q26m1)
arules26d<-apriori(dataint26d,
                    parameter = list(minlen=2, maxlen=7, supp=0.01, conf=0.60),
                    appearance = list(rhs=c("healthtech$q26m1=1"),
                                      default="lhs"),
                    control = list(verbose=TRUE))
arules26dsorted<-sort(arules26d, by="lift")
#inspect(arules26dsorted)
subsetAR26d<-is.subset(arules26dsorted,arules26dsorted)
subsetAR26d[lower.tri(subsetAR26d, diag=T)]<-NA
redundant<-colSums(subsetAR26d, na.rm=T) >= 1
#which(redundant)
subsetAR26dpruned<-arules26dsorted[!redundant]
inspect(subsetAR26dpruned)


#3b. health variables only
dataint26h<-cbind(health,healthtech$q26m1)
arules26h<-apriori(dataint26h,
                   parameter = list(minlen=2, maxlen=7, supp=0.01, conf=0.70),
                   appearance = list(rhs=c("healthtech$q26m1=1"),
                                     default="lhs"),
                   control = list(verbose=TRUE))
arules26hsorted<-sort(arules26h, by="lift")
inspect(arules26hsorted)


#3c. both matrices combined
dataint26b<-cbind(demo,health,healthtech$q26m1)
arules26b<-apriori(dataint26b,
                   parameter = list(minlen=2, maxlen=7, supp=0.02, conf=0.60),
                   appearance = list(rhs=c("healthtech$q26m1=1"),
                                     default="lhs"),
                   control = list(verbose=TRUE))
arules26bsorted<-sort(arules26b, by="lift")
inspect(arules26bsorted)


#------------------------------------------------
#CLUSTERING
#------------------------------------------------

#separate variables into 3 matrices (using dataNumint4 dataset saved specifically for clustering in the pre-processing section)
healthNum<-dataNumint4[,c(22:34,35,39,40,42,43)]
healthtechNum<-dataNumint4[,c(44:82,85:87,110:113,120)]
demoNum<-dataNumint4[,c(9,10,13,15,91:99,103:104,123,124)]
names(demoNum)[names(demoNum)=="educ2"] <- "educ2Num"
names(demoNum)[names(demoNum)=="inc"] <- "incNum"

#normalize variables not in the [0,1] range via decimal scaling
summary(demoNum)
demoNum$sex<-demoNum$sex/10
demoNum$q1<-demoNum$q1/10
demoNum$age<-demoNum$age/100
demoNum$mar<-demoNum$mar/10
demoNum$hh1<-demoNum$hh1/10
demoNum$emplnw<-demoNum$emplnw/10
demoNum$race<-demoNum$race/10
demoNum$educ2Num<-demoNum$educ2Num/10
demoNum$incNum<-demoNum$incNum/10

summary(healthNum)
healthNum$q2<-healthNum$q2/10
healthNum$q5a<-healthNum$q5a/10
healthNum$q5b<-healthNum$q5b/10
healthNum$q5c<-healthNum$q5c/10
healthNum$care9a<-healthNum$care9a/10
healthNum$care9b<-healthNum$care9b/10


#-----CLUSTERING-----
# K-Means Cluster Analysis
dataDHt<-cbind(demoNum,healthtechNum)
head(dataDHt)
summary(dataDHt)
#remove variables with large portions of NA's (300+)
dataDHt<-dataDHt[,-c(4,30:37,43:44,51,55:64)]
#remove obs with any NA's
dataDHt<-na.omit(dataDHt)

#scree plot of k vs. withinss 
wss<-(nrow(dataDHt)-1)*sum(apply(dataDHt,2,var))
for (i in 2:15) wss[i] <- (kmeans(dataDHt,i)$tot.withinss)
plot(1:15, wss, type="l", xlab="Number of Clusters (k)", ylab="Total Within-Cluster Scatter",
     main="Scree Plot 1")


#set.seed(123456)
clustDHt.kmeans<-kmeans(dataDHt,5)
clustDHt.kmeans
clustDHt.kmeans$centers
clustDHt.kmeans$size
clustDHt.kmeans$withinss
clustDHt.kmeans$tot.withinss



#-----------------
#Cluster health and health tech variables
dataHHt<-cbind(healthNum,healthtechNum)
summary(dataHHt)
head(dataHHt)
#remove variables with large portions of NA's (300+) 
dataHHt<-dataHHt[,-c(16:18,31:38,44:45,52,56:65)]
#remove obs with any NA's
dataHHt<-na.omit(dataHHt)

#scree plot of k vs. withinss 
wss<-(nrow(dataHHt)-1)*sum(apply(dataHHt,2,var))
for (i in 2:15) wss[i] <- (kmeans(dataHHt,i)$tot.withinss)
plot(1:15, wss, type="l", xlab="Number of Clusters (k)", 
     ylab="Total Within-Cluster Scatter",
     main="Scree Plot 2")

#set.seed(123456)
clustHHt.kmeans<-kmeans(dataHHt,5)
clustHHt.kmeans
clustHHt.kmeans$centers
clustHHt.kmeans$size
clustHHt.kmeans$withinss
clustHHt.kmeans$tot.withinss



#-----------------
#Cluster all three types of variables
dataAll<-cbind(demoNum,healthNum,healthtechNum)
summary(dataAll)
head(dataAll)
#remove variables with large portions of NA's (300+) 
dataAll<-dataAll[,-c(4,33:35,48:55,61:62,69,73:82)]
#remove obs with any NA's
dataAll<-na.omit(dataAll)

#scree plot of k vs. withinss 
wss<-(nrow(dataAll)-1)*sum(apply(dataAll,2,var))
for (i in 2:15) wss[i] <- (kmeans(dataAll,i)$tot.withinss)
plot(1:15, wss, type="l", xlab="Number of Clusters (k)", ylab="Total Within-Cluster Scatter",
     main="Scree Plot")

clustAll.kmeans<-kmeans(dataAll,5)
clustAll.kmeans
clustAll.kmeans$centers
clustAll.kmeans$size
clustAll.kmeans$withinss
clustAll.kmeans$tot.withinss


#that's all folks!