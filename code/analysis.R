
###################################################################
#### Voucher Analysis

####  We have 3 dataframe
## List of voucher ID linked to ration card -- Each voucher equals a bit more than 15 USD --
## # of vouchers given to each family is calculated in relation with family size
## List of case with ration card id and profile
## A quick form filled during the shopping made with the voucher


source("Documents/voucherUNHCR/Rscript/code/packages.R")
 
## Reformat Voucher ID information
rm(voucher)
voucher <- read.csv("Documents/voucherUNHCR/dataset/datamerge.csv")

## Family size variable is not recognised as numeric
voucher$Family.Size <- as.numeric(voucher$Family.Size)
### Some fields are not normalised 

voucher$PA.Gender..M.F. <- gsub("f", "F", voucher$PA.Gender..M.F.)
voucher$PA.Gender..M.F. <- gsub("m", "M", voucher$PA.Gender..M.F.)

voucher$Collector.gender..M.F. <- gsub("f", "F", voucher$Collector.gender..M.F.)
voucher$Collector.gender..M.F. <- gsub("m", "M", voucher$Collector.gender..M.F.)

#voucher$PA.Gender..M.F. <- ifelse( voucher$PA.Gender..M.F.=="F", "f" , voucher$PA.Gender..M.F.)
#voucher$PA.Gender..M.F. <- ifelse( voucher$PA.Gender..M.F.=="M", "m" , voucher$PA.Gender..M.F.)
#voucher$Collector.gender..M.F. <- ifelse( voucher$Collector.gender..M.F.=="F", "f" , voucher$Collector.gender..M.F.)
#voucher$Collector.gender..M.F. <- ifelse( voucher$Collector.gender..M.F.=="M", "m" , voucher$Collector.gender..M.F.)

names(voucher)

voucher.1 <- voucher [, c("Ration.Card..", "Collector.gender..M.F.", "PA.Gender..M.F.",
                          "Family.Size", "X..of.vouchers", "Voucher.1")]

voucher.2 <- voucher [, c("Ration.Card..", "Collector.gender..M.F.", "PA.Gender..M.F.",
                          "Family.Size", "X..of.vouchers", "Voucher.2")]

voucher.3 <- voucher [, c("Ration.Card..", "Collector.gender..M.F.", "PA.Gender..M.F.",
                          "Family.Size", "X..of.vouchers", "Voucher.3")]

voucher.4 <- voucher [, c("Ration.Card..", "Collector.gender..M.F.", "PA.Gender..M.F.",
                          "Family.Size", "X..of.vouchers", "Voucher.4")]

voucher.5 <- voucher [, c("Ration.Card..", "Collector.gender..M.F.", "PA.Gender..M.F.",
                          "Family.Size", "X..of.vouchers", "Voucher.5")]

voucher.6 <- voucher [, c("Ration.Card..", "Collector.gender..M.F.", "PA.Gender..M.F.",
                          "Family.Size", "X..of.vouchers", "Voucher.6")]

voucher.7 <- voucher [, c("Ration.Card..", "Collector.gender..M.F.", "PA.Gender..M.F.",
                          "Family.Size", "X..of.vouchers", "Voucher.7")]

voucher.8 <- voucher [, c("Ration.Card..", "Collector.gender..M.F.", "PA.Gender..M.F.",
                          "Family.Size", "X..of.vouchers", "Voucher.8")]

## Now rename vocuher column and append
voucher.1 <- rename(voucher.1, c("Voucher.1"="Voucher"))
voucher.2 <- rename(voucher.2, c("Voucher.2"="Voucher"))
voucher.3 <- rename(voucher.3, c("Voucher.3"="Voucher"))
voucher.4 <- rename(voucher.4, c("Voucher.4"="Voucher"))
voucher.5 <- rename(voucher.5, c("Voucher.5"="Voucher"))
voucher.6 <- rename(voucher.6, c("Voucher.6"="Voucher"))
voucher.7 <- rename(voucher.7, c("Voucher.7"="Voucher"))
voucher.8 <- rename(voucher.8, c("Voucher.8"="Voucher"))

voucher <- rbind(voucher.1, voucher.2, voucher.3, voucher.4, voucher.5, voucher.6, voucher.7, voucher.8)

rm(voucher.1)
rm(voucher.2)
rm(voucher.3)
rm(voucher.4)
rm(voucher.5)
rm(voucher.6)
rm(voucher.7)
rm(voucher.8)

## Check unique ID
uniquevoucher <- as.data.frame(unique(voucher$Voucher))

## We have duplicated voucher records that we need to take out before merging
voucher.dup <-   voucher[!duplicated(voucher$Voucher), ] 

voucher.dup2 <-   voucher.dup[!duplicated(voucher.dup$Ration.Card..), ] 


## We may need to clean the ration card before joining
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}


voucher.dup$Ration.Card <- toupper(trim(voucher.dup$Ration.Card..))

#################################################################
###### Now merge ration card with case profile
rm(progres.case)
progres.case <- read.csv("data/progrescase.csv")
progres.case.dup <-   progres.case[!duplicated(progres.case$CurrentRationCardNumber), ] 
voucher.case  <- merge(x=voucher.dup, y= progres.case.dup,  by.x="Ration.Card", by.y="CurrentRationCardNumber", all.x=TRUE) 
names(voucher.case)



###########################################################
## Add links between ration card and voucher
rm(data)
#data <- read.csv("data/extract-2015-03-02.csv") #, encoding="WIN1256"

data <- read.csv("data/123944scans_Jul16_045621.csv")


names(data)

data <- data[, c("Scan.ID" , "Service.Name" , "Device.Name" , "User.name"  ,       
                 "Barcode" ,    
                 "Timestamp.Scanned",  "Timestamp.Received",
                 "Question.1" ,  "Answer.1" ,         
                  "Question.2" ,  "Answer.2", 
                 "Question.3" , "Answer.3"  , 
                 "Question.4", "Answer.4"  )]




#data$Timestamp.Received <- as.Date(as.character(data$Timestamp.Received), "%Y-%m-%d  %H:%M:%S") 
#data$Timestamp.Scanned <- as.Date(as.character(data$Timestamp.Scanned), "%Y-%m-%d  %H:%M:%S") 

#summary(data)
#str(data)
#levels(data$Question.1)

## Check unique ID
uniquebarcode <- as.data.frame(unique(data$Barcode))
## We have duplicated voucher records that we need to take out before merging
data.dup <-   data[!duplicated(data$Barcode), ] 


###################################
## Now we can merge
rm(data.merge)
data.merge <- merge(x=data.dup, y=voucher.case, by.x="Barcode", by.y="Voucher" )

data.merge.all <- merge(x=data.dup, y=voucher.case, by.x="Barcode", by.y="Voucher", all.x=TRUE )

data.merge <- data.merge[ , c( "Ration.Card", "Barcode"  ,     "Scan.ID" ,  "Service.Name"  ,  "Device.Name"  ,  "User.name" ,
                               "Timestamp.Scanned" , "Timestamp.Received",
                               "Question.1"   ,  "Answer.1" ,  
                               "Question.2" ,    "Answer.2"  ,
                               "Question.3" , "Answer.3" ,
                               "Question.4"  ,  "Answer.4" ,             
                               "Collector.gender..M.F.", "PA.Gender..M.F."  ,
                               "Family.Size"   ,   "X..of.vouchers"    ,
                               "Num_Inds"  ,  "Child_0_14"  , "Child_0_17" ,   "Child_0_18"  ,
                               "percentage_0_14" ,  "percentage_0_17"   ,   "percentage_0_18" ,
                               "AVG_Age"   , "STDEV_Age"  ,  "Median_Age"   ,  
                               "Montharrival"  , "YearArrival"  ,
                               "arr_crosspoint" , "admlevel3"   ,  
                               "dem_marriage"  , "dem_age"  ,    "dem_sex",  "edu_highest"  )]


data.merge <- data.merge[!rowSums(is.na(data.merge["Num_Inds"])), ]
data.merge <- data.merge[!rowSums(is.na(data.merge["Scan.ID"])), ]

uniquerationcard <- as.data.frame(unique(data.merge$Ration.Card))

### Add a column per item type
#data.merge$gas.fill <- as.numeric(with(data.merge, ifelse(data$Service.Name == "Zaatari gas"), paste0(0), 1))

data.merge$Adult.diapers <- as.numeric(with(data.merge, 
                                            ifelse(grepl("Adult diapers", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                   paste0(1),0)))

data.merge$Disinfectant <- as.numeric(with(data.merge, 
                                           ifelse(grepl("Disinfectant", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                  paste0(1),0)))

data.merge$Food <- as.numeric(with(data.merge, 
                                   ifelse(grepl("Food", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                          paste0(1),0)))

data.merge$Household.hardware.items <- as.numeric(with(data.merge, 
                                                       ifelse(grepl("Household hardware items", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                              paste0(1),0)))

data.merge$Other.hygiene.items <- as.numeric(with(data.merge, 
                                                  ifelse(grepl("Other hygiene items", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                         paste0(1),0)))

data.merge$Adult.shampoo <- as.numeric(with(data.merge, 
                                            ifelse(grepl("Adult shampoo", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                   paste0(1),0)))
data.merge$Baby.diapers <- as.numeric(with(data.merge, 
                                           ifelse(grepl("Baby diapers", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                  paste0(1),0)))
data.merge$Dishwashing.liquid <- as.numeric(with(data.merge, 
                                                 ifelse(grepl("Dishwashing liquid", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                        paste0(1),0)))
data.merge$Womens.sanitary.napkins <- as.numeric(with(data.merge, 
                                                      ifelse(grepl("Womens' sanitary napkins", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                             paste0(1),0)))
data.merge$Baby.shampoo <- as.numeric(with(data.merge, 
                                           ifelse(grepl("Baby shampoo", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                  paste0(1),0)))
data.merge$Other.items <- as.numeric(with(data.merge, 
                                          ifelse(grepl("Other items", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                 paste0(1),0)))
data.merge$Gas.bottle <- as.numeric(with(data.merge, 
                                         ifelse(grepl("Gas bottle", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                paste0(1),0)))
data.merge$Laundry.soap <- as.numeric(with(data.merge, 
                                           ifelse(grepl("Laundry soap", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                  paste0(1),0)))
data.merge$Soap.bars <- as.numeric(with(data.merge, 
                                        ifelse(grepl("Soap bars", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                               paste0(1),0)))

data.merge$Gas.bottle <- as.numeric(with(data.merge, 
                                         ifelse(grepl("Gas bottle", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                paste0(1),0)))


#########################################################################
#### Last step
#names(data.merge)
#str(data.merge)

write.csv(data.merge, file = "out/datamerge.csv",na="")


#######################################################

#### Now focus 
#data.1 <- data[data$Service.Name == "Zaatari_shopping",]
#data.2 <- data[data$Service.Name == "Zaatari gas",]



#data.sum <- aggregate(data.merge$Barcode ,
#                      list(Adult.diapers  = data.merge$Adult.diapers ,          Disinfectant = data.merge$Disinfectant ,
#                           Food = data.merge$Food ,   Household.hardware.items = data.merge$Household.hardware.items , 
#                           Other.hygiene.items = data.merge$Other.hygiene.items ,      Adult.shampoo = data.merge$Adult.shampoo ,
#                           Baby.diapers = data.merge$Baby.diapers , Dishwashing.liquid = data.merge$Dishwashing.liquid ,
#                           Womens.sanitary.napkins = data.merge$Womens.sanitary.napkins ,  Baby.shampoo = data.merge$Baby.shampoo ,
#                           Other.items = data.merge$Other.items , Gas.bottle = data.merge$Gas.bottle ,
#                           Laundry.soap = data.merge$Laundry.soap , Soap.bars = data.merge$Soap.bars ),
#                      sum, na.rm = TRUE
#                    )
#data.cast <- dcast(data.merge, Barcode ~ Adult.diapers +   Disinfectant +
#                  Food + Household.hardware.items + Other.hygiene.items + Adult.shampoo +
#                    Baby.diapers + Dishwashing.liquid + Womens.sanitary.napkins +
#                    Baby.shampoo + Other.items + Gas.bottle + Laundry.soap + Soap.bars, sum)
#
#data.cast <- dcast(data.merge, Barcode ~ Adult.diapers , sum)









######################################################

###unique ration card
uniqueRationCard <- as.data.frame(unique(voucher$Ration.Card))
dim(uniqueRationCard)

###unique barcode
uniqueBarcode <- as.data.frame(unique(voucher$Barcode))

sum(voucher$X..of.vouchers)

#######################################################
###Gender of principal applicant
# $PA.Gender..M.F and $dem.sex are not identical, which is supposed to be?
identical(voucher$PA..Gender.M.F, voucher$dem.sex)

voucher.malePA1 <- voucher[voucher$PA.Gender..M.F == "M",] # male principal applicant  
voucher.femalePA1 <- voucher[voucher$PA.Gender..M.F =="F",] #female principal applicant
voucher.gendernaPA1 <- voucher[voucher$PA.Gender..M.F =="",] #female principal applicant

voucher.malePA2 <- voucher[voucher$dem_sex == "M",] # male principal applicant according to Refugee Registration database
voucher.femalePA2 <- voucher[voucher$dem_sex == "F",] # female principal applicant according to Refugee Registration database

### Gender of Voucher Collector
voucher.maleCollector <-  voucher[voucher$Collector.gender..M.F == "M",] # male collector 
voucher.femaleCollector <-  voucher[voucher$Collector.gender..M.F == "F",] # female collector 

### Family Size
# $Family.Size and $Num_Inds are not identicial, which is supposed to be?
FamilySizeCount1 <- table(voucher$Family.Size)
FamilySizeCount2 <- table(voucher$Num_Inds)
FamilySizeAverage1 <- mean(voucher$Family.Size)
FamilySizeAverage2 <- mean(voucher$Num_Inds)


######################################################
### shopping behavior
sum(voucher$Adult.diapers)
sum(voucher$Disinfectant)
sum(voucher$Food)
sum(voucher$Household.hardware.items)
sum(voucher$Other.hygiene.items)
sum(voucher$Adult.shampoo)
sum(voucher$Baby.diapers)
sum(voucher$Dishwashing.liquid)
sum(voucher$Womens.sanitary.napkins)
sum(voucher$Baby.shampoo)
sum(voucher$Other.items)
sum(voucher$Gas.bottle)
sum(voucher$laundry.soap)
sum(voucher$Laundry.soap)
sum(voucher$Soap.bars)

dim(voucher$Answer.3)
 
######################################################
###Generalized Linear Model on Answer 3 - "for sale" or "for family use"
purpose.glm <- glm(Answer.3 ~ Family.Size + PA.Gender..M.F. + X..of.vouchers + Num_Inds + admlevel3 
                   + AVG_Age + Child_0_14 + Child_0_17 + Child_0_18 + dem_marriage + dem_age + dem_sex + edu_highest 
                   + Adult.diapers + Disinfectant + Food + Household.hardware.items + Other.hygiene.items + Adult.shampoo + Baby.diapers 
                   + Dishwashing.liquid + Womens.sanitary.napkins + Baby.shampoo + Other.items + Gas.bottle + Laundry.soap + Soap.bars + Median_Age
                   ,data=voucher,family=binomial)
purpose.glm
summary(purpose.glm)

purpose.glm2 <- glm(Answer.3 ~  PA.Gender..M.F. + X..of.vouchers + Num_Inds + edu_highest + dem_marriage + dem_age + dem_sex
                    + Child_0_14 + Child_0_17 + Child_0_18 + Adult.diapers + Food + Household.hardware.items + Other.hygiene.items 
                    + Adult.shampoo + Baby.diapers + Dishwashing.liquid + Other.items + Gas.bottle + Laundry.soap + Soap.bars
                   # + Median_Age + admlevel3 + AVG_Age + Family.Size + Disinfectant + Womens.sanitary.napkins + Baby.shampoo
                   ,data=voucher,family=binomial)
purpose.glm2
summary(purpose.glm2)
anova(purpose.glm,purpose.glm2,test='Chisq')

# five-fold cross-validation
set.seed(17)
purpose.5=rep(0,5)
for(i in 1:5){
  purpose.glm <- glm(Answer.3 ~  PA.Gender..M.F. + X..of.vouchers + Num_Inds + edu_highest + dem_marriage + dem_age + dem_sex
                     + Child_0_14 + Child_0_17 + Child_0_18 + Adult.diapers + Food + Household.hardware.items + Other.hygiene.items 
                     + Adult.shampoo + Baby.diapers + Dishwashing.liquid + Other.items + Gas.bottle + Laundry.soap + Soap.bars
                     # + Median_Age + admlevel3 + AVG_Age + Family.Size + Disinfectant + Womens.sanitary.napkins + Baby.shampoo
                     ,data=voucher,family=binomial)
  purpose.5[i]=cv.glm(voucher,purpose.glm,K=5)$delta[1]
}
purpose.5

######################################################
#########clustering shopping behaviors
### hierarchical agglomerative
purchasingGoods <- c("Adult.diapers","Disinfectant","Food","Household.hardware.items","Other.hygiene.items","Adult.shampoo","Baby.diapers",
                    "Dishwashing.liquid","Womens.sanitary.napkins","Baby.shampoo","Other.items","Gas.bottle","Laundry.soap","Soap.bars")
purchasingData <- voucher[1:5000,purchasingGoods]

d <- dist(purchasingData, method="euclidean")
purchasing.fit <- hclust(d,method="ward.D")
plot(purchasing.fit)
groups <- cutree(purchasing.fit, k=3)
rect.hclust(purchasing.fit,k=3,border="red")

cluster.fit <- pvclust(purchasingData,method.hclust="ward.D",method.dist="euclidean")
plot(cluster.fit)

### k-means 
# preparing data
purchasingData2 <- voucher[purchasingGoods]
purchasingDataNAOmit <- na.omit(purchasingData2)
purchasingDataCleaned <- scale(purchasingDataNAOmit)

#k-means modeling
clustering.fit <- kmeans(purchasingDataCleaned,3)
aggregate(purchasingDataCleaned,by=list(clustering.fit$cluster),FUN=mean)

#visualizing the results
clusplot(purchasingDataCleaned,clustering.fit$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)
plotcluster(purchasingDataCleaned,clustering.fit$cluster)

######################################################
### supervised learning - neural network

### unsupervised learning - anomaly detection

