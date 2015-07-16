
###################################################################
#### Voucher Analysis

####  We have 3 dataframe
## List of voucher ID linked to ration card -- Each voucher equals a bit more than 15 USD --
## # of vouchers given to each family is calculated in relation with family size
## List of case with ration card id and profile
## A quick form filled during the shopping made with the voucher


source("code/packages.R")
 
## Reformat Voucher ID information
rm(voucher)
voucher <- read.csv("data/voucher.csv")

## Family size variable is not recognised a s numeric
voucher$Family.Size <- as.numeric(voucher$Family.Size)
### Soem fields are not normalised 

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




data$Timestamp.Received <- as.Date(as.character(data$Timestamp.Received), "%Y-%m-%d  %H:%M:%S") 
data$Timestamp.Scanned <- as.Date(as.character(data$Timestamp.Scanned), "%Y-%m-%d  %H:%M:%S") 
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
                                                   paste0(0),1)))

data.merge$Disinfectant <- as.numeric(with(data.merge, 
                                           ifelse(grepl("Disinfectant", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                  paste0(0),1)))

data.merge$Food <- as.numeric(with(data.merge, 
                                   ifelse(grepl("Food", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                          paste0(0),1)))

data.merge$Household.hardware.items <- as.numeric(with(data.merge, 
                                                       ifelse(grepl("Household hardware items", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                              paste0(0),1)))

data.merge$Other.hygiene.items <- as.numeric(with(data.merge, 
                                                  ifelse(grepl("Other hygiene items", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                         paste0(0),1)))

data.merge$Adult.shampoo <- as.numeric(with(data.merge, 
                                            ifelse(grepl("Adult shampoo", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                   paste0(0),1)))
data.merge$Baby.diapers <- as.numeric(with(data.merge, 
                                           ifelse(grepl("Baby diapers", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                  paste0(0),1)))
data.merge$Dishwashing.liquid <- as.numeric(with(data.merge, 
                                                 ifelse(grepl("Dishwashing liquid", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                        paste0(0),1)))
data.merge$Womens.sanitary.napkins <- as.numeric(with(data.merge, 
                                                      ifelse(grepl("Womens' sanitary napkins", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                             paste0(0),1)))
data.merge$Baby.shampoo <- as.numeric(with(data.merge, 
                                           ifelse(grepl("Baby shampoo", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                  paste0(0),1)))
data.merge$Other.items <- as.numeric(with(data.merge, 
                                          ifelse(grepl("Other items", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                 paste0(0),1)))
data.merge$Gas.bottle <- as.numeric(with(data.merge, 
                                         ifelse(grepl("Gas bottle", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                paste0(0),1)))
data.merge$Laundry.soap <- as.numeric(with(data.merge, 
                                           ifelse(grepl("Laundry soap", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                  paste0(0),1)))
data.merge$Soap.bars <- as.numeric(with(data.merge, 
                                        ifelse(grepl("Soap bars", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                               paste0(0),1)))

data.merge$Gas.bottle <- as.numeric(with(data.merge, 
                                         ifelse(grepl("Gas bottle", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.merge$Answer.1), 
                                                paste0(0),1)))




#########################################################################
#### Now Summmary of consumption per household
#names(data.merge)
#str(data.merge)






write.csv(data.merge, file = "out/datamerge.csv",na="")


#######################################################

#### Now focus 
data.1 <- data[data$Service.Name == "Zaatari_shopping",]
levels(data.1$Answer.1)
data.2 <- data[data$Service.Name == "Zaatari gas",]



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
data.cast <- dcast(data.merge, Barcode ~ Adult.diapers +   Disinfectant +
                  Food + Household.hardware.items + Other.hygiene.items + Adult.shampoo +
                    Baby.diapers + Dishwashing.liquid + Womens.sanitary.napkins +
                    Baby.shampoo + Other.items + Gas.bottle + Laundry.soap + Soap.bars, sum)

data.cast <- dcast(data.merge, Barcode ~ Adult.diapers , sum)
