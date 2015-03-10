data <- read.csv("data/extract-2015-03-02.csv")
summary(data)

str(data)

levels(data$Question.1)


## Add links between ration card and voucher

voucher <- read.csv("data/voucher.csv")

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


data.merge <- merge(x=data, y=voucher, by.x="Barcode", by.y="Voucher", all.x=TRUE)





#### Now focus 



data.1 <- data[data$Service.Name == "Zaatari_shopping",]
levels(data.1$Answer.1)
data.2 <- data[data$Service.Name == "Zaatari gas",]






### Add a column per item type
data.merge$gas.fill <- as.numeric(with(data.merge, 
                                            ifelse(data$Service.Name == "Zaatari gas"), 
                                                   paste0(0),1)))

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

#### Now Summmary of consumption per household


