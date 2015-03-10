data <- read.csv("data/extract-2015-03-02.csv")
summary(data)

str(data)

levels(data$Question.1)


## Add links between ration card and voucher

voucher <- read.csv("data/voucher.csv")


data.1 <- data[data$Service.Name == "Zaatari_shopping",]

levels(data.1$Answer.1)



### Add a column per item type
data.1$Adult.diapers <- as.numeric(with(data.1, 
                                        ifelse(grepl("Adult diapers", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.1$Answer.1), 
                                               paste0(0),1)))

data.1$Disinfectant <- as.numeric(with(data.1, 
                                        ifelse(grepl("Disinfectant", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.1$Answer.1), 
                                               paste0(0),1)))

data.1$Food <- as.numeric(with(data.1, 
                                        ifelse(grepl("Food", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.1$Answer.1), 
                                               paste0(0),1)))

data.1$Household.hardware.items <- as.numeric(with(data.1, 
                                        ifelse(grepl("Household hardware items", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.1$Answer.1), 
                                               paste0(0),1)))

data.1$Other.hygiene.items <- as.numeric(with(data.1, 
                                        ifelse(grepl("Other hygiene items", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.1$Answer.1), 
                                               paste0(0),1)))

data.1$Adult.shampoo <- as.numeric(with(data.1, 
                                        ifelse(grepl("Adult shampoo", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.1$Answer.1), 
                                               paste0(0),1)))
data.1$Baby.diapers <- as.numeric(with(data.1, 
                                        ifelse(grepl("Baby diapers", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.1$Answer.1), 
                                               paste0(0),1)))
data.1$Dishwashing.liquid <- as.numeric(with(data.1, 
                                        ifelse(grepl("Dishwashing liquid", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.1$Answer.1), 
                                               paste0(0),1)))
data.1$Womens.sanitary.napkins <- as.numeric(with(data.1, 
                                        ifelse(grepl("Womens' sanitary napkins", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.1$Answer.1), 
                                               paste0(0),1)))
data.1$Baby.shampoo <- as.numeric(with(data.1, 
                                        ifelse(grepl("Baby shampoo", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.1$Answer.1), 
                                               paste0(0),1)))
data.1$Other.items <- as.numeric(with(data.1, 
                                       ifelse(grepl("Other items", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.1$Answer.1), 
                                              paste0(0),1)))
data.1$Gas.bottle <- as.numeric(with(data.1, 
                                       ifelse(grepl("Gas bottle", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.1$Answer.1), 
                                              paste0(0),1)))
data.1$Laundry.soap <- as.numeric(with(data.1, 
                                       ifelse(grepl("Laundry soap", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.1$Answer.1), 
                                              paste0(0),1)))
data.1$Soap.bars <- as.numeric(with(data.1, 
                                       ifelse(grepl("Soap bars", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE, data.1$Answer.1), 
                                              paste0(0),1)))