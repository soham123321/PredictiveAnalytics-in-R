library(tidyverse)
library(RColorBrewer)
color_palette <- brewer.pal(5,'RdPu')
colorpalette <- brewer.pal(6, 'PuBuGn')
Colorpalette <- brewer.pal(9,'Purples')
ColorGreen <- brewer.pal(6,"Greens")
ColorylAccent <- brewer.pal(5, "Oranges")
ColorPastel <- brewer.pal(9,"YlGnBu")
Hotels <- read_csv('https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv')


##Box Plots

HotelsBox <- Hotels

canceled <- HotelsBox[HotelsBox$IsCanceled == 1,]
notCanceled <- HotelsBox[HotelsBox$IsCanceled == 0,]

ggplot(HotelsBox, aes(x= LeadTime , y= CustomerType), fill= "White") + 
  geom_boxplot(alpha=0.3, aes(fill = CustomerType)) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")

## Bar Plots
supp <- c("First Time Guest", "Repeated Guests")
names(supp) <- c("0","1")
ggplot(HotelsBox, aes(x= CustomerType , fill = factor(IsCanceled))) + 
  geom_bar(alpha=0.8, position = position_dodge()) +  theme(legend.position="none") + facet_grid(.~IsRepeatedGuest, labeller = labeller(supp = supp))
  scale_fill_brewer(palette="RdPu") 


library(rworldmap)

### Read the data and installed the needed libraries

# The below code is to create world level polygons to plot the map. The aggregate functions are used for gradients. Gradient is based on Number of Cancellations and Avg lead time countrywise.
NewHotels <- Hotels
NewHotels_CountryLevel <- NewHotels %>% group_by(Country) %>% summarize(number_of_cancellations = sum(IsCanceled), avg_lead_time = mean(LeadTime))
NewHotels_CountryMerge <- merge(NewHotels, NewHotels_CountryLevel, by.x = "Country", by.y = "Country")
countryHotelsData <- joinCountryData2Map(NewHotels_CountryMerge, joinCode = "ISO3",nameJoinColumn = "Country")

# Plotting the Maps 

Number_of_Cancellations <- mapCountryData(countryHotelsData,nameColumnToPlot = "number_of_cancellations", colourPalette = Colorpalette, catMethod = "categorical" , addLegend = FALSE)
Avg_Lead_Time <- mapCountryData(countryHotelsData,nameColumnToPlot = "avg_lead_time",colourPalette = "white2Black" ,catMethod = "categorical" , addLegend = FALSE)


# Creating a criteria for cancellations as per market segment. Gradient will display avg_lead_time and number of cancellations for different market segments of the hotels. 

NewHotels_MarketSegment <- NewHotels %>% group_by(Country,MarketSegment) %>% summarize(number_of_cancellations = sum(IsCanceled), avg_lead_time = mean(LeadTime))
NewHotels_MarketMerge <- merge(NewHotels,NewHotels_MarketSegment,by.x = "Country", by.y = "Country")

# Joining the world map data for plotting the map

MarketMerge<- joinCountryData2Map(NewHotels_MarketSegment, joinCode = "ISO3", nameJoinColumn = "Country")

# Creating market specific dataframes
complementary <- NewHotels_MarketMerge %>% filter(MarketSegment.x == "Complementary")
complementary <- joinCountryData2Map(complementary,joinCode = 'ISO3', nameJoinColumn = 'Country' )

corporate <- NewHotels_MarketMerge %>% filter(MarketSegment.x == "Corporate")
corporate <- joinCountryData2Map(corporate,joinCode = 'ISO3', nameJoinColumn = 'Country' )

direct <- NewHotels_MarketMerge %>% filter(MarketSegment.x == "Direct")
direct <- joinCountryData2Map(direct,joinCode = 'ISO3', nameJoinColumn = 'Country' )

group <- NewHotels_MarketMerge %>% filter(MarketSegment.x == "Groups")
group <- joinCountryData2Map(group,joinCode = 'ISO3', nameJoinColumn = 'Country' )

offline <- NewHotels_MarketMerge %>% filter(MarketSegment.x == "Offline TA/TO")
offline <- joinCountryData2Map(offline,joinCode = 'ISO3', nameJoinColumn = 'Country' )

online <- NewHotels_MarketMerge %>% filter(MarketSegment.x == "Online TA")
online <- joinCountryData2Map(online,joinCode = 'ISO3', nameJoinColumn = 'Country' )



# Plotting different maps as per market segments for avg_lead_time

MarketSeg_complementary <- mapCountryData(complementary,nameColumnToPlot = "avg_lead_time", colourPalette = color_palette,catMethod = "categorical")
MarketSeg_corporate <- mapCountryData(corporate,nameColumnToPlot = "avg_lead_time", colourPalette = color_palette,catMethod = "categorical")
MarketSeg_direct <- mapCountryData(direct,nameColumnToPlot = "avg_lead_time", colourPalette = color_palette, catMethod = "categorical")
MarketSeg_group <- mapCountryData(group,nameColumnToPlot = "avg_lead_time", colourPalette = color_palette, catMethod = "categorical")
MarketSeg_offline <- mapCountryData(offline,nameColumnToPlot = "avg_lead_time", colourPalette = color_palette, catMethod = "categorical")
MarketSeg_online <- mapCountryData(online,nameColumnToPlot = "avg_lead_time", colourPalette = ColorPastel, catMethod = "categorical", addLegend =  FALSE)

# Plotting different maps as per market segments for number of cancellations

MarketSeg_complementary_c <- mapCountryData(complementary,nameColumnToPlot = "number_of_cancellations", colourPalette = color_palette, catMethod = "categorical")
MarketSeg_corporate_c <- mapCountryData(corporate,nameColumnToPlot = "number_of_cancellations", colourPalette = color_palette, catMethod = "categorical")
MarketSeg_direct_c <- mapCountryData(direct,nameColumnToPlot = "number_of_cancellations", colourPalette = color_palette, catMethod = "categorical")
MarketSeg_group_c <- mapCountryData(group,nameColumnToPlot = "number_of_cancellations", colourPalette = color_palette, catMethod = "categorical")
MarketSeg_offline_c <- mapCountryData(offline,nameColumnToPlot = "number_of_cancellations", colourPalette = color_palette, catMethod = "categorical")
MarketSeg_online_c <- mapCountryData(online,nameColumnToPlot = "number_of_cancellations", colourPalette = ColorylAccent, catMethod = "categorical", addLegend = FALSE)

# Creating Dataframe for DepositType = No Deposit since its one of the higher frequency variables
NewHotels_DepositType <- NewHotels %>% group_by(Country,DepositType) %>% summarize(number_of_cancellations = sum(IsCanceled), avg_lead_time = mean(LeadTime))
NoDeposit_df <- NewHotels_DepositType %>% filter(DepositType == "No Deposit")

NoDeposit <- joinCountryData2Map(NoDeposit_df, joinCode = "ISO3", nameJoinColumn = "Country")
# Plotting the Graph
NoDeposit_cancellations <- mapCountryData(NoDeposit, nameColumnToPlot = "number_of_cancellations", catMethod = "categorical", addLegend = FALSE, colourPalette = color_palette)
NoDeposit_avgleadtime <- mapCountryData(NoDeposit, nameColumnToPlot = "avg_lead_time", catMethod = "categorical", colourPalette = colorpalette , addLegend = FALSE)

# Barplot for Customer type vs Number of Cancellations
Transient <- NewHotels %>% group_by(CustomerType) %>% summarize(Cancellations = sum(IsCanceled), LeadTime = mean(LeadTime))
ggplot(Transient, aes(x= CustomerType , y= Cancellations)) + 
  geom_bar(alpha=0.3, stat = 'identity', aes(fill = CustomerType)) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")

# Bar plot for market segment wise number of cancellations
ggplot(NewHotels_MarketMerge, aes(x= MarketSegment.x , y= number_of_cancellations), fill= "White") + 
  geom_bar(alpha=0.3, stat = 'identity', aes(fill = MarketSegment.x)) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")

# creating unsupervised ML models to understand the impact of different variables
HotelTrans <- data.frame(iscancelled = as.factor(Hotels$IsCanceled))
HotelTrans$StaysInWeekendNights <- as.factor(Hotels$StaysInWeekendNights)
HotelTrans$StaysInWeekNights <- as.factor(Hotels$StaysInWeekNights)
HotelTrans$Adults <- as.factor(Hotels$Adults)
HotelTrans$Children <- as.factor(Hotels$Children)
HotelTrans$Babies <- as.factor(Hotels$Babies)
HotelTrans$Meal <- as.factor(Hotels$Meal)
HotelTrans$MarketSegment <- as.factor(Hotels$MarketSegment)
HotelTrans$IsRepeatedGuest <- as.factor(Hotels$IsRepeatedGuest)
HotelTrans$PreviousCancellations <- as.factor(Hotels$PreviousCancellations)
HotelTrans$PreviousBookingNotCancelled <- as.factor(Hotels$PreviousBookingsNotCanceled)
HotelTrans$ReservedRoomType <- as.factor(Hotels$ReservedRoomType)
HotelTrans$AssignedRoomType <- as.factor(Hotels$AssignedRoomType)
HotelTrans$DepositType <- as.factor(Hotels$DepositType)
HotelTrans$CustomerType <- as.factor(Hotels$CustomerType)
HotelTrans$RequiredParkingSpaces <- as.factor(Hotels$RequiredCarParkingSpaces)
HotelTrans$TotalofSpecialRequests <- as.factor(Hotels$TotalOfSpecialRequests)
HotelTrans$LeadTime <- as.factor(Hotels$LeadTime < median(Hotels$LeadTime))

# All the above line of code is to create a dataframe with column data stored as factors

## The next line of code is to convert them  into transactions to create rules for unsupervised learning

library("arules")
library("arulesViz")


HotelTrans <- as(HotelTrans, "transactions")
summary(HotelTrans)
### ItemFrequeny to get the relative probability of all the column variables
itemFrequency(HotelTrans)
itemFrequencyPlot(HotelTrans,topN= 20)


rules_no_cancellation <- apriori(HotelTrans,parameter = list(support = 0.4, confidence = 0.8),
                              control = list(verbose = F),
                            appearance = list(default = 'lhs',rhs = 'iscancelled=0' ))
inspect(rules_no_cancellation)
# This tells us that the lead time, when less than the median lead time, leads to a rule with highest support and confidence. This can be an indication of it being the most influential parameter towards no cancellation

rules_cancellation <- apriori(HotelTrans,parameter = list(support = 0.15, confidence = 0.5),
                                 control = list(verbose = F),
                                 appearance = list(default = 'lhs',rhs = 'iscancelled=1' ))

inspect(rules_cancellation)


# The second rule tells us that the highest probability of a cancellation is when the customer is of the type transient and the lead time is very high(higher than the median) for the mentioned support and confidence 



# From further analysis of the rules, I think that we should use the variables LeadTime, CustomerType, RequiredParkingSpaces, Babies, DepositType and PreviousCancellations towards predicting whether a observation is a cancellation

## Machine Learning Models

library(caret)
library(kernlab)

Hotels_ML_DF <- NewHotels[,c("LeadTime", "CustomerType", "RequiredCarParkingSpaces","Babies","DepositType", "PreviousCancellations", "IsRepeatedGuest" ,"IsCanceled")]
Hotels_ML_DF$IsCanceled <- as.factor((Hotels_ML_DF$IsCanceled))
# Creating training and test datasets

trainSet_Hotels <- createDataPartition(y = Hotels_ML_DF$IsCanceled, p = 0.6, list = FALSE)
trainSet <- Hotels_ML_DF[trainSet_Hotels,]
testSet <- Hotels_ML_DF[-trainSet_Hotels,]

### Creating Support Vector Machine

HotelSVM <- ksvm(IsCanceled~., data = trainSet, C = 3, cross = 5 , prob.model= TRUE)
HotelSVM

predDF <- predict(HotelSVM, testSet)
confusionMatrix(predDF,testSet$IsCanceled)


# The values of C and cross have been selected after trying multiple values for both and the result with best accuracy was selected


