#CMTH 8120 

#Load Dataset
housing <- read.csv('Housingdata.csv', header = TRUE)

#Data Summary
summary(housing)
str(housing)
housing <- subset(housing,select = -c(X))

#Change Sale price variable from factor to numeric
housing$SALE.PRICE <- gsub(",","",housing$SALE.PRICE)
housing$SALE.PRICE <- as.numeric(as.character(housing$SALE.PRICE)) 
housing <- subset(housing,housing$SALE.PRICE != 0) #Remove records with no sales price data
housing <- subset(housing,housing$SALE.PRICE != 1) #Remove records with sales price equal to 1 as it suggests there is no information
housing <- subset(housing,housing$SALE.PRICE < 4000000) #Remove records that are equal to and over 4,000,000 
housing <- subset(housing,housing$SALE.PRICE < 1500000) #Remove records that are over 1,500,000

#Box plot and histogram to investigate the variable
boxplot(housing$SALE.PRICE)
hist(housing$SALE.PRICE)

#Change the format of other variables
housing$BOROUGH <- as.factor(housing$BOROUGH)
housing$TAX.CLASS.AT.TIME.OF.SALE <- as.factor(housing$TAX.CLASS.AT.TIME.OF.SALE)
housing$SALE.DATE <- as.Date(housing$SALE.DATE)
housing$Community.Board <- as.factor(housing$Community.Board)
housing$Council.District <- as.factor(housing$Council.District)
housing$ADDRESS <- as.character(housing$ADDRESS)

#Did not change the format of apartment number field as it does not have provide useful information 
housing$TOTAL.UNITS <- gsub(",","",housing$TOTAL.UNITS)
housing$TOTAL.UNITS <- as.numeric(as.character(housing$TOTAL.UNITS)) 

housing$RESIDENTIAL.UNITS <- gsub(",","",housing$RESIDENTIAL.UNITS)
housing$RESIDENTIAL.UNITS <- as.numeric(as.character(housing$RESIDENTIAL.UNITS)) 

housing$COMMERCIAL.UNITS <- gsub(",","",housing$COMMERCIAL.UNITS)
housing$COMMERCIAL.UNITS <- as.numeric(as.character(housing$COMMERCIAL.UNITS)) 

housing$LAND.SQUARE.FEET <- gsub("- 0","0",housing$LAND.SQUARE.FEET)
housing$LAND.SQUARE.FEET <- gsub("","0",housing$LAND.SQUARE.FEET)
housing$LAND.SQUARE.FEET <- gsub(",","",housing$LAND.SQUARE.FEET)
housing$LAND.SQUARE.FEET <- as.numeric(housing$LAND.SQUARE.FEET)

housing$GROSS.SQUARE.FEET <- gsub("- 0","0",housing$GROSS.SQUARE.FEET)
housing$GROSS.SQUARE.FEET <- gsub("","0",housing$GROSS.SQUARE.FEET)
housing$GROSS.SQUARE.FEET <- gsub(",","",housing$GROSS.SQUARE.FEET)
housing$GROSS.SQUARE.FEET <- as.numeric(housing$GROSS.SQUARE.FEET)

housing$YEAR.BUILT <- gsub(",","",housing$YEAR.BUILT)

#Transformed Data Summary
summary(housing)
str(housing)

#Tax class 1 and 2 are residential and building class category 3 and 4 are commercial
#Handle missing values in # of apartments - replace all 0 number of units with 1 res where class is 1 and 2, and 1 commercial where class is 3 and 4
#Get the count of rows with 0 residential unitls and tax class a 1:
nrow(housing[housing$RESIDENTIAL.UNITS == "0" & housing$TAX.CLASS.AT.TIME.OF.SALE == "1",])

housing[is.na(housing$RESIDENTIAL.UNITS) & housing$TAX.CLASS.AT.TIME.OF.SALE == "1","RESIDENTIAL.UNITS"] <- 1 #Replace all NA where Tax class is 1 with 1 residential 
housing[is.na(housing$RESIDENTIAL.UNITS) & housing$TAX.CLASS.AT.TIME.OF.SALE == "2","RESIDENTIAL.UNITS"] <- 1 #Replace all NA where Tax class is 2 with 1 residential 
housing[is.na(housing$RESIDENTIAL.UNITS) & housing$TAX.CLASS.AT.TIME.OF.SALE == "3","RESIDENTIAL.UNITS"] <- 0 #Replace all NA where Tax class is 3 with 0 residential 
housing[is.na(housing$RESIDENTIAL.UNITS) & housing$TAX.CLASS.AT.TIME.OF.SALE == "4","RESIDENTIAL.UNITS"] <- 0 #Replace all NA where Tax class is 4 with 0 residential 

housing[is.na(housing$COMMERCIAL.UNITS) & housing$TAX.CLASS.AT.TIME.OF.SALE == "1","COMMERCIAL.UNITS"] <- 0 #Replace all NA where Tax class is 1 with 0 Commercial  
housing[is.na(housing$COMMERCIAL.UNITS) & housing$TAX.CLASS.AT.TIME.OF.SALE == "2","COMMERCIAL.UNITS"] <- 0 #Replace all NA where Tax class is 2 with 0 Commercial 
housing[is.na(housing$COMMERCIAL.UNITS) & housing$TAX.CLASS.AT.TIME.OF.SALE == "3","COMMERCIAL.UNITS"] <- 1 #Replace all NA where Tax class is 3 with 1 Commercial 
housing[is.na(housing$COMMERCIAL.UNITS) & housing$TAX.CLASS.AT.TIME.OF.SALE == "4","COMMERCIAL.UNITS"] <- 1 #Replace all NA where Tax class is 4 with 1 Commercial 

housing[housing$RESIDENTIAL.UNITS == 0 & housing$TOTAL.UNITS == 0 & housing$TAX.CLASS.AT.TIME.OF.SALE == "1","RESIDENTIAL.UNITS"] <- 1 #Replace all 0 where Tax class is 1 with 1 residential 
housing[housing$RESIDENTIAL.UNITS == 0 & housing$TOTAL.UNITS == 0 & housing$TAX.CLASS.AT.TIME.OF.SALE == "2","RESIDENTIAL.UNITS"] <- 1 #Replace all 0 where Tax class is 2 with 1 residential

housing[housing$COMMERCIAL.UNITS == 0 & housing$TOTAL.UNITS == 0 & housing$TAX.CLASS.AT.TIME.OF.SALE == "3","COMMERCIAL.UNITS"] <- 1 #Replace all 0 where Tax class is 3 with 1 Commercial  
housing[housing$COMMERCIAL.UNITS == 0 & housing$TOTAL.UNITS == 0 & housing$TAX.CLASS.AT.TIME.OF.SALE == "4","COMMERCIAL.UNITS"] <- 1 #Replace all 0 where Tax class is 4 with 1 Commercial  

#recalculate Total units after the 0s have been updated correctly
housing$TOTAL.UNITS <- housing$RESIDENTIAL.UNITS + housing$COMMERCIAL.UNITS

#Investigate residential unit and Commercial units using box plot 
boxplot(housing$RESIDENTIAL.UNITS) 
boxplot(housing$COMMERCIAL.UNITS) 

#remove records with 500 or more residential units and records with more than 400 commercail units to remove outliers
housing <- housing[housing$RESIDENTIAL.UNITS < 100,]
housing <- housing[housing$COMMERCIAL.UNITS < 80,]
summary(housing$RESIDENTIAL.UNITS)
summary(housing$COMMERCIAL.UNITS)
summary(housing$TOTAL.UNITS)
boxplot(housing$RESIDENTIAL.UNITS) 
boxplot(housing$COMMERCIAL.UNITS) 

#Investigate and remove outliers from square feet
boxplot(housing$GROSS.SQUARE.FEET)
housing <- housing[housing$GROSS.SQUARE.FEET < 50000000000,] 
boxplot(housing$LAND.SQUARE.FEET)
housing <- housing[housing$LAND.SQUARE.FEET < 50000000000,] 
boxplot(housing$GROSS.SQUARE.FEET)
boxplot(housing$LAND.SQUARE.FEET)

#Dropping 10 Variables that are not useful for the analysis
CleanHousing <- housing[,c(1,2,3,4,5,6,8,11,12,13,14,15,16,17,18,19,20,21,29)]
str(CleanHousing)


#Moving sales price to be the first column
col_idx <- grep("SALE.PRICE", names(CleanHousing))
CleanHousing <- CleanHousing[, c(col_idx, (1:ncol(CleanHousing))[-col_idx])]
names(CleanHousing)

#Removing records with Tax calss as of final roll as 3 and 4 as they are not residential units
CleanHousing <- CleanHousing[CleanHousing$TAX.CLASS.AS.OF.FINAL.ROLL !="4",]
CleanHousing <- CleanHousing[CleanHousing$TAX.CLASS.AS.OF.FINAL.ROLL !="3",]
CleanHousing <- CleanHousing[CleanHousing$TAX.CLASS.AS.OF.FINAL.ROLL !="",]
# Tax class roll value 1 relates to most residential which are less than 3 storey, and 2 relates to all other properties including mix use so putting into categories

#change 1a,b,c,d to 1 and 2a,b,c to 2
levels(CleanHousing$TAX.CLASS.AS.OF.FINAL.ROLL)[3] <- "1"
levels(CleanHousing$TAX.CLASS.AS.OF.FINAL.ROLL)[3] <- "1"
levels(CleanHousing$TAX.CLASS.AS.OF.FINAL.ROLL)[3] <- "1"
levels(CleanHousing$TAX.CLASS.AS.OF.FINAL.ROLL)[3] <- "1"
levels(CleanHousing$TAX.CLASS.AS.OF.FINAL.ROLL)[4] <- "2"
levels(CleanHousing$TAX.CLASS.AS.OF.FINAL.ROLL)[4] <- "2"
levels(CleanHousing$TAX.CLASS.AS.OF.FINAL.ROLL)[4] <- "2"
levels(CleanHousing$TAX.CLASS.AS.OF.FINAL.ROLL)[1] <- "1"
levels(CleanHousing$TAX.CLASS.AS.OF.FINAL.ROLL)[3] <- "2"
levels(CleanHousing$TAX.CLASS.AS.OF.FINAL.ROLL)[3] <- "2"

#Bar graph Plot to explore Categorical varibales:
#install.packages("ggplot2")
#install.packages("ggplot2",repos = "http://cran.us.r-project.org")
library(ggplot2)
options(repr.plot.width=5, repr.plot.height=4)
ggplot(CleanHousing, aes(x = BOROUGH, fill = BOROUGH )) + 
  geom_bar()+ 
  scale_fill_hue(c = 80)+
  ggtitle("Distribution of Boroughs")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="right", 
  legend.background = element_rect(fill="grey90",size=0.5, linetype="solid",colour ="black"))+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)

options(repr.plot.width=5, repr.plot.height=4)
ggplot(CleanHousing, aes(x = TAX.CLASS.AS.OF.FINAL.ROLL, fill = TAX.CLASS.AS.OF.FINAL.ROLL )) + 
  geom_bar()+ 
  scale_fill_hue(c = 80)+
  ggtitle("Distribution of Tax class")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="right", 
        legend.background = element_rect(fill="grey90",size=0.5, linetype="solid",colour ="black"))+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)

#average number of units per borough:
library(plyr)
ddply(CleanHousing, .(BOROUGH), summarize,  size=mean(TOTAL.UNITS))


#get a summary of price based on building type or location 
library(plyr)
ddply(CleanHousing, .(BOROUGH), summarize,Total = length(BOROUGH),Max_price=max(SALE.PRICE),Min_price=min(SALE.PRICE))
ddply(CleanHousing, .(TAX.CLASS.AT.TIME.OF.SALE), summarize,Total = length(TAX.CLASS.AT.TIME.OF.SALE),Max_price=max(SALE.PRICE),Min_price=min(SALE.PRICE))
ddply(CleanHousing, .(BUILDING.CLASS.AT.TIME.OF.SALE), summarize,Total = length(BUILDING.CLASS.AT.TIME.OF.SALE),Max_price=max(SALE.PRICE),Min_price=min(SALE.PRICE))
ddply(CleanHousing, .(TAX.CLASS.AS.OF.FINAL.ROLL), summarize,Total = length(TAX.CLASS.AS.OF.FINAL.ROLL),Max_price=max(SALE.PRICE),Min_price=min(SALE.PRICE))

#Remove rows where the frequency of a specific categorical data is less than 25
CleanHousing <- CleanHousing[CleanHousing$BUILDING.CLASS.AT.TIME.OF.SALE %in% names(which(table(CleanHousing$BUILDING.CLASS.AT.TIME.OF.SALE) > 25)), ]

#Dropping more varibales before the heat map:
cleanhousing1 <- CleanHousing[,c(1,2,5,6,7,9,10,11,12,13,14)]

#Changing factors into numbers:
cleanhousing1$BOROUGH <- as.numeric(factor(cleanhousing1$BOROUGH),levels = c("1","2","3","4","5"), labels =c(1,2,3,4,5), ordered  = TRUE)
cleanhousing1$TAX.CLASS.AS.OF.FINAL.ROLL <- as.numeric(factor(cleanhousing1$TAX.CLASS.AS.OF.FINAL.ROLL),levels = c("1","2"), labels =c(1,2), ordered  = TRUE)
str(cleanhousing1)


#Creating a correlation heat map
options(repr.plot.width=8, repr.plot.height=6)
library(ggplot2)
library(reshape2)
qplot(x=Var1, y=Var2, data=melt(cor(cleanhousing1, use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))+
  coord_fixed()+
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.4))

#Scatter plot for numeric variables vs sales price
options(repr.plot.width=9, repr.plot.height=6)
ggplot(cleanhousing1, aes(x=GROSS.SQUARE.FEET, y=SALE.PRICE)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , se=FALSE)+
  ggtitle("Scatter plot of SalePrice and Gross Sq Feet") +
  theme(plot.title = element_text(hjust = 0.4))

ggplot(cleanhousing1, aes(x=RESIDENTIAL.UNITS, y=SALE.PRICE)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , se=FALSE)+
  ggtitle("Scatter plot of SalePrice and Res Units") +
  theme(plot.title = element_text(hjust = 0.4))

ggplot(cleanhousing1, aes(x=COMMERCIAL.UNITS, y=SALE.PRICE)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , se=FALSE)+
  ggtitle("Scatter plot of SalePrice and comm Units") +
  theme(plot.title = element_text(hjust = 0.4))

ggplot(cleanhousing1, aes(x=TOTAL.UNITS, y=SALE.PRICE)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , se=FALSE)+
  ggtitle("Scatter plot of SalePrice and total Units") +
  theme(plot.title = element_text(hjust = 0.4))


#Linear regression:
#Set training and test set 
set.seed(10000)
train.index <- sample(c(1:dim(cleanhousing1)[1]), dim(cleanhousing1)[1]*0.8)
train <- cleanhousing1[train.index,]
valid <- cleanhousing1[-train.index,]
model <- lm(SALE.PRICE ~ ., data = train)
summary(model)

#Test model
library(forecast)
#use predict() to make prediction on a new set
pred1 <- predict(model,valid,type = "response")
residuals <- valid$SALE.PRICE - pred1
linreg_pred <- data.frame("Predicted" = pred1, "Actual" = valid$SALE.PRICE, "Residual" = residuals)
accuracy(pred1, valid$SALE.PRICE)

#Classification tree:
#install.packages("rpart.plot")
install.packages("rpart.plot",repos = "http://cran.us.r-project.org")
library(rpart)
library(rpart.plot)
tree.classfication <- rpart(SALE.PRICE~.,data = train,control = rpart.control(cp = 0.01))
plotcp(tree.classfication)
printcp(tree.classfication)
rpart.plot(tree.classfication, box.palette = "BuOr")




