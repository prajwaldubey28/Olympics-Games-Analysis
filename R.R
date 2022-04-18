library(ggplot2)
library(tidyverse)
library(mice)
library(caret)
library(Matrix)
library(glmnet)
library(readr) # CSV file I/O, e.g. the read_csv function
library(plyr)
library(rworldmap)
library(repr)
library(MLmetrics)
library(tree)
library(randomForest)
library(corrplot)
library(RColorBrewer)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
summer <- read_csv("summer.csv")
winter <- read_csv("winter.csv")
dic <- read_csv("dictionary.csv")
wid <- read.csv("WDIData.csv")
summer$season <- "summer"
winter$season <- "winter"
olym <- as.data.frame(rbind(summer,winter))
Indicators <- c("SP.DYN.CDRT.IN","SP.POP.TOTL","SP.POP.GROW","EN.POP.DNST","AG.LND.TOTL.K2","NY.GDP.MKTP.KD.ZG","NY.GDP.MKTP.CD","EN.ATM.CO2E.PC",
                "SP.DYN.CBRT.IN","SP.DYN.LE00.IN","SP.DYN.IMRT.IN","SP.RUR.TOTL","FI.RES.TOTL.CD","NE.TRD.GNFS.ZS","SP.URB.TOTL","AG.PRD.FOOD.XD")
olympicyears <- paste("X",unique(olym$Year),sep="")
olympiccountries <- unique(olym$Country)
wdi <- filter(wid,Country.Code %in% olympiccountries)
wdi2 <- filter(wdi, Indicator.Code %in% Indicators)
wdi3 <- wdi2[,colnames(wdi2) %in% c(colnames(wdi2)[1:4],olympicyears)]
wdi3 <- wdi3[,-5]

wdilong <- gather(wdi3,Year,Value,X1964:X2014)
wdilong$Year <- substring(wdilong$Year,2)
wdilong <- wdilong[,-4]
wdifinal <- spread(wdilong,Indicator.Name,Value)

olym2 <- as.data.frame(olym[olym$Year %in% 1964:2020,])


medals <- olym2 %>% group_by(Country,Year) %>% dplyr::count(Medal)
medals <- medals[-c(1,2),]
medals2 <- spread(medals,Medal,n)
medals2 <- as.data.frame(medals2)
medals2[is.na(medals2)] <- 0

medals2$points <- medals2$Bronze + (2*medals2$Silver) + (3*medals2$Gold)



df2 <- merge(medals2,wdifinal,by.x=c("Country","Year"),by.y=c("Country.Code","Year"))
df <- df2[,c(7,1,2,3,5,4,6,8:15,18,16,17,19:23)]

colnames(df) <- c("Country","Country.Code","Year","Bronze","Silver","Gold","Points","Birth.Rate",
                  "C02.Emissions","Death.Rate","Food.Production","GDP","GDP.Growth","Land.Area","Expectancy","Population.Growth", "Infant.Mortality","Population.Density","Population","Rural.Population","Total.Reserves","Trade.Percentage","Urban.Population")
df$Year <- as.numeric(df$Year)
df[8:23] <- apply(df[,8:23],2,as.numeric)


### Missing values
sum(is.na(df2))/(16*657)*100

### Imputation
dfimpute <- as.matrix(df[,c(3,8:23)])
dfnew <- mice(dfimpute,method="cart")

df[,c(3,8:23)] <- complete(dfnew)

regression <- df[,7:23]


### Total events over time
totalevents <- plyr::count(olym[,c(1,10)])
ggplot(data=totalevents,aes(x=Year,y=freq,color=season)) + geom_point() + ylab("Number of Events")



### Maps
country <- aggregate(medals2$points,by=list(medals2$Country),sum)
colnames(country) <- c("Country","Medals")
country2 <- merge(country,dic,by.x="Country",by.y="Code")


# define colors
palette = colorRampPalette(brewer.pal(n=7, name='Oranges'))(7)
palette = c("white", palette)

# create map
world <- ne_countries(scale = "medium", returnclass = "sf")
country3 <- merge(world, country2, by.x = "name",by.y="Country.y",all.x=TRUE)

worldmedalsmap <- ggplot() +geom_sf(data = country3, aes(fill = Medals)) +
  labs(fill = "Medal Points") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

worldgdpmap <- ggplot() +geom_sf(data = country3, aes(fill = `GDP per Capita`)) +
  labs(fill = "GDP per Capita") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

worldmedalsmap

worldgdpmap


par(mfrow=c(2,1))
summer%>%
  group_by(Gender,Year) %>%
  dplyr::count() %>%
  ggplot() + geom_line(aes(x= Year, y=n, group = Gender, color = Gender)) +
  labs(title = 'Participation In each Summer Olympics based on gender',y="Count")+
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

winter%>%
  group_by(Gender,Year) %>%
  dplyr::count() %>%
  ggplot() + geom_line(aes(x= Year, y=n, group = Gender, color = Gender)) +
  labs(title = 'Participation In each Winter Olympics based on gender',y="Count")+
  theme_minimal()+ theme(plot.title = element_text(hjust = 0.5))


olym%>%
  select(Discipline) %>%
  mutate(Discipline = str_split(Discipline,',')) %>%
  unnest(Discipline) %>%
  group_by(Discipline) %>%
  dplyr::count() %>%
  arrange(desc(n)) %>%
  ggplot() + geom_col(aes(y = reorder(Discipline,n), x = n)) +
  labs(title = 'Number of Disciplines',x = 'Total',y = 'Disciplines') +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size=4))


### Medals won by country
olymsub <- olym[olym$Year %in% 1960:2020,]
olymsub%>%
  filter(!str_detect(Country,',')) %>%
  group_by(Country) %>%
  dplyr::count() %>%
  arrange(desc(n)) %>%
  head(40) %>%
  ggplot() + geom_col(aes(y = reorder(Country,n), x = n)) +
  labs(title = 'Medals won by each Country (1960-2014)',y="Country") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size=6))


### Most medals wont by athletes

olymsub%>%
  group_by(Athlete) %>%
  dplyr::count() %>%
  arrange(desc(n)) %>%
  head(30) %>%
  ggplot() + geom_col(aes(y = reorder(Athlete,n), x = n)) +
  geom_label(aes(y = reorder(Athlete,n,fill="rep"), x = n, label = n)) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))


### Proportion of events held by each city
pie <- ggplot(olymsub, aes(x=factor(City), fill = factor(City))) + geom_bar(width = 1) + 
  labs(title="Number of Events Held by City",y="Count",x="",legend="")
pie + coord_polar() +theme_minimal()+ theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5))


### corrplot
df.cor <- cor(df[7:23])
par(mfrow=c(1,1))
corrplot(df.cor,is.corr = TRUE,method="square",type="upper",tl.srt=45,tl.col="black")


###Boxplots
box <- as.data.frame(scale(df[,8:23]))


ggplot(stack(box),aes(x=ind,y=values)) + geom_boxplot()+ theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45))+
  labs(title="Boxplots of Scaled Variables",x="Variable",y="Scaled Value")


###  Creation of dataset with log of predictors
dflog <- df


### For variables that have values below 0, a started log was used
for (i in 8:23){
  if(min(df[,i])==0){
    dflog[,i] <- dflog[,i] + 1
  }
  if (min(df[,i])<0){
    dflog[,i] <- dflog[,i] + abs(min(dflog[,i])) + 1
  }
}

min(dflog[,16] + abs(min(dflog[,16])) + 1)
dflog[8:23] <- log(dflog[,8:23])

df[,8:23] %>%
  gather() %>%
  ggplot(aes(value)) +  facet_wrap(~ key, scales = "free") +geom_density()


dflog[,8:23] %>%
  gather() %>%
  ggplot(aes(value)) +  facet_wrap(~ key, scales = "free") +geom_density()


regression.log <- dflog[,7:23]
summary(lm(Points~.,data=regression.log))


min(dflog$Total.Reserves <- dflog$Total.Reserves + abs(min(dflog$Total.Reserves)) + 1)

index <- sample(nrow(df),nrow(df)*.8)
traintest <- as.data.frame(cbind(df[index,7],scale(df[index,8:23])))
colnames(traintest)[1] <- "Points"
test <- as.data.frame(cbind(df[-index,7],scale(df[-index,8:23])))
colnames(test)[1] <- "Points"

X <- sparse.model.matrix(as.formula(paste("Points ~", paste(colnames(df[,8:23]),sep = "", collapse=" +"))), data = traintest)
y <- traintest$Points
Xtest <- sparse.model.matrix(as.formula(paste("Points ~", paste(colnames(df[,8:23]),sep = "", collapse=" +"))), data = test)


cv.ridge <- cv.glmnet(X, y, alpha = 0)
best.lam <- cv.ridge$lambda.min

ridge <- glmnet(X,y,alpha=0,lambda = best.lam)

ridge.R2 <- ridge$dev.ratio

ridge.predictions <- predict(ridge,newx=Xtest)
ridge.MSE <- MSE(ridge.predictions,test$Points)

### ridge regression with log
traintest2 <- as.data.frame(cbind(dflog[index,7],scale(dflog[index,8:23])))
colnames(traintest2)[1] <- "Points"
test2 <- as.data.frame(cbind(dflog[-index,7],scale(dflog[-index,8:23])))
colnames(test2)[1] <- "Points"

X2 = sparse.model.matrix(as.formula(paste("Points ~", paste(colnames(df[,8:23]),sep = "", collapse=" +"))), data = traintest2)
y2 <- traintest2$Points
Xtest2 <- sparse.model.matrix(as.formula(paste("Points ~", paste(colnames(dflog[,8:23]),sep = "", collapse=" +"))), data = test)

cv.ridge.log <- cv.glmnet(X2, y2, alpha = 0)
best.lam.log <- cv.ridge$lambda.min

ridge.log <- glmnet(X2,y2,alpha=0,lambda = best.lam)

ridge.log$dev.ratio

ridge.log.predictions <- predict(ridge.log,Xtest2)
ridge.log.MSE <- MSE(ridge.log.predictions,test2$Points)

### Decision Tree
tree <- tree(Points~., data=traintest)
tree.cv <-cv.tree(tree,K=10)

best.size <- tree.cv$size[which.min(tree.cv$dev)]

tree.best <- prune.tree(tree,best=best.size)

tree.predictions <- predict(tree.best,test)
tree.MSE <- MSE(tree.predictions,test$Points)

### Decision Tree with log
tree.log <- tree(Points~., data=traintest2)
tree.cv.log <-cv.tree(tree.log,K=10)

best.size.log <- tree.cv.log$size[which.min(tree.cv.log$dev)]

tree.best.log <- prune.tree(tree.log,best=best.size.log)

tree.predictions.log <- predict(tree.best,test)
tree.MSE.log <- MSE(tree.predictions.log,test2$Points)



### Random Forest
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

tunegrid <- expand.grid(.mtry = (1:15))

rf_gridsearch <- train(Points ~ ., 
                       data = traintest,
                       method = 'rf',
                       metric = 'RMSE',
                       tuneGrid = tunegrid)
print(rf_gridsearch)

rf <- randomForest(Points~.,data=traintest,mtry=1)
forest.predictions <- predict(rf,test)

rf.MSE <- MSE(forest.predictions,test$Points)

#### Random forest with logs
rf.log <- randomForest(Points~.,data=traintest2,mtry=1)
forest.predictions.log <- predict(rf.log,test2)

rf.MSE.log <- MSE(forest.predictions.log,test2$Points)

rsq <- function (x, y) cor(x, y) ^ 2
rsq(forest.predictions,test2$Points)


mse <- c(tree.MSE,rf.MSE)
mselog <- c(tree.MSE.log,rf.MSE.log)

mse.df <- data.frame(mse,mselog)
colnames(mse.df) <- c("RMSE","RMSE with logs")
rownames(mse.df) <- c("Ridge","Lasso","Tree","Random Forest")
rmse.df <- sqrt(mse.df)
rmse.df

impor <- as.data.frame(importance(rf.log))
impor$Variable <- rownames(impor)
impor2 <- impor[order(impor$IncNodePurity,decreasing=TRUE),]


importance.plot <- ggplot(impor2) + geom_col(aes(x=reorder(Variable,-IncNodePurity),y=IncNodePurity,fill=Variable)) + 
  labs(title="Importance of Predictor Variables",x="Variable",y="Importance")

importance.plot+ theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45))
