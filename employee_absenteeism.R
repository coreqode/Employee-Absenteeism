rm(list = ls())
getwd()
setwd('/home/lyfracer/Desktop/employee_absenteeism')


library(gdata)

data <- read.xls('data.xls')
str(data)
# Removing ID as it does not help in any way
data = subset(data, select = -ID)

#data$Reason.for.absence[data$Reason.for.absence %in% 0] = NA
#data$Reason.for.absence = as.factor(as.character(data$Reason.for.absence))

data$Month.of.absence[data$Month.of.absence %in% 0] = NA
data$Month.of.absence = as.factor(as.character(data$Month.of.absence))

data$Day.of.the.week = as.factor(as.character(data$Day.of.the.week))
data$Seasons = as.factor(as.character(data$Seasons))
data$Disciplinary.failure = as.factor(as.character(data$Disciplinary.failure))
data$Education = as.factor(as.character(data$Education))
data$Social.drinker = as.factor(as.character(data$Social.drinker))
data$Social.smoker = as.factor(as.character(data$Social.smoker))
data$Son = as.factor(as.character(data$Son))
data$Pet = as.factor(as.character(data$Pet))
data$Work.load.Average.day. = as.numeric(data$Work.load.Average.day.)


# _________________________________________MISSING VALUE ANALYSIS_____________________________________________________________


cat= c('Reason.for.absence','Month.of.absence','Day.of.the.week',
       'Seasons','Disciplinary.failure', 'Education', 'Social.drinker',
       'Social.smoker', 'Son', 'Pet')

missing_val <- data.frame(apply(data,2,function(x){sum(is.na(x))}))
missing_val$features = row.names(missing_val)
row.names(missing_val) = NULL
names(missing_val)[1] = 'missing_percentage'
missing_val$missing_percentage = (missing_val$missing_percentage/nrow(data))*100
missing_val = missing_val[order(-missing_val$missing_percentage),]
missing_val = missing_val[,c(2,1)]
View(missing_val)
write.csv(missing_val,'missing_percentage.csv',row.names = F)

library(ggplot2)
dev.off()
ggplot(data = missing_val,aes(x = features, y = missing_percentage))+ geom_bar(stat = 'identity',fill = 'grey')+xlab('parameter')+ ggtitle('missing data percentage') + theme_bw()+coord_flip()


# missing values imputation

# For categorical variables
## MODE imputation

MODE=function(x){
  unique_values=unique(x)
  unique_values[which.max(tabulate(match(x,unique_values)))]
}

for(i in cat){
  print(i)
  data[,i][is.na(data[,i])] = MODE(data[,i])
}

#original value = 30   
# mean method  26.67
#View(data)
#data2 = data
#data$Body.mass.index[1] = NA
#data$Body.mass.index[is.na(data$Body.mass.index)] = mean(data$Body.mass.index, na.rm = T)
# Median Imputation  25
#data = data2
#data$Body.mass.index[1] = NA
#data$Body.mass.index[is.na(data$Body.mass.index)] = median(data$Body.mass.index, na.rm = T)
# KNN imputation   30
#data = data2
#data$Body.mass.index[1] = NA
library(DMwR)
data = knnImputation(data, k = 3)
#data$Body.mass.index[1]
#since knn imputation is showng the best accuracy, we imputed the data with the knn imputation method.
#data2 = data

# plotting the different distributions of numerical variable_____________________________________________________

par(mar = rep(2,4))
# Transportation expenses
hist(data$Transportation.expense, probability = T)
lines(density(data$Transportation.expense),col="red")

# Distance from the residance to work
hist(data$Distance.from.Residence.to.Work, probability = T)
lines(density(data$Distance.from.Residence.to.Work),col="red")

# Service Time
hist(data$Service.time, probability = T)
lines(density(data$Service.time),col="red")

# Age
hist(data$Age, probability = T)
lines(density(data$Age),col="red")

# Work load average day
hist(data$Work.load.Average.day., probability = T)
lines(density(data$Work.load.Average.day.),col="red")

# Hit Target
hist(data$Hit.target, probability = T)
lines(density(data$Hit.target),col="red")

# Weight 
hist(data$Weight, probability = T)
lines(density(data$Weight),col="red")

#Height
hist(data$Height, probability = T)
lines(density(data$Height),col="red")

# Body mass index
hist(data$Body.mass.index, probability = T)
lines(density(data$Body.mass.index),col="red")


# ______________________________________OUTLIER ANALYSIS__________________________________________________________________________________


#for numerical variables
numeric_index = sapply(data, is.numeric)
numeric_data = data[,numeric_index]
continuous_variables = colnames(numeric_data)

#for categorical variables
factor_index = sapply(data, is.factor)
factor_data = data[,factor_index]
categorical_variables = colnames(factor_data)

# For continuous variables__________________________
for (i in 1:length(continuous_variables)){
  assign(paste0("gn",i), ggplot(aes_string(y = (continuous_variables[i]), x = "Absenteeism.time.in.hours"), data = subset(data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=continuous_variables[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of Employee Absenteeism for",continuous_variables[i])))
}

#generating plots

gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=4)
gridExtra::grid.arrange(gn5,gn6,gn7,gn8,ncol=4)
gridExtra::grid.arrange(gn9,gn10,ncol=2)


# For categorical variables___________________________


bar1 = ggplot(data = data, aes(x = ID)) + geom_bar() +
  ggtitle("Count of ID") + theme_minimal()

bar2 = ggplot(data = data, aes(x = Reason.for.absence)) +
  geom_bar() + ggtitle("Count of Reason for absence") + theme_bw()

bar3 = ggplot(data = data, aes(x = Month.of.absence)) + geom_bar()+
  ggtitle("Count of Month") + theme_bw()

bar4 = ggplot(data = data, aes(x = Disciplinary.failure)) + 
  geom_bar() + ggtitle("Count of Disciplinary failure") + theme_bw()

bar5 = ggplot(data = data, aes(x = Education)) + geom_bar()+
  ggtitle("Count of Education")+ theme_bw()

bar6 = ggplot(data = data, aes(x = Son)) + geom_bar()+
  ggtitle("Count of Son") + theme_bw() 

bar7 = ggplot(data = data, aes(x = Social.smoker)) + geom_bar() + 
  ggtitle("Count of Social smoker") + theme_bw()


#Replace all outliers with NA and impute

for(i in continuous_variables){
  val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  #print(length(val))
  data[,i][data[,i] %in% val] = NA
}

# using knn imputation to fill missing values in dataset
data = knnImputation(data, k =3)


# _________________________________________FEATURE SELECTION____________________________________________________________________________


# Checking for multicollinearity using VIF

library(usdm)
vifcor(numeric_data)

# checking for multicollineariy using Correlation Plot
library(corrgram)
corrgram(data[,numeric_index], order= F,
         upper.panel=panel.pie, text.panel = panel.txt, main = "Correlation Plot")


# Since 'weight' feature is having collinearity problem, so we are removing this variable from the dataset.
data = subset(data, select = -c(Weight))

library('lsr')

anova_test=aov(Absenteeism.time.in.hours~Reason.for.absence+Month.of.absence+Day.of.the.week+Seasons+
                 Disciplinary.failure+Education+Son+Social.drinker+Social.smoker+Pet,data = data)

#Lets check out the summary
summary(anova_test)

## since features (month of absence, day of the week, seasons, education, social drinker, social smoker ) has collinearity problems

data = subset(data, select = -c(Education,Seasons,Social.smoker,Pet))
#___________________________________________FEATURE SCALING________________________________________________


qqnorm(data$Absenteeism.time.in.hours)
hist(data$Absenteeism.time.in.hours)

## __________________________________________Normalisation_________________________________________________


continuous_variables = c('Transportation.expense', 'Distance.from.Residence.to.Work', 'Service.time',
                         'Age', 'Work.load.Average.day.','Body.mass.index',
                         'Hit.target', 'Height'
                         )

categorical_variables = c('Reason.for.absence','Month.of.absence','Disciplinary.failure','Social.drinker', 'Son')

data$Absenteeism.time.in.hours = log1p(data$Absenteeism.time.in.hours)
for(i in continuous_variables)
{
  if(i != 'Absenteeism.time.in.hours'){
   print(i)
  data[,i] = (data[,i] - min(data[,i]))/(max(data[,i])-min(data[,i]))
}
}

#________________________________MODEL DEVELOPMENT (WITH DUMMIES)______________________________________________

library(dummies)
data=data
data = dummy.data.frame(data, categorical_variables)


set.seed(123)
train_index = sample(1:nrow(data),0.75 * nrow(data))
train = data[train_index,]
test = data[-train_index,]

#____________________________________________RANDOM FOREST__________________________________________________

set.seed(123)
library(randomForest)
library(caret)

model_RF = randomForest(Absenteeism.time.in.hours~., data = train,ntree = 300)
write(capture.output(summary(model_RF)), "RF_Rules.txt")

predict_RF = predict(model_RF, test[,-59],type = 'class')
summary(predict_RF)

Performance_RF=print(postResample(pred =predict_RF,obs = test[,59]))

#   RMSE       Rsquared       MAE 
# 0.4337328   0.5468035    0.3178110 

##______________________________DECISION TREE___________________________

library('rpart')

model_DT=rpart(Absenteeism.time.in.hours~.,data=train,method='anova')

summary(model_DT)
plot(model_DT)

Predict_DT=predict(model_DT,test[,-59])
summary(Predict_DT)

plot(test$Absenteeism.time.in.hours,type="l",lty=1.8,col="Green",main="DT")
lines(Predict_DT,type="l",col="Blue")

Performance_DT=print(postResample(obs = test[,59],pred = Predict_DT))

#   RMSE      Rsquared       MAE 
# 0.4664381   0.4972954   0.3563151 

##____________________________LINEAR REGRESSION__________________________

model_LR=lm(Absenteeism.time.in.hours~.,train)

summary(model_LR)

predict_LR=predict(model_LR,test[,-59])

summary(predict_LR)

Performance_LR=print(postResample(pred = predict_LR,obs = test[,59]))

#   RMSE       Rsquared       MAE 
# 0.4576405   0.5205733    0.31768807 


