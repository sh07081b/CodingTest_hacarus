####Set the git location####
#https://github.com/sh07081b/CodingTest_hacarus.git
setwd('E:/git/CodingTest_hacarus')

#check java_home
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk-13') # for 64-bit version JDK

#install package ar first time
#install.packages(c('xlsx','tidyr','caret'))

#roading package
library(xlsx) 
library(dplyr)
library(caret)

####Data preprocessing####
DF1 <- read.xlsx('./Data/Meal Analysis (2017) .xlsx',1)

#check missing value (ratio < 0.05, pass)
MissingValueCheck <- function(DF){
  df1 <- DF[!is.na(DF$NA.),]
  df2 <- data.frame(all = nrow(df1),missing = colSums(is.na(df1)),ratio = colSums(is.na(df1))/nrow(df1))%>%
    filter(ratio > 0.05)
  if (nrow(df2) == 0){
    check = 'Pass'
  } else {
    check = row.names(df2)
  }
  return(check)
} 
MissingValueCheck(DF1)

####Randomforest####
df1 <- DF1 %>%
  na.omit() %>%
  rename(Score = Score.1.worst.2.bad.3.good.4.best.) %>%
  mutate(avg.E = E.kcal./number.of.dishes,
         avg.P = P.g./number.of.dishes,
         avg.F = F.g./number.of.dishes,
         avg.C = C.g./number.of.dishes,
         avg.Salt = Salt.g./number.of.dishes,
         avg.Vegetables = Vegetables.g./number.of.dishes,
         Type = as.factor(Type),
         gender = as.factor(gender),
         Score = as.factor(Score)) %>%
  select(Type,gender,age,height,weight,EER.kcal.,avg.E,avg.P,avg.F,avg.C,avg.Salt,avg.Vegetables,Score)

set.seed(36)
train <- sample(nrow(df1), 0.7*nrow(df1), replace = FALSE)
TrainSet <- df1[train,]
ValidSet <- df1[-train,]

model_rf <- caret::train(Score ~ .,
                         data = TrainSet,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  verboseIter = FALSE))
predict(model_rf, newdata = TrainSet)

#TrainingScore
#predTrain <- predict(model_rf, TrainSet)
#table(predTrain, TrainSet$Score)  
#mean(predTrain == TrainSet$Score)          

#ValidSetScore
predValid <- predict(model_rf, ValidSet)
table(predValid, ValidSet$Score)  
mean(predValid == ValidSet$Score)          

####Output_Result####
result <- data.frame(ValidSet,predict = predValid)
varImp(model_rf)

####Result insight and discussion 1 - Class unbalanced problem####

sampling <- function(sampling_methods){
  ctrl <- trainControl(method = "repeatedcv", 
                       number = 10, 
                       repeats = 10, 
                       verboseIter = FALSE,
                       sampling = sampling_methods)
  set.seed(36)
  model_rf <- caret::train(Score ~ .,
                           data = TrainSet,
                           method = "rf",
                           preProcess = c("scale", "center"),
                           trControl = ctrl)
  predValid <- predict(model_rf, ValidSet)
  result <- table(predValid, ValidSet$Score)  
  accuracy <- mean(predValid == ValidSet$Score) 
  
  return(list(model_rf,result,accuracy))
} 

#Under-sampling
Undersampling <-  sampling("down")
#Oversampling
#Oversampling <-  sampling("up") #take a lot of time
#SMOTE
SMOTE <-  sampling("smote")


#accuracy
models <- list(original = model_rf,
               down = Undersampling[[1]],
               SMOTE = SMOTE[[1]])

resampling <- resamples(models)
bwplot(resampling)

####Result insight and discussion 2 - feature importance####
varImp(model_rf)

TrainScore <- TrainSet%>%
  group_by(Score) %>%
  summarise(avg.Vegetables = mean(avg.Vegetables),avg.F=mean(avg.F),avg.Salt=mean(avg.Salt),avg.P=mean(avg.P),avg.C=mean(avg.C),avg.E=mean(avg.E))

ResultScore <- result %>%
  group_by(predict) %>%
  summarise(avg.Vegetables = mean(avg.Vegetables),avg.F=mean(avg.F),avg.Salt=mean(avg.Salt),avg.P=mean(avg.P),avg.C=mean(avg.C),avg.E=mean(avg.E))

#plot result
library(highcharter)

PlotColumnChart <- function(data){
  out <- data.frame()
  for (i in 2:ncol(data)) {
    tmp <- data[,c(1,i)]
    tmp$name <- names(tmp[,2])
    names(tmp)[1] <- 'Score'
    names(tmp)[2] <- 'Value'
    out <- bind_rows(out,tmp)
  }
  return(hchart(out, "column", hcaes(x = name, y = Value, group = Score)))
}
PlotColumnChart(TrainScore)
PlotColumnChart(ResultScore)

####Result insight and discussion 3 - Special Case####
SpecialCase <- result %>%
  filter(Score == '1' & predict == '4')

####OutputModel####
#savemodel
#saveRDS(model_rf, "./model/model_rf.rds")

#roadmodel
#model_rf_road <- readRDS("./model/model_rf.rds")
#model_rf_road

