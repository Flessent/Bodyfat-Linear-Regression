##################################################################  Task 1  ################################################################################
library(tidyverse)
library(ggplot2) # load the library ggplot2
#install.packages("viridis")
#install.packages("GGally")
library(GGally)
library(viridis)

setwd('Path_to_your_XLSX_File')
getwd()
bodyFat<-readxl::read_xlsx('Bodyfat.xlsx')
print(bodyFat, n=4)


#################################################################### Descriptive Statistic########################################

# the descriptive statistic boils down to statistical parameters for instance mean, quantile,etc..

summary(bodyFat) # display statistical parameters of each variable of our dataset



#################################################################### Visualizing of each variable ############################################################################



# Variable Age

bodyFat$groupAge<-cut(bodyFat$Age,
                      breaks = c(seq(from=22,to=85,by=3)),
                      right = FALSE)

barAge<-ggplot(data=bodyFat )+geom_bar(mapping =  aes(x=groupAge ,fill=groupAge ))
barAge+ labs(x='Age (in year)',y='Number of persons"',title = 'Age of each participant')#+
barAge

#Variable density
ggplot(data = bodyFat,mapping=aes(x=round(Density, 4)))+
    geom_freqpoly(width=2)+ # most used so as to overlap
    scale_y_continuous(limits=c(0, 30), breaks=seq(from=0, to=30, by=1))+
    labs(x='Density',y='Participants',title = 'Density of Participants')+
    theme(panel.background = element_rect(fill = "yellow")) # change background

#variable Weight
weightInKg<-bodyFat$Weight*0.45359237 #convert lbs to kg
weightInKg
max(weightInKg)

bodyFat$groupWeight<-cut(weightInKg,
                      breaks = c(seq(from=50,to=170,by=2)),
                      right = FALSE)

barWeight<-ggplot(data=bodyFat )+geom_bar(mapping =  aes(x=groupWeight ,fill=groupWeight ))
barWeight+labs(x='Weight (in kg)',y='Number of persons"',title = 'Interval of participants\'s  Weight')+
    scale_y_continuous(limits=c(0, 30), breaks=seq(from=0, to=30, by=1))

    barWeight
    
# we'll now group variables so as to avoid repeated code in order  to save time
# We'll group variables having fluctuating in a same range otherwise we incur to loose certain values
# These variables are : Ankle,Biceps,Forearm,Knee and Thigh
#we call this macro-variable : grouping5Variables
    
    
    grouping5Variables<-bodyFat %>%
        pivot_longer(Thigh:Forearm, names_to = "parameter", values_to = "observation")
    
    Range<-cut(grouping5Variables$observation,
                breaks = c(seq(from=15,to=90,by=2)),
                right = FALSE)
    
    
    grouping5Variables%>%
        ggplot() +
        geom_bar(mapping =  aes(x=Range ,fill=Legend)) +
        facet_wrap(vars(parameter), ncol = 3) +
        labs(x = "Range", y = "Number of participants",title = "Ankle,Biceps,Forearm,Knee and Thigh")+
        scale_y_continuous(limits = c(0,150), breaks = seq(from=0,to=150,by=5))+
        theme(panel.background = element_rect(fill = "black")) # change background
    
    
# grouping of 3 variables : Wrist, Neck and Height
    
  bodyFat$heightInCm<-bodyFat$Height*2.54  # conversion inches  into centimeter so as to group many values with same units 
    
    grouping3Variables<-bodyFat %>%         # works as pipeline
        pivot_longer(cols = c('Wrist','Neck','Height'), names_to = "parameter", values_to = "observation")
    
    Range3<-cut(grouping3Variables$observation,
               breaks = c(seq(from=14,to=80,by=2)),
               right = FALSE)
    
   
    
    
    grouping3Variables%>%
        ggplot() +
        geom_bar(mapping =  aes(x=Range3 ,fill=Range3)) +
        facet_wrap(vars(parameter), ncol = 3) +
        labs(x = "Range", y = "Number of participants",title = "Wrist,Neck,Height")+
        scale_y_continuous(limits = c(0,150), breaks = seq(from=0,to=150,by=5))+
        theme(panel.background = element_rect(fill = "black")) # change background
    
# grouping of 3 variables : Chest, Abdomen, Hip
    
    grouping3Variables<-bodyFat %>%
        pivot_longer(cols = c('Chest','Abdomen','Hip'), names_to = "parameter", values_to = "observation")
    
    Range3<-cut(grouping3Variables$observation,
                breaks = c(seq(from=60,to=150,by=2)),
                right = FALSE)
    
    max(grouping3Variables$observation)
    grouping3Variables%>%
        ggplot() +
        geom_bar(mapping =  aes(x=Range3 ,fill=Range3)) +
        facet_wrap(vars(parameter), ncol = 3) +
        labs(x = "Range", y = "Number of participants",title = "Chest,Abdomen, and Hip")+
        scale_y_continuous(limits = c(0,50), breaks = seq(from=0,to=50,by=3))+
        theme(panel.background = element_rect(fill = "black")) # change background 

# Now we'll be looking in next step how our data behave depending on each other. For this purpose we decide to use ggpair  coming from GGally-library
        library(GGally) # involve including ggplot2-library
    

groupAgeWeight<-cbind(Age=bodyFat$Age,weightInKg)
groupAgeWeight
bodyFat$Age<-as.factor(bodyFat$Age)


        quant_df <- bodyFat[c("Age", "Weight")]
    ggpairs(quant_df, aes(fill=Age))+
        theme(panel.background = element_rect(fill = 'yellow' ))
    
#############################################################Correlation between variables using ggpairs#############################

    
    AgeAndClass<-cbind(Age= bodyFat$Age,class=bodyFat$class)
    AgeAndClass
    ggcorr(AgeAndClass)
    # now we'll try to find out the correlation between density and class. Since density is a key-parameter of determining the body fat percentage
    
    DensityAndClass<-cbind(Age= bodyFat$Density,class=bodyFat$class)
    ggcorr(DensityAndClass)        # returns a negative correlation ~= -1
    
    cor(bodyFat$Density, bodyFat$class)# bring prove about the just above  ggcorr(DensityAndClass) . It returns  -0.9877824
    

    

ggpairs(data = bodyFat, columns = 1:14, aes(colour = 'class')) # returns a pairweise relationships between all variables according to a target variable: class




#######################################################Task 2 : Building a model#######################################################################################

# 1- divide data into training and testing with ratio 85:15

#There are many tools such as sample coming directly from R, dplyr, CaTools,etc. We'll use caTools
install.packages('caTools')
require(caTools)

set.seed(101) # this makes our data reproducible in order to avoid unexpected results. But most useful when we deal with random values following a certain distribution
#We just have to divide the class-variable into training-and testing in order to predict whether a participant belong to a specific class of bodyfat-percentage
percentageClass <- sample.split(bodyFat$class, SplitRatio = .85)
training  <- subset(bodyFat, percentageClass == TRUE)
testing   <- subset(bodyFat, percentageClass == FALSE)
dim(testing) # 38 15
dim(training)# 214 15


#############################################################Dummy or naive approach####################################################################################


#2- But which model we'll  Choose ?. We are going to firstly start with so-called dummy approach or naive approach, that means we assume that al variables
#are relevant and follow a normal distribution. Is it the case ? 
#As we saw in task 1 that density had a greatest(most correlated with class) correlation then we shall see this veracity with following dummy approach 


ggplot(bodyFat, mapping = aes(x=density, y=class)) +
  geom_point() +
  stat_smooth(method = lm)                                       # dummy approach using ggplot


class<-c( bodyFat[[15]])
density<-c(bodyFat[[2]])
reg<-lm(formula =  class~density)                                                                          # dummy approach using built-in R-functions
plot(class~density, col = rep(1:2, each = 10) ,pch=19)
legend("bottomright", legend = paste("Parameters", 1:2), col = 1:2, pch = 19, bty = "n")
abline(reg)
summary(reg)

#finding SSE allows to make a better estimation of a fitted model
sse <- sum((fitted(reg) - bodyFat$class)^2)
sse # 16085.69



############################################################################Multiple Regression Model##############################
library(MASS)
install.packages('car')
library(car)
################################################################ Training data#####################################################
#model <- lm(class ~ Density +Age + Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Biceps+Forearm+Wrist, data = bodyFat)
set.seed(1002)

modelTraining<-lm(formula = class ~ Density +Age + Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Biceps+Forearm+Wrist,
            data = training)
newModelTraining<-lm(formula = class ~ Density +Age +Abdomen+Chest,  data = training)
#AIC is one of the most used way to check the correctness or which of our variables are more signicant than others. It allows to reduce the unnecessary  complexity 
# and choses the most suitable model. The more AIC is smaller the better is our model.

stepModel<-stepAIC(newModelTraining,direction = "both") # give us what are the most relevevante and significant variables

summary(stepModel)
sseStepModel <- sum((fitted(newModelTraining) - modelTraining$class)^2) # 0.00245 (Sum of squared error)
sseStepModel
vif(stepModel) # A general guideline is that a VIF larger than 5 or 10 is large, indicating that the model has problems estimating the coefficient

summary(newModelTraining)$coefficient
summary(newModelTraining)
summary(modelTraining)

############################################## Prediction using test data############################################################################

predictStepModel<-predict(stepModel, testing)

predictStepModel # display predicted data

summary(bodyFat)

############################################### Soft manner for showing out real value and predicted data by adding a new column for predicted values###############

testing["Predicted"]<-predictStepModel
testing


# Hopping that this small overview on Linear-Regression with R has helped you out 

