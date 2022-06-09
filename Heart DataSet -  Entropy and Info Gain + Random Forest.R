######################################

#Name: Om Anish Dalal
#Student Number: R00195850
#DATA8001_24010 Data Science and Analytics - Assignment

#####################################

####################################
#Loading the Data Set
####################################

#Reading the Heart Data file
heart_data <- read.table("heart.dat",stringsAsFactors = T)
View(heart_data)

colnames(heart_data)
#Adding the variable names to the data set
names(heart_data)<-c( "Age", "Sex", "Chest_Pain_Type", "Blood_Pressure", "Cholestoral","Fasting_Blood_Sugar",
                      "Resting_ECG_Results","Max_Heart_Rate_Achieved","Exercise_Induced_Angina", 
                      "Oldpeak","Slope", "No_of_Major_Vessels", "Thal", "Heart_Disease")

attach(heart_data)

#Converting the char variables as Factors
heart_data$Chest_Pain_Type <- as.factor(Chest_Pain_Type)
heart_data$Resting_ECG_Results <- as.factor(Resting_ECG_Results)
heart_data$Slope <- as.factor(Slope)
heart_data$Thal <- as.factor(Thal)
heart_data$Heart_Disease <- as.factor(Heart_Disease)

####################################
#Exploratory Data Analysis
####################################

summary(heart_data)

#Distribution of gender in the data
table(Sex)
round(prop.table(table(Sex)),2)

#Distribution of Absence or Presence of Heart Disease in the data
table(Heart_Disease)
round(prop.table(table(Heart_Disease)),3)

library(ggplot2)

#Bar Chart of Distribution of Absence or Presence of Heart Disease in the data
ggplot(heart_data, aes(Heart_Disease,  fill=Heart_Disease)) +
  geom_bar(position="dodge") + 
  labs(title="Bar Chart of Distribution of Heart Disease", 
       x = "Absence (1) or Presence (2) of Heart Disease",
       y = "Count")


#Histograms for Continuous variables
summary(heart_data$Age)
ggplot(heart_data,aes(Age)) + 
  geom_histogram(fill = "red",
                 col="black", 
                 size=.1) + 
  labs(title = "Age Distribution", x="Age", y="Count")

summary(heart_data$Blood_Pressure)
ggplot(heart_data,aes(Blood_Pressure)) + 
  geom_histogram(fill = "red",
                 col="black", 
                 size=.1) + 
  labs(title = "Blood Pressure Distribution", x="Blood Pressure", y="Count")

summary(heart_data$Cholestoral)
ggplot(heart_data,aes(Cholestoral)) + 
  geom_histogram(fill = "red",
                 col="black", 
                 size=.1)+
  labs(title = "Cholestoral Distribution", x="Cholestoral", y="Count")

ggplot(heart_data,aes(Max_Heart_Rate_Achieved)) + 
  geom_histogram(fill = "red",
                 col="black", 
                 size=.1) + 
  labs(title = "Max Heart Rate Achieved Distribution", x="Max Heart Rate Achieved", y="Count")

ggplot(heart_data,aes(Oldpeak)) + 
  geom_histogram(fill = "red",
                 col="black", 
                 size=.1) + 
  labs(title = "Oldpeak Distribution", x="Oldpeak", y="Count")


# Distribution (Counts) of the categorical values
chest_pain_type <- ggplot(heart_data, aes(Chest_Pain_Type,  fill=Chest_Pain_Type)) +
  geom_bar(stat="count") + 
  labs(title="Chest Pain Type", 
       x = "Chest Pain Type",
       y = "Count")

resting_ECG_results <- ggplot(heart_data, aes(Resting_ECG_Results,  fill=Resting_ECG_Results)) +
  geom_bar(stat="count") + 
  labs(title="Resting ECG Results", 
       x = "Resting ECG Results",
       y = "Count")

slope <- ggplot(heart_data, aes(Slope,  fill=Slope)) +
  geom_bar(stat="count") + 
  labs(title="Slope", 
       x = "Slope",
       y = "Count")

thal <- ggplot(heart_data, aes(Thal,  fill=Thal)) +
  geom_bar(stat="count") + 
  labs(title="Thal", 
       x = "Thal",
       y = "Count")

library(gridExtra)
grid.arrange(chest_pain_type,resting_ECG_results,slope,thal,
             ncol=2,top = "Counts of the Categorical Variables")



#Analysis of Sex with respect to Heart Disease
ggplot(heart_data, aes(as.factor(Sex),fill = Heart_Disease)) + 
  geom_bar(position = 'dodge',
           col="black", 
           size=.1) +
  labs(title="Presense of Heart Disease across Sex", 
       x = "Sex",
       y = "Count")

#Analysis of variables with respect to Heart Disease
chest_pain_type_bar <- ggplot(heart_data, aes(Chest_Pain_Type,  fill=Heart_Disease)) +
  geom_bar(position="dodge") + 
  labs(title="Chest Pain Type", 
       x = "Chest Pain Type",
       y = "Count")

resting_ECG_results_bar <- ggplot(heart_data, aes(Resting_ECG_Results,  fill=Heart_Disease)) +
  geom_bar(position="dodge") + 
  labs(title="Resting ECG Results", 
       x = "Resting ECG Results",
       y = "Count")

slope_bar <- ggplot(heart_data, aes(Slope,  fill=Heart_Disease)) +
  geom_bar(position="dodge") + 
  labs(title="Slope", 
       x = "Slope",
       y = "Count")

thal_bar <- ggplot(heart_data, aes(Thal,  fill=Heart_Disease)) +
  geom_bar(position="dodge") + 
  labs(title="Thal", 
       x = "Thal",
       y = "Count")

grid.arrange(chest_pain_type_bar,resting_ECG_results_bar,slope_bar,thal_bar,
             ncol = 2, top = "Distribution of Categorical variables vs Heart Disease")


#Box Plots of variables with respect to Heart Disease
age_box_plot <- ggplot(heart_data, aes(Heart_Disease,Age,fill = Heart_Disease)) + 
  geom_boxplot() + 
  labs(title="Age",
       x="Heart Disease",
       y="Age")


blood_pressure_box <- ggplot(heart_data, aes(Heart_Disease,Blood_Pressure,fill = Heart_Disease)) + 
  geom_boxplot() + 
  labs(title="Blood Pressure",
       x="Heart Disease",
       y="Blood Pressure")

cholestoral_box <- ggplot(heart_data, aes(Heart_Disease,Cholestoral,fill = Heart_Disease)) + 
  geom_boxplot() + 
  labs(title="Cholestoral",
       x="Heart Disease",
       y="Cholestoral") 


heart_rate_box <- ggplot(heart_data, aes(Heart_Disease,Max_Heart_Rate_Achieved,fill = Heart_Disease)) + 
  geom_boxplot() + 
  labs(title="Max Heart Rate Achieved",
       x="Heart Disease",
       y="Max Heart Rate Achieved")


grid.arrange(age_box_plot,blood_pressure_box,cholestoral_box,
             heart_rate_box,
             ncol = 2, top = "Box Plot of variables grouped by Heart Disease")

####################################
#Spliting the Data Set
####################################

# Now split into training and testing data set 80:20

set.seed(850) # set a random seed
# 270*0.8 = 216
# 270*0.2 = 54
training_set <- sample(1:nrow(heart_data), 216) 
test_set <- heart_data[-training_set,] 

training_set_data <- heart_data[training_set,]

####################################
#Entropy and Information Gain
####################################

#Firstly, we calculate the Entropy and Information Gain of the Categorical predictor Variables
#We compare these values with the Excel Calculations to check if the function is working correctly

#Entropy Function
#Calculating the Sums of Entropy (-1 *probability * log2(probability)) with respect to the response variable (Heart Disease)
entropy_function <- function(x) { 
  #Proportion table of the predictor variable with response variable (Heart Disease)
  #Issue if any probabilities are zero as log2(0) = -Inf, Fixing this with Laplace smoothing 
  proportion <- function(x) {prop.table(table(heart_data[,x],heart_data[,14]) + 1e-6, margin = 1)}
  #We multiply these by the proportion in each of these rows with the Sums of Entropy
  sum(prop.table(table(heart_data[,x]))*rowSums(-proportion(x)*log2(proportion(x))))
  }

#Calculating the overall entropy for the response variable (Heart Disease).
heart_disease_prop <- prop.table(table(heart_data$Heart_Disease))
entropy_heart_disease <-sum(-heart_disease_prop*log2(heart_disease_prop))

#Calculating the Entropy and Info Gain for Categorical predictor variables
categorical_variables <- c(2,3,6,7,9,11,13)

#Calculating all Entropy's and Info Gains and storing in a vector
entropys <- NULL
info_gains <- NULL
for (x in categorical_variables) { 
  entropy <- entropy_function(x)
  entropys <- c(entropys,entropy)
  info_gain <- entropy_heart_disease-entropy_function(x)
  info_gains <- c(info_gains,info_gain)
  if (x == 13) 
  {
    print(c("Entropys:",entropys))
    #Predictor variable having Minimum Entropy
    print(c("Entropy Minimum:",colnames(heart_data[categorical_variables[which.min(entropys)]])))
    
    print(c("Info Gains:",info_gains))
    #Predictor variable having Maximum Information Gain
    print(c("Info Gain maximum", entropy_heart_disease-min(entropys)))
  }  
}

#Checking with the Excel Calculations,'Thal' has the maximum Information Gain
#Function is working correctly
#Now we calculate Entropy and Information Gain for the Training Data Set

####################################
#Entropy and Information Gain for the Training Data Set
####################################

#Calculating the Entropy and Information Gain of all the Categorical predictor Variables in the Training Data Set.

#Entropy Function
#Calculating the Sums of Entropy (-1 *probability * log2(probability)) with respect to the response variable (Heart Disease)
entropy_function <- function(x) { 
  #Proportion table of the predictor variable with response variable (Heart Disease)
  #Issue if any probabilities are zero as log2(0) = -Inf, Fixing this with Laplace smoothing 
  proportion <- function(x) {prop.table(table(training_set_data[,x],training_set_data[,14]) + 1e-6, margin = 1)}
  #We multiply these by the proportion in each of these rows with the Sums of Entropy
  sum(prop.table(table(training_set_data[,x]))*rowSums(-proportion(x)*log2(proportion(x))))
}

#Calculating the overall entropy for the response variable (Heart Disease) in the Training Data Set.
heart_disease_prop <- prop.table(table(training_set_data$Heart_Disease))
entropy_heart_disease <-sum(-heart_disease_prop*log2(heart_disease_prop))

#Calculating the Entropy and Info Gain for Categorical predictor variables in the Training Data Set
categorical_variables <- c(2,3,6,7,9,11,13)

#Calculating all Entropy's and Info Gains and storing in a vector
entropys <- NULL
info_gains <- NULL
for (x in categorical_variables) { 
  entropy <- entropy_function(x)
  entropys <- c(entropys,entropy)
  info_gain <- entropy_heart_disease-entropy_function(x)
  info_gains <- c(info_gains,info_gain)
  if (x == 13) 
  {
    print(c("Entropys:",entropys))
    #Predictor variable having Minimum Entropy in the Training Data Set
    print(c("Entropy Minimum:",colnames(training_set_data[categorical_variables[which.min(entropys)]])))
    
    print(c("Info Gains:",info_gains))
    #Predictor variable having Maximum Information Gain in the Training Data Set
    print(c("Info Gain maximum", entropy_heart_disease-min(entropys)))
  }  
}



####################################
#Using Binary Split - Categorical Predictor Variables
####################################

#Now, we calculate the Entropy and Information Gain for the Categorical variables but having only the Binary Split

#Creating a copy of the Training Data Set
t1 <- training_set_data
head(t1)

#binary split for Chest Pain Type predictor variable
summary(training_set_data$Chest_Pain_Type)
16+36+58
t1$Chest_Pain_Type <- ifelse(t1$Chest_Pain_Type==1 | t1$Chest_Pain_Type==2 | t1$Chest_Pain_Type==3,"0","1")

#binary split for Resting ECG Results predictor variable
summary(training_set_data$Resting_ECG_Results)
t1$Resting_ECG_Results <- ifelse(t1$Resting_ECG_Results==0 | t1$Resting_ECG_Results==1,"0","1")

#binary split for Slope predictor variable
summary(training_set_data$Slope)
t1$Slope <- ifelse(t1$Slope==1 | t1$Slope==3,"0","1")

#binary split for Thal predictor variable
summary(training_set_data$Thal)
t1$Thal <- ifelse(t1$Thal==6 | t1$Thal==7,"Thal 6 0r 7","Thal 3")

#Entropy Function
#Calculating the Sums of Entropy (-1 *probability * log2(probability)) with respect to the response variable (Heart Disease)
entropy_function <- function(x) { 
  #Proportion table of the predictor variable with response variable (Heart Disease)
  #Issue if any probabilities are zero as log2(0) = -Inf, Fixing this with Laplace smoothing 
  proportion <- function(x) {prop.table(table(t1[,x],t1[,14]) + 1e-6, margin = 1)}
  #We multiply these by the proportion in each of these rows with the Sums of Entropy
  sum(prop.table(table(t1[,x]))*rowSums(-proportion(x)*log2(proportion(x))))
}

#Calculating the overall entropy for the response variable (Heart Disease) in the Training Data Set.
heart_disease_prop <- prop.table(table(t1$Heart_Disease))
entropy_heart_disease <-sum(-heart_disease_prop*log2(heart_disease_prop))

#Calculating the Entropy and Info Gain for Categorical predictor variables in the Training Data Set
categorical_variables <- c(2,3,6,7,9,11,13)

#Calculating all Entropy's and Info Gains and storing in a vector
entropys <- NULL
info_gains <- NULL
for (x in categorical_variables) { 
  entropy <- entropy_function(x)
  entropys <- c(entropys,entropy)
  info_gain <- entropy_heart_disease-entropy_function(x)
  info_gains <- c(info_gains,info_gain)
  if (x == 13) 
  {
    print(c("Entropys:",entropys))
    #Predictor variable having Minimum Entropy in the Training Data Set
    print(c("Entropy Minimum:",colnames(t1[categorical_variables[which.min(entropys)]])))
    
    print(c("Info Gains:",info_gains))
    #Predictor variable having Maximum Information Gain in the Training Data Set
    print(c("Info Gain maximum", entropy_heart_disease-min(entropys)))
  }  
}



####################################
#Using Binary Split - including continuous numeric Predictor Variables
####################################


#binary split for Age predictor variable
summary(training_set_data$Age)
t1$Age <- ifelse(t1$Age<=55,"Below 55","Above 55")

#binary split for Blood Pressure predictor variable
summary(training_set_data$Blood_Pressure)
t1$Blood_Pressure <- ifelse(t1$Blood_Pressure<=130,"Below 130","Above 130")

#binary split for Cholesterol predictor variable
summary(training_set_data$Cholestoral)
t1$Cholestoral <- ifelse(t1$Cholestoral<=250,"Below 250","Above 250")

#binary split for Max Heart Rate Achieved predictor variable
summary(training_set_data$Max_Heart_Rate_Achieved)
t1$Max_Heart_Rate_Achieved <- ifelse(t1$Max_Heart_Rate_Achieved<=150,"Below 150","Above 150")

#binary split for Oldpeak predictor variable
summary(training_set_data$Oldpeak)
t1$Oldpeak <- ifelse(t1$Oldpeak<=1.1,"Below 1.1","Above 1.1")

#binary split for No of Major Vessels predictor variable
table(training_set_data$No_of_Major_Vessels)
t1$No_of_Major_Vessels <- ifelse(t1$No_of_Major_Vessels==0 ,"0","1")

View(t1)

#Entropy Function
#Calculating the Sums of Entropy (-1 *probability * log2(probability)) with respect to the response variable (Heart Disease)
entropy_function <- function(x) { 
  #Proportion table of the predictor variable with response variable (Heart Disease)
  #Issue if any probabilities are zero as log2(0) = -Inf, Fixing this with Laplace smoothing 
  proportion <- function(x) {prop.table(table(t1[,x],t1[,14]) + 1e-6, margin = 1)}
  #We multiply these by the proportion in each of these rows with the Sums of Entropy
  sum(prop.table(table(t1[,x]))*rowSums(-proportion(x)*log2(proportion(x))))
}

#Calculating the overall entropy for the response variable (Heart Disease) in the Training Data Set.
heart_disease_prop <- prop.table(table(t1$Heart_Disease))
entropy_heart_disease <-sum(-heart_disease_prop*log2(heart_disease_prop))

#Calculating the Entropy and Info Gain for Categorical predictor variables in the Training Data Set
all_variables <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)

#Calculating all Entropy's and Info Gains and storing in a vector
entropys <- NULL
info_gains <- NULL
for (x in all_variables) { 
  entropy <- entropy_function(x)
  entropys <- c(entropys,entropy)
  info_gain <- entropy_heart_disease-entropy_function(x)
  info_gains <- c(info_gains,info_gain)
  if (x == 13) 
  {
    print(c("Entropys:",entropys))
    #Predictor variable having Minimum Entropy in the Training Data Set
    print(c("Entropy Minimum:",colnames(t1[all_variables[which.min(entropys)]])))
    
    print(c("Info Gains:",info_gains))
    #Predictor variable having Maximum Information Gain in the Training Data Set
    print(c("Info Gain maximum", entropy_heart_disease-min(entropys)))
  }  
}


####################################
#Next Level Split
####################################

#Subsetting the data set into two as Thal is our root node and we need to calculate the next level split
subset_thal_3 <- subset(t1,t1$Thal == 'Thal 3')
View(subset_thal_3)

subset_thal_6or7 <- subset(t1,t1$Thal == 'Thal 6 0r 7')
View(subset_thal_6or7)

#Now calculating the first level split for subset - Thal 3 - Left Side

#Entropy Function
#Calculating the Sums of Entropy (-1 *probability * log2(probability)) with respect to the response variable (Heart Disease)
entropy_function <- function(x) { 
  #Proportion table of the predictor variable with response variable (Heart Disease)
  #Issue if any probabilities are zero as log2(0) = -Inf, Fixing this with Laplace smoothing 
  proportion <- function(x) {prop.table(table(subset_thal_3[,x],subset_thal_3[,14]) + 1e-6, margin = 1)}
  #We multiply these by the proportion in each of these rows with the Sums of Entropy
  sum(prop.table(table(subset_thal_3[,x]))*rowSums(-proportion(x)*log2(proportion(x))))
}

#Calculating the overall entropy for the response variable (Heart Disease) in the Training Data Set.
heart_disease_prop <- prop.table(table(subset_thal_3$Heart_Disease))
entropy_heart_disease <-sum(-heart_disease_prop*log2(heart_disease_prop))

#Calculating the Entropy and Info Gain for Categorical predictor variables in the Training Data Set
#Removing Thal variable
all_variables_but_thal <- c(1,2,3,4,5,6,7,8,9,10,11,12)

#Calculating all Entropy's and Info Gains and storing in a vector
entropys <- NULL
info_gains <- NULL
for (x in all_variables) { 
  entropy <- entropy_function(x)
  entropys <- c(entropys,entropy)
  info_gain <- entropy_heart_disease-entropy_function(x)
  info_gains <- c(info_gains,info_gain)
  if (x == 12) 
  {
    print(c("Entropys:",entropys))
    #Predictor variable having Minimum Entropy in the Training Data Set
    print(c("Entropy Minimum:",colnames(subset_thal_3[all_variables_but_thal[which.min(entropys)]])))
    
    print(c("Info Gains:",info_gains))
    #Predictor variable having Maximum Information Gain in the Training Data Set
    print(c("Info Gain maximum", entropy_heart_disease-min(entropys)))
  }  
}


#We get No of Major Vessels as the next split for the Thal 3 data set

#####################################

#Now calculating the first level split for subset - Thal 6 Or 7 - Right Side

#Entropy Function
#Calculating the Sums of Entropy (-1 *probability * log2(probability)) with respect to the response variable (Heart Disease)
entropy_function <- function(x) { 
  #Proportion table of the predictor variable with response variable (Heart Disease)
  #Issue if any probabilities are zero as log2(0) = -Inf, Fixing this with Laplace smoothing 
  proportion <- function(x) {prop.table(table(subset_thal_6or7[,x],subset_thal_6or7[,14]) + 1e-6, margin = 1)}
  #We multiply these by the proportion in each of these rows with the Sums of Entropy
  sum(prop.table(table(subset_thal_6or7[,x]))*rowSums(-proportion(x)*log2(proportion(x))))
}

#Calculating the overall entropy for the response variable (Heart Disease) in the Training Data Set.
heart_disease_prop <- prop.table(table(subset_thal_6or7$Heart_Disease))
entropy_heart_disease <-sum(-heart_disease_prop*log2(heart_disease_prop))

#Calculating the Entropy and Info Gain for Categorical predictor variables in the Training Data Set
#Removing Thal variable
all_variables_but_thal <- c(1,2,3,4,5,6,7,8,9,10,11,12)

#Calculating all Entropy's and Info Gains and storing in a vector
entropys <- NULL
info_gains <- NULL
for (x in all_variables) { 
  entropy <- entropy_function(x)
  entropys <- c(entropys,entropy)
  info_gain <- entropy_heart_disease-entropy_function(x)
  info_gains <- c(info_gains,info_gain)
  if (x == 12) 
  {
    print(c("Entropys:",entropys))
    #Predictor variable having Minimum Entropy in the Training Data Set
    print(c("Entropy Minimum:",colnames(subset_thal_6or7[all_variables_but_thal[which.min(entropys)]])))
    
    print(c("Info Gains:",info_gains))
    #Predictor variable having Maximum Information Gain in the Training Data Set
    print(c("Info Gain maximum", entropy_heart_disease-min(entropys)))
  }  
}

#We get No of Major Vessels as the next split for the Thal 6 or 7 data set


####################################
#Next Level Splits for the rest of the decision tree
####################################

#After calculating the first level split, we can then calculate the next splits of the decision tree
#Using for loops and subsetting the data, we need to build a function to do the same recursively


####################################
#Building the tree using tree function and Predicting the class for the test set
####################################

#install.packages("tree")
library(tree)

#Setting the plot window to show one plot
par(mfrow=c(1,1))
set.seed(850)
tree.1 <- tree(Heart_Disease~.,heart_data,subset = training_set)
tree.1
plot(tree.1)
text(tree.1,pretty=1)

summary(tree.1)
table(heart_data$Heart_Disease)/nrow(heart_data)
#55.6% is the baseline rate, our model need to be better than this rate.

tree.pred1 <- predict(tree.1,test_set,type="class")

train.test <- Heart_Disease[-training_set]
#Creating a confusion matrix
table(tree.pred1,train.test)
# The overall accuracy rate here is (23 + 16)/(54)
(23 + 16)/(54)
# equals 0.7222
# Since this is better than baseline rate,  this is good model.


####################################
#Pruning the Tree using Cross fold Validation
####################################

# Now use cross fold validation to tune the parameter, size, which equals the number of leaves
# this is done with by pruning the tree using cross fold validation

set.seed(850)
cv.tree.1 <- cv.tree(tree.1,K=10, FUN=prune.misclass)
summary(tree.1)
# 10 folds of cross validation are being used.

names(cv.tree.1)
cv.tree.1

par(mfrow=c(1,2))
plot(cv.tree.1$size,cv.tree.1$dev,type="b")
plot(cv.tree.1$k,cv.tree.1$dev,type="b")

par(mfrow=c(1,1))
#6 seems the best
prune.tree<-prune.misclass(tree.1,best=6)
plot(prune.tree)
text(prune.tree,pretty=0)
summary(prune.tree)

tree.pred<-predict(prune.tree,test_set,type="class")
table(tree.pred,train.test)
(24 + 17)/(54)
#From the confusion matrix, Accuracy Rate is 75.9%, which is better


####################################
#Random Forest Model
####################################

library(randomForest)

set.seed(850)
high.rf <- randomForest(Heart_Disease~.,data = heart_data, importance=TRUE)
print(high.rf)
(129+94)/270 #0.8259259


#training set
#no of tress = 1000
set.seed(850)
high.rf <- randomForest(Heart_Disease~.,data = heart_data,subset=training_set, ntree = 1000, importance=TRUE, do.trace=TRUE)
print(high.rf)
(109+74)/216 #0.8472222


importance(high.rf)
varImpPlot(high.rf)
#Thal and No of Major Vessels have much higher importance than other variables.

set.seed(850)
tuneRF(heart_data[,-c(14)],heart_data[,14], subset=training_set, mtryStart = 12,stepFactor = 2, ntreeTry = 500)

importance(high.rf)
varImpPlot(high.rf)

tree.pred_rf2 <- predict(high.rf, test_set, type = "class")
table(tree.pred_rf2, train.test)
(26 + 16)/(54) #0.7777778
