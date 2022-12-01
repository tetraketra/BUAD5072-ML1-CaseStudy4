# Team 16 -----------------------------------------------------------------------------------------
 # Ben    S
 # Smita  D
 # Jiacan L
 # Andrew L
 # Jeffri B



# Part 0, Setup -----------------------------------------------------------------------------------
rm(list=ls()); options(scipen=999)
if (!require("tidyverse")) {install.packages("tidyverse"); library(tidyverse)}
if (!require("caret")) {install.packages("caret"); library(caret)}
p <- function(x) {par(mfrow = c(x[1], x[2]))}

# Import Data
churnData <- read.csv("Case4.csv", stringsAsFactors = T)
churnData$Surname   <- NULL
churnData$RowNumber <- NULL
    #These are is not predictive.

# Inspect Structure
str(churnData)
summary(churnData)
    #We see a few binary variables that are clearly factors.
    #It may be more appropriate to treat NumOfProducts and Tenure as factors too.
        #We would not expect the effect of using 4 bank products to be 4x the effect of using 1, and the options are few.
        #Your first few years with a company are more uncertain than later years.
    #We'll add factorized versions of both and see if they're worthwhile on a model-by-model basis.
        #NumOfProducts will be factorized directly.
        #Tenure will get bins of c("short", "medium", "long").
            #According to the Bureao of Labor Stats, average tenure is 4 years. 3-5 will thus be "medium" tenure.
            #https://www.bls.gov/news.release/tenure.nr0.htm

# Make Factors
factorize <- c("Exited", "IsActiveMember", "HasCrCard", "NumOfProducts")
churnData[factorize]       <- lapply(churnData[factorize], as.factor)
churnData["Tenure_FACTOR"] <- vapply(churnData["Tenure"], function(x) {ifelse(x <= 2, "short", ifelse(x <= 5, "medium", "long"))}, character(nrow(churnData))) |> as.factor()

# Re-investigate Structure
str(churnData)
summary(churnData)



# Part 1, Small Logistic --------------------------------------------------------------------------------

# Model Training
shortlogmodel <- glm(Exited ~ Age + Gender, family = binomial, data = churnData)
summary(shortlogmodel)
    #All variables are significant.

# Assumption 1: Linear Logit
logit <- shortlogmodel$linear.predictors
plot(logit, churnData$Age)
    #Gender is not a numeric predictor, so we can only check for Age.
    #We see two distinct and parallel lines, one for each gender in the dataset.
    #The logit is linear.

# Assumption 2: No Multicolinearity
car::vif(shortlogmodel)
    #Nothing even close to 5 or 10.
    #No multicolinearity.

# Assumption 3: No Influential Outliers
plot(shortlogmodel, which = 4); abline(h = 4/nrow(churnData), col = "red")
    #Those top 4 points look concerning. Let's extract them.
cooks.distance(shortlogmodel) |> sort(decreasing = T) |> head(4)
    #These range from 6.7 to 8.8 times a reasonable Cook's D cutoff of 4/N.
    #However, over 5% of our data is over the above threshold.
        #Cook's distance is inversely scaled by the number of predictors.
        #Using only two predictors has artificially inflated this measure.
        #The visual inspection is more appropriate, and that still yields four standout points.
        #We can compare our coefficients with and without these points.

coefs_base <- coef(shortlogmodel)

outliers <- cooks.distance(shortlogmodel) |> sort(decreasing = T) |> head(4) |> names() |> as.numeric()
subset <- churnData[-outliers,]

coefs_new <- coef(glm(Exited ~ Age + Gender, family = binomial, data = subset))

rbind(coefs_base, coefs_new)
    #The effect is noticeable, but not extreme.
    #We believe there are no strongly influential outliers.

# Assumption 4: Independence of Errors
plot(shortlogmodel, which = 1)
    #We once again have our two clear groups, one for each gender in the dataset.
    #We see a slight negative relationship.
    #We believe errors are not fully independent.

# Interpretation
summary(shortlogmodel)
    #The effects of Age and Gender on the logged odds of churning are:
        #Each year of age       = +0.0631
        #Being male             = -0.5370
        #Intercept purposefully absent. Read on.
    #These variables do not capture the effect of having a non-modal gender.
    #The leftmost point that is reasonably considerable is not the intercept, but Age = 18.
        #At 18, female employees have a 7.40% probability of churning.
        #At 18, male employees have a 4.46% probability of churning.
        #Both only go up with age.



# Part 2a, Data Split --------------------------------------------------------------------------------
set.seed(1325029) #hexatridecimal "seed" converted to decimal
divideData <- caret::createDataPartition(churnData$Exited, p=.8, list=FALSE)
train <- churnData[ divideData,]
test  <- churnData[-divideData,]



# Part 2b, Big Logistic --------------------------------------------------------------------------------

# Model Selection
logisticmodel <- glm(Exited ~ ., family = binomial, data = train)
summary(logisticmodel)

logisticmodel1 <- glm(Exited ~ CreditScore + Geography + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary + Tenure_FACTOR, family = binomial, data = train)
summary(logisticmodel1)

logisticmodel2 <- glm(Exited ~ CreditScore + Geography + Gender + Age + Tenure + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary + Tenure_FACTOR, family = binomial, data = train)
summary(logisticmodel2)

logisticmodel3 <- glm(Exited ~ CreditScore + Geography + Gender + Age + Tenure + NumOfProducts + HasCrCard + IsActiveMember + Tenure_FACTOR, family = binomial, data = train)
summary(logisticmodel3)

logisticmodel4 <- glm(Exited ~  Geography + Gender + Age + Tenure + NumOfProducts + HasCrCard + IsActiveMember + Tenure_FACTOR, family = binomial, data = train)
summary(logisticmodel4)

logisticmodel5 <- glm(Exited ~  Geography + Gender + Age + NumOfProducts + HasCrCard + IsActiveMember + Tenure_FACTOR, family = binomial, data = train)
summary(logisticmodel5)

logisticmodel6 <- glm(Exited ~  Geography + Gender + Age + NumOfProducts + IsActiveMember + Tenure_FACTOR, family = binomial, data = train)
summary(logisticmodel6)

logisticmodel7<- glm(Exited ~  Geography + Gender + Age + NumOfProducts + IsActiveMember , family = binomial, data = train)
summary(logisticmodel7)

logisticmodel <- logisticmodel7

summary(logisticmodel)
    #I decided on this model with these variables because I took out the ones that were not significant.
    #Some of the variables in this model are not significant like GeographySpain,
    #but since GeographyGermany is significant, GeographySpain must stay.
    #(Same with NumOfProducts4.)


# Assumption 1: Linear Logit
logit <- logisticmodel$linear.predictors
plot(logit, train$Age)


# Assumption 2: No Multicolinearity
car::vif(logisticmodel)
    #Nothing even close to 5 or 10.
    #No multicolinearity.

# Assumption 3: No Influential Outliers
plot(logisticmodel, which = 4); abline(h = 4/nrow(churnData), col = "red")
    #There seem to be many points that are far from the median,
    #but we do not see a reason to extract any after seeing the effect previously.
cooks.distance(logisticmodel) |> sort(decreasing = T) |> head(10)
    #A large portion of our data is above Cook's D cutoff of 4/N, but no outliers seem extraordinary.
    #It is more accurate to say that the data has a wide spread, not that it has many outliers.
    #We should check, just to be sure.

coefs_base <- coef(logisticmodel)
outliers <- cooks.distance(logisticmodel) |> sort(decreasing = T) |> head(4) |> names() |> as.numeric()
subset <- train[-outliers,]
coefs_new <- coef(glm(Exited ~ Geography + Gender + Age + NumOfProducts + IsActiveMember, family = binomial, data = subset))
rbind(coefs_base, coefs_new)
    #The effect is very minor.
    #We believe there are no strongly influential outliers.

# Assumption 4: Independence of Errors
plot(logisticmodel, which = 1)
    #We once again have our two clear groups.
    #We see an odd shape, but no overall directional relationship.
    #We believe errors are independent.

# Model Metrics
probs<-predict(logisticmodel, test, type="response")
head(probs)
pred<-ifelse(probs>.5, 1,0)|> as.factor()
head(pred)

mean(pred!=test$Exited)
mean(pred==test$Exited)

class(pred);class(test$type)
confusionMatrix(pred,test$Exited,positive="1")

#Sensitivity = 0.34644 #This is the true positive rate and it is not very high
#Specificity = 0.96294 #This is the true negative rate and it is very high, so our model is very good at predicting who will stay.
    #This, however, is not the event of interest. Having a high sensitivity is more important and useful, but it is interesting that our model is good at predicting who will stay.
#There are 141 True Positives, 1533 True Negatives, 266 False Negatives, and 59 False Positives

#Overall, the model is not great at predicting who will churn, but it is very good at predicting who will stay. While this may be useful, it was not the purpose of the model


# Part 2c, Center and Scale --------------------------------------------------------------------------------
preprocessing <- train |> preProcess(method=c("center","scale"))
train_trans <- preprocessing |> predict(train)
test_trans  <- preprocessing |> predict(test)



# Part 2d, LDA --------------------------------------------------------------------------------
#TODO ON LDA BRANCH!



# Part 2e, QDA --------------------------------------------------------------------------------
#TODO ON QDA BRANCH!



# Part 2f, KNN --------------------------------------------------------------------------------
#TODO ON KNN BRANCH!



# Part 2g, Model Selection --------------------------------------------------------------------------------
#TODO ON MAIN BRANCH!

