# Team 16 -----------------------------------------------------------------------------------------
 # Ben S
 # Smita D
 # Jiacan L
 # Andrew L
 # Jeffri B



# Part 0, Setup -----------------------------------------------------------------------------------
rm(list=ls()); options(scipen=999)
if (!require("tidyverse")) {install.packages("tidyverse"); library(tidyverse)}
p <- function(x) {par(mfrow = c(x[1], x[2]))}

# Import Data
churnData <- read.csv("Case4.csv", stringsAsFactors = T)
churnData$Surname <- NULL
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



# Part 2a, Data Split --------------------------------------------------------------------------------
set.seed(1325029) #hexatridecimal "seed" converted to decimal
divideData <- caret::createDataPartition(churnData$Exited, p=.8, list=FALSE)
train <- churnData[ divideData,]
test  <- churnData[-divideData,]



# Part 2b, Big Logistic --------------------------------------------------------------------------------
#TODO ON BigLogistic BRANCH!



# Part 2c, Center and Scale --------------------------------------------------------------------------------
library(caret)
preprocessing <- train |> preProcess(method=c("center","scale"))
train_trans <- preprocessing |> predict(train)
test_trans  <- preprocessing |> predict(test)



# Part 2d, LDA --------------------------------------------------------------------------------
library(MASS)

ldamodel <- lda(Exited~.,data = train_trans) ## train model with training set
ldamodel

predictions <- ldamodel |> predict(test_trans)
confusionMatrix(data = predictions$class, reference = test_trans$Exited, positive = "1") 

### True Positives:135
### False Positives:272
### True Negatives:1537
### False Negatives:55
### A total of 1672 samples were correctly predicted out of the total 1999 samples. Thus, the overall accuracy is 83.64%.
### The sensitivity comes from 135/(272+135), equals to 33.17%, indicating that it is not a very good performer.
### The specificity comes from 1537/(1537+55), equals to 96.55%




# Part 2e, QDA --------------------------------------------------------------------------------
#TODO ON QDA BRANCH!



# Part 2f, KNN --------------------------------------------------------------------------------
#TODO ON KNN BRANCH!



# Part 2g, Model Selection --------------------------------------------------------------------------------
#TODO ON MAIN BRANCH!

