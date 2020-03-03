#LR(nL)
#remove.packages("ggplot2") # Unisntall ggplot
#install.packages("ggplot2") # Install it again
#install.packages("ggthemes")
library(ggplot2)
library(MASS)
library(ISLR)
library(class)
library(ggthemes)
library(corrplot)
library(MASS)
library(glmnet)
library(magrittr)
library(dplyr)
library(plyr)

## Data Preparation

# Load data for NYC
load("~/GitHub/DSF/data/ny_inspect_data.RData")
#load("~/GitHub/DSF/data/ny_inspect_data2.RData")
ny_inspect_data_save <- ny_inspect_data
CC <- complete.cases(ny_inspect_data$Inspection.Grade)
ny_inspect_dat <- ny_inspect_data[CC,]
ny_inspect_dat <- na.omit(ny_inspect_dat)

numeric_data <- ny_inspect_data[,-c(1:3, 5:12, 20:24, 60:61, 97)]

# ensure that no variables contain NA
colSums(is.na(ny_inspect_data))

# Remove everything except the potential covariates
ny_data <- ny_inspect_data %>%
  dplyr::select(-c(Address,
                   Trade.Name, 
                   County, 
                   Inspection.Date, 
                   Owner.Name, 
                   Street, 
                   City, 
                   State.Code, 
                   Zip.Code, 
                   Deficiency.Number, 
                   Deficiency.Description, 
                   X, 
                   TractId, 
                   Location, 
                   State.per.CenTrac, 
                   County.per.CenTrac, 
                   State.per.County, 
                   neighbourhood_group, 
                   Pacific.per.County,
                   Latitude,
                   Longitude))

# For computation restriction, we need to limit our analysis to the most important covariates
# Therfore, we take the 20 variables with the highest correlation to Inspection Grade.

# Correlation in aboslute term to Inspetion Grade
res <- abs(cor(ny_data))[,1]
res <- as.data.frame(res)
# 21 largest correlation (inclusive Inspection Grade to itself)
largest_corr <- sort(res[,1], decreasing = TRUE)[1:41]
covariates <- rownames(res)
# 20 variables with the larges correlation to inspection grade
covariates <- covariates[which(res[,1] %in% largest_corr)]

# Demographic data are extremely prone to multicolinrearity
# Check correlation of variables
cor = cor(ny_data)
# exclude all that are highly correlated to each other
covariates <- covariates[which(!(covariates %in% c("SelfEmployed.per.County", 
                                                   "Walk.per.County", 
                                                   "PrivateWork.per.County", 
                                                   "Construction.per.County", 
                                                   "Drive.per.County", 
                                                   "Carpool.per.County", 
                                                   "Men.per.County", 
                                                   "MeanCommute.per.County", 
                                                   "WorkAtHome.per.County",
                                                   "FamilyWork.per.County",
                                                   "PublicWork.per.County",
                                                   "VotingAgeCitizen.per.County",
                                                   "Transit.per.County",
                                                   "Employed.per.County",
                                                   "Native.per.County",
                                                   "TotalPop.per.County",
                                                   "PrivateWork.per.CenTrac",
                                                   "IncomeErr.per.County",
                                                   "IncomePerCap.per.County",
                                                   "Hispanic.per.County")))]

# Select 20 best covariates and modify Inspection.Grade to fit numeric
#ny_data$Inspection.Grade <- na.omit(ny_data$Inspection.Grade)
#ny_data <- ny_data_save

apply(ny_data, 2, class)

ny_data <- ny_data %>%
  mutate(Inspection.Grade = factor(Inspection.Grade, levels = c(1, 2, 3), labels = c("A", "B", "C"))) %>%
  dplyr::select(covariates)

#ny_data <- mutate(ny_data, Inspection.Grade = factor(Inspection.Grade, levels =c(1, 2, 3), labels = c("A", "B", "C")))
#ny_data <- dplyr::select(ny_data,covariates)

#is.numeric(ny_data)
#apply(ny_data,2,class)

#ny_data <- (as.numeric(unlist(ny_data)))

#ny_data_save <- ny_data

rm(ny_inspect_data, res, cor, covariates, largest_corr)

set.seed(123)

# Preliminary visualization

ggplot(data = ny_inspect_data, aes(x=Inspection.Grade)) +
  geom_histogram(stat = "count", fill = "black") +
  theme(legend.position="top") +
  labs(title="Histogram of Inspection Grades",
       x="Grade from A to C",
       y = "Count") +
  theme_gray()

corrplot(cor(tD[,c("Number_of_Reviews", "Income.per.CenTrac")]), method = "circle")
corrplot(cor(ny_data), method = "circle", use="pairwise.complete.obs")
corrplot(cor(numeric_data), method = "circle", use="pairwise.complete.obs")
corrplot.mixed(cor(numeric_data), lower.col = "black", number.cex = 1)




    
#For only "Issue" & "No issue"
    
tD <- ny_inspect_data

tD$Inspection.Grade[tD$Inspection.Grade == 1] <- "Issue"
tD$Inspection.Grade[tD$Inspection.Grade == 2] <- "Issue"
tD$Inspection.Grade[tD$Inspection.Grade == 3] <- "No Issue"

f2 <-function(x){
  if(x == 1){"No Issue"}
  else if(x == 2){"Issue"}
  else {"Issue"}
}


#tN$Inspection.Grade <- lapply(ny_data$Inspection.Grade, f2)
# convert column classes to factor
#tNtest <- tN
#rm(tNtest)
#tN[sapply(tN, is.character)] <- lapply(tN[sapply(tN, is.list)], as.factor)


#tDtrain <- ny_inspect_data[]

ggplot(data = tD, aes(x=Inspection.Grade)) +
  geom_histogram(stat = "count", fill = "black") +
  theme(legend.position="top") +
  labs(title="Histogram of Inspection Grades",
       x="from No Issue to Issue",
       y = "Count") +
  theme_gray()



#####################################
#Logistical Regression (no L)
################################


############# with tN
# Binomial logistical regression i.e. Only "Issue" & "No issue"
tN <- ny_data
tN$Inspection.Grade <- revalue(tN$Inspection.Grade, c(A = 0, B = 1, C = 1))
class(tN$Inspection.Grade)

train_tN <- sample.int(n = nrow(tN), size = floor(.75*nrow(tN)), replace = F)

glm.fits = glm(formula = as.factor(Inspection.Grade) ~ count + chain + shop_density + rating_closest_neighb + subway_distance + TotalPop.per.CenTrac + Women.per.CenTrac + Hispanic.per.CenTrac + White.per.CenTrac + Black.per.CenTrac + Service.per.CenTrac + Drive.per.CenTrac + Carpool.per.CenTrac + Transit.per.CenTrac + Walk.per.CenTrac + PublicWork.per.CenTrac + Women.per.County + White.per.County + Asian.per.County + OtherTransp.per.County, family = binomial, data = tN, subset = train_tN)

summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef

glm.probs = predict(glm.fits, tN[-train_tN,], type = "response")
# gives the probability that there will be an Issue
contrasts(as.factor(tD$Inspection.Grade))
glm.pred = rep("Issue", nrow(tDa[-train_tN,]))
glm.pred[glm.probs>=.5] = "No Issue"

all(is.na(tN$Income.per.CenTrac) == FALSE)
table(glm.pred)
table(tN$Inspection.Grade)
table(glm.pred, tDa[-train_tDa,]$Inspection.Grade)
mean(glm.pred == tDa[-train_tDa,]$Inspection.Grade)
mean(glm.pred != tDa[-train_tDa,]$Inspection.Grade)
# could be interesting to specifically find out how often Issue was guessed correctly

# Compute predictions and compare to actual assessments






#
# Ordinal logistical regression
#

pN <- ny_data
pN$Inspection.Grade <- revalue(pN$Inspection.Grade, c(A = 0, B = 1, C = 2))
#pN <-data.frame(Inspection.Grade=factor(pN[,"Inspection.Grade"]),scale(pDa[,c("Inspection.Grade", "Number_of_Reviews", "Income.per.CenTrac")]))
train_pN <- sample.int(n = nrow(pN), size = floor(.75*nrow(pN)), replace = F)

#p.glm.fits = glm(formula = as.factor(Inspection.Grade) ~ Number_of_Reviews + Income.per.CenTrac, family = multinomial, data = pDa, subset = train_pDa)

polr <- polr(formula = as.factor(Inspection.Grade) ~ Inspection.Grade + count + chain + shop_density + rating_closest_neighb + subway_distance + TotalPop.per.CenTrac + Women.per.CenTrac + Hispanic.per.CenTrac + White.per.CenTrac + Black.per.CenTrac + Service.per.CenTrac + Drive.per.CenTrac + Carpool.per.CenTrac + Transit.per.CenTrac + Walk.per.CenTrac + PublicWork.per.CenTrac + Women.per.County + White.per.County + Asian.per.County + OtherTransp.per.County, data = pN, subset = train_pN)

summary(polr)
coef(polr)
summary(polr)$coef

polr.probs = predict(glm.fits, tDa[-train_pDa,], type = "response")
# gives the probability that there will be an Issue
contrasts(as.factor(pDa$Inspection.Grade))
polr.pred = rep("C", nrow(pDa[-train_tDa,]))
p.glm.pred[polr.probs>=.33 && polr.probs <= .66] = "B"
p.glm.pred[polr.probs>=.66] = "A"

all(is.na(pDa$Income.per.CenTrac) == FALSE)
table(polr.pred)
table(polr.pred, tDa[-train_tDa,]$Inspection.Grade)
mean(polr.pred == tDa[-train_tDa,]$Inspection.Grade)
mean(polr.pred != tDa[-train_tDa,])


#####################################
#Logistical Regression with Lasso
################################
######
# with test values
# Lasso Issue vs. No Issue

x_var <- model.matrix(Inspection.Grade~. , tDa[train_tDa,])[,-1]
y_var <- ifelse(tDa[train_tDa, "Inspection.Grade"] == "Issue", 1, 0)

cv.lasso <- cv.glmnet(x_var, y_var, alpha = 1, family = "binomial", lambda = NULL)
lasso <- glmnet(x_var, y_var, alpha = 1, family = "binomial", lambda = cv.lasso$lambda.min)
coef(lasso)

x_test <- model.matrix(Inspection.Grade~. , tDa[-(train_tDa),])[,-1]
lasso.probabilities <- lasso %>% predict(newx = x_test)
lasso.predicted.classes <- ifelse(lasso.probabilities > 0.5, "Issue", "No Issue")

#Accuracy of model
observed.classes <- tDa[-(train_tDa), "Inspection.Grade"]
mean(predicted.classes == observed.classes)



#Real values
#Lasso Issue vs. No Issue

x_var <- model.matrix(Inspection.Grade~. , tDa[train_tDa,])[,-1]
y_var <- tN$Inspection.Grade

cv.lasso <- cv.glmnet(x_var, y_var, alpha = 1, family = "binomial", lambda = NULL)
lasso <- glmnet(x_var, y_var, alpha = 1, family = "binomial", lambda = cv.lasso$lambda.min)
coef(lasso)

x_test <- model.matrix(Inspection.Grade~. , tDa[-(train_tDa),])[,-1]
lasso.probabilities <- lasso %>% predict(newx = x_test)
lasso.predicted.classes <- ifelse(lasso.probabilities > 0.5, "Issue", "No Issue")

#Accuracy of model
observed.classes <- tDa[-(train_tDa), "Inspection.Grade"]
mean(predicted.classes == observed.classes)

#
# Lasso Ordinal logistic regression
#

