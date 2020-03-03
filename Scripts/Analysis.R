# Header----#############################################################################

# install.packages("rsample") # data splitting
# install.packages("ranger")
# install.packages("caret")
# install.packages("pdp")
# install.packages("vtreat")
# install.packages("xgboost")
# install.packages("h2o")

library(tidyverse)
library(plyr) # for ldply (similar to apply but output is a df not a list)
library(MASS) # For Discriminant Analysis
library(ISLR) # For Discriminant Analysis
library(class) # For KNN
library(randomForest) # basic implementation of random forests
library(vtreat)
library(pdp) # model visualization
library(xgboost)
library(h2o)
library(rsample) # data splitting 
library(ranger) # a faster implementation of random forests
library(caret) # an aggregator package for performing many machine learning models


#########################################################################################

# Tranformation before analysis----######################################################

set.seed(123)

# Load data for NYC
load("./data/ny_inspect_data.RData")

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
# 41 largest correlation (inclusive Inspection Grade to itself)
largest_corr <- sort(res[,1], decreasing = TRUE)[1:41]
covariates <- rownames(res)
# 40 variables with the larges correlation to inspection grade
covariates <- covariates[which(res[,1] %in% largest_corr)]

# Demographic data are extremely prone to multicolinrearity
# Correlation matrix reveals which covariate pairs are an issue
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
#"Construction.per.County", "Drive.per.County"

# Select 20 best covariates
ny_data <- ny_data %>%
  mutate(Inspection.Grade = factor(Inspection.Grade, levels = c(1, 2, 3), labels = c("A", "B", "C"))) %>%
  dplyr::select(covariates)

rm(ny_inspect_data, res, cor, covariates, largest_corr)

#########################################################################################

# Histograms----#########################################################################

# Histogram of the inspection classes - city level
Plot4 <- ggplot(data = ny_data, aes(x=Inspection.Grade)) +
  geom_histogram(stat = "count", color="darkblue", fill="lightblue") +
  theme(legend.position="top") +
  labs(title="Histogram of Inspection Grades",
       x="Grade from A to C",
       y = "Count") +
  theme_gray() +
  theme(legend.position="right",
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

ggsave("./plots/Plot4_Hist_NYC.png", plot = Plot4, width = 7, height = 4, dpi = 300)

# Load state data
load("./data/inspect_data.RData")

# Change inspection grade to factor
inspect_data <- inspect_data %>%
  mutate(Inspection.Grade = factor(Inspection.Grade, levels = c(1, 2, 3), labels = c("A", "B", "C")))

# Histogram of the inspection classes - city level
Plot5 <- ggplot(data = inspect_data, aes(x=Inspection.Grade)) +
  geom_histogram(stat = "count", color="darkblue", fill="lightblue") +
  theme(legend.position="top") +
  labs(title="Histogram of Inspection Grades",
       x="Grade from A to C",
       y = "Count") +
  theme_gray() +
  theme(legend.position="right",
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

ggsave("./plots/Plot5_Hist_NYState.png", plot = Plot5, width = 7, height = 4, dpi = 300)

rm(inspect_data, Plot4, Plot5)

#########################################################################################

# Not used functions----#################################################################

# implements forward-stepwise-selection and K-Fold CV
# as suggested by James et al (pp. 205 - 206)
# First we wanted to estimate all possible model combinations
# But it took too long since we use a relatively high number of covariates
best_subset_selection <- function(df_train, df_test, Y, FUN){
  # all covariates
  col_names <- colnames(df_train)
  col_names <- col_names[which(col_names != Y)]
  comb_nrs <- 2^length(col_names) - 1
  var_comb <- matrix(data = NA, ncol = 1, nrow = comb_nrs)
  row_names <- c()
  row_nr <- 1
  comb_size <- 0
  iterations <- length(col_names)
  for(i in 1:iterations){
    comb_size <- comb_size + ncol(combn(col_names, i))
    row_names <- c(row_names, combn(col_names, i, function(x) paste(x, collapse='\n')))
    var_comb[row_nr:comb_size, 1] <- combn(col_names, i, function(x) paste(x, collapse=' + '))
    row_nr <- row_nr + ncol(combn(col_names, i))
  }
  # add numbers to row labels
  row_names <- paste(1:length(row_names), sep = " ", row_names)
  error_rate <- matrix(data = NA, ncol = 1, nrow = comb_nrs)
  rownames(error_rate) <- row_names
  for(i in 1:comb_nrs){
    myformula <- paste( Y, '~', var_comb[i, 1] )
    myformula <- as.formula(myformula)
    model_fit <- FUN(myformula, data = df_train)
    model_pred <- predict(model_fit, df_test)
    # If model = discriminant analysis
    if (length(model_pred) == 3){
      model_pred <- model_pred$class 
    }
    correct_pred <- which(model_pred != as.matrix(df_test[Y]))
    error <- length(correct_pred) / nrow(df_test[Y])
    error_rate[i, 1] <- error
  }
  return(error_rate)
}

# Implements K-Fold Cross Validation (not integrated in a function)
k_fold_CV <- function(df, Y, K, FUN){
  fold <- round(nrow(df) / K)
  cross_val_err = matrix(data = NA, nrow = 2^(ncol(df)-1) - 1, ncol = K)
  for(i in 1:K){
    train_data <- df[-c((1+(i-1)*fold):(i*fold)),]
    testing_data <- df[(1+(i-1)*fold):(i*fold),]
    err <- model_selection(train_data, testing_data, Y, FUN)
    cross_val_err[,i] <- err[, 1]
  }
  cross_val_err <- as.tibble(apply(cross_val_err, 1, mean, na.rm=TRUE))
  rownames(cross_val_err) = rownames(err)
  return(cross_val_err)
}

# Implements over- and under-bagging with Out-of-bag errors
# We regonized that results with OOB errors are misleading since
# every testing set would be highly imbalanced
over_under_bagging <- function(df, Y, B, sample_size, FUN){
  set.seed(123)
  classes <- as.matrix(unique(df[Y]))
  classes <- sort(classes)
  for(i in 1:length(classes)){
    nam <- paste("subset", classes[i], sep = "")
    assign(nam, df[which(as.matrix(df[Y])== classes[i]), ])
  }
  oob_err = matrix(data = NA, nrow = 2^(ncol(df) - 1) - 1, ncol = B)
  for(i in 1:B){
    sampleA <- sample(1:nrow(subsetA), sample_size[1], replace = T)
    sampleB <- sample(1:nrow(subsetB), sample_size[2], replace = T)
    sampleC <- sample(1:nrow(subsetC), sample_size[3], replace = T)
    train_data <- rbind(subsetA[sampleA, ], subsetB[sampleB, ], subsetC[sampleC, ])
    testing_data <- subsetA[-(sampleA),]
    testing_data <- testing_data[1:(sample_size[1]/2), ]
    testing_data <- rbind(testing_data,
                          subsetB[-(sampleB), ], 
                          subsetC[-(sampleC), ])
    err <- model_selection(train_data, testing_data, Y, FUN)
    oob_err[,i] <- err
  }
  oob_err_final <- as.tibble(apply(oob_err, 1, mean, na.rm=TRUE))
  oob_err_final <- cbind(rownames(err), oob_err_final)
  return(oob_err_final)
}

# Slightly adjusted function for knn
# ( changed or added lines are marked with #<<<<<< )
# Did not work because of limitations of the knn function
# knn has a max number of k of 1000. Discrete data with low variance
# created errors
forward_stepwise_selection <- function(df, Y, FUN, K = 10, k){
  browser()
  # All covariate names
  covariates <- colnames(df)
  covariates <- covariates[which(covariates != Y)]
  # number of covaraies
  p <- length(covariates)
  # df to store errors
  model_errors <- data.frame(Model = character(),
                             Error = double(),
                             stringsAsFactors=FALSE)
  # vector for used covariates
  model_covariates <- c()
  # model estimate for each number of covariates
  for(i in 0:(p-1)){
    # Not yet used covariates
    not_model_covariates <- covariates[which(!(covariates %in% model_covariates))]
    # identity matrix to select each variable once
    select_covaraite <- .col(c((p-i), (p-i))) == .row(c((p-i), (p-i)))
    # covariate combinations for model estimate
    allModelsList <- adply(select_covaraite, 1, function(x) c(model_covariates, not_model_covariates[x]))
    allModelsList <- allModelsList[, 2:(i+2)] 
    # Implement cross validation
    fold <- round(nrow(df) / K)
    cross_val_err = matrix(data = NA, nrow = length(not_model_covariates), ncol = K)
    for(j in 1:K){
      # We need to exclude 4 variables with low variety becuase the tiels of KNN are limited to 1000
      #df <- df %>% #<<<<<<
      #  dplyr::select(-c("count", "rating_closest_neighbour", "White.per.County", "OtherTransp.per.County")) #<<<<<<
      train_data <- df[-c((1+(j-1)*fold):(j*fold)),]
      testing_data <- df[(1+(j-1)*fold):(j*fold),]
      # Class vectors
      Y_train <- train_data[, Y] #<<<<<<
      Y_test <- testing_data[, Y] #<<<<<<
      # Change format that it works with knn function
      Y_train <- factor(as.matrix(Y_train)) #<<<<<<
      # predict models
      model_pred <- lapply(allModelsList, function(x) 
        FUN(train = train_data[, as.matrix(x)], test = testing_data[, as.matrix(x)], cl = Y_train, k = k)) #<<<<<<
      # Each column = Prediction results for one variable used
      model_pred <- data.frame(matrix(unlist(model_pred), ncol=length(model_pred), byrow=F))
      colnames(model_pred) <- not_model_covariates
      # Prediction Error (Rate of Wrong Predictions)
      pred_error <- apply(model_pred, 2, function(x) x != as.matrix(Y_test)) #<<<<<<
      pred_error <- as.data.frame(
        apply(pred_error, 2, function(x) sum(x, na.rm = TRUE)/nrow(testing_data)))
      cross_val_err[,j] <- pred_error[, 1]
    }
    # Average of CV prediction error
    cross_val_err <- as.tibble(apply(cross_val_err, 1, mean, na.rm=TRUE))
    # Best prediction (takes the first one if equal performance)
    best_covariate <- not_model_covariates[which(cross_val_err == min(cross_val_err))[1]]
    model_covariates <- c(model_covariates, best_covariate)
    # Error of best prediction
    model_errors[(i+1), 2] <- min(cross_val_err)
    # best model
    model_errors[(i+1), 1] <- allModelsList[which(cross_val_err == min(cross_val_err))[1]]
  }
  return(model_errors)
}
#########################################################################################

#LDA Model Selection----#################################################################

# implements forward-stepwise-selection and K-Fold CV
# as suggested by James et al (pp. 207 - 208)
# takes a df, the name of Y as character, the model used as well as the number of folds
forward_stepwise_selection <- function(df, Y, FUN, K = 10){
  # All covariate names
  covariates <- colnames(df)
  covariates <- covariates[which(covariates != Y)]
  # number of covaraies
  p <- length(covariates)
  # df to store errors
  model_errors <- data.frame(Model = character(),
                               Error = double(),
                               stringsAsFactors=FALSE)
  # vector for used covariates
  model_covariates <- c()
  # model estimate for each number of covariates
  for(i in 0:(p-1)){
    # Not yet used covariates
    not_model_covariates <- covariates[which(!(covariates %in% model_covariates))]
    # identity matrix to select each variable once
    select_covaraite <- .col(c((p-i), (p-i))) == .row(c((p-i), (p-i)))
    # formula for model estimate
    allModelsList <- apply(select_covaraite, 1, function(x)
      paste(c(model_covariates, not_model_covariates[x]),
            collapse= " + "))
    allFormulasList <- lapply(allModelsList, function(x)
      as.formula(paste(c(Y, x), collapse = " ~ ")))
    # Implement cross validation
    fold <- round(nrow(df) / K)
    cross_val_err = matrix(data = NA, nrow = length(not_model_covariates), ncol = K)
    for(j in 1:K){
      train_data <- df[-c((1+(j-1)*fold):(j*fold)),]
      testing_data <- df[(1+(j-1)*fold):(j*fold),]
      # Fit models
      model_fit <- lapply(allFormulasList, function(x) FUN(x, data=train_data))
      # Predict
      model_pred <-  transpose(ldply(model_fit, function(x) predict(x, newdata=testing_data)$class))
      # Each column = Prediction results for one variable used
      model_pred <- data.frame(matrix(unlist(model_pred), ncol=length(model_pred), byrow=F))
      colnames(model_pred) <- not_model_covariates
      # Prediction Error (Rate of Wrong Predictions)
      pred_error <- apply(model_pred, 2, function(x) x != as.numeric(testing_data[, Y]))
      pred_error <- as.data.frame(
        apply(pred_error, 2, function(x) sum(x, na.rm = TRUE)/nrow(testing_data)))
      cross_val_err[,j] <- pred_error[, 1]
    }
    # Average of CV prediction error
    cross_val_err <- as.tibble(apply(cross_val_err, 1, mean, na.rm=TRUE))
    # Best prediction (takes the first one if equal performance)
    best_covariate <- not_model_covariates[which(cross_val_err == min(cross_val_err))[1]]
    model_covariates <- c(model_covariates, best_covariate)
    # Error of best prediction
    model_errors[(i+1), 2] <- min(cross_val_err)
    # best model
    model_errors[(i+1), 1] <- allModelsList[which(cross_val_err == min(cross_val_err))[1]]
  }
  return(model_errors)
}

# Implement over- and underbagging with CV Errors
# Takes a df, the Y variable as character, the number of bagged models B as well as
# as well as the number of each class from the original df and the model used
over_under_bagging <- function(df, Y, B, sample_size, FUN){
  set.seed(123)
  # all classes of Y
  classes <- as.matrix(unique(df[Y]))
  classes <- sort(classes)
  for(i in 1:length(classes)){
    # Create a subset variable of each class
    nam <- paste("subset", classes[i], sep = "")
    assign(nam, df[which(as.matrix(df[Y])== classes[i]), ])
  }
  Bag_err = matrix(data = NA, nrow = ncol(df) - 1, ncol = B)
  for(i in 1:B){
    # draw a random subset of each class according to sample_size
    sampleA <- sample(1:nrow(subsetA), sample_size[1], replace = T)
    sampleB <- sample(1:nrow(subsetB), sample_size[2], replace = T)
    sampleC <- sample(1:nrow(subsetC), sample_size[3], replace = T)
    # bind the sample to baggin data
    bagging_data <- rbind(subsetA[sampleA, ], subsetB[sampleB, ], subsetC[sampleC, ])
    # randomly rearrange
    bagging_data <- bagging_data[sample(1:sum(sample_size), replace = FALSE),]
    # CV and forward_stepwise_selection
    err <- forward_stepwise_selection(bagging_data, Y, FUN)
    Bag_err[,i] <- as.matrix(err[, 2])
  }
  # Take average of bagged-CV-Erros
  Bag_err_final <- as.tibble(apply(Bag_err, 1, mean, na.rm=TRUE))
  Bag_err_final <- cbind(err[, 1], Bag_err_final)
  return(Bag_err_final)
}

# The functions take a while to run
# Already estimated errors can be loaded here
load("./Results/lda_under_error.RData")
load("./Results/lda_over_error.RData")

# implement LDA with under-bagging
lda_under_bagging_error <- over_under_bagging(ny_data,
                                              Y = "Inspection.Grade",
                                              B = 100,
                                              sample_size = c(700, 700, 700),
                                              FUN = lda)
save(lda_under_bagging_error, file = "./Results/lda_under_error.RData")

# implement LDA with over-bagging
lda_over_bagging_error <- over_under_bagging(ny_data,
                                             Y = "Inspection.Grade",
                                             B = 100,
                                             sample_size = c(5000, 5000, 5000),
                                             FUN = lda)
save(lda_over_bagging_error, file = "./Results/lda_over_error.RData")

# Determine the best model if under-bagging performs better
if (min(lda_under_bagging_error[, 2]) < min(lda_over_bagging_error[, 2])) {
  # get the number of the best model (to highlight in the plot)
  best_model_nr <- which(lda_under_bagging_error[, 2] == min(lda_under_bagging_error[, 2]))
  # get the formula of the best model
  best_model <- lda_under_bagging_error[which(lda_under_bagging_error[, 2] == min(lda_under_bagging_error[, 2])), 1]
  best_model <- as.character(best_model)
  best_model <- as.formula(paste("Inspection.Grade", best_model, sep = " ~ "))
  # Determine the best model if over-bagging performs better
} else {
  # get the number of the best model (to highlight in the plot)
  best_model_nr <- which(lda_over_bagging_error[, 2] == min(lda_over_bagging_error[, 2]))
  # get the formula of the best model
  best_model <- lda_over_bagging_error[which(lda_over_bagging_error[, 2] == min(lda_over_bagging_error[, 2])), 1]
  best_model <- as.character(best_model)
  best_model <- as.formula(paste("Inspection.Grade", best_model, sep = " ~ "))
}

# Implementation with imbalanced data
# (to show in Plot 4 why it is an issue)
lda_imbal_error <- forward_stepwise_selection(ny_data,
                                                  Y = "Inspection.Grade",
                                                  FUN = lda,
                                                  K = 10)

# Implementation without bagging
# (to show in Plot 4 why bagging is necessary)
# 2 balanced subsets
for(i in 1:2){
  # Create subsets of each class
  sampleA <- ny_data[which(ny_data$Inspection.Grade == "A") ,]
  sampleA <- sampleA[sample(1:nrow(sampleA), 700, replace = T), ]
  sampleB <- ny_data[which(ny_data$Inspection.Grade == "B") ,]
  sampleB <- sampleB[sample(1:nrow(sampleB), 700, replace = T), ]
  sampleC <- ny_data[which(ny_data$Inspection.Grade == "C") ,]
  sampleC <- sampleC[sample(1:nrow(sampleC), 700, replace = T), ]
  bagging_data <- rbind(sampleA, sampleB, sampleC)
  # randomly rearrange data
  bagging_data <- bagging_data[sample(1:2100, replace = FALSE),]
  # Get errors for each bagging_set
  nam = paste("lda_NO_bagging_error", i, sep = "_")
  assign(nam, forward_stepwise_selection(bagging_data,
                                                     Y = "Inspection.Grade",
                                                     FUN = lda,
                                                     K = 10))
}

# bind all errors to a tibble
lda_performance <- as.tibble(cbind(Nr_Covariates = 1:(ncol(ny_data) - 1),
                                   lda_under_bagging_error = lda_under_bagging_error[, 2],
                                   lda_over_bagging_error =lda_over_bagging_error[, 2],
                                   lda_NO_bagging_error_1 = lda_NO_bagging_error_1[, 2],
                                   lda_NO_bagging_error_2 = lda_NO_bagging_error_2[, 2],
                                   lda_imbal_error = lda_imbal_error[, 2]))

# reshap to long format (for ggplot2)
lda_performance <- lda_performance %>%
  gather(method, error, c(lda_under_bagging_error,
                          lda_over_bagging_error,
                          lda_NO_bagging_error_1,
                          lda_NO_bagging_error_2,
                          lda_imbal_error))

# error plot to illustrate the usefulness of over- / under-bagging
Plot6 <- ggplot(data = lda_performance) +
  geom_line(aes(x = Nr_Covariates, y = error, colour=method)) +
  geom_point(aes(x = Nr_Covariates, y = error, colour=method)) +
  geom_vline(xintercept = best_model_nr, linetype = "dotted", color = "Black", size = 0.5) +
  labs(title="Methodology illsutrated with LDA",
       x="Number of Covariates",
       y = "Error Rate",
       color = "Method") +
  scale_color_manual(labels = c("Imbalanced", "No Bagging 1", "No Bagging 2","Over-Bagging", "Under-Bagging"), 
                     values = c("green", "blue", "red", "brown", "orange")) +
  theme_gray() +
  theme(legend.position="right",
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

ggsave("./plots/Plot6_Method.png", plot = Plot6, width = 7, height = 4, dpi = 300)

rm(lda_NO_bagging_error_1, lda_NO_bagging_error_2, lda_imbal_error, sampleA, sampleB, sampleC, i, nam, Plot6, bagging_data)

# bind only over- and under-bagging errors to a tibble
lda_performance <- as.tibble(cbind(Nr_Covariates = 1:(ncol(ny_data) - 1),
                                   lda_under_bagging_error = lda_under_bagging_error[, 2],
                                   lda_over_bagging_error =lda_over_bagging_error[, 2]))

# reshap to long format (for ggplot2)
lda_performance <- lda_performance %>%
  gather(method, error, c(lda_under_bagging_error,
                          lda_over_bagging_error))

# error plot LDA
Plot7 <- ggplot(data = lda_performance) +
  geom_line(aes(x = Nr_Covariates, y = error, colour=method)) +
  geom_point(aes(x = Nr_Covariates, y = error, colour=method)) +
  geom_vline(xintercept = best_model_nr, linetype = "dotted", color = "Black", size = 0.5) +
  labs(title="Error Rate LDA",
       x="Number of Covariates",
       y = "Error Rate",
       color = "Method") +
  scale_color_manual(labels = c("Over-Bagging", "Under-Bagging"), 
                     values = c("green", "blue")) +
  theme_gray() +
  theme(legend.position="right",
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

ggsave("./plots/Plot7_LDA_Error.png", plot = Plot7, width = 7, height = 4, dpi = 300)

rm(lda_performance, Plot7)
#########################################################################################

# LDA Estimate Selected Model----########################################################

# Now we implement the best model we have selected before

# Initialize grid values to illustrate decision boundaries
# (Used for all models)
# we use "shop_density" and "White.per.CenTrac" because the data is well distributed
resolution = 200
r <- sapply(ny_data[c("shop_density", "White.per.CenTrac")], range, na.rm = TRUE)
xs <- seq(r[1,1], r[2,1], length.out = resolution)
ys <- seq(r[1,2], r[2,2], length.out = resolution)
g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
colnames(g) <- colnames(r)
g <- as.data.frame(g)

# Returns a bagging sample
bagging_sample <- function(df, Y, sample_size){
  # get all classes of the Y variable
  classes <- as.matrix(unique(df[Y]))
  classes <- sort(classes)
  # create a subset of each class
  for(i in 1:length(classes)){
    nam <- paste("subset", classes[i], sep = "")
    assign(nam, df[which(as.matrix(df[Y])== classes[i]), ])
  }
  # get samples of each class according to the sample size
  sampleA <- sample(1:nrow(subsetA), sample_size[1], replace = T)
  sampleB <- sample(1:nrow(subsetB), sample_size[2], replace = T)
  sampleC <- sample(1:nrow(subsetC), sample_size[3], replace = T)
  bagging_data <- rbind(subsetA[sampleA, ], subsetB[sampleB, ], subsetC[sampleC, ])
  # randomly rearrange
  bagging_data <- bagging_data[sample(1:sum(sample_size), replace = FALSE),]
  return(bagging_data)
}

# number of bagging repetitions
B = 100
bagged_models=list()
# empty matrix for bagging predictions
bagged_predictions=matrix(data = NA, nrow = nrow(ny_data), ncol = B)
# empty matrix for bagging used for the plot
bagged_predictions_plot=matrix(data = NA, nrow = nrow(g), ncol = B)
for (i in 1:B){
  # bagging sample
  sample <- bagging_sample(ny_data,
                 Y = "Inspection.Grade",
                 sample_size = c(5000, 5000, 5000)) # over-bagging performed better than under-bagging
  # fits the model with the bagging sample
  model_fit <- lda(best_model, data = sample)
  bagged_models <- c(bagged_models, list(model_fit))
  # predicts the values for the entire dataset
  model_pred <- predict(model_fit, newdata = ny_data)
  bagged_predictions[, i] <- model_pred$class
  # fit and predicts the values for the generated plot points
  model_fit <- lda(Inspection.Grade~shop_density + White.per.CenTrac, data = sample)
  model_pred <- predict(model_fit, newdata = g)
  bagged_predictions_plot[, i] <- model_pred$class
}

# Implements majority voting over the B bagging predictions
maj_vote <- function(x) {
  table = table(x)
  majority = which.max(table)
}

# Get Majority vote of the B bagged models
pred_lda <- apply(bagged_predictions, 1, maj_vote)
pred_lda <- factor(pred_lda, levels = c(1, 2, 3), labels = c("A", "B", "C"))
pred_lda_plot <- apply(bagged_predictions_plot, 1, maj_vote)
pred_lda_plot <- factor(pred_lda_plot, levels = c(1, 2, 3), labels = c("A", "B", "C"))

# prediction error
lda_final_error <- sum(pred_lda != ny_data$Inspection.Grade) / length(ny_data$Inspection.Grade)

# prediction matrix
lda_pred_matrix <- table(pred_lda, ny_data$Inspection.Grade, dnn = c("prediction", "observed"))

# bind grid data
grid_data <- cbind(pred_lda_plot, g)
grid_data <- as.data.frame(grid_data)

Plot8 <- ggplot(data = ny_data, aes(y = shop_density, x = White.per.CenTrac)) +
  geom_point(data = grid_data, aes(color=pred_lda_plot), alpha=0.3, size = 0.5) +
  geom_point(aes(color=Inspection.Grade), alpha=1)+
  #geom_contour(aes(y = ys, x = xs, z=zs), 
  #             breaks=c(0,.5))
  labs(title="Decision Boundaries LDA",
       x="Ethnicity White per Census Tract",
       y = "Shop Density in 1km Radius",
       color = "Method") +
  theme_gray() +
  theme(legend.position="right",
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

ggsave("./plots/Plot8_LDA_Boundaries.png", plot = Plot8, width = 7, height = 4, dpi = 300)


rm(bagged_models, 
   bagged_predictions_plot, 
   bagged_predictions, 
   model_fit, 
   model_pred, 
   Plot8, 
   r, B, xs, ys, i,
   sample, 
   grid_data,
   pred_lda,
   pred_lda_plot,
   resolution)

#########################################################################################

# QDA Model Selection----################################################################

# The functions take a while to run
# Already estimated errors can be loaded here
load("./Results/qda_under_error.RData")
load("./Results/qda_over_error.RData")

# implement QDA with under-bagging
qda_under_bagging_error <- over_under_bagging(ny_data,
                                              Y = "Inspection.Grade",
                                              B = 100,
                                              sample_size = c(700, 700, 700),
                                              FUN = qda)

# implement QDA with over-bagging
qda_over_bagging_error <- over_under_bagging(ny_data,
                                             Y = "Inspection.Grade",
                                             B = 100,
                                             sample_size = c(5000, 5000, 5000),
                                             FUN = qda)

save(qda_under_bagging_error, file = "./Results/qda_under_error.RData")
save(qda_over_bagging_error, file = "./Results/qda_over_error.RData")

# Determine the best model if under-bagging performs better
if (min(qda_under_bagging_error[, 2]) < min(qda_over_bagging_error[, 2])) {
  # get the number of the best model (to highlight in the plot)
  best_model_nr <- which(qda_under_bagging_error[, 2] == min(qda_under_bagging_error[, 2]))
  # get the formula of the best model
  best_model <- qda_under_bagging_error[which(qda_under_bagging_error[, 2] == min(qda_under_bagging_error[, 2])), 1]
  best_model <- as.character(best_model)
  best_model <- as.formula(paste("Inspection.Grade", best_model, sep = " ~ "))
  # Determine the best model if over-bagging performs better
} else {
  # get the number of the best model (to highlight in the plot)
  best_model_nr <- which(qda_over_bagging_error[, 2] == min(qda_over_bagging_error[, 2]))
  # get the formula of the best model
  best_model <- qda_over_bagging_error[which(qda_over_bagging_error[, 2] == min(qda_over_bagging_error[, 2])), 1]
  best_model <- as.character(best_model)
  best_model <- as.formula(paste("Inspection.Grade", best_model, sep = " ~ "))
}

# bind all errors to a tibble
qda_performance <- as.tibble(cbind(Nr_Covariates = 1:(ncol(ny_data) - 1),
                                   qda_under_bagging_error = qda_under_bagging_error[, 2],
                                   qda_over_bagging_error =qda_over_bagging_error[, 2]))

# reshap to long format (for ggplot2)
qda_performance <- qda_performance %>%
  gather(method, error, c(qda_under_bagging_error,
                          qda_over_bagging_error))

# error plot QDA
Plot9 <- ggplot(data = qda_performance) +
  geom_line(aes(x = Nr_Covariates, y = error, colour=method)) +
  geom_point(aes(x = Nr_Covariates, y = error, colour=method)) +
  geom_vline(xintercept = best_model_nr, linetype = "dotted", color = "Black", size = 0.5) +
  labs(title="Error Rate QDA",
       x="Number of Covariates",
       y = "Error Rate",
       color = "Method") +
  scale_color_manual(labels = c("Over-Bagging", "Under-Bagging"), 
                     values = c("green", "blue")) +
  theme_gray() +
  theme(legend.position="right",
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

ggsave("./plots/Plot9_QDA_Error.png", plot = Plot9, width = 7, height = 4, dpi = 300)

# bind all LDA and QDA errors to a tibble to combine them in one plot
lda_qda_performance <- as.tibble(cbind(Nr_Covariates = 1:(ncol(ny_data) - 1),
                                       qda_under_bagging_error = qda_under_bagging_error[, 2],
                                       qda_over_bagging_error =qda_over_bagging_error[, 2],
                                       lda_under_bagging_error = lda_under_bagging_error[, 2],
                                       lda_over_bagging_error =lda_over_bagging_error[, 2]))

# reshap to long format (for ggplot2)
lda_qda_performance <- lda_qda_performance %>%
  gather(method, error, c(qda_under_bagging_error,
                          qda_over_bagging_error,
                          lda_under_bagging_error,
                          lda_over_bagging_error))

# error plot QDA
Plot10 <- ggplot(data = lda_qda_performance) +
  geom_line(aes(x = Nr_Covariates, y = error, colour=method)) +
  geom_point(aes(x = Nr_Covariates, y = error, colour=method)) +
  geom_vline(xintercept = best_model_nr, linetype = "dotted", color = "Black", size = 0.5) +
  labs(title = "LDA vs QDA Error",
       x="Number of Covariates",
       y = "Error Rate",
       color = "Method") +
  scale_color_manual(labels = c("LDA Over-Bagging", "LDA Under-Bagging", "QDA Over-Bagging", "QDA Under-Bagging"), 
                     values = c("green", "blue", "orange", "red")) +
  theme_gray() +
  theme(legend.position="right",
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

ggsave("./plots/Plot10_LDA_QDA_Error.png", plot = Plot10, width = 7, height = 4, dpi = 300)

rm(lda_qda_performance, Plot10, Plot9, qda_performance)

#########################################################################################

# QDA Estimate Selected Model----########################################################

# number of bagging repetitions
B = 100
bagged_models=list()
# empty matrix for bagging predictions
bagged_predictions=matrix(data = NA, nrow = nrow(ny_data), ncol = B)
# empty matrix for bagging used for the plot
bagged_predictions_plot=matrix(data = NA, nrow = nrow(g), ncol = B)
for (i in 1:B){
  # bagging sample
  sample <- bagging_sample(ny_data,
                           Y = "Inspection.Grade",
                           sample_size = c(5000, 5000, 5000)) # over-bagging performed better than under-bagging
  # fits the model with the bagging sample
  model_fit <- qda(best_model, data = sample)
  bagged_models <- c(bagged_models, list(model_fit))
  # predicts the values for the entire dataset
  model_pred <- predict(model_fit, newdata = ny_data)
  bagged_predictions[, i] <- model_pred$class
  # fit and predicts the values for the generated plot points
  model_fit <- qda(Inspection.Grade~shop_density + White.per.CenTrac, data = sample)
  model_pred <- predict(model_fit, newdata = g)
  bagged_predictions_plot[, i] <- model_pred$class
}

# Get Majority vote of the B bagged models
pred_qda <- apply(bagged_predictions, 1, maj_vote)
pred_qda <- factor(pred_qda, levels = c(1, 2, 3), labels = c("A", "B", "C"))
pred_qda_plot <- apply(bagged_predictions_plot, 1, maj_vote)
pred_qda_plot <- factor(pred_qda_plot, levels = c(1, 2, 3), labels = c("A", "B", "C"))

# prediction error
qda_final_error <- sum(pred_qda != ny_data$Inspection.Grade) / length(ny_data$Inspection.Grade)

# prediction matrix
qda_pred_matrix <- table(pred_qda, ny_data$Inspection.Grade, dnn = c("prediction", "observed"))

# bind grid data
grid_data <- cbind(pred_qda_plot, g)
grid_data <- as.data.frame(grid_data)

Plot11 <- ggplot(data = ny_data, aes(y = shop_density, x = White.per.CenTrac)) +
  geom_point(data = grid_data, aes(color=pred_qda_plot), alpha=0.3, size = 0.5) +
  geom_point(aes(color=Inspection.Grade), alpha=1)+
  #geom_contour(aes(y = ys, x = xs, z=zs), 
  #             breaks=c(0,.5))
  labs(title="Decision Boundaries QDA",
       x="Ethnicity White per Census Tract",
       y = "Shop Density in 1km Radius",
       color = "Method") +
  theme_gray() +
  theme(legend.position="right",
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

ggsave("./plots/Plot11_QDA_Boundaries.png", plot = Plot11, width = 7, height = 4, dpi = 300)

rm(model_fit, 
   model_pred, 
   bagged_models, 
   bagged_predictions, 
   bagged_predictions_plot, 
   sample, 
   best_model, 
   best_model_nr,
   i,
   pred_qda,
   pred_qda_plot,
   Plot11,
   grid_data)

#########################################################################################

# KNN Model Selection----################################################################

# Slightly adjust the function to KNN
# ( changed or added lines are marked with #<<<<<< )
tuning_parameter_selection <- function(df, Y, FUN, K = 10, k){
  # All covariate names
  covariates <- colnames(df)
  covariates <- covariates[which(covariates != Y)]
  # df to store errors
  model_errors <- data.frame(Model = character(),
                             Error = double(),
                             stringsAsFactors=FALSE)
  # folds for CV
  fold <- round(nrow(df) / K) - 1
  cross_val_err = matrix(data = NA, nrow = k, ncol = K)
  for(j in 1:K){
    train_data <- df[-c((1+(j-1)*fold):(j*fold)),]
    testing_data <- df[(1+(j-1)*fold):(j*fold),]
    # Class vectors
    Y_train <- train_data[, Y] #<<<<<<
    Y_test <- testing_data[, Y] #<<<<<<
    # Change format that it works with knn function
    Y_train <- factor(as.matrix(Y_train)) #<<<<<<
    train_data <- train_data[, covariates] #<<<<<<
    testing_data <- testing_data[, covariates] #<<<<<<
    # all the different k to try
    k_vector <- 1:k #<<<<<<
    # predict with all differnt k
    model_pred <- lapply(k_vector, function(x) 
      FUN(train = train_data, test = testing_data, cl = Y_train, k = x)) #<<<<<<
    # Each column = Prediction results for one variable used
    model_pred <- data.frame(matrix(unlist(model_pred), ncol=length(model_pred), byrow=F))
    colnames(model_pred) <- 1:k #<<<<<<
    # Prediction Error (Rate of Wrong Predictions)
    pred_error <- apply(model_pred, 2, function(x) x != as.matrix(Y_test)) #<<<<<<
    pred_error <- as.data.frame(
      apply(pred_error, 2, function(x) sum(x, na.rm = TRUE)/nrow(testing_data)))
    cross_val_err[,j] <- pred_error[, 1]
  }
  # Average of CV prediction error
  cross_val_err <- as.tibble(apply(cross_val_err, 1, mean, na.rm=TRUE))
return(cross_val_err)
}

over_under_bagging <- function(df, Y, B, FUN, sample_size, k){
  set.seed(123)
  classes <- as.matrix(unique(df[Y]))
  classes <- sort(classes)
  for(i in 1:length(classes)){
    nam <- paste("subset", classes[i], sep = "")
    assign(nam, df[which(as.matrix(df[Y])== classes[i]), ])
  }
  CV_err = matrix(data = NA, nrow = k, ncol = B)
  for(i in 1:B){
    sampleA <- sample(1:nrow(subsetA), sample_size[1], replace = T)
    sampleB <- sample(1:nrow(subsetB), sample_size[2], replace = T)
    sampleC <- sample(1:nrow(subsetC), sample_size[3], replace = T)
    bagging_data <- rbind(subsetA[sampleA, ], subsetB[sampleB, ], subsetC[sampleC, ])
    err <- tuning_parameter_selection(df, Y, FUN, K = 10, k)
    CV_err[,i] <- as.matrix(err[, 1])
  }
  CV_err_final <- as.tibble(apply(CV_err, 1, mean, na.rm=TRUE))
  CV_err_final <- cbind(k = 1:k, Error = CV_err_final)
  return(CV_err_final)
}

# The functions take a while to run
# Already estimated errors can be loaded here
load("./Results/knn_under_error.RData")
load("./Results/knn_over_error.RData")

knn_under_bagging_error <- over_under_bagging(ny_data,
                                              Y = "Inspection.Grade",
                                              knn,
                                              B = 100,
                                              k = 50,
                                              sample_size = c(700, 700, 700))

knn_over_bagging_error <- over_under_bagging(ny_data,
                                              Y = "Inspection.Grade",
                                              knn,
                                              B = 100,
                                              k = 50,
                                              sample_size = c(5000, 5000, 5000))

save(knn_under_bagging_error, file = "./Results/knn_under_error.RData")
save(knn_over_bagging_error, file = "./Results/knn_over_error.RData")

# Determine the best k if under-bagging performs better
if (min(knn_under_bagging_error[, 2]) < min(knn_over_bagging_error[, 2])) {
  # get the number of the best model (to highlight in the plot)
  best_model_k <- which(knn_under_bagging_error[, 2] == min(knn_under_bagging_error[, 2]))
  # Determine the best k if over-bagging performs better
} else {
  # get the number of the best model (to highlight in the plot)
  best_model_k <- which(knn_over_bagging_error[, 2] == min(knn_over_bagging_error[, 2]))
}

# bind all errors to a tibble
knn_performance <- as.tibble(cbind(k = 1:50,
                                   knn_under_bagging_error = knn_under_bagging_error[, 2],
                                   knn_over_bagging_error =knn_over_bagging_error[, 2]))

# reshap to long format (for ggplot2)
knn_performance <- knn_performance %>%
  gather(method, error, c(knn_under_bagging_error,
                          knn_over_bagging_error))

# error plot KNN
Plot12 <- ggplot(data = knn_performance) +
  geom_line(aes(x = k, y = error, colour=method)) +
  geom_point(aes(x = k, y = error, colour=method)) +
  geom_vline(xintercept = best_model_k, linetype = "dotted", color = "Black", size = 0.5) +
  labs(title="Error Rate KNN",
       x="Number of Covariates",
       y = "Error Rate",
       color = "Method") +
  scale_color_manual(labels = c("Over-Bagging", "Under-Bagging"), 
                     values = c("green", "blue")) +
  theme_gray() +
  theme(legend.position="right",
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

ggsave("./plots/Plot12_KNN_Error.png", plot = Plot12, width = 7, height = 4, dpi = 300)

rm(knn_performance, Plot12)

#########################################################################################

# KNN Estimate Selected Model----########################################################

# number of bagging repetitions
B = 100
bagged_models=list()
# empty matrix for bagging predictions
bagged_predictions=matrix(data = NA, nrow = nrow(ny_data), ncol = B)
# empty matrix for bagging used for the plot
bagged_predictions_plot=matrix(data = NA, nrow = nrow(g), ncol = B)
for (i in 1:B){
  # bagging sample
  sample <- bagging_sample(ny_data,
                           Y = "Inspection.Grade",
                           sample_size = c(700, 700, 700)) # Over- and under-baggin work equally well
  covariates <- colnames(sample)
  covariates <- covariates[which(covariates != "Inspection.Grade")]
  # Class vectors
  Y_train <- sample[, "Inspection.Grade"]
  # Change format that it works with knn function
  Y_train <- factor(as.matrix(Y_train))
  train_data <- sample[, covariates]
  testing_data <- ny_data[, covariates]
  # fits the model with the bagging sample
  model_pred <- knn(train = train_data, test = testing_data, cl = Y_train, k = best_model_k)
  bagged_predictions[, i] <- model_pred
  # fit and predicts the values for the generated plot points
  model_pred <- knn(train = train_data[, c("shop_density", "White.per.CenTrac")], 
                    test = g, cl = Y_train, k = best_model_k)
  bagged_predictions_plot[, i] <- model_pred
}

# Get Majority vote of the B bagged models
pred_knn <- apply(bagged_predictions, 1, maj_vote)
pred_knn <- factor(pred_knn, levels = c(1, 2, 3), labels = c("A", "B", "C"))
pred_knn_plot <- apply(bagged_predictions_plot, 1, maj_vote)
pred_knn_plot <- factor(pred_knn_plot, levels = c(1, 2, 3), labels = c("A", "B", "C"))

# prediction error
knn_final_error <- sum(pred_knn != ny_data$Inspection.Grade) / length(ny_data$Inspection.Grade)

# prediction matrix
knn_pred_matrix <- table(pred_knn, ny_data$Inspection.Grade, dnn = c("prediction", "observed"))

# bind grid data
grid_data <- cbind(pred_knn_plot, g)
grid_data <- as.data.frame(grid_data)

Plot13 <- ggplot(data = ny_data, aes(y = shop_density, x = White.per.CenTrac)) +
  geom_point(data = grid_data, aes(color=pred_knn_plot), alpha=0.3, size = 0.5) +
  geom_point(aes(color=Inspection.Grade), alpha=1)+
  labs(title="Decision Boundaries KNN",
       x="Ethnicity White per Census Tract",
       y = "Shop Density in 1km Radius",
       color = "Method") +
  theme_gray() +
  theme(legend.position="right",
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

ggsave("./plots/Plot13_KNN_Boundaries.png", plot = Plot13, width = 7, height = 4, dpi = 300)

rm(g, 
   grid_data, 
   bagged_models,
   bagged_predictions,
   bagged_predictions_plot,
   B,
   best_model_k,
   testing_data,
   train_data,
   covariates,
   i,
   model_pred,
   Y_train,
   pred_knn,
   pred_knn_plot,
   plot13,
   sample)

#########################################################################################

# Random Forest----######################################################################

#transform to factors for random forest analysis
ny_data$Inspection.Grade = as.factor(ny_data$Inspection.Grade)
ny_data$chain = as.factor(ny_data$chain)
ny_data$rating_closest_neighb = as.factor(ny_data$rating_closest_neighb)
ny_data$neighbourhood_group = as.factor(ny_data$neighbourhood_group)


###Model Selection---------

#dealing with imbalanced training data
bagging_sampling <- function(df, Y, sample_size) {
  #set.seed(123)
  classes <- as.matrix(unique(df[Y]))
  classes <- sort(classes)
  for(i in 1:length(classes)){
    nam <- paste("subset", classes[i], sep = "")
    assign(nam, df[which(as.matrix(df[Y])== classes[i]), ])
  }
  #set up the balanced samples
  sampleA <- sample(1:nrow(subsetA), sample_size[1], replace = T)
  sampleB <- sample(1:nrow(subsetB), sample_size[2], replace = T)
  sampleC <- sample(1:nrow(subsetC), sample_size[3], replace = T)
  bagging_data <- rbind(subsetA[sampleA, ], subsetB[sampleB, ], subsetC[sampleC, ])
  bagging_data <- bagging_data[sample(nrow(bagging_data)),]
  
  return(bagging_data)
}

#setting parameter for tuning
B = 10
sample_size = c(700, 700, 700)



#set tuning parameter
tuneGrid <- expand.grid(
  mtry       = seq(1, 20, by = 2),  
  min.node.size  = seq(2,9,2),
  splitrule = c("gini"))


#function to apply bagging to crossvalidated random forest
over_under_bagging <- function(df, Y, B, sample_size, tuneGrid) {
  
  #allocate matrix for results
  bagging_error = as_tibble(matrix(ncol = 7, nrow = (B+1)*nrow(tuneGrid)))
  
  for (i in 1:B) {
    
    rf_train = bagging_sampling(ny_data, "Inspection.Grade", sample_size) #set up sample
    
    #calculating models with CV
    ctrl <- trainControl(method = "cv", savePredictions = "final", allowParallel = TRUE) #use parallelization to enhance computation speed
    model_fit <- train(Inspection.Grade~., 
                       data = rf_train, 
                       method = "ranger", #use of random forest
                       trControl = ctrl, #set up the settings for CV
                       tuneGrid = tuneGrid, #calculate the best model using the tuning parameters
                       importance = "impurity") #to analyze the variable importance
    
    index <- seq(1, nrow(bagging_error), by = nrow(tuneGrid)) #save the results in the matrix
    bagging_error[index[i]:(index[i+1]-1),] = model_fit$results[,]
    
    print(i)
  }
  return(bagging_error)
}

#Function Data prep for CV_errors
mean_CV_error = function(bagging_error) {
  
  #calculate the mean for the different tuning combinations to evaluate
  mean_CV_error = bagging_error
  mean_CV_error = mean_CV_error %>% 
    rename(mtry=V1, nodeSize = V2, splitrule = V3, Accuracy = V4, Kappa = V5) %>% 
    mutate(splitrule = "gini") %>% 
    select(-V6, -V7) %>% 
    group_by(mtry, nodeSize, splitrule) %>% 
    summarise(mean = mean(Accuracy, na.rm = TRUE)) %>% 
    arrange(mean)
  
  mean_CV_error = mean_CV_error[1:nrow(mean_CV_error)-1,] #data prep
  
  return(mean_CV_error)
}

#Model without bagging for comparison

for (i in 1:B) {
  
  ctrl <- trainControl(method = "cv", savePredictions = "final", allowParallel = TRUE) #use parallelization to enhance computation speed
  without_model <- train(Inspection.Grade~., 
                         data = ny_data, 
                         method = "ranger", #use of random forest
                         trControl = ctrl, #set up the settings for CV
                         tuneGrid = tuneGrid, #calculate the best model using the tuning parameters
                         importance = "impurity") #to analyze the variable importance
  
  bagging_error[index[i]:(index[i+1]-1),] = without_model$results[,]
  
}
#call the above defined function to calculate the cross validated bagging errors
under_bagging_error <- over_under_bagging(ny_data, "Inspection.Grade", B = 10, tuneGrid = tuneGrid, sample_size = c(700,700,700))
over_bagging_error <- over_under_bagging(ny_data, "Inspection.Grade", B = 10, tuneGrid = tuneGrid, sample_size = c(5000,5000,5000))

#calculating the mean errors over all estimations for under and over-sampling
mean_CV_error_under <- mean_CV_error(under_bagging_error)
mean_CV_error_over <- mean_CV_error(over_bagging_error)
mean_CV_error_without <- mean_CV_error(without_bagging_error)


#Graphs
ggplot(varImp(model_fit))
model_fit$times
ggplot(model_fit)
plot(model_fit)


rm(tuneGrid, ctrl, rf_train, index, i, sample_size)

#take the highest mean of accuracy to get the optimal parameter combination
parameter_optimal_rf_under = mean_CV_error_under[which.max(mean_CV_error_under$mean),]
parameter_optimal_rf_over = mean_CV_error_over[which.max(mean_CV_error_over$mean),]


#train the optimal model, evaluated from above with undersampling

pred_matrix_under = matrix(nrow=nrow(ny_data), ncol=B)

for (i in 1:B) {
  
  #creating undersampling dataset
  rf_optimal_training <- bagging_sampling(df = ny_data, Y = "Inspection.Grade", sample_size = c(700,700,700))
  
  #random forest analysis with new evaluated parameter
  optimal_model_rf_under = ranger(Inspection.Grade~.,
                                  mtry = as.numeric(parameter_optimal_rf_under[1]),
                                  splitrule = as.character(parameter_optimal_rf_under[3]),
                                  min.node.size = as.numeric(parameter_optimal_rf_under[2]),
                                  data = rf_optimal_training,
                                  importance = "impurity",
                                  probability = FALSE)
  
  pred_cv = optimal_model_rf_under$predictions
  pred = predict(optimal_model_rf_under, data = ny_data)
  pred_matrix_under[,i] = pred$predictions #saving the predictions of the new trained model
  print(i)
}


#train the optimal model, evaluated from above from overbagging errors

pred_matrix_over = matrix(nrow=nrow(ny_data), ncol=B)

for (i in 1:B) {
  
  #creating undersampling dataset
  rf_optimal_training <- bagging_sampling(df = ny_data, Y = "Inspection.Grade", sample_size = c(5000,5000,5000))
  
  #random forest analysis with new evaluated parameter
  optimal_model_rf_over = ranger(Inspection.Grade~.,
                                 mtry = as.numeric(parameter_optimal_rf_over[1]),
                                 splitrule = as.character(parameter_optimal_rf_over[3]),
                                 min.node.size = as.numeric(parameter_optimal_rf_over[2]),
                                 data = rf_optimal_training,
                                 importance = "impurity",
                                 probability = FALSE)
  
  pred_cv = optimal_model_rf_over$predictions
  pred = predict(optimal_model_rf_over, data = ny_data)#saving the predictions of the new trained model
  pred_matrix_over[,i] = pred$predictions
  print(i)
}

#Majority vote for the predictions

#function chooses the most frequent prediction per row
chooseBestModel <- function(x) {
  tabulatedOutcomes <- table(x) 
  sortedOutcomes <- sort(tabulatedOutcomes, decreasing=TRUE)
  mostCommonLabel <- names(sortedOutcomes)[1]
  mostCommonLabel
}

#apply majority vote to get the final predictions
pred_matrix_majority_under = as.numeric(apply(pred_matrix_under, 1, chooseBestModel))
pred_matrix_majority_over = as.numeric(apply(pred_matrix_over, 1, chooseBestModel))

#data prep to have one table with all the different prediction as well as the true Y
resultate = tibble(ny_data$Inspection.Grade, pred_matrix_majority_over, pred_matrix_majority_under)
resultate <- resultate %>%
  mutate(pred_matrix_majority_over = factor(pred_matrix_majority_over, levels = c(1, 2, 3), labels = c("A", "B", "C"))) %>%
  mutate(pred_matrix_majority_under = factor(pred_matrix_majority_under, levels = c(1, 2, 3), labels = c("A", "B", "C")))

#conversion matrix for documentation as well as summary of results
table_over = table(resultate$`ny_data$Inspection.Grade`, resultate$pred_matrix_majority_over)
table_under = table(resultate$`ny_data$Inspection.Grade`, resultate$pred_matrix_majority_under)


#creating matrix with all bagging errors
rf_error <- matrix(nrow= nrow(mean_CV_error_under), ncol = 3)
rf_error[,1] <- mean_CV_error_over$mean
rf_error[,2] <- mean_CV_error_under$mean
rf_error[,3] <- 1:nrow(mean_CV_error_over)


#Graphs and analytics of optimal models------

#variable importance
variableImportance_plot_under = vip(
  object = optimal_model_rf_under,
  feature_names = colnames(ny_data),
  train = rf_optimal_training,
  scale = TRUE #calculates importance relative to eachother
)

variableImportance_plot_over = vip(
  object = optimal_model_rf_over,
  feature_names = colnames(ny_data),
  train = rf_optimal_training, 
  scale = TRUE
)

#ICE plot for documentation
ice_subway_over <- optimal_model_rf_over  %>%
  partial(pred.var = "subway_distance", grid.resolution = 30, train = rf_optimal_training, ice = TRUE) %>%
  autoplot(rug = TRUE, train = rf_optimal_training, alpha = .1, center = TRUE, paropts = list(.packages = "ranger")) +
  ggtitle("over-sampling")

ice_subway_under <- optimal_model_rf_under  %>%
  partial(pred.var = "subway_distance", grid.resolution = 30, train = rf_optimal_training, ice = TRUE) %>%
  autoplot(rug = TRUE, train = rf_optimal_training, alpha = .1, center = TRUE, paropts = list(.packages = "ranger")) +
  ggtitle("under-sampling")

gridExtra::grid.arrange(ice_subway_under, ice_subway_over, ncol = 2) #adds the two plots together in one row

#########################################################################################

# Boosting-----##########################################################################

#tuning parameter
hyper_grid <- expand.grid(
  eta = c(.01, .05, .1, .3),
  max_depth = c(1, 3, 5, 7),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(.65, .8, 1), 
  colsample_bytree = c(.8, .9, 1)
)

#set up function for applying the bagging to xgb
over_under_bagging_boosting <- function(df, Y, B, sample_size, tuneGrid) {
  
  #allocate matrix for results
  bagging_error = as_tibble(matrix(ncol = 7, nrow = (B+1)*nrow(tuneGrid)))
  
  for (i in 1:B) {
    
    rf_train = bagging_sampling(ny_data, "Inspection.Grade", sample_size) #set up sample
    
    #calculating models with CV
    ctrl <- trainControl(method = "cv", savePredictions = "final", allowParallel = TRUE) #use parallelization to enhance computation speed
    model_fit <- train(Inspection.Grade~., 
                       data = rf_train, 
                       method = "xbgTree", #use of xtreme boosting gradient
                       trControl = ctrl, #set up the settings for CV
                       tuneGrid = tuneGrid, #calculate the best model using the tuning parameters
                       importance = "impurity") #to analyze the variable importance
    
    index <- seq(1, nrow(bagging_error), by = nrow(tuneGrid)) #save the results in the matrix
    bagging_error[index[i]:(index[i+1]-1),] = model_fit$results[,]
    
    print(i)
  }
  return(bagging_error)
}

#calculate the responding under and over-bagging errors for xtreme gradient boosting
unders_boosting_error <- over_under_bagging_boosting(ny_data, B = 10, Y = "Inspection.Grade", sample_size = c(700,700,700))
over_boosting_error <- over_under_bagging_boosting(ny_data, B = 10, Y = "Inspection.Grade", sample_size = c(5000,5000,5000))

mean_CV_error_under_boosting <- mean_CV_error(under_boosting_error)
mean_CV_error_over_boosting <- mean_CV_error(over_boosting_error)

#the evaluation of the parameters of the optimal model
parameter_optimal_boosting_under = mean_CV_error_under_boosting[which.max(mean_CV_error_under_boosting$mean),]
parameter_optimal_boosting_over = mean_CV_error_over_boosting[which.max(mean_CV_error_over_boosting$mean),]

#train the optimal model, evaluated from above

B = 100
sample_size = c(700,700,700)
pred_matrix_boosting = matrix(nrow=nrow(ny_data), ncol=B)

#optimal evaluated tuning parameter for xgboosting; value of the tuning parameter are arbitrarely due to time restrictions in calculation the 
#hyperparameter tuning 
params <- list(
  eta = 0.1,
  max_depth = 5,
  min_child_weight = 4,
  subsample = .75,
  colsample_bytree = 1
)

#use B = 100 iterations to get a stable perdiction and apply the majority vote for the final prediction
for (i in 1:B) {
  
  boosting_optimal_training <- bagging_sampling(df = ny_data, Y = "Inspection.Grade", sample_size = c(700,700,700))
  
  #calculating final model with evaluated parameters
  optimal_model_boosting  <- xgboost(
    params = params,
    data = boosting_optimal_training,
    label = "Inspection.Grade",
    nrounds = 1430,
    objective = "multi:softprob",
    verbose = 1
  )
  #prediction with new trained optimal boosting model
  pred = optimal_model_boosting$predictions
  pred = predict(optimal_model_boosting, data = ny_data)
  pred_matrix_boosting[,i] = optimal_model_boosting$predictions
}

#importance of the variables used in the boosting model
importance_matrix <- xgb.importance(model = optimal_model_boosting)
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")

#partial dependence plot to show marginal effect of one or two features on outcome
pdp_plot <- optimal_model_boosting %>%
  partial(pred.var = "shop_density", n.trees = 1430, grid.resolution = 100, train = rf_optimal_training,paropts = list(.packages = "xgboost")) %>%
  autoplot(rug = TRUE, train = rf_optimal_training) +
  ggtitle("PDP")

pdp_plot


ice_plot <- optimal_model_boosting %>%
  partial(pred.var = "shop_density", n.trees = 1430, grid.resolution = 100, train = boosting_optimal_training, ice = TRUE) %>%
  autoplot(rug = TRUE, train = boosting_optimal_training, alpha = .1, center = TRUE, paropts = list(.packages = "xgboost")) +
  ggtitle("ICE")

ice_plot

rm(
  bagging_error,
  ctrl,
  ice_subway_over,
  ice_subway_under,
  mean_CV_error_over,
  mean_CV_error_under,
  optimal_model_rf_over,
  optimal_model_rf_under,
  over_bagging_error,
  parameter_optimal_rf_over,
  parameter_optimal_rf_under,
  pred,
  pred_matrix_boosting,
  pred_matrix_majority_over,
  pred_matrix_majority_under,
  resultate,
  rf_error,
  rf_optimal_training,
  variableImportance_plot_over,
  variableImportance_plot_under)

#########################################################################################