require(data.table)
require(MASS)
require(caret)
require(glmnet)
require(parallel)

setwd("~/Code/ffc-R/code")

ncores = 6

load("../data/ffc-impute.RData")

td <- fread("../data/train.csv")
setkey(td, "challengeID")

outcomes <- names(td)[2:NCOL(td)]

prdct <- function(i, d, o) {
  # get outcomes
  outcome <- o[, c(1, i+1), with=FALSE]
  setnames(outcome, c("challengeID", "outcome"))
  # remove NAs
  rs <- rowSums(outcome)
  outcome <- outcome[is.finite(rs)]
  
  # determine if outcome is binary
  isNumeric <- (length(unique(outcome$outcome)) > 2)
  if(!isNumeric) {
    outcome$outcome <- as.factor(outcome$outcome)
  }
  
  # get features
  data <- d
  data$challengeID <- ids
  data <- subset(data, challengeID %in% outcome$challengeID)
  
  # drop IDs, we don't need them for the model
  outcome[, (c("challengeID")):=NULL]
  data[, (c("challengeID")):=NULL]
  
  # create model matrix
  X <- model.matrix(~., data=data)[, -1]
  
  # find optimal hyperparameters
  paramgrid <- expand.grid(.alpha = 1:99 * 0.01, .lambda = 1:99 * 0.01)
  control <- trainControl(method = "repeatedcv", repeats = 20)
  
  if(isNumeric) {
    model <- train(x=X, y = outcome$outcome, method = "glmnet", tuneGrid = paramgrid, trControl = control)
  } else {
    model <- train(x=X, y = outcome$outcome, method = "glmnet", tuneGrid = paramgrid, trControl = control, family = "binomial")
  }
  
  #predict outcomes
  allData <- model.matrix(~., data=d)[, -1]
  if(isNumeric) {
    predict(model, allData, type = "raw")
  } else {
    predict(model, allData, type = "prob")[, 2]
  }
}

cl <- makeForkCluster(ncores)
predictions <- parSapply(cl, seq(1,6), function(i) {
  prdct(i, d, td)
})
stopCluster(cl)
predictions <- as.data.table(predictions)
setnames(predictions, c("gpa", "grit", "materialhardship", "eviction", "layoff", "jobTraining"))


predictions.dt <- data.table(challengeID=ids,
                             gpa=predictions$gpa,
                             grit=predictions$grit,
                             materialHardship=predictions$materialhardship,
                             eviction=predictions$eviction,
                             layoff=predictions$layoff,
                             jobTraining=predictions$jobTraining)
write.csv(predictions.dt, file="../prediction.csv", row.names = F)
