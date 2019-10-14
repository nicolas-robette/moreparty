# @import party
#
#' @importFrom stats predict
#'
#' @export


FeatureSelection <- function(Y, X, recompute = F, ntree = 3000, measure = NULL) {

  if(class(Y)=='factor') mtry <- ceiling(sqrt(ncol(X))) # automatically set mtry to sqrt(p)
  if(class(Y)=='numeric') mtry <- ceiling(ncol(X)/3) # automatically set mtry to p/3

  dat <- cbind(Y, X) # create the data
  names(dat) <- c("response", paste("V", 1:ncol(X), sep = ""))

  if(is.null(measure)) {
    vi_fct <- function(x,measure) { varImp::varImp(x) }
  } else if(measure=="AUC") {
    vi_fct <- function(x,measure) { -varImp::varImpAUC(x) }
  } else {
    vi_fct <- function(x,measure) { varImp::varImp(x, measure=measure) }
  }

  forest <- party::cforest(response ~ ., data = dat, # fit a forest
                          controls = party::cforest_unbiased(mtry = mtry, ntree = ntree))
  selections <- list() # a list that contains the sequence of selected variables
  selections[[ncol(X)]] <- names(sort(vi_fct(forest, measure), decreasing = T))
  errors <- c()

  for (i in ncol(X):1) { # take backward rejection steps
    print(paste('steps to go =',i))
    if(class(Y)=='factor') mtry <- ceiling(sqrt(i)) # set mtry to sqrt() of remaining variables
    if(class(Y)=='numeric') mtry <- ceiling(i/3) # set mtry to ()/3 of remaining variables
    forest <- party::cforest(as.formula(paste("response", paste(selections[[i]],
                                                         collapse = " + "), sep = " ~ ")), data = dat, # fit forest
                             controls = party::cforest_unbiased(mtry = mtry, ntree = ntree))
    errors[i] <- mean((as.numeric(Y) - # compute the OOB-error
                         as.numeric(predict(forest, OOB = T)))^2)
    # define the next set of variables
    if (recompute == F & i > 1) selections[[i - 1]] <- selections[[i]][-i]
    if (recompute == T & i > 1) selections[[i - 1]] <- names(sort(vi_fct(forest, measure), decreasing = T))[-i]
  }

  # compute the error expected when no predictor is used at all
  errors <- c(mean((as.numeric(Y) - ifelse(length(unique(Y)==2),round(mean(as.numeric(Y))), mean(Y)) )^2 ),
              errors)

  # define the number of variables determined by the 0 s.e. and 1 s.e. rule
  optimum.number.0se <- which.min(errors)
  optimum.number.1se <- which(errors <= min(errors) + 1 * ifelse(all(Y %in% 0:1),
                                                                 sqrt(min(errors) * (1 - min(errors)) / nrow(X)), 0))[1]
  # compute the corresponding forests and OOB-errors
  if (optimum.number.0se == 1) {forest.0se <- c(); selection.0se <- c()}
  if (optimum.number.1se == 1) {forest.1se <- c(); selection.1se <- c()}
  if (optimum.number.0se != 1) {
    selection.0se <- selections[[optimum.number.0se - 1]]
    forest.0se <- party::cforest(as.formula(paste("response", paste(selection.0se,
                                                             collapse = " + "), sep = " ~ ")), data = dat,
                                 controls = party::cforest_unbiased(mtry = mtry, ntree = ntree))}
  if (optimum.number.1se != 1) {
    selection.1se <- selections[[optimum.number.1se - 1]]
    forest.1se <- party::cforest(as.formula(paste("response", paste(selection.1se,
                                                             collapse = " + "), sep = " ~ ")), data = dat,
                                 controls = party::cforest_unbiased(mtry = mtry, ntree = ntree))}
  oob.error.0se <- errors[optimum.number.0se]
  oob.error.1se <- errors[optimum.number.1se]
  selection.0se <- names(X)[as.numeric(gsub('V','',selection.0se))]
  selection.1se <- names(X)[as.numeric(gsub('V','',selection.1se))]
  return(list("selection.0se" = selection.0se, "forest.0se" = forest.0se,
              "oob.error.0se" = oob.error.0se, "selection.1se" = selection.1se,
              "forest.1se" = forest.1se, "oob.error.1se" = oob.error.1se))
}
