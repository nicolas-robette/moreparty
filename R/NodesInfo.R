#' @importFrom partykit nodeapply as.simpleparty nodeids info_node

#' @export

NodesInfo <- function(ct) {
  
  response <- ct$fitted[, "(response)"]
  
  rule <- list.rules.party(ct)
  id <- names(rule)
  rule <- as.character(rule)
  rule <- gsub("%in%", "in", rule, fixed = TRUE)
  rule <- gsub("c(", "(", rule, fixed = TRUE)
  rule <- gsub("\"", "", rule, fixed = TRUE)
  rule <- gsub("NA, ", "", rule)
  rule <- gsub(", NA", "", rule)
  rule <- sapply(rule, simplify_rule)
  rule <- data.frame(id = factor(id), rule)
  rownames(rule) <- NULL
  info <- partykit::nodeapply(partykit::as.simpleparty(ct), ids = partykit::nodeids(ct, terminal = TRUE), partykit::info_node)
  rule$freq <- sapply(info, function(x) x$n)
  
  if (is.numeric(response)) {
    rule$prediction <- sapply(info, function(x) x$prediction)
  
  } else if (is.factor(response)) {
    prob <- t(sapply(info, function(x) x$distribution))
    prob <- apply(prob, 2, function(x) x/rule$freq)
    prob <- as.data.frame(prob)
    names(prob) <- paste("prob", levels(response), sep = ".")
    rule <- data.frame(rule, prob)
  }
  
  return(rule)
}
