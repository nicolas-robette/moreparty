#' @export

GetSplitStats <- function(ct) {
  ternodes <- unique(where(ct))
  allnodes <- 1:max(ternodes)
  splnodes <- setdiff(allnodes,ternodes)
  stats <- lapply(splnodes, function(x) round(rev(sort(log(nodes(ct,x)[[1]]$criterion$criterion))),10))
  names(stats) <- splnodes
  return(stats)
}