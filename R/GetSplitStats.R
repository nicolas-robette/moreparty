#' @export

GetSplitStats <- function(ct) {
  if(class(ct)[1]=='BinaryTree') {
    ternodes <- unique(where(ct))
    allnodes <- 1:max(ternodes)
    splnodes <- setdiff(allnodes,ternodes)
    stats <- lapply(splnodes, function(x) round(rev(sort(log(nodes(ct,x)[[1]]$criterion$criterion))),10))
    names(stats) <- splnodes
  }
  if(class(ct)[1]=='constparty') {
    allnodes <- partykit::nodeids(ct, terminal = FALSE)
    ternodes <- partykit::nodeids(ct, terminal = TRUE)
    splnodes <- setdiff(allnodes,ternodes)
    stats <- partykit::nodeapply(ct, ids = splnodes, FUN = function(x) partykit::info_node(x)$criterion)
  }
  return(stats)
}