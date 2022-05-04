#' @export

NiceTreePlot <- function(ct, inner_plots = FALSE) {
  
  Y <- ct[[1]]$fitted[,"(response)"]
  
  getvar <- function(id, nobs) {
    as.character(partykit::nodeapply(partykit::as.simpleparty(ct), ids = partykit::nodeids(ct, terminal = FALSE), function(x) {names(partykit::info_node(x)$p.value)})[id])
  }
  
  if(!inner_plots & is.factor(Y)) {
    plot(ct,
         inner_panel = partykit::node_inner(ct, id = FALSE, pval = FALSE),
         terminal_panel = partykit::node_barplot(ct, id = FALSE, beside = TRUE),
         gp = grid::gpar(cex=0.8),
         ep_args = list(justmin = 15))
  }
  
  if(inner_plots & is.factor(Y)) {
    plot(ct,
         inner_panel = partykit::node_barplot(ct, mainlab = getvar, id = FALSE, beside = TRUE, gp = grid::gpar(cex=0.7)),
         terminal_panel = partykit::node_barplot(ct, id = FALSE, beside = TRUE),
         gp = grid::gpar(cex = 0.8),
         ep_args = list(justmin = 15))
  }
  
  if(!inner_plots & is.numeric(Y)) {
    plot(ct,
         inner_panel = partykit::node_inner(ct, id = FALSE, pval = FALSE),
         terminal_panel = partykit::node_boxplot(ct, id = FALSE),
         gp = grid::gpar(cex = 0.8),
         ep_args = list(justmin = 15))
  }
  
  if(inner_plots & is.numeric(Y)) {
    plot(ct,
         inner_panel = partykit::node_boxplot(ct, mainlab = getvar, id = FALSE, gp = grid::gpar(cex = 0.7)),
         terminal_panel=partykit::node_boxplot(ct, id = FALSE),
         gp = grid::gpar(cex = 0.8),
         ep_args = list(justmin = 15))
  }
}