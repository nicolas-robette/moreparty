#' @importFrom stats as.formula chisq.test complete.cases cor lm

#' @importFrom partykit nodeapply info_node

#' @export

BivariateAssoc <- function(Y,X,xx=TRUE) {

  X <- as.data.frame(X)
  xnames <- names(X)
  xformats <- sapply(X,class)
  yformat <- class(Y)
  
  df <- cbind.data.frame(Y,X)
  formule <- as.formula(paste('Y ~',paste(xnames,collapse='+')))
  ct <- partykit::ctree(formule, df, control=partykit::ctree_control(stump=TRUE))
<<<<<<< HEAD
  info <- partykit::nodeapply(ct, ids = 1, function(n) partykit::info_node(n)$criterion)[[1]]
  p.value <- info[2,]
  criterion <- info[3,]
=======
  log.pval <- partykit::nodeapply(ct, ids = 1, function(n) partykit::info_node(n)$criterion)[[1]][2,]

>>>>>>> e7d3972432d46346c4853e3b0dc4d5c9771414ad
  res <- list()
  for(i in 1:ncol(X)) {
    # print(i)
    if(yformat=='numeric' & xformats[i] %in% c('numeric','integer')) {
      assoc <- cor(Y, X[,i], use='complete.obs', method='kendall')
      mesure='kendall'
    }
    if(yformat=='numeric' & xformats[i]=='factor') {
      assoc <- summary(lm(Y ~ X[,i]))$adj.r.squared
      mesure='eta2'
    }
    if(yformat=='factor' & xformats[i]%in% c('numeric','integer')) {
      assoc <- summary(lm(X[,i] ~ Y))$adj.r.squared
      mesure='eta2'
    }
    if(yformat=='factor' & xformats[i]=='factor') {
      t <- table(Y,X[,i])
      assoc <- sqrt(chisq.test(t)$statistic / (length(Y)*(min(nrow(t),ncol(t))-1)))
      mesure="cramer"
    }
    res[[i]] <- data.frame(mesure,assoc,stringsAsFactors = F)
  }
  res <- do.call('rbind.data.frame',res)
  restot <- data.frame(variable=xnames,p.value=round(p.value,5),criterion=criterion,measure=res$mesure,assoc=round(res$assoc,3))
  restot <- restot[order(restot$criterion, decreasing=T),]
  rownames(restot) <- NULL

  if(xx==TRUE) {
    combi <- utils::combn(xnames,2,simplify=F)
    res <- list()
    for(i in 1:length(combi)) {
      x1 <- X[,combi[[i]][1]]
      x2 <- X[,combi[[i]][2]]

      df <- data.frame(x1,x2,stringsAsFactors=F)
      df <- df[complete.cases(df),]
      ct <- partykit::ctree(x1~x2, data=df, control=partykit::ctree_control(stump=TRUE))
      info <- nodeapply(ct, ids = 1, function(n) info_node(n)$criterion)[[1]]
      p.value <- info[2,]
      criterion <- info[3,]

      if(class(x1) %in% c('numeric','integer') & class(x2) %in% c('numeric','integer')) {
        assoc <- cor(x1,x2, use='complete.obs', method='kendall')
        mesure='kendall'
      }
      if(class(x1) %in% c('numeric','integer') & class(x2)=='factor') {
        assoc <- summary(lm(x1~x2))$adj.r.squared
        mesure='eta2'
      }
      if(class(x1)=='factor' & class(x2) %in% c('numeric','integer')) {
        assoc <- summary(lm(x2~x1))$adj.r.squared
        mesure='eta2'
      }
      if(class(x1)=='factor' & class(x2)=='factor') {
        t <- table(x1,x2)
        assoc <- sqrt(chisq.test(t)$statistic / (length(Y)*(min(nrow(t),ncol(t))-1)))
        mesure="cramer"
      }
      res[[i]] <- data.frame(mesure,assoc,p.value,criterion,stringsAsFactors = F)
    }
    res <- do.call('rbind.data.frame',res)
    noms <- do.call('rbind.data.frame',combi)
    restot2 <- data.frame(variable1=noms[,1],variable2=noms[,2],p.value=round(res$p.value,5),criterion=res$criterion,measure=res$mesure,assoc=round(res$assoc,3),row.names=NULL)
    restot2 <- restot2[order(restot2$criterion, decreasing=T),]
    rownames(restot2) <- NULL
  } else {
    restot2 <- NULL
  }
  
  return(list(YX=restot, XX=restot2))
}
