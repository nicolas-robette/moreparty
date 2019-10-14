#' @import coin

#' @importFrom stats as.formula chisq.test complete.cases cor lm

#' @export

BivariateAssoc <- function(Y,X,xx=TRUE) {

  X <- as.data.frame(X)
  xnames <- names(X)
  xformats <- sapply(X,class)
  yformat <- class(Y)

  df <- cbind.data.frame(Y,X)
  formule <- as.formula(paste('Y ~',paste(xnames,collapse='+')))
  ct <- party::ctree(formule,df,controls=ctree_control(stump=TRUE))
  test.stats <- nodes(ct,1)[[1]]$criterion$statistic

  res <- list()
  for(i in 1:ncol(X)) {
    # print(i)
    if(yformat=='numeric' & xformats[i] %in% c('numeric','integer')) {
      test <- coin::spearman_test(Y ~ X[,i], distribution='asymptotic', teststat='quadratic', alternative='two.sided')
      p.value <- as.numeric(pvalue(test))
      #test.stat <- statistic(test)
      assoc <- cor(Y, X[,i], use='complete.obs', method='kendall')
      mesure='kendall'
    }
    if(yformat=='numeric' & xformats[i]=='factor') {
      test <- coin::oneway_test(Y ~ X[,i], distribution='asymptotic', alternative='two.sided', teststat='quadratic')
      p.value <- as.numeric(pvalue(test))
      #test.stat <- statistic(test)
      assoc <- summary(lm(Y ~ X[,i]))$adj.r.squared
      mesure='eta2'
    }
    if(yformat=='factor' & xformats[i]%in% c('numeric','integer')) {
      test <- coin::oneway_test(X[,i] ~ Y, distribution='asymptotic', alternative='two.sided', teststat='quadratic')
      p.value <- as.numeric(pvalue(test))
      #test.stat <- statistic(test)
      assoc <- summary(lm(X[,i] ~ Y))$adj.r.squared
      mesure='eta2'
    }
    if(yformat=='factor' & xformats[i]=='factor') {
      test <- coin::chisq_test(Y ~ X[,i], distribution='asymptotic', alternative='two.sided', teststat='quadratic')
      p.value <- as.numeric(pvalue(test))
      #test.stat <- statistic(test)
      t <- table(Y,X[,i])
      assoc <- sqrt(chisq.test(t)$statistic / (length(Y)*(min(nrow(t),ncol(t))-1)))
      mesure="cramer"
    }
    res[[i]] <- data.frame(mesure,assoc,p.value,stringsAsFactors = F)
  }
  res <- do.call('rbind.data.frame',res)
  restot <- data.frame(variable=xnames,test.stat=round(test.stats,1),p.value=round(res$p.value,5),measure=res$mesure,assoc=round(res$assoc,3))
  restot <- restot[order(restot$test.stat,decreasing=TRUE),]
  rownames(restot) <- NULL

  if(xx==TRUE) {
    combi <- utils::combn(xnames,2,simplify=F)
    res <- list()
    for(i in 1:length(combi)) {
      x1 <- X[,combi[[i]][1]]
      x2 <- X[,combi[[i]][2]]

      df <- data.frame(x1,x2,stringsAsFactors=F)
      df <- df[complete.cases(df),]
      ct <- party::ctree(x1~x2,df,controls=ctree_control(stump=TRUE))
      test.stat <- nodes(ct,1)[[1]]$criterion$statistic

      if(class(x1) %in% c('numeric','integer') & class(x2) %in% c('numeric','integer')) {
        test <- coin::spearman_test(x1~x2, distribution='asymptotic', teststat='quadratic', alternative='two.sided')
        p.value <- as.numeric(pvalue(test))
        #test.stat <- statistic(test)
        assoc <- cor(x1,x2, use='complete.obs', method='kendall')
        mesure='kendall'
      }
      if(class(x1) %in% c('numeric','integer') & class(x2)=='factor') {
        test <- coin::oneway_test(x1~x2, distribution='asymptotic', alternative='two.sided', teststat='quadratic')
        p.value <- as.numeric(pvalue(test))
        #test.stat <- statistic(test)
        assoc <- summary(lm(x1~x2))$adj.r.squared
        mesure='eta2'
      }
      if(class(x1)=='factor' & class(x2) %in% c('numeric','integer')) {
        test <- coin::oneway_test(x2~x1, distribution='asymptotic', alternative='two.sided', teststat='quadratic')
        p.value <- as.numeric(pvalue(test))
        #test.stat <- statistic(test)
        assoc <- summary(lm(x2~x1))$adj.r.squared
        mesure='eta2'
      }
      if(class(x1)=='factor' & class(x2)=='factor') {
        test <- coin::chisq_test(x1~x2, distribution='asymptotic', alternative='two.sided', teststat='quadratic')
        p.value <- as.numeric(pvalue(test))
        #test.stat <- statistic(test)
        t <- table(x1,x2)
        assoc <- sqrt(chisq.test(t)$statistic / (length(Y)*(min(nrow(t),ncol(t))-1)))
        mesure="cramer"
      }
      res[[i]] <- data.frame(mesure,assoc,p.value,test.stat,stringsAsFactors = F)
    }
    res <- do.call('rbind.data.frame',res)
    noms <- do.call('rbind.data.frame',combi)
    restot2 <- data.frame(variable1=noms[,1],variable2=noms[,2],test.stat=round(res$test.stat,1),p.value=res$p.value,measure=res$mesure,assoc=round(res$assoc,3),row.names=NULL)
    restot2 <- restot2[order(restot2$test.stat,decreasing=TRUE),]
    rownames(restot2) <- NULL
  } else {
    restot2 <- NULL
  }

  return(list(YX=restot, XX=restot2))
}
