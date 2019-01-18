#--------------------------------------
# Algorithme des plus proches voisins
# sans estimation des classes !! juste calcul des voisins et distances
#' @title k-Nearest Neighbours 
#' @author Emilie Poisson-Caillault ? Kevin Rousseeuw ?
#' @description This function performs the k-Nearest Neighbour algorithm without class estimation, but only computation of distances and neighbours.
#' @param train matrix or data frame of training set. 
#' @param test matrix or data frame of test set. If NULL, the training set is also used as test set.
#' @param K number of neighbours considered.
#' @param label data labels.
#' @return The function returns a list with the following components:
#' \item{D}{matrix of squared root of the distances between observations and their nearest neighbours.}
#' \item{idx}{Index of K nearest neighbours of each observation.}
#' \item{classes}{? vector giving the classe of each observation.}
#' @export
# x=runif(100)
# y=(x+round(x,0)*2+runif(100))/4
# f<-data.frame(x,y)
# plot(f)
# ppv<-kppv(f)
# points(f,col=ppv$idx)
# points(f,col=round(ppv$D,0))
# points(f[1,],cex=2,pch=16)
# points(f[2,],cex=2,pch=16)
# 
kppv <- function(train, test=NULL, label=NULL, K=1){
  auto <- FALSE
  if (is.null(test)) {
    test <- train
    auto <- TRUE
  }
  is.label <- !is.null(label)
  
  idx <- matrix(0, nrow(test), K)
  D <- idx
  
  if(K==1){
    #Loop for each query point
    for(k in 1:nrow(test)){
      d <- colSums((t(train)-test[k,])^2) #plus rapide qu'avec un beau 'apply'...
      if (auto)
        d[k] <- Inf
      idx[k] = which.min(d)
      D[k] = d[idx[k]]
    }
  } else {
    
    for(k in 1:nrow(test)){
      d <- colSums((t(train)-test[k,])^2)
      if (auto)
        d[k] <- Inf
      
      #tri partiel moins long que tri complet
      quant <- quantile(d, probs=(K+1)/length(d)) #+1 pour etre sur d'en avoir K
      quant.idx <- which(d<=quant)
      idx[k,] <- quant.idx[order(d[quant.idx], decreasing=FALSE)[1:K]]
      D[k,] <- d[idx[k,]]
    }
  }
  
  #identification labels
  if (is.label) {
    label <- factor(label)
    decompte <- rep(0, length(levels(label)))
  }
  if (is.label) {
    classes <- rep("", nrow(test))
    for (k in 1:nrow(test)) {
      decompte <- table(label[idx[k,]])
      classes[k] <- names(which.max(decompte))
    }
    classes <- factor(classes)
  } else
    classes <- NULL
  
  return(list(D=sqrt(D), idx=idx, classes=classes))
}

