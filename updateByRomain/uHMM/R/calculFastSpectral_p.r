#----------------------------------------
# Similarite noyau gaussien selon Zelnik-Manor et Perona
#' @title Similarity matrix with local scale parameter
#' @author Kevin Rousseeuw ? Emilie Poisson-Caillault ?
#' @description Compute and return the similarity matrix of a data frame using gaussian kernel with a local scale parameter for each data point, 
#' rather than a unique scale parameter.
#' @param data a matrix or numeric data frame.
#' @param K number of neighbours considered to compute scale parameters.
#' @source Zelnik-Manor, Lihi, and Pietro Perona. "Self-tuning spectral clustering." Advances in neural information processing systems. 2004.
#' @return The matrix of similarity.
#' @importFrom stats dist
#' @export

ZPGaussianSimilarity <- function (data, K){
  K = min(K, NROW(data));
  Res_kppv = kppv(train=data, test=NULL, label=NULL, K=K); #test=NULL => test=data
  sim <- matrix(0,NROW(data), NROW(data));
  sigma_local_i <- Res_kppv$D[,K] + .Machine$double.eps
  sigma_local <- outer(sigma_local_i, sigma_local_i, "*")
  dis <- as.matrix(dist(data))
  sim <- exp(-dis^2/sigma_local) 
  return(sim)
}



#----------------------------------------
# Calcul de gap pour determination de K
#' @title Compute gap between eigenvalues of a similarity matrix
#' @author Kevin Rousseeuw ? Emilie Poisson-Caillault ?
#' @description Compute the highest gap between eigenvalues of a similarity matrix.
#' The 2 first eigenvalues are considered as equal to each other (the gap between the 2 first eigenvalues is set to 0).
#' @param similarity a similarity matrix.
#' @param Gmax the maximum gap value allowed (only the first Gmax eigenvalues will be taken into account).
#' @return The function returns a list containing the following components:
#' @return \item{gap}{a vector indicating the gap between similarity matrix eigenvalues (the gap between the 2 first eigenvalues is set to 0)}
#' @return \item{Kmax}{an integer indicating the index of the highest gap (the highest gap is between the Kmax-th and the (Kmax+1)-th eigenvalues)}
#' @examples 
#' similarity<-matrix(runif(100),ncol=10)
#' Gap<-computeGap(matrix(runif(100),ncol=10),10)
#' plot(Gap$gap,type="h",main=paste("Gap =",Gap$K))
#' 
#' @export
computeGap <- function(similarity, Gmax){
  d <- rowSums( abs(similarity) )
  ds <- d^(-0.5)
  L <- ds * t(similarity * ds) #acceleration
  
  e=eigen(L,symmetric=TRUE)
  Z=e$vector[,1:Gmax]
  val=e$value[1:Gmax]
  
  #extraction des Gmax plus grands vecteurs propres  au sens de lambda
  nbVal <- min(Gmax+1, NROW(similarity+1))
  gap <- rep(0,nbVal-1)
  for(v in 2:(nbVal-1)){
    gap[v]=abs(val[v]-val[v+1])
  }
  K <- which.max(gap)
  
  return(list(gap=gap, Kmax=K,valp=val))
}



#----------------------------------------
#' KmeansAutoElbowDicotomie return partition and K number of groups according to kmeans clustering and Elbow method
#' @title KmeansAutoElbowDicotomie function
#' @author Kevin Rousseeuw, Emilie Poisson-Caillault
#' @description KmeansAutoElbowDicotomie performs k-means clustering on a dataframe with selection of optimal number of clusters using elbow criteria.
#' @param features dataframe or matrix of raw data.
#' @param Kmax maximum number of clusters allowed.
#' @param StopCriteria elbow method cumulative explained variance > criteria to stop K-search. (???)
#' @param graph boolean, if TRUE figures are plotted.
#' @return The function returns a list containing the following components:
#' \item{K}{number of groups in data according to explained variance and kmeans algorithm.}
#' \item{res.kmeans}{an object of class "kmeans" (see \code{\link[stats]{kmeans}}) containing classification results.}
#' @examples 
#' x <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' colnames(x) <- c("x", "y")
#' km<-KmeansAutoElbowDicotomie(x,round(dim(x)/25,0)[1],StopCriteria=0.99,graph=TRUE)
#' plot(x,col=km$res.kmeans$cluster)
#' points(km$res.kmeans$centers, col = 1:km$K, pch = 16)
#' 
#' @seealso \code{\link[stats]{kmeans}}
#' @export
#' @importFrom stats kmeans
#' @importFrom graphics plot
KmeansAutoElbowDicotomie<-function(features,Kmax,StopCriteria=0.99,graph=FALSE){
  N=nrow(features);
  Within=rep(-1,Kmax);
  explainedVar=rep(-1,Kmax);
  fastCenters <- NULL;
  start=0;
  end=Kmax;
  i=(end+start)/2
  
  while(abs(i-end)!=0){
    
    fastCenters <- NULL; #find center quickly on 5% of points
    # if(N>20000){
    # idx<-sample((1:N),round(N*0.05),replace=FALSE);
    # res<-kmeans(features[idx,],centers=i, iter.max = 200, nstart = 20, algorithm = c("Hartigan-Wong"));
    # fastCenters=res$centers;
    # }else{ fastCenters=i;}
    fastCenters=i;
    
    #kmeans with this initial centers  if many data
    Res.km <- kmeans(features, centers=fastCenters, iter.max = 200, nstart = 1,algorithm = c("Hartigan-Wong"));
    Within[i] <- Res.km$tot.withinss;
    explainedVar[i]=Res.km$betweenss/Res.km$totss;
    
    if(explainedVar[i]>StopCriteria){
      end=i;
      i=round((end+start)/2)
    }
    
    else{
      start=i;
      i=round((end+start)/2)
    }
    
    if(abs(i-end)==1){
      end=i
    }
  }
  K=length(unique(Res.km$cluster))
  
  if(graph==TRUE) {
    dev.new(noRStudioGD = FALSE); plot(2:K,Within[2:K],type="l", main="total of within-class inertie");
    dev.new(noRStudioGD = FALSE); plot(2:K,explainedVar[2:K], main="explained variance");
  }
  
  return(list(K=K,res.kmeans=Res.km))
}



#------------------------------------------------------
#' Algorithme de Jordan pour un grand jeu de donnees : echantillonage puis spectral
#' @title Jordan Fast Spectral Algorithm
#' @description Perform the Jordan spectral algorithm for large databases. Data are sampled, using K-means with Elbow criteria, before being classified. 
#' @param data numeric matrix or dataframe.
#' @param nK number of clusters desired. If NULL, optimal number of clusters will be computed using gap criteria. 
#' @param Kech maximum number of representative points in sampled data.
#' @param StopCriteriaElbow maximum (minimum ?) de variance expliquees des points representatifs souhaite.
#' @param neighbours number of neighbours considered for the computation of local scale parameters.
#' @param method string specifying the spectral classification method desired, either "PAM" (for spectral kmedoids) or "" (for "spectral kmeans").
#' @param nb.iter number of iterations.
#' @param uHMMinterface logical indicating whether the function is used via the uHMMinterface.
#' @param console frame of the uHMM interface in which messages should be displayed (only if uHMMinterface=TRUE).
#' @param tm a one row dataframe containing text to display in the uHMMinterface (only if uHMMinterface=TRUE).
#' 
#' @return The function returns a list containing:
#' \item{sim}{similarity matrix of representative points, multiplied by its transpose (\code{\link{ZPGaussianSimilarity}}).}
#' \item{label}{vector of cluster sequencing.}
#' \item{gap}{number of clusters.}
#' \item{labelElbow}{vector of prototype sequencing.}
#' \item{vpK}{}
#' \item{valp}{}
#' \item{echantillons}{}
#' \item{label.echantillons}{}
#' \item{numSymbole}{}
#' 
#' @author Emilie Poisson-Caillault ?
#' @export
#' @import tcltk tcltk2
#' @importFrom class knn
#' @importFrom cluster silhouette
#' @importFrom clValid dunn connectivity
#' @importFrom stats dist
#' @seealso \code{\link{KmeansAutoElbowDicotomie}} \code{\link{ZPGaussianSimilarity}}  \code{\link[class]{knn}} 
#' \code{\link[cluster]{silhouette}} \code{\link[clValid]{dunn}} \code{\link[clValid]{connectivity}} \code{\link[stats]{dist}}
#' 
computeFastSpectralJordan<-function(data, nK=NULL, Kech=2000, StopCriteriaElbow=0.97, neighbours=7, method="", nb.iter=10, uHMMinterface=FALSE,console=NULL,tm=NULL){
  #echantillonage des donnees
  #selection des K centres representatifs avec la methode Kmeans Elbow
  silhMoyenne=rep(0,10)
  indiceDunn=rep(0,10)
  connectivite=rep(0,10)
  silhMoyenneP=rep(0,10)
  indiceDunnP=rep(0,10)
  connectiviteP=rep(0,10)
  mncut=rep(100000,10)
  minMNcut=.Machine$integer.max
  simF<-NULL;echantillon<-NULL;labelSpectralF<-NULL;spectralF<-NULL
  
  tailledata=dim(data)[1]
  if (Kech>tailledata) Kech=tailledata-1;
  for(test in 1:nb.iter){
    print("iteration"); print(test); 
    if (uHMMinterface){
      tkinsert(console,"1.0",paste("----- iteration",test,tm$outOfLabel,nb.iter,"-----\n\n")) # display in console
    }
    
    K=nK
    similarity<-NULL;echantillon<-NULL;labelSpectral<-NULL;spectral<-NULL
    KElbow=KmeansAutoElbowDicotomie(features=data,Kmax=Kech,StopCriteria=StopCriteriaElbow,graph=F)
    nbProto=KElbow$K;
    print(paste("Nb prototypes selectionnes par Kmeans/ELbow= ",nbProto));   
    if (uHMMinterface){
      tkinsert(console,"1.0",paste(tm$nbPrototypesLabel,tm$nbProto,"\n")) # display in console
    }
    
    labelInitial=KElbow$res.kmeans$cluster
    
    echantillon=KElbow$res.kmeans$centers;
    distanceEch=as.matrix(dist(echantillon));
    
    #Calcul de la matrice de similarite  des centres representatifs des donnees
    similarity=ZPGaussianSimilarity(data=echantillon, K=neighbours)
    similarity=similarity%*%t(similarity) #Gram matrice pour faire du spectral car avec ZP non gram
    
    #spectral kmeans sur les points representatifs
    #calcul du Gap, Kmax = le nombre de centres representatifs
    if (is.null(nK)){
      gap=computeGap(similarity,nbProto)
      
      K=max(2,gap$Kmax)
      
    }
    print(paste("Nb groupes detectes dans les echantillons - gap= ",K)); 
    if (uHMMinterface){
      tkinsert(console,"1.0",paste(tm$nbGroupsInSamplesLabel,K,"\n"))  # display in console
    }
    
    #calcul du spectral Kmeans
    labelSpectral=rep(0,nbProto);
    if(method=="PAM"){
      print("spectral kmedoids"); 
      if (uHMMinterface){
        tkinsert(console,"1.0",paste("spectral kmedoids","\n"))  # display in console
      }
      spectral=spectralPamClusteringNg(similarity, K); 
      labelSpectral=spectral$label
    }else{
      print("spectral kmeans")
      if (uHMMinterface){
        tkinsert(console,"1.0",paste("spectral kmeans","\n"))  # display in console
      }
      spectral=KpartitionNg(similarity, K)
      labelSpectral=spectral$label
    }
    
    label=rep(0,nrow(data))
    label=knn(train=echantillon,test=data, labelSpectral,k=1, prob=FALSE)
    label=knn(train=data,test=data, label,k=3, prob=FALSE)
    
    print("calcul MNCUT")
    if (uHMMinterface){
      tkinsert(console,"1.0",paste(tm$computeMNCUTLabel,"\n"))  # display in console
    }  
    mncutT=cutCalculation(similarity,labelSpectral,K)$mncut
    mncut[test]=mncutT
    if(mncutT<minMNcut){simF=similarity;echantillonF=echantillon;labelElbow=labelInitial;labelSpectralF=labelSpectral;spectralF=spectral; minMNCut=mncutT} 
    print("calcul sil...")
    if (uHMMinterface){
      tkinsert(console,"1.0",paste(tm$computeSilLabel,"\n"))  # display in console
    }
    distanceProj=as.matrix(dist(spectral$vecteursPropresProjK))
    
    s=silhouette(x=labelSpectral,dmatrix=distanceEch)
    sil=summary(s);
    silhMoyenne[test]=sil$avg.width
    sP=silhouette(x=labelSpectral,dmatrix=distanceProj)
    silP=summary(sP);
    silhMoyenneP[test]=silP$avg.width
    indiceDunn[test]=dunn(distanceEch, clusters=labelSpectral)
    indiceDunnP[test]=dunn(distanceProj, clusters=labelSpectral)
    connectivite[test]=connectivity(distanceEch, clusters=labelSpectral)
    connectiviteP[test]=connectivity(distanceProj, clusters=labelSpectral)
  }
  
  #Labellisation des donnees par kppv avec les prototypes
  
  label=knn(train=echantillonF,test=data, labelSpectralF,k=1, prob=FALSE)
  #correction des points dans l'espace d'entree 
  label=knn(train=data,test=data, label,k=3, prob=FALSE)
  
  numSymbole=knn(train=echantillonF,test=data,cl=1:nrow(echantillonF),k=1,prob=FALSE);
  
  out<-list(sim=simF,label=label,gap=K, labelElbow=labelElbow, vpK=spectralF$vecteursPropresProjK,valp=spectralF$valeursPropresK,echantillons=echantillonF, label.echantillons=labelSpectralF,numSymbole=numSymbole)
}



#----------------------------------------
# Algorithme Ng K>=2
#' @title KpartitionNg function
#' @description Perform spectral classification on the similarity matrix of a dataset (Ng et al. (2001) algorithm), using kmeans algorithm on data projected in the space of its K first eigen vectors.
#' @author Kevin Rousseeuw ? Emilie Poisson Caillault ?
#' @param similarity matrix of similarity.
#' @param K number of clusters.
#' @return The function returns a list containing:
#' \item{label}{vector of cluster sequencing.}
#' \item{centres}{matrix of cluster centers in the space of the K first normalised eigen vectors.}
#' \item{vecteursPropresProjK}{matrix containing, in columns, the K first normalised eigen vectors of the similarity matrix.}
#' \item{valeursPropresK}{vector containing the K first eigen values of the similarity matrix.}
#' \item{vecteursPropres}{matrix containing, in columns, eigen vectors of the similarity matrix.}
#' \item{valeursPropres}{vector containing eigen values of the similarity matrix.}
#' \item{inertieZ}{vector of within-cluster sum of squares, one component per cluster.}
#' @source Ng Andrew, Y., M. I. Jordan, and Y. Weiss. "On spectral clustering: analysis and an algorithm [C]." Advances in Neural Information Processing Systems (2001).
#' @importFrom stats kmeans
#' @export
KpartitionNg<- function(similarity, K){
  diag(similarity)=0;
  #calcul de la matrice des degres puissance 1/2
  d=rowSums(similarity);
  ds=d^(-0.5)
  D=diag(ds);
  #calcul de la matrice Laplacienne
  I=diag(1,nrow(similarity),ncol(similarity));
  L=D %*% similarity %*% D;
  #extraction des K plus grands vecteurs propres  au sens de lambda
  e=eigen(L, symmetric=TRUE);
  Z=e$vectors[,1:K];
  val=e$values[1:K];
  
  #projection de Z sur la sphere unite
  Zn=Z/apply(Z,MARGIN=1,FUN=function(x) norm(matrix(x),"f"));
  #classification
  cl=kmeans(Zn,centers=K,iter.max = 100, nstart = 20);
  
  return(list(label=cl$cluster, centres=cl$centers, vecteursPropresProjK=Zn, valeursPropresK=val, vecteursPropres=e$vectors, valeursPropres=e$values, inertieZ=cl$withinss))
}




#----------------------------------------
# Algorithme spectral PAM Ng K>=2
# + ajout Val. abs. pour generalisation au SSSC
#' @title spectralPamClusteringNg function
#' @description Perform spectral classification on the similarity matrix of a dataset, using pam algorithm (a more robust version of K-means) on projected data.
#' @seealso \code{\link[cluster]{pam}}
#' @author Kevin Rousseeuw ? Emilie Poisson Caillault ?
#' @param similarity matrix of similarity
#' @param K number of clusters
#' @source Ng Andrew, Y., M. I. Jordan, and Y. Weiss. "On spectral clustering: analysis and an algorithm [C]." Advances in Neural Information Processing Systems (2001).
#' @return The function returns a list containing:
#' \item{label}{vector of cluster sequencing.}
#' \item{centres}{matrix of cluster medoids (similar in concept to means, but medoids are members of the dataset) in the space of the K first normalised eigen vectors.}
#' \item{id.med}{integer vector of indices giving the medoid observation numbers.} 
#' \item{vecteursPropresProjK}{matrix containing, in columns, the K first normalised eigen vectors of the similarity matrix.} 
#' \item{valeursPropresK}{vector containing the K first eigen values of the similarity matrix.} 
#' \item{vecteursPropres}{matrix containing, in columns, eigen vectors of the similarity matrix.} 
#' \item{valeursPropres}{vector containing eigen values of the similarity matrix.} 
#' \item{cluster.info}{matrix, each row gives numerical information for one cluster. 
#' These are the cardinality of the cluster (number of observations), 
#' the maximal and average dissimilarity between the observations in the cluster and the cluster's medoid, 
#' the diameter of the cluster (maximal dissimilarity between two observations of the cluster), 
#' and the separation of the cluster (minimal dissimilarity between an observation of the cluster and an observation of another cluster).} 
#' @export
#' @importFrom cluster pam
#' 
spectralPamClusteringNg<- function(similarity,K){
  diag(similarity) <- 0
  #calcul de la matrice des degres puissance 1/2
  d <- rowSums( abs(similarity) ) 
  ds <- d^(-0.5)
  D=diag(ds);
  #calcul de la matrice Laplacienne
  I=diag(1,nrow(similarity),ncol(similarity));
  L=D %*% similarity %*% D;
  #extraction des K plus grands vecteurs propres  au sens de lambda
  e=eigen(L, symmetric=TRUE);
  Z=e$vectors[,1:K];
  val=e$values[1:K];
  #projection de Z sur la sphere unite
  Zn=Z/apply(Z,MARGIN=1,FUN=function(x) norm(matrix(x),"f"));
  #classification
  cl <- pam(Zn, K, diss = FALSE)
  
  list(label=cl$clustering, centres=cl$medoids, id.med=cl$id.med, vecteursPropresProjK=Zn, valeursPropresK=val, vecteursPropres=e$vectors, valeursPropres=e$values, cluster.info=cl$clusinfo)
}




#' @title cutCalculation function
#' @author Kevin Rousseeuw ? Emilie Poisson-Caillault ?
#' @description I don't know.
#' @param similarity a similarity matrix.
#' @param label vector of cluster sequencing. (prototypes ???)
#' @param K number of clusters ???.
#' @return The function returns a list containing:
#' \item{mncut}{the inter cluster cut, i.e. K - sum(ratioCutVol).}
#' \item{ratioCutVol}{vector of intra cluster cuts, one component per cluster.}
#' @details intra cluster cut :
#' \deqn{Cut(g_{k},g_{l}) = \sum_{i=1,x(i)\in g_{k}}^{N_{p}}\sum_{j=1,x(j)\in g_{l}}^{N_{p}}w(x(i),x(j)) }
#' @export

cutCalculation<-function(similarity,label,K){
  setLabel=unique(label)
  nbCluster=length(setLabel)
  clusterVolume=rep(0,length(setLabel))
  intraCut=rep(0,length(setLabel))
  ratio=rep(0,length(setLabel))
  degre=rowSums(similarity)
  for(l in 1:nbCluster){
    num=setLabel[l]
    clusterVolume[l]=sum(degre[label==num])
    intraCut[l]=sum(similarity[label==num,label==num])
  }
  ratio=intraCut/clusterVolume
  mncut=K-sum(ratio)
  return(list(mncut=mncut,ratioCutVol=ratio))
}







