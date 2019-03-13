.detection <- function(uHMMenv)
{
  infodata<-c()
  for(v in uHMMenv$rawData[c(-1,-2)])
  {
    s <- summary(v)
    infodata <- rbind(infodata,s)
  }
  rownames(infodata) <- names(uHMMenv$rawData[c(-1,-2)])
  recap <- infodata[,7]
  .tkAfficherTableau2(recap,nbval=dim(uHMMenv$rawData)[1],title="Nombre NA",messagebas = "*NA : Nombre de valeurs inconnues",messagehaut=paste("Nombre total de mesures : ",dim(uHMMenv$rawData)[1]))
  
  detectData <- uHMMenv$rawData[c(-1,-2)]
  ### Vecteur de boolean inf 1e quartile
  infodata <- cbind(infodata,IQR = infodata[,5]-infodata[,2])
  ext <- c()
  for(i in seq(1,dim(infodata)[1]))
  {
    ext <- c(ext,max(infodata[i,1],infodata[i,2]-1.5*infodata[i,"IQR"]))
  }
  infodata <- cbind(infodata,ext)
  colnames(infodata)[length(colnames(infodata))] <- "Ext. inf."
  ext <- c()
  for(i in seq(1,dim(infodata)[1]))
  {
    ext <- c(ext,min(infodata[i,6],infodata[i,5]+1.5*infodata[i,"IQR"]))
  }
  infodata <- cbind(infodata,ext)
  colnames(infodata)[length(colnames(infodata))] <- "Ext. sup."
  
  
  ### Addition des deux compteurs de valeurs aberrantes
  outliers = c()
  positions = c()
  for(i in seq(1,dim(detectData)[2]))
  {
    v <- detectData[i]
    position <- -(v[!is.na(v)]<infodata[i,"Ext. inf."]) + (v[!is.na(v)]>infodata[i,"Ext. sup."])
    positions <- cbind(positions,position)
    nb <- sum(position!=0)
    outliers <- c(outliers,nb)
  }
  infodata<- cbind(infodata,Outliers=outliers)
  recapNbVA<- infodata[,11]
  positions <- data.frame(positions)
  .tkAfficherTableau2(recapNbVA,nbval=dim(uHMMenv$rawData)[1],title="Nombre Valeurs Aberrantes",messagebas="*Les valeurs abérantes sont les valeurs hors de l'interval [m-1.5(Q3-Q1), m+1.5(Q3-Q1)]",messagehaut = paste("Nombre total de mesures : ",dim(uHMMenv$rawData)[1]))
  
}