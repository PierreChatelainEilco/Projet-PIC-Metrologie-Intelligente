.autoCorrection <- function(uHMMenv){
  
  ### Calcul de toutes les valeurs de la boite a moustache stocker dans une variable table
  infodata<-c()
  for(v in uHMMenv$rawData[c(-1,-2)])
  {
    s <- summary(v)
    infodata <- rbind(infodata,s)
  }
  rownames(infodata) <- names(uHMMenv$rawData[c(-1,-2)])
  
  ### Correction NA par valeur moyenne
  dataCorrige <- uHMMenv$rawData[c(-1,-2)]
  for(i in seq(1,dim(dataCorrige)[2]))
  {
    v <- dataCorrige[i]
    v[is.na(v)] <- infodata[i,"Median"]
    dataCorrige[i] <- v
  }
  
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
  for(i in seq(1,dim(dataCorrige)[2]))
  {
    v <- dataCorrige[i]
    position <- -(v<infodata[i,"Ext. inf."]) + (v>infodata[i,"Ext. sup."])
    positions <- cbind(positions,position)
    nb <- sum(position!=0)
    outliers <- c(outliers,nb)
  }
  infodata<- cbind(infodata,Outliers=outliers)
  positions <- data.frame(positions)
  assign("flagedData",positions,envir=uHMMenv)
  for(i in seq(1,dim(dataCorrige)[2]))
  {
    v <- dataCorrige[i]
    position <- positions[i]
    v[position==-1] <- infodata[i,"Ext. inf."]
    v[position==1] <- infodata[i,"Ext. sup."]
    dataCorrige[i] <- v
  }
  tmp1 <- uHMMenv$rawData[,1:2]
  dataCorrige <- cbind(tmp1, dataCorrige)
  
  assign("dataCorrige",dataCorrige,envir=uHMMenv)
}