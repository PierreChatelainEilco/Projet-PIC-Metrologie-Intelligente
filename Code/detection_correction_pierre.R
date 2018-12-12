### import data test
data <- read.csv(file="C:\\Users\\Remi\\Desktop\\Eil\\ING3\\PIC\\Notebook\\test.csv")

### Calcul de toutes les valeurs de la boite a moustache stocker dans une variable table
infodata<-c()
for(v in data)
{
  s <- summary(v)
  infodata <- rbind(infodata,s)
}
rownames(infodata) <- names(data)

### Correction NA par valeur moyenne
dataCorrige <- data
for(i in seq(1,dim(dataCorrige)[2]))
{
  v <- dataCorrige[i]
  v[is.na(v)] <- infodata[i,"Mean"]
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

