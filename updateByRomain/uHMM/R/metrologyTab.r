#' @title Import tab of the uHMM interface
#' @description This function generates the import tab of the \code{\link{uHMMinterface}}, in which the user can import a data file and select a save directory.
#' @param userType character : either "default" or "expert"
#' @param leftMargin left magin size of interface tabs.
#' @param tm a one row dataframe containing text to display in the interface.
#' @param console frame of the uHMM interface in which messages should be displayed. 
#' @param graphicFrame frame of the uHMM interface in which graphics should be dispayed.
#' @param win1 frame of the uHMM interface containing main tabs.
#' @param uHMMenv environment in which data and intermediate results are stored.
#' @import tcltk tcltk2
#' @export

.metrologyTab<-function(userType,tm,leftMargin=30,console,graphicFrame,win1,uHMMenv){
  
  tkfocus(win1$env$metrology)
  
  #### Fix button
  fixButton<-tk2button(win1$env$metrology,text=tm$fixDataLabel,image="fix",compound = "left")
  fixButton.fun <- function(){
    
    infodata<-c()
    for(v in uHMMenv$rawData[c(-1,-2)])
    {
      s <- summary(v)
      infodata <- rbind(infodata,s)
    }
    rownames(infodata) <- names(uHMMenv$rawData[c(-1,-2)])
    recap <- infodata[,7]
    .tkAfficherTableau(recap,"Nombre NA")
    
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
    print(infodata)
    
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
    print(infodata)
    recapNbVA<- infodata[,11]
    positions <- data.frame(positions)
    .tkAfficherTableau(recapNbVA,"Nombre Valeurs Aberrantes")
  
  }
  tkconfigure(fixButton,command=fixButton.fun)
  tkgrid(fixButton,row=3,column=3)
  
  if (userType == "default")
{
  # Bouton auto-correction
  browseButton<-tk2button(win1$env$metrology,text=tm$autocorrection,image="data",compound = "left")
  browseButton.fun <- function(){
    
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
    print(dataCorrige)
    positions <- data.frame(positions)
    # for (i in seq(1,dim(positions)[1])) {
    #   for (j in seq(1,dim(positions)[2])) {
    #     if(positions[i,j] == 1){
    #       
    #       dataCorrige[i,j] <- infodata[j,"Ext. sup."]
    #     }
    #     if(positions[i,j] == -1){
    #       dataCorrige[i,j] <- infodata[j,"Ext. inf."]
    #     }
    #   }
    # }
    for(i in seq(1,dim(dataCorrige)[2]))
    {
      v <- dataCorrige[i]
      dataCorrige[i] <- infodata[i,"Ext. inf."]*(v<infodata[i,"Ext. inf."]) + infodata[i,"Ext. sup."]*(v>infodata[i,"Ext. sup."])+v*(v==0)
    }
    print(positions)
    print(dataCorrige)
    
  
  }
  tkconfigure(browseButton,command=browseButton.fun);
  tkgrid(browseButton,row=8,column=3)
  }
  

  # Next tab
  nextTabFrame <- tkwidget(win1$env$metrology,"labelframe",text=tm$titleNextFrame,borderwidth = 0)
  tkgrid(nextTabFrame,column=0,row=10,padx=c(leftMargin,0),pady=c(50,0),sticky="w")

  classifButton<-tk2button(nextTabFrame,text=tm$classificationTabLabel,image="run",compound = "left")
  classifButton.fun<-function(){

    if(!exists("rawData",where=uHMMenv)){
      assign("rawData",MarelCarnot,envir=uHMMenv)
      assign("firstDataFile","MarelCarnot",uHMMenv)
      assign("rawMoments",dateProcessing(MarelCarnot),envir=uHMMenv)
    }

    if(!exists("rawData",where=uHMMenv)){
      tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
    }
    else if(uHMMenv$saveDirectory==""){
      tkmessageBox(message=tm$noDirectoryMsg,type="ok",icon="info", title=tm$warningLabel)
    }else{

      .variableTab(leftMargin=leftMargin,userType=userType,tm=tm,
                   console=console,graphicFrame=graphicFrame,win1=win1,
                   uHMMenv=uHMMenv)
      tk2notetab.select(win1$env$nb, tm$variableTabLabel)
      tkinsert(console,"1.0","\n---------------------------------------\n")

    }
  }
  tkconfigure(classifButton,command=classifButton.fun)
  tkgrid(classifButton,row=10,column=0)


  modelingButton<-tk2button(nextTabFrame,text=tm$modelingTabLabel,image="run",compound = "left")
  modelingButton.fun<-function(){

    if(!exists("rawData",where=uHMMenv)){
      assign("rawData",MarelCarnot,envir=uHMMenv)
      assign("firstDataFile","MarelCarnot",uHMMenv)
      assign("rawMoments",dateProcessing(MarelCarnot),envir=uHMMenv)
    }

    if(!exists("rawData",where=uHMMenv)){
      tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
    }
    else if(uHMMenv$saveDirectory==""){
      tkmessageBox(message=tm$noDirectoryMsg,type="ok",icon="info", title=tm$warningLabel)
    }else{

      .modelingTab(leftMargin=leftMargin,tm=tm,
                   console=console,graphicFrame=graphicFrame,win1=win1,
                   uHMMenv=uHMMenv)
      tk2notetab.select(win1$env$nb, tm$modelingTabLabel)
      tkinsert(console,"1.0","\n---------------------------------------\n")
    }
  }
  tkconfigure(classifButton,command=classifButton.fun)
  tkgrid(modelingButton,row=11,column=0)


  predictButton<-tk2button(nextTabFrame,text=tm$predictionTabLabel,image="run",compound = "left")
  predictButton.fun<-function(){

    if(!exists("rawData",where=uHMMenv)){
      assign("rawData",MarelCarnot,envir=uHMMenv)
      assign("firstDataFile","MarelCarnot",uHMMenv)
      assign("rawMoments",dateProcessing(MarelCarnot),envir=uHMMenv)
    }

    if(!exists("rawData",where=uHMMenv)){
      tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
    }
    else if(uHMMenv$saveDirectory==""){
      tkmessageBox(message=tm$noDirectoryMsg,type="ok",icon="info", title=tm$warningLabel)
    }else{
      assign("rawValidData",uHMMenv$rawData,envir=uHMMenv)
      assign("rawValidMoments",uHMMenv$rawMoments,envir=uHMMenv)
      .predictionTab(leftMargin=leftMargin,tm=tm,
                     console=console,graphicFrame=graphicFrame,win1=win1,
                     uHMMenv=uHMMenv)
      .periodSelectionFrame(data=uHMMenv$rawValidData,tm=tm,
                            leftMargin=leftMargin,uHMMenv=uHMMenv,win1=win1)
      tk2notetab.select(win1$env$nb, tm$predictionTabLabel)

      fileLab<-tklabel(win1$env$prediction,text=uHMMenv$firstDataFile)
      tkgrid(fileLab,row=3,column=1,sticky="w")

      tkinsert(console,"1.0","\n---------------------------------------\n")
    }

  }
  tkconfigure(predictButton,command=predictButton.fun)
  tkgrid(predictButton,row=12,column=0)
}
