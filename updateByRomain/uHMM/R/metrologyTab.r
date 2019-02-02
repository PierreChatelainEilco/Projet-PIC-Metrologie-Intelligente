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
  tkconfigure(fixButton,command=fixButton.fun)
  tkgrid(fixButton,row=3,column=3)
  
  #mode classique
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
    graphButton<-tk2button(win1$env$metrology,text=tm$graphBouton)
    graphButton.fun <- function(){
      .representationPlot3(uHMMenv$rawMoments,uHMMenv$rawData,uHMMenv$dataCorrige,tm,"Graphique")
    }
    tkconfigure(graphButton,command=graphButton.fun);
    tkgrid(graphButton,row=12,column=2)
    
    summaryButton<-tk2button(win1$env$metrology,text=tm$summaryLabel,image="loupe",compound = "left")
    summaryButton.fun <-function(){
      if(!exists("dataCorrige",where=uHMMenv)){
        tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
      }else{
        if(exists("metrologyOutput",where=uHMMenv)){
          .unSummary(dataCorrige,uHMMenv$metrologyOutput,summaryLabel=tm$summaryLabel)
        }else{
          .unSummary(dataCorrige,summaryLabel=tm$summaryLabel)
        }
      }
    }
    
    
    tkconfigure(summaryButton,command=summaryButton.fun)
    tkgrid(summaryButton,row=12,column=4)
    
  }
  tkconfigure(browseButton,command=browseButton.fun);
  tkgrid(browseButton,row=8,column=3)
  }
  
  #Mode expert
  if (userType == "expert")
  {
    
    browse1Button<-tk2button(win1$env$metrology,text=tm$meth1correction,image="data",compound = "left")
    browse1Button.fun <- function(){
      
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
      
      tmp1 <- uHMMenv$rawData[,1:2]
      dataCorrige <- cbind(tmp1, dataCorrige)
      
      summaryButton<-tk2button(win1$env$metrology,text=tm$summaryLabel,image="loupe",compound = "left")
      summaryButton.fun <-function(){
        if(!exists("dataCorrige",where=uHMMenv)){
          tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
        }else{
          if(exists("metrologyOutput",where=uHMMenv)){
            .unSummary(dataCorrige,uHMMenv$metrologyOutput,summaryLabel=tm$summaryLabel)
          }else{
            .unSummary(dataCorrige,summaryLabel=tm$summaryLabel)
          }
        }
      }
      tkconfigure(summaryButton,command=summaryButton.fun)
      tkgrid(summaryButton,row=14,column=3)
      
      graphButton<-tk2button(win1$env$metrology,text=tm$graphBouton)
      graphButton.fun <- function(){}
      tkconfigure(graphButton,command=graphButton.fun);
      tkgrid(graphButton,row=12,column=3)
  
    }
    tkconfigure(browse1Button,command=browse1Button.fun);
    tkgrid(browse1Button,row=9,column=2)
    
    browse2Button<-tk2button(win1$env$metrology,text=tm$meth2correction,image="data",compound = "left")
    browse2Button.fun <- function(){
      infodata<-c()
      for(v in uHMMenv$rawData[c(-1,-2)])
      {
        s <- summary(v)
        infodata <- rbind(infodata,s)
      }
      rownames(infodata) <- names(uHMMenv$rawData[c(-1,-2)])
      
      
      dataCorrige <- uHMMenv$rawData[c(-1,-2)]
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
      
      
      graphButton<-tk2button(win1$env$metrology,text=tm$graphBouton)
      graphButton.fun <- function(){}
      tkconfigure(graphButton,command=graphButton.fun);
      tkgrid(graphButton,row=12,column=2)
      
      
      summaryButton<-tk2button(win1$env$metrology,text=tm$summaryLabel,image="loupe",compound = "left")
      summaryButton.fun <-function(){
        if(!exists("dataCorrige",where=uHMMenv)){
          tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
        }else{
          if(exists("metrologyOutput",where=uHMMenv)){
            .unSummary(dataCorrige,uHMMenv$metrologyOutput,summaryLabel=tm$summaryLabel)
          }else{
            .unSummary(dataCorrige,summaryLabel=tm$summaryLabel)
          }
        }
      }
      tkconfigure(summaryButton,command=summaryButton.fun)
      tkgrid(summaryButton,row=14,column=3)
      
    }
      
      
    tkconfigure(browse2Button,command=browse2Button.fun);
    tkgrid(browse2Button,row=9,column=4)
}

  # Next tab
  nextTabFrame <- tkwidget(win1$env$metrology,"labelframe",text=tm$titleNextFrame,borderwidth = 0)
  tkgrid(nextTabFrame,column=0,row=20,padx=c(leftMargin,0),pady=c(50,0),sticky="w")

  classifButton<-tk2button(nextTabFrame,text=tm$classificationTabLabel,image="run",compound = "left")
  classifButton.fun<-function(){
    
    msgBox <- tkmessageBox(message = tm$messageBox,
                           icon = "question", type = "yesnocancel", default = "yes")
    
    msgBoxCharacter <- as.character(msgBox)
    if(msgBoxCharacter == "yes"){
      repertory <- uHMMenv$metrologyOutput
      if(!is.null(repertory)){
        #write.csv(uHMMenv$dataCorrige,file=paste(repertory,"DataCorrected.csv",sep=""),row.names=FALSE)

        n<-dim(uHMMenv$flagedData)[2]
        mergedData <- c()
        mergedData.name <- c()
        for(k in 1:n)
        {
          mergedData.name <- cbind(mergedData.name,colnames(uHMMenv$dataCorrige)[k+2])
          mergedData <- cbind(mergedData,uHMMenv$dataCorrige[,k+2])
          mergedData.name <- cbind(mergedData.name,colnames(uHMMenv$flagedData)[k])
          mergedData <- cbind(mergedData,uHMMenv$flagedData[,k])
        }
        mergedData.name <- cbind(colnames(uHMMenv$dataCorrige)[2],mergedData.name)
        mergedData.name <- cbind(colnames(uHMMenv$dataCorrige)[1],mergedData.name)
        mergedData <- cbind(uHMMenv$dataCorrige[,1:2],mergedData)
        colnames(mergedData)<-mergedData.name
        write.csv(mergedData,file=paste(repertory,"MegaMix.csv",sep=""),row.names=FALSE)
      }
    }

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
  tkgrid(classifButton,row=21,column=0)


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
  tkgrid(modelingButton,row=22,column=0)


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
  tkgrid(predictButton,row=23,column=0)
  
  tkgrid(tklabel(win1$env$metrology, text="      "), column=0, row=23)
}
