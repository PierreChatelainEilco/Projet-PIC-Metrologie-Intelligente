#' @title Variable tab
#' @description This function generates the variable selection tab of the \code{\link{uHMMinterface}}, in which the user can select model variables and the training period.
#' @param userType character : either "default" or "expert"
#' @param leftMargin left magin size of interface tabs.
#' @param tm a one row dataframe containing text to display in the interface.
#' @param console frame of the uHMM interface in which messages should be displayed. 
#' @param graphicFrame frame of the uHMM interface in which graphics should be dispayed.
#' @param win1 frame of the uHMM interface containing main tabs.
#' @param uHMMenv environment in which data and intermediate results are stored.
#' @import tcltk tcltk2
#' @importFrom corrplot corrplot
#' @importFrom chron chron
#' @export

.variableTab<-function(userType,tm,leftMargin=30,
                      console,graphicFrame,win1,uHMMenv){
 
 # tkfocus(win1$env$variables)
  
### Parameter selection frame

  parameterFrame <- tkwidget(win1$env$variables,"labelframe",text=tm$titleParamList)
  tkgrid(parameterFrame , row=2,padx=c(leftMargin,0),pady=c(30,20),sticky="w")
  
  
  #Fenetre avec l'ensemble des parametres
  dataVarList<-tk2listbox(parameterFrame, selectmode="extended",
                          activestyle = "dotbox",height=8,width=27, 
                          background="white")
  paramNames<-c(tm$allParamLabel,colnames(uHMMenv$rawData)[3:length(uHMMenv$rawData)]);
  for(i in 1:length(paramNames)){
    tkinsert(dataVarList,"end",paramNames[i])
  }
  
  tkselection.set(dataVarList, 0)  
  tkgrid(dataVarList, row=2,column=0)
  tkgrid(tk2label(parameterFrame,text=paste(tm$datasetVariableListTitle,":",sep="")), row=1,column=0,pady=c(10,0),sticky="w")
  
  #Fenetre avec les parametres selectionnes
  modelVarList<-tk2listbox(parameterFrame, selectmode = "extended", 
                           activestyle = "dotbox", height = 8, width = 27, 
                           background = "white")
  paramNames2<-"";
  for(i in 1:length(paramNames2)){
    
    tkinsert(modelVarList,"end",paramNames2[i])
    
  }
  
  tkgrid(modelVarList, row=2,column=3)
  tkgrid(tk2label(parameterFrame,text=paste(tm$modelVariableListTitle,":",sep="")), row=1,column=3,pady=c(10,0),sticky="w")
  
  
  
  #Fleche de passage de l'un a l'autre
  if(!exists("selectedNames",where=uHMMenv)){assign("selectedNames",NULL,envir=uHMMenv)}
  
  arrowFrame <- tkwidget(parameterFrame,"labelframe", relief = "groove",borderwidth = 0)
  tkgrid(arrowFrame , column=1, row=2, sticky="w",padx=c(30,30)) 
  
  addButton <- tk2button(arrowFrame ,text="=>", command=function(){     # bouton de selection  image=imgArrowright
    
    #Verification qu'une valeur est selectionnee
    if (tclvalue(tkcurselection(dataVarList))!="") {
      
      #Recherche du parametre selectionne
      selection<-as.numeric(tkcurselection(dataVarList))+1
      
      #Enregistrement de celui-ci dans un tampon
      if(any(selection==1)){
        
        selectedNamesTemp<-colnames(uHMMenv$rawData)[-c(which(colnames(uHMMenv$rawData)=="Dates"),which(colnames(uHMMenv$rawData)=="Hours"))]
        tkinsert(console,"1.0",paste(nrow(uHMMenv$rawData)-sum(apply(is.na(uHMMenv$rawData),1,any)),tm$overallNA,"\n",sep=""))
        
      } else{
        
        selectedNamesTemp<-paramNames[selection]
        for (i in selectedNamesTemp)
        tkinsert(console,"1.0",paste(sum(is.na(uHMMenv$rawData[,i])),tm$VstepNA,i,"\n",sep=""))
      }
      
      #On complete la liste precedente et on supprime les doublons
      assign("selectedNames",unique(c(uHMMenv$selectedNames,selectedNamesTemp)),envir=uHMMenv)
      
      #On efface l'affiche de la liste d'arrivee
      for(i in 1:length(uHMMenv$selectedNames)){
        tkdelete(modelVarList,"end")
      }
      #On affiche la nouvelle liste de parametre
      for(i in 1:length(uHMMenv$selectedNames)){
        
        tkinsert(modelVarList,"end",uHMMenv$selectedNames[i])
        
      }
      
    } else { tkmessageBox(message=tm$noParamMsg,type="ok",icon="info", title=tm$warningLabel) } #On avertit l'utilisateur qu'aucun parametre n'est selectionne
  }) 
  
  tkgrid(addButton) #,row=2,column=1)
  
  tkgrid(tklabel(arrowFrame, text="                  "))
  
  removeButton <- tk2button(arrowFrame ,text="<=")
  removeButton.fun <- function(){    # bouton de deselection    image=imgArrowleft
    
    #Verification qu'une valeur est selectionnee
    if (tclvalue(tkcurselection(modelVarList))!="") {
      
      #Recherche du parametre selectionne
      selection<-as.numeric(tkcurselection(modelVarList))+1
      
      #On supprime le parametre selectionne
      uHMMenv$selectedNames<-uHMMenv$selectedNames[-selection]
      
      if(length(uHMMenv$selectedNames)>=1){
        
        assign("selectedNames",uHMMenv$selectedNames,envir=uHMMenv)
        
        #On efface l'affiche de la liste d'arrivee
        for(i in 1:(length(uHMMenv$selectedNames)+1)){
          tkdelete(modelVarList,"end")
        }
        
        #On affiche la nouvelle liste de parametre
        for(i in 1:length(uHMMenv$selectedNames)){
          
          tkinsert(modelVarList,"end",uHMMenv$selectedNames[i])
          
        }
        
      }else{
        
        #On efface entierement l'affiche de la liste et on met la liste a NULL
        tkdelete(modelVarList,"end")
        assign("selectedNames",NULL,envir=uHMMenv)
        paramNames2<-"";
        for(k in 1:length(paramNames2)){
          
          tkinsert(modelVarList,"end",paramNames2[k])
          
        }
        
      }
      
      
    } else { tkmessageBox(message=tm$noParamMsg,type="ok",icon="info", title=tm$warningLabel) } #On avertit l'utilisateur qu'aucun parametre n'est selectionne
  }
  tkconfigure(removeButton,command=removeButton.fun)
  tkgrid(removeButton)
  
  

  
  # Definition des boutons
  
  exploratoryFrame <- tkwidget(win1$env$variables,"labelframe",text=tm$titleExploratoryFrame,borderwidth = 0)
  tkgrid(exploratoryFrame , row=6,padx=c(leftMargin,0),pady=10,sticky="w")
  
  
  #Bouton de plot
  plotButton<-tk2button(exploratoryFrame,text=tm$plotButtonLabel,image="loupe",compound = "left")
  plotButton.fun<-function(){
    
    if(length(uHMMenv$selectedNames)!=0){  #Verification que des parametres ont ete selectionnes
      
      Dmin<-paste(substring(tclvalue(tkget(laDateMin)),9,10),"/",substring(tclvalue(tkget(laDateMin)),6,7),"/",substring(tclvalue(tkget(laDateMin)),1,4),sep="")
      Dmax<-paste(substring(tclvalue(tkget(laDateMax)),9,10),"/",substring(tclvalue(tkget(laDateMax)),6,7),"/",substring(tclvalue(tkget(laDateMax)),1,4),sep="")
      
      periodMin<-as.numeric(chron(Dmin,tclvalue(tkget(lHeureMin)),format=c("d/m/y","h:m:s")))
      periodMax<-as.numeric(chron(Dmax,tclvalue(tkget(lHeureMax)),format=c("d/m/y","h:m:s")))
      
      periodIndex=(uHMMenv$rawMoments>=periodMin & uHMMenv$rawMoments<=periodMax);
      period<-uHMMenv$rawMoments[periodIndex]
      dataToPlot=uHMMenv$rawData[periodIndex,]
      
  
      .representationPlot(data=dataToPlot,period=period,tm=tm,varNames=uHMMenv$selectedNames)

    } else{
      
      tkmessageBox(message=tm$noParamMsg,type="ok",icon="info", title=tm$warningLabel)
      
    }
  }
  tkconfigure(plotButton,command=plotButton.fun)
  tkgrid(plotButton,row=1,column=1,padx=c(25,10),pady=c(5,5))
  
  
  
  #Bouton de boxplot
  boxplotButton<-tk2button(exploratoryFrame,text=tm$boxplotButtonLabel,image="loupe",compound = "left")
  boxplotButton.fun<-function(){
    
    if(length(uHMMenv$selectedNames)!=0){  #Verification que des parametres ont ete selectionnes
      
      Dmin<-paste(substring(tclvalue(tkget(laDateMin)),9,10),"/",substring(tclvalue(tkget(laDateMin)),6,7),"/",substring(tclvalue(tkget(laDateMin)),1,4),sep="")
      Dmax<-paste(substring(tclvalue(tkget(laDateMax)),9,10),"/",substring(tclvalue(tkget(laDateMax)),6,7),"/",substring(tclvalue(tkget(laDateMax)),1,4),sep="")
      
      periodMin<-as.numeric(chron(Dmin,tclvalue(tkget(lHeureMin)),format=c("d/m/y","h:m:s")))
      periodMax<-as.numeric(chron(Dmax,tclvalue(tkget(lHeureMax)),format=c("d/m/y","h:m:s")))
      
      periodIndex=(uHMMenv$rawMoments>=periodMin & uHMMenv$rawMoments<=periodMax);
      period<-uHMMenv$rawMoments[periodIndex]
      dataToPlot=uHMMenv$rawData[periodIndex,]
      
      
      
      .boxplotWindow(data=dataToPlot,varNames=uHMMenv$selectedNames,tm=tm)
      
    }else{
      
      tkmessageBox(message=tm$noParamMsg,type="ok",icon="info", title=tm$warningLabel)
      
    }
  }
  tkconfigure(boxplotButton, command=boxplotButton.fun)
  tkgrid(boxplotButton,row=1,column=2,padx=10,pady=c(5,5))
  
  
  
  
  
  #Bouton correlation
  corrButton<-tk2button(exploratoryFrame,text=tm$correlationButtonLabel,image="loupe",compound = "left")
  corrButton.fun<-function(){
    
      M<-cor(uHMMenv$rawData[,-which(colnames(uHMMenv$rawData)=="Hours"| colnames(uHMMenv$rawData)=="Dates")],use="pairwise.complete.obs")
      
      cor3win<-tktoplevel()
      tktitle(cor3win)<-tm$corrplotTitle
      cor3<-tkrplot(cor3win,hscale=2.5,vscale=2,function()corrplot(M, method="number"))
      tkgrid(cor3,row=0,column=1,sticky="w")
    
  }
  tkconfigure(corrButton,command=corrButton.fun)
  tkgrid(corrButton,row=1,column=3,padx=10,pady=c(5,5))
  
  
  
  
  #Bouton de l'ACP
  PCAbutton<-tk2button(exploratoryFrame,text=tm$pcaLabel,image="loupe",compound = "left")
  PCAbutton.fun<-function(){
    
    if(length(uHMMenv$selectedNames)!=0){  #Verification que des parametres ont ete selectionnes
      if(length(uHMMenv$selectedNames)>=2){
        
        Dmin<-paste(substring(tclvalue(tkget(laDateMin)),9,10),"/",substring(tclvalue(tkget(laDateMin)),6,7),"/",substring(tclvalue(tkget(laDateMin)),1,4),sep="")
        Dmax<-paste(substring(tclvalue(tkget(laDateMax)),9,10),"/",substring(tclvalue(tkget(laDateMax)),6,7),"/",substring(tclvalue(tkget(laDateMax)),1,4),sep="")
        
        periodMin<-as.numeric(chron(Dmin,tclvalue(tkget(lHeureMin)),format=c("d/m/y","h:m:s")))
        periodMax<-as.numeric(chron(Dmax,tclvalue(tkget(lHeureMax)),format=c("d/m/y","h:m:s")))
        
        periodIndex=(uHMMenv$rawMoments>=periodMin & uHMMenv$rawMoments<=periodMax);
        period<-uHMMenv$rawMoments[periodIndex]
        dataToPlot=uHMMenv$rawData[periodIndex,uHMMenv$selectedNames]
        
        .PCAwindow(data=dataToPlot,tm=tm)
        
      }else{
        tkmessageBox(message=tm$twoParamsWarning,type="ok",icon="info", title=tm$warningLabel)
      }
      
    } else{
      
      tkmessageBox(message=tm$noParamMsg,type="ok",icon="info", title=tm$warningLabel)
      
    }
    
    
  }
  tkconfigure(PCAbutton,command=PCAbutton.fun)
  tkgrid(PCAbutton,row=1,column=4,padx=c(10,25),pady=c(5,5))
  
  

  ### Choix de la periode d'echantillonnage

  dateFrame <- tkwidget(win1$env$variables,"labelframe",text=paste(tm$titlePeriodFrame,":",sep=""))
  tkgrid(dateFrame , row=7,padx=c(leftMargin,0),pady=c(20,20),sticky="w")
  
  #Recuperation des dates minimum et maximum
  firstObsDate<-min(uHMMenv$rawMoments)
  lastObsDate<-max(uHMMenv$rawMoments)
  
  minYear=min(as.numeric(substring(uHMMenv$rawData[,"Dates"],1,4)))
  maxYear=max(as.numeric(substring(uHMMenv$rawData[,"Dates"],1,4)))
  
  minMonth=min(as.numeric(format(chron(firstObsDate),"%m")))
  maxMonth=max(as.numeric(format(chron(lastObsDate),"%m")))
  
  minDay=min(as.numeric(format(chron(firstObsDate),"%d")))
  maxDay=max(as.numeric(format(chron(lastObsDate),"%d")))
  
  #Adaptation de l'ecriture des jours et mois s'ils sont inferieurs a 10
  #ex: "1" devient "01"; "2" devient "02", etc...
  if(minMonth<10){
    minMonth<-paste("0",minMonth,sep="")
  }
  if(maxMonth<10){
    maxMonth<-paste("0",maxMonth,sep="")
  }
  if(minDay<10){
    minDay<-paste("0",minDay,sep="")
  }
  if(maxDay<10){
    maxDay<-paste("0",maxDay,sep="")
  }
  
  #Recuperation de l'heure minimum et maximum
  Hmin<-substring(chron(firstObsDate),11,18)
  Hmax<-substring(chron(lastObsDate),11,18)
  
  #Affichage de la date minimum
  minDateText<-tklabel(dateFrame,text=tm$fromLabel)
  tkgrid(minDateText,row=10,column=0,padx=c(20,20))
  
  laDateMin<-tkentry(dateFrame, width=10, textvariable=tclVar(paste(minYear,"-",minMonth,"-",minDay,sep="")),background = "#ffffff");
  tkgrid(laDateMin,row=10,column=1)
  
  lHeureMin<-tkentry(dateFrame, width=7, textvariable=tclVar(Hmin),background = "#ffffff");
  tkgrid(lHeureMin,row=10,column=2)
  
  #Affichage de la date maximum
  maxDateText<-tklabel(dateFrame,text=tm$toLabel)
  tkgrid(maxDateText,row=11,column=0)
  
  laDateMax<-tkentry(dateFrame, width=10, textvariable=tclVar(paste(maxYear,"-",maxMonth,"-",maxDay,sep="")),background = "#ffffff");
  tkgrid(laDateMax,row=11,column=1) 
  
  lHeureMax<-tkentry(dateFrame, width=7, textvariable=tclVar(Hmax),background = "#ffffff");
  tkgrid(lHeureMax,row=11,column=2)
  
  
  
  
  boutonValider<-tk2button(win1$env$variables,text=tm$runLabel,image="run",compound = "left")
  boutonValider.fun<-function(){
    
    if(length(uHMMenv$selectedNames)!=0){  #Verification que des parametres ont ete selectionnes
      if(length(uHMMenv$selectedNames)>=2){
        
        #Recuperation des dates minimum et maximum selectionnees par l'utilisation
        Dmin<-paste(substring(tclvalue(tkget(laDateMin)),9,10),"/",substring(tclvalue(tkget(laDateMin)),6,7),"/",substring(tclvalue(tkget(laDateMin)),1,4),sep="")
        Dmax<-paste(substring(tclvalue(tkget(laDateMax)),9,10),"/",substring(tclvalue(tkget(laDateMax)),6,7),"/",substring(tclvalue(tkget(laDateMax)),1,4),sep="")
        
        periodMin<-as.numeric(chron(Dmin,tclvalue(tkget(lHeureMin)),format=c("d/m/y","h:m:s")))
        periodMax<-as.numeric(chron(Dmax,tclvalue(tkget(lHeureMax)),format=c("d/m/y","h:m:s")))
        
        periodIndex=(uHMMenv$rawMoments>=periodMin & uHMMenv$rawMoments<=periodMax);

        #Prise en comptes de ces dates pour selectionner la portion de donnees a retenir
        assign("trainingRows",periodIndex,envir=uHMMenv)
        assign("trainingPeriod",uHMMenv$rawMoments[periodIndex],envir=uHMMenv)
        assign("trainingSet",uHMMenv$rawData[periodIndex,uHMMenv$selectedNames],envir=uHMMenv) 
       # assign("rawData",uHMMenv$rawData[periodIndex,],envir=uHMMenv)			
        
        .savePlot(data=uHMMenv$trainingSet,period=uHMMenv$trainingPeriod,tm=tm,output=uHMMenv$diagramsOutput)
        tk2notetab.select(win1$env$nb, tm$classificationTabLabel)
        
        # Display in console
        atLeast1NA<-apply(is.na(uHMMenv$trainingSet),1,any)
        
        varNames<-uHMMenv$selectedNames[1]
        for (i in 2:length(uHMMenv$selectedNames)){varNames<-paste(varNames,uHMMenv$selectedNames[i],sep=", ")}
        
        tkinsert(console,"1.0",paste(tm$selectedVariables,varNames,"\n",
                                     tm$selectedPeriodFrom,chron(periodMin),tm$toMinLabel,chron(periodMax),"\n",
                                     tm$completeObservations, nrow(uHMMenv$trainingSet)-sum(atLeast1NA),"(",sum(atLeast1NA),tm$CstepNAnumber,
                                     "\n\n---------------------------------------\n",sep=""))
        
        if (userType=="default"){
            .classificationTab_standard(tm=tm,leftMargin=leftMargin,
                                       console=console,graphicFrame=graphicFrame,win1=win1,
                                       uHMMenv=uHMMenv
            )
        }
      
        if (userType=="expert"){
            .classificationTab_expert(tm=tm,leftMargin=leftMargin,
                                     console=console,graphicFrame=graphicFrame,win1=win1,
                                     uHMMenv=uHMMenv
            )
        }
        
        
      }else{
        tkmessageBox(message=tm$twoParamsWarning,type="ok",icon="info", title=tm$warningLabel)
      }
      
    } else{
      
      tkmessageBox(message=tm$noParamMsg,type="ok",icon="info", title=tm$warningLabel)
      
    }
    
  }
  tkconfigure(boutonValider,command=boutonValider.fun)
  
  tkgrid(tklabel(win1$env$variables, text="      "), column=0, row=17)
  
  tkgrid(boutonValider,row=18,sticky="w",padx=c(leftMargin,0))
}