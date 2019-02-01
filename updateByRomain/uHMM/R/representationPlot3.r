.representationPlot3<-function(rawMoments, rawData, dataCorrige, tm, title)
{
  
  varNames <- colnames(rawData)[-which(colnames(rawData)==c("Dates","Hours"))]
  
  #La fenetre
  plotWindow <- tktoplevel()
  tktitle(plotWindow) <- title
  
  #Volet a gauche
  selectionFrame<-tkwidget(plotWindow,"labelframe",borderwidth = 0)
  tkgrid(selectionFrame,column=0, row=0)
  
  #Selection des variable
  varList<-tk2listbox(selectionFrame, selectmode="single")
  tkconfigure(varList,height=7, background="white")
  for(i in 1:length(varNames))
  {
    tkinsert(varList,"end",varNames[i])
  }
  tkselection.set(varList, 0) 
  tkgrid(varList, row=1,column=0)
  
  #Bouton d'affichage
  displayButton<-tk2button(selectionFrame,text=tm$displayButtonLabel)
  displayButton.fun<-function()
  {
    
    if (tclvalue(tkcurselection(varList))!="")
    {
      
      #Recherche du parametre selectionne
      selection<-as.numeric(tkcurselection(varList))+1
      tempSelect<-varNames[selection]
      
      affichage1.fun<-function()
      {
        plot(rawData[,tempSelect],ylab="Level",xlab="Date",main=tempSelect)
      }
      myPlot1<-tkrplot(plotWindow,hscale=2.5,affichage1.fun)
      tkgrid(myPlot1,row=0,column=1)
      tkconfigure(myPlot1, bg="white")
      
      affichage2.fun<-function()
      {
        plot(dataCorrige[,tempSelect],ylab="Level",xlab="Date",main=tempSelect)
      }
      myPlot2<-tkrplot(plotWindow,hscale=2.5,affichage2.fun)
      tkgrid(myPlot2,row=1,column=1)
      tkconfigure(myPlot2, bg="white")
      
      
    }
    else
    { 
      #On avertit l'utilisateur qu'aucun parametre n'est selectionne
      tkmessageBox(message=tm$noParamMsg,type="ok",icon="info", title=tm$warningLabel)
    }
    
  }
  tkconfigure(displayButton,command=displayButton.fun)
  tkgrid(displayButton,row=2,column=0)
  
}