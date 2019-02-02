.representationPlot2 <- function(rawMoments, rawData, dataCorrige, tm, title)
{
  #Les varaibles
  varNames <- colnames(rawData)[-which(colnames(rawData)==c("Dates","Hours"))]
  
  #Recuperation des dates minimum et maximum
  firstObsDate<-min(rawMoments)
  lastObsDate<-max(rawMoments)
  
  minYear=min(as.numeric(substring(rawData[,"Dates"],1,4)))
  maxYear=max(as.numeric(substring(rawData[,"Dates"],1,4)))
  
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
  
  #La fenetre
  plotWindow <- tktoplevel()
  tktitle(plotWindow) <- title
  
  #Volet a gauche
  selectionFrame<-tkwidget(plotWindow,"labelframe",borderwidth=0)
  tkgrid(selectionFrame, column=0, row=0)
  
    #Selection de la période
    dateFrame <- tkwidget(selectionFrame,"labelframe",text="Periode")
    tkgrid(dateFrame , row=0, column=0,pady=c(20,20))
  
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
    displayButton.fun <- function()
    {
      if (tclvalue(tkcurselection(varList))!="")
      {
        #Recherche du parametre selectionne
        # selection<-as.numeric(tkcurselection(varList))+1
        # 
        # tempSelect<-varNames[selection]
        # 
        # Dmin<-paste(substring(tclvalue(tkget(laDateMin)),9,10),"/",substring(tclvalue(tkget(laDateMin)),6,7),"/",substring(tclvalue(tkget(laDateMin)),1,4),sep="")
        # Dmax<-paste(substring(tclvalue(tkget(laDateMax)),9,10),"/",substring(tclvalue(tkget(laDateMax)),6,7),"/",substring(tclvalue(tkget(laDateMax)),1,4),sep="")
        # 
        # periodMin<-as.numeric(chron(Dmin,tclvalue(tkget(lHeureMin)),format=c("d/m/y","h:m:s")))
        # periodMax<-as.numeric(chron(Dmax,tclvalue(tkget(lHeureMax)),format=c("d/m/y","h:m:s")))
        # 
        # periodIndex=(rawMoments>=periodMin & rawMoments<=periodMax);
        # period<-rawMoments[periodIndex]
  
        
        affichage.fun <- function()
        {
          #Méthode avec R seul
          
         #A=cbind(rawData[,tempSelect],dataCorrige[,tempSelect])
          A=cbind(1:120000)
          matplot(A)
          
          
          #plot(rawData[,tempSelect]~chron(period),ylab="Level",xlab="Date")
          #points(dataCorrige[,tempSelect]~chron(period),col="red",pch=3)
          #Error : Unable to save metafile to the clipboard
          
          #Méthode avec plotly
          #plot(dataCorrige[,tempSelect]~chron(period))
          #p<-plot_ly(dataCorrige,x=~tempSelect)
          #Pas compatible avec rplot mais compatible avec shiny
          
          #Méthode avec ggplot2
          #gg <- ggplot(dataCorrige,aes(x="Dates",y=tempSelect)) + geom_point()
          #print(gg)
          #Error : Unable to save metafile to the clipboard
        
        }
        
        myPlot<-tkrplot(plotWindow,hscale=2.5,vscale=2,affichage.fun)
        tkgrid(myPlot,row=0,column=1)
        tkconfigure(myPlot, bg="white")
      }
      else
      { 
        #On avertit l'utilisateur qu'aucun parametre n'est selectionne
        tkmessageBox(message=tm$noParamMsg,type="ok",icon="info", title=tm$warningLabel) 
      } 
    }
    tkconfigure(displayButton,command=displayButton.fun)
    tkgrid(displayButton,row=3,column=0)
  
}