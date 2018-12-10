#' @import tcltk tcltk2

library("tcltk2")
win1 <- tktoplevel()


tkwm.resizable(win1, FALSE, FALSE)
tkwm.geometry(win1, "700x500+300+100")
tkwm.title(win1, "Métrologie")

labelwin1<- tklabel(win1, text="Sélectionner un fichier
txt",fg="blue")

labelText <- tclVar("")
labelcheminDATA <- tklabel(win1, text=tclvalue(labelText),fg="black")

butopen<-tkbutton(win1,relief="ridge",activebackground="orange",
                  command=function()func.ouvrir(labelText),text="ouvrir",width=10)
q.but<-tkbutton(win1,text="Charger",relief="ridge",foreground="white",
                background="grey60",command=function()exe(wind1),width=10)

func.ouvrir<-function(x){
  chemintemp<-tkgetOpenFile(initialdir="C:\\")
  tclvalue(x) <- tclvalue(chemintemp)
  tkconfigure(labelcheminDATA,textvariable=x)
}
exe<-function(x){
  tkdestroy(x)
}

tkgrid(labelwin1,sticky="w",columnspan=2,row=0,column=0)
tkgrid(butopen, sticky="w",row=1,column=0)
tkgrid(labelcheminDATA,sticky="w",row=1,column=1)
tkgrid(q.but,sticky="w",row=2, column=0)

Mydata <- read.table(file=tclvalue(labelText),header=TRUE)
Varname <- colnames(mydata)

