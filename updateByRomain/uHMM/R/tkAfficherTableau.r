.tkAfficherTableau <- function(tab,title,family="Arial")
{
  tt <- tktoplevel()
  fontHeading <- tkfont.create(family = family, size = 24,
                               weight = "bold", slant = "italic")
  
  tktitle(tt) <- title
  label<-  tklabel(tt,text = title,font = fontHeading)
  tkpack(label)
  for(i in 1:length(tab))
  {
    label <- tklabel(tt, text = paste(toString(names(tab)[i]),":",toString(tab[i])))
    tkpack(label)
  }
  
}

.tkAfficherTableau2 <- function(tab,nbval=0,title="NoTitle",family="Arial",messagehaut="",messagebas="")
{
  tt <- tktoplevel()
  fontHeading <- tkfont.create(family = family, size = 24,
                               weight = "bold", slant = "italic")
  
  tktitle(tt) <- title
  label<-  tklabel(tt,text = title,font = fontHeading)
  tkpack(label)
  
  label<-tklabel(tt, text = messagehaut)
  tkpack(label)
  
  for(i in 1:length(tab))
  {
    if(nbval>0)
    {
      texte<-paste(toString(names(tab)[i]),":",toString(tab[i]),"(",toString((tab[i]*100/nbval)-(tab[i]*100/nbval)%%0.01),"%)")
    }
    else
    {
      texte<-paste(toString(names(tab)[i]),":",toString(tab[i]))
    }
    label <- tklabel(tt, text = texte)
    tkpack(label)
  }
  
  
  label<-tklabel(tt, text = messagebas)
  tkpack(label)
}