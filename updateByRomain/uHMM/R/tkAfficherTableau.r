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