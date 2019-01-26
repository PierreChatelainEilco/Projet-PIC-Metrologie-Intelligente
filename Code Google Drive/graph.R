library(tkrplot)

affichage.fun.example <- function()
{
  x <- -100:100
  plot(x,x)
  points(x,y=x*x,col="red")
}

tkAfficherGraphique <- function(title, affichage.fun, hscale = 1.5, vscale=1.5)
{
  win <- tktoplevel()
  tktitle(win)<-title
  monGraph <- tkrplot(win, fun=affichage.fun, hscale = hscale, vscale = vscale)
  tkpack(monGraph)
}

tkAfficherTableau <- function(tab, title, method=names)
{
  tt <- tktoplevel()
  for(i in 1:length(tab))
  {
    label <- tklabel(tt, text = paste(toString(method(tab)[i]),":",toString(tab[i])))
    tkpack(label)
  }
    
}



