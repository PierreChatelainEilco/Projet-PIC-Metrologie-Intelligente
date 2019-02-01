 #plotting theme for ggplot2
 .theme<- theme(
   axis.line = element_line(colour = 'gray', size = .75),
   panel.background = element_blank(),
   plot.background = element_blank()
 )

#update group and
#variables based on the data

 observe({
   if(!exists(input$dataset)) return() #make sure upload exists
   var.opts<-colnames(get(input$dataset))
   updateSelectInput(session, "variable", choices = var.opts)
   updateSelectInput(session, "group", choices = var.opts)

 })

output$caption<-renderText({
  switch(input$plot.type,
         "boxplot" 	= 	"Boxplot",
         "histogram" =	"Histogram",
         "density" 	=	"Density plot",
         "bar" 		=	"Bar graph")
})


output$plot <- renderUI({
  plotOutput("p")
})

#get data object
get_data<-reactive({

  if(!exists(input$dataset)) return() # if no upload

  check<-function(x){is.null(x) || x==""}
  if(check(input$dataset)) return()

  obj<-list(data=get(input$dataset),
            variable=input$variable,
            group=input$group
  )

  #require all to be set to proceed
  if(any(sapply(obj,check))) return()
  #make sure choices had a chance to update
  check<-function(obj){
    !all(c(obj$variable,obj$group) %in% colnames(obj$data))
  }

  if(check(obj)) return()


  obj

})

#plotting function using ggplot2
output$p <- renderPlot({

 plot.obj<-get_data()

 #conditions for plotting
   if(is.null(plot.obj)) return()
  
   #make sure variable and group have loaded
   if(plot.obj$variable == "" | plot.obj$group =="") return()
  
   #plot types
   plot.type<-switch(input$plot.type,
                     "boxplot" 	= geom_boxplot(),
                     "histogram" =	geom_histogram(alpha=0.5,position="identity"),
                     "density" 	=	geom_density(alpha=.75),
                     "bar" 		=	geom_bar(position="dodge")
   )


  if(input$plot.type=="boxplot")	{		#control for 1D or 2D graphs
    p<-ggplot(plot.obj$data,
              aes_string(
                x 		= plot.obj$group,
                y 		= plot.obj$variable,
                fill 	= plot.obj$group # let type determine plotting
              )
    ) + plot.type

    if(input$show.points==TRUE)
    {
      p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
    }

  } else {

    p<-ggplot(plot.obj$data,
              aes_string(
                x 		= plot.obj$variable,
                fill 	= plot.obj$group,
                group 	= plot.obj$group
                #color 	= as.factor(plot.obj$group)
              )
    ) + plot.type
  }

  p<-p+labs(
    fill 	= input$group,
    x 		= "",
    y 		= input$variable
  )  +
    .theme
  print(p)
})

# set uploaded file
fichierDonnees<-reactive({

  inFile <- input$file

  if (is.null(inFile))
    return(NULL)

  #could also store in a reactiveValues
  read.csv(inFile$datapath,
           #header = input$header,
           sep = input$sep)
})

observeEvent(input$file,{
  inFile<<-fichierDonnees()
})



#if( Valeurs manquantes/Valeurs aberrantes){ 
observeEvent(input$popup, {
   showModal(modalDialog(
    
    title = "alerte","Des anomalies ont \UE9t\UE9 d\UE9tect\UE9\U65s lors de la lecture des donn\UE9\U65s !",
    br(),
    p("Nombre de valeurs manquantes: $value1"),
    p("Nombre de valeurs aberrantes: $value2"),
    br(),
    p(em("Souhaitez-vous les corriger ?"), style = "color:blue"),
    footer = modalButton("Ignorer"), modalButton("Corriger")
  )
  )
})


