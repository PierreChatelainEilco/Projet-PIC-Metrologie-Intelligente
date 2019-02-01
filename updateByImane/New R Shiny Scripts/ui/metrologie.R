tabPanel(title ="M\UE9trologie",value="onglet2",
         
         #input
         sidebarPanel
         (
           # Input: Select a file ----
           
           fileInput("file", "Choose CSV File",
                     multiple = TRUE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           # # Input: Checkbox if file has header ----
           # checkboxInput("header", "Header", TRUE),
           # 
           # Input: Select separator ----
           radioButtons("sep", "Separator",
                        choices = c(Semicolon = ";",
                                    Comma = ",",
                                    Tab = "\t"),
                        selected = ","),
           # Horizontal line ----
           tags$hr(),
           
           
           # Input: Select what to display
           selectInput("dataset","Data:",
                       choices =list(fichierDonnees = "inFile", fichierRotation="inFile2", fichierGammeCapt="inFile3"), selected=NULL),
           selectInput("variable","Variable:", choices = NULL),
           selectInput("group","Group:", choices = NULL),
           selectInput("plot.type","Plot Type:",
                       list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")
           ),
           checkboxInput("show.points", "show points", TRUE)
         ),
         
         # output
         mainPanel(
           tableOutput("file"),
           
           h3(textOutput("caption")),
           #h3(htmlOutput("caption")),
           uiOutput("plot"), # depends on input
           basicPage(
                  actionButton("popup", "Correction")
                )
         )
)  