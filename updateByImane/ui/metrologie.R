tabPanel(title ="M\UE9trologie",value="onglet2",
         
           sidebarPanel(
             tableOutput("table2")
          # # Input: Select separator ----
          #  radioButtons("sep", "Separator",
          #               choices = c(Semicolon = ";",
          #                           Comma = ",",
          #                           Tab = "\t"),
          #               selected = ","),
          #  # Horizontal line ----
          #  tags$hr(),
          # 
          # 
          #  # Input: Select what to display
          #  #selectInput("dataset",tableOutput("table2"), choices = NULL),
          # 
          #  selectInput("variable","Variable:", choices = NULL),
          #  selectInput("group","Group:", choices = NULL),
          #  selectInput("plot.type","Plot Type:",
          #              list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")
          #  )
           ),
         mainPanel(
           
           h3(textOutput("caption")),
           
           uiOutput("plot"), # depends on input 
           
           basicPage(
             actionButton("popup", "Correction")
           )
         )
         )