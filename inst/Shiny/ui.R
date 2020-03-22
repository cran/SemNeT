library(shiny)

# Interface for SemNeT
ui <- (
  
  navbarPage("Semantic Network Analysis with SemNeT",
             
             # Network Estimation Panel
             tabPanel(
               "Network Estimation",
               
               # Input
               sidebarPanel(
                 
                 tags$div(fileInput("data", label = "Upload preprocessed binary response matrix",
                                    accept = c(".rds", ".csv", ".xls", ".xlsx", ".txt")), id = "data"),
                 
                 tags$div(fileInput("group", label = "Upload group variable"), id = "group"),
                 
                 selectInput("assoc", "Association Measure", c("Angular", "Cosine",
                                                               "Euclidean Distance",
                                                               "Faith", "Jaccard Index",
                                                               "Pearson's Correlation",
                                                               "RR"),
                             selected = "Cosine"),
                 
                 tags$div(textInput("minCase", label = "Minimum Cases", value = "2"), id = "minCase"),
                 
                 actionButton("run_est", label = "Run Semantic Networks")
                 
               ),
               
               # Output
               mainPanel(
                 plotOutput("viz"),
                 tableOutput("measures")
               )
             ),
             
             # Random Network Analyses Panel
             tabPanel(
               "Random Network Analyses",
               
               # Input
               sidebarPanel(
                 
                 numericInput("iters_rand", label = "Number of Iterations", value = 1000, min = 1),
                 
                 uiOutput("cores_rand"),
                 
                 actionButton("run_rand", label = "Run Random Network Analyses")
                 
               ),
               
               # Output
               mainPanel(
                 tableOutput("randnet")
               )
             ),
             
             # Partial Bootstrap Network Analyses Panel
             tabPanel(
               "Partial Bootstrap Network Analyses",
               
               # Input
               sidebarPanel(
                 
                 numericInput("iters_boot", label = "Number of Iterations", value = 1000, min = 1),
                 
                 uiOutput("cores_boot"),
                 
                 checkboxGroupInput("percent", label = "Percentage of Nodes Remaining",
                                    choiceNames = paste(seq(50,90,10),"%",sep=""),
                                    choiceValues = seq(.50,.90,.10), inline = TRUE),
                 
                 actionButton("run_boot", label = "Run Partial Bootstrap Analyses")
                 
               ),
               
               # Output
               mainPanel(
                 tableOutput("aspl"),
                 tableOutput("cc"),
                 tableOutput("q")
               )
             ),
             
             # Plot Bootstrap Analyses Panel
             tabPanel(
               "Plot Bootstrap Analyses",
               
               # Input
               sidebarPanel(
                 
                 actionButton("run_plot", label = "Generate Plots")
                 
               ),
               
               # Output
               mainPanel(
                 plotOutput("asplPlot"),
                 plotOutput("ccPlot"),
                 plotOutput("qPlot")
               )
             ),
             
             # Use shinyalert
             shinyalert::useShinyalert()
  )
)