# Code for SemNeT
server <- function(input, output, session)
{
  # Determine the number of cores
  ## Random Networks
  output$cores_rand <- renderUI({
    
    core_rand <<- seq(1,parallel::detectCores()-1,1)
    names(core_rand) <- paste(core_rand)
    
    selectInput("cores_rand", label = "Number of processing cores",
                choices = core_rand,
                selected = ceiling(length(core_rand) / 2)
    )
  })
  
  ## Bootstrap Partial Networks
  output$cores_boot <- renderUI({
    
    core_boot <<- seq(1,parallel::detectCores()-1,1)
    names(core_boot) <- paste(core_boot)
    
    selectInput("cores_boot", label = "Number of processing cores",
                choices = core_boot,
                selected = ceiling(length(core_boot) / 2)
    )
  })
  
  # Semantic Networks panel
  observeEvent(input$run_est,
               {
                 # Let user know
                 showNotification("Estimating networks...")
                 
                 if(!is.null(input$data))
                 {
                   # Load preprocessed data
                   dat <<- SemNetCleaner::read.data(input$data$datapath)
                   
                   # Load group data
                   group <<- SemNetCleaner::read.data(input$group$datapath)
                 }else{
                   # Load data from SemNetCleaner package
                   dat <<- SemNeT::open.binary
                   
                   # Load group data from SemNetCleaner package
                   group <<- ifelse(SemNetCleaner::open.animals[,1] == 1, "Low", "High")
                 }
                 
                 # Organize group data
                 ## Identify unique groups
                 group <<- unlist(group)
                 uniq <<- unique(group)
                 
                 ## Store binary groups
                 for(i in 1:length(uniq))
                 {assign(paste(uniq[i]),
                         SemNetCleaner::finalize(dat[which(group == uniq[i]),], minCase = as.numeric(input$minCase)),
                         envir = globalenv())}
                 
                 ## Equate groups
                 eq <<- equateShiny(mget(paste(uniq), envir = globalenv()))
                 
                 ## Compute associations
                 assoc <<- lapply(eq, SemNeT::similarity, method = tolower(input$assoc))
                 
                 ## Estimate networks
                 nets <<- lapply(assoc, function(x){NetworkToolbox::TMFG(x)$A})
                 
                 ## Compute network measures
                 meas <<- lapply(nets, SemNeT::semnetmeas)
                 
                 ## Organized output
                 meas.mat <<- sapply(meas, c)
                 
                 ## Generate plot
                 plots <<- compare.netShiny(nets, config = "spring", weighted = FALSE)
                 
                 ## Render semantic networks plot
                 output$viz <- renderPlot({
                   
                   ### Manipulate Shiny plot window
                   if(length(plots$datalist) == 2)
                   {layout(t(1:2))
                   }else if(length(plots$datalist) > 2)
                   {
                     #Find square root
                     len <- floor(sqrt(length(plots$datalist)))
                     
                     #Remainder
                     remain <- length(plots$datalist)%%len
                     
                     #Change layout accordingly
                     layout(t(matrix(1:(length(plots$datalist)+remain),ncol=len)))
                   }
                   
                   ### Generate plot
                   compare.netplotShiny(plots)
                 })
                 
                 ## Render network measures table
                 output$measures <- renderTable(meas.mat, rownames = TRUE,
                                                caption = "Network Measures",
                                                caption.placement = getOption("xtable.caption.placement", "top"))
                 
               }
  )
  
  # Random Networks panel
  observeEvent(input$run_rand,
               {
                 # Let user know
                 showNotification("Computing statistics...")
                 
                 # Print waiting message
                 # FOR R PACKAGE
                 shinyalert::shinyalert(title = "Running...",
                                         text = "Check R Console for the Random Network Analyses Progress",
                                         type = "info")
                 
                 # Run random networks
                 rand_res <- reactive({
                   
                   randres <<- randnet.testShiny(nets, iter = as.numeric(input$iters_rand), cores = as.numeric(input$cores_rand))
                   
                   # Convert into matrix
                   for(i in 1:length(randres))
                   {
                     if(i == 1)
                     {randresmat <- randres[[i]]
                     }else{randresmat <- cbind(randresmat, randres[[i]])}
                   }
                   
                   return(randresmat)
                   
                 })
                 
                 # Render random networks table
                 output$randnet <- renderTable({rand_res()}, rownames = TRUE,
                                               caption = "Random Network Results",
                                               caption.placement = getOption("xtable.caption.placement", "top")
                 )
                 
               }
  )
  
  # Partial Bootstrap Networks panel
  observeEvent(input$run_boot,
               {
                 # Let user know
                 showNotification("Computing statistics...")
                 
                 # Print waiting message
                 # FOR R PACKAGE
                 shinyalert::shinyalert(title = "Running...",
                                        text = "Check R Console for the Partial Bootstrap Network Analyses Progress",
                                        type = "info")
                 
                 # Run partial bootstrap networks
                 part_boot <- reactive({
                   
                   ## Obtain percentages
                   percents <<- as.numeric(input$percent)
                   
                   ## Run partial bootstrap networks
                   for(i in 1:length(percents))
                   {
                     assign(paste(percents[i]),
                            partbootShiny(eq,
                                                   percent = percents[i],
                                                   sim = tolower(input$assoc),
                                                   weighted = FALSE,
                                                   iter = as.numeric(input$iters_boot),
                                                   cores = as.numeric(input$cores_boot)),
                            envir = globalenv())
                   }
                   
                   return(list(mget(paste(percents), envir = globalenv())))
                 })
                 
                 # Render tables
                 if(length(eq) == 2)
                 {
                   ## Average Shortest Path Length
                   output$aspl <- renderTable({
                     res_boot <<- part_boot()
                     
                     partboot.testShiny(unlist(res_boot, recursive = FALSE))$ASPL
                   }, rownames = TRUE,
                   caption = "Average Shortest Path Lengths (ASPL)",
                   caption.placement = getOption("xtable.caption.placement", "top")
                   )
                   
                   ## Clustering Coefficient
                   output$cc <- renderTable({
                     partboot.testShiny(unlist(res_boot, recursive = FALSE))$CC
                   }, rownames = TRUE,
                   caption = "Clustering Coefficient (CC)",
                   caption.placement = getOption("xtable.caption.placement", "top")
                   )
                   
                   ## Modularity
                   output$q <- renderTable({
                     partboot.testShiny(unlist(res_boot, recursive = FALSE))$Q
                   }, rownames = TRUE,
                   caption = "Modularity",
                   caption.placement = getOption("xtable.caption.placement", "top")
                   )
                 }
               }
  )
  
  # Plots panel
  observeEvent(input$run_plot,
               {
                 # Let user know
                 showNotification("Generating plots...")
                 
                 # Generate plots
                 pbplot <<- partbootplotShiny(unlist(res_boot, recursive = FALSE))
                 
                 ## Average Shortest Path Length
                 output$asplPlot <- renderPlot({pbplot$aspl})
                 
                 ## Clustering Coefficient
                 output$ccPlot <- renderPlot({plot(pbplot$cc)})
                 
                 ## Modularity
                 output$qPlot <- renderPlot({plot(pbplot$q)})
                 
               }
               
  )
  
}