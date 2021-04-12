shinyServer(function(input, output, session){
  
  ### not clear where libraries should be initialized...Currently in multiple places
  ### one on github, the "run" file will check for those needed, install, and load
  #source("libsAndFiles.R",local=TRUE)
  ### just a couple of functions used in program.  Others could be added to it.

  #the parallel processing of leave one out needs to know the environment
  #to export functions to the cluster.
  serverEnv <- environment()
  
  # if reset button is pressed
  observeEvent(input$reset_button, {
    js$reset()
    updateTabsetPanel(session, "outputTabActive",selected = "Help")
  })
  
  # Begin with focus on help
  updateTabsetPanel(session, "outputTabActive",selected = "Help")
  #when rvs$setupComplete is 0, it means that the preview button
  #must be clicked to commit selections and view data plot
  #prior to analysis being run.  This will result in rvs$setupComplete
  #being set to 1.
  
  # Reactive vars:
  #   1.  setupComplete
  #       0 means not ready, as in, preview data has not been completed
  #       1 means the the preview button has been clicked, so data should be ready for analysis
  #   2.  SSD.complete
  #       0 after setupComplete = 1 (ready for analysis but not complete)
  #       1 when end of SSD code is reached.
  #   3.  units
  #       Simply holds the units input string
  #   4.  indata
  #       The input data, after processing (ie, data input to the analysis code)
  
  # Initialize reactives
  rvs <- reactiveValues()
  rvs$setupComplete <- 0
  rvs$SSD.complete <- 0
  rvs$dataImported <- 0
  rvs$dataChecked <- 0
  rvs$varsChecked <- 0
  rvs$SSDdata <- 0
  output$dataChecked <- reactive({rvs$dataChecked})
  outputOptions(output,"dataChecked",suspendWhenHidden = FALSE)
  output$setupComplete <- reactive({rvs$setupComplete})
  outputOptions(output,"setupComplete",suspendWhenHidden = FALSE)
  
  #create a reactive expression that will be check for going back to
  #"beginning", without a complete reset
  goBack <- reactive({
    list(
      input$pasteData
      #input$doGrays
      #input$doLegend
    )
  })
  #
  newVars <- reactive({
    list(
      input$sort_x,
      input$sort_y
      #input$doGrays
      #input$doLegend
    )
  })
  observeEvent(goBack(),
               {
                 print("Inside observe event on goBack()")
                 updateTabsetPanel(session, "outputTabActive",selected = "Help")
                 rvs$setupComplete <- 0
                 rvs$SSD.complete <- 0
                 rvs$dataImported <- 0
                 rvs$dataChecked <- 0
                 rvs$varsChecked <- 0
                 rvs$SSDdata <- 0
                 rvs$finalDF <- NULL
                 rvs$inputDF <- NULL
                 #output$dataChecked <- reactive({rvs$dataChecked})
                 #Don't go further unless something is pasted into the data box
                 req(input$pasteData)
                 updateTabsetPanel(session, "outputTabActive",selected = "Output")
                 if(is.null(input$pasteData))return(indata=NULL)
                 print((input$pasteData))
                 #put data into regular object and process it
                 inputText <- (input$pasteData)
                 print(inputText)
                 inputLines <- strsplit(inputText,split = "\n")[[1]]
                 print(inputLines)
                 varNames <- scan(text=inputLines[[1]],sep="\t",what=character())
                 print(varNames)
                 dataBody <- t(sapply(inputLines[-1],FUN=function(x)scan(text=x,sep="\t",what=character())))
                 #if only one column of numbers, the t() above should be undone
                 if(nrow(dataBody)==1)dataBody <- t(dataBody)
                 dimnames(dataBody) <- list(NULL,NULL)
                 print(dataBody)
                 inputDF <- structure(as.data.frame(dataBody,stringsAsFactors=FALSE),names=make.names(varNames))
                 print(inputDF)
                 #above results in character variables only.  Here, if most of the entries in a variable
                 #can be converted to numeric, then convert it to numeric
                 for(i in 1:ncol(inputDF)){
                   if(sum(is.na(as.numeric(inputDF[,i])))<=nrow(inputDF)/2){
                     inputDF[,i] <- as.numeric(inputDF[,i])
                   }
                 }
                 print(inputDF)
                 print(unlist(lapply(inputDF,is.numeric)))
                 print(c("Data import complete"))
                 rvs$dataImported <- 1
                 rvs$inputDF <- inputDF
                 # https://community.rstudio.com/t/extend-width-of-column-with-renderdatatable-in-shiny/50906
                 outputDT.Raw <- as.datatable(formattable(inputDF),
                                              #class = 'row-border stripe hover compact nowrap',
                                              class = 'stripe compact',
                                              #escape = FALSE,
                                              options = list(columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                                             #autoWidth = TRUE,
                                                             pageLength = 10, info = FALSE,
                                                             lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                                             scrollX = TRUE, scrollY = FALSE,
                                                             paging = TRUE, ordering = FALSE,
                                                             searching = FALSE))
                 #options=list(autoWidth = TRUE,scrollX = FALSE,scrollY = FALSE,searching = FALSE))
                 output$DTtableRaw <- DT::renderDT(outputDT.Raw)
               },ignoreInit = TRUE)
  
  observeEvent(newVars(),{
    rvs$SSDdata <- 0
    rvs$dataChecked <- 0
    req(rvs$dataImported == 1 & rvs$dataChecked == 0 & rvs$varsChecked == 0)# & is.null(rvs$finalDF))
    updateTabsetPanel(session, "outputTabActive",selected = "Output")
    #shiny::req(getData())
    testData <- rvs$inputDF
    #req({rvs$dataImported == 1})
    namesInFrame <- names(testData)
    
    if(!input$doGrps){
      # this forces wait until both vars are selected
      print("SSD check 2")
      testData <- rvs$inputDF
      namesInFrame <- names(testData)
      print(testData)
      
      print("ready to do formattable() on testData")
      print(head(testData))
      ### once we get here, we can assume that the data have been correctly identified
      outputDT.1 <- as.datatable(formattable(testData[,c("species","responses")],
                                             #align =c("r","l"),
                                             list(
                                               species = formatter("span", style = ~ style(color = "blue",font.style = "italic")),
                                               responses = formatter("span", style = ~ style(color="green", float="right")))),
                                 class = 'stripe compact',
                                 #escape = FALSE,
                                 options = list(#columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                   #autoWidth = TRUE,
                                   pageLength = 10, info = FALSE,
                                   lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                   scrollX = TRUE, scrollY = FALSE,
                                   paging = TRUE, ordering = FALSE,
                                   searching = FALSE))
      #%>%
      #  DT::formatRound(columns = 2,digits = roundTo)
      #if(FALSE)outputDT <- DT::datatable(outputDT,
      #                          class = 'row-border stripe hover compact nowrap',
      #                          rownames = FALSE,
      #                          autoHideNavigation = TRUE, escape =FALSE) %>%
      #formatStyle(columns = "Species",
      #            target="cell",
      #            fontWeight = styleEqual(1:nrow(outputData), rep("bold",nrow(outputData)))) %>%
      #DT::formatRound(columns = 2,digits = roundTo)
      #output$DTtableRaw <- DT::renderDT(rvs$inputDF)
      output$DTtable <- DT::renderDT(outputDT.1)
      
      #output$table <- renderTable(testData)
      
      # Then, do a couple of checks
      if(length(unique(testData$species))<nrow(testData))alerID <- shinyalert(
        title = "Warning",
        text = "Species labels not all unique.\nCheck if unexpected.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      if(!is.numeric(testData$responses))alerID <- shinyalert(
        title = "Warning",
        text = "Effects variable not numeric.\nCheck input!!",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      rvs$dataChecked <- 1
      #And at this point, blank out other sections of the inputs interface
      #these should only appear once data has been set up.
      output$scaleSelect <- NULL
      output$varLabels <- NULL
      output$downloadExcel <- NULL
      output$downloadPDF <- NULL
      output$Excelbutton <- NULL
      output$PDFbutton <- NULL
      output$setupButton <- NULL
      output$SSD.1.1 <- NULL
      output$SSD.1.2 <- NULL
      output$SSD.1.3 <- NULL
      #output$SSD.2.1 <- NULL
      #output$SSD.2.2 <- NULL
      #output$SSDoptshead <- NULL
      output$SSDconditional2 <- NULL
    }
    
    
    ### as others are added, these will be populated like SSD
    #at this point, remove data with missing values on response
    if(sum(is.na(testData$responses))>0){
      alerID <- shinyalert(
        title = "Warning",
        text = "Missing/non-numeric values in effects values will be removed on preview action.  Check if unexpected.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
    rvs$preSort <- testData
    testData <- testData[!is.na(testData$responses),]
    #calculate the nonparametric quantiles of the data for all plotting
    #order the response values before proceeding with analysis
    testData <- testData[order(testData$responses),]
    if(nrow(testData)<3)alerID <- shinyalert(
      title = "Error",
      text = "This analysis requires 3 or more effect values in the data.  Tool will Reset.",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){js$reset()}
    )
    if(sum(testData$responses<=0)>0)alerID <- shinyalert(
      title = "Error",
      text = "Effect values must all be greater than zero.  Tool will Reset.",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = function(x){js$reset()}
    )
    
    xVals <- quantile(log10(testData$responses),probs = seq(0.0001,0.9999,length=10000),type=8)
    #dummy values are used so that ties don't occur in the y-dimension
    #(tied data will sort in order they appear)
    xValsDummy <- quantile(order(testData$responses),probs = seq(0.0001,0.9999,length=10000),type=8)
    yVals <- seq(0.0001,0.9999,length=10000)
    pointIDs <- sapply(order(testData$responses),FUN = function(x)which.min(abs(x-xValsDummy)))
    print(c(pointIDs=pointIDs))
    pointIDs[1] <- max(which(xValsDummy==xValsDummy[1]))
    print(c(pointIDs=pointIDs))
    testData$yVals <- yVals[pointIDs]
    print(c(yVals=yVals[pointIDs]))
    rvs$finalDF <- testData
    
    req(!is.null(rvs$finalDF))
    print("FinalDF is found, set reactives for SSD")
    if(all(c("species","responses") %in% names(rvs$finalDF))){
      rvs$SSDdata <- 1
      rvs$dataChecked <- 1
    }
  })
  
  ### observeEvent(input$xLab,{
  ###  parenPos <- gregexpr(pattern="[()]",text = input$xLab)[[1]]
  ###  rvs$units <- substring(isolate(input$xLab),first=parenPos[1]+1,last=tail(parenPos,1)-1)
  ### })
  
  ### read the data that has been pasted in.  This is independent of the type of analysis
  
  # Effect level conventions change with the type of experiment/analysis, so they
  # are set independently here
  

  #on new data import, or a change in analysis type, redo the variable matching
  #here, instead of everything in one block, do each analysis type separately
  observe({
    req(rvs$dataImported == 1 & rvs$dataChecked == 0 & rvs$varsChecked == 0)# & is.null(rvs$finalDF))
    updateTabsetPanel(session, "outputTabActive",selected = "Output")
    #shiny::req(getData())
    testData <- rvs$inputDF
    #req({rvs$dataImported == 1})
    namesInFrame <- names(testData)
    #source("ComboDragDrops.R",local = TRUE)
    print("build variable selections interface")
    print(testData)
    numericVars <- which(unlist(lapply(testData,is.numeric)))
    #drop variables with negative values because those cannot be valid doses
    #but, what about missing values here?
    #numericVars <- numericVars[which(colSums(testData[,numericVars,drop=FALSE]<=0,na.rm = TRUE)==0)]
    uniqueCounts <- unlist(lapply(testData,FUN = function(x)length(unique(x))))
    #| max(uniqueCounts)<nrow(testData)
    print(numericVars)
    if(length(numericVars)<2){
      alerID <- shinyalert(
        title = "Error",
        text = "Need two columns of numeric data.  Tool will Reset.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE,
        callbackR = function(x){js$reset()}
      )
      
      #stop("There is a problem with your data (no numeric or every var has duplicates)")
    }
    if(!all(c("x.axis","y.axis") %in% names(testData))){
      alerID <- shinyalert(
        title = "Error",
        text = "Column names must include x.axis and y.axis.  Tool will Reset.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE,
        callbackR = function(x){js$reset()}
      )
      
      #stop("There is a problem with your data (no numeric or every var has duplicates)")
    }
    if(all(c("x.axis","y.axis") %in% names(testData))){
      rvs$SSDdata <- 1
      rvs$dataChecked <- 1
      min.x <- min(testData$x.axis)
      delta.x <- median(diff(testData$x.axis))
      newX <- seq(0,min.x,by=delta.x)
      testData <- rbind(testData,data.frame(x.axis=newX,y.axis=0*newX))
      max.x <- max(testData$x.axis)
      newX <- seq(max.x,max.x+min.x,by=delta.x)[-1]
      testData <- rbind(testData,data.frame(x.axis=newX,y.axis=0*newX))
      testData$y.axis[testData$y.axis<0] <- 0
      testData <- testData[order(testData$x.axis),]
      rvs$finalDF <- testData
    }
    

    req(!is.null(rvs$finalDF))
    print("FinalDF is found, set reactives for SSD")
    if(all(c("x.axis","y.axis") %in% names(rvs$finalDF))){
      rvs$SSDdata <- 1
      rvs$dataChecked <- 1
    }
  })

  # For SSD analysis, set default names on data
  observe({
    print("After SSD var selections made, adjust names/checks.")
    req(rvs$dataImported == 1 & rvs$dataChecked == 0)
    output$runButton <- NULL
    
  })
  
  
  
  observe({
    print("After SSD var selections made, adjust names/checks.")
    
    req(rvs$dataImported == 1 & rvs$dataChecked == 1 & !is.null(rvs$finalDF))
      output$downloadExcel <- downloadHandler(
        filename = "SSD Analysis.xlsx",
        content = function(file){
          file.copy("SSDoutput.xlsx", file)
        }
      )
      
      output$downloadPDF <- downloadHandler(
        filename = "SSD Analysis.pdf",
        content = function(file){
          file.copy("SSDplotOutput.pdf", file)
        }
      )
      
      output$Excelbutton <- renderUI({
        req(rvs$setupComplete == 1)
        req(rvs$SSD.complete == 1)
        #if (input$SSD.complete==0)return(actionButton("dummyButton","DUMMY"))
        if (rvs$SSD.complete==1)return(downloadBttn('downloadExcel', 'Excel Results',"fill"))
      })
      
      
      output$PDFbutton <- renderUI({
        req(rvs$setupComplete == 1)
        req(rvs$SSD.complete == 1)
        if (rvs$SSD.complete==1)return(downloadBttn('downloadPDF', 'PDF Results',"fill"))
      })
      
      output$setupButton <- renderUI({
        req(rvs$dataChecked==1)
        shinyWidgets::actionBttn(
          inputId="previewData",
          label = "Run",
          icon = icon("check"),
          style = "pill",
          color = "danger",
          size = "md",
          block = FALSE,
          no_outline = TRUE
        )
      })
      

      

    

    output$runButton <- renderUI({
      req(rvs$setupComplete == 1)
      shinyWidgets::actionBttn(
        inputId="runAnalysis",
        label = "Run Analysis",
        icon = icon("rocket"),
        style = "pill",
        color = "success",
        size = "md",
        block = FALSE,
        no_outline = TRUE
      )
      #actionButton("runAnalysis", "Run Analysis",class = "btn-primary btn-lg",icon = icon("circle-empty-play",lib = "glyphicon"))
    })
    
  })
  # This section responds to the selection of input analysisType.
  # It puts the proper template into a new slot of input, called "Selected"
  # It also puts header strings into DF that are used in the datatable view
  
  observeEvent(input$previewData,{
    print("Preview Data has been clicked")
    req(rvs$finalDF)
    
    updateTabsetPanel(session, "outputTabActive",selected = "Output")
    testData <- rvs$finalDF
    namesInFrame <- names(testData)
    
    # Do additional processing required for plotting data

    
    ### Now, do the plots
    # Here, for those with (x,y)-type variable analyses (other than SSD)

    # And separately for SSD
    {
      #when preview is clicked, reset compete flag
      #helps with multiple runs?
      rvs$SSD.complete <- 0
      print("SSD plotting")
      #print(testData)
      #req(input$doGrays)
      {
        outputData <- testData[,c("x.axis","y.axis")]
        print("USE DT TO FORMAT")
        #output$DTtable <- renderDT(outputData)

        #options below turn off the search field ,options = list(dom = 't'))
        #but, that also disables scroll so not using now.
        outputDT <- as.datatable(formattable(outputData),
                                     #class = 'row-border stripe hover compact nowrap',
                                     class = 'stripe compact',
                                     #escape = FALSE,
                                     options = list(columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                                    #autoWidth = TRUE,
                                                    pageLength = 5, info = FALSE,
                                                    lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                                    scrollX = TRUE, scrollY = FALSE,
                                                    paging = TRUE, ordering = FALSE,
                                                    searching = FALSE))
      }
      #%>%
      #  DT::formatRound(columns = 2,digits = roundTo)
      #if(FALSE)outputDT <- DT::datatable(outputDT,
      #                          class = 'row-border stripe hover compact nowrap',
      #                          rownames = FALSE,
      #                          autoHideNavigation = TRUE, escape =FALSE) %>%
      #formatStyle(columns = "Species",
      #            target="cell",
      #            fontWeight = styleEqual(1:nrow(outputData), rep("bold",nrow(outputData)))) %>%
      #DT::formatRound(columns = 2,digits = roundTo)
      output$DTtable <- DT::renderDT(outputDT)
      
      #this is same as earlier, but change rows to 5 instead of 10 after preview is clicked
      outputDT.Raw <- as.datatable(formattable(rvs$inputDF),
                                   #class = 'row-border stripe hover compact nowrap',
                                   class = 'stripe compact',
                                   #escape = FALSE,
                                   options = list(columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                                  #autoWidth = TRUE,
                                                  pageLength = 5, info = FALSE,
                                                  lengthMenu = list(c(5,10, -1), c("5","10", "All")),
                                                  scrollX = TRUE, scrollY = FALSE,
                                                  paging = TRUE, ordering = FALSE,
                                                  searching = FALSE))
      #options=list(autoWidth = TRUE,scrollX = FALSE,scrollY = FALSE,searching = FALSE))
      output$DTtableRaw <- DT::renderDT(outputDT.Raw)
      
      #output$DTtableRaw <- DT::renderDT(rvs$inputDF)
      #datatable(outputData) %>% formatStyle("Species","font-style: italic")
      nrow1 <- nrow(testData)
      testData <- na.omit(testData)
      nrow2 <- nrow(testData)
      if(nrow1!=nrow2){
        alerID <- shinyalert(
          title = "Warning",
          text = "One or more rows of data were eliminated for missing values.\nCheck if unexpected.",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      }
      
      output$basePlot <- renderPlot({
        req(rvs$inputDF)
        with(rvs$inputDF,plot(x.axis,y.axis,bty="n",xlab="x",ylab="y",xlim=range(testData$x.axis)))
      })
      
      req(rvs$finalDF)
      CurveOBJ <- rvs$finalDF[,c("x.axis","y.axis")]
      names(CurveOBJ) <- c("x","y")
      splineOBJ <- smooth.spline(x = CurveOBJ$x,y = CurveOBJ$y,spar = 0.05)
      splineFUN <- function(x){
        result <- predict(splineOBJ,x=x)$y
        result[result<0] <- 0
        result
      }
      origAUC <- fullInteg <- integrate(splineFUN,lower = 0,upper = max(CurveOBJ$x))$value
      print(c(fullInteg=fullInteg))
      #rescale
      CurveOBJ$y <- CurveOBJ$y/fullInteg
      #repeat integral (this should be very close to 1)
      #redo spline on rescaled data
      splineOBJ <- smooth.spline(x = CurveOBJ$x,y = CurveOBJ$y,spar = 0.05)
      splineFUN <- function(x){
        result <- predict(splineOBJ,x=x)$y
        result[result<0] <- 0
        result
      }
      fullInteg <- integrate(splineFUN,lower = 0,upper = max(CurveOBJ$x))$value
      print(c(fullInteg=fullInteg))
      print(CurveOBJ)
      rootFUN <- function(x,pctile=0.95){integrate(splineFUN,lower = 0,upper = x)$value - fullInteg*pctile}
      print(c(rootFUN(0,0.95),rootFUN(max(CurveOBJ$x),0.95)))
      pctl.90 <- uniroot(f=rootFUN,interval = c(0,max(CurveOBJ$x)),pctile=0.90)$root
      pctl.95 <- uniroot(f=rootFUN,interval = c(0,max(CurveOBJ$x)),pctile=0.95)$root
      pctl.99 <- uniroot(f=rootFUN,interval = c(0,max(CurveOBJ$x)),pctile=0.99)$root
      print("root finding complete")
      print(c(pctl.90=pctl.90,pctl.95=pctl.95,pctl.99=pctl.99))
      
      output$basePlot2 <- renderPlot({
        with(CurveOBJ,plot(x=x,y=y,bty="n",xlab="x",ylab="y",xlim=range(CurveOBJ$x)))
      })
      output$basePlot3 <- renderPlot({
        #first, spline on whatever scale is given, and normalize so it's really a distribution
        #that integrates to 1.
        
        with(CurveOBJ,plot(x=x,y=y,bty="n",xlab="Exposure",ylab="Density"))
        
        lines(x=seq(0,max(CurveOBJ$x),length=1000),y=splineFUN(seq(0,max(CurveOBJ$x),length=1000)))

        abline(v=(pctl.90),lty=4)
        abline(v=(pctl.95),lty=2)
        abline(v=(pctl.99),lty=3)
      })
      pctlDF <- data.frame(AUC=origAUC,pctl.90=pctl.90,pctl.95=pctl.95,pctl.99=pctl.99)
      print(pctlDF)
      finalDT <- as.datatable(formattable(pctlDF,
                                           #align =c("r","l"),
                                           list(
                                             x.axis = formatter("span", style = ~ style(color = "blue",float="right")),
                                             y.axis = formatter("span", style = ~ style(color="green", float="right")))),
                               class = 'stripe compact',
                               #escape = FALSE,
                               options = list(#columnDefs = list(list(className = 'dt-right', targets = "_all")),
                                 #autoWidth = TRUE,
                                 info = FALSE,
                                 scrollX = TRUE, scrollY = FALSE,
                                 paging = FALSE, ordering = FALSE,
                                 searching = FALSE))
      output$finalDT <- DT::renderDT(finalDT)
      
    }
    #%>%
    #  DT::formatRound(columns = 2,digits = roundTo)
    #if(FALSE)outputDT <- DT::datatable(outputDT,
    #                          class = 'row-border stripe hover compact nowrap',
    #                          rownames = FALSE,
    #                          autoHideNavigation = TRUE, escape =FALSE) %>%
    #formatStyle(columns = "Species",
    #            target="cell",
    #            fontWeight = styleEqual(1:nrow(outputData), rep("bold",nrow(outputData)))) %>%
    #DT::formatRound(columns = 2,digits = roundTo)
    
    #}
    
    ### Now, data is ready to go, so set reactive variables
    rvs$setupComplete <- 1
    rvs$indata <- testData
  })
  
  ### https://community.rstudio.com/t/updating-input-variable-not-triggering-observeevent-if-new-value-is-the-same/57120
  ### https://stackoverflow.com/questions/46732849/shiny-detect-a-change-in-input-with-a-warning-in-ui-r
  ### Not understanding why if input selections are changed, this does not reset setupComplete so the preview needs to be done
  ### again.
  
  #what to do here?  Reset things if something changes that would change numerical results?
  #or also any graphics.  Graphics changes can still be seen by clicking the check inputs
  observeEvent({list(
    input$pasteData,
    #input$figH,
    #input$figW,
    #input$axisSize,
    #input$labelSize,
    #input$lineSize,
    #input$speciesMargin,
    #input$speciesSize,
    #input$hcxSize,
    #input$doLOO,
    #input$doAOI,
    #input$xLab,
    #input$units,
    input$ECXvalue.SSD)},
    {
      req(rvs$dataChecked ==1)
      print("Input changes detected")
      print("Input changes detected")
      print("Input changes detected")
      print("Input changes detected")
      print(c(rvs.setupComplete.pre=rvs$setupComplete))
      rvs$setupComplete <- 0
      print(c(rvs.setupComplete.post=rvs$setupComplete))
    }
  )
  
  # Reset the "readiness" for analysis when inputs are changed
  observeEvent(input$runAnalysis, {
    #if (!is.null(rvs())) write.csv(rvs(), input$select2, row.names = FALSE)
    req({rvs$setupComplete == 1})
    print("Run button clicked")
    print(c(rvs.setupComplete=rvs$setupComplete))
    if(rvs$setupComplete == 0){
      print("You must preview your data first")
      alerID <- shinyalert(
        title = "Error",
        text = "You must preview your data first to confirm it is ready for analysis.",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
    req(rvs$setupComplete == 1)
    #clean up old results files if they exist
    
    testData <- na.omit(rvs$indata)
    BMRinput <- input$ECXvalue
    gZeroInput <- TRUE
    #save(list=c("testData","BMRinput","gZeroInput"),file = "testData.RData")
    if(isolate(input$analysisType)=="BMD"){
      output$markdown <- renderUI({
        HTML(markdown::markdownToHTML(knit('BMD-shiny.Rmd', quiet = FALSE)))
      })
    }
    if(isolate(input$analysisType)=="SSD"){
      #set a flag that will switch to 1 when
      #SSD analysis is complete
      Nsteps <- 2
      stepProgress <- 0
      if(input$doGrps) Nsteps <- Nsteps + 1
      if(input$doLOO) Nsteps <- Nsteps + 4
      if(input$doAOI) Nsteps <- Nsteps + 3
      withProgress(message = 'Calculating:',
                   detail = 'This may take a while...', value = 0,max=1,
                   expr = {source("SSD.run.code.R",local = TRUE)})
      
    }
  })
})
