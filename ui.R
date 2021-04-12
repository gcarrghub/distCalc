installedPackages <- rownames(installed.packages())
packages = unique(c("devtools","shiny","shinyjs","shinyalert","shinyWidgets",# only if for table output
                    "htmlwidgets","magrittr","parallel","formattable","DT",
                    "RColorBrewer","multcomp","openxlsx","ADGofTest",
                    "eha","evd"))
packageTests <- packages %in% installedPackages
if(all(packageTests)){
  cat("\n",paste(rep("#",100),collapse = ""),
      "\n  All required packages are present.",
      "\n",paste(rep("#",100),collapse = ""),"\n")
}
if(sum(!packageTests)>0){
  cat("\n",paste(rep("#",100),collapse = ""),
      "\n  Please wait while these required packages and their dependencies are installed:",
      "\n   ",paste(names(packageTests[!packageTests]),collapse = " "),
      "\n  Requires internet access and sufficient rights to install R packages on your system.",
      "\n",paste(rep("#",100),collapse = ""),"\n")
  install.packages(packages[!packageTests], repos = "https://cran.rstudio.com/", dependencies=TRUE)
  ### In one case, needed to add this to a users install.packages call:  INSTALL_opts = c('--no-lock')
  # recheck for packages
  installedPackages <- rownames(installed.packages())
  packageTests <- packages %in% installedPackages
  if(all(packageTests)){
    cat("\n",paste(rep("#",100),collapse = ""),
        "\n  All required packages were successfully installed.",
        "\n",paste(rep("#",100),collapse = ""),"\n")
  }
  if(!all(packageTests)){
    cat("\n",paste(rep("#",100),collapse = ""),
        "\n  Not all packages were successfully installed:",
        "\n   ",paste(names(packageTests[!packageTests]),collapse = " "),
        "\n",paste(rep("#",100),collapse = ""),"\n")
  }
}


library(shiny)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
#library(sortable)
library(htmlwidgets)
#library(rhandsontable)# only if for table output
library(formattable)
library(DT)
#library(markdown)# for PDF/markdown output
#library(rmarkdown)
#library(knitr)
library(magrittr)
#library(isotone)
library(parallel)
library(RColorBrewer)
library(multcomp)
library(openxlsx)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

### just in case, purge files that might be left over from a previous run
### the code attempts to prevent this by resetting things, but it's all limits of the
### imagination for the order things are entered, changed, etc.  The user
### must assume the ultimate responsibility.  Any critical analysis should be run
### from a reset tool, in the proper order.
if(file.exists("plotOutput.pdf"))unlink("plotOutput.pdf")
if(file.exists("xlsxOutput.xlsx"))unlink("xlsxOutput.xlsx")

print("Build the UI")
shinyUI(
  fluidPage(
    shinyalert::useShinyalert(),  # Sets up shinyalert
    titlePanel("Distribution Estimation"),
    sidebarLayout(
      sidebarPanel(
        splitLayout(
          actionBttn(
            inputId="reset_button",
            label = "Reset Tool",
            icon = icon("redo"),
            style = "pill",
            color = "default",
            size = "md",
            block = FALSE,
            no_outline = TRUE
          ),
          uiOutput("ExampleDownload")
        ),
        
        #actionButton("reset_button", "Reset Page",icon = icon("redo")),
        ### before, we assigned default vars but current version does not work on that idea
        ### splitLayout(
        ###   radioButtons("analysisType",label = "Select Analysis",selected = "SSD",
        ###               choiceValues = list("Count","BMD","SK","Continuous","SSD"),
        ###               choiceNames=list("LCx","Binary BMD","Spearman-Karber","BV","SSD")
        ###  ),
        ###  fluidPage(
        ###    wellPanel(uiOutput("defaultVars"))
        ###  )
        ###),
        # in the server, these SSD inputs are NULLed out if the analysis is not SSD
        textAreaInput("pasteData",label="Data with column labels:",rows=3,
                      placeholder = "Click inside this box and paste data (copied from Excel or similar)."),
        ### always need a response variable
        ### all of these will initally be set to None
        uiOutput("varLabels"),
        #uiOutput("xLabBox"),
        #uiOutput("yLabBox"),
        #textInput("xLab",label="Exposure label",value="Exposure Concentration"),
        #textInput("yLab",label="Response label",value="Mortality Rate"),
        # this puts out the species customizations only if SSD is chosen.
        # otherwise, NULLed out
        uiOutput("setupButton"),
        #uiOutput("runButton"),
        h3("Results:"),
        ### idea is only to offer output when an analysis is complete.
        ### otherwise, old files could get posted.  Another option
        ### is to use a different output file (xls and pdf) for
        ### each analysis, but that's not implemented yet.
        splitLayout(
          uiOutput("Excelbutton"),
          uiOutput("PDFbutton")
        ),
        # https://stackoverflow.com/questions/25062422/restart-shiny-session
        shinyjs::useShinyjs(),                                           # Include shinyjs in the UI
        shinyjs::extendShinyjs(text = jsResetCode, functions = "reset") # Add the js code to the page
        
        
        
        
      ),
      mainPanel(
        ### I think this should work for any analysis:  a view of the input data before selections,
        ### after selections, and a preview plot, and that's it for now.
        tabsetPanel(type="tabs",id = "outputTabActive",
                    tabPanel("Output",
                             h3("Input data:"),
                             DTOutput("DTtableRaw",width = "75%"),
                             h3("Analysis data:"),
                             DTOutput("DTtable",width = "75%"),
                             h3(""),
                             conditionalPanel(condition="output.setupComplete",
                                              h3("Input data:"),
                                              plotOutput("basePlot"),
                                              h3("Analysis data:"),
                                              plotOutput("basePlot2"),
                                              h3("Analysis result:"),
                                              plotOutput("basePlot3"),
                                              h3(""),
                                              h3("Numeric Result:"),
                                              DTOutput("finalDT",width="75%")
                             )#,
                    ),
                    tabPanel("Help",includeHTML("./helpFile.html"))
        )
        ### only for BMD, and that should probably be
        ### simplified since my old version somehow works
        ### outside of shiny, but not inside it.  Go figure.
        #uiOutput("markdown")
      )
    )
  )
)
