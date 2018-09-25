
####################################################################################
#######          Shiny app for Geoprocessing ####################
######    by erik.lindquist@ fao.org, remi.dannunzio@fao.org and yelena.finegold@fao.org 
######    ##############
####################################################################################
####################################################################################
#######          Set options and necessary packages ##########################
####################################################################################
options(stringsAsFactors=FALSE)
options(shiny.launch.browser=T)

####################################################################################
# include all the needed packages here #

packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}


packages(ggplot2) 
packages(xtable) 
packages(raster) 
packages(shiny)
packages(shinyjs)
packages(shinydashboard) 
packages(dismo) 
packages(stringr) 
packages(snow) 
packages(plyr) 
packages(leaflet) 
packages(RColorBrewer) 
packages(DT) 
#packages(RStoolbox) 
packages(rgdal) 
packages(shinyFiles)
#install.packages("caret")

source("helpers.R")

####################################################################################
#######          User Interface definition, all tabs #########################
####################################################################################
print("Starting the process") 
ui <- dashboardPage(
  skin='blue',
  ################## Main dashboard header
  dashboardHeader(
    title= 'SEPAL Geoprocessing',
    titleWidth = 350),
  
  ################## Side bar where all the tools are
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("dashboard")),
      menuItem('Change Detection', tabName = 'OTB_Change_Detection', icon = icon("map-marker")),
      menuItem('Image Segmentation', tabName = 'OTB_Segmentation', icon = icon("map-marker")),
      menuItem('TO BE FILLED!', tabName = 'end', icon = icon("bar-chart"))
      )
    ),
  
  ################## Side bar where all the tools are
  dashboardBody(
    useShinyjs(),
    tabItems(
                        
                        ####################################################################################
                        ##################          Introduction tab content #########################
                        ####################################################################################
                        tabItem(tabName = "Introduction",
                                fluidRow(
                                  tags$h4('Welcome to the SEPAL Geo-processing Module (BETA)'),
                                  tags$p(HTML('The SEPAL geo-processing functionality is based on a combination of available open-source image processing softwares and is meant to serve as a fast and easy to operate module for the processing of remotely-sensed images. The module currently consists of the following open-source, geo-procssing softwares:
                                              
                                              Geospatial Data Abstraction Library - GDAL
					      ORFEO Toolbox
					      OpenFORIS Geospatial Toolkit
                                              ')))
                        ),
                                  
                        ####################################################################################
                        ##################  Automated Change Detecion tab content 
                        ##################  #########################
                        ####################################################################################
                        #otbcli_MultivariateAlterationDetector -in1 usa_2010_test.tif -in2 
                        #usa_2015_test.tif -out blah_test.tif
                        # New Tab
                        tabItem(tabName = 'OTB_Change_Detection',
                                fluidRow(
                                  ####################################################################################
                                  # New box
                                  box(title= "Select Rasters For IMAD Change Detection", status="primary", solidHeader= TRUE,
                                      "Choose data : Raster 1 (multi-band)",
                                      br(),
                                      shinyFilesButton('file1',
                                                       'Select',
                                                       'Please select a file',
                                                       FALSE),
                                      br(),
                                      verbatimTextOutput("filepath1"),
                                      "Choose data : Raster 2 (multi-band)",
                                      br(),
                                      shinyFilesButton('file2',
                                                       'Select',
                                                       'Please select a file',
                                                       FALSE),
                                      verbatimTextOutput("filepath2"),
                                      br(),
                                      shinySaveButton('file3',
                                                      'Save File As',
                                                      'Save the file as:',
                                                      filetype=list(picture=c('tif'))),
                                      verbatimTextOutput("savefilepath"),
                                      br(),
                                      withBusyIndicatorUI(
                                      actionButton("goButton", "Process")
                                      #p("Click the button to initiate processing.")
                                      )
                                  )
                                  
                                )),
                        
                        ########### Mean Shift Segmentation ###########
                        
                        tabItem(tabName = 'OTB_Segmentation',
                                fluidRow(
                                  box(title="Select Raster to Segment", status="primary", solidHeader= TRUE,
                                      "Choose data : Raster",
                                      br(),
                                      shinyFilesButton('file4',
                                                       'Select',
                                                       'Please select a file',
                                                       FALSE),
                                      br(),
                                      verbatimTextOutput("filepath4"),
                                      br(),
                                      selectInput('algorithm', 'Segmentation Algorithm:',
                                                   c("meanshift", "cc - To be added", "watershed - To be added", "mprofiles - To be added"),
                                                   selected = c("meanshift")),
                                      br(),
                                      numericInput("sprad", "Spatial Radius =",
                                                   5,
                                                   min = 1,
                                                   max = 500),
                                      br(),
                                      numericInput("rgrad", "Range Radius =",
                                                   15,
                                                   min = 1,
                                                   max = 5000),
                                      br(),
                                      numericInput("thresh", "Convergence Threshold =",
                                                   0.1,
                                                   min = 0,
                                                   max = 1),
                                      br(),
                                      numericInput("iter", "Max Iterations =",
                                                   100,
                                                   min = 1,
                                                   max = 100000),
                                      br(),
                                      numericInput("size", "Min Region Size =",
                                                   100,
                                                   min = 1,
                                                   max = 100000),
                                      br(),
                                      selectInput("mode", "Mode:",
                                                  c("vector", "raster"),
                                                  selected = c("vector")),
                                      br(),
                                      shinySaveButton('file5',
                                                      'Save File As',
                                                      'Save the file as:',
                                                      filetype=list(vector=c('shp'),("raster"=c('tif')))),
                                      verbatimTextOutput("savefilepath5"),
                                      br(),
                                      withBusyIndicatorUI(
                                      actionButton("goButton2", "Process")
                                      #p("Click the button to initiate processing.")
                                      )
                                      )
                                )),
                         ####################################################################################
                        ##################      !!!!!!!  TO BE CONTINUED !!!!!!  
                        ##################      #########################
                        ####################################################################################
                        
                        tabItem(tabName = 'end',
                                tabPanel('nothing')
                        )
                        
                        ################## End of the list of TAB items
                      )
                      ################## End of the dashboard body
)
################## End of the User Interface
)
##################################################################################################################################
####################################################################################
##################      Server start -> the calculation #########################
####################################################################################
##################################################################################################################################
##################################################################################################################################    
server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  
  ####################################################################################
  ##################      Change Detection #########################
  ####################################################################################
  
  ##################################################################################################################################    
  ############### Select input file (raster OR vector)
 
    
    volumes <- c('User directory'=Sys.getenv("HOME"),
                            'Mon disque C par exemple' = 'C:/')
    
    shinyFileChoose(input,
                    'file1',
                    filetype=c('tif','img','pix','rst','jpeg2000','grd','hdf','shp','sqlite','vrt'),
                    roots=volumes,
                    session=session)
    
    output$filepath1 <- renderPrint({parseFilePaths(volumes, input$file1)})
    
    shinyFileChoose(input,
                    'file2',
                    filetype=c('tif','img','pix','rst','jpeg2000','grd','hdf','shp','sqlite','vrt'),
                    roots=volumes,
                    session=session)
    
    output$filepath2 <- renderPrint({parseFilePaths(volumes, input$file2)})
    
    shinyFileSave(input,
                  'file3',
                  #filetype=c('tif','img','pix','rst','jpeg2000','grd','hdf','shp','sqlite','vrt'),
                  roots=volumes,
                  session=session)
    
    output$savefilepath <- renderPrint({parseSavePath(volumes, input$file3)})
    
    otbcd <- observeEvent(input$goButton, {
      withBusyIndicatorServer("goButton", {
      print("Starting Change Detection")
      df1 <- parseFilePaths(volumes, input$file1)
      df2 <- parseFilePaths(volumes, input$file2)
      df3 <- parseSavePath(volumes, input$file3)
      file1 <- as.character(df1[,"datapath"])
      file2 <- as.character(df2[,"datapath"])
      file3 <- as.character(df3[,"datapath"])
    
      
      system(paste("otbcli_MultivariateAlterationDetector -in1 ",file1," -in2 ",file2," -out ", file3, sep=""))
      })
    })
    
    shinyFileChoose(input,
                    'file4',
                    filetype=c('tif','img','pix','rst','jpeg2000','grd','hdf','shp','sqlite','vrt'),
                    roots=volumes,
                    session=session)
    
    output$filepath4 <- renderPrint({parseFilePaths(volumes, input$file4)})
    
    shinyFileSave(input,
                  'file5',
                  #filetype=c('tif','img','pix','rst','jpeg2000','grd','hdf','shp','sqlite','vrt'),
                  roots=volumes,
                  session=session)
    
    output$savefilepath5 <- renderPrint({parseSavePath(volumes, input$file5)})
    
    otbseg <- observeEvent(input$goButton2, {
      withBusyIndicatorServer("goButton2", {
      print("Starting Segmentation")
      df4 <- parseFilePaths(volumes, input$file4)
      df5 <- parseSavePath(volumes, input$file5)
      file4 <- as.character(df4[,"datapath"])
      file5 <- as.character(df5[,"datapath"])
      segalg <- input$algorithm
      segrad <- input$sprad
      segrng <- input$rgrad
      segthr <- input$thresh
      segitr <- input$iter
      segsze <- input$size
      segmde <- input$mode
      
      
      print(paste("otbcli_Segmentation -in ",file4," -filter ",segalg," -filter.meanshift.spatialr ", segrad,
                   " -filter.meanshift.ranger ",segrng," -filter.meanshift.thres ",segthr,
                   " -filter.meanshift.maxiter ",segitr," -filter.meanshift.minsize ",segsze,
                   " -mode ",segmde," -mode.",segmde,".out ",file5, sep=""))
      
      system(paste("otbcli_Segmentation -in ",file4," -filter ",segalg," -filter.meanshift.spatialr ", segrad,
                   " -filter.meanshift.ranger ",segrng," -filter.meanshift.thres ",segthr,
                   " -filter.meanshift.maxiter ",segitr," -filter.meanshift.minsize ",segsze,
                   " -mode ",segmde," -mode.",segmde,".out ",file5, sep=""))
      })
    })
  
  ####################################################################################
  ################## Stop the shiny server
}
shinyApp(ui, server)
