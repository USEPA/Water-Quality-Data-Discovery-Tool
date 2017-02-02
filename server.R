library(DT)
library(httr)
library(stringr)
library(leaflet)
library(data.table)
library(rCharts)
library(scales)
library(jsonlite)
options(scipen=30)
#Load helper functions
source("external/buildurl.R", local=TRUE)
source("external/readWQPdata_app.R", local=TRUE)
source("external/whatWQPsites_app.R", local=TRUE)
source("external/getData.R", local=TRUE)
#Global variables
display = c("Station", "Name", "ActivityStartDate",  "Characteristic", "Result",
            "Unit", "ResultSampleFractionText", 
            "Method", "Method_ID", "ActivityMediaSubdivisionName", "OrganizationFormalName", "ActivityTypeCode")

display_Map = c("Station", "Name", "Organization",  "Characteristic", "Result", "Unit",
                "Method", "ActivityStartDate")
compute_data <- function(updateProgress = NULL) {
  # Create 0-row data frame which will be used to store data
  dat <- data.frame(x = numeric(0), y = numeric(0))
  
  for (i in 1:60) {
    Sys.sleep(0.5)
    
    # Compute new row of data
    new_row <- data.frame(x = rnorm(1), y = rnorm(1))
    
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      if (round(new_row$x) %% 2 == 0){
        text<-"Please be patient"
      }
      if (round(new_row$x) %% 2 != 0){
        text<-"still working"
      }
      full_text <- paste0("A message will display when the download is complete  ", text)
      updateProgress(detail = full_text)
    }
    
    # Add the new row of data
    dat <- rbind(dat, new_row)
  }  
  dat
}

shinyServer(
  function(input, output, session) {    
    # for Desktop bat file
    session$onSessionEnded(function() {
      stopApp()
    })
    # Take the labels and get the FIPS for State and county
    state_FIPS<-reactive({
      if (is.null(input$state)){
        return (" ")
      } else (as.character(states[states$desc %in% input$state, "FIPS"]))
    })
    county_FIPS<-reactive({
        if (is.null(input$county)){
          return (" ")
        } else (as.character(counties[counties$desc %in% input$county, "value"]))
    })   
    huc8s<-reactive({
      if(is.null(input$huc_ID)){
        return(" ")
      } else (input$huc_ID)
    })
    sample_media<-reactive({
      if(is.null(input$media)|| is.na(input$media)){
        return(" ")
      } else (input$media)
    })
    char_group<-reactive({
      if(is.null(input$group)|| is.na(input$group)){
        return(" ")
      } else (input$group)
    })
    char<-reactive({
      if(is.null(input$chars)|| is.na(input$chars)){
        return(" ")
      } else (input$chars)
    })
    type<-reactive({
      if(is.null(input$site_type)|| is.na(input$site_type)){
        return(" ")
      } else (input$site_type)
    })
    org<-reactive({
      if(is.null(input$org_id)|| is.na(input$org_id)){
        return(" ")
      } else (input$org_id)
    })
    site<-reactive({
      if(is.null(input$site_id)){
        return(" ")
      } else (input$site_id)
    })
    
    ## County selection filter
    output$county <- renderUI({
      countiesdt <- data.table(counties)
      #   countystate <- countiesdt[grepl(input$state, desc, ignore.case = TRUE)]
      if(is.null(input$state)){
        selectizeInput("county", label=p("Choose a County"), selected = NULL,
                       choices = as.character(countiesdt$desc) , multiple = TRUE)
      } else {
        selectizeInput("county", label=p("Choose a County"), selected = NULL,
                       choices = countydt[state %in% input$state ,as.character(unique(desc))] , 
                       multiple = TRUE)
      }
      
    })
    
    # Generate the url for the header pull
    url<-reactive({ 
      url<-buildurl(bBox = c(input$West, input$South, input$East, input$North), lat = input$LAT, long = input$LONG, within = input$distance,
               statecode = state_FIPS(), countycode = county_FIPS(), siteType = type(), organization = org(), 
               siteid = site(), huc = huc8s(), sampleMedia = sample_media(), characteristicType = char_group(), characteristicName = char(),
               startDateLo = as.Date(input$date_Lo, format = '%m-%d-%Y'), startDateHi = as.Date(input$date_Hi, format = '%m-%d-%Y'))
    })
    output$URL<-renderText({
      url()
    })
    
  
    
    # Run the header pull
   # RECORDS<-eventReactive(input$CHECK, {
    #  HEAD(url())$headers$'total-result-count'
   # })
    # Check number of records - conditional panel trigger
    rec_count<-eventReactive(input$CHECK, {
      if(is.null(RECORDS())|as.numeric(RECORDS()) >= 200000|as.numeric(RECORDS())==0){#| (input$CHECK%%2==1 & input$CHECK != 1)) { # won't display if clicked an odd number of times
        return("no")
      } else {#(as.numeric(RECORDS()) < 100000 & as.numeric(RECORDS()) != 0){
        return("yes")
      }
    })
    
  Headerpull<-eventReactive(input$CHECK,{
    progress<-shiny::Progress$new()
    progress$set(message = "Checking Record Count", value = 0)
    on.exit(progress$close())
    return(HEAD(url()))
  })  
  RECORDS<-reactive({
   return(Headerpull()$header$'total-result-count')
  })
  STATIONS<-reactive({
   return(Headerpull()$header$'total-site-count')
  })
  
  #  STATIONS<-eventReactive(input$CHECK, {
    #  HEAD(url())$headers$'total-site-count'
   # })
    # passes to the condition to trigger conditional panel
    output$Rec_count<-renderText({
      rec_count()
    })
    outputOptions(output, 'Rec_count', suspendWhenHidden=FALSE)
    
    # displays record count to User
    output$REC_txt<-renderText({
      withProgress(message = 'Updating',
                   detail = 'Please wait...', value = 0, {
                     for (i in 1:5) {
                       incProgress(1/5)
                       Sys.sleep(0.25)
                     }
                   })
        #if(input$CHECK == 0 | is.null(input$CHECK)) return()
             return( paste("Your query returns ", RECORDS() , " records from ", STATIONS(), " stations."))  
      })

    

      
########################## Modal in Query tab ######################################
output$modal1 <- renderUI({
                    fluidRow(column(1),
                        column(10, h4("You may import your data", style  = "text-align:center")))
             })

output$modal2 <- renderUI({   
 # if(success() != 'yes'){
    fluidRow(column(5),
             column(2, actionButton("IMPORT", "Import Data")))
 # }
})
    
#################################################################################### 

#####    Import the data
    values <- reactiveValues(starting = TRUE)
    session$onFlushed(function() {
      values$starting <- FALSE
    })
url_display<-eventReactive(input$CHECK, {
  url()
})
    data<-eventReactive(input$IMPORT, {
      # Trying a reactive example from http://shiny.rstudio.com/gallery/progress-bar-example.html
      progress<-shiny::Progress$new()
      progress$set(message = "Downloading Data, please be patitient, this may take some time.", value = 0)
      on.exit(progress$close())
      updateProgress<-function(value = NULL, detail = NULL){
        if(is.null(value)){
          value<-progress$getValue()
          value<-value + (progress$getMax() - value)/5
        }
        progress$set(value = value, detail = detail)
      }
      url<-buildurl(bBox = c(input$West, input$South, input$East, input$North), lat = input$LAT, long = input$LONG, within = input$distance,
                    statecode = state_FIPS(), countycode = county_FIPS(), siteType = type(), organization = org(), 
                    siteid = site(), huc = huc8s(), sampleMedia = sample_media(), characteristicType = char_group(), characteristicName = char(),
                    startDateLo = as.Date(input$date_Lo, format = '%m-%d-%Y'), startDateHi = as.Date(input$date_Hi, format = '%m-%d-%Y'))
     #The next line of code is new, and calls the new module for getting the data 
     return(getWQPData_app(url))

    })
 
    # Clear out import modal for each launch - This needs work 11/2/2015
    val<-reactiveValues( display = NULL, display2 = NULL, data = NULL)
    observeEvent(input$CHECK, {
      val$display<-"no"
      val$data<-NULL
      val$display2<-"no"
    })

    observeEvent(input$IMPORT, {
      val$display2<- "yes"
    })

    output$Display<-renderText({ val$display })
    outputOptions(output, 'Display', suspendWhenHidden=FALSE)
    output$Display2<-renderText({ val$display2 })
    outputOptions(output, 'Display2', suspendWhenHidden=FALSE)
    
    success<-reactive({
      if(RECORDS()>0 & dim(data())[1]>0){
        return("yes")
      }else{
        return("no")
      }
    })  
    output$data_check<-renderText({
      success()
    })
    outputOptions(output, 'data_check', suspendWhenHidden=FALSE)

  ### Begin server for Check Data Tab Panel
  # Adding a reactive to filter the data and provide the final dataset to be displayed in the data table and map
  # Using the data.table package for fast manipulations but re-converting to a data frame since the DT package requires this
    data_dt <- reactive({
      data2 <- data.table(data())
      datatt <- attr(data(), "siteInfo")
      datatt<-unique(datatt[, c("MonitoringLocationIdentifier","MonitoringLocationName","LatitudeMeasure", "LongitudeMeasure")])
      data2<-merge(data2, datatt, 
                   by = "MonitoringLocationIdentifier", all.x = T)
      data2[is.na(ResultMeasure.MeasureUnitCode), ResultMeasure.MeasureUnitCode := DetectionQuantitationLimitMeasure.MeasureUnitCode]
      setnames(data2, c("MonitoringLocationIdentifier", "MonitoringLocationName","OrganizationIdentifier", "CharacteristicName", "ResultMeasureValue", 
                        "ResultMeasure.MeasureUnitCode", "ResultAnalyticalMethod.MethodName", "ResultAnalyticalMethod.MethodIdentifier"), 
               c("Station", "Name","Organization", "Characteristic","Result", "Unit", "Method", "Method_ID"))
      data2[, Result := as.numeric(as.character(Result))]
      data2[, Station := as.factor(Station)]
      data2[, Name := as.factor(Name)]
      data2[, Organization := as.factor(Organization)]
      data2[, Characteristic := as.factor(Characteristic)]
      data2[, Unit := as.factor(Unit)]
      data2[, Method := as.factor(Method)]
      data2[, Method_ID := as.factor(as.character(Method_ID))]
      # put non detect method logic here
      if(input$ND_method==2){
        data2[ResultDetectionConditionText %in% c('Not Detected', 'Present Below Quantification Limit'), ':=' (Result = 0, 
                                                                                                               Unit = DetectionQuantitationLimitMeasure.MeasureUnitCode)]
      } else if(input$ND_method == 3){
        data2[ResultDetectionConditionText %in% c('Not Detected', 'Present Below Quantification Limit'), ':=' (Result = DetectionQuantitationLimitMeasure.MeasureValue, 
                                                                                                               Unit = DetectionQuantitationLimitMeasure.MeasureUnitCode)]
      } else if(input$ND_method ==4){
        data2[ResultDetectionConditionText %in% c('Not Detected', 'Present Below Quantification Limit'), ':=' (Result = 0.5*(DetectionQuantitationLimitMeasure.MeasureValue), 
                                                                                                               Unit = DetectionQuantitationLimitMeasure.MeasureUnitCode)]
      }
      return(data2)
    })
    all_data<-reactive({
      data.frame(data_dt())
    })
    
  output$All_Data = DT::renderDataTable(
  all_data()[, display, drop=FALSE],  escape = -1, rownames = FALSE,
  extensions = 'Buttons', options = list(dom = 'lfrBtip', buttons = I('colvis'), 
                                         pageLength = 100,
                                         lengthMenu = c(100, 200, 500),
                                         columnDefs = list(list(visible =  F, targets = list(5,6,7,8)))
  ), server = TRUE)
outputOptions(output, 'All_Data', suspendWhenHidden=TRUE)

    # Generating a character string for the method of non_detects 
    non_detect_method <- reactive({
        method <- switch(input$ND_method,
                         '1' = "Non-Detections removed from data set",
                         '2' = "Non-Detections set equal to zero",
                         '3' = "Non-Detections set equal to the Limit of Detection",
                         '4' = "Non-Detections set equal to the 1/2 times the Limit of Detection")
        return(method)
    })
    
 
    
    meta_gen_alldata <- reactive({
        data <- all_data()
        metadate <- data.table(x = "Date:", y = paste(Sys.time()))
        metadata <- data.table(x = "Dataset:", y = "All data")
        metaurl <- data.table(x = "URL:", y = url_display())
        metanondet <- data.table(x = "Method for non-detects:", y = non_detect_method())
        metaorg <- data.table(x = "Number of organizations:", y = length(unique(data$Organization)))
        metastat <- data.table(x = "Number of stations:", y = length(unique(data$Station)))
        metaparam <- data.table(x = 'Number of characteristics:', y = length(unique(data$Characteristic)))
        metarec <- data.table(x = "Number of records:", y = nrow(data))
        metabr <- data.table(x = "", y = "")
        metabr2 <- data.table(x = "---------------------------------------", y = "")
        
        meta <- rbind(metadate,metadata,metaurl,metanondet, metaorg, metastat, metaparam, metarec, metabr, metabr2)
        
        return(meta)
    })
    
    output$Save_data1 <- downloadHandler(
        filename = function() {
            paste('All_data-', Sys.Date(), '.tsv', sep='')
        },
        content = function(con) {
           write.table(meta_gen_alldata(), con, row.names = F, col.names = FALSE, sep = "\t")
           write.table(all_data(), con, row.names = F, sep = "\t", append = TRUE)
        })
    
    duplicate_logic<-reactive({
      fields<-names(data_dt())
      fields[fields != "ActivityIdentifier"]
      fields[fields != "ActivityTypeCode"]
      return(duplicated(data_dt(), by = fields)) #creates a logical vector
    })
    method_logic<-reactive({ # create logical vector identifying data w/o methods
      !data_dt()$ActivityTypeCode %in% c('Field Msr/Obs', 'Field Msr/Obs-Habitat Assessment', 'Field Msr/Obs-Incidental',
                                                'Field Msr/Obs-Portable Data Logger', 'Quality Control Field Calibration Check',
                                                'Quality Control Field Replicate Habitat Assessment', 'Quality Control Field Replicate Msr/Obs',
                                                'Quality Control Field Replicate Portable Data Logger', 'Quality Control Field Sample Equipment Rinsate Blank',
                                                'Quality Control Sample-Lab Control Sample/Blank Spike', 'Quality Control Sample-Lab Control Sample/Blank Spike Duplicate',
                                                'Quality Control Sample-Lab Matrix Spike Duplicate', 'Quality Control Sample-Lab Spike of a Lab Blank',
                                                'Sample-Depletion Replicate') &
      data_dt()$Method_ID == ""
    })

    filtered_data<-reactive({
      if(input$ND_method==1){
        data.frame(data_dt()[!duplicate_logic()&
                               Unit != "" &
                               !method_logic() &
                               !ResultDetectionConditionText %in% c('Not Detected', 'Present Below Quantititation Limit')])
      } else {
        data.frame(data_dt()[!duplicate_logic()&
                               Unit != "" &
                               !method_logic()])
      }})
    output$Filtered = DT::renderDataTable(
      filtered_data()[, display, drop=FALSE],  escape = -1, rownames = FALSE,
      extensions = 'Buttons', options = list(dom = 'lfrBtip', buttons = I('colvis'), 
                                             pageLength = 100,
                                             lengthMenu = c(100, 200, 500),
                                             columnDefs = list(list(visible =  F, targets = list(5,6,7,8)))
      ), server = TRUE)
    outputOptions(output, 'Filtered', suspendWhenHidden=TRUE)
    
    meta_gen_filtered <- reactive({
        data <- filtered_data()
        metadate <- data.table(x = "Date:", y = paste(Sys.time()))
        metadata <- data.table(x = "Dataset:", y = "Filtered data")
        metaurl <- data.table(x = "URL:", y = url_display())
        metanondet <- data.table(x = "Method for non-detects:", y = non_detect_method())
        metarec <- data.table(x = "Number of records:", y = nrow(data))
        metabr <- data.table(x = "", y = "")
        metabr2 <- data.table(x = "---------------------------------------", y = "")
        
        meta <- rbind(metadate,metadata,metaurl,metanondet, metarec, metabr, metabr2)
        
        return(meta)
    })
    
    output$Save_data6 <- downloadHandler(
      filename = function() {
        paste('Filtered_data-', Sys.Date(), '.tsv', sep='')
      },
      content = function(con) {
          write.table(meta_gen_filtered(), con, row.names = F, col.names = FALSE, sep = "\t")
          write.table(filtered_data(), con, row.names = F, sep = "\t", append = TRUE)
      })    
    no_units<-reactive({
      data.frame(data_dt()[Unit == "" & !ResultDetectionConditionText %in% c('Not Detected', 'Present Below Quantitation Limit')])
    })
    output$NO_UNITS = DT::renderDataTable(
      no_units()[, display, drop=FALSE],  escape = -1, rownames = FALSE,
      extensions = 'Buttons', options = list(dom = 'lfrBtip', buttons = I('colvis'), 
                                             pageLength = 100,
                                             lengthMenu = c(100, 200, 500),
                                             columnDefs = list(list(visible =  F, targets = list(5,6,7,8)))
      ), server = TRUE)
    outputOptions(output, 'NO_UNITS', suspendWhenHidden=TRUE)
    
    meta_gen_no_units <- reactive({
        data <- no_units()
        metadate <- data.table(x = "Date:", y = paste(Sys.time()))
        metadata <- data.table(x = "Dataset:", y = "Records with no units")
        metaurl <- data.table(x = "URL:", y = url_display())
        metanondet <- data.table(x = "Method for non-detects:", y = non_detect_method())
        metarec <- data.table(x = "Number of records:", y = nrow(data))
        metabr <- data.table(x = "", y = "")
        metabr2 <- data.table(x = "---------------------------------------", y = "")
        
        meta <- rbind(metadate,metadata,metaurl,metanondet, metarec, metabr, metabr2)
        
        return(meta)
    })
    
    
    output$Save_data3 <- downloadHandler(
      filename = function() {
        paste('Missing_Units-', Sys.Date(), '.tsv', sep='')
      },
      content = function(con) {
          write.table(meta_gen_no_units(), con, row.names = F, col.names = FALSE, sep = "\t")
          write.table(no_units(), con, row.names = F, sep = "\t", append = TRUE)
      })
    
    no_methods<-reactive({
      data.frame(data_dt()[!ActivityTypeCode %in% c('Field Msr/Obs', 'Field Msr/Obs-Habitat Assessment', 'Field Msr/Obs-Incidental',
                                               'Field Msr/Obs-Portable Data Logger', 'Quality Control Field Calibration Check',
                                               'Quality Control Field Replicate Habitat Assessment', 'Quality Control Field Replicate Msr/Obs',
                                               'Quality Control Field Replicate Portable Data Logger', 'Quality Control Field Sample Equipment Rinsate Blank',
                                               'Quality Control Sample-Lab Control Sample/Blank Spike', 'Quality Control Sample-Lab Control Sample/Blank Spike Duplicate',
                                               'Quality Control Sample-Lab Matrix Spike Duplicate', 'Quality Control Sample-Lab Spike of a Lab Blank',
                                               'Sample-Depletion Replicate') &
                             Method_ID == ""])
    })
    output$NO_METH = DT::renderDataTable(
      no_methods()[, display, drop=FALSE],  escape = -1, rownames = FALSE,
      extensions = 'Buttons', options = list(dom = 'lfrBtip', buttons = I('colvis'), 
                                             pageLength = 100,
                                             lengthMenu = c(100, 200, 500),
                                             columnDefs = list(list(visible =  F, targets = list(5,6,7,8)))
      ), server = TRUE)
    outputOptions(output, 'NO_METH', suspendWhenHidden=TRUE)
    
    meta_gen_no_methods <- reactive({
        data <- no_methods()
        metadate <- data.table(x = "Date:", y = paste(Sys.time()))
        metadata <- data.table(x = "Dataset:", y = "Records with no methods")
        metaurl <- data.table(x = "URL:", y = url_display())
        metanondet <- data.table(x = "Method for non-detects:", y = non_detect_method())
        metarec <- data.table(x = "Number of records:", y = nrow(data))
        metabr <- data.table(x = "", y = "")
        metabr2 <- data.table(x = "---------------------------------------", y = "")
        
        meta <- rbind(metadate,metadata,metaurl,metanondet, metarec, metabr, metabr2)
        
        return(meta)
    })
    
    output$Save_data4 <- downloadHandler(
      filename = function() {
       paste('Missing_Methods-', Sys.Date(), '.tsv', sep='')
      },
      content = function(con) {
          write.table(meta_gen_no_methods(), con, row.names = F, col.names = FALSE, sep = "\t")
          write.table(no_methods(), con, row.names = F, sep = "\t", append = TRUE)
      })
    duplicates<-reactive({
      data.frame(data_dt()[duplicate_logic()])
    })
    output$DUPS = DT::renderDataTable(
      duplicates()[, display, drop=FALSE],  escape = -1, rownames = FALSE,
      extensions = 'Buttons', options = list(dom = 'lfrBtip', buttons = I('colvis'), 
                                             pageLength = 100,
                                             lengthMenu = c(100, 200, 500),
                                             columnDefs = list(list(visible =  F, targets = list(5,6,7,8)))
     ), server = TRUE)
    outputOptions(output, 'DUPS', suspendWhenHidden=TRUE)
    
    meta_gen_duplicates <- reactive({
        data <- duplicates()
        metadate <- data.table(x = "Date:", y = paste(Sys.time()))
        metadata <- data.table(x = "Dataset:", y = "Duplicate records")
        metaurl <- data.table(x = "URL:", y = url_display())
        metanondet <- data.table(x = "Method for non-detects:", y = non_detect_method())
        metarec <- data.table(x = "Number of records:", y = nrow(data))
        metabr <- data.table(x = "", y = "")
        metabr2 <- data.table(x = "---------------------------------------", y = "")
        
        meta <- rbind(metadate,metadata,metaurl,metanondet, metarec, metabr, metabr2)
        
        return(meta)
    })
    
    output$Save_data5 <- downloadHandler(
      filename = function() {
        paste('Duplicates-', Sys.Date(), '.tsv', sep='')
      },
      content = function(con) {
          write.table(meta_gen_duplicates(), con, row.names = F, col.names = FALSE, sep = "\t")
          write.table(duplicates(), con, row.names = F, sep = "\t", append = TRUE)
     })
    non_detects<-reactive({
      data.frame(data_dt()[ResultDetectionConditionText %in% c('Not Detected', 'Present Below Quantification Limit')])
    })
    output$ND_Table = DT::renderDataTable(
      non_detects()[, display, drop=FALSE],  escape = -1, rownames = FALSE,
      extensions = 'Buttons', options = list(dom = 'lfrBtip', buttons = I('colvis'), 
                                             pageLength = 100,
                                             lengthMenu = c(100, 200, 500),
                                             columnDefs = list(list(visible =  F, targets = list(5,6,7,8)))
      ), server = TRUE)
    outputOptions(output, 'ND_Table', suspendWhenHidden=TRUE)
    
    meta_gen_non_detects <- reactive({
        data <- non_detects()
        metadate <- data.table(x = "Date:", y = paste(Sys.time()))
        metadata <- data.table(x = "Dataset:", y = "Non Detects")
        metaurl <- data.table(x = "URL:", y = url_display())
        metanondet <- data.table(x = "Method for non-detects:", y = non_detect_method())
        metarec <- data.table(x = "Number of records:", y = nrow(data))
        metabr <- data.table(x = "", y = "")
        metabr2 <- data.table(x = "---------------------------------------", y = "")
        
        meta <- rbind(metadate,metadata,metaurl,metanondet, metarec, metabr, metabr2)
        
        return(meta)
    })
    
    output$Save_data2 <- downloadHandler(
      filename = function() {
        paste('Non_Detects-', Sys.Date(), '.tsv', sep='')
      },
      content = function(con) {
          write.table(meta_gen_non_detects(), con, row.names = F, col.names = FALSE, sep = "\t")
          write.table(non_detects(), con, row.names = F, sep = "\t", append = TRUE)
      })
    
    summarized<-eventReactive(input$SUMMARY, {
      withProgress(message = 'Summarizing Data',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.25)
                       #return() # this is shortening the time to return the header info
                     }
                   })
      return(data.frame(data_dt()[,.(Minimum = min(Result, na.rm = TRUE), Maximum = max(Result, na.rm = TRUE), Average= mean(Result, na.rm = TRUE), Count = .N),
                                  by = c("Station", "Name", "ActivityMediaName", "Characteristic", "Unit", "ResultSampleFractionText" )]))
    })

    summ_success<-reactive({
      if(dim(summarized())[1]>0){
        return("yes")
      }else{
        return("no")
      }
    })  
    output$Summ_run<-renderText({
      summ_success()
    })
    outputOptions(output, 'Summ_run', suspendWhenHidden=FALSE)
    output$SUMMARIZED<-DT::renderDataTable(
      summarized(), escape = -1, rownames =FALSE, options=list(iDisplayLength = 50))
    outputOptions(output, 'SUMMARIZED', suspendWhenHidden = FALSE)
    
    meta_gen_summarized <- reactive({
        data <- summarized()
        metadate <- data.table(x = "Date:", y = paste(Sys.time()))
        metadata <- data.table(x = "Dataset:", y = "Summary data")
        metaurl <- data.table(x = "URL:", y = url_display())
        metanondet <- data.table(x = "Method for non-detects:", y = non_detect_method())
        metastat <- data.table(x = "Number of stations:", y = length(unique(data$Station)))
        metaparam <- data.table(x = 'Number of characteristics:', y = length(unique(data$Characteristic)))
        metarec <- data.table(x = "Number of records:", y = nrow(data))
        metabr <- data.table(x = "", y = "")
        metabr2 <- data.table(x = "---------------------------------------", y = "")
        
        meta <- rbind(metadate,metadata,metaurl,metanondet, metastat, metaparam, metarec, metabr, metabr2)
        
        return(meta)
    })
    
    output$Save_Summary_Data <- downloadHandler(
      filename = function() {
        paste('Summary_Data-', Sys.Date(), '.tsv', sep='')
      },
      content = function(con) {
          write.table(meta_gen_summarized(), con, row.names = F, col.names = FALSE, sep = "\t")
          write.table(summarized(), con, row.names = F, sep = "\t", append = TRUE)
      })
   output$check1 <- renderUI({
      data<-data.table(data_dt())
      stations<-length(unique(data$Station))
      parameters<-length(unique(data$Characteristic))
      h4("The total imported data set contains ", span(as.numeric(nrow(data_dt())), style = "font-weight: bold"), " records from", 
        span(stations, style = "font-weight: bold")," stations representing",
        span(parameters, style = "font-weight: bold"), "parameters with,",
        span(as.numeric(nrow(non_detects())), style = "font-weight: bold"), " non-detects.",
        span(as.numeric(nrow(no_units())), style = "font-weight: bold"), "records without units",
        span(as.numeric(nrow(no_methods())), style = "font-weight: bold"), "records without methods",
        span(as.numeric(nrow(duplicates())), style = "font-weight: bold"), "duplicate records")
    })
output$home_query<-renderUI({
  h5(span(url_display()), style = "text-align: center")
})
output$home_date<-renderUI({
  fluidRow(h5("Date: ", span(paste(Sys.time())), style = "text-align: center"))
})
##############################  View Data tab  #############################################
############################# Side Panel - View Data tab ###################################
##Create a single filtered Data Set that feeds the map, table and station summary charts
  spfilter_dat <- eventReactive (input$submit_filters, {
    data <- data.table(filtered_data())
   if(!is.null(input$org)) {
       data <- data[OrganizationFormalName %in% input$org]
   }
   if(!is.null(input$stt)) {
       data <- data[Name %in% input$stt]
   }
   if(!is.null(input$fmedia)) {
     data <- data[ActivityMediaName %in% input$fmedia]
   }
   if(!is.null(input$ffrac)) {
     data <- data[ResultSampleFractionText %in% input$ffrac]
   }
   if(!is.null(input$param)) {
       data <- data[Characteristic %in% input$param]
   }
   if(!is.null(input$sidepanelunit)) {
       data <- data[Unit %in% input$sidepanelunit]
   }
   if(!is.null(input$sidepanelmethod)) {
       data <- data[Method %in% input$sidepanelmethod]
   }
   if(!is.null(input$fqual)) {
     data <- data[MeasureQualifierCode %in% input$fqual]
   }
   if(!is.null(input$minvalue)) {
       data <- data[Result >= input$minvalue & Result <= input$maxvalue]
   }
   return(data)
  })

  test<-eventReactive (input$submit_filters, {
    data <- data.table(spfilter_dat())
    data<-data[ActivityStartDate>as.Date(as.character(input$spdate[1])) & ActivityStartDate<as.Date(as.character(input$spdate[2]))]
    })
  ## Create the data for the map from the filtered data
map_df <- reactive ({
  if(input$submit_filters==0){
    dat<-data.table(filtered_data())
    data_sum<-dat[,.(.N, Name = first(Name), LatitudeMeasure = first(LatitudeMeasure), LongitudeMeasure = first(LongitudeMeasure),
                     Samples = length(unique(ActivityIdentifier))), by = "Station"]
    data <- as.data.frame(data_sum)
    return(data)
  } else if(val$display2=="no"){
    dat<-data.table(filtered_data())
    data_sum<-dat[,.(.N, Name = first(Name), LatitudeMeasure = first(LatitudeMeasure), LongitudeMeasure = first(LongitudeMeasure),
                     Samples = length(unique(ActivityIdentifier))), by = "Station"]
    data <- as.data.frame(data_sum)
    return(data)
  } else {
    data_sum<-test()[,.(.N, Name = first(Name), LatitudeMeasure = first(LatitudeMeasure), LongitudeMeasure = first(LongitudeMeasure),
                        Samples = length(unique(ActivityIdentifier))), by = "Station"]
    data <- as.data.frame(data_sum)
    return(data)
  } 
})
######################################################################
## Filters
#####################################################################
output$sporg <- renderUI({
    data <- data.table(filtered_data())
    fluidRow(
        selectizeInput('org', h4("  Select organization:"),
                       choices = unique(data[, as.character(OrganizationFormalName)]),
                       multiple = TRUE,
                       selected = if(input$org_sel==1){
                         unique(data[, as.character(OrganizationFormalName)])
                         } else {NULL})
        
        )
})

output$spstation <- renderUI({
  data <- data.table(filtered_data())
  
  if(is.null(input$org)){
    data <- data
  } else {
    data <- data[OrganizationFormalName %in% input$org]
  }
    fluidRow(
        selectizeInput('stt', h4("  Select station:"),
                       choices = unique(data[, as.character(Name)]),
                       multiple = TRUE,
                       selected = if(input$stat_sel==1){
                         unique(data[, as.character(Name)])
                       } else {NULL})
    )
})

output$spmedia <- renderUI({
  data <- data.table(filtered_data())
  fluidRow(
    selectizeInput('fmedia', h4("  Select Sample Media:"),
                   choices = unique(data[, as.character(ActivityMediaName)]),
                   multiple = TRUE,
                   selected = if(input$media_sel==1){
                     unique(data[, as.character(ActivityMediaName)])
                   } else {NULL})
  )
})

output$spfraction <- renderUI({
  data <- data.table(filtered_data())
  fluidRow(if(length(unique(data[, as.character(ResultSampleFractionText)]))<1){
    h5("There are no values for sample fraction in this data set")
  } else {
    selectizeInput('ffrac', h4("  Select Sample Fraction:"),
                   choices = unique(data[, as.character(ResultSampleFractionText)]),
                   multiple = TRUE,
                   selected = if(input$frac_sel==1){
                     unique(data[, as.character(ResultSampleFractionText)])
                   } else {NULL})
  })
})

output$spparam <- renderUI({
    data <- data.table(filtered_data())
    if(is.null(input$stt)) {
      data <- data
    } else {
      data <- data[Name %in% input$stt]
    }
    fluidRow(
        selectizeInput('param', h4("  Select parameter:"),
                       choices = unique(data[, as.character(Characteristic)]),
                       multiple = TRUE,
                       selected = if(input$param_sel==1){
                         unique(data[, as.character(Characteristic)])
                       } else {NULL})
    )
})
    
output$spunit <- renderUI({
    data <- data.table(filtered_data())
    unit <- unique(data[Characteristic %in% input$param, as.character(Unit)])
    
    if(length(unit) > 1) {
        selectizeInput('sidepanelunit', h4(" "),
                                              choices = unit,
                       multiple = TRUE,
                       selected = if(input$unit_sel==1){
                         unit
                       } else {NULL})
    } else {
        p(h5('This station/parameter(s) combination only has one unit'), unit)
    }
})

output$spmethod <- renderUI({
    data <- data.table(filtered_data())
    method <- unique(data[Characteristic %in% input$param, as.character(Method)])
    
    if(length(method) > 1) {
        selectizeInput('sidepanelmethod', h4(" "),
                       choices = method,
                       multiple = TRUE,
                       selected = if(input$method_sel==1){
                         method
                       } else {NULL})
    }else {
        p(h5('This station/parameter/unit(s) combination only has one method'), method)
    }
})
output$spqual <- renderUI({
  data <- data.table(filtered_data())
  fluidRow(if(length(unique(data[, as.character(MeasureQualifierCode)]))>1){
    selectizeInput('fqual', h4("  Select Qualifier Code:"),
                   choices = unique(data[, as.character(MeasureQualifierCode)]),
                   multiple = TRUE,
                   selected = if(input$qual_sel==1){
                     unique(data[, as.character(MeasureQualifierCode)])
                   } else {NULL})
  } else {
    p(h5(paste("There is only one value for Result Measure Qualifier",unique(data[, as.character(MeasureQualifierCode)]), sep=" "))) 
  })
})

output$spvalue <- renderUI({
    data <- data.table(filtered_data())
    fluidRow(column(6,
                    numericInput('minvalue', h5("Minimum:"),
                                 value = min(as.numeric(as.character(filtered_data()$Result)), na.rm = TRUE)
                    )),
             column(6,
                    numericInput('maxvalue', h5("Maximum:"),
                                 value = max(as.numeric(as.character(filtered_data()$Result)), na.rm = TRUE)
                    )),
             bsPopover("minvalue", "Enter Minimum Value", "Do not leave this field blank.  You must enter a minimum value.",
                       "top", trigger = "hover", options = list(container = "body")),
             bsPopover("maxvalue", "Enter Maximum Value", "Do not leave this field blank.  You must enter a maximum value.",
                       "top", trigger = "hover", options = list(container = "body")))
    
})
observe({
  updateDateRangeInput(session, "spdate",
                       start = min(filtered_data()$ActivityStartDate, na.rm = TRUE),
                       end = max(filtered_data()$ActivityStartDate, na.rm = TRUE))
})

#################################  Map and draggable panel #############################################
    output$Map_title<-renderUI({
      h3("Map Displays ", span("Filtered Data"))
    })

    addPopover(session, "Map_title", "Mapped Data", placement = "top", content = paste(
      " The ", span("Filtered Dataset", style = "color:#0099CC"), " includes only results with units and methods. All results with non detections have been removed, as have duplicate records.",
      " The complete ", span("Filtered Dataset", style = "color:#0099CC"), " is displayed at the bottom of this screen.", 
      fluidRow(br()), 
      "The data displayed on the map can be interactively queried using the filters in the side panel. The map will automatically redraw based on the queries applied in the table"))
    
    output$map<-renderLeaflet({
      radiusFactor <- 50
      leaflet(map_df()) %>%
        fitBounds(lng1 = ~min(LongitudeMeasure), lat1 = ~min(LatitudeMeasure), 
                  lng2 = ~max(LongitudeMeasure), lat2 = ~max(LatitudeMeasure)) %>%
        addTiles( "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png") %>%
        addCircleMarkers(
          lng= ~LongitudeMeasure, 
          lat= ~LatitudeMeasure, 
          radius = (log(map_df()$N) + 2)  * radiusFactor / 5^2,
          layerId = row.names(map_df())
         # color = ~ifelse(Tot_Exceed == 0, 'black','blue'),
        )
    })
    
    observeEvent(input$map_marker_click, {
      leafletProxy("map")%>%clearPopups()
      content<- as.character(tagList(
        tags$strong(paste("Latitude ", map_df()[row.names(map_df()) == input$map_marker_click["id"], ][["LatitudeMeasure"]], 
                          ", Longtitude ", map_df()[row.names(map_df()) == input$map_marker_click["id"], ][["LongitudeMeasure"]])),
        tags$br(),
        tags$strong(paste("Station ID:", map_df()[row.names(map_df()) == input$map_marker_click["id"], ][["Station"]], ',')),
        tags$strong(paste("Station Name:", map_df()[row.names(map_df()) == input$map_marker_click["id"], ][["Name"]], ',')),
        tags$strong(paste(map_df()[row.names(map_df()) == input$map_marker_mouseover["id"], ][['Samples']], " sample(s), ")),
       tags$strong(paste(map_df()[row.names(map_df()) == input$map_marker_mouseover["id"], ][["N"]], " results"))
       ))
      leafletProxy("map")%>% addPopups(map_df()[row.names(map_df()) == input$map_marker_click["id"], ][["LongitudeMeasure"]], map_df()[row.names(map_df()) == input$map_marker_click["id"], ][["LatitudeMeasure"]], 
                                        paste(content, '<br></br>',
                                              actionButton("Stat_Summary", "Select this Location", 
                                                           onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())'),
                                              sep = ""))
    })
    
    
    
    #################################  Station Summary  ##########################################
 
    station_info<-eventReactive(input$button_click,{
     data <- map_df()[row.names(map_df()) == input$map_marker_click$id,]
     data <- data.table(data)
     data[, Name := as.character(Name)]
     data[, Station := as.character(Station)]
     
     return(data)
    })
    
    station_data <- eventReactive(input$button_click, {
      mapData <- data.table(dat_display())
      data1<- mapData[Station == station_info()$Station, ]
      data1[, Name := as.character(Name)]
      data1[, Station := as.character(Station)]
      data1[, Characteristic := as.character(Characteristic)]
      
      return(data1)
    })   

output$param_range_freq <- renderUI({
 
  data2 <- station_data()
    if(is.null(data2) | is.na(data2)) {
      br()
      br()
      h3('Please select a station on the map.', style  = "text-align:center ; color: #990000 ;")
    }else{
      if(is.null(input$param)){
        data2 <- data2
      } else {
        data2 <- data2[Characteristic %in% input$param]
      }
      wellPanel(selectizeInput('param_range_freq_sel', h4('Select parameters to view date range and frequency (up to 30 parameters)'),
                     choices = unique(as.character(data2$Characteristic)), 
                     selected = unique(as.character(data2$Characteristic))[c(1:10)],
                     multiple = TRUE),
      bsPopover("param_range_freq_sel", "Sampling Frequency",
                "Please select Characteristics of interest to view date range and sample collection frequency. Multiple characterisitcs may be selected.",
                "top", trigger = "hover", options = list(container = "body")))
    }
    
})

    output$Station_Summary_Panel <- renderUI({
      h4(paste("Summary for station ", station_info()$Name))
    })
    
    output$Station_Summary_Panel2 <- renderUI({
      h4(paste("Summary for station ", station_info()$Name))
    })
    
    output$Station_Summary_text<-renderUI({
   
        dataStat <- station_data()
        records<-dim(dataStat)[1]
        parameters<-length(unique(dataStat$Characteristic))
      p(paste("There are ", records, " records representing ", parameters, " characteristics at this station."))
    })

    
    output$Station_data_time_plot<-renderPlot({
      station_subset<-station_data()[Characteristic %in% input$param_range_freq_sel] # this could be  modified
      p1 <- ggplot(station_subset, aes(x=ActivityStartDate, y=Characteristic))+
        geom_point(color = "blue", size = 5, alpha = 1/2)+
        labs(x = '',y='')+
        theme_bw()+
        scale_y_discrete(labels = function(y) str_wrap(y, width = 20))+
        scale_x_date(labels = date_format("%b-%d-%y"))+
        theme(axis.text.x=element_text(angle=35, vjust=1, hjust=1))
      
      print(p1)
    })
    
## Begin Code for Filtering Data table
    dat_display<-reactive({
      if(input$submit_filters==0){
        filtered_data()
      } else {test()}
    })
    output$Map_Table = DT::renderDataTable(
      data.frame(dat_display())[, display_Map, drop=FALSE], rownames = FALSE, server = TRUE, 
      options = list(dom = 'lfrtip', pageLength = 100,
                                             lengthMenu = c(100, 200, 500)
      ))
    outputOptions(output, 'Map_Table', suspendWhenHidden=TRUE)

    output$save_map_data <- downloadHandler(
        filename = function() {
            paste('Map_data-', Sys.Date(), '.tsv', sep='')
        },
        content = function(con) {
            write.table(data.frame(dat_display()), con, row.names = F, sep = "\t")
        })

# Highcharts portion
output$pieplot <- renderChart2({
 
  data <- station_data()
  data[, charnum := length(Station), by = 'Characteristic']
  data <- data[!duplicated(data[, list(Characteristic)])]
  if(is.null(input$param)){
    data <- data
  } else {
    data <- data[Characteristic %in% input$param]
  }
  
  m <- rCharts::Highcharts$new()
  m$series(
    data = toJSONArray2(data[, list(Characteristic, charnum)], names = FALSE, json = FALSE),
    zIndex = 1,
    type = 'pie',
    name = 'Result count'
  )
  return(m)
})

output$piepresent <- renderUI({
  station <- station_info()$Name
    if(is.null(station)) {
        br()
        br()
        h3('Please select a station on the map.', style  = "text-align:center ; color: #990000 ;")
    }else{
        h4(station, style  = "text-align:center")
        showOutput("pieplot", "highcharts")
    }
    })

output$scatterpresent <- renderUI({
  station <- station_info()$Name
  
    if(is.null(station)) {
        br()
        br()
        h3('', style  = "text-align:center ; color: #990000 ;")
    }else{
        p(h4(station, style  = "text-align:center"))
        plotOutput("Station_data_time_plot")
    }
    
})

# Creating a data table with the combined parameter/unit columns
charunit <- reactive({
  data<- if(input$submit_filters==0){
    data.table(filtered_data())
  } else {  data.table(test())}  #[s, , drop = FALSE])
  data <- data[Station == station_info()$Station]
  if(is.null(input$param)){
    data <- data
  } else {
    data <- data[Characteristic %in% input$param]
  }
  data[, CharUnit := paste(Characteristic, " (", Unit, ")", sep = "")]
  setnames(data, "ActivityStartDateTime", "Date")
  data <- data[, list(Result, CharUnit, Date, Characteristic)]
  data[, charlength := length(Date), by = 'CharUnit']
  data <- data[charlength > 0]
  return(data)
})

output$paramgraph2 <- renderUI({
  data <- charunit()
  data <- data[!duplicated(data[, list(Date, CharUnit)])]  
  
  selectizeInput("G1_PU1", label = p("First Parameter/Unit Choice"),
                 choices = unique(data[, CharUnit]), multiple = FALSE)
})

output$paramgraph3 <- renderUI({
  data <- charunit()
  data <- data[!duplicated(data[, list(Date, CharUnit)])]  
  
  selectizeInput("G1_PU2", label = p("Second Parameter/Unit Choice"),
                 choices = unique(data[, CharUnit]), multiple = FALSE,
                 selected = unique(data[, CharUnit])[2])
})

output$paramgraph4 <- renderUI({
  data <- charunit()
  data <- data[!duplicated(data[, list(Date, CharUnit)])]  
  
  selectizeInput("G1_PU3", label = p("Third Parameter/Unit Choice"),
                 choices = unique(data[, CharUnit]), multiple = FALSE,
                 selected = unique(data[, CharUnit])[3])
})

output$timepresent <- renderUI({
    station <- unique(station_data()[['Station']])
    if(is.null(station)) {
        br()
        br()
        h2('Please select a station on the map.', style  = "text-align:center ; color: #990000 ;")
    }else{
        p(h4(station, style  = "text-align:center"))
        showOutput('timeseries', 'highcharts')
    }
    
})

timedata <- reactive({
  data <- charunit()
  data[, Characteristic := NULL]
  data <- data[CharUnit %in% c(input$G1_PU1, input$G1_PU2, input$G1_PU3)]
  data <- data[!duplicated(data[, list(Date, CharUnit)])]  
  testc <- dcast.data.table(data, Date ~ CharUnit, value.var = 'Result')
  return(testc)
})

output$timeseries <- renderChart2({
  datatime <- timedata()
  datatime[, Date := gsub(" UTC", "", Date)]
  datatime$Date =  as.numeric(as.POSIXct(datatime$Date))*1000
  
  ln <- rCharts::Highcharts$new()
  ln$colors("#FFCC00", "#08519C","#D94801" )
  ln$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%m-%d}'))
  ln$yAxis(list(list(title = list(text = names(datatime)[2]))
                , list(title = list(text = names(datatime)[3]), 
                       opposite = TRUE)
                , list(title = list(text = names(datatime)[4]), 
                       opposite = TRUE)))

  for(i in 2:ncol(datatime)) {
      ln$series(
        data = toJSONArray2(datatime[, c(1,i), with = FALSE], names = FALSE, json = FALSE),
        name = names(datatime)[i],
        type = 'spline',
        yAxis = (i-2)
      )
    }

    ln$plotOptions(spline = list(connectNulls = TRUE))
    ln$chart(marginTop = 70, zoomType = 'xy', panKey = 'shift', panning = TRUE) 
    ln$exporting(filename = "Line chart")

  return(ln)
  })
})