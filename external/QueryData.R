function(){
  tabPanel("Query Data", 
           fluidPage(
             h1("Select filters to build your data query", style  = "text-align:center"),
             fluidRow(br()),
             fluidRow(wellPanel(bsCollapse(multiple = TRUE, open = "col1", id = "collapse1",
                                           bsCollapsePanel(h3("Location", style  = "text-align:center"), 
                                                           wellPanel( style = "overflow: auto;",
                                                                      column( 4,                                     
                                                                              h3("Place"),
                                                                              selectizeInput("state", label=p("Choose a State"), 
                                                                                             choices = as.character(states$desc) , multiple = TRUE),
                                                                              bsPopover("state", "State Help", "States can be selected from the drop down list. Multiple states may be selected.",
                                                                                        "right", trigger = "hover", options = list(container = "body")),
                                                                              uiOutput('county'),
#                                                                               selectizeInput("county", label=p("Choose a County"), selected = NULL,
#                                                                                              choices = as.character(counties$desc) , multiple = TRUE),
                                                                              bsPopover("county", "County Help", "Counties for the selected states can be selected from the drop down list. Multiple counties may be selected.",
                                                                                        "right", trigger = "hover", options= list(container = "body")),
                                                                              #selectizeInput("huc_ID", label=p("HUC"), selected = NULL, choices = NULL, multiple = TRUE ),
                                                                              textInput("huc_ID", label=p("HUC"), value = "", placeholder = "Enter a huc code (ex. 03020201)"),
                                                                              bsPopover("huc_ID", "HUC Help",
                                                                                        "A HUC is a federal code used to identify the hydrologic unit of the monitoring location to the cataloging unit level of precision. Full hydrologic unit codes (HUCs) or partial HUCs using trailing wildcards may be entered. Only trailing wildcards are accepted. More than one HUC may be entered, separated by semicolons. The lists and maps of hydrologic units are available from the USGS.",
                                                                                        "top", trigger = "hover", options = list(container = "body"))),
                                                                      column( 4,
                                                                              tags$h3(id = "Point", "Point Location", style  = "text-align:center"),
                                                                              bsPopover(id = "Point", "Point Location Help", "Enter a latitude and longitude and a radial distance to create a search area. Distance should be entered in miles. Latitude and longitude should be entered in decimal degrees relative to the NAD83 datum. Longitudes in the western hemisphere should be preceded with a negative sign (-). Many stations outside the continental US do not have latitude and longitude referenced to WGS84 and therefore cannot be found using these parameters.", 
                                                                                        "bottom", trigger = "hover",options= list(container = "body")),
                                                                              column(6, numericInput("LAT", "Latitude (decimal degrees)", value = 0, min = 0, max = 100)),
                                                                              column(6, numericInput("LONG", "Longitude (decimal degrees)", value = 0, min = 0, max = 100)),
                                                                              column(12, numericInput("distance", "Distance from point (miles)", value = 0, min = 0, max = 100))),
                                                                      column(4, 
                                                                             tags$h3(id = "BBOX", "Bounding Box", style  = "text-align:center"),
                                                                             bsPopover(id = "BBOX", "Bounding Box Help", 
                                                                                       "Enter the North and South latitudes and the East and West longitudes to create a bounding box. Latitude and Longitude should be entered in decimal degrees relative to the NAD83 datum. Longitudes in the western hemisphere should be preceded with a negative sign (-).",
                                                                                       "bottom", trigger = "hover", options = list(container = "body")),
                                                                             column(6, numericInput("North", "North", value = 0, min = -100, max = 100) ),
                                                                             column(6, numericInput("South", "South", value = 0, min = -100, max = 100) ),
                                                                             column(6, numericInput("East", "East", value = 0, min = -100, max = 100) ),
                                                                             column(6, numericInput("West", "West", value = 0, min = -100, max = 100) )))),
                                           bsCollapsePanel(h3("Sampling Parameters", style  = "text-align:center"), 
                                                           wellPanel(style = "overflow: auto;",
                                                                     column(3, h4("Date Range"), h5("(MM-DD-YYYY)"),
                                                                            dateInput("date_Lo", "From", format = "mm-dd-yyyy", value = ""),
                                                                            dateInput("date_Hi", "To", format = "mm-dd-yyyy", value = "")
                                                                            ),
                                                                     column(3,selectizeInput('media',  label = HTML("<a href = 'http://www.waterqualitydata.us/portal_userguide/#WQPUserGuide-Table2'>Sample Media</a>"), multiple = TRUE, choices = '', options = list(
                                                                       valueField = 'value',
                                                                       labelField = 'value',
                                                                       searchField = 'value',
                                                                       options = list(),
                                                                       create = FALSE,
                                                                       load = I("function(query, callback) {
                                                                                $.ajax({
                                                                                url: 'http://waterqualitydata.us/Codes/samplemedia?mimeType=json',
                                                                                type: 'GET',
                                                                                error: function() {
                                                                                callback(res.codes);
                                                                                },
                                                                                success: function(res) {
                                                                                callback(res.codes);
                                                                                }
                                                                                });
}")
                                                                     )),
                                                                            bsPopover("media", "Sample Media Help",
                                                                                      "A sample media indicates the environmental medium where a sample was taken.  Click the sample media heading for a list and description of each media.  Sample media can be selected from the drop down list.  Select multiple items by clicking on each desired option.  The selected options will appear in the sample media box.  To remove a selected item, click on it to select it, then click the backspace key.",
                                                                                      "top", trigger = "hover", options = list(container = "body"))),    
                                                                     column(3, selectizeInput('group', 'Characteristic Group', multiple = TRUE, choices = '', selected = NULL, options = list(
                                                                       valueField = 'value',
                                                                       labelField = 'value',
                                                                       searchField = 'value',
                                                                       options = list(),
                                                                       create = FALSE,
                                                                       load = I("function(query, callback) {
                                                                                $.ajax({
                                                                                url: 'http://waterqualitydata.us/Codes/characteristictype?mimeType=json',
                                                                                type: 'GET',
                                                                                error: function() {
                                                                                callback();
                                                                                },
                                                                                success: function(res) {
                                                                                callback(res.codes);
                                                                                }
                                                                                });
}")
#                                                                        load = I("function(query, callback) {
#                                                                                 if (!query.length) return callback();
#                                                                                 $.ajax({
#                                                                                 url: 'http://waterqualitydata.us/Codes/characteristictype?text=' + encodeURIComponent(query) + '&pagesize=20&pagenumber=1&mimeType=json',
#                                                                                 type: 'GET',
#                                                                                 error: function() {
#                                                                                 callback();
#                                                                                 },
#                                                                                 success: function(res) {
#                                                                                 callback(res.codes);
#                                                                                 }
#                                                                                 });
#}")
                                                                     )),
                                                                            bsPopover("group", "Characteristic Group Help", 
                                                                                      "To select a characteristic group start typing the group name in the box.  This will generate a drop-down list.  Characteristic groups can be selected from the drop-down list.   Select multiple items by clicking on each desired option.  The selected options will appear in the Characteristic Group box.  To remove a selected item, click on it to select it, then click the backspace key.",
                                                                                      "top", trigger = "hover", options = list(container = "body"))),
                                                                     column(3, selectizeInput('chars', 'Characteristic', multiple = TRUE, choices = '',  options = list(
                                                                       valueField = 'value',
                                                                       labelField = 'value',
                                                                       searchField = 'value',
                                                                       options = list(),
                                                                       create = FALSE,
                                                                       load = I("function(query, callback) {
                                                                                if (!query.length) return callback();
                                                                                $.ajax({
                                                                                url: 'http://waterqualitydata.us/Codes/characteristicname?mimeType=json',
                                                                                type: 'GET',
                                                                                error: function() {
                                                                                callback();
                                                                                },
                                                                                success: function(res) {
                                                                                callback(res.codes);
                                                                                }
                                                                                });
}")
      )),
                                                                            bsPopover("chars", "Characteristic Help",
                                                                                      "A characteristic identifies different types of environmental measurements.  These names are derived from the USEPA Substance Registry System (SRS).  To select a characteristic start typing the characteristic name in the box.  This will generate a drop-down list.  Characteristics can be selected from the drop-down list.   Select multiple items by clicking on each desired option.  The selected options will appear in the Characteristic Group box.  To remove a selected item, click on it to select it, then click the backspace key.",
                                                                                      "top", trigger = "hover", options = list(container = "body"))))),
                                           bsCollapsePanel(h3("Site Parameters", style  = "text-align:center"), 
                                                           wellPanel( style = "overflow: auto;",
                                                                      column(4, selectizeInput('site_type', label = HTML("<a href = 'http://www.waterqualitydata.us/portal_userguide/#WQPUserGuide-Table1'>Site Type</a>"), multiple = TRUE, choices = '', options = list(
                                                                        valueField = 'value',
                                                                        labelField = 'value',
                                                                        searchField = 'value',
                                                                        options = list(),
                                                                        create = FALSE,
                                                                        load = I("function(query, callback) {
                                                                                 if (!query.length) return callback();
                                                                                 $.ajax({
                                                                                 url: 'http://waterqualitydata.us/Codes/sitetype?mimeType=json',
                                                                                 type: 'GET',
                                                                                 error: function() {
                                                                                 callback();
                                                                                 },
                                                                                 success: function(res) {
                                                                                 callback(res.codes);
                                                                                 }
                                                                                 });
}")
                                                                     )),
                                                                             bsPopover("site_type", "Site Type Help",
                                                                                       "A site type is a generalized location in the hydrologic cycle, or a man-made feature thought to affect the hydrologic conditions measured at a site.  To select a site type start typing the name in the box.  This will generate a drop-down list.  Site Types can be selected from the drop-down list.   Select multiple items by clicking on each desired option.  The selected options will appear in the Site Type box.  To remove a selected item, click on it to select it, then click the backspace key.",
                                                                                       "top", trigger = "hover", options = list(container = "body"))),                              
                                                                      column(4, selectizeInput('org_id', 'Organization', multiple = TRUE, choices = '', options = list(
                                                                        valueField = 'value',
                                                                        labelField = 'desc',
                                                                        searchField = 'desc',
                                                                        options = list(),
                                                                        create = FALSE,
                                                                        load = I("function(query, callback) {
                                                                                if (!query.length) return callback();
                                                                                $.ajax({
                                                                                url: 'http://waterqualitydata.us/Codes/organization?mimeType=json',
                                                                                type: 'GET',
                                                                                error: function() {
                                                                                callback();
                                                                                },
                                                                                success: function(res) {
                                                                                callback(res.codes);
                                                                                }
                                                                                });
}")
                                                                      )),
                                                                             bsPopover("org_id", "Organization Help",
                                                                                       "A designator used to identify a unique business establishment within a context.  Select from a list of organization IDs represented in the source databases.  Multiple IDs may be selected.",
                                                                                       "top", trigger = "hover", options = list( container = "body"))),
                                                                      column(4, textInput("site_id", label=p("Site ID")),
                                                                             bsPopover("site_id", "Site ID Help",
                                                                                       "A site id is a designator used to describe the unique name, number, or code assigned to identify the monitoring location. Site IDs are case-sensitive and should be entered in the following format: AGENCY-STATION NUMBER. More than one site ID may be entered, separated by semicolons. If you are entering an NWIS site, use USGS as the AGENCY.",
                                                                                       "top", trigger = "hover", options = list(container = "body"))))
             )))),
      fluidRow(
        column(5),
        column(2,
               bsButton("CHECK", "Retrieve Data", style = "primary"),
               bsModal("moMod", "WQP Query Processing", trigger = "CHECK",
                       br(),
                       fluidRow(
                         h3(textOutput("REC_txt"), 
                            style  = "text-align:center"),
                         conditionalPanel("output.Rec_count == 'yes' ",
                                          uiOutput('modal1'),
                                           uiOutput('modal2')
                                          ),
                        
                         bsPopover("modal2", "Please only click once", "Importing data takes time. Please wait for the import complete message to display. Clicking the button more than once will cause the data to download multiple times and will therefore take much longer.",
                                   "top", trigger = "hover", options = list(container = "body")),
                         conditionalPanel(
                           condition = "output.data_check == 'yes' & output.Display2 == 'yes' ",
                           fluidRow(h2("Data Import Complete", style  = "text-align:center; color:green")),
                           fluidRow(h3("Click close and proceed to the Check Data Tab. ", style  = "text-align:center; color:#204060")
                           ))))
      ))),fluidRow(br()), 
             fluidRow(wellPanel(fluidRow(h4("WQP Web Service Query URL", style  = "text-align:center")),
               verbatimTextOutput("URL")))
           )}



