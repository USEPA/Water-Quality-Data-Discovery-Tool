
# This file contains code that sets up the UI for the View Data tab

function() {
    tabPanel("View Data",
             fluidPage(
                 sidebarLayout(
                     sidebarPanel(h3("Data filters"),
                                  fluidRow(column(3),
                                           column(2, bsButton("submit_filters", "Submit!")),
                                           bsPopover("submit_filters", "Click Submit after applying filters", "Only filters with items selected will be applied. Note: At least one station must be selected.",
                                                     "top", trigger = "hover", options = list(container = "body"))),
                                  br(),
                                bsCollapse(multiple = TRUE, # open = 'Filter Organization, Station & Parameter',
                                    bsCollapsePanel('Filter by Organization', style = 'info',
                                                    fluidRow(column(1), column(10, radioButtons('org_sel', "", c("Select All"=1, "Deselect All"=2), selected =1))),
                                                    uiOutput('sporg')),
                                    bsCollapsePanel('Filter by Station', style = 'info', 
                                                    fluidRow(column(1), column(10, radioButtons('stat_sel', "", c("Select All"=1, "Deselect All"=2), selected =1))),
                                                    uiOutput('spstation')),
                                    bsCollapsePanel('Filter by Sample Media', style = 'info',
                                                    fluidRow(column(1), column(10, radioButtons('media_sel', "", c("Select All"=1, "Deselect All"=2), selected =1))),
                                                    uiOutput('spmedia')),
                                    bsCollapsePanel('Filter by Sample Fraction', style = 'info',
                                                    fluidRow(column(1), column(10, radioButtons('frac_sel', "", c("Select All"=1, "Deselect All"=2), selected =1))),
                                                    uiOutput('spfraction')),
                                    bsCollapsePanel('Filter by Parameter', style = 'info',  
                                                    fluidRow(column(1), column(10, radioButtons('param_sel', "", c("Select All"=1, "Deselect All"=2), selected =1))),
                                                    uiOutput('spparam')),
                                    bsCollapsePanel('Filter by Units', style = 'info', 
                                                    fluidRow(column(1), column(10, radioButtons('unit_sel', "", c("Select All"=1, "Deselect All"=2), selected =1))),
                                                    uiOutput('spunit')),
                                    bsCollapsePanel('Filter by Methods', style = 'info', 
                                                    fluidRow(column(1), column(10, radioButtons('method_sel', "", c("Select All"=1, "Deselect All"=2), selected =1))),
                                                    uiOutput('spmethod')),
                                    bsCollapsePanel('Filter by Result Qualifier', style = 'info',  
                                                    fluidRow(column(1), column(10, radioButtons('qual_sel', "", c("Select All"=1, "Deselect All"=2), selected =1))),
                                                    uiOutput('spqual'))),
                                h4("Select value range:"),
                                  uiOutput('spvalue'), 
                                fluidRow(
                                  dateRangeInput("spdate", "Select a Date Range:"))
                                ),
                     mainPanel( 
                         tabsetPanel(type = "tabs",
                             tabPanel("Map",    
                                      fluidRow(uiOutput("Map_title"), style  = "text-align:center"),
                                      # fluidRow(h4("Please select a station on the map")), # style  = "text-align:center")),
                                      # fluidRow(#column(1),
                                      #   column(8, p(h4("Station currently selected: "), textOutput("Map_select"))), 
                                      #   column(2,
                                      #          br(),
                                      #          bsButton("Station_select", "Select Station", style = "primary")
                                      #   )),
                                      # bsPopover(id = 'Station_select', 'Summarize Station', 
                                      #           'Clicking this button will display a parameter summary for this station in the "Station Summary" tab and a line chart in the "Parameter/Unit Summary" tab.',
                                      #           placement = 'top', trigger = "hover", options = list(container = "body")
                                      # ),
                                      br(),
                                      leafletOutput("map")
                                      ),
                             tabPanel("Table",
                                      br(),
                                      fluidRow(column(5), column(2, downloadButton("save_map_data", "Save Data")),
                                               bsPopover("save_map_data", "Save Data", "Click to download a .tsv file containing the data being viewed.",
                                                         "top", trigger = "hover", options = list(container = "body"))),
                                      br(),
                                      fluidRow(DT::dataTableOutput("Map_Table")),
                                      br(),
                                      br(),
                                      br()),
                             tabPanel("Station Summary",
                                      h4(uiOutput("Station_Summary_Panel"), style  = "text-align:center"),
                                      h5(uiOutput("Station_Summary_text"), style  = "text-align:center"),
                                      uiOutput('piepresent'),
                                      fluidRow(h3("Sampling Frequency"), style  = "text-align:center"),
                                      uiOutput("param_range_freq"),
                                      fluidRow(uiOutput('scatterpresent'))),
                             tabPanel("Parameter/Unit Summary",
                                      h4(uiOutput("Station_Summary_Panel2"), style  = "text-align:center"),
                                      br(),
                                      fluidRow(h4("Please select up to 3 parameter/unit combinations to plot", style  = "text-align:center")),
                                      fluidRow(h4(textOutput("station1"), style  = "text-align:center")),
                                      fluidRow(column(4,
                                                      uiOutput('paramgraph2')),
                                               column(4,
                                                      uiOutput('paramgraph3')),
                                               column(4,
                                                      uiOutput('paramgraph4'))
                                               ),
                                      uiOutput('timepresent'))),
                            h3("* To populate the tabs 'Station Summary' and 'Parameter/Unit Summary', please select a station using the panel located on the map."),
                            br(),
                            br(),
                            br(),
                            br()))))
}



















