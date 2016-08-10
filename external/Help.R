function(){
  tabPanel("Help", 
           fluidPage(
             tags$style(HTML("
                             p{ text-align:center; }")),
           fluidRow(h2("WQP Data Discovery Tool Help", style  = "text-align:center")),
           HTML("<h4  style  = 'text-align:center'> This tool imports data from the <strong><a href = 'http://www.waterqualitydata.us/'>Water Quality Portal WQP</a></strong> using 
                <strong><a href = 'http://www.waterqualitydata.us/webservices_documentation/'>Web Services.</a></strong></h4>
                <p>The WQP Data Discovery tool is a desktop application that provides an easy to use interface allowing users to query, summarize, QC and display data from the WQP.  The tool uses open source R, a statistical programming language and several add-on packages, to visualize the data selected from the portal and to assist users in selecting data for analysis.</p>
                <br>
                <p>Click on an expandable panel below to view tool tips and troubleshooting guidance.</p>
                <div class='panel-group' id='accordion' role='tablist' aria-multiselectable='true'>
  <div class='panel panel-default'>
    <div class='panel-heading' role='tab' id='headingOne'>
                <h2 class='panel-title'>
                <a role='button' data-toggle='collapse' href='#collapseOne' aria-expanded='true' aria-controls='collapseOne'>
                Query Data
                </a>
                  </h2>
                  </div>
                  <div id='collapseOne' class='panel-collapse collapse in' role='tabpanel' aria-labelledby='headingOne'>
                  <div class='panel-body'>
                  The <strong>'Query Data'</strong> Screen allows users to build a query.  This tab sorts the web query parameters into three categories: location, sampling parameters and site parameters. Click on any one of these categories to expand the panels and view the possible query parameters.  
                  <br></br>
                  The Data Discovery Tool will only allow you to import data when your query returns less than 100,000 records. Check the number of records by clicking the <i>'Import Data'</i> button.
                  <br></br>
<b>The tool performs best when you import less than 20,000 records. We recommend refining your query to return fewer records before importing the data. </b>
<br></br>
Location Panel - State and county can be selected from a drop-down menu.  HUC, Point Location and Bounding box must be entered manually.
<br></br>
Sampling Parameters - Sample Media can be selected from a drop-down menu.  To enter a characteristic group or characteristic begin typing the first few letters and then selecting from the drop-down list.  Alternatively you can type in the name and click Enter.
<br></br>
Site Parameters - Site Type can be selected from a drop-down list.  To enter an organization begin typing the first few letters and then selecting from the drop-down list.  Alternatively you can type in the name and click Enter.  Site ID must be entered manually.
<br></br>
At the base of the 'Query Data' screen the web service url is displayed. As you apply and modify query parameters on this screen the query url will automatically update. 
<br></br>
      </div>
    </div>
  </div>
  <div class='panel panel-default'>
    <div class='panel-heading' role='tab' id='headingTwo'>
      <h2 class='panel-title'>
        <a class='collapsed' role='button' data-toggle='collapse' href='#collapseTwo' aria-expanded='false' aria-controls='collapseTwo'>
          Check Data
        </a>
      </h2>
    </div>
    <div id='collapseTwo' class='panel-collapse collapse' role='tabpanel' aria-labelledby='headingTwo'>
      <div class='panel-body'>
                The 'Check Data' screen allows you to examine and download the data imported from the WQP. The Home tab provides a data summary, the select method for Non-Detections menu, and a description of available data sets.
<br></br>
<b>Select method for Non-Detects Menu</b> - Use this menu to decide how non-detects are shown in the data set. By default the tool is set to 'Ignore Non-Detections- remove from data set'. Users can also choose: 'Set Non-Detections equal to zero', 'Set Non-Detections equal to the limit of detection', or 'Set Non-Detections equal to 1/2 times the Limit of Detection.
<br></br>
Preliminary QA/QC measures are applied to the data to generate the six different data sets defined below. The data sets are presented in separate tabs. Click the tabs to view each data set. Each tab provides users with the ability to download the data set they are viewing. To download a data set, first select the tab containing the data set you want. Then  the 'Save Data' button.
<br></br>

                <b> All Data</b> - These are all the data records imported from the WQP.
                <br></br>
<b>Non Detects</b> - These are the records with values for the 'Result Detection Condition Text' field equal to 'Not Detected' or 'Present below Quantification Limit'.
<br></br>
<b>W/O Units</b> - These data records have no data entered in either the <i>'Result Measure - Measure Unit Code'</i> or the <i>'Quantitation Limit Measure - Measure Unit Code'</i> fields. 
<br></br>
<b>W/O Methods</b> - There are 14 Activity Type Codes which do not require a sample to have a specified method. These data records do not match those 14 Activity Type Codes AND have no data entered in the <i>'Result Analytical Method - Method Identifier'</i> field. 
<br></br>
<b>Duplicates</b> - These data records are duplicated within the imported data set. This means these records match all fields of another record in the data set except for the <i>'Activity Type'</i> and <i>'Activity ID '</i> fields.
<br></br>
<b>Filtered Data</b> - This data set includes only results with units and methods. Results with non-detects and duplicate records have been removed. This is the data set shown in the Map on the <b>'View Data'</b> tab. 
<br></br>
<b>Summary</b> - This tab shows summary statistics of all unique combinations of station, media, characteristic, unit and sample fraction.
<br></br>
</div>
          </div>
        </div>
      <div class='panel panel-default'>
        <div class='panel-heading' role='tab' id='headingThree'>
          <h2 class='panel-title'>
            <a class='collapsed' role='button' data-toggle='collapse' href='#collapseThree' aria-expanded='false' aria-controls='collapseThree'>
                View Data
            </a>
          </h2>
        </div>
      <div id='collapseThree' class='panel-collapse collapse' role='tabpanel' aria-labelledby='headingThree'>
        <div class='panel-body'>
              The view data tab shows only the filtered data from the Check Data Tab.  The left side of the screen contains Data Filters, the Right side contains a tab delimited section with a Map, Table, Station Summary and Parameter/Unit Summary.  
<br></br>
The <b>Data filters</b> panel provides users with several options for refining their data set.  This table drives the data available for display in the Interactive Map, table, station summary and Parameter/Unit Summary tabs.  To apply a data filter, click on the panel to expand it. Users can choose to 'Select All' or Deselect All' items. <b> Note: If you 'Deselect All', an item must be added back from the drop-down list for the filter to work.</b> Users can also define a value range and/or a date range. Once you have applied all the filters click the 'Submit!' button to update the Map, Tale, Station Summary, and Parameter/Unit tab. 
<br></br>
<b>Interactive Map</b> - The map shows circles for all the stations currently displayed in the <b>Interactive Data Table</b>. The size of the circle marking each station is determined by the number of results available. The circles will resize as queries/filters are applied to the <b>Interactive Data Table</b>. Hovering your mouse over any station marker will reveal a pop-up displaying the Latitude, longitude, station name, number of samples, and number of results. Clicking the <i>Select Station</i> button and then the <i>Summarize Station</i> button to generate and update the <b> Station Summary Panel</b>. 
<br></br>
<b>Table Tab</b> This tab shows the filtered data shown on the map.  Click on the column headings to change the sort order.  Click Save Data to download the dataset.  Click on Show/hide columns to hide column or show additional columns such as 'Method' or 'ActivityStartDate'.
<br></br>
<b>Station Summary Tab</b> This tab will only appear after selecting a station on the Map and clicking the <i>Select Station</i> button.  The top pie chart shows the breakdown of the total results for the station by characteristic. The <b>Sampling Frequency</b> chart shows the timing of sample collection and associated results for the station. Note that parameters can be added to and removed from these charts by clicking in the Select parameters to view data range and frequency box.
<br></br>
<b>Parameter/Unit Summary Tab</b> - This tab will allow you to select up to three parameters to graph over time.
<br></br>
</div>
      </div>
    </div>
      <div class='panel panel-default'>
    <div class='panel-heading' role='tab' id='headingFour'>
      <h2 class='panel-title'>
        <a class='collapsed' role='button' data-toggle='collapse' href='#collapseFour' aria-expanded='false' aria-controls='collapseFour'>
          Troubleshooting
        </a>
      </h2>
    </div>
    <div id='collapseFour' class='panel-collapse collapse' role='tabpanel' aria-labelledby='headingFour'>
      <div class='panel-body'>
              <strong> <h4 style = 'color: black;'> Troubleshooting guidance </h4></strong>
              <strong><h5 style = 'color: black;'> Update R </h5> </strong>
              If the Tool is not performing as expected, the most likely issue is the version of R you are running.  At the time of this release (Version 1 March 1, 2016),
              the current version of R is 3.2.3 released December 10, 2015. If you are running an earlier version of R, there may be data display and functionality 
              issues with the tool.  The best solution is to update R and all your packages. You will need <strong>administrative privileges</strong> on your computer to do this.
              If you are an experienced R user, you can do this from the console yourself. If you are <strong>not</strong> an experienced R user, there are two options available:
              <br> 1. Follow the instructions in the Quick start guide to Install/Update R.
              <br> 2. Run the 'Troubleshooting.R' script included with the app by following the directions in the 'Troubleshooting.pdf' document.
              <br><strong><h5 style = 'color: black;'> Update R Packages</h5> </strong>
              If you are running the most recent version of R and still experiencing issues with the tool, the next step is to update the R packages.  
              To do this follow the directions in the 'Troubleshooting.pdf' document to run the 'Troubleshooting.R' script included with the app.
              <br><strong><h5 style = 'color: black;'> Log a Bug</h5> </strong>
              If the 'Troubleshooting.pdf' guide doesn't resolve your issue, please consider logging a bug with the support team.  There is a step-by-step guide to log a bug
              included in the 'User Feedback Form.docx' included in the Tool Folder.
            </div>
          </div>
        </div>
  </div>
</div>")
  ))}

