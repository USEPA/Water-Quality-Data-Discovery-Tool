# This script checks for updates of R
# it will launch a gui to help the user step through the process of updating R
# Then it removes the packages rquired by the app to allow for a 
# fresh/updated install of all required packages the next time the Launch Visualization script is run

if ("installr" %in% rownames(installed.packages()) ){
  packinfo<-installed.packages(fields = c("Package", "Version"))
  if (as.numeric(substr(packinfo["installr", "Version"], 1, 3))<0.16){
    update.packages("installr", repos='http://cran.cnr.Berkeley.edu')
  }  
} else {install.packages("installr")}
library(installr)
setInternet2(TRUE)
updateR()
packageNeeds <- c('shiny', 'shinyBS', 'data.table', 'DT',
                  'dplyr', 'dataRetrieval', 'devtools', 'httr', 
                  'ggplot2', 'stringr', 'scales', "leaflet", "rCharts")
if(length(packageNeeds)>0){
  remove.packages(packageNeeds)
}
update.packages(repos='http://cran.cnr.Berkeley.edu')
