library(dataRetrieval)
#This is a new module that retrieves the data and replaces readWQPdata_app.R.  It appears to perform much faster
getWQPData_app <- function(urlCall){
  retval <- importWQP(urlCall,FALSE, tz="")
  urlStation <- gsub("/Result/", "/Station/", urlCall)
  if(!all(is.na(retval))){
    siteInfo <- importWQP(urlStation, FALSE, tz="")
    
    siteInfoCommon <- data.frame(station_nm=siteInfo$MonitoringLocationName,
                                 agency_cd=siteInfo$OrganizationIdentifier,
                                 site_no=siteInfo$MonitoringLocationIdentifier,
                                 dec_lat_va=siteInfo$LatitudeMeasure,
                                 dec_lon_va=siteInfo$LongitudeMeasure,
                                 hucCd=siteInfo$HUCEightDigitCode,
                                 stringsAsFactors=FALSE)
    
    siteInfo <- cbind(siteInfoCommon, siteInfo)
    
    retvalVariableInfo <- retval[,c("CharacteristicName","USGSPCode",
                                    "ResultMeasure.MeasureUnitCode","ResultSampleFractionText")]
    retvalVariableInfo <- unique(retvalVariableInfo)
    
    variableInfo <- data.frame(characteristicName=retval$CharacteristicName,
                               parameterCd=retval$USGSPCode,
                               param_units=retval$ResultMeasure.MeasureUnitCode,
                               valueType=retval$ResultSampleFractionText,
                               stringsAsFactors=FALSE)
    
    if(any(!is.na(variableInfo$parameterCd))){
      pcodes <- unique(variableInfo$parameterCd[!is.na(variableInfo$parameterCd)])
      pcodes <- pcodes["" != pcodes]
      paramINFO <- readNWISpCode(pcodes)
      names(paramINFO)["parameter_cd" == names(paramINFO)] <- "parameterCd"
      
      pCodeToName <- pCodeToName
      varExtras <- pCodeToName[pCodeToName$parm_cd %in% unique(variableInfo$parameterCd[!is.na(variableInfo$parameterCd)]),]
      names(varExtras)[names(varExtras) == "parm_cd"] <- "parameterCd"
      variableInfo <- merge(variableInfo, varExtras, by="parameterCd", all = TRUE)
      variableInfo <- merge(variableInfo, paramINFO, by="parameterCd", all = TRUE)
      variableInfo <- unique(variableInfo)
    }
    
    attr(retval, "siteInfo") <- siteInfo
    attr(retval, "variableInfo") <- variableInfo
    attr(retval, "url") <- urlCall
    attr(retval, "queryTime") <- Sys.time()
    
    return(retval)
  } else {
    message("The following url returned no data:\n")
    message(urlCall)
  }
}

