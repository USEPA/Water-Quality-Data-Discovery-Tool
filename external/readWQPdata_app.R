library(dataRetrieval)
readWQPdata_app <- function(...){
  matchReturn <- list(...)
  # Added from constructWQPurl()
  options <- c("bBox", "lat", "long", "within", "countrycode", 
               "statecode", "countycode", "siteType", "organization", 
               "siteid", "huc", "sampleMedia", "characteristicType", 
               "characteristicName", "pCode", "activityId", "startDateLo", 
               "startDateHi", "mimeType", "Zip", "providers")
  if (!all(names(matchReturn) %in% options)) 
    warning(matchReturn[!(names(matchReturn) %in% options)], 
            "is not a valid query parameter to the Water Quality Portal")
  # Checks for user input filters - removes null or default filters
  if (0 %in% matchReturn$bBox)
    matchReturn$bBox<-NULL
  if (matchReturn$lat== 0 | matchReturn$lat== FALSE)
    matchReturn$lat<-NULL
  if (matchReturn$long== 0 | matchReturn$long== FALSE)
    matchReturn$long<-NULL
  if (matchReturn$within== 0 | matchReturn$within== FALSE)
    matchReturn$within<-NULL
  if (matchReturn$statecode[1] == " "| matchReturn$statecode== FALSE | matchReturn$statecode == "0")
    matchReturn$statecode<-NULL
  if (matchReturn$countycode == " "| matchReturn$countycode== FALSE)
    matchReturn$countycode<-NULL
  if (matchReturn$siteType == " "| matchReturn$siteType== FALSE)
    matchReturn$siteType<-NULL
  if (matchReturn$organization == " "| matchReturn$organization== FALSE)
    matchReturn$organization<-NULL
  if (matchReturn$siteid == " "| matchReturn$siteid== FALSE)
    matchReturn$siteid<-NULL
  if (matchReturn$huc==" "| matchReturn$huc== FALSE)
    matchReturn$huc<-NULL
  if (matchReturn$sampleMedia==" "| matchReturn$sampleMedia== FALSE)
    matchReturn$sampleMedia<-NULL
  if (matchReturn$characteristicType==" "| matchReturn$characteristicType== FALSE)
    matchReturn$characteristicType<-NULL
  if (matchReturn$characteristicName==" "| matchReturn$characteristicName== FALSE | is.null(matchReturn$characteristicName))
    matchReturn$characteristicName<-NULL
  if (matchReturn$startDateLo==Sys.Date() & matchReturn$startDateHi == Sys.Date()) {
    matchReturn$startDateLo<-NULL
    matchReturn$startDateHi<-NULL
  }
  values <- sapply(matchReturn, function(x) URLencode(as.character(paste(eval(x),collapse=";",sep=""))))
  
  if("bBox" %in% names(values)){
    values['bBox'] <- gsub(pattern = ";", replacement = ",", x = values['bBox'])
  }
  
  values <- checkWQPdates(values)
  
  names(values)[names(values) == "siteNumber"] <- "siteid"
  names(values)[names(values) == "siteNumbers"] <- "siteid"
  
  #   if("statecode" %in% names(values)){
  #     stCd <- values["statecode"]
  #     if(!grepl("US:",stCd)){
  #       values["statecode"] <- paste0("US:",stateCdLookup(stCd, "id"))
  #     }
  #   }
  #   if("statecode" %in% names(values)){
  #     values["statecode"] <- as.list(values["statecode"] )
  #     }
  #   
  #   if("stateCd" %in% names(values)){
  #     stCd <- values["stateCd"]
  #     if(!grepl("US:",stCd)){
  #       values["stateCd"] <- paste0("US:",stateCdLookup(stCd, "id"))
  #     }
  #     names(values)[names(values) == "stateCd"] <- "statecode"
  #   }
  
  if("tz" %in% names(values)){
    tz <- values["tz"]
    if(tz != ""){
      rTZ <- c("America/New_York","America/Chicago",
               "America/Denver","America/Los_Angeles",
               "America/Anchorage","America/Honolulu",
               "America/Jamaica","America/Managua",
               "America/Phoenix","America/Metlakatla","UTC")
      tz <- match.arg(tz, rTZ)
      if("UTC" == tz) tz <- ""
    }
    values <- values[!(names(values) %in% "tz")]
  } else {
    tz <- ""
  }
  
  values <- gsub(",","%2C",values)
  values <- gsub("%20","+",values)
  values <- gsub(":","%3A",values)
  values <- gsub("c(","",values, fixed="TRUE")
  values <- gsub('""',"",values, fixed="TRUE")
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  
  baseURL <- "http://www.waterqualitydata.us/Result/search?"
  urlCall <- paste0(baseURL,
                    urlCall,
                    "&mimeType=tsv&sorted=no")
  
   retval <- importWQP(urlCall,FALSE, tz=tz)
  
  if(!all(is.na(retval))){
    siteInfo <- whatWQPsites_app(...)
    
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