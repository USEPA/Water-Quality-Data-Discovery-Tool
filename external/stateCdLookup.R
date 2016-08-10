stateCdLookup <- function(input, outputType="postal"){
  
  outputType <- match.arg(outputType, c("postal","fullName","tableIndex","id"))
  
  if(is.numeric(input) | !is.na(suppressWarnings(as.numeric(input)))){
    input <- which(input == as.numeric(stateCd$STATE))
  } else if(nchar(input) == 2){
    input <- which(tolower(input) == tolower(stateCd$STUSAB))
  } else {
    input <- which(tolower(input) == tolower(stateCd$STATE_NAME))
  }
  
  retVal <- switch(outputType,
                   postal = stateCd$STUSAB[input],
                   fullName = stateCd$STATE_NAME[input],
                   tableIndex = input,
                   id = stateCd$STATE[input]
  )
  
  return(retVal)
}