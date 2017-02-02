# Dropdowns
counties<-read.csv("external/County_dropdown.csv")

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Adding a state column to the counties file to use it for filtering
countydt <- data.table(counties)
countydt[, desc := as.character(desc)]
statenames <- strsplit(countydt[, desc], ",")
sn <- vector("list", nrow(countydt))

for(i in seq(nrow(countydt))) {
    name <- statenames[[i]][2]
    sn[i] <- trim(name)
}
sname <- unlist(sn)

countydt[, state := sname]

save(countydt, file = "external/countydt.rdata")





