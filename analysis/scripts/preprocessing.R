## @knitr prepareYearlyData
prepareYearlyData <- function(subsetDf, year){
  # add columns for numbers which are not contained in subsetDf
  subsetDf[,as.character(setdiff(parties$ID_BFS, as.numeric(names(subsetDf))))] <- NA
  # replace numerical column names with party names from parties
  partyNames <- paste(left_join(data.frame(ID_BFS = as.numeric(names(subsetDf))), parties, by = "ID_BFS")$Abbr_D, year, sep = "_")
  names(subsetDf) <- partyNames
  return(subsetDf)
}
prepareYearlyDataForVis <- function(subsetDf, year){
  # add columns for numbers which are not contained in subsetDf
  subsetDf[,as.character(setdiff(parties$ID_BFS, as.numeric(names(subsetDf))))] <- NA
  # replace numerical column names with party names from parties
  partyNames <- paste(as.numeric(names(subsetDf)), year, sep = "_")
  names(subsetDf) <- partyNames
  return(subsetDf)
}

combineAndDelete <- function(destination, sources, year){
  cols <- sapply(sources, function(x){
    paste(x, year, sep = "_")
  })
  filled_data[, paste(destination, year, sep = "_")] <- rowSums(filled_data[, cols], na.rm = T)
  to_delete <- setdiff(sources, destination)
  cols_to_delete <- sapply(to_delete, function(x){
    paste(x, year, sep = "_")
  })
  for(col_to_delete in cols_to_delete){
    filled_data[, col_to_delete] <- NULL
  }
  return(filled_data)
}

combineAndDeleteCountry <- function(destination, sources, year){
  long_data_country[long_data_country$year == year & long_data_country$party == destination,]$support <- sum(long_data_country[long_data_country$year == year & long_data_country$party %in% sources,]$support, na.rm = T)
  to_deletes <- setdiff(sources, destination)
  for(to_delete in to_deletes){
    long_data_country[long_data_country$year == year & long_data_country$party == to_delete,]$support <- NA
  }
  return(long_data_country)
}