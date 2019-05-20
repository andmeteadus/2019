library(shiny)
library(shinydashboard)
library(rhandsontable)
library(plyr)
library(dplyr)
library(ggplot2)


### Objects

inputData <- data.frame(
  id = c(),
  sampleName = c(),
  fileType = c(),
  fileName = c(),
  status = c(),
  warnings = c(),
  dilutionFactor = c(),
  measuredParticles = c(),
  scale = c(),
  measurement = c(),
  stringsAsFactors = FALSE
)

inputData.noManual <- inputData
inputData.empty <- inputData

inputData.display.empty <- data.frame(
  sampleName = c(""),
  fileType = c(""),
  status = c(""),
  warnings = c(""),
  dilutionFactor = c(""),
  measuredParticles = c(""),
  fileName = c(""),
  stringsAsFactors = FALSE
)

dilutionFactorList <- list()
names(dilutionFactorList) <- c()
particleNoList <- list()
names(particleNoList) <- c()

sizeDF <- NULL
zpDF <- NULL
sizeFreqDF <- NULL
zpFreqDF <- NULL

DFgroups1 <- NULL
DFgroups1.empty <- data.frame(Sample1 = c(TRUE))
rownames(DFgroups1.empty) <- c("Group1")

DFgroups2 <- NULL
DFgroups2.empty <- data.frame(Sample1 = c(TRUE))
rownames(DFgroups2.empty) <- c("Group1")

GroupsList.level1 <- NULL
GroupsList.level2 <- NULL


### Functions

add_input_data <- function(fileInputTable) {
  
  fileNames <- fileInputTable$name
  fileNamesSplit <- lapply(fileNames, function(x) unlist(strsplit(x, "_")))
  sampleNames <- unlist(lapply(fileNamesSplit, function(x) paste(x[3:(length(x)-1)], collapse = "_")))
  
  fileTypes <- rep("", length(sampleNames))
  zetaFiles <- grep("prof", fileInputTable$name, value = FALSE)
  sizeFiles <- grep("size", fileInputTable$name, value = FALSE)
  fileTypes[zetaFiles] <- "ZP"
  fileTypes[sizeFiles] <- "size"
  
  ids <- paste(sampleNames, fileTypes, sep = "_")
  
  
  ########################################################## file data
  scales <- vector()
  measurements <- vector()
  warnings <- vector()
  warning <- ""
  
  for (i in 1:nrow(fileInputTable)) {
    
    name <- sampleNames[i]
    
    # Read the file
    filePath <- fileInputTable$datapath[i]
    fileContents <- readLines(filePath)
    
    #### if size file
    if (fileTypes[i] == "size") {
      
      # Finds the row with the dilution factor
      dilutionRowNum <- grep("Dilution", fileContents)
      
      # Sets the default dilution factor and warnings value
      dilutionNum <- 1
      
      # If it finds the correct row, extracts the dilution factor
      if (length(dilutionRowNum) > 0) {
        
        dilutionRow <- fileContents[dilutionRowNum]
        dilutionStr <- tail(unlist(strsplit(dilutionRow, "\t")), n=1)
        dilutionNum <- as.double(dilutionStr)
        
        # If the dilution factor is not found in the input file, notifies the user
      } else {
        warning <- paste(warning, "Dilution factor not found. Dilution factor defaulted to 1", sep = "; ")
      }
      dilutionFactorList.names.prev <- names(dilutionFactorList)
      dilutionFactorList.values.prev <- unlist(dilutionFactorList)
      
      dilutionFactorList <<- as.list(c(dilutionFactorList.values.prev, dilutionNum))
      names(dilutionFactorList) <<- c(dilutionFactorList.names.prev, sampleNames[i])
      
      # Finds the beginning of size data table in the input file
      tableStartRowNum <- grep("Size / nm", fileContents) + 1
      finalRowNum <- grep("-1.000000", fileContents) - 1
      
      # If it finds the table...
      if (length(tableStartRowNum) > 0) {
        
        # Selects the size data table rows from the first to the last
        selectedData <- fileContents[(tableStartRowNum):(finalRowNum)]
        
        # Convert data columns into strings
        selectDataSplit <- strsplit(selectedData, "\t")
        fileSizeMatrix <- do.call(rbind, selectDataSplit)
        scale <- paste(fileSizeMatrix[,1], collapse = "_")
        measurement <- paste(fileSizeMatrix[,3], collapse = "_")
        
        # Total particle numbers
        particleNoList.names.prev <- names(particleNoList)
        particleNoList.values.prev <- unlist(particleNoList)
        
        particleNoList <<- as.list(c(particleNoList.values.prev, round(sum(as.numeric(fileSizeMatrix[,3])))))
        names(particleNoList) <<- c(particleNoList.names.prev, sampleNames[i])
        
        # If it fails to find the size table in the input file, the user is notified
      } else {
        warning <- paste(warning, "Size distribution table not found in file", sep = "; ")
        scale <- ""
        measurement <- ""
      }
      
      ### if ZP file  
    } 
    
    else if (fileTypes[i] == "ZP") {
      
      # Finds the beginning of ZP data table in the input file
      tableStartRowNum <- suppressWarnings(grep("Electroph Mobility", fileContents))
      
      # If the table is found...
      if (length(tableStartRowNum) > 0) {
        
        selectedData <- fileContents[(tableStartRowNum+1):length(fileContents)]
        selectDataSplit <- strsplit(selectedData, "\t")
        
        # Repeat every detected ZP by the times it is reported in the raw data file 
        rawValueVec <- unlist(lapply(selectDataSplit, function(x) rep(as.double(x[2]), as.double(x[3]))))
        
        # Converts the raw data into bins with the size of 5. This results in a vector of bin order numbers
        # instead of actual ZP bins. This needs to be translated into corresponding ZP bins
        intervalVec <- findInterval(rawValueVec, seq(-135, 135, by=5))
        
        # Creates a list for translating bin position values into ZP bins. In the translation list
        # every bin position value is the key for accessing the corresponding ZP bin value
        # This results in a vector of ZP bins repeated the number of times they were assigned a "particle count value"
        # in the raw data file.
        translationList <- seq(-135, 130, by=5)
        names(translationList) <- seq(1:54)
        freqVec <- unlist(lapply(intervalVec, function(x) translationList[x]))
        
        # freqVec is in turn converted into a frequency data frame with counts or raw data "particle count values"
        # ZP bins are also assigned as row values to enable subsequent matching
        freqVecDataframe <- as.data.frame(table(freqVec))
        rownames(freqVecDataframe) <- freqVecDataframe$freqVec
        
        # An empty file data frame is created with as many rows as there are ZP bins to store the
        # existing counts and empty cells for bins not detected in the data
        fileDataframe <- data.frame(matrix(nrow = 54, ncol = 0))
        rownames(fileDataframe) <- seq(-135, 130, by=5)
        
        # Data is matched with the empty fileDataframe by rows
        fileDataframe <-merge(fileDataframe, freqVecDataframe["Freq"], by="row.names", all.x=TRUE)
        fileDataframe$Row.names <- as.numeric(fileDataframe$Row.names)
        
        # The resulting data frame is ordered just in case, and row and column names assigned
        fileDataframe <- fileDataframe[ order(fileDataframe$Row.names), ]
        rownames(fileDataframe) <- fileDataframe$Row.names
        colnames(fileDataframe) <- c("ZP", "Freq")
        
        # Empty bins (marked by value NA) are repleaced with 0
        fileDataframe[is.na(fileDataframe)] <- 0
        
        # Raw data "particle values" are summed and frequency calculated based on the sum
        freqColSum <- sum(fileDataframe$Freq)
        fileDataframe$Freq <- fileDataframe$Freq / freqColSum
        
        # Modified data
        scale <- paste(fileDataframe$ZP, collapse = "_")
        measurement <- paste(fileDataframe$Freq, collapse = "_")
        
        
      } else {
        warning <- paste(warning, "ZP distribution table not found in file.", sep = "; ")
        scale <- ""
        measurement <- ""
      }
      
    }
    
    warnings <- c(warnings, warning)
    scales <- c(scales, scale)
    measurements <- c(measurements, measurement)
    
  }
  
  inputData.new <- data.frame(
    id = ids,
    sampleName = sampleNames,
    fileType = fileTypes,
    fileName = fileInputTable$name,
    status = rep("OK", length(ids)),
    warnings = warnings,
    dilutionFactor = rep("", length(ids)),
    measuredParticles = rep("", length(ids)),
    scale = scales,
    measurement = measurements,
    stringsAsFactors = FALSE
  )
  
  inputData <<- rbind(inputData, inputData.new)
  
  dilutionFactors <- vector()
  for (i in 1:length(inputData$sampleName)) {
    name <- inputData$sampleName[i]
    
    if (name %in% names(dilutionFactorList)) {
      dilutionFactors <- c(dilutionFactors, dilutionFactorList[[name]])
    } else {
      dilutionFactors <- c(dilutionFactors, "")
    }
  }
  
  particleNos <- vector()
  for (i in 1:length(inputData$sampleName)) {
    name <- inputData$sampleName[[i]]
    
    if (name %in% names(particleNoList)) {
      particleNos <- c(particleNos, particleNoList[[name]])
    } else {
      particleNos <- c(particleNos, "")
    }
  }
  
  # If duplicated
  status = rep("OK", length(inputData$id))
  status[duplicated(inputData$id)] <- "duplicate"
  
  # If mismatch
  zetaFiles <- grep("prof", inputData$fileName, value = FALSE)
  if (length(zetaFiles) > 0) {
    
    sampleNames.size <- inputData$sampleName[inputData$fileType == "size"]
    sampleNames.zeta <- inputData$sampleName[inputData$fileType == "ZP"]
    
    status[(!inputData$sampleName %in% sampleNames.zeta) & (inputData$fileType == "size") & (!status == "duplicate")] <- "mismatch"
    status[(!inputData$sampleName %in% sampleNames.size) & (inputData$fileType == "ZP") & (!status == "duplicate")] <- "mismatch"
    
  }
  
  inputData$status <<- status
  inputData$dilutionFactor <<- dilutionFactors
  inputData$measuredParticles <<- particleNos
  
  inputData.noManual <<- inputData
  
}

refresh_input_data <- function(inputData) {
  
  # If duplicated
  sampleNames <- inputData$sampleName
  fileTypes <- inputData$fileType
  ids <- paste(sampleNames, fileTypes, sep = "_")
  
  status = rep("OK", length(ids))
  status[duplicated(ids)] <- "duplicate"
  
  # If mismatch
  zetaFiles <- grep("prof", inputData$fileName, value = FALSE)
  if (length(zetaFiles) > 0) {
    
    sampleNames.size <- sampleNames[fileTypes == "size"]
    sampleNames.zeta <- sampleNames[fileTypes == "ZP"]
    
    status[(!sampleNames %in% sampleNames.zeta) & (fileTypes == "size") & (!status == "duplicate")] <- "mismatch"
    status[(!sampleNames %in% sampleNames.size) & (fileTypes == "ZP") & (!status == "duplicate")] <- "mismatch"
    
  }
  
  inputData$status <<- status
}

make_size_table <- function(inputData) {
  
  sizeData <- inputData[inputData$fileType == "size", ]
  
  if (nrow(sizeData) > 0) {
    
    for (i in 1:nrow(sizeData)) {
      
      if (i == 1) {
        scaleVec <- unlist(strsplit(sizeData$scale[i], "_"))
        sizeDF <- data.frame(nm = as.numeric(scaleVec))
      }
      measurementVec <- as.numeric(unlist(strsplit(sizeData$measurement[i], "_"))) * sizeData$dilutionFactor[i]
      sizeDF <- cbind(sizeDF, measurementVec)
    }
    colnames(sizeDF) <- c("nm", sizeData$sampleName)
    
  } else {
    sizeDF <- NULL
  }
  
  return(sizeDF)
}

make_zp_table <- function(inputData) {
  
  zpData <- inputData[inputData$fileType == "ZP", ]
  
  if (nrow(zpData) > 0) {
    
    for (i in 1:nrow(zpData)) {
      
      if (i == 1) {
        scaleVec <- unlist(strsplit(zpData$scale[i], "_"))
        zpDF <- data.frame(mV = as.numeric(scaleVec))
      }
      measurementVec <- as.numeric(unlist(strsplit(zpData$measurement[i], "_"))) * zpData$measuredParticles[i]
      zpDF <- cbind(zpDF, measurementVec)
    }
    colnames(zpDF) <- c("mV", zpData$sampleName)
    
  } else {
    zpDF <- NULL
  }
  
  return(zpDF)
}

make_size_freq_table <- function(inputData) {
  
  sizeData <- inputData[inputData$fileType == "size", ]
  
  if (nrow(sizeData) > 0) {
    
    for (i in 1:nrow(sizeData)) {
      
      if (i == 1) {
        scaleVec <- unlist(strsplit(sizeData$scale[i], "_"))
        sizeFreqDF <- data.frame(nm = as.numeric(scaleVec))
      }
      measurementVec <- as.numeric(unlist(strsplit(sizeData$measurement[i], "_"))) / sizeData$measuredParticles[i]
      sizeFreqDF <- cbind(sizeFreqDF, measurementVec)
    }
    colnames(sizeFreqDF) <- c("nm", sizeData$sampleName)
    
  } else {
    sizeFreqDF <- NULL
  }
  
  return(sizeFreqDF)
}

make_zp_freq_table <- function(inputData) {
  
  zpData <- inputData[inputData$fileType == "ZP", ]
  
  if (nrow(zpData) > 0) {
    
    for (i in 1:nrow(zpData)) {
      
      if (i == 1) {
        scaleVec <- unlist(strsplit(zpData$scale[i], "_"))
        zpFreqDF <- data.frame(mV = as.numeric(scaleVec))
      }
      measurementVec <- as.numeric(unlist(strsplit(zpData$measurement[i], "_")))
      zpFreqDF <- cbind(zpFreqDF, measurementVec)
    }
    colnames(zpFreqDF) <- c("mV", zpData$sampleName)
    
  } else {
    zpFreqDF <- NULL
  }
  
  return(zpFreqDF)
}

add_sample_groups <- function(DFgroups.input, inputData, defaultStatus) {
  
  DFgroups.output <- NULL
  
  if (is.null(DFgroups.input)) {
    
    sampleNames <- unique(inputData[,2])
    DFgroups.output <- as.data.frame(matrix(ncol = length(sampleNames) + 1, nrow = 1))
    colnames(DFgroups.output) <- c("GroupName", sampleNames)
    
    DFgroups.output[1,1] <- "Group1"
    DFgroups.output[1,2:(length(sampleNames)+1)] <- defaultStatus
    
  } else {
    
    if (defaultStatus) {
      if (nrow(DFgroups.input) > 1) {
        defaultStatus.addition <- FALSE
      } else {
        defaultStatus.addition <- TRUE
      }
    } else {
      defaultStatus.addition <- FALSE
    }
    
    DFgroups.output <- DFgroups.input
    existingSamples <- colnames(DFgroups.input)[2:length(colnames(DFgroups.input))]
    newSamples <- setdiff(inputData[,2], existingSamples)
    
    if (length(newSamples) > 0) {
      
      for (i in 1:length(newSamples)) {
        
        DFgroups.temp <- data.frame(temp = rep(defaultStatus.addition, nrow(DFgroups.input)))
        
        DFgroups.output <- cbind(DFgroups.output, DFgroups.temp)
        
      }
      colnames(DFgroups.output) <- c("GroupName", existingSamples, newSamples)
    }
    
    obsoleteSamples <- setdiff(c(existingSamples, newSamples), inputData[,2])
    
    DFgroups.output <- DFgroups.output[ ,!(colnames(DFgroups.output) %in% obsoleteSamples)]
    
  }
  return(DFgroups.output)
}

convert_groups_table_to_list <- function(groupsTable) {
  
  groupsList <- list()
  
  groupNames <- (groupsTable[,1])
  sampleNames <- colnames(groupsTable)[2:ncol(groupsTable)]
  
  for (i in 1:nrow(groupsTable)) {
    bool <- as.vector(groupsTable[i, 2:ncol(groupsTable)], mode = "logical")
    bool[is.na(bool)] <- FALSE
    samplesInGroup <- list(sampleNames[bool])
    samplesInGroup <- 
      groupsList <- c(groupsList, samplesInGroup)
  }
  names(groupsList) <- groupNames
  
  return(groupsList)
  
}

fix_groupings <- function(list1, list2) {
  
  if (is.null(list1)) {
    output <- list("list1" = NULL,
                   "list2" = NULL)
  } else if (is.null(list2)) {
    output <- list("list1" = list1,
                   "list2" = NULL)
  } else {
    samples1 <- unlist(list1)
    samples2 <- unlist(list2)
    
    inList1_notInList2 <- setdiff(samples1, samples2)
    inList2_notInList1 <- setdiff(samples2, samples1)
    
    if (length(inList2_notInList1) > 0) {
      names <- names(list1)
      list1 <- c(list1, list(inList2_notInList1))
      names(list1) <- c(names, "ungrouped")
    }
    
    if (length(inList1_notInList2) > 0) { 
      names <- names(list2)
      list2 <- c(list2, list(inList1_notInList2))
      names(list2) <- c(names, "ungrouped")
    }
    
    output <- list("list1" = list1,
                   "list2" = list2)
  }
  return(output)
}

#' Factor to numeric converter
#'
#' This function converts the levels of a given factor into a numeric vector
#' @param x Input factor
#' @return Numeric vector
#' @examples as.numeric.factor()
as.numeric.factor <- function(x) {
  
  # Converts the levels of a given factor into a numeric vector
  as.numeric(levels(x))[x]
}

#' Calculate summary statistics
#'
#' This function calculates summary statistics  suitable for ggplot2.
#' Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#' Credits to www.cookbook-r.com
#' @param data A data frame
#' @param measurevar The name of the column that contains the variable to be summarized
#' @param groupvars  Vector containing names of columns that contain grouping variables
#' @param na.rm A boolean that indicates whether to ignore NA's. Defaults to FALSE
#' @param  conf.interval The percent range of the confidence interval (default is 95%)
#' @param .drop should combinations of variables that do not appear in the input data be preserved (FALSE) or dropped (TRUE, default)
#' @return A data frame with count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#' @import plyr
#' @examples summarySE()
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  # Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
  #
  # Credit: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
  #
  #   data: a data frame.
  #   measurevar: the name of a column that contains the variable to be summariezed
  #   groupvars: a vector containing names of columns that contain grouping variables
  #   na.rm: a boolean that indicates whether to ignore NA's. Defaults to FALSE
  #   conf.interval: the percent range of the confidence interval (default is 95%)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  colnames(datac) <- replace(colnames(datac), colnames(datac) == "mean", measurevar)
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

translate_distribution_plot_parameters <- function(groupSettings, sampleSettings, errorBarSettings, facetSettings,
                                                   colorSettings, styleSettings, GroupsList.level1, GroupsList.level2) {
  
  groups <- switch(
    as.numeric(groupSettings),
    GroupsList.level1,
    GroupsList.level2,
    GroupsList.level1,
    NULL
  )
  
  conditions <- switch(
    as.numeric(groupSettings),
    NULL,
    NULL,
    GroupsList.level2,
    NULL
  )
  
  plotType <- switch(
    as.numeric(sampleSettings),
    "grouped",
    "separate"
  )
  
  plotStyle <- switch(
    as.numeric(styleSettings),
    "line",
    "histogram",
    "scatter"
  )
  
  errorBars <- switch(
    as.numeric(errorBarSettings),
    "SD",
    "SEM",
    NULL
  )
  
  facetByGroup <- switch(
    as.numeric(facetSettings),
    TRUE,
    FALSE,
    TRUE,
    FALSE
  )
  
  facetByCondition <- switch(
    as.numeric(facetSettings),
    FALSE,
    TRUE,
    TRUE,
    FALSE
  )
  
  colorBy <- switch(
    as.numeric(colorSettings),
    "group",
    "condition",
    "separate"
  )
  
  
  if (is.null(groups) & is.null(conditions)) {
    facetByGroup <- FALSE
    facetByCondition <- FALSE
  }
  
  return(
    list(
      "groups" = groups,
      "conditions" = conditions,
      "plotType" = plotType,
      "plotStyle" = plotStyle,
      "errorBars" = errorBars,
      "facetByGroup" = facetByGroup,
      "facetByCondition" = facetByCondition,
      "colorBy" = colorBy
    )
  )
}

choose_distribution_plot_data_set <- function(dataSetType, dataSetFormat) {
  
  if (dataSetType == "1" & dataSetFormat == "1") {
    output <- sizeDF
  } else if (dataSetType == "1" & dataSetFormat == "2") {
    output <- sizeFreqDF
  } else if (dataSetType == "2" & dataSetFormat == "1") {
    output <- zpDF
  } else if (dataSetType == "2" & dataSetFormat == "2") {
    output <- zpFreqDF
  }
  
  return(output)
  
}

#' Convert the created data frames into ggplot2 compatible format
#'
#' Use this to acquire input data for ggplot2 plots of your own design
#' @param df The data frame holding the data to be visualized
#' @param groups Named list specifying groups
#' @param condition Optional parameter.  Named list specifying a second level of groups
#' (supplied as condtition = myCondition=. Default value is NULL.
#' @return Data frame in the long format
#' @export
#' @examples getGgplotDataframe()
getGgplotDataframe <- function(df, groups, condition = NULL, type = "distribution") {
  
  # If additive data plot specified, then the data must be first converted
  # This conversion works for both input data frame types (total number and fraction)
  if (type == "additive") {
    additiveDf <- df
    
    # Skipping the first column. If data is in the right format, first column is the 'scale'
    columnSums <- colSums(df[, 2:ncol(df)])
    
    for (i in 2:ncol(df)) {
      
      additiveSum <- 0
      for (j in 1:nrow(additiveDf)) {
        additiveDf[j,i] <- (additiveDf[j,i] / columnSums[i-1]) + additiveSum
        additiveSum <- additiveDf[j,i]
      }
    }
    colnames(additiveDf) <- colnames(df)
    df <- as.data.frame(additiveDf)
  }
  
  # If the string "ZP" is found in one of the column names of 'df', then the
  # length of the resulting vector is > 0 (len = 1 if correct data input).
  if (length(grep("mV", colnames(df))) > 0 ) {
    columnName <- "mV"
    
    # If "ZP" is not found, the function assumes it is a data frame of size data
  } else {
    columnName <- "nm"
  }
  
  if (length(groups) > 0) {
    
    groupsNames <- names(groups)
    groupsNamesVec <- vector(mode = "character")
    conditionVec <- vector(mode = "character")
    sampleNames <- vector(mode = "character")
    numEntries <- 0
    
    # If condition has been specified, prepares a named condition vector for 
    # ggplot-compatible data frame construction
    
    if (!is.null(condition)) {
      conditionList <- vector(mode = "character")
      conditionListNames <- vector(mode = "character")
      conditionNames <- names(condition)
      
      # Constructs conditionList - a vector of condition group names repeated as many times
      # as the corresponding condition group contains elements. ConditionListNames will contain
      # all sample names in the order of occurring groups. Basically, this just inverts the 
      # conditions list structure
      for (i in 1:length(condition)) {
        conditionList <- append(conditionList, rep(conditionNames[i], length(condition[[i]])))
        conditionListNames <- append(conditionListNames, unlist(condition[[i]]))
      }
      names(conditionList) <- conditionListNames
    }
    
    # Constructs vectors for ggplot-compatible data frame construction
    # - groupsNamesVec contains group names in the order specified in the input groups list
    # that are repeated for every data point (size bin) of every sample in given group
    # - sampleNames is a vector of all sample names specified in the groups list in that
    # particular order
    # - numEntries is the number of names specified in the groups list
    for (i in 1:length(groups)) {
      groupsNamesVec <- append(groupsNamesVec, rep(groupsNames[[i]], length(groups[[i]]) * nrow(df)))
      sampleNames <- append(sampleNames, groups[[i]])
    }
    numEntries <- length(sampleNames)
    
    # ScaleVec is a vector of scale values (ZP - mv or size - nm) that are repeated as many times
    # as there are samples
    scaleVec <- rep(df[[columnName]], numEntries)
    
    # sampleNamesVec is a vector of all sample names in the order they appear in groups list,
    # that are individually repeated for as many times as the number of rows of data they have
    sampleNamesVec <- unlist(lapply(sampleNames, function(x) rep(x, nrow(df))))
    
    # conditionVec is a vector of condition group names corresponding to each sample in the order
    # the samples appear in the ggplot-compatible data frame. The sapply function loops over the
    # constructed sampleNamesVec and adds to conditionvec a condition name, which corresponds to 
    # the given sample name in the conditionList (named vector)
    if (!is.null(condition)) {
      conditionVec <- sapply(sampleNamesVec, function(x) conditionList[[x]])
    }
    # Just in case remove the names which were transferred over from conditionList
    names(conditionVec) <- NULL
    
    # measurementVec is a vector of measured values (either size or ZP), which are taken from the
    # input data frame and concatenated into a single vector in the order of samples in groups list
    measurementVec <- vector(mode = "numeric")
    for (i in 1:length(sampleNames)) {
      measurementVec <- append(measurementVec, df[[sampleNames[[i]]]])
    }
    
    # Constructs the ggplot-compatible data frame from the constructed vectors
    plotDataframe <- data.frame(Group = factor(groupsNamesVec, levels=groupsNames),
                                Sample = sampleNamesVec,
                                Scale = scaleVec,
                                Num = measurementVec)
    
    # If condition was specified, adds the constructed contitionVec to the returnable data frame
    if (!is.null(condition)) {
      plotDataframe$Condition <- conditionVec
    }
    
  } else {
    
    sampleNames <- vector(mode = "character")
    groupsVec <- vector(mode = "character")
    
    # Constructs vectors for ggplot-compatible data frame construction
    for (i in 2:ncol(df)) {
      sampleNames <- append(sampleNames, rep(colnames(df)[i], nrow(df)))
      groupsVec <- append(groupsVec, rep(colnames(df)[i], nrow(df)))
      
    }
    
    # ScaleVec is a vector of scale values (ZP - mv or size - nm) that are repeated as many times
    # as there are samples
    scaleVec <- rep(df[[columnName]], ncol(df)-1)
    
    # measurementVec is a vector of measured values (either size or ZP), which are taken from the
    # input data frame and concatenated into a single vector in the order of samples in groups list
    measurementVec <- vector(mode = "numeric")
    for (i in 2:ncol(df)) {
      measurementVec <- append(measurementVec, df[,i])
    }
    
    # Constructs the ggplot-compatible data frame from the constructed vectors
    plotDataframe <- data.frame(Group = factor(groupsVec),
                                Sample = sampleNames,
                                Scale = scaleVec,
                                Num = measurementVec)
  }
  return(plotDataframe)
}

#' Plot the distribution graph
#'
#' Use this to plot distribution graphs
#' @param df The data frame holding the data to be visualized
#' @param groups Named list specifying groups. Default value is NULL.
#' @param condition Optional parameter. Named list specifying a second level of groups
#' (supplied as condtition = myCondition). Default value is NULL.
#' @param plotType Specifies whether samples should be grouped by the info supplied
#' with the 'groups' argument (plotType = 'grouped') or plotted separately (plotType = 'separate').
#' Defaults to 'separate'.
#' @param plotStyle Specifies the graphical style of the plot. Defaults to "line".
#' "line" - lineplot
#' "histogram" - histogram / barplot
#' "scatter" - dotplot
#' @param errorBars Argument for specifying error bar type. The default value is NULL, 
#' which results in no error bars. Possible values: 
#' “SD” - only group means will be plotted accopanied by standard deviation bars;
#' “SEM” - only group means will be plotted accopanied by standard error of the mean bars.
#' @param facetByGroup Boolean. Every group will be plotted on a separate sub-plot (TRUE).
#' All groups on the same large graph (FALSE). Defaults to FALSE.
#' @param facetByCondition Boolean. Every condition will be plotted on a separate sub-plot (TRUE).
#' All conditions on the same large graph (FALSE). Defaults to FALSE. Note that if both 
#' facetByGroup and facetByCondition are TRUE, then the graph will be separated both ways.
#' @param colorBy argument for specifying how the plot should be colored. By default this
#' argument is “group”. Three possible values: “group” – every group will have a separate color;
#' “condition” - every condition will have a separate color; “separate” - every sample will have 
#' a separate color. Please note that this option is only applicable when type = “separate”.
#' @return ggplot object
#' @import ggplot2
#' @export
#' @examples plotDistribution()
plotDistribution <- function(df, groups = NULL,  conditions = NULL, plotType = "separate", plotStyle = "line",
                             errorBars = NULL, facetByGroup = FALSE, facetByCondition = FALSE, colorBy = "group") {
  
  # Check supplied parametes
  if (!is.data.frame(df)) {
    stop("The supplied object is not a data frame")
    
  } else {
    
    if (!is.list(groups) && length(groups) > 0) {
      warning("The object supplied by 'groups' argument is not a list. Defaulting to NULL.")
      groups = NULL
    }
    if (!is.list(conditions) && length(conditions) > 0) {
      warning("The object supplied by 'conditions' argument is not a list. Defaulting to NULL.")
      conditions = NULL
    }
    if (!plotType %in% c("separate", "grouped") ) {
      warning(paste("Invalid value as 'plotType' parameter. The value can be either 'separate' or 'grouped",
                    "Defaulting plotType to 'separate'", sep = "\n"))
      plotType = "separate"
    }
    if (!plotStyle %in% c("line", "histogram", "scatter") ) {
      warning(paste("Invalid value as 'plotStyle' parameter. The value can be: 'line', 'histogram' or 'scatter",
                    "Defaulting plotStyle to 'line'", sep = "\n"))
      plotStyle = "line"
    }
    if (length(errorBars) > 0 && !errorBars %in% c("SD", "SEM") ) {
      warning(paste("Invalid value as 'errorBars' parameter. The value can be: 'SD', 'SEM' or NULL",
                    "Defaulting errorBars to NULL", sep = "\n"))
      errorBars = NULL
    }
    if (!is.logical(facetByGroup)) {
      warning("The facetByGroup parameter can be either TRUE or FALSE. Defaulting to FALSE")
      facetByGroup = FALSE
    }
    if (!is.logical(facetByCondition)) {
      warning("The facetByCondition parameter can be either TRUE or FALSE. Defaulting to FALSE")
      facetByCondition = FALSE
    }
    if (!colorBy %in% c("group", "condition", "separate") ) {
      warning(paste("Invalid value as 'colorBy' parameter. The value can be: 'group', 'condition' or 'separate",
                    "Defaulting colorBy to 'group'. If no groupig information is available, coloring will be separate", sep = "\n"))
      colorBy = "group"
    }
    
    # Determines if the input data frame contains size or ZP data
    # ZP data is plotted as a histogram; size data as a dotted lineplot
    if (length(grep("mV", colnames(df))) > 0 ) {
      measurmentType <- "mV"
    } else {
      measurmentType <- "nm"
    }
    
    # Recieves the ggplot-compatible data frame
    plotDataframe <- getGgplotDataframe(df, groups, conditions)
    
    # Determine Y-axis lable
    if (max(plotDataframe$Num) > 1) {
      yLab <- "Number of particles"
    } else {
      yLab <- "Fraction of total particles"
    }
    
    # If the user specifies error bars, 
    # either for standard error of the mean (SEM) or standard deviation (SD)
    if (length(groups) > 0 && plotType == "grouped") {
      # Determine whether all sample names supplied under condition and groups arguments are valid
      inputNames <- c(unlist(groups), unlist(conditions))
      names.ok <- TRUE
      for (i in 1:length(inputNames)) {
        if (!inputNames[i] %in% colnames(df)) {
          names.ok <- FALSE
        }
      }
      
      if (!names.ok) {
        stop(paste("One or more sample names supplied as groups or conditions is not correct!",
                   "Please check that the supplied names match table column names.", sep = "\n"))
      } 
      else {
        
        # If the user has requested to separate groups further by condition, this
        # will be taken into account in the SEM and SD calculations
        if (!is.null(conditions)) {
          plotDataframe$additionalGroup <- paste(plotDataframe$Group, plotDataframe$Condition, sep = "_")
          plotDataframeMod <- summarySE(plotDataframe, measurevar="Num", groupvars=c("Group", "Condition", "Scale", "additionalGroup"))
        } else {
          plotDataframeMod <- summarySE(plotDataframe, measurevar="Num", groupvars=c("Group", "Scale"))
        }  
        
        # If size data, generates the dotted line plot
        if (plotStyle == "line") {
          
          # Evaluates the coloring variable according to input parameter
          if (colorBy == "condition" && !is.null(conditions)) {
            p <- ggplot(plotDataframeMod, aes(x = Scale, y = Num, color = Condition, group = additionalGroup)) + 
              geom_line(position = position_dodge(nrow(plotDataframeMod) / length(unique(plotDataframeMod$Scale)))) +
              geom_point(position = position_dodge(nrow(plotDataframeMod) / length(unique(plotDataframeMod$Scale))), size = 1)
          } else {
            
            if (!is.null(conditions)) {
              p <- ggplot(plotDataframeMod, aes(x = Scale, y = Num, color = additionalGroup, group = additionalGroup))
            } else {
              p <- ggplot(plotDataframeMod, aes(x = Scale, y = Num, color = Group, group = Group))
            }
            
            p <- p + geom_line(position = position_dodge(nrow(plotDataframeMod) / length(unique(plotDataframeMod$Scale)))) +
              geom_point(position = position_dodge(nrow(plotDataframeMod) / length(unique(plotDataframeMod$Scale))), size = 1)      
          }
        } 
        else if (plotStyle == "histogram") {
          
          # Evaluates the coloring variable accordin to input parameter
          if (colorBy == "condition" && !is.null(conditions)) {
            p <- ggplot(plotDataframeMod, aes(x = Scale, y = Num, group = additionalGroup, fill = Condition)) + 
              geom_bar(position=position_dodge(), stat = "identity")  
          } else {
            
            if (!is.null(conditions)) {
              p <- ggplot(plotDataframeMod, aes(x = Scale, y = Num, color = additionalGroup, group = additionalGroup))
            } else {
              p <- ggplot(plotDataframeMod, aes(x = Scale, y = Num, color = Group, group = Group))
            }
            
            p <- ggplot(plotDataframeMod, aes(x = Scale, y = Num, group = Group, fill = Group)) + 
              geom_bar(position=position_dodge(), stat = "identity")
          }
        } 
        else if (plotStyle == "scatter") {
          
          # Evaluates the coloring variable accordin to input parameter
          if (colorBy == "condition" && !is.null(conditions)) {
            p <- ggplot(plotDataframeMod, aes(x = Scale, y = Num, color = Condition, group = additionalGroup)) + 
              suppressWarnings(sgeom_point(position = position_dodge(nrow(plotDataframeMod) / length(unique(plotDataframeMod$Scale))), size = 2))
          } else {
            
            if (!is.null(conditions)) {
              p <- ggplot(plotDataframeMod, aes(x = Scale, y = Num, color = additionalGroup, group = additionalGroup))
            } else {
              p <- ggplot(plotDataframeMod, aes(x = Scale, y = Num, color = Group, group = Group))
            }
            
            p <- ggplot(plotDataframeMod, aes(x = Scale, y = Num, color = Group, group = Group)) + 
              suppressWarnings(geom_point(position = position_dodge(nrow(plotDataframeMod) / length(unique(plotDataframeMod$Scale))), size = 2))      
          }
        }
        
        # SEM or SD bars are added to the plot if the user has requested some samples to be combined
        if (length(errorBars) > 0 && errorBars == "SEM") {
          if (plotStyle == "histogram") {
            p <- p + geom_errorbar(aes(ymin=Num-se, ymax=Num+se),
                                   position=position_dodge(),
                                   color = "black",
                                   size=0.2)
          } else {
            p <- p + geom_errorbar(aes(ymin=Num-se, ymax=Num+se),
                                   position=position_dodge(nrow(plotDataframeMod) / length(unique(plotDataframeMod$Scale))),
                                   color = "black",
                                   size=0.2)
          }
          
        } 
        else if (length(errorBars) > 0 && errorBars == "SD") {
          if (plotStyle == "histogram") {
            p <- p + geom_errorbar(aes(ymin=Num-sd, ymax=Num+sd),
                                   position=position_dodge(),
                                   color = "black",
                                   size=0.2)
          } else {
            p <- p + geom_errorbar(aes(ymin=Num-sd, ymax=Num+sd),
                                   position=position_dodge(nrow(plotDataframeMod) / length(unique(plotDataframeMod$Scale))),
                                   color = "black",
                                   size=0.2)
          }
        }
      }
    }
    
    # If the user has requested all samples to be plotted as individal lines
    if (length(groups) == 0 || plotType == "separate") {
      if (plotStyle == "line" || plotStyle == "scatter") {
        
        # Evaluates the coloring variable according to input parameter
        if (!is.null(conditions) && colorBy == "condition") {
          p <- ggplot(plotDataframe, aes(x = Scale, y = Num, color = Condition, group = Sample))
        } else if (colorBy == "separate") {
          p <- ggplot(plotDataframe, aes(x = Scale, y = Num, color = Sample, group = Sample))
        } else {
          p <- ggplot(plotDataframe, aes(x = Scale, y = Num, color = Group, group = Sample))
        }
        
      } 
      else if (plotStyle == "histogram") {
        print("YYY")
        
        # Evaluates the coloring variable accordin to input parameter
        if (colorBy == "condition" && !is.null(conditions)) {
          print("1")
          p <- ggplot(plotDataframe, aes(color = Condition, group = Sample, fill = Condition))
        } else if (colorBy == "separate") {
          print("2")
          p <- ggplot(plotDataframe, aes(color = Sample, group = Sample, fill = Sample))
        } else {
          print("3")
          p <- ggplot(plotDataframe, aes(color = Group, group = Sample, fill = Group))
        }
      }
      
      if (plotStyle == "line") {
        p <- p + geom_line(position = position_dodge(nrow(plotDataframe) / length(unique(plotDataframe$Scale)))) +
          geom_point(position = position_dodge(nrow(plotDataframe) / length(unique(plotDataframe$Scale))), size = 1)
      } 
      else if (plotStyle == "scatter") {
        p <- p + geom_point(position = position_dodge(10), size = 2)
      } 
      else if (plotStyle == "histogram") {
        if (measurmentType == "mV") {
          p <- p + suppressWarnings(geom_histogram(aes(x = Scale, weight = Num), 
                                                   position = "dodge", binwidth = 5))
        } else if (measurmentType == "nm") {
          print("NNN")
          p <- p + suppressWarnings(geom_histogram(aes(x = Scale, weight = Num), 
                                                   position = "dodge", binwidth = 30))
        }
      }
    }
    
    # If the user requested graphs to be separated both by group and condition
    if (facetByGroup == TRUE && facetByCondition == TRUE && !is.null(conditions)) {
      p <- p + facet_grid(Group ~ Condition)
      
      if (max(plotDataframe$Num) > 1) { # if total num
        p <- p + scale_y_continuous(labels = scales::scientific)
      }
    }
    
    # If the user requested graphs to be separated by condition
    else if (facetByCondition == TRUE && !is.null(conditions)) {
      p <- p + facet_grid(Condition ~ .) + 
        scale_x_continuous(breaks = pretty(plotDataframe$Scale, n = 10))
      
      if (max(plotDataframe$Num) > 1) {  # if total num
        p <- p + scale_y_continuous(labels = scales::scientific)
      }
    }
    
    # If the user requested graphs to be separated by group
    else if (facetByGroup == TRUE) {
      p <- p + facet_grid(Group ~ .) + 
        scale_x_continuous(breaks = pretty(plotDataframe$Scale, n = 10))
      
      if (max(plotDataframe$Num) > 1) { # if total num
        p <- p + scale_y_continuous(labels = scales::scientific)
      }
      
    } else {
      if (max(plotDataframe$Num) > 1) {  # if total num
        p <- p + scale_y_continuous(breaks = pretty(plotDataframe$Num, n = 10), labels = scales::scientific)
      } else {
        p <- p + scale_y_continuous(breaks = pretty(plotDataframe$Num, n = 10))
      }
    }
    
    p <- p +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "grey45", linetype = "dashed", size = 0.1),
            axis.text = element_text(color = "black"),
            axis.line = element_line(color="black", size = 0.3),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.text=element_text(size=12),
            legend.title = element_text(size=12),
            plot.margin=unit(c(1, 1, 1, 1), "cm")) +
      ylab(yLab) +
      xlab(measurmentType)
    
    
    if (measurmentType == "nm") {
      p <- p + scale_x_continuous(breaks = pretty(plotDataframe$Scale, n = 200))
    } else {
      p <- p + scale_x_continuous(breaks = pretty(plotDataframe$Scale, n = 20))
    }
    
    return(p)
  }
}

translate_additive_plot_parameters <- function(groupSettings, sampleSettings, errorBarSettings, facetSettings,
                                                   colorSettings, GroupsList.level1, GroupsList.level2) {
  
  
  groups <- switch(
    as.numeric(groupSettings),
    GroupsList.level1,
    GroupsList.level2,
    GroupsList.level1,
    NULL
  )
  
  conditions <- switch(
    as.numeric(groupSettings),
    NULL,
    NULL,
    GroupsList.level2,
    NULL
  )
  
  plotType <- switch(
    as.numeric(sampleSettings),
    switch(
      as.numeric(errorBarSettings),
      "SD",
      "SEM",
      "SD"),
    "separate"
  )
  
  meansOnly <- switch(
    as.numeric(errorBarSettings),
    FALSE,
    FALSE,
    TRUE
  )
  
  facetByGroup <- switch(
    as.numeric(facetSettings),
    TRUE,
    FALSE,
    TRUE,
    FALSE
  )
  
  facetByCondition <- switch(
    as.numeric(facetSettings),
    FALSE,
    TRUE,
    TRUE,
    FALSE
  )
  
  colorBy <- switch(
    as.numeric(colorSettings),
    "group",
    "condition",
    "separate"
  )
  
  
  if (is.null(groups) & is.null(conditions)) {
    facetByGroup <- FALSE
    facetByCondition <- FALSE
  }
  
  return(
    list(
      "groups" = groups,
      "conditions" = conditions,
      "plotType" = plotType,
      "meansOnly" = meansOnly,
      "facetByGroup" = facetByGroup,
      "facetByCondition" = facetByCondition,
      "colorBy" = colorBy
    )
  )
}

choose_additive_plot_data_set <- function(dataSetType) {
  
  if (dataSetType == "1") {
    output <- sizeFreqDF
  } else if (dataSetType == "2") {
    output <- zpFreqDF
  }
  
  return(output)
  
}

#' Plot the additive graph
#'
#' Use this to plot additive curve graphs
#' @param df The data frame holding the data to be visualized
#' @param groups Named list specifying groups. Default value is NULL.
#' @param condition Optional parameter.  Named list specifying a second level of groups
#' (supplied as condtition = myCondition). Default value is NULL.
#' @param type Argument for specifying plot type. The default value is “SD”. Possible values: 
#' “SD” - only group means will be plotted accopanied by standard deviation bars;
#' “SEM” - only group means will be plotted accopanied by standard error of the mean bars;
#' “separate” - all samples will be plottes as individual lines / bars
#' @param meansOnly Boolean. If FALSE plots a hue of SD or SEM values around the mean line.
#' If TRUE, only the mean line will be plotted. Defaults to FALSE.
#' @param facetByGroup Boolean. Every group will be plotted on a separate sub-plot (TRUE).
#' All groups on the same large graph (FALSE). Defaults to FALSE.
#' @param facetByCondition Boolean. Every condition will be plotted on a separate sub-plot (TRUE).
#' All conditions on the same large graph (FALSE). Defaults to FALSE. Note that if both 
#' facetByGroup and facetByCondition are TRUE, then the graph will be separated both ways.
#' @param colorBy argument for specifying how the plot should be colored. By default this
#' argument is “group”. Three possible values: “group” – every group will have a separate color;
#' “condition” - every condition will have a separate color; “separate” - every sample will have 
#' a separate color. Note that this option is only applicable when type = “separate”.
#' @return ggplot object
#' @export
#' @examples plotAdditive()
plotAdditive <- function(df, groups = NULL, conditions = NULL, type = "SD", meansOnly = FALSE, 
                         facetByGroup = FALSE, facetByCondition = FALSE, colorBy = "group") {
  
  # Check supplied parametes
  if (!is.data.frame(df)) {
    stop("The supplied object is not a data frame")
    
  } else {
    
    if (!is.list(groups) && length(groups) > 0) {
      warning("The object supplied by 'groups' argument is not a list. Defaulting to NULL.")
      groups = NULL
    }
    if (!is.list(conditions) && length(conditions) > 0) {
      warning("The object supplied by 'conditions' argument is not a list. Defaulting to NULL.")
      conditions = NULL
    }
    if (!type %in% c("SD", "SEM", "separate") ) {
      warning(paste("Invalid value as 'type' parameter. The value can be: 'SD', 'SEM' or 'separate",
                    "Defaulting type to 'separate'", sep = "\n"))
      type = "separate"
    }
    if (!is.logical(facetByGroup)) {
      warning("The facetByGroup parameter can be either TRUE or FALSE. Defaulting to FALSE")
      facetByGroup = FALSE
    }
    if (!is.logical(facetByCondition)) {
      warning("The facetByCondition parameter can be either TRUE or FALSE. Defaulting to FALSE")
      facetByCondition = FALSE
    }
    if (!is.logical(meansOnly)) {
      warning("The meansOnly parameter can be either TRUE or FALSE. Defaulting to FALSE")
      meansOnly = FALSE
    }
    if (!colorBy %in% c("group", "condition", "separate") ) {
      warning(paste("Invalid value as 'colorBy' parameter. The value can be: 'group', 'condition' or 'separate",
                    "Defaulting colorBy to 'group'. If no grouping information is available, coloring will be separate", sep = "\n"))
      colorBy = "group"
    }
    
    # Determines if the input data frame contains size or ZP data
    # in order to assign correct plot lables
    if (length(grep("mV", colnames(df))) > 0 ) {
      measurmentType <- "mV"
    } else {
      measurmentType <- "nm"
    }
    
    # Recieves the ggplot-compatible data frame
    plotDataframe <- getGgplotDataframe(df, groups, conditions, type = "additive")
    
    if (length(groups) > 0 && !type == "separate") {
      # If the user specifies error bars, 
      # either for standard error of the mean (SEM) or standard deviation (SD)
      if (type == "SEM" | type == "SD") {
        
        # If the user has requested to separate groups further by condition, this
        # will be taken into account in the SEM and SD calculations    
        if (facetByCondition == TRUE && !is.null(conditions)) {
          
          plotDataframeMod <- summarySE(plotDataframe, measurevar="Num", groupvars=c("Group", "Scale", "Condition"))
          
        } else {
          
          plotDataframeMod <- summarySE(plotDataframe, measurevar="Num", groupvars=c("Group", "Scale"))
        }
        
        # Evaluates the coloring variable accordin to input parameter
        if (colorBy == "condition" && !is.null(conditions)) {
          
          p <- ggplot(plotDataframeMod) +
            geom_line(aes(x = Scale, y = Num, color = Condition))
          
        } else {
          
          if (!is.null(conditions)) {
            p <- ggplot(plotDataframeMod) +
              geom_line(aes(x = Scale, y = Num, color = additionalGroup, group = additionalGroup))
          } else {
            p <- ggplot(plotDataframeMod) +
              geom_line(aes(x = Scale, y = Num, color = Group))          
          }
        }
        
        # Generates line plot
        p <- ggplot(plotDataframeMod) +
          geom_line(aes(x = Scale, y = Num, color = Group))
        
        # SEM and SD ribbons are plotted if not requested otherwise
        if (!meansOnly) {
          if (type == "SEM") {
            
            p <- p + geom_ribbon(aes(x=Scale, ymin = Num - se, ymax = Num + se, 
                                     color = Group, fill = Group), linetype = 0, alpha=0.25)
            
          } else if (type == "SD") {
            
            p <- p + geom_ribbon(aes(x=Scale, ymin = Num - sd, ymax = Num + sd, 
                                     color = Group, fill = Group), linetype = 0, alpha=0.25)
          }
        }
        
        # If the user requested graphs to be separated both by group and condition  
        if (facetByGroup == TRUE && facetByCondition == TRUE && !is.null(conditions)) {
          
          p <- p + facet_grid(Group ~ Condition)
          
          # If the user requested graphs to be separated by condition            
        } else if (facetByGroup == TRUE) {
          p <- p + facet_grid(Group ~ .)
          
          # If the user requested graphs to be separated by condition    
        } else if (facetByCondition == TRUE && !is.null(conditions)) {
          
          p <- p + facet_grid(Condition ~ .)
          
        }
      }
    }
    
    # If the user has requested all samples to be plotted as individal lines
    if (length(groups) == 0 || type == "separate") {
      
      # Evaluates the coloring variable accordin to input parameter
      if (colorBy == "condition" && !is.null(conditions)) {
        
        p <- ggplot(plotDataframe) +
          geom_line(aes(x = Scale, y = Num, color = Condition, group = Sample))
        
      } else if (length(groups) > 0 && colorBy == "group") {
        
        p <- ggplot(plotDataframe) +
          geom_line(aes(x = Scale, y = Num, color = Group, group = Sample))
        
      } else if (length(groups) > 0 && colorBy == "separate") {
        
        p <- ggplot(plotDataframe) +
          geom_line(aes(x = Scale, y = Num, color = Sample, group = Sample))
        
      } else {
        
        p <- ggplot(plotDataframe) +
          geom_line(aes(x = Scale, y = Num, color = Group, group = Sample))
      }
      
      # If the user requested graphs to be separated both by group and condition    
      if (facetByGroup == TRUE && facetByCondition == TRUE && !is.null(conditions) && !is.null(groups)) {
        
        p <- p + facet_grid(Group ~ Condition)
        
        # If the user requested graphs to be separated by condition      
      } else if (facetByCondition == TRUE && !is.null(conditions)) {
        
        p <- p + facet_grid(Condition ~ .)
        
        
        # If the user requested graphs to be separated by group  
      } else if (facetByGroup == TRUE && !is.null(groups)) {
        p <- p + facet_grid(Group ~ .)
        
      } else {
        p <- p + scale_y_continuous(breaks = pretty(plotDataframe$Num, n = 10))
      }
      
    }
    
    if (measurmentType == "nm") {
      p <- p + scale_x_continuous(breaks = pretty(plotDataframe$Scale, n = 200))
    } else {
      p <- p + scale_x_continuous(breaks = pretty(plotDataframe$Scale, n = 20))
    }
    
    p <- p +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "grey45", linetype = "dashed", size = 0.1),
            axis.text = element_text(color = "black"),
            axis.line = element_line(color="black", size = 0.3),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.text=element_text(size=12),
            legend.title = element_text(size=12),
            plot.margin=unit(c(1, 1, 1, 1), "cm")) +
      ylab("Fraction") +
      xlab(measurmentType)
    
    return(p)
  }
}
