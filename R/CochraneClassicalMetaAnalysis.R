#
# Copyright (C) 2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

CochraneClassicalMetaAnalysis   <- function(jaspResults, dataset, options, state = NULL) {
  saveRDS(options, file = "C:/Projects/JASP/jasp-R-debug/options.RDS")

  # load the database
  if (is.null(jaspResults[["database"]]))
    .cochraneLoadDatabase(jaspResults)
  
  # select data
  if (is.null(jaspResults[["dataset"]]))
    .cochraneSelectDataset(jaspResults, options)
  
  dataset <- jaspResults[["dataset"]]$object
  
  # add data
  if (options[["addStudy"]])
    dataset <- .cochraneAddData(dataset, options)
  
  # sort data
  dataset <- .cochraneSortData(dataset, options)
  
  # overview table
  if (is.null(jaspResults[["selectedOverviewTable"]]))
    .cochraneSelectedOverviewTable(jaspResults, options)
  
  
  # reusing classical meta-analysis
  options <- .cochraneEmulateMetaAnalysisOptions(options)
  
  
  ready <- .cochraneReady(options, dataset)
  
  if (options[["analyzeData"]] %in% c("reviews", "metaAnalyses")){
    
    selectWith <- if(options[["analyzeData"]] == "metaAnalyses") "titleMetaAnalysis" else "titleReview"
    selection  <- unique(dataset[,selectWith])
    selection  <- selection[selection != "_add"]
    
    startProgressbar(length(selection))
    
    for (title in selection){
      
      tempDataset   <- dataset[dataset[,selectWith] %in% c("_add", title),]
      tempContainer <- .cochraneGetOutputContainer(jaspResults, title)
      
      # overview figures
      if (options[["plotEffectSizes"]])
        .cochraneDecriptivePlot(tempContainer, tempDataset, "effectSize")
      
      # overview figures
      if (options[["plotSampleSizes"]])
        .cochraneDecriptivePlot(tempContainer, tempDataset, "sampleSize")
      
      .ClassicalMetaAnalysisCommon(tempContainer, tempDataset, ready, options)
      
      progressbarTick()
    }
    
  } else if (options[["analyzeData"]] == "together"){
    
    container <- .cochraneGetOutputContainer(jaspResults)
    
    # overview figures
    if (options[["plotEffectSizes"]])
      .cochraneDecriptivePlot(container, dataset, "effectSize")
    
    # overview figures
    if (options[["plotSampleSizes"]])
      .cochraneDecriptivePlot(container, dataset, "sampleSize")
    
    .ClassicalMetaAnalysisCommon(container, dataset, ready, options)
  }

  
  return()
}

.cochraneLoadDatabase          <- function(jaspResults){
  
  database         <- createJaspState()
  database$object  <- readRDS("C:/Projects/JASP/jaspCochraneMetaAnalyses/R/resources/database.RDS")
  jaspResults[["database"]] <- database
  
  return()
}
.cochraneAddData               <- function(dataset, options){
  
  additionalEstimates <- sapply(options[["additionalStudies"]], function(add)add[["values"]], simplify = FALSE)
  additionalEstimates <- data.frame(do.call(rbind, additionalEstimates))
  colnames(additionalEstimates) <- c("effectSize",  "effectSE", "titleStudy", "studyYear")
  additionalEstimates[,"titleStudy"] <- paste0("_add", additionalEstimates[,"titleStudy"])
  additionalEstimates <- cbind(additionalEstimates, doi = "_add", titleReview = "_add", titleMetaAnalysis = "_add", sampleSize = NA)
  additionalEstimates <- additionalEstimates[,colnames(dataset)]
  
  dataset <- rbind(dataset, additionalEstimates)
  return(dataset)
}
.cochraneSortData              <- function(dataset, options){
  
  if(options[["forestPlotOrder"]] == "yearAscending"){
    dataset <- dataset[order(dataset[,"studyYear"]),]
  }else if(options[["forestPlotOrder"]] == "yearDescending"){
    dataset <- dataset[order(dataset[,"studyYear"], decreasing = TRUE),]
  }else if(options[["forestPlotOrder"]] == "effectSizeAscending"){
    dataset <- dataset[order(dataset[,"effectSize"]),]
  }else if(options[["forestPlotOrder"]] == "effectSizeDescending"){
    dataset <- dataset[order(dataset[,"effectSize"], decreasing = TRUE),]
  }
  
  return(dataset)
}
.cochraneSelectDataset         <- function(jaspResults, options){
  
  # create a notifier for updating the dataset
  dataset <- createJaspState()
  dataset$dependOn(c("selectionType", "topicsSelected", "keywordsSelected", "textSearch", "analyzeData"))
  jaspResults[["dataset"]] <- dataset
  
  # create an object for storing dataset overview
  if (is.null(jaspResults[["datasetOverview"]])) {
    datasetOverview <- createJaspState()
    jaspResults[["datasetOverview"]] <- datasetOverview
  } else
    datasetOverview <- jaspResults[["datasetOverview"]]
  
  
  # select the datasets and indexing object
  studies  <- jaspResults[["database"]]$object[["studies"]]
  indexing <- jaspResults[["database"]]$object[[if(options[["analyzeData"]] == "metaAnalyses") "metaAnalyses" else "reviews"]]
  
  if (options[["selectionType"]] == "selectionTopics") {
    
    selectedTitles <- unname(unlist(sapply(indexing, function(indx){
      if (indx[["topic"]] %in% options[["topicsSelected"]])
        return(indx[["title"]])
      else
        return(NULL)
    })))
    
  } else if (options[["selectionType"]] == "selectionKeywords") {
    
    selectedTitles <- unname(unlist(sapply(indexing, function(indx){
      if (any(indx[["keywords"]] %in% options[["keywordsSelected"]]))
        return(indx[["title"]])
      else
        return(NULL)
    })))
    
  } else if (options[["selectionType"]] == "selectionTextSearch") {
    
    textSearch <- options[["textSearch"]] 
    textSearch <- gsub("\n", ",", gsub(";", ",", gsub(" ", ",", textSearch)))
    textSearch <- unlist(strsplit(textSearch, split = ","))
    textSearch <- textSearch[textSearch != ""]
    textSearch <- tolower(textSearch)
    
    searchPositive <- textSearch[substr(textSearch, 1, 1) != "-"]
    searchNegative <- textSearch[substr(textSearch, 1, 1) == "-"]
    searchNegative <- substr(searchNegative, 2, nchar(searchNegative))
    
    indexingTitles <- tolower(unname(sapply(indexing, function(indx)indx[["title"]])))
    
    textSearchPositive <- apply(matrix(sapply(searchPositive, function(text){
      return(grepl(text, indexingTitles, fixed = TRUE))
    }), ncol = length(searchPositive)), 1, any)
    
    textSearchNegative <- apply(matrix(sapply(searchNegative, function(text){
      return(grepl(text, indexingTitles, fixed = TRUE))
    }), ncol = length(searchNegative)), 1, any)
    
    if (length(textSearchNegative) == 0)
      selectedTitles <- sapply(indexing, function(inx)inx[["title"]])[textSearchPositive]
    else
      selectedTitles <- sapply(indexing, function(inx)inx[["title"]])[textSearchPositive & !textSearchNegative]
    
  }
  
  datasetOverview$object <- indexing[selectedTitles]
  dataset$object         <- studies[studies[[if(options[["analyzeData"]] == "metaAnalyses") "titleMetaAnalysis" else "titleReview"]] %in% selectedTitles, ]
  
  return()
}
.cochraneSelectedOverviewTable <- function(jaspResults, options){
  
  datasetOverview      <- jaspResults[["datasetOverview"]]$object
  
  selectedOverviewTable <- createJaspTable(
    title = if(options[["analyzeData"]] == "metaAnalyses") gettext("Meta-Analyses") else gettext("Review"))
  selectedOverviewTable$addColumnInfo(name = "name",     title = gettext("Title"),             type = "string")
  selectedOverviewTable$addColumnInfo(name = "year",     title = gettext("Year"),              type = "integer")
  selectedOverviewTable$addColumnInfo(name = "nStudies", title = gettext("Number of studies"), type = "integer")
  selectedOverviewTable$position <- 1
  selectedOverviewTable$dependOn(c("selectionType", "topicsSelected", "keywordsSelected", "textSearch", "analyzeData"))
  jaspResults[["selectedOverviewTable"]] <- selectedOverviewTable
  
  
  for (overview in datasetOverview)
    selectedOverviewTable$addRows(with(overview, list(
      name      = title,
      year      = year,
      nStudies  = nStudies
    )))
  
  return()
}
.cochraneEmulateMetaAnalysisOptions    <- function(options){
  options[["dependent"]]       <- "effectSize"
  options[["wlsWeights"]]      <- "effectSE"
  options[["includeConstant"]] <- TRUE
  options[["studyLabels"]]     <- "titleStudy"
  options[["factors"]]         <- list()
  options[["covariates"]]      <- list()
  options[["modelTerms"]]      <- list()
  options[["components"]]      <- list()
  
  return(options)
}
.cochraneReady                 <- function(options, dataset){
  if (options[["selectionType"]] == "selectionTopics")
    return(length(options[["topicsSelected"]]) > 0 && nrow(dataset) > 0)
  else if (options[["selectionType"]] == "selectionKeywords")
    return(length(options[["keywordsSelected"]]) > 0 && nrow(dataset) > 0)
  else if (options[["selectionType"]] == "selectionTextSearch")
    return(nchar(options[["textSearch"]]) > 0 && nrow(dataset) > 0)
}
.cochraneDecriptivePlot        <- function(container, dataset, variable){
  
  if (!is.null(container[[paste0(variable,"Plot")]]))
    return()
  
  
  descriptivePlot <- createJaspPlot(
    plot         = jaspDescriptives:::.plotMarginalCorDescriptives(
      dataset[[variable]],
      xName = if(variable == "effectSize") gettext("Effect Size") else gettext("Sample Size"),
      yName = gettext("Density")),
    width        = 300,
    aspectRatio  = 1,
    title        = if(variable == "effectSize") gettext("Effect Sizes") else gettext("Sample Sizes"),
    position     = if(variable == "effectSize") -2 else -1,
    dependencies = if(variable == "effectSize") "plotEffectSizes" else "plotSampleSizes"
  )
  
  container[[paste0(variable,"Plot")]] <- descriptivePlot
  
  return()
}
.cochraneGetOutputContainer    <- function(jaspResults, title = ""){
  if (!is.null(jaspResults[[paste0("modelContainer",title)]])) {
    modelContainer <- jaspResults[[paste0("modelContainer",title)]]
  } else {
    modelContainer <- createJaspContainer(title)
    modelContainer$dependOn(c("selectionType", "topicsSelected", "keywordsSelected", "textSearch",
                              "analyzeData", "addStudy", "additionalStudies", 
                              "forestPlotOrder", "studyLabels",
                              "method", "test", "regressionCoefficientsConfidenceIntervalsInterval"))
    jaspResults[[paste0("modelContainer",title)]] <- modelContainer
  }
  return(modelContainer)
}

# test
if(FALSE){
  library(jaspTools)
  library(jaspResults)
  setPkgOption('module.dirs', "C:/Projects/JASP/jaspMetaAnalysis")
  options <- jaspTools::analysisOptions("ClassicalMetaAnalysis")
  options <- readRDS("C:/Projects/JASP/jasp-R-debug/options.RDS")
}
