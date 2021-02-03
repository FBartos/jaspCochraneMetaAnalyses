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
  
  ### work with the database and dependent menus
  # load the database
  if (is.null(jaspResults[["database"]]))
    .cochraneLoadDatabase(jaspResults)
  
  # create the qml options based on the database
  if(is.null(jaspResults[["sourceTopics"]]))
    .cochraneCreateDatabaseTopics(jaspResults)
  if(is.null(jaspResults[["sourceKeywords"]]))
    .cochraneCreateDatabaseKeywords(jaspResults, options)
  

  ### create data set based on the database and selection
  # select data
  if (is.null(jaspResults[["dataset"]]))
    selectedDataset <- .cochraneSelectDataset(jaspResults, options)
  else
    selectedDataset <- jaspResults[["dataset"]][["object"]]
  
  # sort the data for the forest plots
  selectedDataset   <- .cochraneSortData(selectedDataset, options)
  
  # prepare additional qml gadget based on the selected dataset
  if (is.null(jaspResults[["selectionGadget"]]))
    .cochraneCreateSelectorGadget(jaspResults, selectedDataset)

  # overview table
  if (is.null(jaspResults[["selectedOverviewTable"]]))
    .cochraneSelectedOverviewTable(jaspResults, options)
  
  
  ### based with the restricted / modified dataset based with the additional studies + restrictions
  # applying the additional selection done from the check box interface
  dataset <- .cochraneRestrictDataset(selectedDataset, options)
  
  # add data
  if (options[["addStudy"]])
    dataset <- .cochraneAddData(dataset, options)
  
  
  # reusing classical meta-analysis
  options <- .cochraneEmulateMetaAnalysisOptions(options)
  ready   <- .cochraneReady(options, dataset)
  
  # apply the classical meta-analysis to the dataset
  if (options[["analyzeData"]] %in% c("reviews", "metaAnalyses")){
    
    selectWith <- if(options[["analyzeData"]] == "metaAnalyses") "titleMetaAnalysis" else "titleReview"
    selection  <- unique(dataset[,selectWith])
    selection  <- selection[selection != "_add"]
    
    startProgressbar(length(selection))
    
    for (title in sort(selection, decreasing = TRUE)){
      
      tempDataset   <- dataset[dataset[,selectWith] %in% c("_add", title),]
      tempContainer <- .cochraneGetOutputContainer(jaspResults, title)
      
      # overview figures
      if (options[["plotEffectSizes"]])
        .cochraneDecriptivePlot(tempContainer, tempDataset, "effectSize", options)
      
      # overview figures
      if (options[["plotSampleSizes"]])
        .cochraneDecriptivePlot(tempContainer, tempDataset, "sampleSize", options)
      
      .ClassicalMetaAnalysisCommon(tempContainer, tempDataset, ready, options)
      
      progressbarTick()
    }
    
  } else if (options[["analyzeData"]] == "together"){
    
    container <- .cochraneGetOutputContainer(jaspResults)
    
    # overview figures
    if (options[["plotEffectSizes"]])
      .cochraneDecriptivePlot(container, dataset, "effectSize", options)
    
    # overview figures
    if (options[["plotSampleSizes"]])
      .cochraneDecriptivePlot(container, dataset, "sampleSize", options)
    
    .ClassicalMetaAnalysisCommon(container, dataset, ready, options)
  }

  
  return()
}

.cochraneDataDependencies       <- c("selectionType", "topicsSelected", "keywordsSelected", "textSearch", "analyzeData",
                                     "addStudy", "additionalStudies", "selectionGadget")
.cochraneLoadDatabase           <- function(jaspResults){
  
  database         <- createJaspState()
  database$object  <- readRDS("C:/Projects/JASP/jaspCochraneMetaAnalyses/R/resources/database.RDS")
  jaspResults[["database"]] <- database
  
  return()
}
.cochraneAddData                <- function(dataset, options){
  
  if (length(options[["additionalStudies"]]) == 0)
    return(dataset)
    
  additionalEstimates <- sapply(options[["additionalStudies"]], function(study)unlist(study), simplify = F)
  additionalEstimates <- data.frame(do.call(rbind, additionalEstimates))

  for (i in 1:ncol(additionalEstimates))
    additionalEstimates[,i] <- as.character(additionalEstimates[,i])
  
  for (col in c("effectSize", "effectSE", "lCI", "uCI"))
    additionalEstimates[,col] <- as.numeric(additionalEstimates[,col])
  
  additionalEstimates <- additionalEstimates[!is.na(additionalEstimates[,"effectSize"]),]
  additionalEstimates <- additionalEstimates[!is.na(additionalEstimates[,"effectSE"]) | (!is.na(additionalEstimates[,"lCI"]) & !is.na(additionalEstimates[,"uCI"])),]
  
  if (nrow(additionalEstimates) == 0)
    return(dataset)
  
  for (i in 1:nrow(additionalEstimates))
    if (is.na(additionalEstimates[i,"effectSE"]) && all(is.numeric(unlist(additionalEstimates[i, c("lCI", "uCI")])))){
      if (additionalEstimates[i,"lCI"] > additionalEstimates[i,"effectSize"] || additionalEstimates[i,"uCI"] < additionalEstimates[i,"effectSize"])
        .quitAnalysis(gettext("The effect size does not lie within the confidence interval in one of the specified studies."))
      additionalEstimates[i,"effectSE"] <- (additionalEstimates[i,"uCI"] - additionalEstimates[i,"lCI"]) / (qnorm(.975) * 2)
    }
      
  
  if (any(additionalEstimates[,"effectSE"] < 0))
    .quitAnalysis(gettext("One of the specified studies has a negative standard error."))
  
  additionalEstimates <- additionalEstimates[,c("effectSize",  "effectSE", "titleStudy")]
  additionalEstimates$titleStudy        <- paste0("_add", additionalEstimates$titleStudy)
  additionalEstimates$studyYear         <- NA
  additionalEstimates$doi               <- "_add" 
  additionalEstimates$titleReview       <- "_add"
  additionalEstimates$titleMetaAnalysis <- "_add"
  additionalEstimates$sampleSize        <- NA
  additionalEstimates <- additionalEstimates[,colnames(dataset)]

  dataset <- rbind(dataset, additionalEstimates)
  
  return(dataset)
}
.cochraneSortData               <- function(dataset, options){
  
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
.cochraneSelectDataset          <- function(jaspResults, options){
  
  # create a notifier for updating the dataset
  dataset <- createJaspState()
  dataset$dependOn(c("selectionType", "topicsSelected", "keywordsSelected", "textSearch", "keywordsSearch", "analyzeData"))
  jaspResults[["dataset"]] <- dataset
  
  # create an object for storing dataset overview
  if (is.null(jaspResults[["datasetOverview"]])) {
    datasetOverview <- createJaspState()
    datasetOverview$dependOn(c("selectionType", "topicsSelected", "keywordsSelected", "textSearch", "keywordsSearch", "analyzeData"))
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
  selectedDataset        <- studies[studies[[if(options[["analyzeData"]] == "metaAnalyses") "titleMetaAnalysis" else "titleReview"]] %in% selectedTitles, ]
  dataset[["object"]]    <- selectedDataset
  
  return(selectedDataset)
}
.cochraneRestrictDataset        <- function(dataset, options){
  
  # skip the restriction step unless the selector gadget was created
  if (length(options$selectionGadget) == 0)
    return(NULL)
  
  # skip the restriction step unless at least one study was selected
  if (all(sapply(options[["selectionGadget"]], function(item)!item[["selected"]])))
    return(NULL)
  
  # skip the removal of data sets if all are selected  
  if (all(sapply(options[["selectionGadget"]], function(item)item[["selected"]])))
    return(dataset)

  # do the selection
  selected <- unlist(sapply(options[["selectionGadget"]], function(item)item[["value"]][item[["selected"]]]))
  
  if (options[["analyzeData"]] == "metaAnalyses")
    dataset <- dataset[dataset[,"titleMetaAnalysis"] %in% selected, ]
  else
    dataset <- dataset[dataset[,"titleReview"]       %in% selected, ]
  
  return(dataset)
}
.cochraneSelectedOverviewTable  <- function(jaspResults, options){
  
  datasetOverview      <- jaspResults[["datasetOverview"]]$object
  
  selectedOverviewTable <- createJaspTable(
    title = if(options[["analyzeData"]] == "metaAnalyses") gettext("Meta-Analyses") else gettext("Review"))
  selectedOverviewTable$addColumnInfo(name = "name",     title = gettext("Title"),             type = "string")
  selectedOverviewTable$addColumnInfo(name = "year",     title = gettext("Year"),              type = "integer")
  selectedOverviewTable$addColumnInfo(name = "nStudies", title = gettext("Number of studies"), type = "integer")
  selectedOverviewTable$position <- 1
  selectedOverviewTable$dependOn(.cochraneDataDependencies)
  jaspResults[["selectedOverviewTable"]] <- selectedOverviewTable
  
  
  for (overview in datasetOverview[order(sapply(datasetOverview, function(item)item[["title"]]))])
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
.cochraneReady                  <- function(options, dataset){
  
  # don't even try running the analysis before the selector gadget was generated and updated
  if (length(options$selectionGadget) == 0)
    return(FALSE)
  
  if (options[["selectionType"]] == "selectionTopics")
    return(length(options[["topicsSelected"]]) > 0 && nrow(dataset) > 0)
  else if (options[["selectionType"]] == "selectionKeywords")
    return(length(options[["keywordsSelected"]]) > 0 && nrow(dataset) > 0)
  else if (options[["selectionType"]] == "selectionTextSearch")
    return(nchar(options[["textSearch"]]) > 0 && nrow(dataset) > 0)
}
.cochraneDecriptivePlot         <- function(container, dataset, variable, options){
  
  if (!is.null(container[[paste0(variable,"Plot")]]))
    return()
  
  saveRDS(dataset, file = "C:/Projects/JASP/jasp-R-debug/dataset.RDS")
  descriptivePlot <- createJaspPlot(
    plot         = .plotMarginal(#jaspDescriptives:::.plotMarginal(
      column         = dataset[[variable]],
      variableName   = if(variable == "effectSize") gettext("Effect Size") else gettext("Sample Size"),
      displayDensity = options[["distPlotDensity"]],
      rugs           = options[["distPlotRug"]],
      binWidthType   = options[["binWidthType"]],
      numberOfBins   = options[["numberOfBins"]]),
    width        = 300,
    aspectRatio  = 1,
    title        = if(variable == "effectSize") gettext("Effect Sizes") else gettext("Sample Sizes"),
    position     = if(variable == "effectSize") -2 else -1,
    dependencies = c("distPlotDensity", "distPlotRug", "binWidthType", "numberOfBins", if(variable == "effectSize") "plotEffectSizes" else "plotSampleSizes")
  )
  
  container[[paste0(variable,"Plot")]] <- descriptivePlot
  
  return()
}
.cochraneGetOutputContainer     <- function(jaspResults, title = ""){
  if (!is.null(jaspResults[[paste0("modelContainer",title)]])) {
    modelContainer <- jaspResults[[paste0("modelContainer",title)]]
  } else {
    modelContainer <- createJaspContainer(title)
    modelContainer$dependOn(c(.cochraneDataDependencies, "addStudy", "additionalStudies", 
                              "forestPlotOrder", "studyLabels",
                              "method", "test", "regressionCoefficientsConfidenceIntervalsInterval"))
    jaspResults[[paste0("modelContainer",title)]] <- modelContainer
  }
  return(modelContainer)
}
.cochraneCreateDatabaseTopics   <- function(jaspResults){
  
  database <- jaspResults[["database"]]$object
  
  jaspResults[["sourceTopics"]] <- createJaspQmlSource(
    "sourceTopics",
    sort(unique(sapply(database$metaAnalyses, function(metaAnalysis)metaAnalysis[["topic"]])))
    )


  return()
}
.cochraneCreateDatabaseKeywords <- function(jaspResults, options){
  
  database <- jaspResults[["database"]]$object
  
  keywords <- sort(unique(unlist(sapply(database$metaAnalyses, function(metaAnalysis)metaAnalysis[["keywords"]]))))
  
  if (options[["keywordsSearch"]] == "")
    keywords <- keywords
  else
    keywords <- keywords[grepl(options[["keywordsSearch"]], keywords, ignore.case = TRUE)]
  
  keywords <- na.omit(keywords[1:100])
  
  jaspResults[["sourceKeywords"]] <- createJaspQmlSource(
    "sourceKeywords",
    keywords,
    "keywordsSearch"
  )
  
  return()
}
.cochraneCreateSelectorGadget   <- function(jaspResults, dataset){
  
  datasetOverview      <- jaspResults[["datasetOverview"]][["object"]]
  datasetTitles        <- sapply(datasetOverview, function(overview)overview$title)

  if(length(datasetTitles) > 0){
    jaspResults[["selectionGadget"]] <- createJaspQmlSource(
      "selectionGadget",
      datasetTitles,
      c("selectionType", "topicsSelected", "keywordsSelected", "textSearch", "analyzeData"))
  }

  return()
}
# test
if(FALSE){
  library(jaspTools)
  library(jaspResults)
  setPkgOption('module.dirs', "C:/Projects/JASP/jaspMetaAnalysis")
  options <- jaspTools::analysisOptions("ClassicalMetaAnalysis")
  options <- readRDS("C:/Projects/JASP/jasp-R-debug/options.RDS")
}


.plotMarginal                <- function(column, variableName, rugs = FALSE, displayDensity = FALSE, binWidthType = c("doane", "fd", "scott", "sturges", "manual"), numberOfBins = NA) {
  binWidthType <- match.arg(binWidthType)
  column   <- as.numeric(column)
  variable <- na.omit(column)
  
  if (length(variable) == 0)
    return(NULL)
  
  if (binWidthType == "doane") {  # https://en.wikipedia.org/wiki/Histogram#Doane's_formula
    sigma.g1 <- sqrt((6*(length(variable) - 2)) / ((length(variable) + 1)*(length(variable) + 3)))
    g1 <- mean(abs(variable)^3)
    k <- 1 + log2(length(variable)) + log2(1 + (g1 / sigma.g1))
    binWidthType <- k
  } else if (binWidthType == "fd" && nclass.FD(variable) > 10000) { # FD-method will produce extreme number of bins and crash ggplot, mention this in footnote
    binWidthType <- 10000
  } else if (binWidthType == "manual") { 
    binWidthType <- numberOfBins
  }
  
  
  h <- hist(variable, plot = FALSE, breaks = binWidthType)
  
  if (!displayDensity)
    yhigh <- max(h$counts)
  else {
    dens <- density(variable)
    yhigh <- max(max(h$density), max(dens$y))
  }
  
  ylow <- 0
  xticks <- base::pretty(c(variable, h$breaks), min.n = 3)
  
  if (!displayDensity) {
    p <-
      jaspGraphs::drawAxis(
        xName = variableName, yName = gettext("Counts"), xBreaks = xticks,
        yBreaks = base::pretty(c(0, h$counts)), force = TRUE, xLabels = xticks
      )
  } else {
    p <-
      jaspGraphs::drawAxis(
        xName = variableName, yName = gettext("Density"), xBreaks = xticks,
        yBreaks = c(0,  1.05 * yhigh), force = TRUE, yLabels = NULL,
        xLabels = xticks
      )
  }
  
  
  if (displayDensity) {
    p <- p +
      ggplot2::geom_histogram(
        data = data.frame(variable),
        mapping = ggplot2::aes(x = variable, y = ..density..),
        binwidth = (h$breaks[2] - h$breaks[1]),
        fill = "grey",
        col = "black",
        size = .7,
        center = ((h$breaks[2] - h$breaks[1])/2)
      ) +
      ggplot2::geom_line(
        data = data.frame(x = dens$x, y = dens$y),
        mapping = ggplot2::aes(x = x, y = y),
        lwd = 1,
        col = "black"
      )
  } else {
    p <- p +
      ggplot2::geom_histogram(
        data     = data.frame(variable),
        mapping  = ggplot2::aes(x = variable, y = ..count..),
        binwidth = (h$breaks[2] - h$breaks[1]),
        fill     = "grey",
        col      = "black",
        size     = .7,
        center    = ((h$breaks[2] - h$breaks[1])/2)
      )
  }
  
  if (rugs)
    p <- p + ggplot2::geom_rug(data = data.frame(variable), mapping = ggplot2::aes(x = variable), sides = "b")
  
  
  # JASP theme
  p <- jaspGraphs::themeJasp(p,
                             axisTickWidth = .7,
                             bty = list(type = "n", ldwX = .7, lwdY = 1))
  
  if (displayDensity)
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  
  return(p)
}
.plotMarginalCorDescriptives <- function (variable, xName = NULL, yName = gettext("Density")){
  variable <- na.omit(variable)
  isNumeric <- !(is.factor(variable) || (is.integer(variable) && 
                                           length(unique(variable)) <= 10))
  if (isNumeric) {
    p <- ggplot2::ggplot(data = data.frame(x = variable))
    h <- hist(variable, plot = FALSE)
    hdiff <- h$breaks[2L] - h$breaks[1L]
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(variable, 
                                                 h$breaks), min.n = 3)
    dens <- h$density
    yBreaks <- c(0, 1.2 * max(h$density))
    p <- p + ggplot2::geom_histogram(mapping = ggplot2::aes(x = x, 
                                                            y = ..density..), binwidth = hdiff, fill = "grey", 
                                     col = "black", size = 0.3, center = hdiff/2, 
                                     stat = "bin") + ggplot2::scale_x_continuous(name = xName, 
                                                                                 breaks = xBreaks, limits = range(xBreaks))
  }
  else {
    p <- ggplot2::ggplot(data = data.frame(x = factor(variable)))
    hdiff <- 1L
    xBreaks <- unique(variable)
    yBreaks <- c(0, max(table(variable)))
    p <- p + ggplot2::geom_bar(mapping = ggplot2::aes(x = x), 
                               fill = "grey", col = "black", size = 0.3, 
                               stat = "count") + ggplot2::scale_x_discrete(name = xName, 
                                                                           breaks = xBreaks)
  }
  yLim <- range(yBreaks)
  if (isNumeric) {
    density <- density(variable)
    p <- p + ggplot2::geom_line(data = data.frame(x = density$x, 
                                                  y = density$y), mapping = ggplot2::aes(x = x, y = y), 
                                lwd = 0.7, col = "black")
  }
  
  p <- p + ggplot2::geom_rug(ggplot2::aes(variable))
  
  thm <- ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), 
                        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, 
                                                                                      r = -5, b = 0, l = 0)))
  p <- p + ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, 
                                       labels = c("", ""), limits = yLim) + ggplot2::theme()
  return(jaspGraphs::themeJasp(p) + thm)
}