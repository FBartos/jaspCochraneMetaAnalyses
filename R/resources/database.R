# function
getKeywords <- function(keywordsRaw){
  if(all(is.na(keywordsRaw)))
    return(NA)
  keywordsSplit <- unlist(strsplit(unique(keywordsRaw), ";", fixed = TRUE))
  keywordsSplit <- trimws(unique(keywordsSplit), which = c("both"))
  keywordsClean <- gsub("*", "", keywordsSplit, fixed = TRUE)
  keywordsClean <- gsub("â€\u0090", " ", keywordsClean, fixed = TRUE)
  keywordsClean <- sapply(keywordsClean, function(keyword){
    bracketStart <- regexec("[", keyword, fixed = TRUE)[[1]][1]
    if(bracketStart == -1){
      return(keyword)
    }else{
      return(substr(keyword, 1, bracketStart - 2))
    }
  })
  return(unname(keywordsClean))
}
getTopic    <- function(topicRaw){
  return(gsub("[", "", gsub("]", "", unique(topicRaw), fixed = TRUE), fixed = TRUE))
}
getLabels   <- function(dataStudies){
  label <- dataStudies$study_id
  label <- gsub("STD-", "", label)
  label <- gsub("-", " ", label)
  label <- paste0(label, ", (", dataStudies$group_label_1, " vs ", dataStudies$group_label_2,")")
  return(label)
}
# load data
dataContinuous  <- read.csv("C:/Projects/JASP/jaspCochraneMetaAnalyses/R/resources/dataContinuous.csv",  stringsAsFactors = FALSE)
dataDichotomous <- read.csv("C:/Projects/JASP/jaspCochraneMetaAnalyses/R/resources/dataDichotomous.csv", stringsAsFactors = FALSE)


# creating the studies object
dataStudies <- dataContinuous
# transform measures into standardized effect sizes
for(i in 1:nrow(dataStudies)){
  tempEffectCalc <- with(
    dataStudies[i,],
    metafor::escalc(
      measure = "SMD",
      m2i     = study_mean_2,
      m1i     = study_mean_1,
      n2i     = study_total_2,
      n1i     = study_total_1,
      sd2i    = study_sd_2,
      sd1i    = study_sd_1
    ))
  dataStudies$effectSize[i] <- tempEffectCalc[1]
  dataStudies$effectSE[i]   <- sqrt(tempEffectCalc[2])
  dataStudies$sampleSize[i] <- sum(dataStudies[i,c("study_total_1", "study_total_2")])
  dataStudies$titleStudy[i] <- getLabels(dataStudies[i,])
}
# TODO: some studies without reported SE ... their effect sizes are computed as NAs and omited in the next step
dataStudies <- dataStudies[,c("review_doi", "study_year", "effectSize", "effectSE", "sampleSize", "review_title", "name", "titleStudy")]
colnames(dataStudies)[c(1:2, 6:7)] <- c("doi", "studyYear", "titleReview", "titleMetaAnalysis")
# these are formatted as lists for whatever reason
dataStudies$effectSize <- as.numeric(dataStudies$effectSize)
dataStudies$effectSE   <- as.numeric(dataStudies$effectSE)
dataStudies$sampleSize <- as.numeric(dataStudies$sampleSize)
dataStudies <- na.omit(dataStudies)


# create an indexing object for reviews
uniqueDataMetaAnalyses <- dataContinuous[!duplicated(dataContinuous$name), c("review_doi", "meta.table.size", "review_year", "review_topic", "name", "review_keywords")]

dataMetaAnalyses     <- list()
for(n in uniqueDataMetaAnalyses$name){
  dataMetaAnalyses[[n]] <- with(
    uniqueDataMetaAnalyses[uniqueDataMetaAnalyses$name == n,],
    list(
      title      = name,
      nStudies   = sum(dataStudies$titleMetanalysis == n),
      year       = review_year,
      doi        = review_doi,
      topic      = getTopic(review_topic),
      keywords   = getKeywords(review_keywords)
    ))
}


# create an indexing object for meta-analyses
uniqueDataReviews <- dataContinuous[!duplicated(dataContinuous$review_title), c("review_doi", "meta.table.size", "review_year", "review_topic", "review_title", "review_keywords")]

dataReviews     <- list()
for(title in uniqueDataReviews$review_title){
  dataReviews[[gsub("â€\u0090", " ", title, fixed = TRUE)]] <- with(
    uniqueDataReviews[uniqueDataReviews$review_title == title,],
    list(
      title     = gsub("â€\u0090", " ", review_title, fixed = TRUE),
      nStudies  = sum(dataStudies$titleReview == title),
      year      = review_year,
      doi       = review_doi,
      topic     = getTopic(review_topic),
      keywords  = getKeywords(review_keywords)
    ))
}



saveRDS(list(
  studies      = dataStudies,
  metaAnalyses = dataMetaAnalyses,
  reviews      = dataReviews
), file = "C:/Projects/JASP/jaspCochraneMetaAnalyses/R/resources/database.RDS")

# extract topics and keywords for the qml interface
paste(sort(unique(sapply(dataMetaAnalyses, function(metaAnalysis)metaAnalysis[["topic"]]))),            collapse = "; ")
paste(sort(unique(unlist(sapply(dataMetaAnalyses, function(metaAnalysis)metaAnalysis[["keywords"]])))), collapse = "; ")
paste(sort(unique(unlist(sapply(dataReviews,      function(metaAnalysis)metaAnalysis[["keywords"]])))), collapse = "; ")
