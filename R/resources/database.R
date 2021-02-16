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

#### continuous ####
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
dataStudies <- dataStudies[,c("review_doi", "study_year", "effectSize", "effectSE", "sampleSize", "review_year", "titleStudy", "name", "review_title", "review_topic", "review_keywords")]
colnames(dataStudies)[c(1:2, 6:11)] <- c("doi", "studyYear", "reviewYear", "titleStudy", "titleMetaAnalysis", "titleReview", "reviewTopic", "reviewKeywords")
# these are formatted as lists for whatever reason
dataStudies$effectSize  <- as.numeric(dataStudies$effectSize)
dataStudies$effectSE    <- as.numeric(dataStudies$effectSE)
dataStudies$sampleSize  <- as.numeric(dataStudies$sampleSize)
dataStudies             <- na.omit(dataStudies)
# fix the names of reviews
dataStudies$titleReview <- gsub("â€\u0090", " ", dataStudies$titleReview, fixed = TRUE)

# create an indexing object for meta-analyses 
uniqueDataMetaAnalyses <- dataStudies[!duplicated(dataStudies$titleMetaAnalysis),]

dataMetaAnalyses     <- list()
for(title in uniqueDataMetaAnalyses$titleMetaAnalysis){
  dataMetaAnalyses[[title]] <- with(
    uniqueDataMetaAnalyses[uniqueDataMetaAnalyses$titleMetaAnalysis == title,],
    list(
      title      = titleMetaAnalysis,
      nStudies   = sum(dataStudies$titleMetaAnalysis == title),
      year       = reviewYear,
      doi        = doi,
      topic      = getTopic(reviewTopic),
      keywords   = getKeywords(reviewKeywords)
    ))
}


# create an indexing object for reviews
uniqueDataReviews <- dataStudies[!duplicated(dataStudies$titleReview),]

dataReviews     <- list()
for(title in uniqueDataReviews$titleReview){
  dataReviews[[title]] <- with(
    uniqueDataReviews[uniqueDataReviews$titleReview == title,],
    list(
      title     = titleReview,
      nStudies  = sum(dataStudies$titleReview == title),
      year      = reviewYear,
      doi       = doi,
      topic     = getTopic(reviewTopic),
      keywords  = getKeywords(reviewKeywords)
    ))
}



saveRDS(list(
  studies      = dataStudies[,c("doi", "effectSize", "effectSE", "sampleSize", "studyYear", "reviewYear", "titleStudy", "titleMetaAnalysis", "titleReview")],
  metaAnalyses = dataMetaAnalyses,
  reviews      = dataReviews
), file = "C:/Projects/JASP/jaspCochraneMetaAnalyses/R/resources/databaseSMD.RDS")

rm(list(dataStudies, dataMetaAnalyses, dataReviews))

#### dichotomous ####
# creating the studies object
dataStudies <- dataDichotomous
table(dataDichotomous$effect_measure)

# transform measures into standardized effect sizes
for(i in 166812:nrow(dataStudies)){
  tempOR <- with(
    dataStudies[i,],
    metafor::escalc(
      measure = "OR",
      ai   = study_events_1,
      ci   = study_events_2,
      n1i  = study_total_1,
      n2i  = study_total_2
    ))
  tempRR <- with(
    dataStudies[i,],
    metafor::escalc(
      measure = "RR",
      ai   = study_events_1,
      ci   = study_events_2,
      n1i  = study_total_1,
      n2i  = study_total_2
    ))
  tempPOR <- with(
    dataStudies[i,],
    metafor::escalc(
      measure = "PETO",
      ai   = study_events_1,
      ci   = study_events_2,
      n1i  = study_total_1,
      n2i  = study_total_2
    ))
  tempRD <- with(
    dataStudies[i,],
    metafor::escalc(
      measure = "RD",
      ai   = study_events_1,
      ci   = study_events_2,
      n1i  = study_total_1,
      n2i  = study_total_2
    ))
  
  dataStudies$effectSizeOR[i]  <- tempOR[1]
  dataStudies$effectSEOR[i]    <- sqrt(tempOR[2])
  dataStudies$effectSizeRR[i]  <- tempRR[1]
  dataStudies$effectSERR[i]    <- sqrt(tempRR[2])
  dataStudies$effectSizePOR[i] <- tempPOR[1]
  dataStudies$effectSEPOR[i]   <- sqrt(tempPOR[2])
  dataStudies$effectSizeRD[i]  <- tempRD[1]
  dataStudies$effectSERD[i]    <- sqrt(tempRD[2])
  dataStudies$sampleSize[i]    <- sum(dataStudies[i,c("study_total_1", "study_total_2")])
  dataStudies$titleStudy[i]    <- getLabels(dataStudies[i,])
}


dataStudies <- dataStudies[,c("review_doi", "study_year", "effectSizeOR", "effectSEOR", "effectSizeRR", "effectSERR", "effectSizePOR", "effectSEPOR", "effectSizeRD", "effectSERD", "sampleSize", "review_year", "titleStudy", "name", "review_title", "review_topic", "review_keywords")]
colnames(dataStudies)[c(1:2, 12:17)] <- c("doi", "studyYear", "reviewYear", "titleStudy", "titleMetaAnalysis", "titleReview", "reviewTopic", "reviewKeywords")
# these are formatted as lists for whatever reason
dataStudies$effectSizeOR  <- as.numeric(dataStudies$effectSizeOR)
dataStudies$effectSEOR    <- as.numeric(dataStudies$effectSEOR)
dataStudies$effectSizeRR  <- as.numeric(dataStudies$effectSizeRR)
dataStudies$effectSERR    <- as.numeric(dataStudies$effectSERR)
dataStudies$effectSizePOR <- as.numeric(dataStudies$effectSizePOR)
dataStudies$effectSEPOR   <- as.numeric(dataStudies$effectSEPOR)
dataStudies$effectSizeRD  <- as.numeric(dataStudies$effectSizeRD)
dataStudies$effectSERD    <- as.numeric(dataStudies$effectSERD)
dataStudies$sampleSize    <- as.numeric(dataStudies$sampleSize)
dataStudies               <- na.omit(dataStudies)
# fix the names of reviews
dataStudies$titleReview <- gsub("â€\u0090", " ", dataStudies$titleReview, fixed = TRUE)

# create an indexing object for meta-analyses 
uniqueDataMetaAnalyses <- dataStudies[!duplicated(dataStudies$titleMetaAnalysis),]

dataMetaAnalyses     <- list()
for(title in uniqueDataMetaAnalyses$titleMetaAnalysis){
  dataMetaAnalyses[[title]] <- with(
    uniqueDataMetaAnalyses[uniqueDataMetaAnalyses$titleMetaAnalysis == title,],
    list(
      title      = titleMetaAnalysis,
      nStudies   = sum(dataStudies$titleMetaAnalysis == title),
      year       = reviewYear,
      doi        = doi,
      topic      = getTopic(reviewTopic),
      keywords   = getKeywords(reviewKeywords)
    ))
}


# create an indexing object for reviews
uniqueDataReviews <- dataStudies[!duplicated(dataStudies$titleReview),]

dataReviews     <- list()
for(title in uniqueDataReviews$titleReview){
  dataReviews[[title]] <- with(
    uniqueDataReviews[uniqueDataReviews$titleReview == title,],
    list(
      title     = titleReview,
      nStudies  = sum(dataStudies$titleReview == title),
      year      = reviewYear,
      doi       = doi,
      topic     = getTopic(reviewTopic),
      keywords  = getKeywords(reviewKeywords)
    ))
}



saveRDS(list(
  studies      = dataStudies[,c("doi", "effectSizeOR", "effectSEOR", "effectSizeRR", "effectSERR", "effectSizePOR", "effectSEPOR", "effectSizeRD", "effectSERD", "sampleSize", "studyYear", "reviewYear", "titleStudy", "titleMetaAnalysis", "titleReview")],
  metaAnalyses = dataMetaAnalyses,
  reviews      = dataReviews
), file = "C:/Projects/JASP/jaspCochraneMetaAnalyses/R/resources/databaseDichotomous.RDS")
