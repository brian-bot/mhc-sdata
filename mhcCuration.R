require(synapseClient)
synapseLogin()

## x IS EXPECTED TO BE A CHARACTER VECTOR TO BE CLEANED UP
cleanString <- function(x){
  gsub('[', '', gsub(']', '', gsub('"', '', x, fixed=T), fixed=T), fixed=T)
}

## x IS EXPECTED TO BE A LIST OF COLUMN MODEL OBJECTS
## ct IS THE COLUMN TYPE TO BE EXTRACTED
whichColumns <- function(x, ct){
  cc <- sapply(as.list(1:length(x)), function(y){
    if(x[[y]]@columnType==ct){
      return(x[[y]]@name)
    } else{
      return(NULL)
    }
  })
  cc <- unlist(cc)
  return(cc)
}

## synId IS THE SYNAPSE ID OF THE TABLE TO CLEAN UP
cleanTable <- function(synId, strictVersion=TRUE){
  ## GET MAPPING OF SHARING STATUS
  sm <- synGet("syn11424946")
  shareMap <- read.delim(getFileLocation(sm), stringsAsFactors = FALSE)
  theseShared <- shareMap$healthCode[ shareMap$sharingScope == "all_qualified_researchers" ]
  
  ## VARIABLES TO BE USED ACROSS TABLES
  firstDate <- as.Date("2015-03-09")
  lastDate <- as.Date("2015-10-27")
  coreNames <- c("recordId", "healthCode", "createdOn", "appVersion", "phoneInfo")
  releaseVersions <- c("version 1.0, build 5.1",
                       "version 1.0.2, build 6",
                       "version 1.0.3, build 1",
                       "version 1.0.4, build 7",
                       "version 1.0.5, build 1",
                       "version 1.0.6, build 9",
                       "version 1.0.7, build 32",
                       "version 1.0.8, build 10",
                       "version 1.0.9, build 9",
                       "version 1.0.10, build 1")

  ## GET ALL OF THE COLUMNS
  allCols <- synGetColumns(synId)@content
  stringCols <- whichColumns(allCols, "STRING")
  fhCols <- whichColumns(allCols, "FILEHANDLEID")
  
  ## GET THE RAW DATA IN A CERTAIN RANGE DATA
  tb <- synTableQuery(paste0("SELECT * FROM ", synId))
  df <- tb@values
  df <- df[ which(df$healthCode %in% theseShared), ]
  ## DO SOME CLEANING
  for(i in stringCols){
    df[[i]] <- cleanString(df[[i]])
  }
  remCols <- c("externalId", "dataGroups", "uploadDate", "createdOnTimeZone", "userSharingScope", "validationErrors")
  df <- df[, -which(names(df) %in% remCols)]

  dfSub <- as.data.frame(df[, setdiff(names(df), coreNames)])
  dfIdx <- rowSums(is.na(dfSub)) != ncol(dfSub)
  df <- df[ dfIdx, ]
  df <- df[ as.Date(df$createdOn) >= firstDate & as.Date(df$createdOn) <= lastDate, ]
  ## FOR CERTAIN TABLES, ALLOW appVersion TO BE NA
  if(strictVersion){
    df <- df[ df$appVersion %in% releaseVersions, ]
  } else{
    df <- df[ df$appVersion %in% c(NA, releaseVersions), ]
  }
  df <- df[ which(!duplicated(df[, c("healthCode", "createdOn")])), ]
  df[ order(df$createdOn), ]
  
  tb@values <- df
  return(list(table=tb, fhCols=fhCols, fhToUpload=FALSE))
}


## DAY ONE SURVEY
dayOne <- cleanTable("syn3420238")
dayOne$table@values$welcome <- NULL

## PAR-Q QUIZ
parq <- cleanTable("syn3420257")

## DAILY CHECK
dailyCheck <- cleanTable("syn3420261")

## ACTIVITY AND SLEEP SURVEY
activitySleep <- cleanTable("syn3420264")

## RISK FACTOR SURVEY
riskFactor <- cleanTable("syn3420385")

## CARDIO DIET SURVEY
cardioDiet <- cleanTable("syn3420518")

## SATISFIED SURVEY
satisfied <- cleanTable("syn3420615")
satisfied$table@values$zip[ nchar(satisfied$table@values$zip) == 5] <- NA
satisfied$table@values$zip3 <- sprintf("%03d", satisfied$table@values$zip)
satisfied$table@values$zip3[ is.na(satisfied$table@values$zip) ] <- NA
satisfied$table@values$zip <- NULL
theseZips <- c("036", "692", "878", "059", "790", "879", "063", "821", "884", "102", "823", "890", "203", "830", "893", "556", "831")
satisfied$table@values$zip3[ which(satisfied$table@values$zip3 %in% theseZips) ] <- "000"

## SIX MINUTE WALK TASK
smWalk <- cleanTable("syn3458480")

## APH HEART AGE SURVEY
aph1 <- cleanTable("syn3458936")
aph2 <- cleanTable("syn4586968")
heartAge <- aph2
heartAge$table@values <- rbind(aph1$table@values, aph2$table@values)
rm(aph1, aph2)
heartAge$table@values$heartAgeDataGender <- gsub("HKBiologicalSex", "", heartAge$table@values$heartAgeDataGender, fixed = TRUE)
heartAge$table@values$heartAgeDataAge <- as.integer(heartAge$table@values$heartAgeDataAge)
heartAge$table@values$heartAgeDataAge[ which(heartAge$table@values$heartAgeDataAge<18) ] <- NA
# lots of outliers - decided to let others determine how to parse PROs

## DEMOGRAPHICS
demographics <- cleanTable("syn3917840")
names(demographics$table@values) <- sub("NonIdentifiableDemographics.json.", "", names(demographics$table@values), fixed=TRUE)
notThese <- grep("NonIdentifiableDemographics.", names(demographics$table@values), fixed=TRUE)
demographics$table@values <- demographics$table@values[, -notThese]
demographics$table@values$patientCurrentAge[ which(demographics$table@values$patientCurrentAge < 18) ] <- NA
demographics$table@values$patientCurrentAge[ which(demographics$table@values$patientCurrentAge > 89) ] <- 89
## CLEAN UP WEIGHT - RULES AS PER DISCUSSION WITH GOVERNANCE TEAM
demographics$table@values$patientWeightPounds[ which(demographics$table@values$patientWeightPounds < 50) ] <- NA
demographics$table@values$patientWeightPounds[ which(demographics$table@values$patientWeightPounds < 80) ] <- 79
demographics$table@values$patientWeightPounds[ which(demographics$table@values$patientWeightPounds > 700) ] <- NA
demographics$table@values$patientWeightPounds[ which(demographics$table@values$patientWeightPounds > 350) ] <- 351
## CLEAN UP HEIGHT - RULES AS PER DISCUSSION WITH GOVERNANCE TEAM
demographics$table@values$patientHeightInches[ which(demographics$table@values$patientHeightInches < 30) ] <- NA
demographics$table@values$patientHeightInches[ which(demographics$table@values$patientHeightInches < 60) ] <- 59
demographics$table@values$patientHeightInches[ which(demographics$table@values$patientHeightInches > 90) ] <- NA
demographics$table@values$patientHeightInches[ which(demographics$table@values$patientHeightInches > 78) ] <- 79
## GET HOUR AND MIN OF TIMES
demographics$table@values$patientWakeUpTime <- substr(demographics$table@values$patientWakeUpTime, 12, 16)
demographics$table@values$patientGoSleepTime <- substr(demographics$table@values$patientGoSleepTime, 12, 16)





## HealthKitDataCollector
hkData <- cleanTable('syn3560085', strictVersion=FALSE)
hkData$table@values <- hkData$table@values[ !is.na(hkData$table@values$data.csv), ]
hkDataFiles <- synDownloadTableColumns(hkData$table, "data.csv")
hkDataFiles <- as.data.frame(as.character(hkDataFiles), stringsAsFactors = FALSE)
names(hkDataFiles) <- "data.csv"
hkDataFiles$recordId <- hkData$table@values$recordId
hkDataFilesNew <- sapply(1:nrow(hkDataFiles), function(x){
  a <- try(read.csv(hkDataFiles$data.csv[x], stringsAsFactors=FALSE), silent=TRUE)
  if( class(tryCatch) == "try-error" ){
    return(NA)
  }
  fl <- file.path(getwd(), "data", "data", paste0(hkDataFiles$recordId[x], ".csv"))
  ids <- any(names(a)=="source")
  idsi <- any(names(a)=="sourceIdentifier")
  
  if( ids & idsi ){
    idw <- grepl("watch", tolower(a$source), fixed=TRUE)
    idp <- grepl("phone", tolower(a$source), fixed=TRUE)
    ido <- !(idw | idp)
    a$source[idw] <- "watch"
    a$source[idp] <- "phone"
    a$source[ido] <- "other"
    tmpSi <- strsplit(as.character(a$sourceIdentifier), ".", fixed=TRUE)
    tmpSi <- sapply(tmpSi, function(y){
      if(length(y) == 1 ){
        return(y)
      } else if( length(y) == 2){
        return(paste(y, collapse="."))
      } else if( length(y) > 2){
        return(paste(y[1:3], collapse="."))
      } else{
        return(NA)
      }
    })
    a$sourceIdentifier <- tmpSi
    write.csv(a, file=fl, quote = FALSE, row.names=FALSE)
    return(fl)
  } else{
    return(NA)
  }
})
hkDataFiles$newFile <- hkDataFilesNew
hkDataFiles <- hkDataFiles[ !is.na(hkDataFiles$newFile), ]
rownames(hkDataFiles) <- hkDataFiles$recordId
## UPDATE THE ORIGINAL TABLE
hkData$fhToUpload <- TRUE
hkData$table@values <- hkData$table@values[ hkData$table@values$recordId %in% hkDataFiles$recordId, ]
hkData$table@values$data.csv <- hkDataFiles[ hkData$table@values$recordId, "newFile"]


## HealthKitSleepCollector
hkSleep <- cleanTable('syn3560086')
hkSleep$table@values <- hkSleep$table@values[ !is.na(hkSleep$table@values$data.csv), ]
## DOWNLOAD ALL OF THE FILES AND PARSE FOR POSSIBLY SENSITIVE INFORMATION
hkSleepFiles <- synDownloadTableColumns(hkSleep$table, "data.csv")
hkSleepFiles <- as.data.frame(as.character(hkSleepFiles), stringsAsFactors = FALSE)
names(hkSleepFiles) <- "data.csv"
hkSleepFiles$recordId <- hkSleep$table@values$recordId
hkSleepFilesNew <- sapply(1:nrow(hkSleepFiles), function(x){
  a <- try(read.csv(hkSleepFiles$data.csv[x], stringsAsFactors=FALSE), silent=TRUE)
  if( class(tryCatch) == "try-error" ){
    return(NA)
  }
  fl <- file.path(getwd(), "data", "sleep", paste0(hkSleepFiles$recordId[x], ".csv"))
  ids <- any(names(a)=="source")
  idsi <- any(names(a)=="sourceIdentifier")
  
  if( ids & idsi ){
    #####
    ## SLEEP COLLECTOR HAS SOURCE AND SOURCEIDENTIFIER FLIPPED - FIX HERE
    ts <- a$source
    a$source <- a$sourceIdentifier
    a$sourceIdentifier <- ts
    #####
    idw <- grepl("watch", tolower(a$source), fixed=TRUE)
    idp <- grepl("phone", tolower(a$source), fixed=TRUE)
    ido <- !(idw | idp)
    a$source[idw] <- "watch"
    a$source[idp] <- "phone"
    a$source[ido] <- "other"
    tmpSi <- strsplit(as.character(a$sourceIdentifier), ".", fixed=TRUE)
    tmpSi <- sapply(tmpSi, function(y){
      if(length(y) == 1 ){
        return(y)
      } else if( length(y) == 2){
        return(paste(y, collapse="."))
      } else if( length(y) > 2){
        return(paste(y[1:3], collapse="."))
      } else{
        return(NA)
      }
    })
    a$sourceIdentifier <- tmpSi
    write.csv(a, file=fl, quote = FALSE, row.names=FALSE)
    return(fl)
  } else{
    return(NA)
  }
})
hkSleepFiles$newFile <- hkSleepFilesNew
hkSleepFiles <- hkSleepFiles[ !is.na(hkSleepFiles$newFile), ]
rownames(hkSleepFiles) <- hkSleepFiles$recordId
## UPDATE THE ORIGINAL TABLE
hkSleep$fhToUpload <- TRUE
hkSleep$table@values <- hkSleep$table@values[ hkSleep$table@values$recordId %in% hkSleepFiles$recordId, ]
hkSleep$table@values$data.csv <- hkSleepFiles[ hkSleep$table@values$recordId, "newFile"]


## HealthKitWorkoutCollector
hkWorkout <- cleanTable('syn3560095')
hkWorkout$table@values <- hkWorkout$table@values[ !is.na(hkWorkout$table@values$data.csv), ]
## DOWNLOAD ALL OF THE FILES AND PARSE FOR POSSIBLY SENSITIVE INFORMATION
hkWorkoutFiles <- synDownloadTableColumns(hkWorkout$table, "data.csv")
hkWorkoutFiles <- as.data.frame(as.character(hkWorkoutFiles), stringsAsFactors = FALSE)
names(hkWorkoutFiles) <- "data.csv"
hkWorkoutFiles$recordId <- hkWorkout$table@values$recordId
hkWorkoutFilesNew <- sapply(1:nrow(hkWorkoutFiles), function(x){
  a <- try(read.csv(hkWorkoutFiles$data.csv[x], stringsAsFactors=FALSE), silent=TRUE)
  if( class(tryCatch) == "try-error" ){
    return(NA)
  }
  fl <- file.path(getwd(), "data", "workout", paste0(hkWorkoutFiles$recordId[x], ".csv"))
  ids <- any(names(a)=="source")
  idsi <- any(names(a)=="sourceIdentifier")
  
  if( ids & idsi ){
    idw <- grepl("watch", tolower(a$source), fixed=TRUE)
    idp <- grepl("phone", tolower(a$source), fixed=TRUE)
    ido <- !(idw | idp)
    a$source[idw] <- "watch"
    a$source[idp] <- "phone"
    a$source[ido] <- "other"
    tmpSi <- strsplit(as.character(a$sourceIdentifier), ".", fixed=TRUE)
    tmpSi <- sapply(tmpSi, function(y){
      if(length(y) == 1 ){
        return(y)
      } else if( length(y) == 2){
        return(paste(y, collapse="."))
      } else if( length(y) > 2){
        return(paste(y[1:3], collapse="."))
      } else{
        return(NA)
      }
    })
    a$sourceIdentifier <- tmpSi
    write.csv(a, file=fl, quote = FALSE, row.names=FALSE)
    return(fl)
  } else{
    return(NA)
  }
})
hkWorkoutFiles$newFile <- hkWorkoutFilesNew
hkWorkoutFiles <- hkWorkoutFiles[ !is.na(hkWorkoutFiles$newFile), ]
rownames(hkWorkoutFiles) <- hkWorkoutFiles$recordId
## UPDATE THE ORIGINAL TABLE
hkWorkout$fhToUpload <- TRUE
hkWorkout$table@values <- hkWorkout$table@values[ hkWorkout$table@values$recordId %in% hkWorkoutFiles$recordId, ]
hkWorkout$table@values$data.csv <- hkWorkoutFiles[ hkWorkout$table@values$recordId, "newFile"]


## motionTracker
motionTracker <- cleanTable('syn3420486', strictVersion=FALSE)
motionTracker$table@values$appVersion <- NULL
motionTracker$table@values$phoneInfo <- NULL

## 6MWT Displacement Data
smDisp <- cleanTable('syn4214144')













#####
## LOG IN AS BRIDGE EXPORTER TO UPLOAD TABLES WITH FILE HANDLES OWNED BY THAT USER
#####
outputProjId <- "syn11269541"
ud <- synapseClient:::getUploadDestinations(outputProjId)[[1]]

storeThese <- list('Day One Survey' = dayOne,
                   'PAR-Q Survey' = parq,
                   'Daily Check Survey' = dailyCheck,
                   'Activity and Sleep Survey' = activitySleep,
                   'Risk Factor Survey' = riskFactor,
                   'Cardio Diet Survey' = cardioDiet,
                   'Satisfied Survey' = satisfied,
                   'APH Heart Age Survey' = heartAge,
                   'Six Minute Walk Activity' = smWalk,
                   'Demographics Survey' = demographics,
                   'HealthKit Data' = hkData,
                   'HealthKit Sleep' = hkSleep,
                   'HealthKit Workout' = hkWorkout,
                   'Motion Tracker' = motionTracker,
                   'Six Minute Walk - Displacement Vectors' = smDisp)

storeThese <- lapply(storeThese, function(tt){
  ## FIRST SEE IF THERE ARE FILE HANDLES TO UPLOAD
  if(tt$fhToUpload){
    fhs <- sapply(tt$table@values[[tt$fhCols]], function(fp){
      fh <- synapseClient:::uploadAndAddToCacheMap(filePath=fp, uploadDestination=ud)
      return(fh$id)
    })
    tt$table@values[[tt$fhCols]] <- fhs
  }
  
  ## USE TAB DELIMITED FOR CASES WHERE COMMAS ARE USED IN TEXT FIELDS
  tcs <- as.tableColumns(tt$table@values)
  for(i in 1:length(tcs$tableColumns)){
    ## NAMES IN tcs HAVE . REMOVED - NEED TO KEEP CONSISTENT
    tcs$tableColumns[[i]]@name <- names(tt$table@values)[i]
    if(tcs$tableColumns[[i]]@name %in% tt$fhCols){
      tcs$tableColumns[[i]]@columnType <- "FILEHANDLEID"
      tcs$tableColumns[[i]]@defaultValue <- character(0)
      tcs$tableColumns[[i]]@maximumSize <- integer(0)
      tcs$tableColumns[[i]]@enumValues <- character(0)
    }
  }
  return(tcs)
})

## FINALLY, STORE THE OUTPUT
for(i in length(storeThese):1){
  theEnd <- synStore(Table(TableSchema(name=names(storeThese)[i],
                                       parent=outputProjId,
                                       columns=storeThese[[i]]$tableColumns),
                           values = storeThese[[i]]$fileHandleId))
}

