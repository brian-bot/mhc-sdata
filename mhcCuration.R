require(synapseClient)
synapseLogin()

## VARIABLES TO BE USED ACROSS TABLES
firstDate <- as.Date("2015-03-09")
lastDate <- as.Date("2015-09-09")
coreNames <- c("recordId", "healthCode", "createdOn", "appVersion", "phoneInfo")
releaseVersions <- c("version 1.0.10, build 1", "version 1.0.3, build 1", "version 1.0.6, build 9", "version 1.0, build 5.1", "version 1.0.2, build 6", "version 1.0.4, build 7", "version 1.0.7, build 32", "version 1.0.8, build 10", "version 1.0.9, build 9")
theseZips <- c("036", "692", "878", "059", "790", "879", "063", "821", "884", "102", "823", "890", "203", "830", "893", "556", "831")
outputProjId <- ""

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
cleanTable <- function(synId){
  ## GET ALL OF THE COLUMNS
  allCols <- synGetColumns(synId)@content
  stringCols <- whichColumns(allCols, "STRING")
  fhCols <- whichColumns(allCols, "FILEHANDLE")
  
  ## GET THE RAW DATA IN A CERTAIN RANGE DATA
  df <- synTableQuery(paste0("SELECT * FROM ", synId))@values
  ## DO SOME CLEANING
  for(i in stringCols){
    df[[i]] <- cleanString(df[[i]])
  }
  remCols <- c("externalId", "dataGroups", "uploadDate")
  df <- df[, -which(names(df) %in% remCols)]

  dfSub <- df[, setdiff(names(df), coreNames)]
  dfIdx <- rowSums(is.na(dfSub)) != ncol(dfSub)
  df <- df[ dfIdx, ]
  df <- df[ as.Date(df$createdOn) >= firstDate & as.Date(df$createdOn) <= lastDate, ]
  df <- df[ df$appVersion %in% releaseVersions, ]
  df <- df[ which(!duplicated(df[, c("healthCode", "createdOn")])), ]
  df[ order(df$createdOn), ]
  
  return(list(data=df, fhCols=fhCols))
}


## DAY ONE SURVEY
# syn3420238
dayOne <- cleanTable("syn3420238")

## PAR-Q QUIZ
# syn3420257
parq <- cleanTable("syn3420257")

## DAILY CHECK
# syn3420261
dailyCheck <- cleanTable("syn3420261")

## ACTIVITY AND SLEEP SURVEY
# syn3420264
activitySleep <- cleanTable("syn3420264")

## RISK FACTOR SURVEY
# syn3420385
# syn4703171
rf1 <- cleanTable("syn3420385")
rf2 <- cleanTable("syn4703171")
for(cc in setdiff(names(rf2$data), names(rf1$data))){
  rf1$data[[cc]] <- NA
}
riskFactor <- list(data=rbind(rf1$data, rf2$data),
                   fhCols=rf2$fhCols)

## CARDIO DIET SURVEY
# syn3420518
cardioDiet <- cleanTable("syn3420518")

## SATISFIED SURVEY
# syn3420615
# syn4857042
sf1 <- cleanTable("syn3420615")
sf2 <- cleanTable("syn4857042")
satisfied <- list(data=rbind(sf1$data, sf2$data),
                  fhCols=sf2$fhCols)

## SIX MINUTE WALK TASK
# syn3458480
# syn4857044
sm1 <- cleanTable("syn3458480")
sm2 <- cleanTable("syn4857044")
for(cc in setdiff(names(sm2$data), names(sm1$data))){
  sm1$data[[cc]] <- NA
}
smWalk <- list(data=rbind(sm1$data, sm2$data),
                   fhCols=sm2$fhCols)

## APH HEART AGE SURVEY
# syn3458936
# syn4586968
aph1 <- cleanTable("syn3458936")
aph2 <- cleanTable("syn4586968")
heartAge <- list(data=rbind(aph1$data, aph2$data),
                 fhCols=aph2$fhCols)

## DEMOGRAPHICS
# syn3917840
demographics <- cleanTable("syn3917840")
names(demographics$data) <- sub("NonIdentifiableDemographics.json.", "", names(demographics$data), fixed=TRUE)
notThese <- grep("NonIdentifiableDemographics.", names(demographics$data), fixed=TRUE)
demographics$data <- demographics$data[, -notThese]




## OTHER CODE
# pid <- "syn3270436"
# 
# qq <- synQuery(paste0('SELECT id, name FROM table WHERE parentId=="', pid, '"'))
# thisId <- qq$table.id[ grep('appVersion', qq$table.name)]
# 
# theseTables <- synTableQuery(paste0("SELECT DISTINCT originalTable FROM ", thisId))@values
# q <- qq[which(qq$table.name %in% theseTables$originalTable), ]
# 
# allDat <- lapply(q$table.id, function(x){
#   xx <- synTableQuery(paste0("SELECT * FROM ", x, " WHERE createdOn < '2015-09-09'"))@values
#   xx <- xx[ xx$appVersion %in% releaseVersions, ]
#   return(xx)
# })
# names(allDat) <- q$table.name
# 
# allDat <- allDat[which(sapply(allDat, nrow) != 0)]

# > names(allDat)[[10]]
# [1] "cardiovascular-HealthKitDataCollector-v1"
# > names(allDat)[[11]]
# [1] "cardiovascular-HealthKitSleepCollector-v1"
# > names(allDat)[[12]]
# [1] "cardiovascular-HealthKitWorkoutCollector-v1"
# > names(allDat)[[14]]
# [1] "cardiovascular-displacement-v1"
# > names(allDat)[[15]]
# [1] "cardiovascular-6MWT Displacement Data-v1"
# > names(allDat)[[16]]
# [1] "cardiovascular-motionActivityCollector-v1"



# > names(allDat)[[7]]
# [1] "cardiovascular-satisfied-v1"
## CONTAINS 3 DIGIT ZIP

# > names(allDat)[[9]]
# [1] "cardiovascular-2-APHHeartAge-7259AC18-D711-47A6-ADBD-6CFCECDED1DF-v1"
# heartAgeDataAge

# > names(allDat)[[17]]
# [1] "cardiovascular-2-APHHeartAge-7259AC18-D711-47A6-ADBD-6CFCECDED1DF-v2"
# heartAgeDataAge

# > names(allDat)[[13]]
# [1] "cardiovascular-NonIdentifiableDemographicsTask-v2"
# NonIdentifiableDemographics.json.patientCurrentAge






