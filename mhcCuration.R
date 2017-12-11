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
cleanTable <- function(synId){
  ## GET MAPPING OF SHARING STATUS
  sm <- synGet("syn11424946")
  shareMap <- read.delim(getFileLocation(sm), stringsAsFactors = FALSE)
  theseShared <- shareMap$healthCode[ shareMap$sharingScope == "all_qualified_researchers" ]
  
  ## VARIABLES TO BE USED ACROSS TABLES
  firstDate <- as.Date("2015-03-09")
  lastDate <- Sys.Date()
  # lastDate <- as.Date("2015-09-09")
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
                       "version 1.5.1, build 10")

  ## GET ALL OF THE COLUMNS
  allCols <- synGetColumns(synId)@content
  stringCols <- whichColumns(allCols, "STRING")
  fhCols <- whichColumns(allCols, "FILEHANDLE")
  
  ## GET THE RAW DATA IN A CERTAIN RANGE DATA
  df <- synTableQuery(paste0("SELECT * FROM ", synId))@values
  df <- df[ which(df$healthCode %in% theseShared), ]
  ## DO SOME CLEANING
  for(i in stringCols){
    df[[i]] <- cleanString(df[[i]])
  }
  remCols <- c("externalId", "dataGroups", "uploadDate", "createdOnTimeZone", "userSharingScope", "validationErrors")
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
dayOne <- cleanTable("syn3420238")
# device - multiple choice
# labwork - (1,2)
# welcome - T/F

## PAR-Q QUIZ
parq <- cleanTable("syn3420257")
# all T/F

## DAILY CHECK
dailyCheck <- cleanTable("syn3420261")
# two activities

## ACTIVITY AND SLEEP SURVEY
activitySleep <- cleanTable("syn3420264")
# numbers and T/F

## RISK FACTOR SURVEY
rf1 <- cleanTable("syn3420385")
rf2 <- cleanTable("syn4703171")
for(cc in setdiff(names(rf2$data), names(rf1$data))){
  rf1$data[[cc]] <- NA
}
riskFactor <- list(data=rbind(rf1$data, rf2$data),
                   fhCols=rf2$fhCols)
# family_history, heart_disease, medications_to_treat, vascular, ethinicity, race, education

## CARDIO DIET SURVEY
cardioDiet <- cleanTable("syn3420518")
# fish, fruit, grains, sodium, sugar_drinks, vegetable

## SATISFIED SURVEY
sf1 <- cleanTable("syn3420615")
sf2 <- cleanTable("syn4857042")
satisfied <- list(data=rbind(sf1$data, sf2$data),
                  fhCols=sf2$fhCols)
satisfied$data$zip <- formatC(satisfied$data$zip, width=3, flag="0")
theseZips <- c("036", "692", "878", "059", "790", "879", "063", "821", "884", "102", "823", "890", "203", "830", "893", "556", "831")
satisfied$data$zip[ which(satisfied$data$zip %in% theseZips) ] <- "000"
# feel_worthwhile1, feel_worthwhile2, feel_worthwhile3, feel_worthwhile4, riskfactors1, riskfactors2, riskfactors3, riskfactors4, satisfiedwith_life, zip

## SIX MINUTE WALK TASK
sm1 <- cleanTable("syn3458480")
sm2 <- cleanTable("syn4857044")
for(cc in setdiff(names(sm2$data), names(sm1$data))){
  sm1$data[[cc]] <- NA
}
smWalk <- list(data=rbind(sm1$data, sm2$data),
                   fhCols=sm2$fhCols)
# pedometer_fitness.walk.items, accel_fitness_walk.json.items, deviceMotion_fitness.walk.items, HKQuantityTypeIdentifierHeartRate_fitness.walk.items, accel_fitness_rest.json.items, deviceMotion_fitness.rest.items, HKQuantityTypeIdentifierHeartRate_fitness.rest.items, measurementSystem.measurementSystem, measurementSystem.deviceRegion

## APH HEART AGE SURVEY
aph1 <- cleanTable("syn3458936")
aph2 <- cleanTable("syn4586968")
heartAge <- list(data=rbind(aph1$data, aph2$data),
                 fhCols=aph2$fhCols)
heartAge$data$heartAgeDataGender <- gsub("HKBiologicalSex", "", heartAge$data$heartAgeDataGender, fixed = TRUE)
# bloodPressureInstruction, bloodPressureInstruction_unit, heartAgeDataBloodGlucose, heartAgeDataBloodGlucose_unit, heartAgeDataDiabetes, heartAgeDataGender, heartAgeDataEthnicity, heartAgeDataHdl, heartAgeDataHdl_unit, heartAgeDataHypertension, heartAgeDataLdl, heartAgeDataLdl_unit, smokingHistory, heartAgeDataSystolicBloodPressure, heartAgeDataSystolicBloodPressure_unit, heartAgeDataTotalCholesterol, heartAgeDataTotalCholesterol_unit, heartAgeDataAge
# age is character -- and has massive outliers

## DEMOGRAPHICS
# syn3917840
demographics <- cleanTable("syn3917840")
names(demographics$data) <- sub("NonIdentifiableDemographics.json.", "", names(demographics$data), fixed=TRUE)
notThese <- grep("NonIdentifiableDemographics.", names(demographics$data), fixed=TRUE)
demographics$data <- demographics$data[, -notThese]
# patientWeightPounds, patientBiologicalSex, patientHeightInches, patientWakeUpTime, patientCurrentAge, patientGoSleepTime

# outputProjId <- "syn11269541"




# require(synapseClient)
# synapseLogin()
# 
# 
# theseTables <- synQuery("SELECT id, name FROM table WHERE parentId=='syn3270436'")
# 
# tId <- theseTables$table.id[ grep("appVersion", theseTables$table.name, fixed=TRUE) ]
# tNames <- synTableQuery(paste0('SELECT DISTINCT originalTable FROM ', tId))@values$originalTable
# 
# theseTables <- theseTables[ theseTables$table.name %in% tNames, ]
# 
# res <- lapply(theseTables$table.id, function(x){
#   tq <- synTableQuery(paste0('SELECT healthCode, appVersion FROM ', x))@values
#   return(tq)
# })
# names(res) <- theseTables$table.name
# 
# sapply(res, function(x){table(x$appVersion)})
