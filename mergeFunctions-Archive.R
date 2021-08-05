# Call required packages 
requireOrInstall(tidyverse)
requireOrInstall(readxl)
requireOrInstall(googledrive)
requireOrInstall(googlesheets4)
requireOrInstall(nnet)

# pre-set paths for data housed in REDACTED.  Changed changed variable names to maximize consitency with discussion (i.e. SDC/PDC)
# working directory stem (wdstem) set in mergeCommands
.locationSDC <- paste0(wdStem,"Score_defined_cues")
.locationPDC <- paste0(wdStem,"Performance_defined_cues")
.locationERP <- paste0(wdStem,"Perceptual_Data")

.summaryFiltering=0  # for globally controlling amount of summary to window  Note note implemented yet

# URLs for data housed online
.setInfoURL <- #REDACTED
.setInfoSheet <- ("Set Info")
.albumInfoURL <- #REDACTED
.albumInfoSheet <- ("Album Info")
.expDetailsSheet <- ("Experiment Details")

#In-progess Folder variables
.IpLocationSDC <- paste0(.locationSDC,"/-in_progress")
.IpLocationPDC <- paste0(.locationPDC,"/-in_progress")
.IpLocationERP <- paste0(.locationERP,"/-in_progress")
.legacyLocationERP <- paste0(.locationERP,"/-legacy_data")
.legacyLocationSDC <- paste0(.locationSDC,"/-legacy_data")

.albumInfoCopy=NULL    # local copy of GDrive album info
.setInfoCopy=NULL    # local copy of GDrive set info

###### MAIN FUNCTIONS (Private) ##########

#### Pull "Set level" information from Drive sheet ###
# refreshGdriveInfo: Defaults to using local copy to avoid excessive pulls
#                    But pulls a "fresh" copy (if T) or if no local copy exists
.getSetInfo <- function(refreshGdriveInfo=F)
{  # Return Set Info data from Google Doc
  echoOutput=T
  if (refreshGdriveInfo)     # flag to force refreshing of info from Google drive
  {                          # typically false so it only loads once per session (rather than every time info is asked for)
    cat ("refreshing setInfo from Gdrive \n")
    .setInfoCopy<<-NULL         # Note: unsure about memory leak issues with changing to NULL but not removing, so review later
  }
  #cat ("about to check if albumInfoCopy is NULL \n")
  if (is.null(.setInfoCopy))    # if no local copy of setinfo, then read in
  {  
    cat ("Pulling Set Info from Gdrive \n")
    # note: I changed range argument from 2:100 to 2:300 as of 2021-07-29
    setDat <- read_sheet(.setInfoURL,.setInfoSheet,range="2:300") #Read in Google Sheet
    setDat <- setDat[,-1] #Remove header rows
    setDat$era <- tolower(setDat$era)         #Make everything lowercase to line up with other sheets
    setDat$composer <- tolower(setDat$composer)
    setDat <- lapply(setDat,function(x){as.factor(as.character(x))})#Make everything a factor
    setDat <- as.data.frame(setDat) #Check for data frame again (May be redundant)
    setDat=subset(setDat,!is.na(set))   # remove if we didn't assign a set number (also gets rid of blank rows at end)
    # temp=.addSetcode(temp)   # add in the setCode  # now thinking should be done after the final merge
    .setInfoCopy<<-setDat
  }
  
  if (echoOutput)
    displaySummary(.setInfoCopy)
  return(.setInfoCopy)
}

#### Pull Album information from Drive sheet ###
.getAlbumInfo <- function(refreshGdriveInfo=F)
{   #Return album info data from Google Sheets
  echoOutput=T
  #BEN-is there a way to read from 2:end of file?    Would be better not to have to update as this grows
    #Unfortunately not, read_sheet() takes in a string and doesn't recognize normal indexing syntax. Might have to do with not being able to determine where the "end"
  if (refreshGdriveInfo)     # flag to force refreshing of info from Google drive
  {                          # typically false so it only loads once per session (rather than every time info is asked for)
    cat ("refreshing albumInfo from Gdrive \n")
    .albumInfoCopy<<-NULL         # Note: unsure about memory leak issues with changing to NULL but not removing, so review later
  }
  #cat ("about to check if albumInfoCopy is NULL \n")
  if (is.null(.albumInfoCopy))    # if no local copy of albumInfo, then read in
  {  
    cat ("Pulling Album Info from Gdrive \n")
    # note: I changed range argument from 2:100 to 2:300 as of 2021-07-29 CA
    albumInfo <- read_sheet(.albumInfoURL,.albumInfoSheet,range="2:300")  #Read in Google Sheet  
    albumInfo <- lapply(albumInfo,function(x){as.factor(as.character(x))})#Make everything a factor
    albumInfo <- as.data.frame(albumInfo) #Check for data.frame
    albumInfo <- select(albumInfo,-c(type)) #Remove redundant type( In another data set)
    albumInfo=subset(albumInfo,!is.na(composer)) #Remove any rows without composer
    .albumInfoCopy<<-albumInfo
  }
  
  if (echoOutput)
    displaySummary(.albumInfoCopy)  
  return(.albumInfoCopy)
}

#Pull Experiment Details from Drive Sheet
.getExpDetails <- function(refreshGdriveInfo=F)
{   #Return album info data from Google Sheets
  #echoOutput=T
  if (refreshGdriveInfo)     # flag to force refreshing of info from Google drive
  {                          # typically false so it only loads once per session (rather than every time info is asked for)
    cat ("refreshing albumInfo from Gdrive \n")
    .expDetailsCopy<<-NULL         # Note: unsure about memory leak issues with changing to NULL but not removing, so review later
  }
  #cat ("about to check if albumInfoCopy is NULL \n")
  if (is.null(.expDetailsCopy))    # if no local copy of albumInfo, then read in
  {  
    cat ("Pulling Album Info from Gdrive \n")
    expDetails <- read_sheet(.albumInfoURL,.expDetailsSheet)  #Read in Google Sheet  
    expDetails <- lapply(expDetails,function(x){as.factor(as.character(x))})#Make everything a factor
    expDetails <- as.data.frame(expDetails) #Check for data.frame
    #expDetails <- select(expDetails,-c(type)) #Remove redundant type( In another data set)
    expDetails=subset(expDetails,!is.na(composer)) #Remove any rows without composer
    .expDetailsCopy<<-expDetails
  }
  
  #if (echoOutput)
  #  displaySummary(.expDetailsCopy)  
  return(.expDetailsCopy)
}

#### Load spreadsheets with Score Defined Cues (SDCs).  Also referred to as "Extractions" ###
# define and explain each of the variables here
# scoreDir: Location of the score defined cues
# collapseMeasures: F retains measure-by-measure breakdown/T collapses using mmWt
.getSDC <- function(scoreDir=.locationSDC,includeIP= F)
{  #Return data from Compilations in the DB
  echoOutput=T
  setwd(scoreDir)#Set working directory to folder with all Files you want to bind
  scoreList=list.files(pattern=".xlsx") #Get list of all files to merge
  scoreCues=list() #Create empty list
  if (length(scoreList)==0)
  {
     cat ("No SDC found 'in progress', returning to regular directory\n")
     return (NULL)
  }
  for (scoreIndex in 1:length(scoreList)) #Performs functions for everything in the list
    #Index is the order of items in the list
  {#Read in and bind all data from the folder
    #Assign performs the read function on each item in the list, do.call('rbind',performaceList) might work better
    scoreDat <- assign(scoreList[scoreIndex], 
                       read_xlsx(scoreList[scoreIndex],sheet='Compilation')) #Pick from specific spreadsheet
    scoreDat <- data.frame(scoreDat) #Turn to data.frame
    if (!("pieceID" %in% colnames(scoreDat))) 
    {  #Check if uniquePiece is there
      scoreDat$pieceID<- array(with(scoreDat, 
                                    paste0(substring(mode, 1, 1), chroma))) #If not, Make a new column for pieceID
    }
    if (scoreIndex == 1) #Bind first sheet to empty dataframe
    {
      scoreCues <- rbind(scoreCues,scoreDat) #Bind each dataframe to the list
    } 
      else if (ncol(scoreDat)==ncol(scoreCues)) #All other sheets must match column numbers
      {#CHECKS FOR THE CORRECT NUMBER OF COLUMNS, DOES NOT CHECK FOR COLUMN NAMES. 
        #Tried that and we getting lots and lots of warnings with colnames.
        scoreCues <- rbind(scoreCues,scoreDat) #Bind items to list 
      } 
      else # If a sheet doesn't match column numbers, error message is printed and that sheet is skipped
      {
        print("Score Cues Error")
      }
  }

  scoreCues <- lapply(scoreCues,function(x){as.factor(x)}) #Make everything a factor
  scoreCues <- data.frame(scoreCues) #Ensure scoreCues is a data.frame
  major <- subset(scoreCues,mode=="Major") #Separate Major and minor USE is.na(mode) WHEN COLUMNS BLANK IN EXCEL FILES
  minor <- subset(scoreCues,mode=="minor")
  naMode <- scoreCues[is.na(scoreCues$mode),]
  minor$key <- tolower(minor$key) #Lowercase the Key column to align with mode
  scoreCues <- rbind(major,minor,naMode) # Rebind Major, minor, and NA
  if ("arScore" %in% colnames(scoreCues))   # cols have different names in legacy data
    scoreCues$arScore <- as.numeric(as.character(scoreCues$arScore)) #Turn these into numeric again for later calculation
  if ("pitchHeight" %in% colnames(scoreCues))  # so these checkes are needed in order
    scoreCues$pitchHeight <- as.numeric(as.character(scoreCues$pitchHeight))
  if ("attacks" %in% colnames(scoreCues))   # to use same code on them
    scoreCues$attacks <- as.numeric(as.character(scoreCues$attacks))
  
  if (echoOutput)
    displaySummary(scoreCues)
  
  if (includeIP) # Code to include IP cues if just analyzing this specific data set
  {
    cat ("    ...and now getting 'In progress' SDC...\n ")
    inProgressSDC <- .getSDC(.IpLocationSDC,includeIP = F) #Need to set as false here, otherwise will loop infinitely, reading the same inProg folder.
    if (!is.null(inProgressSDC))   # skip if there there weren't actually any cues in progress
      scoreCues <- rbind(scoreCues,inProgressSDC) #Bind inProgress with data from the "normal" folder
  }
  return(scoreCues)
}
#For Legacy SDC data specifically
.getLegacySDC=function()
{
  echoOutput=T
  
  legDat=.getSDC(.legacyLocationSDC)  # read in legacy ERP
  phCol=which(colnames(legDat)=="pitchHeight")  # figure out which columns   
  arCol=which(colnames(legDat)=="articRate")   # contain pitchHeight and attackRate
  colnames(legDat)[phCol]="pitchHeight"  # rename pitchHeight to avoid duplicate col names when mereged with SDC values
  colnames(legDat)[arCol]="arScore"   # album-derived timings previously encoded
  legDat$pitchHeight <- as.numeric(as.character(legDat$pitchHeight))  # not handled automatically in .getSDC
  legDat$arScore <- as.numeric(as.character(legDat$arScore))    # since col names slightly different
  
  #Adding columns and values to align with out update SDC formatting 
  legDat$set=1     # add in set info (started using this more recently)
  legDat$set[legDat$corpus=="wtc1Fugues"]=2 #These are hardcoded to Bach as wtc1 is the only set in the legacy data. 
  legDat$timeSignature= NA
  legDat$mm= NA
  legDat$assignedTempo= NA
  legDat$notatedTempo= NA
  legDat$attacks= NA
  legDat$mmWt= NA
  legDat$issues= NA
  legDat=.addSetcode(legDat)   # for convenience
  
  #Reordering legacy data columns to correctly bind to 
  legDat <- legDat[,c("corpus","composer","title","year","era","set","pieceID",
                "num","key","chroma","mode","timeSignature","mm","assignedTempo",
                "notatedTempo","adjTempo","attacks","arScore","pitchHeight","mmWt","issues")]
  if (echoOutput)
    displaySummary(legDat)
  return (legDat)
}

#### Read in Performance Defined Cues (PDCs).  Album specific-information ###
.getPDC <- function(performanceDir=.locationPDC,includeIP= F)
{        # Return performance data from DB
  echoOutput=T
  setwd(performanceDir)               #Set working directory to folder with all Files you want to bind
  performanceList=list.files(pattern="*.xlsx") #Get list of all files to merge
  perfDat=list() #Create empty list
  for (performanceIndex in 1:length(performanceList))
  {#Index is the order of files from the list
    #Assign performs the read function on each item in the list, do.call('rbind',performaceList) might work better
    dat <- assign(performanceList[performanceIndex], read_xlsx(performanceList[performanceIndex])) #Pick from specific spreadsheet
    dat <- data.frame(dat) #Turn to data.frame
    dat <-  mutate(dat, composer = stringr::str_to_lower(composer)) #Change all composer names to the standard lowercase version
    dat$composer <- as.factor(dat$composer) #Change composer back to factor 
    dat <- subset(dat,select=c("composer","set","pieceID","albumID","beatDiv","durationSec",
                               "perfTempo","rmsSeeWaveFade","rmsSeeWaveNoFade","rmsAudacityFade",
                               "rmsAudacityNoFade"))
  
    if (performanceIndex == 1)#Refer to the similar section in getSDC for info on the indexing
      perfDat <- rbind(perfDat,dat) # Get first data set into the list
    else if (ncol(dat)==ncol(perfDat)) # Check the number of columns from each new data added against the number of columns of the first file
      perfDat <- rbind(perfDat,dat) #Add data in
    else print(paste("Performance cue error: Mismatching columns in ",unique(dat$albumID)))
  }
  perfDat <- lapply(perfDat,function(x){as.factor(x)})#Make everything a factor
  perfDat <- data.frame(perfDat)
  if (echoOutput)
    displaySummary(perfDat)
  if (includeIP)
  {
    cat ("    ...and now getting 'In progress' PDC...\n ")
    inProgressPDC <- .getPDC(.IpLocationPDC,includeIP = F) #Must be false to avoid looping.Basically just changing the folder location and rerunning the same function.
    perfDat <- rbind(perfDat,inProgressPDC)
  }
  return(perfDat)
}

#### Read in Perceptual Ratings. (acquired from participants in experiments) ###
.getERP<- function(ratingsDir=.locationERP,includeIP= F)
{      
  echoOutput=T 
  setwd(ratingsDir)              #Set working directory to folder with all Files you want to bind
  ratingsList=list.files(pattern="*.csv") #Get list of all files to merge
  expDat=list() #Create empty list
  for (ratingsIndex in 1:length(ratingsList))
  {
    #cat ("just read in file ",ratingsIndex, "\n")
    #Assign performs the read function on each item in the list, do.call('rbind',performaceList) might work better
    dat <- assign(ratingsList[ratingsIndex], read.csv(ratingsList[ratingsIndex])) #Pick from specific spreadsheet
    dat <- lapply(dat,function(x){as.factor(x)}) #Make everything a factor
    dat <- data.frame(dat) #Turn to data.frame
    #dat$subj=as.numeric(as.factor(as.numeric(unique(dat$participant))))  # make consecutive subject numbers starting at 1
    dat$subj= as.integer(as.factor(dat$participant))  # Cam's fix
    if (ratingsIndex == 1) #Refer to same section in getSDC
      expDat <- rbind(expDat,dat)
    else if (ncol(dat)==ncol(expDat))  # The !=1 check is irrelevant here
      expDat <- rbind(expDat,dat)
    else print("Ratings Error") #If # of columns don't match
  }
  expDat <- lapply(expDat,function(x){as.factor(x)})#Make everything a factor
  expDat <- data.frame(expDat) #Check for data frame
  expDat <- expDat[,-1] #Remove extra index column
  if (echoOutput)
    displaySummary(expDat)
  if (includeIP) # Adds in Progess data if the argument is True in this specific function. Only used when called indepedently
  {
    cat ("       ....checking for 'In Progress' ratings. \n       ....")
    inProgressPR <- .getERP(.IpLocationERP,includeIP = F) #Must be false to avoid looping.Basically just changing the folder location and rerunning the same function. 
    expDat <- rbind(expDat,inProgressPR) # Bind inP rogress and final 
  }
  cam <- c(#REDACTED) 
    )#List of ID's associated with Cam
  expDat <- dplyr::filter(expDat, !(participant %in% cam)) #Remove Cam's ids
  return(expDat)
}

##Collect specific performer attack rate

.addArPerf <- function (dat){
  #Provides estimated durations and performer attack rates for each measure
  temp <- dat %>% 
    group_by(composer,set,albumID,pieceID,participant) %>% #Collapse across measures
    summarize(mmWtTotal=sum(mmWt)) # Sum mm values
  dat <- merge(dat,temp,by=c("composer","set","albumID","pieceID","participant")) #Merge into full
  dat$mmDuration <- ((dat$durationSec)/(dat$mmWtTotal)) # Gets duration for each measure 
  dat$arPerf <- ((dat$attacks)/(dat$mmDuration))   # Gets the  performer attack rate by dividing attacks per measure by measure duration
  return(dat)
}

#Changes everything that needs to be a numeric into a numeric (from character or factor)
#also adds set/albumCodes for displaySumamry
.tidyDat=function(dat)
{
  dat$num <- reformat(dat$num)
  dat$yearSimple <- reformat(dat$yearSimple)
  dat$assignedTempo <- reformat(dat$assignedTempo) #Change things thare should be numerics to num
  dat$notatedTempo <- reformat(dat$notatedTempo)
  dat$adjTempo <- reformat(dat$adjTempo)
  dat$mm <- reformat(dat$mm)
  dat$perfTempo <- reformat(dat$perfTempo)
  dat$rmsSeeWaveFade <- reformat(dat$rmsSeeWaveFade)
  dat$rmsSeeWaveNoFade <- reformat(dat$rmsSeeWaveNoFade)
  dat$rmsAudacityFade <- reformat(dat$rmsAudacityFade)
  dat$rmsAudacityNoFade <- reformat(dat$rmsAudacityNoFade)
  dat$valence <- reformat(dat$valence)
  dat$arousal <- reformat(dat$arousal)
  dat$attacks <- reformat(dat$attacks)  #Make numeric for calulations
  dat$durationSec <- reformat(dat$durationSec)
  dat$mmWt <- reformat(dat$mmWt) # Prepare for calculations below
  
  dat=.addSetcode(dat) #Adding set Code
  
  dat$albumCode=paste(dat$setCode,dat$performer,sep="-") #Creating album code
  dat$albumCode=factor(dat$albumCode)
  dat=subset(dat,!is.na(set))  # get rid of entries that are not being explored (if they lack a set number not of functional interest)
  dat[]=lapply(dat, function(column) if(is.factor(column)) factor(column) else column)  # drop all unused levels
  
  return(dat)
}

# Removes columns not typically needed, asigns a single "standardized" RMS value
# rmsValuesFrom: Column to use as basis for the assigned "standard" RMS value.  
# note in future upgrade so that if rmsValuesFrom is set to NULL or NA then nothing 
.prepDat=function(dat)
{
  retDat=dat    # save a local copy
  # only needed in gluing together the data set, but apparently still has use/need
  #retDat=subset(dat,select=-c(pieceID,albumID))
  # pull out stuff we're *virtual never going to need
  retDat=subset(retDat,select=-c(opus,title,work,corpus,
                                 performedOnDetailed,recordingCut,backupLoc,notes,
                                 pieces.for.cam.to.check,pdcIP,pdcVerify))
  # pull out things we're rarely likely to need
  retDat=subset(retDat,select=-c(type,beatDiv,perfTempo,primaryPerf,isPrimary,
                                 year,yearRecorded,yearReleased,performedOn,ra,issues))
  # could be useful for some analyses but often not
  retDat=subset(retDat,select=-c(timeSignature,assignedTempo,notatedTempo,adjTempo))
  return(retDat)
}

#Relevels pertinent columns into orders that make more sense
#Can easily include other columns
.relevelColumns <- function(dat)
{
  dat$composer <- fct_relevel(dat$composer,sort)
  dat$albumID <- fct_relevel(dat$albumID,sort)
  keyLevel <- c("c","C","C/a","c#","C#","d","D","D/b","Db","Db/bb","e",
                "E","E/c#","eb","d#","Eb","Eb/c","f","F","F/d","f#","F#","F#/d#",
                "g","G","G/e","g#","Gb","Gb/bb","Gb/eb",
                "a","A","A/f#","ab","Ab","Ab/f","b","B","B/g#","bb","Bb","Bb/g") #Hardcoding order of keys, can be modified in the future
  dat$key <- factor(dat$key,keyLevel)
  
  dat$era <- factor(dat$era,c("baroque","classical","romantic","20th century")) #Hardcoding order of eras
  return(dat)
}

################################################################

#PRIVATE FUNCTIONS I DID NOT WRITE
#Some of these functions were implemented into my own so I've left them in
#Public functions I wrote start on 504, or you can hit the small arrow next to 385 (at least in RStudio)
################################################################

# function to summarize data set brought in from REDACTED.  Mostly to aid manual checking/validation of fields
# dat: Dat to summarize
# style: Specific outputs for SDC, PDC, Perceptual data.  if NA does a generic summary
displaySummary=function(dat,style=NA) 
{
  if (is.na(style))    # if not explicitly defined
    style=.dataType(dat)  # simply determine from data
  if (style=="setInfo") 
  {
    # must be a way to break this into several columns in output so that it doesn't take so many lines
    dat=.addSetcode(dat)  # add in setCode info
    temp=plyr::count(dat$setCode)  # for some reason can't do this with dplyr
    knownSetSummary=paste(temp[,1], ifelse(temp[,2]==1,"",paste("(",temp[,2]," seperate publications)",sep="")))
    cat ("'Set Info' sheet in Gdrive contains", length(knownSetSummary),"sets.\n")
    for (i in 1:length(knownSetSummary))
      cat ("  ",knownSetSummary[i],"\n")
    #cat (knownSetSummary)
  }
  
  if (style=="albumInfo") 
  {
    dat=.addSetcode(dat)  # add in setCode info
    setsWithAlbums=unique(as.character(dat$setCode))  # list of all sets for which we have albumIDs defined in albumInfo
    cat ("Album Info sheet in Gdrive includes", length(unique(dat$albumID)),"albums","from", length(setsWithAlbums),"sets","\n")
    for (curSet in 1:length(setsWithAlbums))  # iterate through each set
    {
      tempDat=subset(dat,setCode==setsWithAlbums[curSet])   # extract only that particular set to look at recordings
      cat ("  ",length(unique(tempDat$albumID)),"albums include",setsWithAlbums[curSet],"  These are:",as.character(unique(tempDat$albumID))," \n")
    }  
  }
  
  if (style=="SDC") 
  {
    if ("set" %in% colnames(dat))   # legacy SDC from poon don't have "set" defined (will be added in later)
    {
      dat$setCode=paste(dat$composer,dat$set,sep="-")  # make a temp set code
      temp=aggregate(as.numeric(as.character(set))~setCode,FUN=mean,data=dat)  # get list of composer-set pairs
      setCodeList=unique(temp$setCode) #Create list of unique codes
      cat ("Found SDC for",length(setCodeList),"sets:\n") # Show # of sets
      for (index in 1:length(setCodeList))
      {
        tempSet=subset(dat,setCode==setCodeList[index])
        tempSet$pieceID=droplevels(tempSet$pieceID)
        cat ("    ", setCodeList[index], "with",length(unique(tempSet$pieceID)), "pieces: (",sort(as.character(unique(tempSet$pieceID))), ")\n") #Create summarizing statement for output
      }
      code=paste(as.character(dat$composer),"-",as.character(dat$set),sep="")   # Glue outputs together 
    }
    #else cat ("Reading in legacy SDC, cannot calculate display details\n")  
  }
  
  if (style=="PDC")
  {
    
    dat$setCode=paste(dat$composer,dat$set,sep="-")  # make a temp set code
    dat$setAlbumCode=paste(dat$composer,dat$set,dat$album,sep="-")  # make a temp set code
    
    temp=aggregate(as.numeric(as.character(set))~setAlbumCode+setCode+albumID+pieceID,FUN=mean,data=dat)  # get list of composer-set pairs
    setAlbumList=as.character(unique(temp$setAlbumCode))
    cat ("Found PDC for",length(setAlbumList),"albums:\n")
    for (index in 1:length(setAlbumList))
    {
      tempSet=subset(dat,setAlbumCode==setAlbumList[index])
      tempSet$pieceID=droplevels(tempSet$pieceID)
      cat ("    ", as.character(unique(tempSet$albumID)), "(",unique(tempSet$setCode),"), with",length(unique(tempSet$pieceID)), "pieces:",sort(as.character(unique(tempSet$pieceID))), "\n")
    }
    code=paste(as.character(dat$composer),"-",as.character(dat$set),sep="")   
  }
  if (style=="ERP")
  {
    code=paste(as.character(dat$expID),": ",as.character(dat$composer),"-",as.character(dat$set), " (", as.character(dat$albumID),")",sep="")
    expList=unique(dat$expID)
    
    #summaryList=unique(code)
    cat ("Found participant ratings from",length(expList),"experiments:\n")
    
    for (index in 1:length(expList))
    {
      tempSet=subset(dat,expID==expList[index])
      tempSet$setCode=paste(tempSet$composer,tempSet$set,sep="-")
      tempSet$setCode2=paste(tempSet$setCode,as.character(tempSet$albumID), sep=", ")  # horrible way of doing this, but works for now
      numSubj=length(unique(tempSet$subj))
      cat ("   ",as.character(unique(tempSet$expID)), "(",numSubj,"subj).",as.character(unique(tempSet$setCode2)), "(",length(as.character(unique(tempSet$pieceID))),") pieces:", sort(as.character(unique(tempSet$pieceID))), ")\n")
    }
    
  }
  
}

# convenience function to identify the type of data (PDC, SetInfo, etc.) based on key column identifiers
# note if column names are changed in future this will possibly fail!
#Used mainly in display summary 
.dataType=function(dat)
{
  retVal=NA
  if (any(colnames(dat)=="title"))
    retVal="setInfo"
  if (any(colnames(dat)=="recordingCut"))
    retVal="albumInfo"
  if (any(colnames(dat)=="expID"))
    retVal="ERP"
  if (any(colnames(dat)=="durationSec"))
    retVal="PDC"
  if (any(colnames(dat)=="pitchHeight"))
    retVal="SDC"
  return (retVal)
}

#convenience function to add setCode to dataset
.addSetcode=function(dat)
{
  dat$setCode=paste(dat$composer,dat$set,sep="-")  # make a temp set code
  dat$setCode=factor(dat$setCode) #turned to factor and levels set
  return (dat)
}

##### Public functions for integration/cleaning   #####

# Public functions --------------------------------------------------------


#Left merging takes all the rows of one set and glues any other rows that fit from the other data frame
#Results in a lot of NA's but has some more info availible

buildData <- function(sdc, pdc, erp, mainRMS="rmsAudacityNoFade", refreshGdriveInfo=F, shrinkData=T)
{  #Get massive data set 
  
  # cue info comes from from locations
  setInfo=.getSetInfo(refreshGdriveInfo)   # get fresh copy if signaled (or if not exists)
  setInfo <- setInfo[,-c(3,5,10:length(colnames(setInfo)))] # Get Rid of Extra Columns
  albumInfo=.getAlbumInfo(refreshGdriveInfo)  # (only reads from Gdrive if needed)
  sdc <- select(sdc,-c(era,title))            # Prevent redundancy in merge (include in .getSDC() ?)
  
  # First merge the cues themselves.  Note: (x/y) refer to (first/second) dataframe
  # Appends unique columns from df-y to right of df-x
  allNotatedCues <- merge(sdc,setInfo, by=c("composer","set"), all.y=TRUE) #Right merge by setInfo, keeps all set info rows
  allPerformedCues <- merge(pdc,albumInfo, by=c("composer","set","albumID"), all.y=TRUE) #Right merge by albumID, keeps all album info
  allCues<- merge(allNotatedCues,allPerformedCues, by=c("composer","set","pieceID"),all.x=TRUE) # Left  merge by allNotatedCues, keeps al columns from setInfo still
  
  # now merge cues with experimental data
  master <- merge(allCues,erp, by=c("composer","set","pieceID","albumID"),all.x=TRUE) # Right merge by allCues, keeps setInfo rows
  
  master <- .tidyDat(master) #Reformats many columns
  master <- .addArPerf(master)  #Adding performer attack rate
  if (shrinkData){master <- .prepDat(master)} #Remove extraneous columns
  
  master$rms <- master[,mainRMS] #Set a standard RMS 
  
  master <- .relevelColumns(master) #Releveling many columns to more sensible orders 
  
  #cat ("at end of function,is.null(.setInfoCopy)=",is.null(.setInfoCopy))
  return(master)
}

#Select only the best experiment ID's from the whole dataset based on the Experiment Details drive sheet
getBestExpID <- function(dat,refreshGdriveInfo=T)
{
  expDeets <- .getExpDetails(refreshGdriveInfo) #Read in exp Details
  expDeets <- subset(expDeets,select=c("composer","albumID","expID","bestExperiments")) #Trim columns
  
  withBestExp <- merge(expDeets,dataset, by=c("composer","albumID","expID"), all.y=TRUE) #Merge with main data
  
  withBestExp <- subset(withBestExp,bestExperiments==TRUE) #Select only the best experiments
  
  return(withBestExp)
}

# Collapses by piece (one row per-measure), passing aggregate perceptual ratings through
# Note: perceptual ratings (valence/arousal/labels) are collapsed across all experiments for a given piece  
# Useful for rough exploration, but SHOULD NOT be taken as "true" values
# Note: can be done before or after running collapseMeasures (but collapseMeasures only works when run first)
# Note: annoyingly, gives error about "Unknown or uninitialized column: 'mm' when adding this in.  Not sure why
# it's a a "legit" thing to do.  Some other complaints about this online but unclear as of Nov 1, 2020
cuesAlone=function(dat,compact=F)
{
  # would be better to make mm an "option" field to include if it exists
  # right now hack allows it to work but rather inelegant
  retDat=dat
  removeMM=F  # flag to track whether to remove mm column before completing function
  if (is.null(retDat$mm))
  {
    retDat$mm=99  # engage in deceptive hack.  Add in dummy value so summarize still works
    removeMM=T    # flag to remove this later
  }
  catLevels=levels(retDat$label)
  retDat <- retDat %>% 
    group_by(#setCode,
             albumCode,performer,composer,set,mode,key,mm, # Musical cues (SDC and PDC)
             yearSimple, albumID,pieceID)    %>% # Other needed info just don't want it up front
    summarize(arScore=mean(arScore),pitchHeight=mean(pitchHeight),rms=mean(rms), 
              arPerf=mean(arPerf),mmDuration=mean(mmDuration),
              arousal=mean(arousal),valence=mean(valence),label=popularLabel(label))
  
  retDat$label=factor(retDat$label)  # should be a factor
  if (removeMM)   # check flag to see if mm was a hack, and if so 
    retDat=subset(retDat,select=-mm)   #  remove it
  if (compact)
    retDat=subset(retDat,!is.na(pitchHeight))  # remove entries that don't yet have SDC
  
  # handy way to drop unused levels from throughout the frame (particularly handy if compact=T)
  retDat[]=lapply(retDat, function(column) if(is.factor(column)) factor(column) else column)  
  retDat
  # would be good find a generalized way of aligning column order to put in function and then call here
  # that would make things more consistent with output here and from collapseMeasuress
}

collapseMeasures=function(dat,includePickups=T)
{
  retDat=dat
  if(!includePickups)
  {
    retDat=subset(dat,mm!=0)
  }   # simply remove these before aggregating if undesired.  This syntax works even if mm is a factor
  
  retDat <- retDat %>% 
    group_by(composer,set,albumID, pieceID, participant)    %>% # select grouping variables
    mutate(arScore = sum(arScore*mmWt)/sum(mmWt), #manipulates only select variables, leaving rest of data structure
           arPerf = sum(arPerf*mmWt)/sum(mmWt), #take mean attackrate of performer tempo
           pitchHeight = sum(pitchHeight*mmWt)/sum(mmWt),#Get weighted #'s across measures
           attacks=sum(attacks),
           durationSec = mean(durationSec))
  
  retDat=subset(retDat,select=-c(mm,mmDuration,mmWt)) #drop unnecessary variables
  retDat = retDat %>% select(composer,set,albumID,pieceID,key,chroma,mode,pitchHeight,attacks,arScore,arPerf,rms,durationSec,
                             participant,valence,arousal,label,
                             pool,expLoc,expID,subj) #Reorder labels
  #remove duplicates
  retDat %>% distinct() -> retDat 
}


# returns the most popularly chosen label within a list of labels
# designed to be used to consolidate choices for individual piece
# if all labels are NA then returns NA (table didn't seem to do this by default)
# Note: in future might also include numerical value indicating degree of "win" for a given category
# Warning when more than one label ties for the mode
popularLabel <- function(labels)
{
  if(all(is.na(labels)))
  {# if nothing but NAs
    retLabel=NA# value should be NA (which.max defaults to 1st category in this situation)
  } else 
  {
    labelList<- levels(labels) #Find all levels of the labels
    modeLabel <- labelList[which.is.max(table(labels))]#Create the variable that has A max label (not THE in the case of ties)
    if(sum(table(labels)==max(table(labels)))>1) #If the number of tables matching the largest value from the table is more than one, print warning.
    {
      message(paste("Warning 1/2: At least two labels have tied for the most common label for piece at index:",cur_group_id()))
      message("Warning 2/2: View the output dataframe to match the indices to the pieces with the tied labels.")
    }
    return(modeLabel)
  }
}

# Display-ready name of composer
# note in future expand to give options for both names, last name, or initials
# i think as it scales up this would be cleaner if this is pulled from a spreadsheet with metadata (either on drive or separate) CA
prettyComposer <- function(composerName)
{
  retName= switch(tolower(as.character(composerName)), #Will need to be actively updated
                  "alkan"="Charles-Henri-Valentin Alkan",
                  "auerbach"= "Lera Auerbach",
                  "bach"="Johann Sebastian Bach",
                  "beethoven"="Ludwig van Beethoven",
                  "blacher"="Boris Blacher",
                  "blumenfeld"="Felix Mikhailovich Blumenfeld",
                  "bowen"="Edwin York Bowen",
                  "burge"="John David Bryson Burge",
                  "busoni"="Ferruccio Busoni",
                  "casadesus"="Robert Casadesus",
                  "chasins"="Abram Chasins",
                  "clementi"="Muzio Filippo Vincenzo Francesco Saverio Clementi",
                  "chopin"="Fredric Chopin",
                  "concone"="Giuseppe Concone",
                  "cope"="David Cope",
                  "cui"="César Antonovich Cui",
                  "cumming"="Richard Cumming",
                  "debussy"="Claude Debussy",
                  "fischer"="Johann Caspar Ferdinand Fischer",
                  "gliere"="Reinhold Moritzevich Glière",
                  "guillet"="Charles Guillet",
                  "heller"="Stephen Heller",
                  "henselt"="Georg Martin Adolf von Henselt",
                  "herz"="Henri Herz",
                  "hummel"="Johann Nepomuk Hummel",
                  "kabalevsky"="Dmitry Borisovich Kabalevsky",
                  "kalkbrenner"="Friedrich Wilhelm Michael Kalkbrenner",
                  "kapustin"="Nikolai Girshevich Kapustin",
                  "kessler"="Joseph Christoph Kessler",
                  "liszt~lyapunov"="Franz Liszt & Sergei Michailovich Lyapunov",
                  "madsen"="Trygve Madsen",
                  "palmgren"="Selim Gustaf Adolf Palmgren",
                  "rääts"="Jaan Rääts",
                  "rachmaninoff"="Sergei Rachmaninoff",
                  "rachmaninov"="Sergei Rachmaninoff",
                  "rubin"="Justin Henry Rubin",
                  "scriabin"="Alexander Scriabin",
                  "shostakovich"="Dmitri Shostakovich",
                  "stanford"="Sir Charles Villiers Stanford",
                  "vierne"="Louis Victor Jules Vierne",
                  "zaderatsky"="Vsevolod Petrovich Zaderatsky",
                  "zwaag"="Wim Zwaag")
  if (is.null(retName))
    retName="Not found"
  # prettyComposer should be a function that can be called when displaying composer names 
  # organize eras chronologically 
  retName
}

# Check between any two datasets if one participant ran both experiments.
# Only for byPiece=F dichotomized dat or any other data with participant numbers. 
checkPar <- function(dat1,dat2)
{
  dat1 <- unique(dat1$participant) #Find all participant numbers in each data set.
  dat2 <- unique(dat2$participant)
  for (i in 1:length(dat1))
  {
    print(dat1[[i]] %in% dat2) #Reads out if each participant # appears in the other. 
  }
}

################################################################

#PUBLIC FUNCTIONS I DID NOT WRITE
#Some of these functions were implemented into my own so I've left them in

#That's all of my code! Thanks a lot for your interest and I'd be happy to 
#answer any questions or showcase how they work IRL!
################################################################

# handy convenience routine for pulling out composers other than debussy, alkan
'%notin%'=Negate('%in%')


# convenience function to return only SDC/PDC paired with completed experiments
completeERP=function(dat)
{
  retDat=subset (dat,!is.na(pool))
  retDat[]=lapply(retDat, function(column) if(is.factor(column)) factor(column) else column)  # drop all unused levels
  return (retDat)
}

#adding function for re-ordering columns according to desired order. We'll need to create an array of 
#the desired column ordering and use that as the masterColOrder vector. CA



##reorders columns based on user-specified order. If fromList set to TRUE, queries pre-specified lists from .fetchFlavours
.colReorder <- function(dat,fromList=F,masterColOrder)
{
  if(fromList){masterColOrder = .fetchFlavour()}
  
  require(dplyr)
  #select values from masterColOrder that exist in data:
  reducedVector <- masterColOrder[masterColOrder%in%names(dat)]
  #select and return those that do exist in order:
  retDat <- dat %>% select(all_of(reducedVector)) 
  return(retDat)
}

##added feb 18: provides options for users to choose selections of columns from the dataset based on analytical requirements,
##can add new flavours as desired but remember to update orderList with new entries (changing anything from the for loop could break functionality)
.fetchFlavour <- function()
{
.perf <- c("composer",  "performer" ,  "set" , "yearSimple",  
           "key",  "mode","pitchHeight", "arScore", "arPerf", 
           "rmsSeeWaveFade", "rmsSeeWaveNoFade","rmsAudacityFade", "rmsAudacityNoFade",  
           "valence", "arousal", "label", "subj", "pool", "expLoc", "expID")

.perfVerbose = c("composer",  "performer" ,"set","yearSimple", "primaryPerf", 
                  "performedOn", "albumID","yearRecorded", "yearReleased",  
                  "key",  "mode", "chroma",  "pitchHeight", "arScore", "arPerf", "perfTempo", 
                  "durationSec", "rmsSeeWaveFade", "rmsSeeWaveNoFade", "rmsAudacityFade", "rmsAudacityNoFade",  
                  "valence", "arousal", "label", "subj", "pool", "expLoc", "expID") 

.score = c("composer", "era",  "set" , "yearSimple",  
            "key",  "mode", "chroma",  "timeSignature", "pitchHeight", "arScore") 

.scoreVerbose = c("composer", "era",  "set" ,"opus",  "work" , "type","yearSimple",  
                   "key",  "mode", "chroma",  "pitchHeight",  
                   "attacks", "mm", "mmWt", "mmWtTotal",  "mmDuration", "arScore", 
                   "assignedTempo", "notatedTempo", "adjTempo")

.diagnostic = c("composer",  "performer" ,"primaryPerf","isPrimary","set" , 
                 "pieceID",  "recordingCut", "backupLoc", "notes","issues", "pieces.for.cam.to.check")

.all = unique(c(.perf, .perfVerbose,.score,.scoreVerbose,.diagnostic))

orderList = list("PDC" = .perf, "PDC Verbose" = .perfVerbose, 
     "SDC" = .score, "SDC Verbose" = .scoreVerbose,
     "Status" = .diagnostic, "All" = .all)

for(name in 1:length(names(orderList))) message(paste0("(",name,")", names(orderList[name])))
input = readline("Select an order from one of the following (numeric input): ")
colOrder = orderList[as.numeric(input)]
colOrder = as.vector(unlist(colOrder))
return(colOrder)
}



