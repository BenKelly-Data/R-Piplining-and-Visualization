#Example of how all this comes together for simple analysis of many different subsets
#A package of code with all this information is provided to new students to familiarize 
#themselves with the data and what analysis we've already built out

#Here I've focussed on how the main dataset comes together and how one would encounter each of my modules

# Component loader: -------------------------------------------------------
wdStem <- #REDACTED
#wdStem <- ("~/Dropbox/Lab Dropbox folders/DATA_ARCHIVE/Emotion/")
#load in analysis modules
source(paste0(wdStem,"Merge Code/Component Loader.R")) 
#I did not creat the component loader, while adds all functions and their required 
#packages we may use to the global enviroment.
loadComponents('requireOrInstall')
loadComponents()

# Data load-in ------------------------------------------------------------

#Load in base build for MUDS (Master of the Universe DataSet):
source(paste0(wdStem,"Merge Code/mergeFunctions_Current.R")) 
baseSDC <- .getSDC() #get score-defined cues. We have a private function for the Legacy data.
legacySDC <- .getLegacySDC()
basePDC <- .getPDC() #get performance-defined cues
baseERP <- .getERP() #get experiment ratings from participants
# clarify what "shrinkData"  does.  Remove extra columns?
#combine score-defined, performance-defined, and experiment rating data:
dataset <- buildData(baseSDC, #IF YOU RECEIVE A CURL ERROR, RUN AGAIN
                     basePDC,
                     baseERP,
                     mainRMS = "rmsAudacityNoFade",
                     refreshGdriveInfo=F,
                     shrinkData=F)

#take subset of data for code demonstrations:
bach <-  subset(dataset, expID == "109")
dat <- bach
#Can also use raw data subsets when cycling through multiple.

##########################
#BEGIN FROM HERE IF YOU WANT TO TEST ANY OF MY CODE ON THE BACH DATA I'VE PROVIDED
#Just use *dat <- read_csv(filepath of the trial csv)* below
##########################

# Cue Plots ---------------------------------------------------------------
#Visualizes each analyzed cue on a per piece basis. 
#Box plot replaces points with a boxplot **(does not work with mode)**
#Default is False
#y range and text size can be modified with ylim= and textSize=

#col assigns a colour to each label (rarely should be used)

#Levels orders them along the x axis
#xLab formats the labels **(MUST MATCH ORDER OF LEVELS)**
#If one or more labels is never the most common, remove that label from the levels and xLab.
#Verbose prints out the data to the console or not if not saved to GE

#Run first with swapSomeLabels as False, if there is no warning you're all set!
#If there is a warning, view the output data 

cuePlot(dat, cue="arPerf", boxplot=F,verbose=F,swapSomeLabels = F
        ,labelSwaps=list(list(12,"Light/Effervescent"),list(16,"Joyful"))) #Label swaps provided as an example of how to format (Bach  ExpID 109)
cuePlot(dat, "pitchHeight")
cuePlot(dat, "rms")
cuePlot(dat, "mode")

# Stacked Categorization Plot ---------------------------------------------
#Shows how many pieces were provided each label and how they align with Horn and Huron 2015's labels
#Order of the labels is currently hard coded as LE,J,TL,P,SR
#Levels and xLabels can both be modified as arguments.Must match order. Remove missing labels.
#ylim and textSize can be modified, and proportion does not work with byPiece plots

#Label swapping is the same as for individual cues

stackCatPlot(dat,byPiece = T,swapSomeLabels = F
             ,labelSwaps=list(list(10,"Light/Effervescent"),list(12,"Joyful"))) #Example for Bach ExpID 109
stackCatPlot(dat,byPiece=F,proportion = F) #No label swapping needed

# Association Rules -------------------------------------------------------
#ONLY WORKS ON FULL DATA (not collapsed)
#supp, conf, and mL are parameters to refine the association rules, read more in the module itself
#Sort types allows for easier inspection and identifying ranges of associations to visualize (max 100)
#Standardized lift can be added (check with Cam for code)
#DefaultType allows some ignoring of the leftHS and rightHS specifications. Can be "lhs", "rhs","both", or "none".

#leftHS and rightHS idetify what types of items should be on which sides of the association. 
#Inputs for the left/right hand sides:
#All ratings c("Sad/Relaxed", "Light/Effervescent","Joyful","Passionate","Tender/Lyrical","lowVal","midVal","highVal" ,"lowAro","midAro","highAro")
#Just labels c("Sad/Relaxed", "Light/Effervescent","Joyful","Passionate","Tender/Lyrical")
#Musical cues c("Major","minor","loud","quiet","fast","slow","high","low")
#All c("Sad/Relaxed", "Light/Effervescent","Joyful","Passionate","Tender/Lyrical","lowVal","midVal","highVal" ,"lowAro","midAro","highAro","Major","minor","loud","quiet","fast","slow","high","low")

exploreARules(bach,
              leftHS=c("Major","minor","loud","quiet","fast","slow","high","low"),
              rightHS=c("Sad/Relaxed", "Light/Effervescent","Joyful","Passionate",
                        "Tender/Lyrical","lowVal","midVal","highVal" ,"lowAro",
                        "midAro","highAro"),
              supp=0.1, 
              conf=0.1,
              mL=2, 
              sortType= "lift",
              defaultType="none",
              ruleNum=24)#Just a place holder, no Default as it should be specific to each analysis
