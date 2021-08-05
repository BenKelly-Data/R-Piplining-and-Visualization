requireOrInstall(arules, exclude = "summary")
requireOrInstall(arulesViz)
requireOrInstall(sjmisc)

#Explaination for category selection in Stacked Categorization Plot.R module

#Association rules can be tricky to understand, and I recommend this article to give you a decent overview: https://towardsdatascience.com/association-rules-2-aa9a77241654
#We are essentially looking for common co-occurrences between different types of musical cues.
#We can do this with any categorical or binned numerical (low valence= 1-2, mid valence= 3-5, etc.)
#For each participant rating, there is a "transaction" of a value for each musical cue, circumplex rating, and a label.
#This code looks through all the participants ratings to find associations between these different items.
#For example, many sad/relaxed labels from participants might be for minor pieces, and if the sad/relaxed + minor 
#occurs much more than other label with minor, then that is an interesting association.

#Creates to association list to be inspected and then visualized 
#(not sure what stats can be run on the direct output)

exploreARules <- function(aDat, #Data to analyze
                          leftHS, #Restrict the left hand side of the associations
                          rightHS, #Restrict the right hand side of the associations
                          supp=0.1, #The minimum support an association needs to be visualized
                          conf=0.1, #The minimum confidence an association needs to be visualized
                          mL=2, #The minimum number of items (length) an association needs to be visualized
                          sortType= "lift",#Identify the column by which you'd like to sort the data for best inspection
                                            #Mainly "lift", "support", or "confidence"
                          defaultType="none",#If set to "both", ignores rightHS and leftHS to include everything not specified
                          ruleNum,#Number of rules to include in the plot
                          tS=15) 
                                              #Can be "rhs" and "lhs" to ignore lists in either hand.
{
  #Make labels title case
  aDat <-  mutate(aDat, label = stringr::str_to_title(label)) 
  
  if (as.character(aDat$label[1]) %notin% c("Light/Effervescent","Joyful","Tender/Lyrical","Passionate","Sad/Relaxed"))
  {
    print("Labels absent or invalid, make sure they do not appear in the LHS or RHS") #Prints a warning to remove labels from the hands if they do not match the list
  }
  compName <- str_to_title(as.character(unique(aDat$composer)))#Assign composer for title and capitalize
  
  df <- aDat %>% #Might not be needed with new format. Formulas in summarize function are there for ease of use, but generally represetn the means of one number. 
    group_by(composer,pieceID, participant) %>% 
    summarize(AR=(sum(attacks)/unique(durationSec)), #Mean attack rate, not using arPerf to avoid complications with measure weightings
              PH=mean(pitchHeight),RMS=mean(rms), #Basically the unique values of pitch and RMS
              #comment about line 43 from Cam: why use mean() rather than unique() to summarize unique values?
              mode=unique(mode),label=unique(label),key=unique(key), #Finds the unique key, mode, and label
              valence=mean(valence),arousal=mean(arousal)) #Unique circumplex ratings from each participant rating
  
  #DICHOTIMIZE VARIABLES, match musical cues to Horn and Hurons cues (ie loudness is either loud or quiet)
  df$RMS <- as.character(dicho(df$RMS,dich.by="median")) #Dichotomize all relevant variables
  df$AR <- as.character(dicho(df$AR,dich.by="median"))
  df$PH <- as.character(dicho(df$PH,dich.by="median")) #Dichotomize by median
  df$RMS <- ifelse(df$RMS == "1","loud","quiet")#Describe the dichotimized notation cues
  df$AR <- ifelse(df$AR == "1","fast","slow") #Ex. If AR is 1, then it's fast, otherwise it's slow. On a per piece basis.
  df$PH <- ifelse(df$PH == "1","high","low")
  
  #Bin (dichotomize but with more than 2 categories) emotion ratings and rename them
  df$valence <- discretize(df$valence,"frequency", label=FALSE) 
  df$valence <- ifelse(df$valence == "1","lowVal", ifelse(df$valence=="2","midVal","highVal")) #If 1 then low, 2 is mid and 3 is high. 
  df$arousal <- discretize(df$arousal,"frequency", label=FALSE)
  df$arousal <- ifelse(df$arousal == "1","lowAro", ifelse(df$arousal=="2","midAro","highAro"))
  
  
  #Trim the identifying columns, should be agnostic of multiple composers or expIDs in a provided dataset.
  #Column remove may need to be altered if looking at composer associations. Would also have to be specified in rightHS or leftHS.
  df <- df[,-c(1,2,3)] 
  
  df <- df%>%mutate_if(is.character, as.factor) #Change everything to factors
  tdf <- as.data.frame(t(as.matrix(df))) #Transpose the dataframe (change the columns to rows)
  tdf <- as.list(tdf) #Change to list
  basket <- as(tdf,"transactions") #Change the list to a format that the aprior arules accepts
  
  rules <- arules::apriori(basket,parameter=list(support=supp, #Set variables from arguments
                                                 confidence=conf, 
                                                 minlen=mL),
                           appearance = list(rhs= rightHS , lhs= leftHS, default=defaultType)) #Doesn't seem to change anything if default is "rhs"
  
  sortedRules <- sort(rules,by=sortType, decreasing=TRUE) #Sorts the list by defined sort
  
  print(plot(sortedRules[1:ruleNum], #Print the plot with a set range of aRules results
       method = "graph"
       #,control = list(type = "items")
       )+
    ggtitle(compName)+ #Making a variable for composer name from the main association Data
    theme(panel.background=element_rect(fill="White"),
          panel.grid.major=element_line(linetype="blank"),
          plot.title=element_text(hjust=0.5,face="bold",size=20))) #Clean plot lines (everything in theme)
  
  invisible(as.data.frame(inspect(sortedRules))) #Return the data frame but do not read it to console 
}
  

