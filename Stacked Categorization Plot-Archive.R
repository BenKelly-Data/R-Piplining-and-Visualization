##Categories
## We only focus on the 5 as they were the most prominent from Horn and Huron 2015 across all eras.
##They are also much easier to explain to participants than Regal, Sneaky, and Serious music

#Creates a stacked bar plot, coloured based on the number of matching cues to Horn and Huron, either highlighting
#summarized data for individual piece or the ratings by each participant
requireOrInstall(ggrepel)
requireOrInstall(ggstance)
requireOrInstall(ggpubr)
requireOrInstall(sjmisc)

#require(sjmisc);require(ggpubr); require(yarrr); require(ggrepel); require(ggstance)
#Can add more parameters for other aspects of the graph.
#Dat should be the output of dichotomizedDat
#yLimit should be in c(#,#) format, to be adjusted for each composer or each composer comparison
#textSize helps scale all axis label text

stackCatPlot <- function(dat,byPiece=T,ylim,textSize=15, proportion=F,verbose=F, #Proportion changes the bars to represent the breakdown of each label, verbose adds the option to read data to the console.
                         swapSomeLabels=F,labelSwaps=list(list(indexNum,label),list(indexNum,label)), #Allows for the easiest swapping of labels within the function for mode ties.
                         levels=c("Light/Effervescent","Joyful","Tender/Lyrical","Passionate","Sad/Relaxed"),#Order of labels in plot
                         xlab=c("Light\nEffervescent","Joyful","Tender\nLyrical","Passionate","Sad\nRelaxed")) #Formatting the labels themselves
{
  
  if (as.character(dat$label[1]) %notin% c("light/effervescent","joyful","tender/lyrical","passionate","sad/relaxed"))
  {
    print("Labels absent or invalid...NO PLOT FOR YOU") #Does not return a plot if labels are missing or incorrect
  }else{
    composer <- str_to_title(as.character(unique(dat$composer))) #Making a variable for composer name.
    
    #Assigned colours for the Stack Plot.
    #The higher the number the darker the colour (3 is darkred, 1 is brightred etc)
    stacCol <- c("3J"="darkred","2J"="red1","1J"="tomato1","0J"="white", #Joyful
                 "3L"="darkgoldenrod4","2L"="goldenrod3","1L"="yellow1","0L"="white",#Light/Effervescent
                 "3T"="darkgreen","2T"="green3","1T"="palegreen2","0T"="white", #Tender/Lyrical
                 "3S"="blue4","2S"="dodgerblue3","1S"="skyblue2","0S"="white", #Sad/relaxed
                 "3P"="deeppink4","2P"="deeppink","1P"="pink1","0P"="white") #Passionate
    
    #dat$label <- as.factor(dat$label)
    if (byPiece)
    {
      dfCat <- dat %>% 
        group_by(composer,pieceID) %>% #Collapse across measures. Just composer and pieceID as only one exp from each composer exist here. can include other exp in the future by adding expID here.
        summarize(arPerf=((sum(attacks)/length(unique(participant)))/unique(durationSec)),#Attacks divided by participants to avoid summing attacks across all participants. Can be improved.
                  pitchHeight=mean(pitchHeight),rms=unique(rms), #Performing summary calculations
                  mode=unique(mode),label=popularLabel(label),key=unique(key),
                  LE=length(label[label=="light/effervescent"]),J=length(label[label=="joyful"]), #Adding label columns for trouble shooting 
                  TL=length(label[label=="tender/lyrical"]),P=length(label[label=="passionate"]),SR=length(label[label=="sad/relaxed"]),
                  valence=mean(valence),arousal=mean(arousal),
                  .groups='keep') 
    } else
    {
      dfCat <- dat %>% 
        group_by(composer,pieceID, participant) %>% #Collapse across measures
        summarize(arPerf=(sum(attacks)/unique(durationSec)), #Avoids mm issues with arPerf
                  pitchHeight=mean(pitchHeight),rms=mean(rms),mode=unique(mode), #Summarize for collapsing
                  label=unique(label),key=unique(key),valence=mean(valence),arousal=mean(arousal)) 
    }
    df <- dfCat
    #DICHOTIMIZE VARIABLES
    df$dichRMS <- as.character(dicho(df$rms,dich.by="median")) #Dichotomize all relevant variables
    df$dichPH <- as.character(dicho(df$pitchHeight,dich.by="median"))
    df$dichAR <- as.character(dicho(df$arPerf,dich.by="median"))
    df$dichRMS <- ifelse(df$dichRMS == "1","loud","quiet") #Describe the dichotimized notation cues
    df$dichPH <- ifelse(df$dichPH== "1","high","low")
    df$dichAR <- ifelse(df$dichAR == "1","fast","slow")
    
    ## Secondary Cat Organization
    # Does not include PitchHeight, since PH was not included in HH.Some other modules (like aRules) includes it, and PH could be incorporated here following a smiliar coding structure.
    df$catAR <- "N/A" #Creates empty columns
    df$catRMS <- "N/A"
    df$catMode <- "N/A"
    
    df <-  mutate(df, label = stringr::str_to_title(label)) #Capitalize labels
    
    #Identifies if we're replacing any labels, then swaps each label in the the given lists
    if (swapSomeLabels==T) for (lab in labelSwaps){df$label[as.numeric(lab[1])] <- as.character(lab[2])}
    
    #Setting new values to match the Horn and Huron cues.
    df$catAR[df$dichAR == "slow" & df$label=="Passionate"] <- 0
    df$catAR[df$dichAR == "fast" & df$label=="Passionate"] <- 1
    df$catRMS[df$dichRMS == "quiet" & df$label=="Passionate"] <- 0
    df$catRMS[df$dichRMS == "loud" & df$label=="Passionate"] <- 1
    df$catMode[df$mode == "Major" & df$label=="Passionate"] <- 0 ##Lots of repetitive code for checking if each participant-provided label aligns with Horn and Huron.
    df$catMode[df$mode == "minor" & df$label=="Passionate"] <- 1 
    df$catAR[df$dichAR == "slow" & df$label=="Joyful"] <- 0     ## Can probably be written much better.
    df$catAR[df$dichAR == "fast" & df$label=="Joyful"] <- 1
    df$catRMS[df$dichRMS == "quiet" & df$label=="Joyful"] <- 0
    df$catRMS[df$dichRMS == "loud" & df$label=="Joyful"] <- 1
    df$catMode[df$mode == "minor" & df$label=="Joyful"] <- 0
    df$catMode[df$mode == "Major" & df$label=="Joyful"] <- 1
    df$catAR[df$dichAR == "slow" & df$label=="Light/Effervescent"] <- 0
    df$catAR[df$dichAR == "fast" & df$label=="Light/Effervescent"] <- 1
    df$catRMS[df$dichRMS == "loud" & df$label=="Light/Effervescent"] <- 0
    df$catRMS[df$dichRMS == "quiet" & df$label=="Light/Effervescent"] <- 1
    df$catMode[df$mode == "minor" & df$label=="Light/Effervescent"] <- 0
    df$catMode[df$mode == "Major" & df$label=="Light/Effervescent"] <- 1
    df$catAR[df$dichAR == "fast" & df$label=="Tender/Lyrical"] <- 0
    df$catAR[df$dichAR == "slow" & df$label=="Tender/Lyrical"] <- 1
    df$catRMS[df$dichRMS == "loud" & df$label=="Tender/Lyrical"] <- 0
    df$catRMS[df$dichRMS == "quiet" & df$label=="Tender/Lyrical"] <- 1
    df$catMode[df$mode == "minor" & df$label=="Tender/Lyrical"] <- 0
    df$catMode[df$mode == "Major" & df$label=="Tender/Lyrical"] <- 1
    df$catAR[df$dichAR == "fast" & df$label=="Sad/Relaxed"] <- 0
    df$catAR[df$dichAR == "slow" & df$label=="Sad/Relaxed"] <- 1
    df$catRMS[df$dichRMS == "loud" & df$label=="Sad/Relaxed"] <- 0
    df$catRMS[df$dichRMS == "quiet" & df$label=="Sad/Relaxed"] <- 1
    df$catMode[df$mode == "Major" & df$label=="Sad/Relaxed"] <- 0
    df$catMode[df$mode == "minor" & df$label=="Sad/Relaxed"] <- 1
    
    ## Summing
    df$catAR <- as.numeric(df$catAR) #Turns columns into numerics for later aggregation.
    df$catRMS <- as.numeric(df$catRMS)
    df$catMode <- as.numeric(df$catMode)
  
    df$catSum <- apply(df[,c("catAR","catRMS","catMode")],1,sum) #Selects different columns to sum for a total
    df$catSum <- as.factor(df$catSum)
    df$catSum<- array(with(df, paste0(substring(catSum, 1, 1), substring(label, 1, 1)))) #Add to cat sum to associate the variable with the label for easier plotting. 
    
    #df$AR <- scale(df$AR) #Can normalize AR if we want in the future.
    
    #Create a seperate dataframe for the byPiece plot which does odd things with columns
    dfByP <- df
    dfByP$pieceID <- dfByP$key
    if (!byPiece) #If the participant column exists then...
    {
      dat <- df %>% count(label,participant,catSum) #Assigned each participant instance with a 1
      dat <- dat[,-c(1)]
      if (!proportion){
        dat <- dat %>% group_by(label,catSum) %>% summarize(n=sum(n)) # Summarize by catSum to create a total count of each catSum variable.
        plot <- ggplot(data = dat, 
                       aes(x=fct_relevel(label, levels), #Reorder labels
                           y=n, fill=catSum)) + #Inputting data and fills.
          geom_bar(stat="identity",color="black",size=1) + #Create the outline
          ggtitle(composer) + #Title the plot
          ylab("Count") + #Set y title
          scale_x_discrete(labels= xlab)+ #Define x labels
          theme(panel.background=element_rect(fill="White"),
                panel.grid.major=element_line(linetype="blank"),
                plot.title=element_text(hjust=0.5,face="bold",size=20), #Clean plotting parameters (everything in theme)
                axis.title.x = element_blank(),
                legend.position="none",
                axis.title.y=element_text(size=textSize),
                axis.text.x=element_text(size=textSize), #Custom or standard textSize
                axis.text.y=element_text(size=textSize))+
          scale_fill_manual(values=stacCol)+ #Assign colours based on the variable stacCol (created outside of the function)
          if (!missing(ylim)) ylim(ylim) #Setting ylim if not set as parameter
        print(plot)
      } else{
        dat <- df %>% group_by(label,catSum) #%>% summarize(n=sum(n)) # Summarize by catSum to create a total count of each catSum variable.
        dat$label <- as.factor(dat$label)
        dat$catSum <- as.factor(dat$catSum)
        plot <- ggplot(dat, 
                       aes(x=fct_relevel(label, levels), fill=catSum)) + #Inputting data and fills.
          geom_bar(position="fill",color="black",size=1)+#stat="identity",color="black",size=1) + #Create the outline
          ggtitle(composer) + #Title the plot with composer name 
          #xlab("Label") +
          ylab("Count") + #Set y title
          scale_x_discrete(labels= xlab)+ #Define x labels
          theme(panel.background=element_rect(fill="White"),
                panel.grid.major=element_line(linetype="blank"),
                plot.title=element_text(hjust=0.5,face="bold",size=20), #Clean plotting parameters (everything in theme)
                axis.title.x = element_blank(),
                legend.position="none",
                axis.title.y=element_text(size=textSize),
                axis.text.x=element_text(size=textSize), #Custom or standard textSize
                axis.text.y=element_text(size=textSize))+
          scale_fill_manual(values=stacCol)
        print(plot)
      }
    } else {
      
      dat <- dfByP %>% count(label, catSum) #Assign simpler values for each row
      dat <- dat[,-c(1)]
      dat$colour <- ifelse(startsWith(dat$catSum, "3"),"white","black") #Determine text colour for plot text based on the colour gradient
      plot <- ggplot(data = dat, #Drawing in data and fills for the plot.
                     aes(x=fct_relevel(label,levels), #Reorder participant labels
                         y=n, fill=catSum)) + 
        geom_bar(stat = "identity",colour="black")+ #Creates outlines between bars
        ggtitle(composer) + #Title the plot with composer name 
        geom_text(aes(label=dat$pieceID), position=position_stack(vjust=1.0),vjust=2,size=5,color=dat$colour)+ #Create label for each bar corresponding to the piece key. Capitalization denotes mode (MAJOR, minor)
        ylab("Count") + #y label
        #ylim(y)+ #Defines ylim in the plot
        scale_x_discrete(labels= xlab)+ #Define x labels
        theme(panel.background=element_rect(fill="White"),
              panel.grid.major=element_line(linetype="blank"),
              plot.title=element_text(hjust=0.5,face="bold",size=20), #Clean plot lines (everything in theme)
              axis.title.x = element_blank(),
              axis.title.y=element_text(size=textSize),
              axis.text.x=element_text(size=textSize), #Custom or standard textsSizes
              axis.text.y=element_text(size=textSize))+
        scale_fill_manual(values=stacCol)+ #CatSum colours, as fill is denoted as catSum in line 155
        if (!missing(ylim)) ylim(ylim) #Setting ylim if not set as parameter
      print(plot)
    }
    if (verbose){return(df)}else{invisible(df)} #Optionally return the data to console or not
  }
}
