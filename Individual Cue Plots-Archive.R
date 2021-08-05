#Explanation for category selection in Stacked Categorization Plot.R module

#Works for all cues, even if they are not included in the Horn and Huron 2015 paper
#Provides violin plots for extracted cues (AR,PH,RMS) and a barplot for Mode

#Double checking that these packages are loaded, loads if not. 
requireOrInstall(ggrepel)
requireOrInstall(ggstance)
requireOrInstall(ggpubr)
requireOrInstall(sjmisc)

#cue is which of the musical cues you want to visualize ("AR","RMS","PH","Mode")
#yLimit is for specifc y-axis limits c(#,#) 
#textSize is a numeric for establishing label sizes. 

cuePlot <- function(dat,cue,boxplot=F,ylim,textSize=15, verbose=F,
                    swapSomeLabels=F,labelSwaps=list(list(indexNum,label),list(indexNum,label)), #Allows for the easiest swapping of labels within the function for mode ties. 
                    col=c("Sad/Relaxed"="dodgerblue1","Passionate"="deeppink","Tender/Lyrical"="limegreen","Light/Effervescent"="goldenrod","Joyful"="orangered1"), #Order of labels in plot
                    levels=c("Light/Effervescent","Joyful","Tender/Lyrical","Passionate","Sad/Relaxed"),#Order of labels in plot
                    xlab=c("Light\nEffervescent","Joyful","Tender\nLyrical","Passionate","Sad\nRelaxed")) #Formatting the labels themselves
{ 
  ts <- textSize
  
  #Make labels title case
  if (as.character(dat$label[1]) %notin% c("light/effervescent","joyful","tender/lyrical","passionate","sad/relaxed"))
  {
    print("Labels absent or invalid...NO PLOT FOR YOU")
  }else{
    composer <- as.character(unique(dat$composer))#Assign composer for title
    composer <- str_to_title(composer) #Capitalize for the titles
    
    dat$rms <- scale(dat$rms) #Normalizes RMS within composers to better compare between, as the actual values have no meaning across performances
    
    dat <- dat %>% 
      group_by(composer,pieceID) %>% #Collapse across measures. Just composer and pieceID as only one exp from each composer exist here. can include other exp in the future by adding expID here.
      summarize(arPerf=((sum(attacks)/length(unique(participant)))/unique(durationSec)),#Attacks divided by participants to avoid summing attacks across all participants. Can be improved.
                pitchHeight=mean(pitchHeight),rms=unique(rms), #Performing summary calculations
                mode=unique(mode),LE=length(label[label=="light/effervescent"]),J=length(label[label=="joyful"]),
                TL=length(label[label=="tender/lyrical"]),P=length(label[label=="passionate"]),SR=length(label[label=="sad/relaxed"]),
                label=popularLabel(label),key=unique(key),arScore=mean(arScore),#((arScore*mmWt)/mmWtTotal),
                .groups='keep') 
    
    dat <-  mutate(dat, label = stringr::str_to_title(label)) #Capitalize labels
    
    #Identifies if we're replacing any labels, then swaps each label in the the given lists
    if (swapSomeLabels==T) for (lab in labelSwaps){dat$label[as.numeric(lab[1])] <- as.character(lab[2])}
    
    
    if (cue=="mode")
    {
      dat <- dat %>% count(label, mode,key) #Create a value for the number of pieces in each mode
      dat$n <- with(dat,ifelse(mode=="minor",-n,n))#Set Major for positive values and minor for negative. 
      
      plot <- ggplot(dat, #Input data, relevel labels, and set fills.
                     aes(x  = fct_relevel(label,levels), 
                         y = n, fill = label)) + 
        #geom_bar(stat = "identity", position = "identity",show.legend = F) + #Define as barplot
        geom_bar(stat = "identity",colour="black")+ #Type of plot and bar outline
        geom_hline(yintercept=0)+ #Set mid line
        scale_x_discrete(labels= xlab)+ #Set x lables
        theme(panel.background=element_rect(fill="White"),
              panel.grid.major=element_line(linetype="blank"), #Standard ggPlot theme settings for the fewest lines possible
              axis.title.y=element_text(size=15),
              axis.title.x = element_blank(),
              legend.position = "none",
              plot.title=element_text(hjust=0.5,face="bold",size=20), #Clean plot lines
              axis.text.x=element_text(size=ts),#Setting the custom or standard textSize
              axis.text.y=element_text(size=ts))+
        ggtitle(composer) + #Title
        #ylim(y)+ #Lims, may cause error if you have unbalance set, but should only be Debussy
        ylab("Minor             Mode             Major")+ #Creating the yLabel for mode
        scale_fill_manual(values = col)+ #Assigning colours
        if(!missing(ylim)) ylim(ylim) #Add ylim if it has been set
      print(plot)
    } else 
    {
      #Assign particular y labels for each input cue, "Other" for future potential plots
      yLab <- switch(cue,
                     "rms" = "Scaled RMS",
                     "arPerf" = "Performance Attack Rate",
                     "pitchHeight" = "Pitch Height",
                     "arScore" = "Score Attack Rate",
                     "Other")
      if(!missing(ylim)) #Defined here so boxplot can be applied to all violin plots as well 
        #(could not figure out how to add 2 if statements at the end that didn't depend on eachother)
      {
        plot <- ggplot(dat, 
                       aes(x=fct_relevel(label, levels), #Order labels in x label
                           y=dat[[cue]],fill=label)) + 
          geom_violin(trim=FALSE)+ #Sets Violin plot
          geom_point(fill="black", size=0.5)+ #Adds each piece and a point on the violin plot
          stat_summary(fun=median, geom="crossbar", size=0.15, color="black")+ #Adds median bar for each plot
          ggtitle(composer) + #Set title as composer name
          ylim(ylim)+ #Set arguments for y limits
          ylab(yLab)+ #Set arguments for y labels
          scale_x_discrete(labels= xlab)+ #Set labels for x axis
          theme(panel.background=element_rect(fill="White"),
                panel.grid.major=element_line(linetype="blank"), #Standard clean ggPlot stuf
                axis.title.y=element_text(size=15),
                axis.title.x = element_blank(),
                legend.position = "none",
                plot.title=element_text(hjust=0.5,face="bold",size=20), #Clean plot lines
                axis.text.x=element_text(size=ts),
                axis.text.y=element_text(size=ts))+ #Setting custom or standard text Sizes
          scale_fill_manual(values=col,aes())+ #Input violin colours
          if(boxplot==T)geom_boxplot(width=0.2, alpha = 0.7) #Adds the optional boxplot
        print(plot)
      } else{
        plot <- ggplot(dat, 
                       aes(x=fct_relevel(label, levels), #Order labels in x label
                           y=dat[[cue]],fill=label)) + 
          geom_violin(trim=FALSE)+ #Sets Violin plot
          geom_point(fill="black", size=0.5)+ #Adds each piece and a point on the violin plot
          stat_summary(fun=median, geom="crossbar", size=0.15, color="black")+ #Adds median bar for each plot
          ggtitle(composer) + #Set title as composer name
          #ylim(y)+ #Set arguments from # 
          ylab(yLab)+ #Set arguments for y labels
          scale_x_discrete(labels= xlab)+ #Set labels for x axis
          theme(panel.background=element_rect(fill="White"),
                panel.grid.major=element_line(linetype="blank"), #Standard clean ggPlot stuf
                axis.title.y=element_text(size=15),
                axis.title.x = element_blank(),
                legend.position = "none",
                plot.title=element_text(hjust=0.5,face="bold",size=20), #Clean plot lines
                axis.text.x=element_text(size=ts),
                axis.text.y=element_text(size=ts))+ #Setting custom or standard text Sizes
          scale_fill_manual(values=col,aes())+ #Adding colours to the violins 
          if(boxplot==T)geom_boxplot(width=0.2, alpha = 0.7) #Creating optional boxplots
        print(plot)
      }
    }
    if (verbose){return(dat)}else{invisible(dat)} #Optionally return the data to console or not
  }}
