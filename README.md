# MAPLE Lab Project
Here is an archive of my final data pipelining and visualization code from working under the Emotional Piano Project. The project itself is spread throughout GoogleDrive and DropBox so this repo just highlights some of my parts in the project!

I followed the labs coding standards and wrote comments in a way that incoming students could understand what they were running. I have modified the code to allow for anyone to now download and (hopefully) have no issues creating some of our visualizations! Since many of our findings are yet to be published I have only made one small set of the data availible (Bach Preludes). The pipeline functions to connect many different sets, but all the showcased visualizations were designed to compare between composer sets.

**Individual Cue Plots** highlight the structural differences between participant-chosen categories. There are many setting you can tweak that I have described in the comments, and it is currently set to create a "Mode" plot but all four cues of interest can be run with the bottom four lines of code. For example this is the "Pitch Height" (how high or low the notes average) plot from the data: 

<img width="563" alt="Screen Shot 2022-01-13 at 1 12 05 PM" src="https://user-images.githubusercontent.com/80587489/149385786-0e2fd367-bcf5-4091-bcfb-4595f45c09aa.png">

**Stacked Categorization Plots** showcase the frequencies of each category that participants used, and byPiece=T shows the data aggregated by each of the 24 pieces, using the modal category for each piece (any ties will be mentioned in the errors). Since the meaning of the modal piece is something we have struggled to define, byPiece=F showcases all category freqiencies across all pieces. Here is an example of what that looks like: 

<img width="563" alt="Screen Shot 2022-01-13 at 1 15 24 PM" src="https://user-images.githubusercontent.com/80587489/149386257-5cf9f821-aee5-4fed-8d4c-504f30906352.png">

Both of these visualizations were highlighted in this open access paper in Auditory Perception & Cognition: https://www.tandfonline.com/doi/full/10.1080/25742442.2021.1988422

**Association Rules** finally explores some of the first machine learning applications to the project. There are many parameters that I describe in the code, but you should find a plot similar to this: 

<img width="523" alt="Screen Shot 2022-01-13 at 1 19 54 PM" src="https://user-images.githubusercontent.com/80587489/149386946-dba40410-1bc6-46a8-98a9-ad3253e2bca3.png">


I presented some Association Rules Analysis at the 2021 Future Directions of Music Cognition: https://osf.io/38dky/


The mergeFunctions code is just a highlight of all the work/code that went in to cleaning and consolidating years of data while staying robust to incoming data and updates. It will not run alone but acts as a highlight of a lot of my work managing the massive data pipeline.


Please keep in mind that this is a very small subset of a project spanning dosens of composers and performers so if you're interested you can follow the projects publications here: https://maplelab.net/papers-2/ 

Don't hesitate to reach out with any problems you have with the code or any questions!
