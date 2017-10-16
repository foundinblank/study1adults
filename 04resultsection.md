Results Section (study1adults)
================
Adam Stone, PhD
10-16-2017

-   [Refreshing Ourselves](#refreshing-ourselves)
-   [Participant Characteristics](#participant-characteristics)
-   [Lexical Recall Data, Summarized.](#lexical-recall-data-summarized.)
-   [Eye Gaze Data, Summarized.](#eye-gaze-data-summarized.)
-   [Bivariate Correlations](#bivariate-correlations)
    -   [Behavioral Correlations](#behavioral-correlations)
    -   [Forward Correlations](#forward-correlations)
    -   [Reversed Correlations](#reversed-correlations)
    -   [Summary](#summary)
-   [ANOVAs](#anovas)
    -   [All-ANOVA](#all-anova)
    -   [Forward ANOVA](#forward-anova)
    -   [Reversed ANOVA](#reversed-anova)
    -   [Summary of ANOVAs](#summary-of-anovas)
-   [Recovery](#recovery)
-   [Other Notes from Rain](#other-notes-from-rain)
-   [Deaf Hearing Analyses](#deaf-hearing-analyses)

Refreshing Ourselves
====================

> The main goal of the study is to see if comprehension is related to gaze behavior and if AoA impacts gaze behavior.

**Key Questions:** Can gaze behavior reflect whether or not a person can understand the story? When a person has a harder time understanding a difficult story, maybe because they lack the skills, can this be observed in gaze behavior?

> It is more important to show that our behavioral measure IS sensitive to effects of AoA. Period. *The cause is secondary.*

Now let's load in the dataset called `cleanpercentdata.csv`. It contains item-level data showing the percent of looking for each AOI (10 AOIs total) for each story and each participant *still in the study*. All data defined as "bad" (entire participants with poor calibration, or individual stories with too little looking data) have been dropped. We will then move this to subject-level by averaging each participant's two forward stories together and two reversed stories together.

``` r
#library(corrplot)
#library(Hmisc)
library(tidyverse)
library(stringr)
library(lme4)
library(lmerTest)
library(prettydoc)
library(broom)
library(knitr)
library(xtable)
library(kableExtra)
library(viridis)
options(knitr.table.format = "html") 

# Import!
data <- read_csv('cleanpercentdata.csv',col_types = 
                   cols(
                     id = col_integer(),
                     participant = col_character(),
                     hearing = col_character(),
                     videogroup = col_character(),
                     aoagroup = col_character(),
                     languagegroup = col_character(),
                     maingroup = col_character(),
                     video = col_character(),
                     story = col_character(),
                     direction = col_character(),
                     age = col_double(),
                     selfrate = col_double(),
                     signyrs = col_double(),
                     aoasl = col_integer(),
                     acc = col_double(),
                     aoi = col_character(),
                     percent = col_double()
                   ))

# And factorize
data <- data %>%
  mutate(participant = as.factor(participant)) %>%
  mutate(id = as.factor(id)) %>%
  mutate(hearing = as.factor(hearing)) %>%
  mutate(videogroup = as.factor(videogroup)) %>%
  mutate(aoagroup = as.factor(aoagroup)) %>%
  mutate(languagegroup = as.factor(languagegroup)) %>%
  mutate(maingroup = as.factor(maingroup)) %>%
  mutate(video = as.factor(video)) %>%
  mutate(story = as.factor(story)) %>%
  mutate(direction = as.factor(direction)) %>%
  mutate(aoi = as.factor(aoi))

dataoriginal <- data # Save item-level data just in case

# Take out HearingNoviceASL
# data <- data %>%
#   filter(maingroup!="HearingNoviceASL")

# Load awesome function to make correlation tables with stars for significance
# From: https://myowelt.blogspot.co.uk/2008/04/beautiful-correlation-tables-in-r.html
corstarsl <- function(x){ 
require(Hmisc) 
x <- as.matrix(x) 
R <- Hmisc::rcorr(x)$r 
p <- Hmisc::rcorr(x)$P 
## define notions for significance levels; spacing is important.
mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
## trunctuate the matrix that holds the correlations to two decimal
R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
## build a new matrix that includes the correlations with their apropriate stars 
Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
diag(Rnew) <- paste(diag(R), " ", sep="") 
rownames(Rnew) <- colnames(x) 
colnames(Rnew) <- paste(colnames(x), "", sep="") 
## remove upper triangle
Rnew <- as.matrix(Rnew)
Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
Rnew <- as.data.frame(Rnew) 
## remove last column and return the matrix (which is now a data frame)
Rnew <- cbind(Rnew[1:length(Rnew)-1])
return(Rnew) 
}

# Pull out subject info, and averge the accuracy scores
data.subjectinfo <- data %>%
  select(-aoi,-percent,-video,-story) %>%
  distinct() %>%
  group_by(participant,direction) %>%
  mutate(acc = mean(acc,na.rm=TRUE)) %>%
  distinct()

# Now collapse eye gaze data to subject-level 
data <- data %>%
  group_by(participant,direction,aoi) %>%
  dplyr::summarize(percent = mean(percent,na.rm=TRUE))
data[data=="NaN"] <- NA

# Join subject info with data that's now subject-level
data <- left_join(data,data.subjectinfo, by=c("participant","direction"))

# Set reference levels
data$maingroup <- relevel(data$maingroup, ref="DeafNative")
# data.face3 <- filter(data2, aoi == "eyes" | aoi == "mouth" | aoi == "chin") %>%
#   mutate(aoi = as.factor(aoi))
# data.face3$aoi <- factor(data.face3$aoi, levels=c("eyes","mouth","chin"))
# data.face3$aoi <- relevel(data.face3$aoi, ref="eyes")

# But now we need to go back and add in a complete lexical recall dataset, even including those trials that got thrown out in 03eyegaze.nb.html. Because the lexical accuracy data is still good. So let's work on that. 
cleanlexdata <- read_csv('cleandata.csv',col_types = 
                   cols(
                     id = col_integer(),
                     participant = col_character(),
                     hearing = col_character(),
                     videogroup = col_character(),
                     aoagroup = col_character(),
                     languagegroup = col_character(),
                     maingroup = col_character(),
                     video = col_character(),
                     story = col_character(),
                     direction = col_character(),
                     age = col_double(),
                     selfrate = col_double(),
                     signyrs = col_double(),
                     aoasl = col_integer(),
                     acc = col_double(),
                     aoi = col_character(),
                     percent = col_double()
                   ))

cleanlexdata <- cleanlexdata %>%
  select(participant,direction,acc) %>%
  group_by(participant,direction) %>%
  dplyr::summarize(acc = mean(acc,na.rm=TRUE)) %>%
  ungroup()

data <- data %>%
  select(-acc) %>%
  left_join(cleanlexdata,by=c("participant","direction")) %>%
  ungroup() %>%
  mutate(direction = as.factor(direction),
         participant = as.factor(participant))
```

Participant Characteristics
===========================

Let's do the basic participant table here.

``` r
groupmeans <- data %>%
  ungroup() %>%
  select(id,participant,maingroup,age,selfrate,signyrs,aoasl) %>%
  distinct() %>%
  group_by(maingroup) %>%
  dplyr::summarize(n = n(),
            age.m = mean(age),
            age.sd = sd(age),
            selfrate.m = mean(selfrate),
            selfrate.sd = sd(selfrate),
            signyrs.m = mean(signyrs),
            signyrs.sd = sd(signyrs),
            aoasl.m = mean(aoasl),
            aoasl.sd = sd(aoasl))
kable(groupmeans, digits=1) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
maingroup
</th>
<th style="text-align:right;">
n
</th>
<th style="text-align:right;">
age.m
</th>
<th style="text-align:right;">
age.sd
</th>
<th style="text-align:right;">
selfrate.m
</th>
<th style="text-align:right;">
selfrate.sd
</th>
<th style="text-align:right;">
signyrs.m
</th>
<th style="text-align:right;">
signyrs.sd
</th>
<th style="text-align:right;">
aoasl.m
</th>
<th style="text-align:right;">
aoasl.sd
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
DeafNative
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
33.0
</td>
<td style="text-align:right;">
9.2
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
32.6
</td>
<td style="text-align:right;">
9.4
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:right;">
0.5
</td>
</tr>
<tr>
<td style="text-align:left;">
DeafEarlyASL
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
34.8
</td>
<td style="text-align:right;">
6.6
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
29.8
</td>
<td style="text-align:right;">
7.6
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
2.7
</td>
</tr>
<tr>
<td style="text-align:left;">
DeafLateASL
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
38.0
</td>
<td style="text-align:right;">
5.9
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
23.2
</td>
<td style="text-align:right;">
5.3
</td>
<td style="text-align:right;">
14.2
</td>
<td style="text-align:right;">
3.0
</td>
</tr>
<tr>
<td style="text-align:left;">
HearingLateASL
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
28.9
</td>
<td style="text-align:right;">
6.2
</td>
<td style="text-align:right;">
4.6
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
11.8
</td>
<td style="text-align:right;">
4.8
</td>
<td style="text-align:right;">
17.2
</td>
<td style="text-align:right;">
3.4
</td>
</tr>
<tr>
<td style="text-align:left;">
HearingNoviceASL
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
20.2
</td>
<td style="text-align:right;">
1.3
</td>
<td style="text-align:right;">
3.0
</td>
<td style="text-align:right;">
0.7
</td>
<td style="text-align:right;">
2.4
</td>
<td style="text-align:right;">
1.0
</td>
<td style="text-align:right;">
17.6
</td>
<td style="text-align:right;">
1.8
</td>
</tr>
</tbody>
</table>
``` r
#groupmeans
```

Lexical Recall Data, Summarized.
================================

Quick summary of lexical recall data here. We have to collapse here as well from subject-level to group-level.

``` r
data.groupacc <- data %>%
  ungroup() %>%
  select(-aoi,-percent) %>%
  distinct() %>%
  group_by(maingroup,direction) %>%
  dplyr::summarize(mean = mean(acc, na.rm=TRUE),
            sd = sd(acc, na.rm=TRUE))
data.acc1 <- data.groupacc %>%
  select(-mean) %>%
  spread(direction,sd) %>%
  rename(forward.sd = forward,
         reversed.sd = reversed)
data.acc2 <- data.groupacc %>%
  select(-sd) %>%
  spread(direction,mean) %>%
  rename(forward.mean = forward,
         reversed.mean = reversed)
groupcount <- select(groupmeans,maingroup,n)
data.groupacc <- left_join(data.acc2,data.acc1, by="maingroup") %>%
  left_join(groupcount, by="maingroup") %>%
  # mutate(forward.se = forward.sd/sqrt(n),
  #        reversed.se = reversed.sd/sqrt(n)) %>%
  #select(maingroup,n,forward.mean,forward.sd,forward.se,reversed.mean,reversed.sd,reversed.se)
  select(maingroup,n,forward.mean,forward.sd,reversed.mean,reversed.sd)
#data.acc
kable(data.groupacc, digits=2) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
maingroup
</th>
<th style="text-align:right;">
n
</th>
<th style="text-align:right;">
forward.mean
</th>
<th style="text-align:right;">
forward.sd
</th>
<th style="text-align:right;">
reversed.mean
</th>
<th style="text-align:right;">
reversed.sd
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
DeafNative
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.10
</td>
</tr>
<tr>
<td style="text-align:left;">
DeafEarlyASL
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.66
</td>
<td style="text-align:right;">
0.09
</td>
</tr>
<tr>
<td style="text-align:left;">
DeafLateASL
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.06
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left;">
HearingLateASL
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.09
</td>
</tr>
<tr>
<td style="text-align:left;">
HearingNoviceASL
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.11
</td>
</tr>
</tbody>
</table>
And a boxplot that can go with it.

``` r
ggplot(data,aes(maingroup,acc,fill=direction)) + 
  geom_boxplot() +
  scale_y_continuous(limits=c(0,1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

![](04resultsection_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png) And Rain wanted the error plot so here it is. Lines represent SEM. Good to have this too!

``` r
data.acc1 <- data.acc1 %>% 
  ungroup() %>% 
  gather(direction,sd,forward.sd:reversed.sd) %>%
  mutate(direction = str_sub(direction,1,-4))
data.acc2 <- data.acc2 %>% 
  ungroup() %>% 
  gather(direction,mean,forward.mean:reversed.mean) %>%
  mutate(direction = str_sub(direction,1,-6))
data.acc.se <- left_join(data.acc1,data.acc2, by=c("maingroup","direction")) %>%
  left_join(groupcount, by="maingroup") %>%
  mutate(se = sd/sqrt(n))
ggplot(data.acc.se,aes(maingroup,mean,color=direction)) +
  geom_point(position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.25,size=1,position=position_dodge(0.5)) +
  scale_y_continuous(limits=c(0,1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + xlab("") + ylab("mean accuracy")
```

![](04resultsection_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

Eye Gaze Data, Summarized.
==========================

I guess we should have a big table of means and SDs for all AOIs for each group for forward and backward. I'll write the code for this later. For now, here's the boxplot. This is the part where we will say, from all the data we have concluded that we can work with **three base AOIs** from here on due to sufficient data: chin, eyes, and mouth.

``` r
data.aoionly <- data %>%
  filter(aoi != "facechest" & aoi != "mouthchin" & aoi != "moutheyes")
ggplot(data.aoionly,aes(aoi,percent,fill=direction)) + 
  geom_boxplot() +
  scale_y_continuous(limits=c(0,1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 346 rows containing non-finite values (stat_boxplot).

![](04resultsection_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

THEN we can show heat maps too. For this I would include forehead and upperchest just to give it some dimension.

``` r
data.five <- data %>%
  ungroup() %>%
  filter(aoi == "upperchest" | aoi == "chin" | aoi == "mouth" | aoi == "eyes" | aoi == "forehead") %>%
  group_by(maingroup,aoi,direction) %>%
  dplyr::summarize(mean = mean(percent, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(aoi = factor(aoi,levels=c("upperchest","chin","mouth","eyes","forehead")))

# data.five <- data %>%
#   ungroup() %>%
#   filter(aoi == "upperchest" | aoi == "chin" | aoi == "mouth" | aoi == "eyes" | aoi == "forehead" | aoi == "upperchest" | aoi == "midchest" | aoi == "lowerchest" | aoi == "belly") %>%
#   group_by(maingroup,aoi,direction) %>%
#   dplyr::summarize(mean = mean(percent, na.rm=TRUE)) %>%
#   ungroup() %>% 
#   mutate(aoi = factor(aoi,levels=c("belly","lowerchest","midchest","upperchest","chin","mouth","eyes","forehead")))

# data.five <- data %>%
#   ungroup() %>%
#   filter(aoi == "torso" | aoi == "face") %>%
#   group_by(maingroup,aoi,direction) %>%
#   dplyr::summarize(mean = mean(percent, na.rm=TRUE)) %>%
#   ungroup()

ggplot(data.five, aes(x = maingroup, y = aoi)) +
  geom_tile(aes(fill=mean),color="lightgray",na.rm=TRUE) + 
#  scale_fill_gradient(low = "lightblue",high = "steelblue") +
#  scale_fill_distiller(type="div", palette = "RdYlBu") +
  scale_fill_viridis(option = "magma", direction=-1) +
  facet_wrap("direction") +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

![](04resultsection_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

Also of interest is how the FaceChest Index changes. And it shows us there's definitely more of a pull to the Chest during reversed stories.

``` r
data.indexonly <- data %>%
  filter(aoi == "facechest")
ggplot(data.indexonly,aes(aoi,percent,fill=direction)) + 
  geom_boxplot() +
  scale_y_continuous(limits=c(-1,1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

![](04resultsection_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

Bivariate Correlations
======================

We temporarily drop groups here. We just want to ask: *"What is correlated in terms of subject characteristics and their behavioral measures?"* We'll do this separately for forward and reversed, and using only important AOIs. We can also ask additional questions like, *which is stronger - signing years or AOA? is lexical recall correlated with looking at any AOI?*

Behavioral Correlations
-----------------------

First, we'll look at correlations between participant characteristics, including AoASL, and their performance on the lexical recall task.

-   Forward accuracy is correlated with all characteristics BUT AoASL or Signing Years. So your self-rating or age predict forward accuracy, but age of acquisition or signing years doesn't.
-   Reversed accuracy *is correlated with AoASL* and signing years and self-rating. Now, self-rating is really subjective so let's not use that. Your age doesn't predict reversed accuracy. AoASL does!
-   Reversal effect is not correlated with anything.

`*` p &lt; 0.05 `**` p &lt; 0.01 `***` p &lt; 0.001

``` r
# We're going to need to make another data frame with participant-level accuracy information, this time including a reversal effect which we've calculated for each participant. 
data.subjectinfo <- data.subjectinfo %>%
  ungroup() %>%
  select(-direction,-acc)
data.acc <- data %>%
  ungroup() %>%
  select(-aoi,-percent) %>%
  distinct() %>%
  group_by(participant,direction) %>%
  dplyr::summarize(mean = mean(acc, na.rm=TRUE)) %>%
  spread(direction,mean) %>%
  mutate(effect = forward - reversed) %>%
  rename(acc.forward = forward,
         acc.reversed = reversed,
         acc.effect = effect) %>%
  left_join(data.subjectinfo, by="participant") %>%
  ungroup() %>% 
  distinct() %>%
  select(aoasl,signyrs,selfrate,age,acc.forward,acc.reversed,acc.effect)

#data.acc.results <- Hmisc::rcorr(as.matrix(data.acc))
corstarsl(data.acc) # Use the awesome function!
```

    ##                 aoasl  signyrs selfrate      age acc.forward acc.reversed
    ## aoasl                                                                    
    ## signyrs      -0.79***                                                    
    ## selfrate     -0.51***  0.71***                                           
    ## age           -0.31*   0.83***  0.63***                                  
    ## acc.forward    -0.08    0.30*   0.46***  0.36**                          
    ## acc.reversed  -0.33*    0.31*   0.40**     0.17      0.39**              
    ## acc.effect      0.26    -0.07    -0.04     0.12      0.39**      -0.69***

``` r
# Gather the AOIs we want
data.allaoi <- data # save current dataset
data <- data %>%
 filter(aoi == "eyes" | aoi == "mouth" | aoi == "chin" | aoi == "face" | aoi == "chest" | aoi == "facechest")

# Need to put AOIs in their own columns
data.spread <- data %>%
  spread(aoi,percent) %>%
  ungroup()

# Fwd and Rev data
data.fw <- data.spread %>%
  filter(direction == "forward") %>%
  select(acc,aoasl,signyrs,selfrate,age,eyes,mouth,chin,face,chest,facechest)

data.rv <- data.spread %>%
  filter(direction == "reversed") %>%
  select(acc,aoasl,signyrs,selfrate,age,eyes,mouth,chin,face,chest,facechest)

# Correlations
# data.fw.corr <- cor(data.fw, use="pairwise.complete.obs")
# data.rv.corr <- cor(data.rv, use="pairwise.complete.obs")
# corrplot.mixed(data.rv.corr, lower ="number", upper = "circle")
```

Forward Correlations
--------------------

Here's the Pearson's correlation matrix for forward stories.

-   No eye behavior metric predicts accuracy on forward stories.
-   AoASL or Signing Years do not predict accuracy.
-   Self-rating is medium-correlated with amount of time looking at the face, and a higher face/chest ratio
-   Same for age.

So. For forward stories it's a crapshoot.

``` r
#Hmisc::rcorr(as.matrix(data.fw))
corstarsl(data.fw)
```

    ##                acc    aoasl  signyrs selfrate      age     eyes    mouth
    ## acc                                                                     
    ## aoasl       -0.08                                                       
    ## signyrs     0.30*  -0.79***                                             
    ## selfrate   0.46*** -0.51***  0.71***                                    
    ## age        0.36**   -0.31*   0.83***  0.63***                           
    ## eyes        -0.17     0.07    -0.09    -0.10    -0.07                   
    ## mouth        0.18    -0.06     0.22     0.12     0.25  -0.65***         
    ## chin         0.01    -0.12    -0.04     0.10    -0.16  -0.39**  -0.40** 
    ## face         0.12    -0.24   0.38**    0.31*   0.37**    -0.11   0.46***
    ## chest       -0.05     0.15   -0.31*   -0.30*   -0.34*    -0.11   -0.29* 
    ## facechest    0.06    -0.16    0.31*    0.30*    0.34*     0.11    0.29* 
    ##              chin     face    chest
    ## acc                                
    ## aoasl                              
    ## signyrs                            
    ## selfrate                           
    ## age                                
    ## eyes                               
    ## mouth                              
    ## chin                               
    ## face       -0.19                   
    ## chest      0.29*  -0.92***         
    ## facechest -0.28*   0.92*** -1.00***

Reversed Correlations
---------------------

Here's the Pearson's correlation matrix for reversed stories.

-   Again, no eye behavior metric predicts accuracy on reversed stories.
-   Unlike forward stories, we see an effect of AoASL on looking at the face, chest, and FaceChest ratio. Those who acquired ASL later are more likely to look at the chest more and less at the face. Nice! Sign Years too at this.
-   As before, signyears continues to be a strong correlator with looking behavior, and so does age.
-   Same for age.

So.

``` r
#Hmisc::rcorr(as.matrix(data.rv))
corstarsl(data.rv)
```

    ##                acc    aoasl  signyrs selfrate      age     eyes    mouth
    ## acc                                                                     
    ## aoasl      -0.33*                                                       
    ## signyrs     0.31*  -0.78***                                             
    ## selfrate   0.40**  -0.51***  0.71***                                    
    ## age          0.17   -0.31*   0.83***  0.63***                           
    ## eyes        -0.16     0.09    -0.02     0.02     0.08                   
    ## mouth        0.14    -0.09     0.25     0.14    0.29*  -0.62***         
    ## chin         0.07    -0.11    -0.10    -0.06    -0.25  -0.47*** -0.43** 
    ## face         0.27    -0.25   0.38**    0.28*   0.36**    -0.18   0.59***
    ## chest       -0.21    0.30*  -0.44**  -0.41**  -0.41**    -0.13  -0.37** 
    ## facechest    0.23   -0.30*   0.44**   0.39**   0.40**     0.12   0.38** 
    ##             chin     face    chest
    ## acc                               
    ## aoasl                             
    ## signyrs                           
    ## selfrate                          
    ## age                               
    ## eyes                              
    ## mouth                             
    ## chin                              
    ## face      -0.10                   
    ## chest      0.26  -0.79***         
    ## facechest -0.24   0.82*** -1.00***

Summary
-------

What have we learned from the bivariate correlations? AoA is correlated with accuracy on the reversed tasks, showing that our manipulation **is** sensitive to effects of AoASL. Specifically, AoASL was correlated with accuracy such that people who acquired ASL later tended to do worse on lexical recall for reversed stories. There was no such relationship found for lexical recall for forward stories.

For no AOI was looking behavior was correlated with accuracy, for either forward or reversed stories. However, AoASL was correlated with looking behavior *only* for reversed stories. Late ASL learners, when confronted with reversed stories, looked more at the chest and less at the face (a more scattered pattern).

Years of signing appeared to have an influence on the amount of time spent looking at the face area, both by itself and in contrast with the chest. People with more signing-years spend more time on the face and less on the chest. *(We can run a quick linear regression on it...it is very significant)*

``` r
ggplot(filter(data,aoi=="facechest"),aes(x=aoasl,y=percent,color=direction)) + 
  geom_point() + 
  geom_smooth(method="lm",se=FALSE) +
  ylab("Face-Chest Ratio")
```

![](04resultsection_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-1.png)

ANOVAs
======

Now we're going to do ANOVAs. We'll do an all-factors ANOVA including Direction, then also do separate Forward and Reversed ANOVAs.

> We do this separately for lexical recall and gaze data. But be prepared to compare the two sets of results side by side, in a summary like say “subject groups differed significantly for lexical recall (stat values) but not gaze data (stat values)” or whatever the result is. At least this can be done in the discussion, if not in the results section.

I like the idea of treating Native Deaf as baseline or the gold standard in comparisons.

All-ANOVA
---------

Factors: Maingroup & Direction. First ANOVA summary is with Accuracy as outcome, second ANOVA summary is FaceChest Ratio.

-   For accuracy, there are main effects of group (p = 0.01) and direction (p &lt; 0.001), and no interactions (p = 0.42).
    -   Posthocs for maingroup tell us that DeafNative and HearingNovice are significantly different (p = 0.005) but no other group-pairs are.
-   For facechest ratio, there is a main effect of group (p = 0.001), no main effect of direction (p = 0.14), and no interactions (p = 0.61).
    -   Posthocs for maingroup tell us that the main effect was driven by HearingNovice being significantly different from DeafNative (p &lt; 0.01), DeafEarly (p = 0.01), DeafLate (p = 0.01), and HearingLate (p = 0.02). No other pairs were significant. *recheck p-values*

``` r
data.aov.all <- data %>% filter(aoi=="facechest")

# Lex Recall ANOVA. Outcome: Acc. Factors: MainGroup, Direction
aov.lex.all <- aov(data=data.aov.all, acc ~ maingroup * direction)
aov.lex.all.tidy <- tidy(aov.lex.all)

# Gaze Behavior ANOVA. Outcome: FaceChest. Factors: MainGroup, Direction
aov.gaze.all <- aov(data=data.aov.all, percent ~ maingroup * direction)
aov.gaze.all.tidy <- tidy(aov.gaze.all)

# Prettify
aov.lex.all.tidy <- aov.lex.all.tidy %>%
  select(term,statistic,p.value) %>%
  mutate(statistic = round(statistic,3),
         p.value = round(p.value,3)) %>%
  rename(F.acc = statistic,
         P.acc = p.value)

aov.gaze.all.tidy <- aov.gaze.all.tidy %>%
  select(term,statistic,p.value) %>%
  mutate(statistic = round(statistic,3),
         p.value = round(p.value,3)) %>%
  rename(F.facechest = statistic,
         P.facechest = p.value)

left_join(aov.lex.all.tidy,aov.gaze.all.tidy,by="term") %>%
  print()
```

    ##                  term  F.acc P.acc F.facechest P.facechest
    ## 1           maingroup  4.672 0.002       5.700       0.000
    ## 2           direction 75.504 0.000       3.443       0.067
    ## 3 maingroup:direction  1.306 0.273       0.804       0.525
    ## 4           Residuals     NA    NA          NA          NA

``` r
#TukeyHSD(aov.lex.all,'maingroup',conf.level = 0.95) 
#TukeyHSD(aov.gaze.all,'maingroup',conf.level = 0.95) 
```

Forward ANOVA
-------------

There is no main effect of group on accuracy (p = 0.16) or facechest ratio (p = 0.17) for forward stories.

``` r
data.aov.fw <- data %>% filter(aoi=="facechest" & direction=="forward")

# Lex Recall ANOVA. Outcome: Acc. Factors: MainGroup, Direction
aov.lex.fw <- aov(data=data.aov.fw, acc ~ maingroup)
aov.lex.fw.tidy <- tidy(aov.lex.fw)

# Gaze Behavior ANOVA. Outcome: FaceChest. Factors: MainGroup, Direction
aov.gaze.fw <- aov(data=data.aov.fw, percent ~ maingroup)
aov.gaze.fw.tidy <- tidy(aov.gaze.fw)

# Prettify
aov.lex.fw.tidy <- aov.lex.fw.tidy %>%
  select(term,statistic,p.value) %>%
  mutate(statistic = round(statistic,3),
         p.value = round(p.value,3)) %>%
  rename(F.acc = statistic,
         P.acc = p.value)

aov.gaze.fw.tidy <- aov.gaze.fw.tidy %>%
  select(term,statistic,p.value) %>%
  mutate(statistic = round(statistic,3),
         p.value = round(p.value,3)) %>%
  rename(F.facechest = statistic,
         P.facechest = p.value)

left_join(aov.lex.fw.tidy,aov.gaze.fw.tidy,by="term") %>%
  print()
```

    ##        term F.acc P.acc F.facechest P.facechest
    ## 1 maingroup 2.478 0.057        2.06       0.101
    ## 2 Residuals    NA    NA          NA          NA

``` r
#TukeyHSD(aov.lex.fw,'maingroup',conf.level = 0.95) 
#TukeyHSD(aov.gaze.fw,'maingroup',conf.level = 0.95) 
```

Reversed ANOVA
--------------

There are main effects of group on accuracy (p = 0.04) and on facechest ratio (p = 0.01). Posthocs tell us the effect is driven by a significant difference between DeafNative and HearingNovice (p = 0.01).

``` r
data.aov.rv <- data %>% filter(aoi=="facechest" & direction=="reversed")

# Lex Recall ANOVA. Outcome: Acc. Factors: MainGroup, Direction
aov.lex.rv <- aov(data=data.aov.rv, acc ~ maingroup)
aov.lex.rv.tidy <- tidy(aov.lex.rv)

# Gaze Behavior ANOVA. Outcome: FaceChest. Factors: MainGroup, Direction
aov.gaze.rv <- aov(data=data.aov.rv, percent ~ maingroup)
aov.gaze.rv.tidy <- tidy(aov.gaze.rv)

# Prettify
aov.lex.rv.tidy <- aov.lex.rv.tidy %>%
  select(term,statistic,p.value) %>%
  mutate(statistic = round(statistic,3),
         p.value = round(p.value,3)) %>%
  rename(F.acc = statistic,
         P.acc = p.value)

aov.gaze.rv.tidy <- aov.gaze.rv.tidy %>%
  select(term,statistic,p.value) %>%
  mutate(statistic = round(statistic,3),
         p.value = round(p.value,3)) %>%
  rename(F.facechest = statistic,
         P.facechest = p.value)

left_join(aov.lex.rv.tidy,aov.gaze.rv.tidy,by="term") %>%
  print()
```

    ##        term F.acc P.acc F.facechest P.facechest
    ## 1 maingroup 3.384 0.017       3.872       0.009
    ## 2 Residuals    NA    NA          NA          NA

``` r
#TukeyHSD(aov.lex.rv,'maingroup',conf.level = 0.95) 
#TukeyHSD(aov.gaze.rv,'maingroup',conf.level = 0.95) 
```

Summary of ANOVAs
-----------------

What have the ANOVAs told us? Let's make a table here, and graphs below. We are able to find significant effects of group on both accuracy and face-chest ratio in the all-ANOVA, as well as the reversed ANOVA. However, most of our significant effects of group are driven by HearingNovice doing poorly on accuracy and looking at the hands/chest more. So...the ANOVA story is that, really, Hearing Novices don't really know ASL well, and that's why they're doing poorly. There is no *strong* AoASL story there, it's more of a L2 language learning thing?.

``` r
aov.summary <- tribble(~ANOVA, ~Accuracy, ~FaceChestRatio,
        "All-MainGroup","Sig.","Sig.",
        "All-Direction","Sig.","ns",
        "All-Interactions","ns","ns",
        "Forward-MainGroup","ns","ns",
        "Reversed-MainGroup","Sig.","Sig.")
aov.summary
```

    ## # A tibble: 5 x 3
    ##                ANOVA Accuracy FaceChestRatio
    ##                <chr>    <chr>          <chr>
    ## 1      All-MainGroup     Sig.           Sig.
    ## 2      All-Direction     Sig.             ns
    ## 3   All-Interactions       ns             ns
    ## 4  Forward-MainGroup       ns             ns
    ## 5 Reversed-MainGroup     Sig.           Sig.

``` r
data.aov.chart <- data.aov.all %>%
  select(participant,maingroup,direction,acc,percent) %>%
  rename(facechest = percent) %>%
  gather(metric,value,acc:facechest)

ggplot(data.aov.chart,aes(x=maingroup,y=value,fill=direction)) +
  geom_boxplot() + facet_wrap("metric") +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

![](04resultsection_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

Recovery
========

Throwing this in, will move it into a better place for now but it's great! It supports AoASL effects on the video manipulation. Because look at the medians of each boxplot.

``` r
accdiff <- dataoriginal %>%
  select(participant,maingroup,video,acc) %>%
  distinct() %>%
  spread(video,acc) %>%
  mutate(fw = fw3-fw1,
         rv = rv4-rv2) %>%
  select(participant,maingroup,fw,rv) %>%
  gather(direction,diff,fw:rv)

accdiff$maingroup = factor(accdiff$maingroup,levels=c("DeafNative","DeafEarlyASL","DeafLateASL","HearingLateASL","HearingNoviceASL"))

ggplot(accdiff,aes(x=maingroup,y=diff,fill=direction)) + geom_boxplot() + ylab("diff: 2nd rv story - 1st rv story")
```

    ## Warning: Removed 18 rows containing non-finite values (stat_boxplot).

![](04resultsection_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-18-1.png)

Other Notes from Rain
=====================

It would be nice to include all lexical data in an ANOVA just to confirm that reversal DOES have an effect on comprehension (it does) and confirm that AoA also has an effect on comprehension, which is to be expected, right? This just backs up what every one else has found – that the later you learn the worse you do.

So maybe think of it as first confirming that we KNOW lexical recall is impacted. That is confirmed with ANOVA. Get that out of the way.

Then, with that confirmed….. then (and only then) we are in a position to ask:\* What is the relationship between a) the range of intelligibility scores (accuracy) and gaze…. and b) between AoA and gaze. And you can even load all these factors in together to find the unique variance. \* We know who performs well and who performs poorly…. do their gaze behaviors differ?

1.  If you do LLM or ANCOVA, do so with AoA, and without subject group or hearing status. I would set aside any stats you did with both Hearing Status and AoA, I don't think you can do that.

How come lexical recall isn’t a predictor in a model with gaze data? Yes, that is an important goal of the paper.

Here is an example of what I had noted to myself previously, which is not current any more, and I would put correlation values in here:

-   Remarkably, percent-looking at mid chest and lower chest, for both forward and reversed (and left side for reversed) are highly negatively correlated with years signing and positively correlated with AoA. That means that greater looking in those areas are associated with older ages of acquisition and fewer years of experience. What is equally interesting is that looking at the eyes was not related to subject characteristics at all (contrast with Emmorey’s finding).
-   Also, looking at the mouth for reversed stimuli was significantly correlated with years signing (r = 0.38), this means the longer one signed, the more (in terms of % looking) one looked at the mouth.

Then, maybe we can have a section called “Hearing Status” and in this paragraph say what happens when we compare hearing and deaf, using the same range of AoA, excluding hearing Novice. Or maybe separate regressions for hearing and deaf groups, looking at AoA, AOI’s, and lexical recall, to examine the relationship between the three. I don't know.

Do you find that NATIVE signers pretty much look at the same place for forward and reversed but that the novice signers show greater dispersion of gaze points for reversed than forward (like more on the chest, by way of lower face looking percent)? Because I always thought that experts are already using their efficient gaze pattern, nothing throws this off…. But novice are easily “thrown off”, gaze-wise. They have very little tolerance of phonetic variation. So they might get really good at understanding their own ASL teacher, and then give them a new variant of a signer, they start to look at the signers’ hands maybe. The same thing happens for the reversed stories, maybe.

``` r
# Me trying to run models for eye behavior -> lexical accuracy
datafc <- dataoriginal
model <- lmer(acc ~ percent * signyrs + (1|id) + (1|story), data = filter(datafc,aoi=="mouth" & direction=="forward"))
summary(model)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: acc ~ percent * signyrs + (1 | id) + (1 | story)
    ##    Data: filter(datafc, aoi == "mouth" & direction == "forward")
    ## 
    ## REML criterion at convergence: -172.9
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.70047 -0.54813  0.02755  0.56490  1.57075 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.004572 0.06762 
    ##  story    (Intercept) 0.001253 0.03540 
    ##  Residual             0.003407 0.05837 
    ## Number of obs: 96, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                  Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)      0.869297   0.046222 54.510000  18.807   <2e-16 ***
    ## percent         -0.112457   0.062870 89.260000  -1.789   0.0771 .  
    ## signyrs         -0.002043   0.002048 89.010000  -0.998   0.3211    
    ## percent:signyrs  0.006208   0.002805 82.090000   2.213   0.0297 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) percnt sgnyrs
    ## percent     -0.815              
    ## signyrs     -0.754  0.707       
    ## prcnt:sgnyr  0.704 -0.830 -0.905

``` r
model <- lmer(percent ~ direction + (1|id), data = filter(datafc,aoi=="mouth"))
summary(model) 
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: percent ~ direction + (1 | id)
    ##    Data: filter(datafc, aoi == "mouth")
    ## 
    ## REML criterion at convergence: -8.5
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.60785 -0.47492  0.08233  0.61375  2.32278 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.04720  0.2173  
    ##  Residual             0.03269  0.1808  
    ## Number of obs: 193, groups:  id, 52
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)         0.63581    0.03532  67.13000  18.004  < 2e-16 ***
    ## directionreversed  -0.09134    0.02622 141.33000  -3.483 0.000661 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## dirctnrvrsd -0.363

``` r
# plot(model)
# library(ggfortify)
# autoplot(model2)
# autoplot(model2,which=1:6,data = datafc,colour='direction',label.size=3)
```

Deaf Hearing Analyses
=====================

Let's try running models with deaf/hearing. First, a scatterplot...

``` r
data.acc <- dataoriginal %>% filter(aoi=="facechest") 
ggplot(data.acc, aes(x=signyrs,y=acc,color=direction)) + geom_jitter() + geom_smooth(method="lm") + facet_grid(.~hearing)
```

    ## Warning: Removed 4 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](04resultsection_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-20-1.png)

So the slopes don't change for direction, but the seem to change for deaf/hearing. In other words, signyears doesn't have a big effect for deaf, but it does for hearing.

``` r
model <- lmer(acc ~ hearing * direction * signyrs + (1|id) + (1|story), data = data.acc)
summary(model)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: acc ~ hearing * direction * signyrs + (1 | id) + (1 | story)
    ##    Data: data.acc
    ## 
    ## REML criterion at convergence: -257.2
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.06050 -0.61191 -0.01016  0.52982  2.41102 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.002876 0.05363 
    ##  story    (Intercept) 0.001423 0.03772 
    ##  Residual             0.008566 0.09256 
    ## Number of obs: 189, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error         df
    ## (Intercept)                               7.917e-01  6.211e-02  8.122e+01
    ## hearingHearing                           -8.394e-03  6.578e-02  9.474e+01
    ## directionreversed                        -7.270e-02  6.599e-02  1.322e+02
    ## signyrs                                   2.107e-03  1.955e-03  9.649e+01
    ## hearingHearing:directionreversed         -7.468e-02  7.322e-02  1.323e+02
    ## hearingHearing:signyrs                    3.361e-03  3.736e-03  9.651e+01
    ## directionreversed:signyrs                -2.025e-03  2.176e-03  1.319e+02
    ## hearingHearing:directionreversed:signyrs  2.881e-04  4.099e-03  1.334e+02
    ##                                          t value Pr(>|t|)    
    ## (Intercept)                               12.746   <2e-16 ***
    ## hearingHearing                            -0.128    0.899    
    ## directionreversed                         -1.102    0.273    
    ## signyrs                                    1.078    0.284    
    ## hearingHearing:directionreversed          -1.020    0.310    
    ## hearingHearing:signyrs                     0.900    0.370    
    ## directionreversed:signyrs                 -0.930    0.354    
    ## hearingHearing:directionreversed:signyrs   0.070    0.944    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) hrngHr drctnr sgnyrs hrngHrng:d hrngHrng:s drctn:
    ## hearingHrng -0.857                                                  
    ## dirctnrvrsd -0.529  0.499                                           
    ## signyrs     -0.916  0.865  0.534                                    
    ## hrngHrng:dr  0.477 -0.555 -0.901 -0.481                             
    ## hrngHrng:sg  0.479 -0.742 -0.279 -0.524  0.413                      
    ## drctnrvrsd:  0.509 -0.481 -0.959 -0.557  0.865      0.291           
    ## hrngHrng:d: -0.270  0.419  0.508  0.296 -0.746     -0.573     -0.531

Eh, no effect here. How about eye gaze...

``` r
data.fc <- dataoriginal %>% filter(aoi=="facechest") 
ggplot(data.fc, aes(x=signyrs,y=percent,color=direction)) + geom_jitter() + geom_smooth(method="lm") + facet_grid(.~hearing) + ylab("FaceChest Ratio")
```

![](04resultsection_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-22-1.png)

``` r
data.mouth <- dataoriginal %>% filter(aoi=="mouth") 
ggplot(data.mouth, aes(x=signyrs,y=percent,color=direction)) + geom_jitter() + geom_smooth(method="lm") + facet_grid(.~hearing) + ylab("Mouth AOI Percent")
```

![](04resultsection_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-22-2.png)

``` r
data.eyes <- dataoriginal %>% filter(aoi=="eyes") 
ggplot(data.eyes, aes(x=signyrs,y=percent,color=direction)) + geom_jitter() + geom_smooth(method="lm") + facet_grid(.~hearing) + ylab("Eyes AOI Percent")
```

    ## Warning: Removed 17 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 17 rows containing missing values (geom_point).

![](04resultsection_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-22-3.png)

Looks like we've got different reversal effects for FaceChest Ratio, Mouth AOI, and Eyes AOI...not terribly strong, though. Let's run models.

``` r
model <- lmer(percent ~ hearing * direction * signyrs + (1|id) + (1|story), data = data.fc)
summary(model)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: percent ~ hearing * direction * signyrs + (1 | id) + (1 | story)
    ##    Data: data.fc
    ## 
    ## REML criterion at convergence: -221.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9054 -0.1062  0.0588  0.3202  1.7591 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  id       (Intercept) 0.0099310 0.09965 
    ##  story    (Intercept) 0.0002315 0.01522 
    ##  Residual             0.0089095 0.09439 
    ## Number of obs: 193, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error         df
    ## (Intercept)                                0.945368   0.081369  68.110000
    ## hearingHearing                            -0.068859   0.090593  67.210000
    ## directionreversed                         -0.043905   0.064525 135.840000
    ## signyrs                                    0.001081   0.002681  68.030000
    ## hearingHearing:directionreversed          -0.069944   0.072232 136.800000
    ## hearingHearing:signyrs                     0.005604   0.005180  70.190000
    ## directionreversed:signyrs                  0.001016   0.002142 135.930000
    ## hearingHearing:directionreversed:signyrs   0.003715   0.004144 137.530000
    ##                                          t value Pr(>|t|)    
    ## (Intercept)                               11.618   <2e-16 ***
    ## hearingHearing                            -0.760    0.450    
    ## directionreversed                         -0.680    0.497    
    ## signyrs                                    0.403    0.688    
    ## hearingHearing:directionreversed          -0.968    0.335    
    ## hearingHearing:signyrs                     1.082    0.283    
    ## directionreversed:signyrs                  0.474    0.636    
    ## hearingHearing:directionreversed:signyrs   0.897    0.372    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) hrngHr drctnr sgnyrs hrngHrng:d hrngHrng:s drctn:
    ## hearingHrng -0.891                                                  
    ## dirctnrvrsd -0.395  0.355                                           
    ## signyrs     -0.956  0.859  0.382                                    
    ## hrngHrng:dr  0.353 -0.397 -0.894 -0.342                             
    ## hrngHrng:sg  0.495 -0.744 -0.198 -0.518  0.300                      
    ## drctnrvrsd:  0.380 -0.342 -0.957 -0.399  0.857      0.207           
    ## hrngHrng:d: -0.197  0.299  0.496  0.208 -0.742     -0.419     -0.519

``` r
model <- lmer(percent ~ hearing * direction * signyrs + (1|id) + (1|story), data = data.mouth)
summary(model)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: percent ~ hearing * direction * signyrs + (1 | id) + (1 | story)
    ##    Data: data.mouth
    ## 
    ## REML criterion at convergence: 15.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6678 -0.4859  0.1212  0.5886  2.0450 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.045749 0.21389 
    ##  story    (Intercept) 0.003448 0.05872 
    ##  Residual             0.029281 0.17112 
    ## Number of obs: 193, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error         df
    ## (Intercept)                               6.028e-01  1.683e-01  6.273e+01
    ## hearingHearing                           -2.853e-02  1.854e-01  6.052e+01
    ## directionreversed                         4.764e-03  1.170e-01  1.338e+02
    ## signyrs                                   2.601e-03  5.482e-03  6.111e+01
    ## hearingHearing:directionreversed         -1.663e-01  1.311e-01  1.342e+02
    ## hearingHearing:signyrs                    3.866e-05  1.058e-02  6.278e+01
    ## directionreversed:signyrs                -2.038e-03  3.885e-03  1.338e+02
    ## hearingHearing:directionreversed:signyrs  2.651e-03  7.527e-03  1.347e+02
    ##                                          t value Pr(>|t|)    
    ## (Intercept)                                3.581 0.000669 ***
    ## hearingHearing                            -0.154 0.878231    
    ## directionreversed                          0.041 0.967583    
    ## signyrs                                    0.474 0.636852    
    ## hearingHearing:directionreversed          -1.268 0.206873    
    ## hearingHearing:signyrs                     0.004 0.997095    
    ## directionreversed:signyrs                 -0.525 0.600756    
    ## hearingHearing:directionreversed:signyrs   0.352 0.725261    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) hrngHr drctnr sgnyrs hrngHrng:d hrngHrng:s drctn:
    ## hearingHrng -0.881                                                  
    ## dirctnrvrsd -0.346  0.315                                           
    ## signyrs     -0.946  0.859  0.339                                    
    ## hrngHrng:dr  0.309 -0.353 -0.894 -0.303                             
    ## hrngHrng:sg  0.491 -0.745 -0.176 -0.519  0.267                      
    ## drctnrvrsd:  0.333 -0.303 -0.957 -0.354  0.856      0.185           
    ## hrngHrng:d: -0.173  0.266  0.495  0.184 -0.743     -0.373     -0.519

``` r
model <- lmer(percent ~ hearing * direction * signyrs + (1|id) + (1|story), data = data.eyes)
summary(model)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: percent ~ hearing * direction * signyrs + (1 | id) + (1 | story)
    ##    Data: data.eyes
    ## 
    ## REML criterion at convergence: -17.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4992 -0.4637 -0.1208  0.3070  3.7205 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.034767 0.18646 
    ##  story    (Intercept) 0.001375 0.03709 
    ##  Residual             0.023858 0.15446 
    ## Number of obs: 176, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error         df
    ## (Intercept)                               1.544e-01  1.484e-01  6.152e+01
    ## hearingHearing                            5.430e-02  1.646e-01  6.001e+01
    ## directionreversed                        -3.957e-02  1.083e-01  1.173e+02
    ## signyrs                                   9.223e-04  4.858e-03  6.016e+01
    ## hearingHearing:directionreversed         -1.261e-02  1.215e-01  1.177e+02
    ## hearingHearing:signyrs                    4.575e-03  9.369e-03  6.174e+01
    ## directionreversed:signyrs                 1.036e-03  3.574e-03  1.170e+02
    ## hearingHearing:directionreversed:signyrs  8.837e-03  7.010e-03  1.187e+02
    ##                                          t value Pr(>|t|)
    ## (Intercept)                                1.041    0.302
    ## hearingHearing                             0.330    0.743
    ## directionreversed                         -0.365    0.716
    ## signyrs                                    0.190    0.850
    ## hearingHearing:directionreversed          -0.104    0.917
    ## hearingHearing:signyrs                     0.488    0.627
    ## directionreversed:signyrs                  0.290    0.772
    ## hearingHearing:directionreversed:signyrs   1.261    0.210
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) hrngHr drctnr sgnyrs hrngHrng:d hrngHrng:s drctn:
    ## hearingHrng -0.887                                                  
    ## dirctnrvrsd -0.367  0.330                                           
    ## signyrs     -0.952  0.858  0.354                                    
    ## hrngHrng:dr  0.328 -0.371 -0.892 -0.317                             
    ## hrngHrng:sg  0.494 -0.744 -0.184 -0.519  0.278                      
    ## drctnrvrsd:  0.352 -0.318 -0.954 -0.370  0.852      0.193           
    ## hrngHrng:d: -0.180  0.276  0.486  0.190 -0.734     -0.380     -0.512

Let's ask a very simple question. Are deaf and hearing doing differently on the lexical recall test?

``` r
model <- lmer(acc ~ hearing * direction + (1|id) + (1|story), data = data.acc)
summary(model)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: acc ~ hearing * direction + (1 | id) + (1 | story)
    ##    Data: data.acc
    ## 
    ## REML criterion at convergence: -293.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.09522 -0.64935  0.02551  0.57563  2.44869 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.003066 0.05537 
    ##  story    (Intercept) 0.001417 0.03764 
    ##  Residual             0.008504 0.09221 
    ## Number of obs: 189, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                   Estimate Std. Error        df t value
    ## (Intercept)                        0.85293    0.02505   7.25000  34.044
    ## hearingHearing                    -0.03096    0.02457  95.42000  -1.260
    ## directionreversed                 -0.13166    0.01861 137.32000  -7.075
    ## hearingHearing:directionreversed  -0.02712    0.02708 136.14000  -1.002
    ##                                  Pr(>|t|)    
    ## (Intercept)                      2.86e-09 ***
    ## hearingHearing                      0.211    
    ## directionreversed                6.96e-11 ***
    ## hearingHearing:directionreversed    0.318    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) hrngHr drctnr
    ## hearingHrng -0.444              
    ## dirctnrvrsd -0.354  0.359       
    ## hrngHrng:dr  0.242 -0.544 -0.682

Interestingly they are not. Direction has a big effect here but deaf v. hearing isn't. How about eye behavior.

``` r
model <- lmer(percent ~ hearing * direction + (1|id) + (1|story), data = data.fc)
summary(model)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: percent ~ hearing * direction + (1 | id) + (1 | story)
    ##    Data: data.fc
    ## 
    ## REML criterion at convergence: -253.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1127 -0.1637  0.0656  0.4088  1.7340 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  id       (Intercept) 0.0108277 0.10406 
    ##  story    (Intercept) 0.0002018 0.01420 
    ##  Residual             0.0089273 0.09448 
    ## Number of obs: 193, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                   Estimate Std. Error        df t value
    ## (Intercept)                        0.97680    0.02436  47.87000  40.106
    ## hearingHearing                    -0.05126    0.03493  69.85000  -1.468
    ## directionreversed                 -0.01444    0.01874 140.00000  -0.771
    ## hearingHearing:directionreversed  -0.06470    0.02752 139.39000  -2.351
    ##                                  Pr(>|t|)    
    ## (Intercept)                        <2e-16 ***
    ## hearingHearing                     0.1467    
    ## directionreversed                  0.4422    
    ## hearingHearing:directionreversed   0.0201 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) hrngHr drctnr
    ## hearingHrng -0.638              
    ## dirctnrvrsd -0.365  0.253       
    ## hrngHrng:dr  0.248 -0.388 -0.678

``` r
model <- lmer(percent ~ hearing * direction + (1|id) + (1|story), data = data.mouth)
summary(model)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: percent ~ hearing * direction + (1 | id) + (1 | story)
    ##    Data: data.mouth
    ## 
    ## REML criterion at convergence: -18.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.66774 -0.47980  0.08954  0.60212  2.06463 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.043936 0.20961 
    ##  story    (Intercept) 0.003408 0.05838 
    ##  Residual             0.028918 0.17005 
    ## Number of obs: 193, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                   Estimate Std. Error        df t value
    ## (Intercept)                        0.67822    0.05401  21.13000  12.556
    ## hearingHearing                    -0.08487    0.06817  64.45000  -1.245
    ## directionreversed                 -0.05396    0.03378 137.42000  -1.597
    ## hearingHearing:directionreversed  -0.10290    0.04960 136.78000  -2.075
    ##                                  Pr(>|t|)    
    ## (Intercept)                      2.89e-11 ***
    ## hearingHearing                     0.2176    
    ## directionreversed                  0.1125    
    ## hearingHearing:directionreversed   0.0399 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) hrngHr drctnr
    ## hearingHrng -0.561              
    ## dirctnrvrsd -0.296  0.233       
    ## hrngHrng:dr  0.201 -0.359 -0.676

``` r
model <- lmer(percent ~ hearing * direction + (1|id) + (1|story), data = data.eyes)
summary(model)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: percent ~ hearing * direction + (1 | id) + (1 | story)
    ##    Data: data.eyes
    ## 
    ## REML criterion at convergence: -47.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7796 -0.4861 -0.1210  0.3115  3.7233 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.035204 0.18763 
    ##  story    (Intercept) 0.001676 0.04094 
    ##  Residual             0.023844 0.15442 
    ## Number of obs: 176, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                    Estimate Std. Error         df t value
    ## (Intercept)                        0.181117   0.046191  30.790000   3.921
    ## hearingHearing                     0.067607   0.062051  64.540000   1.090
    ## directionreversed                 -0.008796   0.032577 123.350000  -0.270
    ## hearingHearing:directionreversed   0.026508   0.047558 121.730000   0.557
    ##                                  Pr(>|t|)    
    ## (Intercept)                      0.000459 ***
    ## hearingHearing                   0.279965    
    ## directionreversed                0.787598    
    ## hearingHearing:directionreversed 0.578289    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) hrngHr drctnr
    ## hearingHrng -0.598              
    ## dirctnrvrsd -0.322  0.238       
    ## hrngHrng:dr  0.220 -0.362 -0.681

Okay, so we get interactions for FaceChest Ratio and for Mouth...hearing and reversed do "worse". That at least. Of course, I'm worried about the Novice screwing up those results...let's check. Okay I checked all the models again with HearingNovice taken out, and yep, the effects disappeared.

What if we compared DeafLate and HearingLate, using signyrs as a covariate?

``` r
data.acc.late <- data.acc %>% filter(maingroup=="HearingLateASL" | maingroup=="DeafLateASL")
model <- lmer(acc ~ hearing * direction * signyrs + (1|id) + (1|story), data = data.acc.late)
summary(model)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: acc ~ hearing * direction * signyrs + (1 | id) + (1 | story)
    ##    Data: data.acc.late
    ## 
    ## REML criterion at convergence: -73.9
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.00529 -0.56776  0.02437  0.49060  2.42451 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.002091 0.04572 
    ##  story    (Intercept) 0.002223 0.04715 
    ##  Residual             0.008697 0.09326 
    ## Number of obs: 73, groups:  id, 20; story, 4
    ## 
    ## Fixed effects:
    ##                                           Estimate Std. Error        df
    ## (Intercept)                               0.709832   0.162404 47.210000
    ## hearingHearing                            0.180327   0.173830 44.590000
    ## directionreversed                         0.055002   0.199187 47.040000
    ## signyrs                                   0.006700   0.006706 45.410000
    ## hearingHearing:directionreversed         -0.300184   0.216218 47.560000
    ## hearingHearing:signyrs                   -0.009230   0.008529 41.870000
    ## directionreversed:signyrs                -0.008825   0.008351 47.500000
    ## hearingHearing:directionreversed:signyrs  0.014511   0.010551 48.180000
    ##                                          t value Pr(>|t|)    
    ## (Intercept)                                4.371 6.76e-05 ***
    ## hearingHearing                             1.037    0.305    
    ## directionreversed                          0.276    0.784    
    ## signyrs                                    0.999    0.323    
    ## hearingHearing:directionreversed          -1.388    0.172    
    ## hearingHearing:signyrs                    -1.082    0.285    
    ## directionreversed:signyrs                 -1.057    0.296    
    ## hearingHearing:directionreversed:signyrs   1.375    0.175    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) hrngHr drctnr sgnyrs hrngHrng:d hrngHrng:s drctn:
    ## hearingHrng -0.920                                                  
    ## dirctnrvrsd -0.610  0.579                                           
    ## signyrs     -0.972  0.914  0.608                                    
    ## hrngHrng:dr  0.567 -0.615 -0.933 -0.568                             
    ## hrngHrng:sg  0.770 -0.932 -0.491 -0.794  0.576                      
    ## drctnrvrsd:  0.599 -0.571 -0.983 -0.618  0.920      0.502           
    ## hrngHrng:d: -0.481  0.576  0.794  0.499 -0.939     -0.616     -0.811

Weird! Hearing has no effect (reversal does, though). Let's look at eye data.

``` r
data.fc.late <- data.fc %>% filter(maingroup=="HearingLateASL" | maingroup=="DeafLateASL")
model <- lmer(percent ~ hearing * direction * signyrs + (1|id) + (1|story), data = data.fc.late)
summary(model)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: percent ~ hearing * direction * signyrs + (1 | id) + (1 | story)
    ##    Data: data.fc.late
    ## 
    ## REML criterion at convergence: -103.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3094 -0.1773  0.0360  0.4459  1.7308 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  id       (Intercept) 0.0039593 0.06292 
    ##  story    (Intercept) 0.0002182 0.01477 
    ##  Residual             0.0051858 0.07201 
    ## Number of obs: 75, groups:  id, 20; story, 4
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error         df
    ## (Intercept)                               9.799e-01  1.390e-01  2.473e+01
    ## hearingHearing                           -1.965e-02  1.533e-01  2.479e+01
    ## directionreversed                        -4.865e-03  1.260e-01  5.140e+01
    ## signyrs                                   4.074e-04  5.879e-03  2.514e+01
    ## hearingHearing:directionreversed         -6.917e-02  1.408e-01  5.126e+01
    ## hearingHearing:signyrs                    2.119e-05  7.836e-03  2.561e+01
    ## directionreversed:signyrs                -5.631e-04  5.396e-03  5.107e+01
    ## hearingHearing:directionreversed:signyrs  2.279e-03  7.272e-03  5.119e+01
    ##                                          t value Pr(>|t|)    
    ## (Intercept)                                7.048 2.32e-07 ***
    ## hearingHearing                            -0.128    0.899    
    ## directionreversed                         -0.039    0.969    
    ## signyrs                                    0.069    0.945    
    ## hearingHearing:directionreversed          -0.491    0.625    
    ## hearingHearing:signyrs                     0.003    0.998    
    ## directionreversed:signyrs                 -0.104    0.917    
    ## hearingHearing:directionreversed:signyrs   0.313    0.755    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) hrngHr drctnr sgnyrs hrngHrng:d hrngHrng:s drctn:
    ## hearingHrng -0.907                                                  
    ## dirctnrvrsd -0.453  0.416                                           
    ## signyrs     -0.976  0.888  0.449                                    
    ## hrngHrng:dr  0.410 -0.455 -0.905 -0.407                             
    ## hrngHrng:sg  0.736 -0.925 -0.343 -0.754  0.427                      
    ## drctnrvrsd:  0.443 -0.408 -0.978 -0.459  0.887      0.352           
    ## hrngHrng:d: -0.334  0.422  0.738  0.347 -0.928     -0.464     -0.757

``` r
data.mouth.late <- data.fc %>% filter(maingroup=="HearingLateASL" | maingroup=="DeafLateASL")
model <- lmer(percent ~ hearing * direction * signyrs + (1|id) + (1|story), data = data.mouth.late)
summary(model)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: percent ~ hearing * direction * signyrs + (1 | id) + (1 | story)
    ##    Data: data.mouth.late
    ## 
    ## REML criterion at convergence: -103.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3094 -0.1773  0.0360  0.4459  1.7308 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  id       (Intercept) 0.0039593 0.06292 
    ##  story    (Intercept) 0.0002182 0.01477 
    ##  Residual             0.0051858 0.07201 
    ## Number of obs: 75, groups:  id, 20; story, 4
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error         df
    ## (Intercept)                               9.799e-01  1.390e-01  2.473e+01
    ## hearingHearing                           -1.965e-02  1.533e-01  2.479e+01
    ## directionreversed                        -4.865e-03  1.260e-01  5.140e+01
    ## signyrs                                   4.074e-04  5.879e-03  2.514e+01
    ## hearingHearing:directionreversed         -6.917e-02  1.408e-01  5.126e+01
    ## hearingHearing:signyrs                    2.119e-05  7.836e-03  2.561e+01
    ## directionreversed:signyrs                -5.631e-04  5.396e-03  5.107e+01
    ## hearingHearing:directionreversed:signyrs  2.279e-03  7.272e-03  5.119e+01
    ##                                          t value Pr(>|t|)    
    ## (Intercept)                                7.048 2.32e-07 ***
    ## hearingHearing                            -0.128    0.899    
    ## directionreversed                         -0.039    0.969    
    ## signyrs                                    0.069    0.945    
    ## hearingHearing:directionreversed          -0.491    0.625    
    ## hearingHearing:signyrs                     0.003    0.998    
    ## directionreversed:signyrs                 -0.104    0.917    
    ## hearingHearing:directionreversed:signyrs   0.313    0.755    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) hrngHr drctnr sgnyrs hrngHrng:d hrngHrng:s drctn:
    ## hearingHrng -0.907                                                  
    ## dirctnrvrsd -0.453  0.416                                           
    ## signyrs     -0.976  0.888  0.449                                    
    ## hrngHrng:dr  0.410 -0.455 -0.905 -0.407                             
    ## hrngHrng:sg  0.736 -0.925 -0.343 -0.754  0.427                      
    ## drctnrvrsd:  0.443 -0.408 -0.978 -0.459  0.887      0.352           
    ## hrngHrng:d: -0.334  0.422  0.738  0.347 -0.928     -0.464     -0.757

``` r
data.eyes.late <- data.fc %>% filter(maingroup=="HearingLateASL" | maingroup=="DeafLateASL")
model <- lmer(percent ~ hearing * direction * signyrs + (1|id) + (1|story), data = data.eyes.late)
summary(model)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: percent ~ hearing * direction * signyrs + (1 | id) + (1 | story)
    ##    Data: data.eyes.late
    ## 
    ## REML criterion at convergence: -103.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3094 -0.1773  0.0360  0.4459  1.7308 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  id       (Intercept) 0.0039593 0.06292 
    ##  story    (Intercept) 0.0002182 0.01477 
    ##  Residual             0.0051858 0.07201 
    ## Number of obs: 75, groups:  id, 20; story, 4
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error         df
    ## (Intercept)                               9.799e-01  1.390e-01  2.473e+01
    ## hearingHearing                           -1.965e-02  1.533e-01  2.479e+01
    ## directionreversed                        -4.865e-03  1.260e-01  5.140e+01
    ## signyrs                                   4.074e-04  5.879e-03  2.514e+01
    ## hearingHearing:directionreversed         -6.917e-02  1.408e-01  5.126e+01
    ## hearingHearing:signyrs                    2.119e-05  7.836e-03  2.561e+01
    ## directionreversed:signyrs                -5.631e-04  5.396e-03  5.107e+01
    ## hearingHearing:directionreversed:signyrs  2.279e-03  7.272e-03  5.119e+01
    ##                                          t value Pr(>|t|)    
    ## (Intercept)                                7.048 2.32e-07 ***
    ## hearingHearing                            -0.128    0.899    
    ## directionreversed                         -0.039    0.969    
    ## signyrs                                    0.069    0.945    
    ## hearingHearing:directionreversed          -0.491    0.625    
    ## hearingHearing:signyrs                     0.003    0.998    
    ## directionreversed:signyrs                 -0.104    0.917    
    ## hearingHearing:directionreversed:signyrs   0.313    0.755    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) hrngHr drctnr sgnyrs hrngHrng:d hrngHrng:s drctn:
    ## hearingHrng -0.907                                                  
    ## dirctnrvrsd -0.453  0.416                                           
    ## signyrs     -0.976  0.888  0.449                                    
    ## hrngHrng:dr  0.410 -0.455 -0.905 -0.407                             
    ## hrngHrng:sg  0.736 -0.925 -0.343 -0.754  0.427                      
    ## drctnrvrsd:  0.443 -0.408 -0.978 -0.459  0.887      0.352           
    ## hrngHrng:d: -0.334  0.422  0.738  0.347 -0.928     -0.464     -0.757

Ehhhh. Wha tabout using gaze behavior to predict accuracy, would that differ based on deafness?

``` r
model <- lmer(acc ~ percent * aoasl * direction * hearing + (1|id) + (1|story), data = data.eyes)
summary(model)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: acc ~ percent * aoasl * direction * hearing + (1 | id) + (1 |  
    ##     story)
    ##    Data: data.eyes
    ## 
    ## REML criterion at convergence: -204.4
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.44320 -0.53124  0.01888  0.52296  2.15330 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.003829 0.06188 
    ##  story    (Intercept) 0.001612 0.04015 
    ##  Residual             0.007943 0.08912 
    ## Number of obs: 172, groups:  id, 52; story, 4
    ## 
    ## Fixed effects:
    ##                                                  Estimate Std. Error
    ## (Intercept)                                      0.852542   0.035269
    ## percent                                         -0.015586   0.102953
    ## aoasl                                            0.001309   0.003449
    ## directionreversed                               -0.124240   0.035335
    ## hearingHearing                                  -0.031431   0.202203
    ## percent:aoasl                                   -0.003674   0.012099
    ## percent:directionreversed                        0.055902   0.151347
    ## aoasl:directionreversed                         -0.001915   0.003913
    ## percent:hearingHearing                           0.082488   0.453610
    ## aoasl:hearingHearing                            -0.001501   0.011626
    ## directionreversed:hearingHearing                -0.432235   0.240891
    ## percent:aoasl:directionreversed                 -0.007179   0.018674
    ## percent:aoasl:hearingHearing                    -0.001482   0.028120
    ## percent:directionreversed:hearingHearing         1.000858   0.520702
    ## aoasl:directionreversed:hearingHearing           0.027178   0.014003
    ## percent:aoasl:directionreversed:hearingHearing  -0.061338   0.034818
    ##                                                        df t value Pr(>|t|)
    ## (Intercept)                                     20.660000  24.173  < 2e-16
    ## percent                                        144.450000  -0.151 0.879880
    ## aoasl                                          116.300000   0.379 0.705038
    ## directionreversed                              133.900000  -3.516 0.000598
    ## hearingHearing                                 114.810000  -0.155 0.876747
    ## percent:aoasl                                  143.980000  -0.304 0.761796
    ## percent:directionreversed                      142.730000   0.369 0.712403
    ## aoasl:directionreversed                        124.110000  -0.489 0.625498
    ## percent:hearingHearing                         134.030000   0.182 0.855977
    ## aoasl:hearingHearing                           116.940000  -0.129 0.897526
    ## directionreversed:hearingHearing               125.620000  -1.794 0.075167
    ## percent:aoasl:directionreversed                133.390000  -0.384 0.701265
    ## percent:aoasl:hearingHearing                   144.910000  -0.053 0.958051
    ## percent:directionreversed:hearingHearing       127.550000   1.922 0.056819
    ## aoasl:directionreversed:hearingHearing         126.350000   1.941 0.054497
    ## percent:aoasl:directionreversed:hearingHearing 129.920000  -1.762 0.080477
    ##                                                   
    ## (Intercept)                                    ***
    ## percent                                           
    ## aoasl                                             
    ## directionreversed                              ***
    ## hearingHearing                                    
    ## percent:aoasl                                     
    ## percent:directionreversed                         
    ## aoasl:directionreversed                           
    ## percent:hearingHearing                            
    ## aoasl:hearingHearing                              
    ## directionreversed:hearingHearing               .  
    ## percent:aoasl:directionreversed                   
    ## percent:aoasl:hearingHearing                      
    ## percent:directionreversed:hearingHearing       .  
    ## aoasl:directionreversed:hearingHearing         .  
    ## percent:aoasl:directionreversed:hearingHearing .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 16 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##   vcov(x)     if you need it
