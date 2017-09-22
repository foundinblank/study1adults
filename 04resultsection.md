Results Section (study1adults)
================
Adam Stone, PhD
09-22-2017

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

Refreshing Ourselves
====================

> The main goal of the study is to see if comprehension is related to gaze behavior and if AoA impacts gaze behavior.

**Key Questions:** Can gaze behavior reflect whether or not a person can understand the story? When a person has a harder time understanding a difficult story, maybe because they lack the skills, can this be observed in gaze behavior?

> It is more important to show that our behavioral measure IS sensitive to effects of AoA. Period. *The cause is secondary.*

Now let's load in the dataset called `cleanpercentdata.csv`. It contains item-level data showing the percent of looking for each AOI (10 AOIs total) for each story and each participant *still in the study*. All data defined as "bad" (entire participants with poor calibration, or individual stories with too little looking data) have been dropped. We will then move this to subject-level by averaging each participant's two forward stories together and two reversed stories together.

``` r
#library(corrplot)
library(Hmisc)
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
R <- rcorr(x)$r 
p <- rcorr(x)$P 
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
11
</td>
<td style="text-align:right;">
32.9
</td>
<td style="text-align:right;">
9.7
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
32.5
</td>
<td style="text-align:right;">
9.8
</td>
<td style="text-align:right;">
0.3
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
7
</td>
<td style="text-align:right;">
34.9
</td>
<td style="text-align:right;">
5.8
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
0.0
</td>
<td style="text-align:right;">
29.7
</td>
<td style="text-align:right;">
5.3
</td>
<td style="text-align:right;">
5.1
</td>
<td style="text-align:right;">
2.4
</td>
</tr>
<tr>
<td style="text-align:left;">
DeafLateASL
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
37.2
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
23.7
</td>
<td style="text-align:right;">
6.2
</td>
<td style="text-align:right;">
12.8
</td>
<td style="text-align:right;">
1.7
</td>
</tr>
<tr>
<td style="text-align:left;">
HearingLateASL
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
28.9
</td>
<td style="text-align:right;">
6.5
</td>
<td style="text-align:right;">
4.7
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
11.5
</td>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
17.5
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
11
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
7
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.10
</td>
</tr>
<tr>
<td style="text-align:left;">
DeafLateASL
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.71
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
11
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.08
</td>
<td style="text-align:right;">
0.68
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

    ## Warning: Removed 311 rows containing non-finite values (stat_boxplot).

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
  scale_fill_viridis(option = "inferno") +
  facet_wrap("direction") +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

![](04resultsection_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

Also of interest is how the FaceChest Index changes. And it shows us there's definitely more of a pull to the Chest during reversed stories.

``` r
data.indexonly <- data %>%
  filter(aoi == "facechest")
ggplot(data.indexonly,aes(aoi,percent,fill=direction)) + 
  geom_violin() +
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

#data.acc.results <- rcorr(as.matrix(data.acc))
corstarsl(data.acc) # Use the awesome function!
```

    ##                 aoasl  signyrs selfrate     age acc.forward acc.reversed
    ## aoasl                                                                   
    ## signyrs      -0.80***                                                   
    ## selfrate     -0.53***  0.71***                                          
    ## age           -0.35*   0.84***  0.63***                                 
    ## acc.forward    -0.09     0.28   0.46**   0.34*                          
    ## acc.reversed  -0.35*    0.30*   0.40**    0.15       0.37*              
    ## acc.effect      0.27    -0.07    -0.04    0.11      0.41**      -0.69***

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
#rcorr(as.matrix(data.fw))
corstarsl(data.fw)
```

    ##                acc    aoasl  signyrs selfrate     age     eyes    mouth
    ## acc                                                                    
    ## aoasl       -0.09                                                      
    ## signyrs      0.28  -0.80***                                            
    ## selfrate   0.46**  -0.53***  0.71***                                   
    ## age         0.34*   -0.35*   0.84***  0.63***                          
    ## eyes        -0.22     0.16    -0.15    -0.12   -0.08                   
    ## mouth        0.19    -0.14     0.25     0.10    0.24  -0.61***         
    ## chin         0.05    -0.14    -0.01     0.13   -0.13  -0.38**  -0.46** 
    ## face         0.10    -0.25   0.38**    0.30*   0.35*    -0.09   0.46** 
    ## chest       -0.03     0.16   -0.30*   -0.30*  -0.33*    -0.09   -0.32* 
    ## facechest    0.03    -0.17    0.31*    0.30*   0.33*     0.09    0.32* 
    ##             chin     face    chest
    ## acc                               
    ## aoasl                             
    ## signyrs                           
    ## selfrate                          
    ## age                               
    ## eyes                              
    ## mouth                             
    ## chin                              
    ## face      -0.19                   
    ## chest      0.27  -0.93***         
    ## facechest -0.27   0.94*** -1.00***

Reversed Correlations
---------------------

Here's the Pearson's correlation matrix for reversed stories.

-   Again, no eye behavior metric predicts accuracy on reversed stories.
-   Unlike forward stories, we see an effect of AoASL on looking at the face, chest, and FaceChest ratio. Those who acquired ASL later are more likely to look at the chest more and less at the face. Nice! Sign Years too at this.
-   As before, signyears continues to be a strong correlator with looking behavior, and so does age.
-   Same for age.

So.

``` r
#rcorr(as.matrix(data.rv))
corstarsl(data.rv)
```

    ##                acc    aoasl  signyrs selfrate      age     eyes    mouth
    ## acc                                                                     
    ## aoasl      -0.35*                                                       
    ## signyrs     0.30*  -0.80***                                             
    ## selfrate   0.40**  -0.53***  0.71***                                    
    ## age          0.15   -0.35*   0.84***  0.63***                           
    ## eyes        -0.15     0.12    -0.01     0.06     0.11                   
    ## mouth        0.16    -0.16     0.28     0.12     0.27  -0.63***         
    ## chin         0.04    -0.06    -0.14    -0.07    -0.26  -0.47**  -0.44** 
    ## face         0.27   -0.30*   0.40**     0.28    0.35*    -0.14   0.58***
    ## chest       -0.20    0.33*  -0.44**  -0.41**  -0.38**    -0.17   -0.36* 
    ## facechest    0.22   -0.34*   0.44**   0.40**    0.38*     0.15    0.37* 
    ##             chin     face    chest
    ## acc                               
    ## aoasl                             
    ## signyrs                           
    ## selfrate                          
    ## age                               
    ## eyes                              
    ## mouth                             
    ## chin                              
    ## face      -0.13                   
    ## chest      0.29  -0.79***         
    ## facechest -0.28   0.81*** -1.00***

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
    ## 1           maingroup  3.565 0.010       5.136       0.001
    ## 2           direction 61.539 0.000       2.225       0.140
    ## 3 maingroup:direction  1.186 0.323       0.683       0.605
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
    ## 1 maingroup 1.956 0.119       1.696       0.169
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
    ## 1 maingroup   2.7 0.044       3.586       0.014
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

    ## Warning: Removed 15 rows containing non-finite values (stat_boxplot).

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
