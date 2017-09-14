Eye Gaze Analysis (study1adults)
================
Adam Stone, PhD
09-14-2017

-   [Re-Initializing](#re-initializing)
-   [AOIs](#aois)
-   [Data Cleaning](#data-cleaning)
    -   [Percentage Data and Viz](#percentage-data-and-viz)
-   [Big Five AOIs](#big-five-aois)
    -   [ANOVAS](#anovas)
    -   [Groups Only](#groups-only)
    -   [Age of ASL & Hearing Status ANCOVA](#age-of-asl-hearing-status-ancova)
-   [3 Face AOIs Only](#face-aois-only)
    -   [Visualizations](#visualizations)
    -   [Group ANOVA](#group-anova)
    -   [Age of ASL & Hearing Status ANCOVA](#age-of-asl-hearing-status-ancova-1)
-   [Left/Right Analysis](#leftright-analysis)
-   [Assorted/older stuff pushed to the bottom](#assortedolder-stuff-pushed-to-the-bottom)

Re-Initializing
===============

This assumes you've already done [01dataimportclean](01dataimportclean.nb.html) and so there'll be a nice new .csv file to re-import here. Also we gotta import all the libraries again. This shouldn't depend on anything we did for 02 Lexical Recall Analysis.

``` r
# Import packages we'll need.
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(stringr)
library(lme4)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

``` r
library(lmerTest)
```

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
library(prettydoc)
library(broom)
library(knitr)
library(xtable)
library(kableExtra)
options(knitr.table.format = "html") 
data <- read_csv('cleandata.csv',col_types = 
                   cols(.default = col_double(),
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
                        aoasl = col_integer(),
                        acc = col_double(),
                        forehead = col_double(),
                        eyes = col_double(),
                        mouth = col_double(),
                        chin = col_double(),
                        upperchest = col_double(),
                        midchest = col_double(),
                        lowerchest = col_double(),
                        belly = col_double(),
                        left = col_double(),
                        right = col_double(),
                        total = col_double()
                   ))
# And factorize
data <- data %>%
  mutate(hearing = as.factor(hearing)) %>%
  mutate(videogroup = as.factor(videogroup)) %>%
  mutate(aoagroup = as.factor(aoagroup)) %>%
  mutate(languagegroup = as.factor(languagegroup)) %>%
  mutate(maingroup = as.factor(maingroup)) %>%
  mutate(video = as.factor(video)) %>%
  mutate(story = as.factor(story)) %>%
  mutate(direction = as.factor(direction))
```

AOIs
====

These are our current AOIs. 1. Forehead (above eyes) 2. Eyes 3. Mouth 4. Chin (below chin) 5. Upper Chest 6. Middle Chest 7. Lower Chest 8. Belly 9. Left 10. Right

It's possible to do a secondary analysis combining some of these AOIs (in particular, maybe 5-6 and 7-8 can be combined into Torso Upper Half and Torso Lower Half). Anyway, the face AOIs are important, and the division of them into 4 areas is theoretically motivated and also previously seen in the literature.

*Why 4 AOIs on Face?* Emmorey et al. (2008) did this same setup. We generally know people fixate on the face across all conditions and langauge experiences, but **where** on the face is important for us to know. So these 4 AOIs. *Write one sentence here about predictions for language experience on face-looking.*

*Why 4 AOIs for Torso?* Past papers tend to just classify the body as “body” with no further breakdown, or two-part breakdown. In our study, we have higher resolution to break this down into four AOI’s, defined as upper chest, middle chest, lower chest, and belly. We know that hands start and end at the belly and the hands spend the majority of the time in these four areas in front of the torso (and the hands spend very little time overlapping the face). If an observer (child or adult) glances at the hands (or if the hands have any “gravity” upon gaze behavior), then gaze samples will fall within these four torso areas. Although we expect that all observers do spend the most amount of time on the face, we also predict that the number of gazes towards the hands (by way of “torso”) might be impacted by language experience. *(make a footnote here: As a future project, we will analyze the data using dynamic “hand” AOIs, for each hand, in which we document, on each frame, where the left and right hands are in viewing space.)*

*Why 2 AOIs for left vs right?* People have talked how sign language impacts left vs right visual field asymmetries, as related to hemispheric laterality for language processing, so it is worth checking this. If we do find an asymmetry, we will then just touch upon this literature in the discussion, but also acknowledge that it could be the signer’s hand dominance that drives a lateral asymmetry too, not just a hemispheric asymmetry. Meaning, if the signer is right handed, her dominant hand might have some “gravity” in the viewer’s left visual field. (And we can check this with the future analysis of dynamic hand AOIs.)

Data Cleaning
=============

This is my process of documenting how I'm weeding through data and making sure all's good. Let's visualize first of all.

``` r
# Reduce dataset to face AOIs only
data.face <- select(data,-upperchest,-midchest,-lowerchest,-belly,-left,-right)
# Reshape data so we can easily facet our charts based on face AOIs
data.face <- data.face %>% gather(aoi,looking,forehead:chin)
# Graph!
ggplot(data.face,aes(x=maingroup,y=looking,fill=direction)) +
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  facet_wrap("aoi")
```

    ## Warning: Removed 157 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png) Okay, right away I see some issues - I want to check for outliers but I'm not sure what could count as an outlier. All 4 stories are different lengths - a data point at 30 seconds would be fine for King Midas (0:37) but impossible for Red Riding Hood (0:18) so outliers need to be *relative* to the story length itself. Let's back up and do histograms for each story.

``` r
ggplot(data.face,aes(x=looking)) +
  geom_histogram(binwidth=1) +
  facet_wrap("story") +
  xlab("secs") +
  ggtitle("Face AOI sums for each story for each participant")
```

    ## Warning: Removed 157 rows containing non-finite values (stat_bin).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png) Loooks good but I see weird outliers for Red Riding Hood (before Cinderalla too, which I fixed) - those single data points are past the video length (and that's just the face AOIs!). Let's sum up *all* AOIs across each story for each participant...back to the big dataset, and we'll do histograms again.

``` r
# data2 <- data %>%
#   group_by(id,story) %>%
#   mutate(total = sum(forehead,eyes,mouth,chin,upperchest,
#                      midchest,lowerchest,belly,left,right,na.rm=TRUE))
ggplot(data,aes(x=total)) +
  geom_histogram(binwidth=1) +
  facet_wrap("story") +
  xlab("secs") +
  ggtitle("Sum of ALL AOIs for each participant for each story")
```

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png) The tall bars are near the end of the story, right? So we see two issues: 1. Some barely watched the story at all. (Those are the ones with bars at or near zero). We should remove those. We need a rule for it. 1. A few people's AOI data has total seconds higher than the video itself! (Those are the ones with very short bars to the right of the very tall bars.) Those should be investigated, something went wrong in the data.

I'll highlight those rows that's for \#2 and send to Rain to look at.

But maybe a good way for diagnosing problem trials is to look at each AOI for each story, instead of sums of AOIs. Any outliers can be easily seen in the histograms. Let's reshape the data again and generate histograms.

``` r
data.reshape <- data %>% gather(aoi,looking,forehead:total)
ggplot(data.reshape,aes(x=looking)) +
  geom_histogram(binwidth=1) +
  facet_grid(aoi ~ story) +
  xlab("secs") +
  ggtitle("Looking times of each AOI for each participant for each story")
```

    ## Warning: Removed 812 rows containing non-finite values (stat_bin).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

Need to mark this up.

Cool. Now I want to know how many people have really low looking times for each story. We can do this easily. Here's a table for how many have looking time sums that are less than 25% of the story, or less than 50% of the story.

``` r
# Split into 4 datasets (1 each story)
data.cinderella <- filter(data,story == "Cinderella")
data.goldilocks <- filter(data,story == "Goldilocks")
data.kingmidas <- filter(data,story == "KingMidas")
data.redridinghood <- filter(data,story== "RedRidingHood")

# FALSE = less than quarter or total story length
data.cinderella$quarter <- data.cinderella$total >= 5.5
data.cinderella$half <- data.cinderella$total >= 11
data.goldilocks$quarter <- data.goldilocks$total >=  5.25
data.goldilocks$half <- data.goldilocks$total >= 10.5
data.kingmidas$quarter <- data.kingmidas$total >= 9.25
data.kingmidas$half <- data.kingmidas$total >= 18.5
data.redridinghood$quarter <- data.redridinghood$total >= 4.5
data.redridinghood$half <- data.redridinghood$total >= 9

# Put it back together
data <- bind_rows(data.cinderella,data.goldilocks,data.kingmidas,data.redridinghood)

# Calculations
lowlooking <- data %>%
  group_by(story) %>%
  summarize(lessthan25 = sum(!quarter),
            lessthan50 = sum(!half),
            total = sum(!quarter,quarter))
lowlooking
```

    ## # A tibble: 4 x 4
    ##           story lessthan25 lessthan50 total
    ##          <fctr>      <int>      <int> <int>
    ## 1    Cinderella          4          8    46
    ## 2    Goldilocks          3          4    46
    ## 3     KingMidas          3          5    46
    ## 4 RedRidingHood          3          4    46

``` r
lowlookingid <- filter(data,quarter==FALSE) %>% 
  ungroup() %>%
  select(id,participant,hearing,videogroup,story,direction,total) %>%
  arrange(participant)
write.csv(lowlookingid, file="lessthan25.csv")
select(lowlookingid,-participant)
```

    ## # A tibble: 13 x 6
    ##       id hearing videogroup         story direction total
    ##    <int>  <fctr>     <fctr>        <fctr>    <fctr> <dbl>
    ##  1    10    Deaf    Group 1    Cinderella  reversed  4.62
    ##  2    10    Deaf    Group 1     KingMidas   forward  2.66
    ##  3    32 Hearing    Group 2    Goldilocks  reversed  4.08
    ##  4    31 Hearing    Group 2    Cinderella   forward  4.73
    ##  5     6    Deaf    Group 1    Cinderella  reversed  4.43
    ##  6     6    Deaf    Group 1 RedRidingHood  reversed  3.80
    ##  7     5    Deaf    Group 1     KingMidas   forward  2.91
    ##  8     5    Deaf    Group 1 RedRidingHood  reversed  1.96
    ##  9    25    Deaf    Group 2    Goldilocks  reversed  4.62
    ## 10     7    Deaf    Group 1    Cinderella  reversed  0.81
    ## 11     7    Deaf    Group 1     KingMidas   forward  6.84
    ## 12    30 Hearing    Group 1    Goldilocks   forward  2.56
    ## 13    17    Deaf    Group 1 RedRidingHood  reversed  0.52

So I will filter out the data with &lt;25% looking time. Maybe the threshold should be higher, but we'll revisit that later.

``` r
originalrows <- nrow(data)
data <- filter(data,quarter==TRUE)
difference <- originalrows - nrow(data)
```

So 13 stories were dropped from the previous total of 184 stories for a new total of 171 stories.

Percentage Data and Viz
-----------------------

We need to work with percentages, because of participants' idiosyntractic eye behavior. Some blink a lot, some don't, so automatically the maximum number of eye gaze data points each participant is able to contribute is different. For that reason we work with percent of total data points on a per-participant basis. That's also why we took out stories with &lt;25% looking data. Now here's the boxplots for each AOI.

``` r
# data2 uses percentage data!
data2 <- data %>%
  mutate_at(vars(forehead:right), funs(./total)) %>%
  select(-total, -quarter, -half) %>%
  gather(aoi,percent,forehead:right)

ggplot(data2, aes(x=aoi,y=percent)) +
  geom_boxplot() 
```

    ## Warning: Removed 748 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-1.png)

Big Five AOIs
=============

> You’ll have to decide how to put the AOIs in an ANOVA. All of them together is too many. And you cannot put ALL the AOIs in. If they all sum to 100% (which they currently do), then the observations are not independent. Also, you can’t put AOIs that have near-zero values in with AOIs that have super high values, you’ll get whopping significance that is too obvious to reveal anything meaningful.

Based on the boxplot there are five AOIs that got hit the most: forehead, eyes, mouth, chin, and upperchest. **But this is really important...I think there is one or two outliers in forehead. And maybe it's better to get rid of forehead and upper chest for the big ANOVAs to keep things simple. Neither of them touch 50%...but then again they are not "non-significant." I am going ahead with all 5 for now.**

*Important reference levels* - AOI reference level is eyes - MainGroup reference level is NativeDeaf

Creating the `data.big5` thing here.

``` r
# Make Big5 df with reference levels
data.big5 <- filter(data2,aoi == "forehead" | aoi == "eyes" 
                    | aoi == "mouth" | aoi == "chin" | aoi == "upperchest") %>%
  mutate(aoi = as.factor(aoi))
data.big5$aoi <- factor(data.big5$aoi, levels=c("upperchest","chin","mouth","eyes","forehead"))
data.big5$aoi <- relevel(data.big5$aoi, ref="eyes")
data.big5$maingroup <- relevel(data.big5$maingroup, ref="NativeDeaf")
```

ANOVAS
------

Because we're doing ANOVAs, that means we need subject-level data, not trial-level data. Let's bump the `data.big5` up one level.

``` r
data.big5.item <- data.big5 # save item-level data for later

# Pull out and save subject info
data.big5.subjectinfo <- data.big5 %>%
  select(-acc,-aoi,-percent,-video,-story) %>%
  distinct()

# Now collapse data.big5 to subject-level 
data.big5 <- data.big5 %>%
  group_by(participant,direction,aoi) %>%
  summarize(percent = mean(percent,na.rm=TRUE))
data.big5[data.big5=="NaN"] <- NA


# Join subject info with data.big5 that's now subject-level
data.big5 <- left_join(data.big5,data.big5.subjectinfo, by=c("participant","direction"))
```

Groups Only
-----------

Now we can do the ANOVAs.

> First, check the most important stats: Subject Groups, Video Condition, and some AOIs that are on the central body like: eyes, nose, mouth, neck, chest, below chest. You can combine some. If you see that no one ever looked at the forehead, you can dispense of that, and say that in the paper when you rationalize your AOIs used for stats. There might be a significant group main effect, OR a group main effect for reversed and not for forward, and furthermore, maybe only the late AoA groups show a reversal effect.

First let's do groups only (no continuous variables). First the viz, then the stats.

``` r
ggplot(data.big5) + 
  geom_boxplot(aes(x=maingroup,y=percent,color=direction)) +
  facet_wrap("aoi") + 
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 69 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-1.png)

``` r
data.big5.viz <- data.big5 %>%
  group_by(maingroup,direction,aoi) %>%
  summarize(mean = mean(percent,na.rm=TRUE), sd = sd(percent,na.rm=TRUE))
ggplot(data.big5.viz, aes(x=maingroup,y=mean,color=direction)) +
  geom_point(position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.1,position=position_dodge(0.5)) +
  facet_wrap("aoi") +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-1.png)

Or a heat map!

``` r
data.big5.reduce <- data.big5 %>%
  group_by(maingroup,aoi,direction) %>%
  summarize(meanlooking = mean(percent, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(aoi = factor(aoi,levels=c("upperchest","chin","mouth","eyes","forehead")))
ggplot(data.big5.reduce, aes(x = maingroup, y = aoi)) +
  geom_tile(aes(fill=meanlooking),na.rm=TRUE) + 
  scale_fill_gradient(low = "lightblue",high = "steelblue") +
  facet_wrap("direction") +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-14-1.png)

The ANOVA below tells us there's a significant effect of AOI, and significant interactions of AOI x Direction and AOI x MainGroup.

``` r
group.anova <- aov(data=data.big5,percent ~ aoi * direction * maingroup)
anova(group.anova)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                          Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## aoi                       4 15.1766  3.7942 87.2717 < 2.2e-16 ***
    ## direction                 1  0.0000  0.0000  0.0009  0.976339    
    ## maingroup                 4  0.0193  0.0048  0.1107  0.978720    
    ## aoi:direction             4  0.5698  0.1424  3.2763  0.011805 *  
    ## aoi:maingroup            16  1.5712  0.0982  2.2588  0.003935 ** 
    ## direction:maingroup       4  0.0022  0.0005  0.0126  0.999685    
    ## aoi:direction:maingroup  16  0.1647  0.0103  0.2368  0.999144    
    ## Residuals               336 14.6077  0.0435                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Here's the posthoc (Tukey's HSD) for AOI, which tells us most AOIs are different from each other, except: 1. chin vs. eyes 1. forehead vs. upper chest

Forget doing posthocs for all the interactions, way too many.

``` r
group.anova.posthoc <- TukeyHSD(group.anova,'aoi',conf.level = 0.95) 
group.anova.posthoc
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = percent ~ aoi * direction * maingroup, data = data.big5)
    ## 
    ## $aoi
    ##                            diff         lwr          upr     p adj
    ## upperchest-eyes     -0.16887467 -0.25770293 -0.080046421 0.0000032
    ## chin-eyes           -0.02410231 -0.11010420  0.061899581 0.9394317
    ## mouth-eyes           0.37010631  0.28410441  0.456108200 0.0000000
    ## forehead-eyes       -0.14357701 -0.25497083 -0.032183183 0.0042182
    ## chin-upperchest      0.14477236  0.05712807  0.232416660 0.0000799
    ## mouth-upperchest     0.53898098  0.45133668  0.626625278 0.0000000
    ## forehead-upperchest  0.02529767 -0.08736902  0.137964349 0.9725244
    ## mouth-chin           0.39420862  0.30943014  0.478987093 0.0000000
    ## forehead-chin       -0.11947470 -0.22992672 -0.009022678 0.0265760
    ## forehead-mouth      -0.51368332 -0.62413533 -0.403231296 0.0000000

Age of ASL & Hearing Status ANCOVA
----------------------------------

MainGroup is actually two different variables combined together: deaf and hearing, and native/early/late learners. What if we separated those out and regressed AoASL as a continuous variable, and added deaf/hearing as a factor. Again, first the viz, then the stats.

``` r
# Draw it
ggplot(data.big5, aes(x=aoasl,y=percent)) +
  geom_point(aes(color=direction,shape=hearing)) +
  geom_smooth(aes(color=direction,linetype=hearing),method="lm",se=FALSE) +
  facet_wrap("aoi")
```

    ## Warning: Removed 69 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 69 rows containing missing values (geom_point).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

Let's move to ANOVAs. This is technically an ANCOVA, and AoASL is the covariate. The output tells us there is a significant main effect of AOI, and significant interactions of AOI x Direction and AOI x Hearing and AOI x Hearing X AoASL. So really it's very similar to what we got with the group ANOVA.

``` r
continuous.anova <- aov(data=data.big5, percent ~ aoi * direction * hearing * aoasl)
anova(continuous.anova)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                              Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## aoi                           4 15.1766  3.7942 88.5884 < 2.2e-16 ***
    ## direction                     1  0.0000  0.0000  0.0009  0.976161    
    ## hearing                       1  0.0132  0.0132  0.3085  0.578973    
    ## aoasl                         1  0.0004  0.0004  0.0087  0.925885    
    ## aoi:direction                 4  0.5691  0.1423  3.3222  0.010903 *  
    ## aoi:hearing                   4  0.7265  0.1816  4.2405  0.002304 ** 
    ## direction:hearing             1  0.0003  0.0003  0.0075  0.931263    
    ## aoi:aoasl                     4  0.1820  0.0455  1.0625  0.374988    
    ## direction:aoasl               1  0.0004  0.0004  0.0083  0.927254    
    ## hearing:aoasl                 1  0.0053  0.0053  0.1246  0.724300    
    ## aoi:direction:hearing         4  0.0272  0.0068  0.1587  0.958980    
    ## aoi:direction:aoasl           4  0.0159  0.0040  0.0925  0.984780    
    ## aoi:hearing:aoasl             4  0.5592  0.1398  3.2640  0.012020 *  
    ## direction:hearing:aoasl       1  0.0003  0.0003  0.0078  0.929538    
    ## aoi:direction:hearing:aoasl   4  0.0162  0.0040  0.0945  0.984175    
    ## Residuals                   346 14.8189  0.0428                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Maybe it's a problem that all the hearing people have late AoASL while most deaf have early AoASL. And I don't like the forehead outliers throwing off some of those lines from the graph above so that needs to be looked at. What if we tried sign-years instead of AoASL.

``` r
ggplot(data.big5, aes(x=signyrs,y=percent)) +
  geom_point(aes(color=direction,shape=hearing)) +
  geom_smooth(aes(color=direction,linetype=hearing),method="lm",se=FALSE) +
  facet_wrap("aoi")
```

    ## Warning: Removed 69 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 69 rows containing missing values (geom_point).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-19-1.png)

Look interesting and you can sort of compare the deaf/hearing lines better, although we should get rid of that person who's been signing for 60 years if we do a years-of-signing analysis. Here's the ANCOVA.

Let's move to ANOVAs. This is technically an ANCOVA, and AoASL is the covariate. The output tells us there is a significant main effect of AOI, and significant interactions of AOI x Direction and AOI x Hearing and AOI x SignYrs. Again a rather similar thing result.

``` r
continuous.anova.sy <- aov(data=data.big5, percent ~ aoi * direction * hearing * signyrs)
anova(continuous.anova.sy)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                                Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## aoi                             4 15.1766  3.7942 88.5694 < 2.2e-16 ***
    ## direction                       1  0.0000  0.0000  0.0009  0.976163    
    ## hearing                         1  0.0132  0.0132  0.3084  0.579014    
    ## signyrs                         1  0.0015  0.0015  0.0361  0.849424    
    ## aoi:direction                   4  0.5685  0.1421  3.3179  0.010982 *  
    ## aoi:hearing                     4  0.7262  0.1815  4.2380  0.002314 ** 
    ## direction:hearing               1  0.0003  0.0003  0.0069  0.933720    
    ## aoi:signyrs                     4  0.4723  0.1181  2.7562  0.027897 *  
    ## direction:signyrs               1  0.0000  0.0000  0.0004  0.983219    
    ## hearing:signyrs                 1  0.0023  0.0023  0.0531  0.817971    
    ## aoi:direction:hearing           4  0.0213  0.0053  0.1245  0.973581    
    ## aoi:direction:signyrs           4  0.0207  0.0052  0.1206  0.975103    
    ## aoi:hearing:signyrs             4  0.2038  0.0510  1.1894  0.315163    
    ## direction:hearing:signyrs       1  0.0002  0.0002  0.0051  0.943196    
    ## aoi:direction:hearing:signyrs   4  0.0824  0.0206  0.4808  0.749822    
    ## Residuals                     346 14.8220  0.0428                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

3 Face AOIs Only
================

I'm giving this a shot. Because the boxplot way up there tells us there wasn't much looking to the forehead or the upper chest, compared with eyes, mouth, and chin. **So I'm going to take out forehead and upper chest AOIs and see what we get.**

``` r
# Make face3 df with reference levels
data.face3 <- filter(data2, aoi == "eyes" | aoi == "mouth" | aoi == "chin") %>%
  mutate(aoi = as.factor(aoi))
data.face3$aoi <- factor(data.face3$aoi, levels=c("eyes","mouth","chin"))
#data.face3$aoi <- relevel(data.face3$aoi, ref="eyes")
data.face3$maingroup <- relevel(data.face3$maingroup, ref="NativeDeaf")

data.face3.item <- data.face3 # save item-level data for later

# Pull out and save subject info
data.face3.subjectinfo <- data.face3 %>%
  select(-acc,-aoi,-percent,-video,-story) %>%
  distinct()

# Now collapse data.big5 to subject-level 
data.face3 <- data.face3 %>%
  group_by(participant,direction,aoi) %>%
  summarize(percent = mean(percent,na.rm=TRUE))
data.face3[data.face3=="NaN"] <- NA

# Join subject info with data.big5 that's now subject-level
data.face3 <- left_join(data.face3,data.face3.subjectinfo, by=c("participant","direction"))
```

Visualizations
--------------

Let's start with the visualizations.

``` r
ggplot(data.face3) + 
  geom_boxplot(aes(x=maingroup,y=percent,fill=direction)) +
  facet_grid(direction ~ aoi) + 
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-22-1.png)

But we have less levels so maybe another way of looking at the boxplots:

``` r
ggplot(data.face3) + 
  geom_boxplot(aes(x=maingroup,y=percent,fill=aoi)) +
  facet_grid(direction~.) + 
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-23-1.png)

Or another way even

``` r
ggplot(data.face3) + 
  geom_boxplot(aes(x=aoi,y=percent,fill=direction)) +
  facet_grid(direction~maingroup) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-24-1.png)

Group ANOVA
-----------

Anyway. Onto the ANOVAs. It gives us virtually identical results as the `big5` ANOVAs.

``` r
group.anova.face3 <- aov(data=data.face3,percent ~ aoi * direction * maingroup)
anova(group.anova.face3)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                          Df  Sum Sq Mean Sq F value  Pr(>F)    
    ## aoi                       2  8.8187  4.4094 75.9048 < 2e-16 ***
    ## direction                 1  0.0151  0.0151  0.2606 0.61015    
    ## maingroup                 4  0.0461  0.0115  0.1984 0.93902    
    ## aoi:direction             2  0.5114  0.2557  4.4017 0.01327 *  
    ## aoi:maingroup             8  1.4350  0.1794  3.0878 0.00248 ** 
    ## direction:maingroup       4  0.0104  0.0026  0.0447 0.99621    
    ## aoi:direction:maingroup   8  0.1000  0.0125  0.2152 0.98803    
    ## Residuals               238 13.8255  0.0581                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The posthocs are easier to interpret, too. No difference between chin and eyes. It's the mouth vs. chin and the mouth vs. eyes contrasts that drive the differences.

``` r
group.anova.face3.posthoc <- TukeyHSD(group.anova.face3,'aoi',conf.level = 0.95) 
group.anova.face3.posthoc
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = percent ~ aoi * direction * maingroup, data = data.face3)
    ## 
    ## $aoi
    ##                   diff        lwr         upr     p adj
    ## mouth-eyes  0.37010631  0.2846193  0.45559329 0.0000000
    ## chin-eyes  -0.02410231 -0.1095893  0.06138467 0.7840376
    ## chin-mouth -0.39420862 -0.4784795 -0.30993773 0.0000000

Age of ASL & Hearing Status ANCOVA
----------------------------------

Now we're using AoASL as a covariate and putting in hearing as a factor. Let's visualize that...what we see here is that AoASL isn't having much of an effect, but being deaf or hearing does.

``` r
# Draw it
ggplot(data.face3, aes(x=aoasl,y=percent)) +
  geom_point(aes(color=direction,shape=hearing)) +
  geom_smooth(aes(color=direction,linetype=hearing),method="lm",se=FALSE) +
  facet_wrap("aoi")
```

    ## Warning: Removed 5 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 5 rows containing missing values (geom_point).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-27-1.png) and the ANCOVA itself...which gives us almost identical results as the `big5` stats. So maybe it's easier overall to just drop all AOIs except eye, mouth, chin when trying to look for AoA, group effects, etc? We can present summary stats overall for all AOIs, then when it gets down to the dirty stats work, we keep it simple and show ... that whatever we found.

``` r
continuous.anova.face3 <- aov(data=data.face3, percent ~ aoi * direction * hearing * aoasl)
anova(continuous.anova.face3)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                              Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## aoi                           2  8.8187  4.4094 77.0664 < 2.2e-16 ***
    ## direction                     1  0.0151  0.0151  0.2646  0.607418    
    ## hearing                       1  0.0186  0.0186  0.3255  0.568834    
    ## aoasl                         1  0.0034  0.0034  0.0603  0.806244    
    ## aoi:direction                 2  0.5104  0.2552  4.4602  0.012518 *  
    ## aoi:hearing                   2  0.6997  0.3498  6.1142  0.002565 ** 
    ## direction:hearing             1  0.0005  0.0005  0.0089  0.924727    
    ## aoi:aoasl                     2  0.1587  0.0794  1.3871  0.251765    
    ## direction:aoasl               1  0.0035  0.0035  0.0605  0.805849    
    ## hearing:aoasl                 1  0.0215  0.0215  0.3755  0.540617    
    ## aoi:direction:hearing         2  0.0056  0.0028  0.0488  0.952347    
    ## aoi:direction:aoasl           2  0.0018  0.0009  0.0156  0.984517    
    ## aoi:hearing:aoasl             2  0.5283  0.2641  4.6167  0.010764 *  
    ## direction:hearing:aoasl       1  0.0009  0.0009  0.0150  0.902481    
    ## aoi:direction:hearing:aoasl   2  0.0152  0.0076  0.1324  0.876019    
    ## Residuals                   244 13.9605  0.0572                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

We have to figure out what the itneractions mean. That's where simple linear models can be helpful here. Let me try it here. Okay, so the results are slightly different. ANOVAs in R default to Type I sums of squares, while regressions use more of a Type III sum of squares approach, I believe. But we can interpret the results here a bit more easily. Almost all the interactions have to do with being hearing vs. deaf, and there seems to be no effect of Age of ASL acquisition. So that's interesting.

``` r
lm.face3 <- lm(data=data.face3, percent ~ aoi * direction * aoasl * hearing)
summary(lm.face3)
```

    ## 
    ## Call:
    ## lm(formula = percent ~ aoi * direction * aoasl * hearing, data = data.face3)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.61587 -0.14844 -0.03784  0.15304  0.64269 
    ## 
    ## Coefficients:
    ##                                                   Estimate Std. Error
    ## (Intercept)                                      0.0931195  0.0696057
    ## aoimouth                                         0.5794713  0.0964970
    ## aoichin                                          0.1349878  0.0964970
    ## directionreversed                                0.0424817  0.1008157
    ## aoasl                                            0.0067500  0.0103190
    ## hearingHearing                                   0.6571568  0.3409805
    ## aoimouth:directionreversed                      -0.1771985  0.1382430
    ## aoichin:directionreversed                        0.0300791  0.1382430
    ## aoimouth:aoasl                                  -0.0021634  0.0144608
    ## aoichin:aoasl                                   -0.0196756  0.0144608
    ## directionreversed:aoasl                         -0.0049741  0.0149847
    ## aoimouth:hearingHearing                         -1.0627784  0.4818269
    ## aoichin:hearingHearing                          -1.1218006  0.4818269
    ## directionreversed:hearingHearing                -0.1335229  0.4973469
    ## aoasl:hearingHearing                            -0.0354791  0.0215605
    ## aoimouth:directionreversed:aoasl                 0.0071584  0.0207936
    ## aoichin:directionreversed:aoasl                  0.0001892  0.0207936
    ## aoimouth:directionreversed:hearingHearing        0.2699738  0.6922043
    ## aoichin:directionreversed:hearingHearing         0.1158865  0.6922043
    ## aoimouth:aoasl:hearingHearing                    0.0487191  0.0304279
    ## aoichin:aoasl:hearingHearing                     0.0683193  0.0304279
    ## directionreversed:aoasl:hearingHearing           0.0110424  0.0315900
    ## aoimouth:directionreversed:aoasl:hearingHearing -0.0212871  0.0438464
    ## aoichin:directionreversed:aoasl:hearingHearing  -0.0046483  0.0438464
    ##                                                 t value Pr(>|t|)    
    ## (Intercept)                                       1.338   0.1822    
    ## aoimouth                                          6.005 6.89e-09 ***
    ## aoichin                                           1.399   0.1631    
    ## directionreversed                                 0.421   0.6738    
    ## aoasl                                             0.654   0.5136    
    ## hearingHearing                                    1.927   0.0551 .  
    ## aoimouth:directionreversed                       -1.282   0.2011    
    ## aoichin:directionreversed                         0.218   0.8279    
    ## aoimouth:aoasl                                   -0.150   0.8812    
    ## aoichin:aoasl                                    -1.361   0.1749    
    ## directionreversed:aoasl                          -0.332   0.7402    
    ## aoimouth:hearingHearing                          -2.206   0.0283 *  
    ## aoichin:hearingHearing                           -2.328   0.0207 *  
    ## directionreversed:hearingHearing                 -0.268   0.7886    
    ## aoasl:hearingHearing                             -1.646   0.1011    
    ## aoimouth:directionreversed:aoasl                  0.344   0.7309    
    ## aoichin:directionreversed:aoasl                   0.009   0.9927    
    ## aoimouth:directionreversed:hearingHearing         0.390   0.6969    
    ## aoichin:directionreversed:hearingHearing          0.167   0.8672    
    ## aoimouth:aoasl:hearingHearing                     1.601   0.1106    
    ## aoichin:aoasl:hearingHearing                      2.245   0.0256 *  
    ## directionreversed:aoasl:hearingHearing            0.350   0.7270    
    ## aoimouth:directionreversed:aoasl:hearingHearing  -0.485   0.6278    
    ## aoichin:directionreversed:aoasl:hearingHearing   -0.106   0.9157    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2392 on 244 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.4362, Adjusted R-squared:  0.3831 
    ## F-statistic: 8.208 on 23 and 244 DF,  p-value: < 2.2e-16

Left/Right Analysis
===================

> Then, how about a section on Percent Looking for AOI’s examines whether there are side biases by doing an ANOVA with the entire Left vs Right side as 2 AOI levels, with Subject Groups, and Forward and Reversed. If there are side biases, hopefully it is for the later AoA groups, then this means they are being drawn to the hands more than native signers. It’s possible that this is driven by where the signer puts her dominant hand or by a hemispheric bias.

> Maybe even the native signers have a little side bias too for other reasons, but I doubt it. This is kind of the hand-gravity idea, because there is nothing ever in the sides but arms and hands.

> If nothing is significant with side biases, you still write all this up in a section in the paper and in the Discussion revisit that saying there were no side biases found.

Let's go for it. Creating the `data.lr` thing here. And again because we're still doing ANOVAs and not hierarchical linear models we need to bump `data.lr` up one level.

``` r
# Make LR df with reference levels
data.lr <- filter(data2,aoi == "left" | aoi == "right") %>%
  mutate(aoi = as.factor(aoi))
data.lr$maingroup <- relevel(data.lr$maingroup, ref="NativeDeaf")

data.lr.item <- data.lr # save item-level data for later

# Pull out and save subject info
data.lr.subjectinfo <- data.lr %>%
  select(-acc,-aoi,-percent,-video,-story) %>%
  distinct()

# Now collapse data.big5 to subject-level 
data.lr <- data.lr %>%
  group_by(participant,direction,aoi) %>%
  summarize(percent = mean(percent,na.rm=TRUE))
data.lr[data.lr=="NaN"] <- NA

# Join subject info with data.big5 that's now subject-level
data.lr <- left_join(data.lr,data.lr.subjectinfo, by=c("participant","direction"))
```

Problem is, I can already tell this dataset is rather sparse. There are 113 empty cells out of 182. Soooo. Let's give this a shot anyway but probably not a good idea? The graph below, I changed the colors so they map on left/right AOI, and each facet is direction. So we can directly compare L/R biases.

``` r
ggplot(data.lr) + 
  geom_boxplot(aes(x=maingroup,y=percent,fill=aoi)) +
  facet_wrap("direction") + 
  theme(axis.text.x=element_text(angle=45,hjust=1))
```

    ## Warning: Removed 113 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-31-1.png)

Let's try the group ANOVA and the AoASL ANCOVAs. Group ANOVA first...nothing significant here.

``` r
group.lranova <- aov(data=data.lr,percent ~ aoi * direction * maingroup)
anova(group.lranova)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                         Df   Sum Sq    Mean Sq F value Pr(>F)
    ## aoi                      1 0.000106 0.00010557  0.0520 0.8206
    ## direction                1 0.000046 0.00004597  0.0226 0.8810
    ## maingroup                4 0.009611 0.00240277  1.1827 0.3298
    ## aoi:direction            1 0.000023 0.00002272  0.0112 0.9162
    ## aoi:maingroup            4 0.006628 0.00165694  0.8156 0.5213
    ## direction:maingroup      4 0.002123 0.00053078  0.2613 0.9014
    ## aoi:direction:maingroup  3 0.002643 0.00088095  0.4336 0.7299
    ## Residuals               50 0.101581 0.00203161

But the ANCOVA here shows some almost significant effects of hearing, and significant AOI:Hearing and AOASL:Hearing interactions.

``` r
continuous.lranova <- aov(data=data.lr,percent ~ aoi * direction * aoasl * hearing)
anova(continuous.lranova)
```

    ## Analysis of Variance Table
    ## 
    ## Response: percent
    ##                             Df   Sum Sq   Mean Sq F value  Pr(>F)  
    ## aoi                          1 0.000106 0.0001056  0.0655 0.79902  
    ## direction                    1 0.000046 0.0000460  0.0285 0.86654  
    ## aoasl                        1 0.001119 0.0011187  0.6939 0.40857  
    ## hearing                      1 0.006440 0.0064397  3.9945 0.05079 .
    ## aoi:direction                1 0.000008 0.0000082  0.0051 0.94331  
    ## aoi:aoasl                    1 0.000094 0.0000944  0.0586 0.80969  
    ## direction:aoasl              1 0.000015 0.0000151  0.0094 0.92321  
    ## aoi:hearing                  1 0.008208 0.0082082  5.0916 0.02819 *
    ## direction:hearing            1 0.000193 0.0001931  0.1198 0.73062  
    ## aoasl:hearing                1 0.010984 0.0109836  6.8132 0.01174 *
    ## aoi:direction:aoasl          1 0.000434 0.0004341  0.2693 0.60597  
    ## aoi:direction:hearing        1 0.001466 0.0014663  0.9095 0.34457  
    ## aoi:aoasl:hearing            1 0.002789 0.0027894  1.7303 0.19404  
    ## direction:aoasl:hearing      1 0.002022 0.0020220  1.2543 0.26779  
    ## aoi:direction:aoasl:hearing  1 0.003393 0.0033927  2.1045 0.15276  
    ## Residuals                   53 0.085442 0.0016121                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

So that's interesting. Let's plot those out. Again, pay attention to the legend. And ahh, there's one huge outlier point with 0.30% (forward, right AOI, hearing person who learned ASL around age 12) which could be throwing off the entire stats too. Still, it's also interesting the deaf group has an increasing bias to the left the later they learn ASL. And I'm suspecting there is no general bias for hearing signers (once we fix that outlier).

``` r
ggplot(data.lr, aes(x=aoasl,y=percent)) +
  geom_point(aes(color=aoi,shape=hearing)) +
  geom_smooth(aes(color=aoi,linetype=hearing),method="lm",se=FALSE) +
  facet_wrap("direction")
```

    ## Warning: Removed 113 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 113 rows containing missing values (geom_point).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-34-1.png)

Assorted/older stuff pushed to the bottom
=========================================

> Then when you do a multiple regression analysis, which will be looking at whether gaze behavior can be used to predict accuracy on lexical recall, this will have hearing status, AoA, lexical recall accuracy …. For reversed and not forward? You can’t put both in. And a few of the AOI measures, maybe just one. Maybe a looking-ratio. Maybe a measure of scatter? I don't know. That's where viewing space comes in, and that's saved for later. If we end up saving this for later, that's fine.

This will go into a separate data notebook (04).

Let's jump straight to a big linear mixed model for the Big 5. We'll try both groups and regressing on AoA. Here are the ANOVA tables in order: 1. Linear model (no random terms) with MainGroups 1. Linear mixed model with MainGroups 1. Linear model (no random terms) with AoASL and Hearing 1. Linear mixed model with AoASL and Hearing

But that can be complicated because of so many possible interactions (groups x aois x direction x hearing) in the posthoc analyses. We'll try separating for direction. Because we think there is no difference among groups for forward, but there should be for reverse.

What if we use AoA as linear and then deaf/hearing
