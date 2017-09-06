Eye Gaze Analysis (study1adults)
================
Adam Stone, PhD
09-06-2017

-   [Re-Initializing](#re-initializing)
-   [AOIs](#aois)
-   [Face AOIs](#face-aois)

Re-Initializing
===============

This assumes you've already done 01dataimportclean and so there'll be a nice new .csv file to re-import here. Also we gotta import all the libraries again. This shouldn't depend on anything we did for 02 Lexical Recall Analysis.

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
                        right = col_double()
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

*Why 4 AOIs on Face?* Emmorey et al. (2008) did this same setup. We generally know people fixate on the face across all conditions and langauge experiences, but **where** on the face is important for us to know. So these 4 AOIs.

*Why 4 AOIs for Torso?* At the same time, all papers, tend to just classify the body as "body" with no further breakdown, or two-part breakdown. We know there is a lot of different things happening in that area for signers, too, plus we're lookina at CHILDREN with different language experiences, so they may be interested in different things than adults. So just makes sense to break down body into different regions.

Face AOIs
=========

Let's visualize first of all.

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

    ## Warning: Removed 192 rows containing non-finite values (stat_boxplot).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png) Okay, right away I see some issues - I want to check for outliers but I'm not sure what could count as an outlier. All 4 stories are different lengths - a data point at 30 seconds would be fine for King Midas (0:37) but impossible for Red Riding Hood (0:18) so outliers need to be *relative* to the story length itself. Let's back up and do histograms for each story.

``` r
ggplot(data.face,aes(x=looking)) +
  geom_histogram(binwidth=1) +
  facet_wrap("story") +
  xlab("secs") +
  ggtitle("Face AOI sums for each story for each participant")
```

    ## Warning: Removed 192 rows containing non-finite values (stat_bin).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png) Loooks good but I see weird outliers for Cinderella and Red Riding Hood - those single data points are past the video length (and that's just the face AOIs!). Let's sum up *all* AOIs across each story for each participant...back to the big dataset, and we'll do histograms again.

``` r
data2 <- data %>%
  group_by(id,story) %>%
  mutate(total = sum(forehead,eyes,mouth,chin,upperchest,
                     midchest,lowerchest,belly,left,right,na.rm=TRUE))
ggplot(data2,aes(x=total)) +
  geom_histogram(binwidth=1) +
  facet_wrap("story") +
  xlab("secs") +
  ggtitle("Sum of ALL AOIs for each participant for each story")
```

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png) The tall bars are near the end of the story, right? So we see two issues: 1. Some barely watched the story at all. (Those are the ones with bars at or near zero). We should remove those. We need a rule for it. 1. A few people's AOI data has total seconds higher than the video itself! (Those are the ones with very short bars to the right of the very tall bars.) Those should be investigated, something went wrong in the data.

I'll highlight those rows that's for \#2 and send to Rain to look at.

But maybe a good way for diagnosing problem trials is to look at each AOI for each story, instead of sums of AOIs. Any outliers can be easily seen in the histograms. Let's reshape the data again and generate histograms.

``` r
data.reshape <- data2 %>% gather(aoi,looking,forehead:total)
ggplot(data.reshape,aes(x=looking)) +
  geom_histogram(binwidth=1) +
  facet_grid(aoi ~ story) +
  xlab("secs") +
  ggtitle("Looking times of each AOI for each participant for each story")
```

    ## Warning: Removed 881 rows containing non-finite values (stat_bin).

![](03eyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

Nohting really huge jumps out except Cinderella/Mouth and that's one of \#2's that I've already marked up for Rain. Maybe Red Riding Hood/Chin but that's also someone I've marked up.

Cool. Now I want to know how many people have really low looking times for each story. We can do this easily. Here's a table for how many have looking time sums that are less than 25% of the story, or less than 50% of the story.

``` r
# Split into 4 datasets (1 each story)
data.cinderella <- filter(data2,story == "Cinderella")
data.goldilocks <- filter(data2,story == "Goldilocks")
data.kingmidas <- filter(data2,story == "KingMidas")
data.redridinghood <- filter(data2,story== "RedRidingHood")

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
data2 <- bind_rows(data.cinderella,data.goldilocks,data.kingmidas,data.redridinghood)

# Calculations
lowlooking <- data2 %>%
  group_by(story) %>%
  summarize(lessthan25 = sum(!quarter),
            lessthan50 = sum(!half),
            total = sum(!quarter,quarter))
lowlooking
```

    ## # A tibble: 4 x 4
    ##           story lessthan25 lessthan50 total
    ##          <fctr>      <int>      <int> <int>
    ## 1    Cinderella          5          9    47
    ## 2    Goldilocks          6          6    47
    ## 3     KingMidas          4          7    47
    ## 4 RedRidingHood          5          7    47

``` r
lowlookingid <- filter(data2,quarter==FALSE) %>% 
  ungroup() %>%
  select(id,participant,hearing,videogroup,story,direction,total) %>%
  arrange(participant)
write.csv(lowlookingid, file="lessthan25.csv")
select(lowlookingid,-participant)
```

    ## # A tibble: 20 x 6
    ##       id hearing videogroup         story direction total
    ##    <int>  <fctr>     <fctr>        <fctr>    <fctr> <dbl>
    ##  1     8    Deaf    Group 2    Cinderella   forward  0.00
    ##  2     8    Deaf    Group 2    Goldilocks  reversed  0.00
    ##  3     8    Deaf    Group 2     KingMidas  reversed  0.00
    ##  4     8    Deaf    Group 2 RedRidingHood   forward  0.00
    ##  5    10    Deaf    Group 1    Cinderella  reversed  0.00
    ##  6    10    Deaf    Group 1    Goldilocks   forward  0.00
    ##  7    10    Deaf    Group 1     KingMidas   forward  0.00
    ##  8    10    Deaf    Group 1 RedRidingHood  reversed  0.00
    ##  9    32 Hearing    Group 2    Goldilocks  reversed  4.08
    ## 10    31 Hearing    Group 2    Cinderella   forward  4.73
    ## 11     6    Deaf    Group 1    Cinderella  reversed  0.00
    ## 12     6    Deaf    Group 1    Goldilocks   forward  0.00
    ## 13     6    Deaf    Group 1 RedRidingHood  reversed  3.80
    ## 14     5    Deaf    Group 1     KingMidas   forward  2.91
    ## 15     5    Deaf    Group 1 RedRidingHood  reversed  1.96
    ## 16    25    Deaf    Group 2    Goldilocks  reversed  4.62
    ## 17     7    Deaf    Group 1    Cinderella  reversed  0.81
    ## 18     7    Deaf    Group 1     KingMidas   forward  6.84
    ## 19    30 Hearing    Group 1    Goldilocks   forward  2.56
    ## 20    17    Deaf    Group 1 RedRidingHood  reversed  0.52

You’ll have to decide how to put the AOIs in an ANOVA. All of them together is too many. And you cannot put ALL the AOIs in. If they all sum to 100% (which they currently do), then the observations are not independent. Also, you can’t put AOIs that have near-zero values in with AOIs that have super high values, you’ll get whopping significance that is too obvious to reveal anything meaningful.

First, check the most important stats: Subject Groups, Video Condition, and some AOIs that are on the central body like: eyes, nose, mouth, neck, chest, below chest. You can combine some. If you see that no one ever looked at the forehead, you can dispense of that, and say that in the paper when you rationalize your AOIs used for stats. There might be a significant group main effect, OR a group main effect for reversed and not for forward, and furthermore, maybe only the late AoA groups show a reversal effect.

Then, how about a section on Percent Looking for AOI’s examines whether there are side biases by doing an ANOVA with the entire Left vs Right side as 2 AOI levels, with Subject Groups, and Forward and Reversed. If there are side biases, hopefully it is for the later AoA groups, then this means they are being drawn to the hands more than native signers. It’s possible that this is driven by where the signer puts her dominant hand or by a hemispheric bias.

Maybe even the native signers have a little side bias too for other reasons, but I doubt it. This is kind of the hand-gravity idea, because there is nothing ever in the sides but arms and hands.

If nothing is significant with side biases, you still write all this up in a section in the paper and in the Discussion revisit that saying there were no side biases found.

Then when you do a multiple regression analysis, which will be looking at whether gaze behavior can be used to predict accuracy on lexical recall, this will have hearing status, AoA, lexical recall accuracy …. For reversed and not forward? You can’t put both in. And a few of the AOI measures, maybe just one. Maybe a looking-ratio. Maybe a measure of scatter? I don't know. That's where viewing space comes in, and that's saved for later. If we end up saving this for later, that's fine.
