library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(zoo)
library(crqa)
library(furrr)
library(ggstatsplot)
library(lme4)

# The results I like come from lines 315-355

# Load All Participants' Eye Tracking Data --------------------------------

files <- list.files("../Adult Data/rawdata/rawdataXLSX", 
                             full.names = T) %>%
  set_names(.)

rawdata <- files %>%
  map_dfr(read_xlsx, .id = "source") %>%
  clean_names() %>%
  rename(x = gaze_point_x_mc_spx,
         y = gaze_point_y_mc_spx,
         language = language_value,
         name = participant_name,
         group = group_value) %>%
  select(source, name, gaze_point_index, x, y)

# Fix for stories that don't start at gaze_point_index = 1
rawdata <- rawdata %>%
  group_by(source, name) %>%
  arrange(source, name, gaze_point_index) %>%
  mutate(gaze_point_index = row_number()) %>%
  ungroup()

# Extract story and direction
rawdata <- rawdata %>%
  mutate(source = str_remove(source, "../Adult Data/rawdata/rawdataXLSX/"),
         source = str_remove(source, ".xlsx")) %>%
  mutate(direction = str_sub(source, -2, -1)) %>%
  mutate(story = str_remove(source, "fw"),
         story = str_remove(story, "rv")) %>%
  select(-source)

# Pull clean names (and handle Laura2 for Laura (missing stories))
cleannames <- read_csv("partnames.csv") %>%
  distinct() %>%
  rename(name = participant) %>%
  filter(name != "Laura2")

# Pull final group assignments
cleangroups <- read_csv("finaldataset.csv") %>%
  select(participant, maingroup) %>%
  rename(name = participant) %>%
  distinct()

# Join both to rawdata
data <- rawdata %>%
  left_join(cleannames, by = "name") %>%
  select(-name) %>%
  rename(name = new_participant) %>%
  filter(!is.na(name)) %>%
  left_join(cleangroups, by = "name")

# Exclude those who weren't included in previous analyses (find out why not)
# Also exclude 4 stories (Sarah & Josh) because theirs were too long 
# (Find out how we dealt with those before)
# And exclude more because they have no data...???
excluded <- read_csv("finaldataset.csv") %>%
  select(participant, story, direction, eye_exclude) %>%
  rename(name = participant) %>%
  mutate(story = tolower(story)) %>%
  mutate(story = case_when(
    story == "goldilocks" ~ "bears",
    story == "kingmidas" ~ "midas",
    TRUE ~ story
  )) %>%
  mutate(direction = case_when(
    direction == "forward" ~ "fw",
    direction == "reversed" ~ "rv"
  )) %>%
  # Excluded because too much data?
  mutate(eye_exclude = case_when(
    name == "Sara" & story == "cinderella" & direction == "rv" ~ TRUE,
    name == "Josh" & story == "redridinghood" & direction == "fw" ~ TRUE,
    name == "Sara" & story == "midas" & direction == "fw" ~ TRUE,
    name == "Josh" & story == "midas" & direction == "rv" ~ TRUE,
    TRUE ~ eye_exclude
  )) %>%
  # Excluded because they have no data?
  mutate(eye_exclude = case_when(
    name == "Amy" & story == "midas" & direction == "fw" ~ TRUE,
    name == "Amy" & story == "redridinghood" & direction == "rv" ~ TRUE,
    name == "Laura (missing stories)" & story == "midas" & direction == "fw" ~ TRUE,
    TRUE ~ eye_exclude
  )) %>%
  # Excluded because they still had too much data?
  mutate(eye_exclude = case_when(
    name == "Alicia" & story == "midas" ~ TRUE,
    name == "Alicia" & story == "redridinghood" ~ TRUE,
    TRUE ~ eye_exclude
  ))

data <- data %>% 
  left_join(excluded, by = c("name", "story", "direction")) %>%
  filter(!eye_exclude) %>%
  filter(!is.na(eye_exclude)) %>%
  select(-eye_exclude) %>%
  mutate_at(vars(name, maingroup, story, direction), as.factor)

# Counting
data %>% 
  distinct(name, maingroup) %>% 
  count(maingroup) %>% 
  rename(participants = n)

data %>%
  distinct(name, maingroup, story, direction) %>% 
  count(maingroup, story, direction) %>%
  spread(direction, n)


# Data Smoothing ----------------------------------------------------------
# First, fill in missing values with a straightforward fill()
# We're adding smoothing with a moving average window size - 5. (y_ma5). Good for removing noise. Then we'll eliminate 30 samples from the start and end (that's 0.25 seconds on both sides). 


bookend <- 30
max_end <- data %>% 
  group_by(story, maingroup) %>%
  summarise(max = max(gaze_point_index)) %>%
  group_by(story) %>%
  summarise(max_end = min(max) - bookend)

data_ma <- data %>%
  group_by(name, story, direction) %>%
  fill(y, .direction = "down") %>%
  fill(y, .direction = "up") %>%
  mutate(y_ma5 = rollmean(y, 5, fill = NA)) %>%
  ungroup() %>%
  left_join(max_end, by = "story") %>%
  filter(gaze_point_index > bookend & gaze_point_index < max_end) %>%
  mutate(gaze_point_index = gaze_point_index - bookend)

# data_ma %>%
#   filter(name == "Monica",
#          story == "bears") %>%
#   ungroup() %>%
#   gather(key = "metric", value = "y_position", c(y, y_ma5)) %>%
#   ggplot(aes(x = gaze_point_index, y = y_position, color = metric)) +
#   geom_line(alpha = 0.5) +
#   scale_y_reverse() +
#   ggtitle("Monica - see effect of smoothing with MA = 5")

  

# Get Right Hand Data -----------------------------------------------------
# At around line 600 we combine this with LEFT HAND DATA 
max_per_story <- data_ma %>% 
  group_by(story, name) %>%
  summarise(max_rows = n()) %>%
  group_by(story) %>%
  summarise(max = max(max_rows)) %>%
  spread(story, max)

rhand_files <- list.files("aoi_position",
                          pattern = "right",
                          full.names = T) %>%
  set_names(.)

rhand <- rhand_files %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(sec = col_double(),
                           x = col_double(),
                           y = col_double()
          )) %>%
  mutate(story = str_remove(source, "aoi_position/aoi_"),
         story = str_remove(story, "_right.csv")) %>%
  select(-source) %>%
  mutate(sec = floor(sec*120)) %>%
  mutate(sec = sec + 1) %>%
  add_row(story = "bears", sec = max_per_story$bears) %>%
  add_row(story = "redridinghood", sec = max_per_story$redridinghood) %>%
  add_row(story = "midas", sec = max_per_story$midas) %>%
  add_row(story = "cinderella", sec = max_per_story$cinderella) %>%
  arrange(story, sec) %>%
  group_by(story) %>%
  fill(x, y) %>%
  # Fix duplicate gaze_point_index 
  group_by(story, sec) %>%
  slice(1)

bears_rhand <- seq(1, max_per_story$bears) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(rhand, story == "bears"), by = "sec") %>%
  fill(story, x, y)
  
cinderella_rhand <- seq(1, max_per_story$cinderella) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(rhand, story == "cinderella"), by = "sec") %>%
  fill(story, x, y)

midas_rhand <- seq(1, max_per_story$midas) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(rhand, story == "midas"), by = "sec") %>%
  fill(story, x, y) 

redridinghood_rhand <- seq(1, max_per_story$redridinghood) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(rhand, story == "redridinghood"), by = "sec") %>%
  fill(story, x, y)

rhand_all <- bind_rows(bears_rhand,
                       cinderella_rhand,
                       midas_rhand,
                       redridinghood_rhand) %>%
  group_by(story) %>%
  summarise(rhand_y = list(y)) %>%
  mutate(rhand_len = lengths(rhand_y))



# Combine Both Datasets ---------------------------------------------------

# Combine our eye tracking data and right hand data
# We can check lengths to make sure both vectors match for each participant & story

data_lists <- data_ma %>%
  group_by(name, maingroup, story, direction) %>%
  summarise(eye_y = list(y_ma5)) %>%
  mutate(eye_len = lengths(eye_y)) %>%
  left_join(rhand_all, by = "story") %>%
  ungroup()

data_lists %>%
  mutate(samesame = rhand_len == eye_len) %>%
  filter(!samesame)


# Establish CRQA Parameters -----------------------------------------------
# We will work with forward stories only to establish their parameters, as that's the baseline. 
# We will establish 8 sets of parameters, one for each story and direction. 
# Note - I re-ran with moving average = 10 and the parameters barely changed so if we want to experiment with more smoothed data, we dont' need to change the parameters for now

# mlpar <- list(lgM = 120, 
#               radiusspan = 100, 
#               radiussample = 10, 
#               normalize = 2, 
#               rescale = 2, 
#               mindiagline = 2, 
#               minvertline = 2, 
#               tw = 0, 
#               whiteline = FALSE, 
#               recpt = FALSE, 
#               fnnpercent = 10, 
#               typeami = "maxlag")

# We might have issues so let's go through each of the 8 batches one by one
# fw bears - (34, 14, 117)
# fw rrh - (23, 10, 120) with filter(name != "Cami")
# fw cinderella - (28, 14, 110)
# fw midas - (20, 10, 120) - took a long time
# rv bears - (19, 14, 117)
# rv rrh - (25, 9, 120) with filter(name != "Allison" & name != "Rebecca")
# rv cinderella - (32, 13, 104) with filter(name != "Valerie" & name != "Jesse")
# rv midas - (18, 10, 120)

# I use this to help catch when specific people throw phase-space errors
# my_optimize_params <- safely(optimizeParam)

# plan(multiprocess)
# optimize_params <- data_lists %>%
#   filter(direction == "fw") %>%
#   filter(story == "bears") %>%
#   mutate(params = future_map2(eye_y, rhand_y, ~ optimizeParam(.x, .y, mlpar)))
# 
# output_params <- optimize_params %>%
#   group_by(name) %>%
#   mutate(r = pluck(params, 1, 1),
#          dim = pluck(params, 1, 2),
#          delay = pluck(params, 1, 3))
# 
# param_means <- output_params %>%
#   ungroup() %>%
#   summarise(r_mean = ceiling(mean(r)),
#             dim_mean = ceiling(mean(dim)),
#             delay_mean = ceiling(mean(delay)))
# 
# param_means

# Collect all our parameters (yes, it's manual, deal with it.)
story_parameters <- tribble(
  ~story, ~direction, ~r, ~embeddim, ~delay,
  "bears", "fw", 34, 14, 117,
  "redridinghood", "fw", 23, 10, 120,
  "cinderella", "fw", 28, 14, 110, 
  "midas", "fw", 20, 10, 120,
  "bears", "rv", 19, 14, 117, 
  "redridinghood", "rv", 25, 9, 120,
  "cinderella", "rv", 32, 13, 104,
  "midas", "rv", 18, 10, 120
)

# There are many ways we could do this:
# 1. One parameter for all 8 videos
# 2. One averaged forward parameter for all 8 videos
# 3. Use story's forward parameter for each story, in both fw/rv directions
# 4. Use each story and each direction's parameter for that story/direction
# 5. One averaged reversed parameter for all videos (hmm?)

# I am feeling #3 is best. But I think the CRQA authors would think #4. 



# Optimizing Radius  ------------------------------------------------------
# Per Wallot & Leonardi, we're going to run multiple CRQAs on each participant using 
# the averaged CRQA parameters for embeddim & delay, fixing the target
# %REC (that's RR) at 5.0% and then optimizing the radius. 
# Then we'll run CRQA again using that radius. 

test_radius <- list(seq(5,60))

run_crqa_return_rr <- function(a, b, c, d, e){
  results <- crqa(a, 
                  b, 
                  delay = d, 
                  embed = c, 
                  rescale = 2, 
                  radius = e, 
                  normalize = 2, 
                  mindiagline = 2, 
                  minvertline = 2, 
                  tw = 0, 
                  whiteline = FALSE, 
                  recpt = FALSE, 
                  side = 'both')
  return(results[['RR']])
}

plan(multiprocess)
start_time <- Sys.time()
optimizing_radius <- data_lists %>%
  left_join(story_parameters, by = c("story", "direction")) %>%
  select(-r) %>%
  add_column(test_radius) %>%
  unnest(test_radius, .drop = F) %>%
  group_by(name, story, direction) %>%
  mutate(rr_values = future_pmap_dbl(list(eye_y,
                                          rhand_y,
                                          embeddim,
                                          delay,
                                          test_radius), run_crqa_return_rr))
end_time <- Sys.time()
end_time - start_time
# Time difference of 2.501935 hours

saveRDS(optimizing_radius, "crqa_rhand.RDS")

# optimizing_radius <- readRDS("crqa_rhand.RDS")
target_radius <- optimizing_radius %>%
  select(name, story, direction, test_radius, rr_values) %>%
  mutate(diff = abs(5 - rr_values)) %>%
  group_by(name, story, direction) %>%
  filter(diff == min(diff)) %>%
  ungroup() %>%
  select(name, story, direction, test_radius)

# checking normality 
target_radius %>% ggplot(aes(x = test_radius)) + geom_density()
car::qqPlot(target_radius$test_radius)

# CRQA on All Stories All Directions --------------------------------------

data_lists_optimized <- data_lists %>%
  left_join(story_parameters, by = c("story", "direction")) %>%
  select(-r) %>%
  left_join(target_radius, by = c("name", "story", "direction")) %>%
  rename(optimized_r = test_radius)

run_crqa <- function(a, b, c, d, e){
  crqa(a, 
       b, 
       radius = c, 
       embed = d, 
       delay = e, 
       rescale = 2, 
       normalize = 2, 
       mindiagline = 2, 
       minvertline = 2, 
       tw = 0, 
       whiteline = FALSE, 
       recpt = FALSE, 
       side = 'both')
}

crqa_data <- data_lists_optimized %>%
  mutate(rhand = future_pmap(list(eye_y, rhand_y, optimized_r, embeddim, delay),
                             run_crqa))

crqa_results <- crqa_data %>%
  mutate(rhand_rr = map_dbl(rhand, pluck, "RR"),
         rhand_det = map_dbl(rhand, pluck, "DET"),
         rhand_nrline = map_dbl(rhand, pluck, "NRLINE"),
         rhand_maxl = map_dbl(rhand, pluck, "maxL"),
         rhand_l = map_dbl(rhand, pluck, "L"),
         rhand_entr = map_dbl(rhand, pluck, "ENTR"),
         rhand_r_ENTR = map_dbl(rhand, pluck, "rENTR"),
         rhand_lam = map_dbl(rhand, pluck, "LAM"),
         rhand_tt = map_dbl(rhand, pluck, "TT")) %>%
  select(name, maingroup, story, direction, embeddim, delay, optimized_r, rhand_rr:rhand_tt)

write_csv(crqa_results, "crqa_results.csv")
crqa_results <- read_csv("crqa_results.csv")

plot(crqa_results[,5:16])

library(lmerTest)
m1 <- lmer(data = crqa_results, optimized_r ~ maingroup * direction + (1|story) + (1|name))
summary(m1)

crqa_results %>%
  ggbetweenstats(x = maingroup, 
                 y = optimized_r,
                 pairwise.comparisons = TRUE,
                 pairwise.annotation = "p.value",
                 p.adjust.method = "holm")

crqa_results %>%
  grouped_ggbetweenstats(x = maingroup, 
                         y = optimized_r,
                         grouping.var = direction,
                         pairwise.comparisons = TRUE,
                         pairwise.annotation = "p.value",
                         p.adjust.method = "holm")



# CRQA on All Stories -----------------------------------------------------

# Now let's run CRQA on everything! Eee! 
# We'll do #1 first. 
# For #2 just add filter(story == "fw") %>% to our_params thing

our_params <- story_parameters %>%
  summarise(r = ceiling(mean(r)),
            embeddim = max(embeddim),
            delay = ceiling(mean(delay)))

run_crqa <- function(x, y){
  crqa(x, 
       y, 
       delay = our_params$delay, 
       embed = our_params$embeddim, 
       rescale = 2, 
       radius = our_params$r, 
       normalize = 2, 
       mindiagline = 2, 
       minvertline = 2, 
       tw = 0, 
       whiteline = FALSE, 
       recpt = FALSE, 
       side = 'both')
}

crqa_data <- data_lists %>%
  mutate(rhand = future_map2(rhand_y, eye_y, run_crqa))

crqa_results <- crqa_data %>%
  mutate(rhand_rr = map_dbl(rhand, pluck, "RR"),
         rhand_det = map_dbl(rhand, pluck, "DET")) %>%
  select(name, maingroup, story, direction, rhand_rr, rhand_det)

crqa_results %>%
  ggbetweenstats(x = maingroup, 
                 y = rhand_rr,
                 pairwise.comparisons = TRUE,
                 pairwise.annotation = "p.value",
                 p.adjust.method = "holm")

m1 <- lm(data = crqa_results, rhand_det ~ maingroup)
summary(m1)
m1 <- lmer(data = crqa_results, rhand_det ~ maingroup + (1|story) + (1|name))
summary(m1)

# This is #3

our_params <- story_parameters %>%
  filter(direction == "fw") %>%
  group_by(story) %>%
  summarise(r = ceiling(mean(r)),
            embeddim = max(embeddim),
            delay = ceiling(mean(delay)))


current_story <- "midas"

run_crqa <- function(x, y){
  crqa(x, 
       y, 
       delay = filter(our_params, story == current_story)$delay, 
       embed = filter(our_params, story == current_story)$embeddim, 
       rescale = 2, 
       radius = filter(our_params, story == current_story)$r, 
       normalize = 2, 
       mindiagline = 2, 
       minvertline = 2, 
       tw = 0, 
       whiteline = FALSE, 
       recpt = FALSE, 
       side = 'both')
  }

crqa_data <- data_lists %>%
  filter(story == current_story) %>%
  mutate(rhand = future_map2(eye_y, rhand_y, run_crqa))

# Change name of assigned df
crqa_results_midas <- crqa_data %>%
  mutate(rhand_rr = map_dbl(rhand, pluck, "RR"),
         rhand_det = map_dbl(rhand, pluck, "DET")) %>%
  select(name, maingroup, story, direction, rhand_rr, rhand_det)

crqa_results <- bind_rows(crqa_results_bears,
                          crqa_results_redridinghood,
                          crqa_results_cinderella,
                          crqa_results_midas)


# Visualization and testing...
crqa_results %>%
  group_by(maingroup) %>%
  summarise(rr = mean(rhand_rr),
            det = mean(rhand_det, na.rm = T))

crqa_results %>%
  ggplot(aes(x = maingroup, y = rhand_rr, color = maingroup, fill = maingroup)) +
  geom_boxplot()

crqa_results %>%
  ggbetweenstats(x = maingroup, 
                 y = rhand_det,
                 pairwise.comparisons = TRUE,
                 pairwise.annotation = "p.value",
                 p.adjust.method = "holm")

crqa_results %>%
  grouped_ggbetweenstats(x = maingroup, 
                         y = rhand_det,
                         grouping.var = story,
                         pairwise.comparisons = TRUE,
                         pairwise.annotation = "p.value",
                         p.adjust.method = "holm")

m1 <- lmer(data = crqa_results, rhand_det ~ maingroup + (1|story) + (1|name))
summary(m1)


# I'm gonna try #4 - using each story/direction's parameter for that story & direction
run_crqa_per_story <- function(a, b, c, d, e){
  crqa(ts1 = a, 
       ts2 = b, 
       delay = c, 
       embed = d, 
       rescale = 2, 
       radius = e, 
       normalize = 2, 
       mindiagline = 2, 
       minvertline = 2, 
       tw = 0, 
       whiteline = FALSE, 
       recpt = FALSE, 
       side = 'both')
}

data_lists_with_params <- data_lists %>%
  left_join(story_parameters, by = c("story", "direction"))

plan(multiprocess)
crqa_results_per_video <- data_lists_with_params %>%
  mutate(rhand = future_pmap(list(eye_y, 
                                  rhand_y, 
                                  delay, 
                                  embeddim, 
                                  r), 
                             run_crqa_per_story))

crqa_results_per_video <- crqa_results_per_video %>%
  mutate(rhand_rr = map_dbl(rhand, pluck, "RR"),
         rhand_det = map_dbl(rhand, pluck, "DET")) %>%
  select(-eye_y, -rhand_y, -rhand)

crqa_results_per_video %>%
  ggplot(aes(x = rhand_rr)) +
  geom_histogram(binwidth = 0.5) +
  geom_vline(xintercept = 1) +
  geom_vline(xintercept = 5) +
  facet_grid(story ~ direction)

m2 <- lmer(data = crqa_results_per_video, rhand_det ~ maingroup * direction + (1|story) + (1|name))
summary(m2)



# NEW STUFF AS OF 17 August
# Get Right Hand & Left Hand Data -----------------------------------------------------
max_per_story <- data_ma %>% 
  group_by(story, name) %>%
  summarise(max_rows = n()) %>%
  group_by(story) %>%
  summarise(max = max(max_rows)) %>%
  spread(story, max)

rhand_files <- list.files("aoi_position",
                          pattern = "right",
                          full.names = T) %>%
  set_names(.)

rhand <- rhand_files %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(sec = col_double(),
                           x = col_double(),
                           y = col_double()
          )) %>%
  mutate(story = str_remove(source, "aoi_position/aoi_"),
         story = str_remove(story, "_right.csv")) %>%
  select(-source) %>%
  mutate(sec = floor(sec*120)) %>%
  mutate(sec = sec + 1) %>%
  add_row(story = "bears", sec = max_per_story$bears) %>%
  add_row(story = "redridinghood", sec = max_per_story$redridinghood) %>%
  add_row(story = "midas", sec = max_per_story$midas) %>%
  add_row(story = "cinderella", sec = max_per_story$cinderella) %>%
  arrange(story, sec) %>%
  group_by(story) %>%
  fill(x, y) %>%
  # Fix duplicate gaze_point_index 
  group_by(story, sec) %>%
  slice(1)

bears_rhand <- seq(1, max_per_story$bears) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(rhand, story == "bears"), by = "sec") %>%
  fill(story, x, y)

cinderella_rhand <- seq(1, max_per_story$cinderella) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(rhand, story == "cinderella"), by = "sec") %>%
  fill(story, x, y)

midas_rhand <- seq(1, max_per_story$midas) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(rhand, story == "midas"), by = "sec") %>%
  fill(story, x, y) 

redridinghood_rhand <- seq(1, max_per_story$redridinghood) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(rhand, story == "redridinghood"), by = "sec") %>%
  fill(story, x, y)

rhand_all <- bind_rows(bears_rhand,
                       cinderella_rhand,
                       midas_rhand,
                       redridinghood_rhand) %>%
  group_by(story) %>%
  summarise(rhand_y = list(y)) %>%
  mutate(rhand_len = lengths(rhand_y))



# Combine Both Datasets ---------------------------------------------------

# Combine our eye tracking data and right hand data
# We can check lengths to make sure both vectors match for each participant & story

data_lists <- data_ma %>%
  group_by(name, maingroup, story, direction) %>%
  summarise(eye_y = list(y_ma5)) %>%
  mutate(eye_len = lengths(eye_y)) %>%
  left_join(rhand_all, by = "story") %>%
  ungroup()

data_lists %>%
  mutate(samesame = rhand_len == eye_len) %>%
  filter(!samesame)

# Now get left hand data

max_per_story <- data_ma %>% 
  group_by(story, name) %>%
  summarise(max_rows = n()) %>%
  group_by(story) %>%
  summarise(max = max(max_rows)) %>%
  spread(story, max)

lhand_files <- list.files("aoi_position",
                          pattern = "left",
                          full.names = T) %>%
  set_names(.)

lhand <- lhand_files %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(sec = col_double(),
                           x = col_double(),
                           y = col_double()
          )) %>%
  mutate(story = str_remove(source, "aoi_position/aoi_"),
         story = str_remove(story, "_left.csv")) %>%
  select(-source) %>%
  mutate(sec = floor(sec*120)) %>%
  mutate(sec = sec + 1) %>%
  add_row(story = "bears", sec = max_per_story$bears) %>%
  add_row(story = "redridinghood", sec = max_per_story$redridinghood) %>%
  add_row(story = "midas", sec = max_per_story$midas) %>%
  add_row(story = "cinderella", sec = max_per_story$cinderella) %>%
  arrange(story, sec) %>%
  group_by(story) %>%
  fill(x, y) %>%
  # Fix duplicate gaze_point_index 
  group_by(story, sec) %>%
  slice(1)

bears_lhand <- seq(1, max_per_story$bears) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(lhand, story == "bears"), by = "sec") %>%
  fill(story, x, y)

cinderella_lhand <- seq(1, max_per_story$cinderella) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(lhand, story == "cinderella"), by = "sec") %>%
  fill(story, x, y)

midas_lhand <- seq(1, max_per_story$midas) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(lhand, story == "midas"), by = "sec") %>%
  fill(story, x, y) 

redridinghood_lhand <- seq(1, max_per_story$redridinghood) %>%
  enframe(name = NULL, value = 'sec') %>%
  left_join(filter(lhand, story == "redridinghood"), by = "sec") %>%
  fill(story, x, y)

lhand_all <- bind_rows(bears_lhand,
                       cinderella_lhand,
                       midas_lhand,
                       redridinghood_lhand) %>%
  group_by(story) %>%
  summarise(lhand_y = list(y)) %>%
  mutate(lhand_len = lengths(lhand_y))



# Combine Both Datasets ---------------------------------------------------

# Add left hand data to data_lists
data_lists <- data_lists %>%
  left_join(lhand_all, by = "story")

data_lists %>%
  mutate(samesame = lhand_len == eye_len) %>%
  filter(!samesame)

# Run CRQA 
# First, get story_parameters from above 
# And get target_radius from above too

# So really, we're using the same target radius found via rhand for lhand too 
data_lists_optimized <- data_lists %>%
  left_join(story_parameters, by = c("story", "direction")) %>%
  select(-r) %>%
  left_join(target_radius, by = c("name", "story", "direction")) %>%
  rename(optimized_r = test_radius)

run_crqa <- function(a, b, c, d, e){
  crqa(a, 
       b, 
       radius = c, 
       embed = d, 
       delay = e, 
       rescale = 2, 
       normalize = 2, 
       mindiagline = 2, 
       minvertline = 2, 
       tw = 0, 
       whiteline = FALSE, 
       recpt = FALSE, 
       side = 'both')
}

plan(multiprocess)
crqa_data <- data_lists_optimized %>%
  mutate(rhand = future_pmap(list(eye_y, rhand_y, optimized_r, embeddim, delay),
                             run_crqa)) %>%
  mutate(lhand = future_pmap(list(eye_y, lhand_y, optimized_r, embeddim, delay),
                             run_crqa))


crqa_results <- crqa_data %>%
  mutate(rhand_rr = map_dbl(rhand, pluck, "RR"),
         rhand_det = map_dbl(rhand, pluck, "DET"),
         rhand_nrline = map_dbl(rhand, pluck, "NRLINE"),
         rhand_maxl = map_dbl(rhand, pluck, "maxL"),
         rhand_l = map_dbl(rhand, pluck, "L"),
         rhand_entr = map_dbl(rhand, pluck, "ENTR"),
         rhand_r_ENTR = map_dbl(rhand, pluck, "rENTR"),
         rhand_lam = map_dbl(rhand, pluck, "LAM"),
         rhand_tt = map_dbl(rhand, pluck, "TT")) %>%
  mutate(lhand_rr = map_dbl(lhand, pluck, "RR"),
         lhand_det = map_dbl(lhand, pluck, "DET"),
         lhand_nrline = map_dbl(lhand, pluck, "NRLINE"),
         lhand_maxl = map_dbl(lhand, pluck, "maxL"),
         lhand_l = map_dbl(lhand, pluck, "L"),
         lhand_entr = map_dbl(lhand, pluck, "ENTR"),
         lhand_r_ENTR = map_dbl(lhand, pluck, "rENTR"),
         lhand_lam = map_dbl(lhand, pluck, "LAM"),
         lhand_tt = map_dbl(lhand, pluck, "TT")) %>%
  select(name, maingroup, story, direction, embeddim, delay, optimized_r, rhand_rr:lhand_tt)

# Making sure we got the same results again for rhand
# old_results <- read_csv("crqa_results.csv")
# head(old_results$rhand_rr)
# head(crqa_results$rhand_rr)

#write_csv(crqa_results, "crqa_results_both_hands.csv")


library(lmerTest)
m1 <- lmer(data = crqa_results, optimized_r ~ maingroup * direction + (1|story) + (1|name))
summary(m1)

crqa_results %>%
  ggbetweenstats(x = maingroup, 
                 y = optimized_r,
                 pairwise.comparisons = TRUE,
                 pairwise.annotation = "p.value",
                 p.adjust.method = "holm")

crqa_results %>%
  grouped_ggbetweenstats(x = maingroup, 
                         y = optimized_r,
                         grouping.var = direction,
                         pairwise.comparisons = TRUE,
                         pairwise.annotation = "p.value",
                         p.adjust.method = "holm")

gathered <- crqa_results %>%
  select(name, maingroup, story, direction, rhand_det, lhand_det) %>%
  rename(rhand = rhand_det,
         lhand = lhand_det) %>%
  gather(hand, det, rhand:lhand)

m1 <- lmer(data = gathered, det ~ maingroup * direction * hand + (1|story) + (1|name))
summary(m1)
