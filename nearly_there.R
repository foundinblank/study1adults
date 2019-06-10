# building a crqa loop 
# Do averaged of fw bears

radius_fixed <- 34
embed_fixed <- 14
delay_fixed <- 117

target <- data_lists %>%
  filter(name == "Adam" & story == "bears")

eye_y_target <- data_lists %>%
  pull(eye_y) %>%
  pluck(1)

rhand_y_target <- data_lists %>%
  pull(rhand_y) %>%
  pluck(1)

test_radius <- list(seq(10,50))

run_crqa_rr <- function(x, y, z){
  results <- crqa(x, 
       y, 
       delay = 117, 
       embed = 14, 
       rescale = 2, 
       radius = z, 
       normalize = 2, 
       mindiagline = 2, 
       minvertline = 2, 
       tw = 0, 
       whiteline = FALSE, 
       recpt = FALSE, 
       side = 'both')
  return(results[['RR']])
}

radius_loop <- function(x, y, z){
  for(i in 1:length(z)){
    u = run_crqa(x, y, i)
  }
}


l <- list(list(eye_y_target), list(rhand_y_target), test_radius)

pmap_dbl(l, run_crqa_rr)

run_crqa(eye_y_target, rhand_y_target, 37)



bears <- data_lists %>%
  filter(story == 'bears')

looping <- bears %>%
  add_column(test_radius) %>%
  unnest(test_radius, .drop = F) %>%
  group_by(name) %>%
  mutate(rec_values = future_pmap_dbl(list(eye_y, rhand_y, test_radius), run_crqa_rr))
         
target_radius <- looping %>%
  select(name, test_radius, rec_values) %>%
  mutate(diff = abs(5 - rec_values)) %>%
  group_by(name) %>%
  filter(diff == min(diff)) %>%
  ungroup() %>%
  arrange(rec_values)

target_radius_to_join <- target_radius %>%
  select(name, test_radius) %>%
  rename(radius = test_radius)


run_crqa <- function(x, y, z){
  crqa(x, 
       y, 
       delay = 117, 
       embed = 14, 
       rescale = 2, 
       radius = z, 
       normalize = 2, 
       mindiagline = 2, 
       minvertline = 2, 
       tw = 0, 
       whiteline = FALSE, 
       recpt = FALSE, 
       side = 'both')
}

crqas <- bears %>%
  left_join(target_radius_to_join, by = "name") %>%
  mutate(rhand = future_pmap(list(rhand_y, eye_y, radius), run_crqa))

crqa_results <- crqas %>%
  mutate(rhand_rr = map_dbl(rhand, pluck, "RR"),
         rhand_det = map_dbl(rhand, pluck, "DET")) %>%
  select(name, maingroup, story, direction, radius, rhand_rr, rhand_det)

crqa_results %>%
  ggbetweenstats(x = maingroup, 
                 y = rhand_det,
                 pairwise.comparisons = TRUE,
                 pairwise.annotation = "p.value",
                 p.adjust.method = "holm")

model1 <- lmer(data = crqa_results, rhand_det ~ maingroup + (1|direction))
summary(model1)
