# Run to line 300

monica_y <- monica %>% pull(y_ma5)
rhand_y <- rhand_expanded %>% pull(y)

monica_new <- monica_y[121:2400]
rhand_new <- rhand_y[121:2400]

mlpar <- list(lgM = 120, 
             radiusspan = 100, 
             radiussample = 10, 
             normalize = 2, 
             rescale = 2, 
             mindiagline = 2, 
             minvertline = 2, 
             tw = 0, 
             whiteline = FALSE, 
             recpt = FALSE, 
             fnnpercent = 10, 
             typeami = "maxlag")

lists <- data_ma %>%
  group_by(name) %>%
  fill(y_ma5, .direction = "down") %>%
  fill(y_ma5, .direction = "up") %>%
#  slice(121:(n()-120)) %>%
  summarise(y = list(y_ma5))


library(furrr)
plan(multiprocess)

optimize_params <- lists %>%
  mutate(params = future_map(y, ~ optimizeParam(.x, rhand_y, mlpar)))

output_params <- optimize_params %>%
  group_by(name) %>%
  mutate(r = pluck(params, 1, 1),
         dim = pluck(params, 1, 2),
         delay = pluck(params, 1, 3))

param_means <- output_params %>%
  ungroup() %>%
  summarise(r_mean = ceiling(mean(r)),
            dim_mean = ceiling(mean(dim)),
            delay_mean = ceiling(mean(delay)))

crqa_data <- lists %>%
  group_by(name) %>%
  mutate(rhand = future_map(y, ~ crqa(.x, rhand_y, 
                                      delay = param_means$delay_mean, 
                                      embed = param_means$dim_mean, 
                                      rescale = 2, 
                                      radius = param_means$r_mean+2, 
                                      normalize = 2, 
                                      mindiagline = 2, 
                                      minvertline = 2, 
                                      tw = 0, 
                                      whiteline = FALSE, 
                                      recpt = FALSE, 
                                      side = 'both')))

write_rds(crqa_data, "~/Desktop/crqa.rds")
crqa_results <- crqa_data %>%
  mutate(rhand_rr = map_dbl(rhand, pluck, "RR"),
         rhand_det = map_dbl(rhand, pluck, "DET")) %>%
  left_join(participants) %>%
  select(name, maingroup, rhand_rr, rhand_det)

crqa_results %>%
  ggplot(aes(x = maingroup, y = rhand_rr, color = maingroup)) +
  geom_jitter()

crqa_results %>%
  ggplot(aes(x = rhand_rr)) + geom_histogram()


params_and_output <- crqa_results %>%
  left_join(output_params, by = "name") %>%
  select(-y, -params)

params_and_output %>%
  ggplot(aes(x = rhand_det, y = rhand_rr, label = name)) +
  geom_point()

library(GGally)
ggpairs(params_and_output[,3:7])

# fitler out one extreme value...
params_and_output %>%
  ungroup() %>%
  filter(rhand_rr < 60) %>%
  ggstatsplot::ggbetweenstats(x = maingroup, y = rhand_rr)



that1 <- optimizeParam(monica_y, rhand_y, mlpar)
that1
rqaAns <- crqa(monica_y, rhand_y, radius = 33, embed = 14, delay = 117, rescale = 2, normalize = 2, mindiagline = 2, minvertline = 2, tw = 0, whiteline = FALSE, recpt = FALSE, side = "both")
rqaMetrics <- c(rqaAns[1], rqaAns[2], rqaAns[5]); rqaMetrics
mRP <- data.table::melt(as.matrix(rqaAns$RP), varnames=c("TimeV1", "TimeV2"), value.name="Recurrence")

binary <- ggplot(mRP, aes(x = TimeV1, y = TimeV2, fill = Recurrence)) + 
  geom_raster() + 
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  ggtitle("Binary Cross Recurrence Plot") +
  scale_fill_manual(values = c("#9999ff","#000066"), 
                    breaks = c(TRUE, FALSE)) +
  theme(legend.position = "none", plot.title = element_text(size = 16, face = "bold"))
binary


library(tseriesChaos)
lists %>%
  group_by(name) %>%
  mutate(ami = map(y, ~ mutual(.x, lag.max = 240, plot = F))) %>%
  mutate(that = map_dbl(ami, min))


OKAY BREAKTHROUGH 