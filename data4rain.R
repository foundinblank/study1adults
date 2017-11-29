eyeperf_2 <- fulldata %>%
  filter(eye_exclude == FALSE) %>%
  select(participant, maingroup, hearing, direction, acc, gist, eyes, mouth, neck, facechest) %>%
  group_by(maingroup, participant, direction) %>%
  mutate(gist = mean(gist, na.rm = TRUE),
         lex = mean(acc, na.rm = TRUE),
         eyes = mean(eyes, na.rm = TRUE),
         mouth = mean(mouth, na.rm = TRUE),
         neck = mean(neck, na.rm = TRUE),
         facechest = mean(facechest, na.rm = TRUE)) %>%
  ungroup() %>%
  select(maingroup, participant, hearing, direction, gist, lex, eyes, mouth, neck, facechest) %>%
  distinct() %>%
  gather(metric, value, gist:facechest) %>%
  unite(metricvalue, c(metric, direction), sep = "_") %>%
  spread(metricvalue, value) %>%
  select(participant, maingroup, hearing, gist_forward, gist_reversed, lex_forward, lex_reversed, eyes_forward, eyes_reversed,
         mouth_forward, mouth_reversed, neck_forward, neck_reversed, facechest_forward, facechest_reversed)

write_csv(eyeperf_2, "~/Desktop/eyegaze-performance.csv")
