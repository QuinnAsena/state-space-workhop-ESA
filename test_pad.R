story_pollen_top5_count <- story_pollen_long %>%
  group_by(variablename) %>%
  mutate(total_count = sum(value)) %>%
  slice(1) %>%
  ungroup() %>%
  select(variablename, total_count) %>%
  arrange(desc(total_count)) %>%
  slice(1:5)

story_top5 <- story_pollen_long %>%
  filter(variablename %in% c(story_pollen_top5_count$variablename))

story_other <- story_pollen_long %>%
  filter(!variablename %in% c(story_pollen_top5_count$variablename)) %>%
  mutate(variablename = "other") %>%
  group_by(variablename, age) %>%
  summarise(value = sum(value))


story_top5_res <- bind_rows(story_top5, story_other) %>%
  mutate(variablename = factor(variablename, levels =  c("other", rev(story_pollen_top5_count$variablename))))



ggplot(story_top5_res, aes(x = age, y = value)) +
  geom_area(colour = "grey90") +
  geom_col() +
  scale_x_reverse(breaks = scales::breaks_pretty(n = 6)) +
  coord_flip() +
  # ylim(0, 0.5) +
  labs(y = "Pollen counts", x = "Time (ybp)") +
  facet_wrap(~variablename,
             nrow = 1) +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
  )


hardwood_names <- c("Carya", "Ostrya/Carpinus", "Acer", "Juglans", "Fraxinus")
target_taxa <- c("Ulmus", "Fagus grandifolia", "Quercus")

story_pollen_long_hardwood <- story_pollen_long %>%
  filter(variablename %in% hardwood_names) %>%
  mutate(variablename = "hardwood") %>%
  group_by(siteid, sitename,
           sampleid, variablename, units, age,
           agetype, depth, datasetid,
           long, lat) %>%
  summarise(value = sum(value), .groups='keep')

unique(story_pollen_long_hardwood$variablename)


story_pollen_long_targets <- story_pollen_long %>%
    filter(variablename %in% target_taxa)
unique(story_pollen_long_targets$variablename)


story_other_taxa <- story_pollen_long %>%
  filter(!variablename %in% c(hardwood_names, target_taxa)) %>%
  mutate(variablename = "other") %>%
  group_by(siteid, sitename,
           sampleid, variablename, units, age,
           agetype, depth, datasetid,
           long, lat) %>%
  summarise(value = sum(value), .groups='keep')


state_variables <- bind_rows(story_other_taxa, story_pollen_long_hardwood, story_pollen_long_targets) %>%
  mutate(variablename = factor(variablename, levels =  c("other", "hardwood", target_taxa)),
         value = replace_na(value, 0))


ggplot(state_variables, aes(x = age, y = value)) +
  geom_area(colour = "grey90") +
  geom_col() +
  scale_x_reverse(breaks = scales::breaks_pretty(n = 6)) +
  coord_flip() +
  # ylim(0, 0.5) +
  labs(y = "Pollen counts", x = "Time (ybp)") +
  facet_wrap(~variablename,
             nrow = 1) +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
  )


state_variables_wide <- state_variables %>%
    pivot_wider(id_cols = c(age, depth), names_from = variablename, values_from = value) %>%
    arrange(age)

max(state_variables_wide$age) / nrow(state_variables_wide)
diff(state_variables_wide$age)
range(diff(state_variables_wide$age))
sd(diff(state_variables_wide$age))


# Notice that there are many more observations of pollen
# than there are of charcoal
dim(state_variables_wide)
dim(story_char_wide)

# Clip covariate data to the extent of the state-variables
# Join the two by age so that we get a square matrix
story_join <- story_char_wide %>%
  filter(age <= max(state_variables_wide$age)) %>%
  left_join(state_variables_wide)

# Always double check dimentions before/after joining!
dim(story_join)
tail(story_join)



bin_width <- 50
bins <- cut(story_join$age,
            breaks = seq(from = min(story_join$age),
            to = max(story_join$age + bin_width),
            by = bin_width), include.lowest = TRUE, labels = FALSE)


story_binned <- bind_cols(bins = bins, story_join) %>%
  group_by(bins) %>%
  summarise(
    age = mean(age, na.rm = T),
    char_acc = mean(char_acc, na.rm = T),
    other = sum(other, na.rm = T),
    hardwood = sum(hardwood, na.rm = T),
    `Fagus grandifolia` = sum(`Fagus grandifolia`, na.rm = T),
    Ulmus = sum(Ulmus, na.rm = T),
    Quercus = sum(Quercus, na.rm = T)
  ) %>%
  arrange(desc(age))


story_pollen_matrix <- as.matrix(story_binned[ ,colnames(story_binned) %in% c("other", "hardwood", target_taxa)])
story_char_matrix <- as.matrix(story_binned[,3], drop = FALSE)
story_char_matrix_scaled <- scale(story_char_matrix)

sample_idx <- which(rowSums(story_pollen_matrix) != 0)
story_pollen_matrix[sample_idx, ]

library(multinomialTS)
# Y <- story_pollen_matrix
# X <- scale(story_char_matrix)

p <- ncol(story_char_matrix) + 1 # Number of independent variables plus intercept
n <- ncol(story_pollen_matrix)

# Set-up parameters
p <- ncol(story_char_matrix_scaled) + 1 # Number of independent variables plus intercept
n <- ncol(story_pollen_matrix) # number of taxa
V.fixed.glmm <- diag(n)
diag(V.fixed.glmm) <- NA
V.fixed.glmm[1] <- 1
# V.fixed.glmm <- diag(n)
# V.fixed.glmm <- matrix(NA, n, n)
# V.fixed.glmm[1] <- 1 # reference taxa/group [1,1] is set to 1
B.fixed.glmm <- matrix(c(rep(0,p),rep(NA, (n - 1) * p)), p, n) # reference taxa [,1] are set to 0
B.start.glmm <- matrix(c(rep(0,p),rep(.01, (n - 1) * p)), p, n) # reference taxa [,1] are set to 0


B.fixed.glmm <- matrix(c(rep(0,p),rep(NA, (n - 1) * p)), p, n)
B.start.glmm <- matrix(c(rep(0,p),rep(.01, (n - 1) * p)), p, n)

glmm_mod <- mnGLMM(Y = story_pollen_matrix[sample_idx, ],
                   X = story_char_matrix_scaled[sample_idx, ,drop = F],
                   B.start = B.start.glmm, B.fixed = B.fixed.glmm,
                   V.fixed = V.fixed)
summary(glmm_mod)

start_time <- Sys.time()
glmm_mod2 <- mnGLMM(Y = story_pollen_matrix[sample_idx, ],
                   X = story_char_matrix_scaled[sample_idx, ,drop = F],
                   B.start = B.start.glmm,
                   B.fixed = B.fixed.glmm,
                   V.fixed = diag(n))
end_time <- Sys.time()
end_time - start_time

B0.start <- glmm_mod$B[1, , drop = F]
B.start <- glmm_mod$B[2, , drop = F]

sigma.start <- glmm_mod$sigma

V.fixed = matrix(NA, n, n) # Covariance matrix of environmental variation in process eq
V.fixed[1] = 1

V.start = V.fixed
V.start <- glmm_mod$V
# V.start <- diag(diag(V.start))

B.fixed <- matrix(NA, p-1, n)
B.fixed[,1] <- 0
B0.fixed = matrix(c(0, rep(NA, n - 1)), nrow = 1, ncol = n)


C.start = .5 * diag(n)
C.fixed <- C.start
C.fixed[C.fixed != 0] <- NA


start_time <- Sys.time()
mnTS_mod <- mnTS(Y = story_pollen_matrix[sample_idx, ],
                 X = story_char_matrix_scaled, Tsample = sample_idx,
                 B0.start = B0.start.mnTS, B0.fixed = B0.fixed.mnTS,
                 B.start = B.start.mnTS, B.fixed = B.fixed.mnTS,
                 C.start = C.start.mnTS, C.fixed = C.fixed.mnTS,
                 V.start = V.start.mnTS, V.fixed = V.fixed.mnTS,
                 dispersion.fixed = 1, maxit.optim = 1e+06)
end_time <- Sys.time()
end_time - start_time



start_time <- Sys.time()
mnTS_mod_intx <- mnTS(Y = story_pollen_matrix[sample_idx, ],
                     X = story_char_matrix_scaled, Tsample = sample_idx,
                     B0.start = B0.start.mnTS, B0.fixed = B0.fixed.mnTS,
                     B.start = B.start.mnTS, B.fixed = B.fixed.mnTS,
                     C.start = C.start.int.mnTS, C.fixed = C.fixed.int.mnTS,
                     V.start = mnTS_mod$V, V.fixed = V.fixed.mnTS,
                     dispersion.fixed = 1, maxit.optim = 1e+06)
end_time <- Sys.time() 
end_time - start_time

start_time <- Sys.time()
mnTS_mod_int2 <- mnTS(Y = story_pollen_matrix[sample_idx, ],
                      X = story_char_matrix_scaled, Tsample = sample_idx,
                      B0.start = B0.start.mnTS, B0.fixed = B0.fixed.mnTS,
                      B.start = B.start.mnTS, B.fixed = B.fixed.mnTS,
                      C.start = C.start.int.mnTS, C.fixed = C.fixed.int.mnTS,
                      V.start = V.start.mnTS, V.fixed = V.fixed.mnTS,
                      dispersion.fixed = 1, maxit.optim = 1e+06)
end_time <- Sys.time() 
end_time - start_time

start_time <- Sys.time()
mnTS_mod_int3 <- mnTS(Y = story_pollen_matrix[sample_idx, ],
                      X = story_char_matrix_scaled, Tsample = sample_idx,
                      B0.start = mnTS_mod_int$B0, B0.fixed = B0.fixed.mnTS,
                      B.start = mnTS_mod_int$B, B.fixed = B.fixed.mnTS,
                      C.start = mnTS_mod_int$C, C.fixed = C.fixed.int.mnTS,
                      V.start = mnTS_mod_int$V, V.fixed = V.fixed.mnTS,
                      dispersion.fixed = 1, maxit.optim = 1e+06)
end_time <- Sys.time() 
end_time - start_time

start_time <- Sys.time()
mnTS_mod_int4 <- mnTS(Y = story_pollen_matrix[sample_idx, ],
                      X = story_char_matrix_scaled, Tsample = sample_idx,
                      B0.start = mnTS_mod_int2$B0, B0.fixed = B0.fixed.mnTS,
                      B.start = mnTS_mod_int2$B, B.fixed = B.fixed.mnTS,
                      C.start = mnTS_mod_int2$C, C.fixed = C.fixed.int.mnTS,
                      V.start = mnTS_mod_int2$V, V.fixed = V.fixed.mnTS,
                      dispersion.fixed = 1, maxit.optim = 1e+06)
end_time <- Sys.time() 
end_time - start_time

start_time <- Sys.time()
mnTS_mod_int4 <- mnTS(Y = story_pollen_matrix[sample_idx, ],
                      X = story_char_matrix_scaled, Tsample = sample_idx,
                      B0.start = mnTS_mod_int3$B0, B0.fixed = B0.fixed.mnTS,
                      B.start = mnTS_mod_int3$B, B.fixed = B.fixed.mnTS,
                      C.start = mnTS_mod_int3$C, C.fixed = C.fixed.int.mnTS,
                      V.start = mnTS_mod_int3$V, V.fixed = V.fixed.mnTS,
                      dispersion.fixed = 1, maxit.optim = 1e+06)
end_time <- Sys.time() 
end_time - start_time


start_time <- Sys.time()
mnTS_mod_int5 <- mnTS(Y = story_pollen_matrix[sample_idx, ],
                     X = story_char_matrix_scaled, Tsample = sample_idx,
                     B0.start = B0.start.mnTS, B0.fixed = B0.fixed.mnTS,
                     B.start = B.start.mnTS, B.fixed = B.fixed.mnTS,
                     C.start = C.start.int.mnTS, C.fixed = C.fixed.int.mnTS,
                     V.start = diag(n), V.fixed = V.fixed.mnTS,
                     dispersion.fixed = 1, maxit.optim = 1e+06)
end_time <- Sys.time() 
end_time - start_time