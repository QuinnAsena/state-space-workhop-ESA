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
Tsample <- which(rowSums(story_pollen_matrix) == 0)


library(MultinomialStateSpace)
source("../MultinomialStateSpace/cpp_testing_groud/simulate_func_TEMP.R")
library(Rcpp)
library(RcppArmadillo)
library(minqa)
sourceCpp("../MultinomialStateSpace/R/source_multinomialSS.cpp")


X <- story_char_matrix
Y <- story_pollen_matrix
p <- ncol(X) + 1 # Number of independent variables plus intercept
n <- ncol(Y)

V.fixed = diag(n) # Covariance matrix of environmental variation in process eq
B.fixed <- matrix(c(rep(0,p),rep(NA, (n - 1) * p)), p, n)
B.start <- matrix(c(rep(0,p),rep(.01, (n - 1) * p)), p, n)

glmm_mod <- multinomialGLMM(Y = Y, X = X, B.start = B.start, B.fixed = B.fixed,
                            V.fixed = V.fixed)
summary(glmm_mod)

B0.start <- glmm_mod$B[1, , drop = F]
B.start <- glmm_mod$B[2, , drop = F]

sigma.start <- glmm_mod$sigma

V.fixed = matrix(NA, n, n) # Covariance matrix of environmental variation in process eq
V.fixed[1] = 1

V.start = V.fixed
V.start <- glmm_mod$V
# V.start <- diag(diag(V.start))

B.fixed <- matrix(NA, ncol(X), n)
B.fixed[,1] <- 0
B0.fixed = matrix(c(0, rep(NA, n - 1)), nrow = 1, ncol = n)


C.start = .5 * diag(n)
C.fixed <- C.start
C.fixed[C.fixed != 0] <- NA


ss_mod0 <- multinomialSS_cpp(Y = Y, X = X, Tsample = Tsample, B0.start = B0.start, B.start = B.start,
                  C.start = C.start, C.fixed = C.fixed, B0.fixed = B0.fixed,
                  V.fixed = V.fixed, V.start = V.start,
                  B.fixed = B.fixed, dispersion.fixed = 1, maxit.optim = 1e+06)
