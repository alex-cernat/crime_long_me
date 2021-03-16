#############################################################
#
# Project looking estimating measurement error in crime data using
# longitudinal models
#
# We start by using data from Barcelona 2015-2019
#
#  TO DO
#  - corraltion and means by 2 years
#  - sensitivity analysis multi-item qs 2 items at a time
#  - try multi-item qs with police
#  - mtmm 2 methods at a time
#  - do qs and mtmm with weight 2
#  - do mtmm with the police as well
#  - all models w2
#
#
#
#############################################################

# clear working space
rm(list = ls())

# Admin -------------------------------------------------------------------

# folders for installed packages (for Alex work machine)
# .libPaths(c(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox (The University of Manchester)/R/package"),  .libPaths()))



# create folders (run just once)
# dir.create("./output")
# dir.create("./output/figs/")
# dir.create("./output/tabs/")
# dir.create("./functions")
# dir.create("./data")
# dir.create("./data/raw/")
# dir.create("./data/clean/")
# dir.create("./mplus")
# dir.create("./info")

# install packages
pkg <- c("knitr", "tidyverse", "lavaan", "ggcorrplot",
         "blavaan", "rstan", "viridis", "ggpubr")

# sapply(pkg, install.packages)
sapply(pkg, library, character.only = T)


# rstan set
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# load data ---------------------------------------------------------------

barca_raw_full <- read_csv("./data/clean/neighbourhoods.csv")
barca_2y_full <- read_csv("./data/clean/neighbourhoods_2years.csv")
barca_6y_full <- read_csv("./data/clean/neighbourhoods_6years.csv")

# data cleaning -----------------------------------------------------------

barca_2y <- barca_2y_full %>%
  mutate(id = as.factor(Barri) %>% as.numeric()) %>%
  rename_all(~str_to_lower(.)) %>%
  rename(row = x1, year = any) %>%
  select(row, id, everything())

barca_6y <- barca_6y_full %>%
  mutate(id = as.factor(Barri) %>% as.numeric()) %>%
  rename_all(~str_to_lower(.)) %>%
  rename(row = x1, year = any) %>%
  select(row, id, everything())


# descriptives ------------------------------------------------------------


vars_int <- str_c(c("property_vh", "property_personal", "violence") %>%
                    rep(each = 4),
                  c("", ".r.w", ".f.w", ".p.w") %>%
                    rep(3))

vars_int2 <- str_remove_all(vars_int, "erty|onal|ence")


# change in time

dat_s <- barca_2y %>%
  select(id, year, vars_int)



# variances
descriptives <- dat_s %>%
  select(-id) %>%
  group_by(year) %>%
  summarise_all(list(mean = ~mean(.),
                     sd = ~sd(.))) %>%
  gather(-year, key = key, value = value) %>%
  mutate(stat = ifelse(str_detect(key, "mean$"), "Mean", "SD"),
         group = case_when(str_detect(key, "\\.r\\.w") ~ "Survey victim",
                           str_detect(key, "\\.p\\.w") ~ "Survey reported",
                           str_detect(key, "\\.f\\.w") ~ "Survey places",
                           TRUE ~ "Police "),
         topic = case_when(
           str_detect(key, "property_personal") ~ "Property personal",
           str_detect(key, "property_vh") ~ "Property household/vehicle",
           TRUE ~ "Violence"),
         var = str_remove_all(key, "_mean|_sd|\\.r\\.w"))

descriptives %>%
  ggplot(aes(year, value,
             color = group, group = as.factor(key))) +
  geom_line(size = 1.5) +
  facet_wrap(topic~ stat, scales = "free_y", ncol = 2) +
  theme_bw() +
  labs(color = "Data source",
       linetype = "Variable",
       x = "Year",
       y = "Value") +
  viridis::scale_color_viridis(discrete = T)


ggsave("./output/figs/avg_sd_change.png")
write_csv(descriptives, "./output/tabs/descriptive_change.csv")


# overall correlation matrix
# corelations


corr_data <- barca_6y %>%
  select(id, vars_int) %>%
  rename_all(~str_remove_all(., "erty|onal|ence")) %>%
  ungroup() %>%
  select(-id) %>%
  mutate_all(~log(. + 1))


corr_data %>%
  cor() %>%
  as_tibble() %>%
  mutate(row = names(corr_data)) %>%
  select(row, everything()) %>%
  filter(!str_detect(row, "\\.w")) %>%
  gather(-row, key = key, value = cor) %>%
  mutate(group = case_when(str_detect(key, "\\.r\\.w") ~ "Survey victims",
                           str_detect(key, "\\.p\\.w") ~ "Survey reported",
                           str_detect(key, "\\.f\\.w") ~ "Survey places",
                           TRUE ~ "Police"),
         topic = case_when(
           str_detect(key, "prop_pers") ~ "Property personal",
           str_detect(key, "prop_vh") ~ "Property household/vehicle",
           TRUE ~ "Violence"),
         key2 = str_remove_all(key, "\\..+")) %>%
  filter(row == key2, cor != 1) %>%
  ggplot(aes(cor, topic, color = group)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw() +
  viridis::scale_color_viridis(discrete = T) +
  labs(x = "Correlation with police data", y = "Topic", color = "Question")

ggsave("./output/figs/cor_police_overall.png")

corr_data %>%
  summarise_all(~mean(.)) %>%
  gather() %>%
  mutate(group = case_when(str_detect(key, "\\.r\\.w") ~ "Survey victims",
                           str_detect(key, "\\.p\\.w") ~ "Survey reported",
                           str_detect(key, "\\.f\\.w") ~ "Survey places",
                           TRUE ~ "Police"),
         topic = case_when(
           str_detect(key, "prop_pers") ~ "Property personal",
           str_detect(key, "prop_vh") ~ "Property household/vehicle",
           TRUE ~ "Violence"),
         key2 = str_remove_all(key, "\\..+"),
         est = exp(value) - 1) %>%
  ggplot(aes(value, topic, color = group)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw() +
  viridis::scale_color_viridis(discrete = T) +
  labs(x = "Log crime rates", y = "Topic", color = "Question")


ggsave("./output/figs/logrates_police_overall.png")


# Quasi-simplex models ----------------------------------------------------



# takes a while to run so just re-load

# make log
b2yw2 <- dat_s %>%
  select(matches(vars_int2)) %>%
  mutate_all(~log(. + 1))



# qs <- function(var) {
#   model <- str_c("t1 =~ ", var, "_15\n",
#                  "t2 =~ ", var, "_17\n",
#                  "t3 =~ ", var, "_19\n\n",
#                  "t2 ~ t1 \nt3 ~ t2\n\n",
#                  var, "_15 ~~ a*", var, "_15\n",
#                  var, "_17 ~~ a*", var, "_17\n",
#                  var, "_19 ~~ a*", var, "_19")
#
#   bsem(model, data = b2y_long2,
#        burnin = 8000, sample = 2000, n.chains = 8)
# }
#
# res_qs <- map(vars_int2, qs)
# save(res_qs, file = "./output/quasi_simple.RData")

load("./output/quasi_simple.RData")


qs_rel <- map(res_qs, function(x) summary(x, standardized = TRUE) %>%
      .[1:3, 7] %>% as.numeric()
) %>%
  reduce(cbind) %>%
  as_tibble() %>%
  setNames(vars_int2) %>%
  mutate(year = c(15, 17, 19))




qs_rel %>%
  gather(-year, key = key, value = value) %>%
  filter(str_detect(key, "f\\.w|r\\.w|p\\.w")) %>%
  mutate(group = case_when(str_detect(key, "\\.r\\.w") ~ "Survey victim",
                           str_detect(key, "\\.p\\.w") ~ "Survey reported",
                           str_detect(key, "\\.f\\.w") ~ "Survey places",
                           TRUE ~ "Police"),
         var = str_remove_all(key, "\\.f\\.w|\\.p\\.w|\\.r\\.w"),
         topic = case_when(
           str_detect(key, "prop_pers") ~ "Property personal",
           str_detect(key, "prop_vh") ~ "Property household/vehicle",
           TRUE ~ "Violence")) %>%
  ggplot(aes(as.factor(year), value,
             color = group, group = group)) +
  geom_line(size = 1.5) +
  facet_wrap(~topic) +
  theme_bw() +
  labs(color = "Question",
       x = "Year",
       y = "Reliability") +
  viridis::scale_color_viridis(discrete = T)


ggsave("./output/figs/reliability_survey_qs.png")



# multi-item QS -----------------------------------------------------------


# run only once
#
# # take log
#
# # property vehicle
# model <- c("t1 =~ 1*prop_vh.f.w_15 + 1*prop_vh.p.w_15 + 1*prop_vh.r.w_15
#             t2 =~ 1*prop_vh.f.w_17 + 1*prop_vh.p.w_17 + 1*prop_vh.r.w_17
#             t3 =~ 1*prop_vh.f.w_19 + 1*prop_vh.p.w_19 + 1*prop_vh.r.w_19
#
#             t2 ~ t1
#             t3 ~ t2
#            ")
# m1_propvh <- bsem(model, data = b2yw2,
#                   burnin = 8000, sample = 2000, n.chains = 8)
#
# summary(m1_propvh, standardized = TRUE)
#
# # property personal
# model <- c("t1 =~ 1*prop_pers.f.w_15 + 1*prop_pers.p.w_15 + 1*prop_pers.r.w_15
#             t2 =~ 1*prop_pers.f.w_17 + 1*prop_pers.p.w_17 + 1*prop_pers.r.w_17
#             t3 =~ 1*prop_pers.f.w_19 + 1*prop_pers.p.w_19 + 1*prop_pers.r.w_19
#
#             t2 ~ t1
#             t3 ~ t2
#            ")
#
# m1_proppers <- bsem(model, data = b2yw2,
#                     burnin = 8000, sample = 2000, n.chains = 8)
#
# summary(m1_proppers, standardized = TRUE)
#
#
# # violence
# model <- c("t1 =~ 1*viol.f.w_15 + 1*viol.p.w_15 + 1*viol.r.w_15
#             t2 =~ 1*viol.f.w_17 + 1*viol.p.w_17 + 1*viol.r.w_17
#             t3 =~ 1*viol.f.w_19 + 1*viol.p.w_19 + 1*viol.r.w_19
#
#             t2 ~ t1
#             t3 ~ t2
#            ")
# m1_viol <- bsem(model, data = b2yw2,
#                 burnin = 8000, sample = 2000, n.chains = 8)
#
# summary(m1_viol, standardized = TRUE)
#
#
# qs_m1 <- list(m1_propvh, m1_proppers, m1_viol)


# save(qs_m1, file = "./output/qs_m1.RData")

load("./output/qs_m1.RData")


summary(qs_m1[[3]], standardized = T)


# Models 2 with method effect ---------------------------------------------
#
# # property vehicle
# model <- c("t1 =~ 1*prop_vh.f.w_15 + 1*prop_vh.p.w_15 + 1*prop_vh.r.w_15
#             t2 =~ 1*prop_vh.f.w_17 + 1*prop_vh.p.w_17 + 1*prop_vh.r.w_17
#             t3 =~ 1*prop_vh.f.w_19 + 1*prop_vh.p.w_19 + 1*prop_vh.r.w_19
#
#             t2 ~ t1
#             t3 ~ t2
#
#             m_rep =~ 1*prop_vh.p.w_15 + 1*prop_vh.p.w_17 + 1*prop_vh.p.w_19
#             m_vic =~ 1*prop_vh.r.w_15 + 1*prop_vh.r.w_17 + 1*prop_vh.r.w_19
#             m_loc =~ 1*prop_vh.f.w_15 + 1*prop_vh.f.w_17 + 1*prop_vh.f.w_19
#
#             m_vic ~~ 0*m_rep
#             m_vic ~~ 0*m_loc
#             m_vic ~~ 0*t1
#             m_rep ~~ 0*m_loc
#             m_rep ~~ 0*t1
#             m_loc ~~ 0*t1
#
#
#             t1 ~ 1
#             t2 ~ 1
#             t3 ~ 1
#
#             m_vic ~ 1
#             m_rep ~ 1
#             m_loc ~ 1
#
#             prop_vh.p.w_15 + prop_vh.p.w_17 + prop_vh.p.w_19 + prop_vh.r.w_15 + prop_vh.r.w_17 + prop_vh.r.w_19 + prop_vh.f.w_15 + prop_vh.f.w_17 + prop_vh.f.w_19 ~ 0*1
#
#            ")
#
#
# m2_propvh <- bsem(model, data = b2yw2,
#                   burnin = 8000, sample = 2000, n.chains = 8)
#
# summary(m2_propvh, standardized = TRUE)
#
#
#
# # property personal
# model <- c("t1 =~ 1*prop_pers.f.w_15 + 1*prop_pers.p.w_15 + 1*prop_pers.r.w_15
#             t2 =~ 1*prop_pers.f.w_17 + 1*prop_pers.p.w_17 + 1*prop_pers.r.w_17
#             t3 =~ 1*prop_pers.f.w_19 + 1*prop_pers.p.w_19 + 1*prop_pers.r.w_19
#
#             t2 ~ t1
#             t3 ~ t2
#
#             m_rep =~ 1*prop_pers.p.w_15 + 1*prop_pers.p.w_17 + 1*prop_pers.p.w_19
#             m_vic =~ 1*prop_pers.r.w_15 + 1*prop_pers.r.w_17 + 1*prop_pers.r.w_19
#             m_loc =~ 1*prop_pers.f.w_15 + 1*prop_pers.f.w_17 + 1*prop_pers.f.w_19
#
#             m_vic ~~ 0*m_rep
#             m_vic ~~ 0*m_loc
#             m_vic ~~ 0*t1
#             m_rep ~~ 0*m_loc
#             m_rep ~~ 0*t1
#             m_loc ~~ 0*t1
#
#
#             t1 ~ 1
#             t2 ~ 1
#             t3 ~ 1
#
#             m_vic ~ 1
#             m_rep ~ 1
#             m_loc ~ 1
#
#             prop_pers.p.w_15 + prop_pers.p.w_17 + prop_pers.p.w_19 + prop_pers.r.w_15 + prop_pers.r.w_17 + prop_pers.r.w_19 + prop_pers.f.w_15 + prop_pers.f.w_17 + prop_pers.f.w_19 ~ 0*1
#
#            ")
# m2_proppers <- bsem(model, data = b2yw2,
#                     burnin = 8000, sample = 2000, n.chains = 8)
#
# summary(m2_proppers, standardized = TRUE)
#
#
#
#
#
# # violence
# model <- c("t1 =~ 1*viol.f.w_15 + 1*viol.p.w_15 + 1*viol.r.w_15
#             t2 =~ 1*viol.f.w_17 + 1*viol.p.w_17 + 1*viol.r.w_17
#             t3 =~ 1*viol.f.w_19 + 1*viol.p.w_19 + 1*viol.r.w_19
#
#             t2 ~ t1
#             t3 ~ t2
#
#             m_rep =~ 1*viol.p.w_15 + 1*viol.p.w_17 + 1*viol.p.w_19
#             m_vic =~ 1*viol.r.w_15 + 1*viol.r.w_17 + 1*viol.r.w_19
#             m_loc =~ 1*viol.f.w_15 + 1*viol.f.w_17 + 1*viol.f.w_19
#
#
#             m_vic ~~ 0*m_rep
#             m_vic ~~ 0*m_loc
#             m_vic ~~ 0*t1
#             m_rep ~~ 0*m_loc
#             m_rep ~~ 0*t1
#             m_loc ~~ 0*t1
#
#             t1 ~ 1
#             t2 ~ 1
#             t3 ~ 1
#
#             m_vic ~ 1
#             m_rep ~ 1
#             m_loc ~ 1
#
#             viol.p.w_15 + viol.p.w_17 + viol.p.w_19 + viol.r.w_15 + viol.r.w_17 + viol.r.w_19 + viol.f.w_15 + viol.f.w_17 + viol.f.w_19 ~ 0*1
#
#            ")
# m2_viol <- bsem(model, data = b2yw2,
#                 burnin = 8000, sample = 2000, n.chains = 8)
#
# summary(m2_viol, standardized = TRUE)
#
#
# qs_m2 <- list(m2_propvh, m2_proppers, m2_viol)

# save(qs_m2, file = "./output/qs_m2.RData")

load("./output/qs_m2.RData")
summary(qs_m2[[2]], standardized = T)


qual_qsm2 <- qs_m2 %>%
  map_df(function(x) {
    parameterestimates(x, standardized = T) %>%
      filter(op == "=~" | op == "~~") %>%
      filter(!std.all %in% c(0, 1)) %>%
      filter(!(op == "~~" & str_detect(lhs, "^m_|t[1-9]")))  %>%
      mutate(
        group = case_when(
          str_detect(rhs, "\\.r\\.w") ~ "Survey victim",
          str_detect(rhs, "\\.p\\.w") ~ "Survey reported",
          str_detect(rhs, "\\.f\\.w") ~ "Survey places",
          TRUE ~ "Police"
        ),
        source = case_when(
          str_detect(lhs, "t[1-9]") ~ "Trait",
          str_detect(lhs, "m_") ~ "Method",
          TRUE ~ "Random error"
        ),
        qual = ifelse(source != "Random error", std.all ^ 2, std.all),
        year = str_extract(rhs, "[0-9].+")
      ) %>%
      select(-op, -est, -std.lv, -std.nox)
  }) %>%
  mutate(Trait = case_when(str_detect(rhs, "viol") ~ "Violence",
                  str_detect(rhs, "vh") ~ "Property household/vehicle",
                  str_detect(rhs, "pers") ~ "Property personal"))




qual_qsm2 %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  ggplot(aes(year, qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_grid(Trait~group) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Question",
       fill = "Source") +
  theme_bw()

ggsave("./output/figs/qsm2_quality.png")



qs_m2 %>%
  map_df(function(x) {
    parameterestimates(x, standardized = T) %>%
      filter(op == "~1", est != 0) %>%
      mutate(type = rep(c("Trait", "Method"), each = 3),
             year = c("15", "17", "19", rep("overall", 3)))
  }) %>%
  mutate(trait = rep(c("Property household/vehicle", "Property personal",
                        "Violence"), each = 6),
         source = case_when(str_detect(lhs, "vic") ~ "Survey victim",
                           str_detect(lhs, "rep") ~ "Survey reported",
                           str_detect(lhs, "loc") ~ "Survey places"),
         source = ifelse(is.na(source), year, source),
         type = as.factor(type) %>% fct_rev()) %>%
  ggplot(aes(est, source)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, alpha = 0.5) +
  facet_wrap(trait~ type, ncol = 2, scales = "free_y") +
  theme_bw() +
  labs(x = "Mean (log rates)",
       y = "Dimension")

ggsave("./output/figs/qsm2_mean.png")





# MTMM model --------------------------------------------------------------

b6yw <- barca_6y %>%
  rename_all(~str_remove_all(., "erty|onal|ence")) %>%
  select(matches(str_subset(vars_int2, "w"))) %>%
  select(-ends_with("2")) %>%
  mutate_all(~log(. + 1))

b6yw %>%
  cor() %>%
  ggcorrplot() +
  viridis::scale_fill_viridis(direction = -1)

ggsave("./output/figs/mtmm_corr_matrix.png")

# model <- c("t_viol =~ 1*viol.f.w + 1*viol.p.w + 1*viol.r.w
#             t_vh =~ 1*prop_vh.f.w + 1*prop_vh.p.w + 1*prop_vh.r.w
#             t_pers =~ 1*prop_pers.f.w + 1*prop_pers.p.w + 1*prop_pers.r.w
#
#             m_rep =~ 1*viol.p.w + 1*prop_vh.p.w + 1*prop_pers.p.w
#             m_vic =~ 1*viol.r.w + 1*prop_vh.r.w + 1*prop_pers.r.w
#             m_loc =~ 1*viol.f.w + 1*prop_vh.f.w + 1*prop_pers.f.w
#
#             m_vic ~~ 0*m_rep
#             m_vic ~~ 0*m_loc
#             m_vic ~~ 0*t_viol
#             m_vic ~~ 0*t_vh
#             m_vic ~~ 0*t_pers
#
#             m_rep ~~ 0*m_loc
#             m_rep ~~ 0*t_viol
#             m_rep ~~ 0*t_vh
#             m_rep ~~ 0*t_pers
#
#             m_loc ~~ 0*t_viol
#             m_loc ~~ 0*t_vh
#             m_loc ~~ 0*t_pers
#
#             t_viol ~ 1
#             t_vh ~ 1
#             t_pers ~ 1
#             m_vic ~ 1
#             m_rep ~ 1
#             m_loc ~ 1
#
#             viol.p.w + prop_vh.p.w + prop_pers.p.w + viol.r.w + prop_vh.r.w + prop_pers.r.w + viol.f.w + prop_vh.f.w + prop_pers.f.w ~ 0*1
#
#            ")
# mtmm <- bsem(model, data = b6yw,
#              burnin = 8000, sample = 2000, n.chains = 8)
#
# summary(mtmm, standardized = TRUE)
#
# save(mtmm, file = "./output/mtmm.RData")

load("./output/mtmm.RData")
summary(mtmm, standardized = TRUE)

mtmm_est <- lavaan::parameterestimates(mtmm, standardized = T)

mtmm_qual <- mtmm_est %>%
  filter(op == "=~" | op == "~~") %>%
  filter(!(op == "~~" & str_detect(lhs, "t_|m_"))) %>%
  mutate(Trait = case_when(str_detect(rhs, "viol") ~ "Violence",
                           str_detect(rhs, "vh") ~ "Property household/vehicle",
                           str_detect(rhs, "pers") ~ "Property personal"),
         group = case_when(str_detect(rhs, "\\.r\\.w") ~ "Survey victim",
                           str_detect(rhs, "\\.p\\.w") ~ "Survey reported",
                           str_detect(rhs, "\\.f\\.w") ~ "Survey places",
                           TRUE ~ "Police"),
         source = case_when(str_detect(lhs, "t_") ~ "Trait",
                            str_detect(lhs, "m_") ~ "Method",
                            TRUE ~ "Random error"),
         qual = ifelse(source != "Random error", std.all^2, std.all)) %>%
  select(-op, -est, -std.lv, -std.nox)


mtmm_qual %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  ggplot(aes(group, qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait, nrow = 3) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Question",
       fill = "Source") +
    theme_bw()

ggsave("./output/figs/mtmm_qual_overall.png", height = 8)


mtmm_est %>%
  filter(op == "~1", est != 0) %>%
  select(lhs, est) %>%
  mutate(type = rep(c("Trait", "Method"), each = 3),
         source = c("Violence", "Property household/vehicle",
                    "Property personal", "Survey victim",
                    "Survey reported", "Survey places"),
         type = as.factor(type) %>% fct_rev()) %>%
  ggplot(aes(est, source)) +
  facet_wrap(~ type, nrow = 2, scales = "free_y") +
  geom_point(size = 2) +
  theme_bw() +
  labs(x = "Mean (log rates)",
       y = "Dimension")

ggsave("./output/figs/mtmm_mean_overall.png")


launch_shinystan(mtmm)
