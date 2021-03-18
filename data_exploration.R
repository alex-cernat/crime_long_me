#############################################################
#
# Project looking estimating measurement error in crime data using
# longitudinal models
#
# https://journals.sagepub.com/home/jrc
#
# We start by using data from Barcelona 2015-2019
#
#  TO DO
#  - correlation and means by 2 years
#  - sensitivity analysis multi-item qs 2 items at a time
#  - try multi-item qs with police
#  - mtmm 2 methods at a time
#  - do mtmm with the police as well
#  - do correlations with police in w2
#  - redo analysis with measures without weights
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



vars_int_w2 <- str_c(c("property_vh", "property_personal", "violence") %>%
                       rep(each = 2),
                     c(".f.w2", ".p.w2") %>%
                       rep(3))

vars_int2_w2 <- str_remove_all(vars_int_w2, "erty|onal|ence")




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
# correlations


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

# make wide data

b2yw <- barca_2y %>%
  select(id, year, vars_int) %>%
  mutate(year = str_remove_all(year, "20|-.+")) %>%
  pivot_wider(
    values_from = property_vh:violence.p.w,
    names_sep = "_",
    names_from = year) %>%
  rename_all(~str_remove_all(., "erty|onal|ence"))



# takes a while to run so just re-load

# make log
b2yw2 <- b2yw %>%
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
#   bsem(model, data = b2yw2,
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





# Sensitivity analysis with new weighted estimates ------------------------



# make wide

# make wide data

b2yw_w2 <- barca_2y %>%
  select(id, year, vars_int_w2, vars_int[c(1:2, 5:6, 9:10)]) %>%
  mutate(year = str_remove_all(year, "20|-.+")) %>%
  pivot_wider(
    values_from = property_vh.f.w2:violence.r.w,
    names_sep = "_",
    names_from = year) %>%
  rename_all(~str_remove_all(., "erty|onal|ence"))


# change in time

b2yw2_w2 <- b2yw_w2 %>%
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
#   bsem(model, data = b2yw2_w2,
#        burnin = 8000, sample = 2000, n.chains = 8)
# }
#
# res_qs_w2 <- map(vars_int2_w2, qs)
# save(res_qs_w2, file = "./output/quasi_simple_w2.RData")

load("./output/quasi_simple_w2.RData")


qs_rel_w2 <- map(res_qs_w2, function(x) summary(x, standardized = TRUE) %>%
                .[1:3, 7] %>% as.numeric()
) %>%
  reduce(cbind) %>%
  as_tibble() %>%
  setNames(vars_int2_w2) %>%
  mutate(year = c(15, 17, 19))


qs_rel %>%
  select(year, matches("r.w")) %>%
  full_join(qs_rel_w2, by = "year") %>%
  gather(-year, key = key, value = value) %>%
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


ggsave("./output/figs/reliability_survey_qs_w2.png")




# Models 2 with method effect ---------------------------------------------

# # property vehicle
# model <- c("t1 =~ 1*prop_vh.f.w2_15 + 1*prop_vh.p.w2_15 + 1*prop_vh.r.w_15
#             t2 =~ 1*prop_vh.f.w2_17 + 1*prop_vh.p.w2_17 + 1*prop_vh.r.w_17
#             t3 =~ 1*prop_vh.f.w2_19 + 1*prop_vh.p.w2_19 + 1*prop_vh.r.w_19
#
#             t2 ~ t1
#             t3 ~ t2
#
#             m_rep =~ 1*prop_vh.p.w2_15 + 1*prop_vh.p.w2_17 + 1*prop_vh.p.w2_19
#             m_vic =~ 1*prop_vh.r.w_15 + 1*prop_vh.r.w_17 + 1*prop_vh.r.w_19
#             m_loc =~ 1*prop_vh.f.w2_15 + 1*prop_vh.f.w2_17 + 1*prop_vh.f.w2_19
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
#             prop_vh.p.w2_15 + prop_vh.p.w2_17 + prop_vh.p.w2_19 + prop_vh.r.w_15 + prop_vh.r.w_17 + prop_vh.r.w_19 + prop_vh.f.w2_15 + prop_vh.f.w2_17 + prop_vh.f.w2_19 ~ 0*1
#
#            ")
#
#
# m2_propvh_w2 <- bsem(model, data = b2yw2_w2,
#                   burnin = 8000, sample = 2000, n.chains = 8)
#
# summary(m2_propvh_w2, standardized = TRUE)
#
#
#
# # property personal
# model <- c("t1 =~ 1*prop_pers.f.w2_15 + 1*prop_pers.p.w2_15 + 1*prop_pers.r.w_15
#             t2 =~ 1*prop_pers.f.w2_17 + 1*prop_pers.p.w2_17 + 1*prop_pers.r.w_17
#             t3 =~ 1*prop_pers.f.w2_19 + 1*prop_pers.p.w2_19 + 1*prop_pers.r.w_19
#
#             t2 ~ t1
#             t3 ~ t2
#
#             m_rep =~ 1*prop_pers.p.w2_15 + 1*prop_pers.p.w2_17 + 1*prop_pers.p.w2_19
#             m_vic =~ 1*prop_pers.r.w_15 + 1*prop_pers.r.w_17 + 1*prop_pers.r.w_19
#             m_loc =~ 1*prop_pers.f.w2_15 + 1*prop_pers.f.w2_17 + 1*prop_pers.f.w2_19
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
#             prop_pers.p.w2_15 + prop_pers.p.w2_17 + prop_pers.p.w2_19 + prop_pers.r.w_15 + prop_pers.r.w_17 + prop_pers.r.w_19 + prop_pers.f.w2_15 + prop_pers.f.w2_17 + prop_pers.f.w2_19 ~ 0*1
#
#            ")
# m2_proppers_w2 <- bsem(model, data = b2yw2_w2,
#                     burnin = 8000, sample = 2000, n.chains = 8)
#
# summary(m2_proppers_w2, standardized = TRUE)
#
#
#
#
#
# # violence
# model <- c("t1 =~ 1*viol.f.w2_15 + 1*viol.p.w2_15 + 1*viol.r.w_15
#             t2 =~ 1*viol.f.w2_17 + 1*viol.p.w2_17 + 1*viol.r.w_17
#             t3 =~ 1*viol.f.w2_19 + 1*viol.p.w2_19 + 1*viol.r.w_19
#
#             t2 ~ t1
#             t3 ~ t2
#
#             m_rep =~ 1*viol.p.w2_15 + 1*viol.p.w2_17 + 1*viol.p.w2_19
#             m_vic =~ 1*viol.r.w_15 + 1*viol.r.w_17 + 1*viol.r.w_19
#             m_loc =~ 1*viol.f.w2_15 + 1*viol.f.w2_17 + 1*viol.f.w2_19
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
#             viol.p.w2_15 + viol.p.w2_17 + viol.p.w2_19 + viol.r.w_15 + viol.r.w_17 + viol.r.w_19 + viol.f.w2_15 + viol.f.w2_17 + viol.f.w2_19 ~ 0*1
#
#            ")
# m2_viol_w2 <- bsem(model, data = b2yw2_w2,
#                 burnin = 8000, sample = 2000, n.chains = 8)
#
# summary(m2_viol_w2, standardized = TRUE)
#
#
# qs_m2_w2 <- list(m2_propvh_w2, m2_proppers_w2, m2_viol_w2)
#
# save(qs_m2_w2, file = "./output/qs_m2_w2.RData")



load("./output/qs_m2_w2.RData")



summary(qs_m2_w2[[1]])


qual_qsm2_w2 <- qs_m2_w2 %>%
  map_df(function(x) {
    parameterestimates(x, standardized = T) %>%
      filter(op == "=~" | op == "~~") %>%
      filter(!std.all %in% c(0, 1)) %>%
      filter(!(op == "~~" & str_detect(lhs, "^m_|t[1-9]")))  %>%
      mutate(
        group = case_when(
          str_detect(rhs, "\\.r\\.w") ~ "Survey victim",
          str_detect(rhs, "\\.p\\.w2") ~ "Survey reported",
          str_detect(rhs, "\\.f\\.w2") ~ "Survey places",
          TRUE ~ "Police"
        ),
        source = case_when(
          str_detect(lhs, "t[1-9]") ~ "Trait",
          str_detect(lhs, "m_") ~ "Method",
          TRUE ~ "Random error"
        ),
        qual = ifelse(source != "Random error", std.all ^ 2, std.all),
        year = str_extract(rhs, "_([0-9].+)") %>% str_remove("_")
      ) %>%
      select(-op, -est, -std.lv, -std.nox)
  }) %>%
  mutate(Trait = case_when(str_detect(rhs, "viol") ~ "Violence",
                           str_detect(rhs, "vh") ~ "Property household/vehicle",
                           str_detect(rhs, "pers") ~ "Property personal"))




qual_qsm2_w2 %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  ggplot(aes(year, qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_grid(Trait~group) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Question",
       fill = "Source") +
  theme_bw()

ggsave("./output/figs/qsm2_quality_w2.png")



qs_m2_w2 %>%
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

ggsave("./output/figs/qsm2_mean_w2.png")













# MTMM model --------------------------------------------------------------

b6yw_w2 <- barca_6y %>%
  rename_all(~str_remove_all(., "erty|onal|ence")) %>%
  select(matches(vars_int2_w2), vars_int2[c(2, 6, 10)]) %>%
  mutate_all(~log(. + 1))

b6yw_w2 %>%
  cor() %>%
  ggcorrplot() +
  viridis::scale_fill_viridis(direction = -1)


# model <- c("t_viol =~ 1*viol.f.w2 + 1*viol.p.w2 + 1*viol.r.w
#             t_vh =~ 1*prop_vh.f.w2 + 1*prop_vh.p.w2 + 1*prop_vh.r.w
#             t_pers =~ 1*prop_pers.f.w2 + 1*prop_pers.p.w2 + 1*prop_pers.r.w
#
#             m_rep =~ 1*viol.p.w2 + 1*prop_vh.p.w2 + 1*prop_pers.p.w2
#             m_vic =~ 1*viol.r.w + 1*prop_vh.r.w + 1*prop_pers.r.w
#             m_loc =~ 1*viol.f.w2 + 1*prop_vh.f.w2 + 1*prop_pers.f.w2
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
#             viol.p.w2 + prop_vh.p.w2 + prop_pers.p.w2 + viol.r.w + prop_vh.r.w + prop_pers.r.w + viol.f.w2 + prop_vh.f.w2 + prop_pers.f.w2 ~ 0*1
#
#            ")
# mtmm_w2 <- bsem(model, data = b6yw_w2,
#              burnin = 8000, sample = 2000, n.chains = 8)
#
# summary(mtmm_w2, standardized = TRUE)
#
# save(mtmm_w2, file = "./output/mtmm_w2.RData")

load("./output/mtmm_w2.RData")




mtmm_w2_est <- lavaan::parameterestimates(mtmm_w2, standardized = T)

mtmm_w2_qual <- mtmm_w2_est %>%
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


mtmm_w2_qual %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  ggplot(aes(group, qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait, nrow = 3) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Question",
       fill = "Source") +
  theme_bw()

ggsave("./output/figs/mtmm_qual_overall_w2.png", height = 8)


mtmm_w2_est %>%
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

ggsave("./output/figs/mtmm_mean_overall_w2.png")





# MTMM with 4 methods -----------------------------------------------------

# make data only with the vars we want
b6yw2_w2 <- barca_6y %>%
  rename_all(~str_remove_all(., "erty|onal|ence")) %>%
  select(matches(vars_int2_w2), vars_int2[c(1:2, 5:6, 9:10)]) %>%
  mutate_all(~log(. + 1))


# correlation matrix
b6yw2_w2 %>%
  cor() %>%
  ggcorrplot() +
  viridis::scale_fill_viridis(direction = -1)




model <- c("t_viol =~ 1*viol + 1*viol.f.w2 + 1*viol.p.w2 + 1*viol.r.w
            t_vh =~ 1*prop_vh + 1*prop_vh.f.w2 + 1*prop_vh.p.w2 + 1*prop_vh.r.w
            t_pers =~ 1*prop_pers + 1*prop_pers.f.w2 + 1*prop_pers.p.w2 + 1*prop_pers.r.w

            m_rep =~ 1*viol.p.w2 + 1*prop_vh.p.w2 + 1*prop_pers.p.w2
            m_vic =~ 1*viol.r.w + 1*prop_vh.r.w + 1*prop_pers.r.w
            m_loc =~ 1*viol.f.w2 + 1*prop_vh.f.w2 + 1*prop_pers.f.w2
            m_pol =~ 1*viol + 1*prop_vh + 1*prop_pers

            m_vic ~~ 0*m_rep
            m_vic ~~ 0*m_loc
            m_vic ~~ 0*m_pol
            m_vic ~~ 0*t_viol
            m_vic ~~ 0*t_vh
            m_vic ~~ 0*t_pers


            m_rep ~~ 0*m_loc
            m_rep ~~ 0*m_pol
            m_rep ~~ 0*t_viol
            m_rep ~~ 0*t_vh
            m_rep ~~ 0*t_pers

            m_loc ~~ 0*m_pol
            m_loc ~~ 0*t_viol
            m_loc ~~ 0*t_vh
            m_loc ~~ 0*t_pers

            m_pol ~~ 0*t_viol
            m_pol ~~ 0*t_vh
            m_pol ~~ 0*t_pers

            t_viol ~ 1
            t_vh ~ 1
            t_pers ~ 1
            m_vic ~ 1
            m_rep ~ 1
            m_loc ~ 1
            m_pol ~ 1


            viol + prop_vh + prop_pers + viol.p.w2 + prop_vh.p.w2 + prop_pers.p.w2 + viol.r.w + prop_vh.r.w + prop_pers.r.w + viol.f.w2 + prop_vh.f.w2 + prop_pers.f.w2 ~ 0*1

           ")
mtmm2_w2 <- bsem(model, data = b6yw2_w2,
             burnin = 8000, sample = 2000, n.chains = 8)

summary(mtmm2_w2, standardized = TRUE)

save(mtmm2_w2, file = "./output/mtmm2_w2.RData")

load("./output/mtmm2_w2.RData")

summary(mtmm2_w2, standardized = T)


mtmm2_w2_est <- lavaan::parameterestimates(mtmm2_w2, standardized = T)

mtmm2_w2_qual <- mtmm2_w2_est %>%
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


mtmm2_w2_qual %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  ggplot(aes(group, qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait, nrow = 3) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Question",
       fill = "Source") +
  theme_bw()

ggsave("./output/figs/mtmm2_qual_overall_w2.png", height = 8)


mtmm2_w2_est %>%
  filter(op == "~1", est != 0) %>%
  select(lhs, est) %>%
  mutate(type = rep(c("Trait", "Method"), each = 3) %>% c("Method"),
         source = c("Violence", "Property household/vehicle",
                    "Property personal", "Survey victim",
                    "Survey reported", "Survey places", "Police"),
         type = as.factor(type) %>% fct_rev()) %>%
  ggplot(aes(est, source)) +
  facet_wrap(~ type, nrow = 2, scales = "free_y") +
  geom_point(size = 2) +
  theme_bw() +
  labs(x = "Mean (log rates)",
       y = "Dimension")

ggsave("./output/figs/mtmm2_mean_overall_w2.png")














# Models 2 with method effect and police --------------------------------------

# property vehicle
model <- c("t1 =~ 1*prop_vh_15 + 1*prop_vh.f.w2_15 + 1*prop_vh.p.w2_15 + 1*prop_vh.r.w_15
            t2 =~ 1*prop_vh_17 + 1*prop_vh.f.w2_17 + 1*prop_vh.p.w2_17 + 1*prop_vh.r.w_17
            t3 =~ 1*prop_vh_19 + 1*prop_vh.f.w2_19 + 1*prop_vh.p.w2_19 + 1*prop_vh.r.w_19

            t2 ~ t1
            t3 ~ t2

            m_rep =~ 1*prop_vh.p.w2_15 + 1*prop_vh.p.w2_17 + 1*prop_vh.p.w2_19
            m_vic =~ 1*prop_vh.r.w_15 + 1*prop_vh.r.w_17 + 1*prop_vh.r.w_19
            m_loc =~ 1*prop_vh.f.w2_15 + 1*prop_vh.f.w2_17 + 1*prop_vh.f.w2_19
            m_pol =~ 1*prop_vh_15 + 1*prop_vh_17 + 1*prop_vh_19

            m_vic ~~ 0*m_rep
            m_vic ~~ 0*m_loc
            m_vic ~~ 0*t1
            m_rep ~~ 0*m_loc
            m_rep ~~ 0*t1
            m_loc ~~ 0*t1

            m_pol ~~ 0*m_vic
            m_pol ~~ 0*m_rep
            m_pol ~~ 0*m_loc
            m_pol ~~ 0*t1


            t1 ~ 1
            t2 ~ 1
            t3 ~ 1

            m_vic ~ 1
            m_rep ~ 1
            m_loc ~ 1
            m_pol ~ 1

            prop_vh_15 + prop_vh_17 + prop_vh_19 + prop_vh.p.w2_15 + prop_vh.p.w2_17 + prop_vh.p.w2_19 + prop_vh.r.w_15 + prop_vh.r.w_17 + prop_vh.r.w_19 + prop_vh.f.w2_15 + prop_vh.f.w2_17 + prop_vh.f.w2_19 ~ 0*1

           ")


m2_propvh_w2p <- bsem(model, data = b2yw2_w2,
                     burnin = 8000, sample = 2000, n.chains = 8)

summary(m2_propvh_w2p, standardized = TRUE)



# property personal
model <- c("t1 =~ 1*prop_pers_15 + 1*prop_pers.f.w2_15 + 1*prop_pers.p.w2_15 + 1*prop_pers.r.w_15
            t2 =~ 1*prop_pers_17 + 1*prop_pers.f.w2_17 + 1*prop_pers.p.w2_17 + 1*prop_pers.r.w_17
            t3 =~ 1*prop_pers_19 + 1*prop_pers.f.w2_19 + 1*prop_pers.p.w2_19 + 1*prop_pers.r.w_19

            t2 ~ t1
            t3 ~ t2

            m_rep =~ 1*prop_pers.p.w2_15 + 1*prop_pers.p.w2_17 + 1*prop_pers.p.w2_19
            m_vic =~ 1*prop_pers.r.w_15 + 1*prop_pers.r.w_17 + 1*prop_pers.r.w_19
            m_loc =~ 1*prop_pers.f.w2_15 + 1*prop_pers.f.w2_17 + 1*prop_pers.f.w2_19
            m_pol =~ 1*prop_pers_15 + 1*prop_pers_17 + 1*prop_pers_19

            m_vic ~~ 0*m_rep
            m_vic ~~ 0*m_loc
            m_vic ~~ 0*t1
            m_rep ~~ 0*m_loc
            m_rep ~~ 0*t1
            m_loc ~~ 0*t1

            m_pol ~~ 0*m_vic
            m_pol ~~ 0*m_rep
            m_pol ~~ 0*m_loc
            m_pol ~~ 0*t1

            t1 ~ 1
            t2 ~ 1
            t3 ~ 1

            m_vic ~ 1
            m_rep ~ 1
            m_loc ~ 1
            m_pol ~ 1

            prop_pers_15 + prop_pers_17 + prop_pers_19 + prop_pers.p.w2_15 + prop_pers.p.w2_17 + prop_pers.p.w2_19 + prop_pers.r.w_15 + prop_pers.r.w_17 + prop_pers.r.w_19 + prop_pers.f.w2_15 + prop_pers.f.w2_17 + prop_pers.f.w2_19 ~ 0*1

           ")
m2_proppers_w2p <- bsem(model, data = b2yw2_w2,
                       burnin = 8000, sample = 2000, n.chains = 8)

summary(m2_proppers_w2p, standardized = TRUE)





# violence
model <- c("t1 =~ 1*viol_15 + 1*viol.f.w2_15 + 1*viol.p.w2_15 + 1*viol.r.w_15
            t2 =~ 1*viol_17 + 1*viol.f.w2_17 + 1*viol.p.w2_17 + 1*viol.r.w_17
            t3 =~ 1*viol_19 + 1*viol.f.w2_19 + 1*viol.p.w2_19 + 1*viol.r.w_19

            t2 ~ t1
            t3 ~ t2

            m_rep =~ 1*viol.p.w2_15 + 1*viol.p.w2_17 + 1*viol.p.w2_19
            m_vic =~ 1*viol.r.w_15 + 1*viol.r.w_17 + 1*viol.r.w_19
            m_loc =~ 1*viol.f.w2_15 + 1*viol.f.w2_17 + 1*viol.f.w2_19
            m_pol =~ 1*viol_15 + 1*viol_17 + 1*viol_19


            m_vic ~~ 0*m_rep
            m_vic ~~ 0*m_loc
            m_vic ~~ 0*t1
            m_rep ~~ 0*m_loc
            m_rep ~~ 0*t1
            m_loc ~~ 0*t1

            m_pol ~~ 0*m_vic
            m_pol ~~ 0*m_rep
            m_pol ~~ 0*m_loc
            m_pol ~~ 0*t1


            t1 ~ 1
            t2 ~ 1
            t3 ~ 1

            m_vic ~ 1
            m_rep ~ 1
            m_loc ~ 1
            m_pol ~ 1

            viol_15 + viol_17 + viol_19 + viol.p.w2_15 + viol.p.w2_17 + viol.p.w2_19 + viol.r.w_15 + viol.r.w_17 + viol.r.w_19 + viol.f.w2_15 + viol.f.w2_17 + viol.f.w2_19 ~ 0*1

           ")
m2_viol_w2p <- bsem(model, data = b2yw2_w2,
                   burnin = 8000, sample = 2000, n.chains = 8)

summary(m2_viol_w2p, standardized = TRUE)


qs_m2_w2p <- list(m2_propvh_w2p, m2_proppers_w2p, m2_viol_w2p)

save(qs_m2_w2p, file = "./output/qs_m2_w2p.RData")



load("./output/qs_m2_w2p.RData")



summary(qs_m2_w2p[[1]])


qual_qsm2_w2p <- qs_m2_w2p %>%
  map_df(function(x) {
    parameterestimates(x, standardized = T) %>%
      filter(op == "=~" | op == "~~") %>%
      filter(!std.all %in% c(0, 1)) %>%
      filter(!(op == "~~" & str_detect(lhs, "^m_|t[1-9]")))  %>%
      mutate(
        group = case_when(
          str_detect(rhs, "\\.r\\.w") ~ "Survey victim",
          str_detect(rhs, "\\.p\\.w2") ~ "Survey reported",
          str_detect(rhs, "\\.f\\.w2") ~ "Survey places",
          TRUE ~ "Police"
        ),
        source = case_when(
          str_detect(lhs, "t[1-9]") ~ "Trait",
          str_detect(lhs, "m_") ~ "Method",
          TRUE ~ "Random error"
        ),
        qual = ifelse(source != "Random error", std.all ^ 2, std.all),
        year = str_extract(rhs, "_([0-9].+)") %>% str_remove("_")
      ) %>%
      select(-op, -est, -std.lv, -std.nox)
  }) %>%
  mutate(Trait = case_when(str_detect(rhs, "viol") ~ "Violence",
                           str_detect(rhs, "vh") ~ "Property household/vehicle",
                           str_detect(rhs, "pers") ~ "Property personal"))




qual_qsm2_w2p %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  ggplot(aes(year, qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_grid(Trait~group) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Question",
       fill = "Source") +
  theme_bw()

ggsave("./output/figs/qsm2_quality_w2p.png")



qs_m2_w2p %>%
  map_df(function(x) {
    parameterestimates(x, standardized = T) %>%
      filter(op == "~1", est != 0) %>%
      mutate(type = rep(c("Trait", "Method"), each = 3) %>% c("Method"),
             year = c("15", "17", "19", rep("overall", 4)))
  }) %>%
  mutate(trait = rep(c("Property household/vehicle", "Property personal",
                       "Violence"), each = 7),
         source = case_when(str_detect(lhs, "vic") ~ "Survey victim",
                            str_detect(lhs, "rep") ~ "Survey reported",
                            str_detect(lhs, "loc") ~ "Survey places",
                            str_detect(lhs, "pol") ~ "Police"),
         source = ifelse(is.na(source), year, source),
         type = as.factor(type) %>% fct_rev()) %>%
  ggplot(aes(est, source)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, alpha = 0.5) +
  facet_wrap(trait~ type, ncol = 2, scales = "free_y") +
  theme_bw() +
  labs(x = "Mean (log rates)",
       y = "Dimension")

ggsave("./output/figs/qsm2_mean_w2p.png")


