#############################################################
#
# Project looking estimating measurement error in crime data using
# longitudinal models
#
# We start by using data from Barcelona 2015-2019
#
#  TO DO
#  - re-focus the paper on comparing survey estimates
#  - clean syntax an estimate reliability for all three survey measures
#      and police
#  - try multi-item simplex model with survey data
#  - try mtmm with pooled data
#  - do correlations with crime data
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

barca_raw_full <- read_csv("./data/raw/neighbourhoods.csv")
barca_2y_full <- read_csv("./data/clean/neighbourhoods_2years.csv")


# data cleaning -----------------------------------------------------------

barca_2y <- barca_2y_full %>%
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
           str_detect(key, "property_personal") ~ "Personal property",
           str_detect(key, "property_vh") ~ "Personal vechicle",
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
# redo with aggregated data


# corelations

b2yw <- barca_2y %>%
  select(id, year, vars_int) %>%
  mutate(year = str_remove_all(year, "20|-.+")) %>%
  pivot_wider(
    values_from = property_vh:violence.p.w,
      names_sep = "_",
      names_from = year) %>%
  rename_all(~str_remove_all(., "erty|onal|ence"))



corr_data <- barca_2y %>%
  select(id, vars_int) %>%
  group_by(id) %>%
  summarise_all(~sum(.)) %>%
  rename_all(~str_remove_all(., "erty|onal|ence")) %>%
  ungroup() %>%
  select(-id)


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
           str_detect(key, "prop_pers") ~ "Personal property",
           str_detect(key, "prop_vh") ~ "Personal vechicle",
           TRUE ~ "Violence"),
         key2 = str_remove_all(key, "\\..+")) %>%
  filter(row == key2, cor != 1) %>%
  ggplot(aes(cor, topic, color = group)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw() +
  viridis::scale_color_viridis(discrete = T) +
  labs(x = "Correlation with police data", y = "Topic", color = "Question")

ggsave("./output/figs/cor_police_overall.png")



g1 <- corr_data %>%
  select(starts_with("viol")) %>%
  setNames(c("Police", "Survey victims", "Survey places", "Survey reported")) %>%
  cor() %>%
  ggcorrplot(type = "upper") +
  viridis::scale_fill_viridis(limits = c(-1, 1), direction = -1) +
  labs(fill = "Correlation")

g2 <- corr_data %>%
  select(starts_with("prop_vh")) %>%
  setNames(c("Police", "Survey victims", "Survey places", "Survey reported")) %>%
  cor() %>%
  ggcorrplot(type = "upper") +
  viridis::scale_fill_viridis(limits = c(-1, 1), direction = -1) +
  labs(fill = "Correlation")

g3 <- corr_data %>%
  select(starts_with("prop_pers")) %>%
  setNames(c("Police", "Survey victims", "Survey places", "Survey reported")) %>%
  cor() %>%
  ggcorrplot(type = "upper") +
  viridis::scale_fill_viridis(limits = c(-1, 1), direction = -1) +
  labs(fill = "Correlation")


ggarrange(g1 +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank()),
          g2 +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank()),
          g3 +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank()),
          nrow = 3,
          common.legend = T,
          labels = c("Violence",
                     "Property - vehicle",
                     "Proporty - personal"),
          font.label = list(size = 10, align = "right"))




# Quasi-simplex models ----------------------------------------------------



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
  mutate(group = ifelse(str_detect(key, "\\.r\\.w"), "Survey", "Police"),
         var = str_remove_all(key, "_mean|_sd|\\.r\\.w")) %>%
  ggplot(aes(as.factor(year), value, color = var,
             group = var)) +
  geom_line(size = 1.5) +
  facet_wrap(~ group) +
  theme_bw() +
  labs(color = "Variable",
       x = "Year",
       y = "Value")



qs_rel %>%
  gather(-year, key = key, value = value) %>%
  mutate(group = ifelse(str_detect(key, "\\.r\\.w"), "Survey", "Police"),
         var = str_remove_all(key, "_mean|_sd|\\.r\\.w")) %>%
  group_by(group, var) %>%
  summarise(reliability = mean(value)) %>%
  group_by(group) %>%
  mutate(reliability_source = mean(reliability))



data_s2 <- barca_2y %>%
  select(id, year, vars_int3)

dat_s2l <- data_s2 %>%
  mutate(year = str_remove_all(year, "20|-.+")) %>%
  pivot_wider(
    values_from = property_vh:violence.p.w,
    names_sep = "_",
    names_from = year) %>%
  rename_all(~str_remove_all(., "erty|onal|ence"))


dat_s2l %>%
  select(-id) %>%
  cor() %>%
  corrplot()







# make quasi-simplex for "corrected" survey estimates
# run only once


#
# dat_s3l <- dat_s2l %>%
#   select(matches(vars_int4)) %>%
#   mutate_all(~log(. + 1))
#
#
# qs <- function(var) {
#   model <- str_c("t1 =~ ", var, "_15\n",
#                  "t2 =~ ", var, "_17\n",
#                  "t3 =~ ", var, "_19\n\n",
#                  "t2 ~ t1 \nt3 ~ t2\n\n",
#                  var, "_15 ~~ a*", var, "_15\n",
#                  var, "_17 ~~ a*", var, "_17\n",
#                  var, "_19 ~~ a*", var, "_19")
#
#   bsem(model, data = dat_s3l,
#        burnin = 8000, sample = 2000, n.chains = 8)
# }
#
# res_qs2 <- map(vars_int4, qs)
# save(res_qs2, file = "./output/quasi_simple_extra.RData")

load("./output/quasi_simple_extra.RData")

qs_rel2 <- map(res_qs2, function(x) summary(x, standardized = TRUE) %>%
                 .[1:3, 7] %>% as.numeric()
) %>%
  reduce(cbind) %>%
  as_tibble() %>%
  setNames(vars_int4) %>%
  mutate(year = c(15, 17, 19))


full_join(qs_rel, qs_rel2, by = "year") %>%
  gather(-year, key = key, value = value) %>%
  filter(str_detect(key, "f\\.w|r\\.w|p\\.w")) %>%
  mutate(group = case_when(str_detect(key, "\\.r\\.w") ~ "Original survey",
                           str_detect(key, "\\.p\\.w") ~ "Survey corrected",
                           str_detect(key, "\\.f\\.w") ~ "Survey places"),
         var = str_remove_all(key, "\\.f\\.w|\\.p\\.w|\\.r\\.w")) %>%
  ggplot(aes(as.factor(year), value,
             group = group, color = group)) +
  geom_line(size = 1.5) +
  facet_wrap(~var) +
  theme_bw() +
  labs(color = "Data",
       x = "Year",
       y = "Reliability")




# multi-item QS -----------------------------------------------------------


# run only once
#
# # take log

# property vehicle
model <- c("t1 =~ 1*prop_vh.f.w_15 + 1*prop_vh.p.w_15 + 1*prop_vh.r.w_15
            t2 =~ 1*prop_vh.f.w_17 + 1*prop_vh.p.w_17 + 1*prop_vh.r.w_17
            t3 =~ 1*prop_vh.f.w_19 + 1*prop_vh.p.w_19 + 1*prop_vh.r.w_19

            t2 ~ t1
            t3 ~ t2
           ")
m1_propvh <- bsem(model, data = b2yw2,
                  burnin = 8000, sample = 2000, n.chains = 8)

summary(m1_propvh, standardized = TRUE)

# property personal
model <- c("t1 =~ 1*prop_pers.f.w_15 + 1*prop_pers.p.w_15 + 1*prop_pers.r.w_15
            t1 =~ 1*prop_pers.f.w_17 + 1*prop_pers.p.w_17 + 1*prop_pers.r.w_17
            t1 =~ 1*prop_pers.f.w_19 + 1*prop_pers.p.w_19 + 1*prop_pers.r.w_19

            t2 ~ t1
            t3 ~ t2
           ")


m1_proppers <- bsem(model, data = b2yw2,
                    burnin = 8000, sample = 2000, n.chains = 8)

summary(m1_proppers, standardized = TRUE)


# violence
model <- c("t1 =~ 1*viol.f.w_15 + 1*viol.p.w_15 + 1*viol.r.w_15
            t1 =~ 1*viol.f.w_17 + 1*viol.p.w_17 + 1*viol.r.w_17
            t1 =~ 1*viol.f.w_19 + 1*viol.p.w_19 + 1*viol.r.w_19

            t2 ~ t1
            t3 ~ t2
           ")
m1_viol <- bsem(model, data = b2yw2,
                burnin = 8000, sample = 2000, n.chains = 8)

summary(m1_viol, standardized = TRUE)


qs_m1 <- list(m1_propvh, m1_proppers, m1_viol)

save(qs_m1, file = "./output/qs_m1.RData")



summary(qs_m1[[3]], standardized = T)


# Models 2 with method effect ---------------------------------------------

# property vehicle
model <- c("t1 =~ 1*prop_vh.f.w_15 + 1*prop_vh.p.w_15 + 1*prop_vh.r.w_15
            t2 =~ 1*prop_vh.f.w_17 + 1*prop_vh.p.w_17 + 1*prop_vh.r.w_17
            t3 =~ 1*prop_vh.f.w_19 + 1*prop_vh.p.w_19 + 1*prop_vh.r.w_19

            t2 ~ t1
            t3 ~ t2

            m_vic =~ 1*prop_vh.p.w_15 + 1*prop_vh.p.w_17 + 1*prop_vh.p.w_19
            m_rep =~ 1*prop_vh.r.w_15 + 1*prop_vh.r.w_17 + 1*prop_vh.r.w_19
            m_loc =~ 1*prop_vh.f.w_15 + 1*prop_vh.f.w_17 + 1*prop_vh.f.w_19

            m_vic ~~ 0*m_rep 0*m_loc 0*t1
            m_rep ~~ 0*m_loc 0*t1
            m_loc ~~ 0*t1
           ")


m2_propvh <- bsem(model, data = b2yw2,
                  burnin = 8000, sample = 2000, n.chains = 8)

summary(m2_propvh, standardized = TRUE)



# property personal
model <- c("t1 =~ 1*prop_pers.f.w_15 + 1*prop_pers.p.w_15 + 1*prop_pers.r.w_15
            t1 =~ 1*prop_pers.f.w_17 + 1*prop_pers.p.w_17 + 1*prop_pers.r.w_17
            t1 =~ 1*prop_pers.f.w_19 + 1*prop_pers.p.w_19 + 1*prop_pers.r.w_19

            t2 ~ t1
            t3 ~ t2

            m_vic =~ 1*prop_pers.p.w_15 + 1*prop_pers.p.w_17 + 1*prop_pers.p.w_19
            m_rep =~ 1*prop_pers.r.w_15 + 1*prop_pers.r.w_17 + 1*prop_pers.r.w_19
            m_loc =~ 1*prop_pers.f.w_15 + 1*prop_pers.f.w_17 + 1*prop_pers.f.w_19

            m_vic ~~ 0*m_rep 0*m_loc 0*t1
            m_rep ~~ 0*m_loc 0*t1
            m_loc ~~ 0*t1
           ")
m2_proppers <- bsem(model, data = b2yw2,
                    burnin = 8000, sample = 2000, n.chains = 8)

summary(m2_proppers, standardized = TRUE)





# violence
model <- c("t1 =~ 1*viol.f.w_15 + 1*viol.p.w_15 + 1*viol.r.w_15
            t1 =~ 1*viol.f.w_17 + 1*viol.p.w_17 + 1*viol.r.w_17
            t1 =~ 1*viol.f.w_19 + 1*viol.p.w_19 + 1*viol.r.w_19

            t2 ~ t1
            t3 ~ t2

            m_vic =~ 1*viol.p.w_15 + 1*viol.p.w_17 + 1*viol.p.w_19
            m_rep =~ 1*viol.r.w_15 + 1*viol.r.w_17 + 1*viol.r.w_19
            m_loc =~ 1*viol.f.w_15 + 1*viol.f.w_17 + 1*viol.f.w_19


            m_vic ~~ 0*m_rep 0*m_loc 0*t1
            m_rep ~~ 0*m_loc 0*t1
            m_loc ~~ 0*t1
           ")
m2_viol <- bsem(model, data = b2yw2,
                burnin = 8000, sample = 2000, n.chains = 8)

summary(m2_viol, standardized = TRUE)


qs_m2 <- list(m2_propvh, m2_proppers, m2_viol)

save(qs_m2, file = "./output/qs_m2.RData")



m_vic =~ 1*viol.p.w + 1*prop_vh.p.w + 1*prop_pers.p.w
m_rep =~ 1*viol.r.w + 1*prop_vh.r.w + 1*prop_pers.r.w
m_loc =~ 1*viol.f.w + 1*prop_vh.f.w + 1*prop_pers.f.w





# MTMM model --------------------------------------------------------------




model <- c("t_viol =~ 1*viol.f.w + 1*viol.p.w + 1*viol.r.w
            t_vh =~ 1*prop_vh.f.w + 1*prop_vh.p.w + 1*prop_vh.r.w
            t_pers =~ 1*prop_pers.f.w + 1*prop_pers.p.w + 1*prop_pers.r.w

            m_vic =~ 1*viol.p.w + 1*prop_vh.p.w + 1*prop_pers.p.w
            m_rep =~ 1*viol.r.w + 1*prop_vh.r.w + 1*prop_pers.r.w
            m_loc =~ 1*viol.f.w + 1*prop_vh.f.w + 1*prop_pers.f.w

            m_vic ~~ 0*m_rep 0*m_loc 0*t_viol 0*t_vh 0*t_pers
            m_rep ~~ 0*m_loc 0*t_viol 0*t_vh 0*t_pers
            m_loc ~~ 0*t_viol 0*t_vh 0*t_pers

           ")
m2_viol <- bsem(model, data = dat_s3l,
                burnin = 8000, sample = 2000, n.chains = 8)

summary(m2_viol, standardized = TRUE)


qs_m2 <- list(m2_propvh, m2_proppers, m2_viol)

save(qs_m2, file = "./output/qs_m2.RData")

