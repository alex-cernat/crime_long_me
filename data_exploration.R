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
pkg <- c("knitr", "tidyverse", "lavaan", "corrplot",
         "blavaan", "rstan")

# sapply(pkg, install.packages)
sapply(pkg, library, character.only = T)


# rstan set
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# load data ---------------------------------------------------------------

barca_raw <- read_csv("./data/raw/neighbourhoods.csv")
barca_2y <- read_csv("./data/clean/neighbourhoods_2years.csv")


# data cleaning -----------------------------------------------------------

barca_2y <- barca_2y %>%
  mutate(id = as.factor(Barri) %>% as.numeric()) %>%
  rename_all(~str_to_lower(.)) %>%
  rename(row = x1, year = any) %>%
  select(row, id, everything())


# descriptives ------------------------------------------------------------


vars_int <- str_c(c("property_vh", "property_personal", "violence") %>%
                    rep(each = 2),
                  c("", ".r.w") %>%
                    rep(3))

count(barca_2y, year)

# change in time

dat_s <- barca_2y %>%
  select(id, year, vars_int)


# variances
dat_s %>%
  select(-id) %>%
  group_by(year) %>%
  summarise_all(list(mean = ~mean(.),
                     sd = ~sd(.))) %>%
  gather(-year, key = key, value = value) %>%
  mutate(stat = ifelse(str_detect(key, "mean$"), "Mean", "SD"),
         group = ifelse(str_detect(key, "\\.r\\.w"), "Survey", "Police"),
         var = str_remove_all(key, "_mean|_sd|\\.r\\.w")) %>%
  ggplot(aes(year, value, linetype = var,
             color = group, group = as.factor(key))) +
  geom_line(size = 1.5) +
  facet_wrap(~ stat, scales = "free_y") +
  theme_bw() +
  labs(color = "Data source",
       linetype = "Variable",
       x = "Year",
       y = "Value")

# corelations

b2y_long <- barca_2y %>%
  select(id, year, vars_int) %>%
  mutate(year = str_remove_all(year, "20|-.+")) %>%
  pivot_wider(
    values_from = property_vh:violence.r.w,
      names_sep = "_",
      names_from = year) %>%
  rename_all(~str_remove_all(., "erty|onal|ence"))


b2y_long %>%
  select(-id) %>%
  cor() %>%
  corrplot()






# auto-regressive models --------------------------------------------------

vars_int2 <- str_remove_all(vars_int, "erty|onal|ence")
#
# autoreg <- function(var) {
#   model <- str_c(var, "_17 ~ ", var, "_15\n",
#                  var, "_19 ~ ", var, "_17")
#
#   sem(model, data = b2y_long)
# }
#
# res_autoreg <- map(vars_int2, autoreg)
#





# model <- c("t1 =~ prop_vh_15
#             t2 =~ prop_vh_17
#             t3 =~ prop_vh_19
#
#             t2 ~ t1
#             t3 ~ t2
#
#            prop_vh_15 ~~ a*prop_vh_15
#            prop_vh_17 ~~ a*prop_vh_17
#            prop_vh_19 ~~ a*prop_vh_19")
# m1 <- sem(model, data = b2y_long)
# m1b <- bsem(model, data = b2y_long)
#
# summary(m1)
# summary(m1b)


# takes a while to run so just re-load

# make log
# b2y_long2 <- b2y_long %>%
#   select(matches(vars_int2)) %>%
#   mutate_all(~log(. + 1))
#
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






vars_int3 <- str_c(c("property_vh", "property_personal", "violence") %>%
                     rep(each = 4),
                   c("", ".r.w", ".f.w", ".p.w") %>%
                     rep(3))

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









vars_int4 <- dat_s2l %>%
  select(matches("\\.f.\\w"), matches("\\.p\\.w")) %>%
  names() %>%
  str_remove("_1[1-9]$") %>%
  unique()

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



vars_int5 <- str_c(c("prop_vh", "prop_pers", "viol") %>%
                     rep(each = 2),
                   c("", ".p.w") %>%
                     rep(3))


dat_s2l %>%
  select(matches(str_c(vars_int5, "_"))) %>%
  cor() %>%
  corrplot()




# run only once
#
# # take log
# dat_s3l <- dat_s2l %>%
#   mutate_at(vars(starts_with("prop_vh")),
#             ~ log(. + 1))
#
# model <- c("t1 =~ 1*prop_vh_15 + 1*prop_vh.f.w_15
#             t2 =~ 1*prop_vh_17 + 1*prop_vh.f.w_17
#             t3 =~ 1*prop_vh_19 + 1*prop_vh.f.w_19
#
#             t2 ~ t1
#             t3 ~ t2
#            ")
# m1_propvh <- bsem(model, data = dat_s3l,
#                   burnin = 8000, sample = 2000, n.chains = 8)
#
# summary(m1_propvh, standardized = TRUE)
#
#
#
# dat_s3l <- dat_s2l %>%
#   mutate_at(vars(starts_with("prop_pers")),
#             ~ log(. + 1))
#
# model <- c("t1 =~ 1*prop_pers_15 + 1*prop_pers.f.w_15
#             t2 =~ 1*prop_pers_17 + 1*prop_pers.f.w_17
#             t3 =~ 1*prop_pers_19 + 1*prop_pers.f.w_19
#
#             t2 ~ t1
#             t3 ~ t2
#            ")
#
# # try to take the log as model is not estimating properly
# m1_proppers <- bsem(model, data = dat_s3l,
#                     burnin = 8000, sample = 2000, n.chains = 8)
#
# summary(m1_proppers, standardized = TRUE)
#
#
# dat_s3l <- dat_s2l %>%
#   mutate_at(vars(starts_with("viol")),
#             ~ log(. + 1))
#
# model <- c("t1 =~ 1*viol_15 + 1*viol.f.w_15
#             t2 =~ 1*viol_17 + 1*viol.f.w_17
#             t3 =~ 1*viol_19 + 1*viol.f.w_19
#
#             t2 ~ t1
#             t3 ~ t2
#            ")
# m1_viol <- bsem(model, data = dat_s3l,
#                 burnin = 8000, sample = 2000, n.chains = 8)
#
# summary(m1_viol, standardized = TRUE)
#
#
# qs_m1 <- list(m1_propvh, m1_proppers, m1_viol)
#
# save(qs_m1, file = "./output/qs_m1.RData")


summary(qs_m1[[3]], standardized = T)





dat_s3l <- dat_s2l %>%
  mutate_at(vars(starts_with("prop_vh")),
            ~ log(. + 1))

model <- c("t1 =~ 1*prop_vh_15 + 1*prop_vh.f.w_15
            t2 =~ 1*prop_vh_17 + 1*prop_vh.f.w_17
            t3 =~ 1*prop_vh_19 + 1*prop_vh.f.w_19

            t2 ~ t1
            t3 ~ t2

            pol =~ 1*prop_vh_15 + 1*prop_vh_17 + 1*prop_vh_19
            surv =~ 1*prop_vh.f.w_15 + 1*prop_vh.f.w_17 + 1*prop_vh.f.w_19

            pol ~~ 0*surv
            pol ~~ 0*t1
            surv ~~ 0*t1
           ")


m2_propvh <- bsem(model, data = dat_s3l,
                  burnin = 8000, sample = 2000, n.chains = 8)

summary(m2_propvh, standardized = TRUE)







dat_s3l <- dat_s2l %>%
  mutate_at(vars(starts_with("prop_pers")),
            ~ log(. + 1))


model <- c("t1 =~ 1*prop_pers_15 + 1*prop_pers.f.w_15
            t2 =~ 1*prop_pers_17 + 1*prop_pers.f.w_17
            t3 =~ 1*prop_pers_19 + 1*prop_pers.f.w_19

            t2 ~ t1
            t3 ~ t2


            pol =~ 1*prop_pers_15 + 1*prop_pers_17 + 1*prop_pers_19
            surv =~ 1*prop_pers.f.w_15 + 1*prop_pers.f.w_17 + 1*prop_pers.f.w_19

            pol ~~ 0*surv
            pol ~~ 0*t1
            surv ~~ 0*t1
           ")
m2_proppers <- bsem(model, data = dat_s3l,
                    burnin = 8000, sample = 2000, n.chains = 8)

summary(m2_proppers, standardized = TRUE)







dat_s3l <- dat_s2l %>%
  mutate_at(vars(starts_with("viol")),
            ~ log(. + 1))


model <- c("t1 =~ 1*viol_15 + 1*viol.f.w_15
            t2 =~ 1*viol_17 + 1*viol.f.w_17
            t3 =~ 1*viol_19 + 1*viol.f.w_19

            t2 ~ t1
            t3 ~ t2

            pol =~ 1*viol_15 + 1*viol_17 + 1*viol_19
            surv =~ 1*viol.f.w_15 + 1*viol.f.w_17 + 1*viol.f.w_19


            pol ~~ 0*surv
            pol ~~ 0*t1
            surv ~~ 0*t1
           ")
m2_viol <- bsem(model, data = dat_s3l,
                burnin = 8000, sample = 2000, n.chains = 8)

summary(m2_viol, standardized = TRUE)


qs_m2 <- list(m2_propvh, m2_proppers, m2_viol)

save(qs_m2, file = "./output/qs_m2.RData")


