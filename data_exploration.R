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
         "blavaan", "rstan", "viridis", "ggpubr",
         "MplusAutomation")

# sapply(pkg, install.packages)
sapply(pkg, library, character.only = T)


# rstan set
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
future::plan("multiprocess")

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
                    rep(each = 5),
                  c("", ".r.wo", ".f.wo", ".p.wo", ".rp.wo") %>%
                    rep(3))

vars_int2 <- str_remove_all(vars_int, "erty|onal|ence")


#
#
# vars_int_w2 <- str_c(c("property_vh", "property_personal", "violence") %>%
#                        rep(each = 2),
#                      c(".f.w2", ".p.w2") %>%
#                        rep(3))
#
# vars_int2_w2 <- str_remove_all(vars_int_w2, "erty|onal|ence")
#
#
#
# vars_int_wo <- str_c(c("property_vh", "property_personal", "violence") %>%
#                        rep(each = 3),
#                      rep(c(".r.wo", ".f.wo", ".p.wo"), 3))
#
# vars_int2_wo <- str_remove_all(vars_int_wo, "erty|onal|ence")
#




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
         group = case_when(
           str_detect(key, "\\.r\\.w") ~ "Survey victim",
           str_detect(key, "\\.p\\.w") ~ "Survey reported",
           str_detect(key, "\\.f\\.w") ~ "Survey places",
           str_detect(key, "\\.rp\\.w") ~ "Survey victims reported",
           TRUE ~ "Police "
           ),
         topic = case_when(
           str_detect(key, "property_personal") ~ "Property personal",
           str_detect(key, "property_vh") ~ "Property household/vehicle",
           TRUE ~ "Violence"),
         var = str_remove_all(key, "_mean|_sd|\\.r\\.wo"))

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


# ggsave("./output/figs/avg_sd_change.png")
# write_csv(descriptives, "./output/tabs/descriptive_change.csv")


# overall correlation matrix
# correlations


corr_data <- barca_6y %>%
  select(id, vars_int) %>%
  rename_all(~str_remove_all(., "erty|onal|ence")) %>%
  ungroup() %>%
  select(-id) %>%
  mutate_all(~log(. + 1))


pol_vars <- c("prop_vh", "prop_pers", "viol")

s_vars <- map(pol_vars, function(x) {
  names(corr_data) %>% str_subset(x) %>% .[-1]
  }
  )

cor_vars <- tibble(x = pol_vars %>% rep(each = 4),
                   y = unlist(s_vars))

cors_data <- pmap_df(cor_vars, function(x, y) {

  cor_info <- cor.test(corr_data[[x]], corr_data[[y]])

  tibble(est = cor_info$estimate,
         lci = cor_info$conf.int[1],
         uci = cor_info$conf.int[2])

  }) %>%
  cbind(cor_vars)



cors_data %>%
  mutate(group = case_when(
    str_detect(y, "\\.r\\.w") ~ "Victim residence",
    str_detect(y, "\\.p\\.w") ~ "Offence location (report)",
    str_detect(y, "\\.f\\.w") ~ "Offence location",
    str_detect(y, "\\.rp\\.w") ~ "Victim residence (report)",
    TRUE ~ "Police"
    ),
         topic = case_when(
           str_detect(x, "prop_pers") ~ "Property (personal)",
           str_detect(x, "prop_vh") ~ "Property (household/vehicle)",
           TRUE ~ "Violence"
           )) %>%
  ggplot(aes(est, topic, color = group)) +
  geom_point(size = 3, alpha = 0.8,
             position = position_dodge(0.5)) +
  geom_linerange(aes(xmax = uci, xmin = lci),
                 position = position_dodge(0.5)) +
  theme_bw() +
  viridis::scale_color_viridis(discrete = T,
                               guide = guide_legend(reverse = TRUE)) +
  labs(x = "Correlation with police data",
       y = "Type of crime", color = "Measure")

ggsave("./output/figs/cor_police_overall.png")


# Models with original weights --------------------------------------------------


# MTMM with 4 methods -----------------------------------------------------

# make data only with the vars we want


b6yw2_wo <- barca_6y %>%
  rename_all(~str_remove_all(., "erty|onal|ence")) %>%
  select(all_of(vars_int2)) %>%
  mutate_all(~log(. + 1))

# save data to mplus
# b6yw2_wo %>%
#   rename_all(~str_replace_all(., "\\.", "_") %>%
#                str_remove_all(., "_pers|_wo")) %>%
#   MplusAutomation::prepareMplusData("./mplus/b6yw2_wo.dat")


# correlation matrix
b6yw2_wo %>%
  cor() %>%
  ggcorrplot() +
  viridis::scale_fill_viridis(direction = -1)




# model <- c("t_viol =~ 1*viol.f.wo + 1*viol.p.wo + 1*viol.r.wo + 1*viol.rp.wo
#             t_vh =~ 1*prop_vh.f.wo + 1*prop_vh.p.wo + 1*prop_vh.r.wo + 1*prop_vh.rp.wo
#             t_pers =~ 1*prop_pers.f.wo + 1*prop_pers.p.wo + 1*prop_pers.r.wo + 1*prop_pers.rp.wo
#
#             m_rep =~ 1*viol.p.wo + 1*prop_vh.p.wo + 1*prop_pers.p.wo
#             m_vic =~ 1*viol.r.wo + 1*prop_vh.r.wo + 1*prop_pers.r.wo
#             m_loc =~ 1*viol.f.wo + 1*prop_vh.f.wo + 1*prop_pers.f.wo
#             m_vicrep =~ 1*viol.rp.wo + 1*prop_vh.rp.wo + 1*prop_pers.rp.wo
#
#             m_vic ~~ 0*m_rep
#             m_vic ~~ 0*m_loc
#             m_vic ~~ 0*m_vicrep
#             m_vic ~~ 0*t_viol
#             m_vic ~~ 0*t_vh
#             m_vic ~~ 0*t_pers
#
#
#             m_rep ~~ 0*m_loc
#             m_rep ~~ 0*m_vicrep
#             m_rep ~~ 0*t_viol
#             m_rep ~~ 0*t_vh
#             m_rep ~~ 0*t_pers
#
#             m_loc ~~ 0*m_vicrep
#             m_loc ~~ 0*t_viol
#             m_loc ~~ 0*t_vh
#             m_loc ~~ 0*t_pers
#
#             m_vicrep ~~ 0*t_viol
#             m_vicrep ~~ 0*t_vh
#             m_vicrep ~~ 0*t_pers
#            ")
# mtmm_wo <- bsem(model, data = b6yw2_wo,
#                 burnin = 50000, sample = 5000, n.chains = 8)
#
# summary(mtmm_wo, standardized = TRUE)
#
# save(mtmm_wo, file = "./output/mtmm_wo.RData")

load("./output/mtmm_wo.RData")



summary(mtmm_wo, standardized = T)


mtmm_wo_est <- lavaan::parameterestimates(mtmm_wo, standardized = T)

mtmm_wo_qual <- mtmm_wo_est %>%
  filter(op == "=~" | op == "~~") %>%
  filter(!(op == "~~" & str_detect(lhs, "t_|m_"))) %>%
  mutate(Trait = case_when(str_detect(rhs, "viol") ~ "Violence",
                           str_detect(rhs, "vh") ~ "Property (household/vehicle)",
                           str_detect(rhs, "pers") ~ "Property (personal)"),
         group = case_when(
           str_detect(rhs, "\\.r\\.") ~ "Victim residence",
           str_detect(rhs, "\\.p") ~ "Offence location (report)",
           str_detect(rhs, "\\.f") ~ "Offence location",
           str_detect(rhs, "\\.rp") ~ "Victim residence (report)",
           TRUE ~ "Police"),
         source = case_when(str_detect(lhs, "t_") ~ "Trait",
                            str_detect(lhs, "m_") ~ "Method",
                            TRUE ~ "Random error"),
         qual = ifelse(source != "Random error", std.all^2, std.all)) %>%
  select(-op, -est, -std.lv, -std.nox)


mtmm_wo_qual %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  ggplot(aes(group, qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait, nrow = 3) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Question type",
       fill = "Source") +
  theme_bw()

ggsave("./output/figs/mtmm_qual_overall_wo.png", height = 8)










# Models 2 with method effect ---------------------------------------------




# make wide data

b2yw_wo <- barca_2y %>%
  rename_all(~str_remove_all(., "erty|onal|ence")) %>%
  select(id, year, vars_int2) %>%
  mutate(year = str_remove_all(year, "20|-.+")) %>%
  pivot_wider(
    values_from = prop_vh:viol.rp.wo,
    names_sep = "_",
    names_from = year) %>%
  mutate_at(vars(-id),
            ~log(. + 1))


# save data to mplus
# b2yw_wo %>%
#   rename_all(~str_replace_all(., "\\.", "_") %>%
#                str_remove_all(., "_pers|_wo")) %>%
#   MplusAutomation::prepareMplusData("./mplus/b2yw_wo.dat")



#
#
# # property vehicle
# model <- c("t1 =~ 1*prop_vh.f.wo_15 + 1*prop_vh.p.wo_15 + 1*prop_vh.r.wo_15 + 1*prop_vh.rp.wo_15
#             t2 =~ 1*prop_vh.f.wo_17 + 1*prop_vh.p.wo_17 + 1*prop_vh.r.wo_17 + 1*prop_vh.rp.wo_17
#             t3 =~ 1*prop_vh.f.wo_19 + 1*prop_vh.p.wo_19 + 1*prop_vh.r.wo_19 + 1*prop_vh.rp.wo_19
#
#             t2 ~ t1
#             t3 ~ t2
#
#             m_rep =~ 1*prop_vh.p.wo_15 + 1*prop_vh.p.wo_17 + 1*prop_vh.p.wo_19
#             m_vic =~ 1*prop_vh.r.wo_15 + 1*prop_vh.r.wo_17 + 1*prop_vh.r.wo_19
#             m_loc =~ 1*prop_vh.f.wo_15 + 1*prop_vh.f.wo_17 + 1*prop_vh.f.wo_19
#             m_vicrep =~ 1*prop_vh.rp.wo_15 + 1*prop_vh.rp.wo_17 + 1*prop_vh.rp.wo_19
#
#             m_vic ~~ 0*m_rep
#             m_vic ~~ 0*m_loc
#             m_vic ~~ 0*m_vicrep
#             m_vic ~~ 0*t1
#             m_rep ~~ 0*m_loc
#             m_rep ~~ 0*m_vicrep
#             m_rep ~~ 0*t1
#             m_loc ~~ 0*m_vicrep
#             m_loc ~~ 0*t1
#             m_vicrep ~~ 0*t1
#            ")
#
#
# m2_propvh_wo <- bsem(model, data = b2yw_wo,
#                      burnin = 50000, sample = 5000, n.chains = 8)
#
# summary(m2_propvh_wo, standardized = TRUE)
#
#
#
# # property personal
# model <- c("t1 =~ 1*prop_pers.f.wo_15 + 1*prop_pers.p.wo_15 + 1*prop_pers.r.wo_15 + 1*prop_pers.rp.wo_15
#             t2 =~ 1*prop_pers.f.wo_17 + 1*prop_pers.p.wo_17 + 1*prop_pers.r.wo_17 + 1*prop_pers.rp.wo_17
#             t3 =~ 1*prop_pers.f.wo_19 + 1*prop_pers.p.wo_19 + 1*prop_pers.r.wo_19 + 1*prop_pers.rp.wo_19
#
#             t2 ~ t1
#             t3 ~ t2
#
#             m_rep =~ 1*prop_pers.p.wo_15 + 1*prop_pers.p.wo_17 + 1*prop_pers.p.wo_19
#             m_vic =~ 1*prop_pers.r.wo_15 + 1*prop_pers.r.wo_17 + 1*prop_pers.r.wo_19
#             m_loc =~ 1*prop_pers.f.wo_15 + 1*prop_pers.f.wo_17 + 1*prop_pers.f.wo_19
#             m_vicrep =~ 1*prop_pers.rp.wo_15 + 1*prop_pers.rp.wo_17 + 1*prop_pers.rp.wo_19
#
#             m_vic ~~ 0*m_rep
#             m_vic ~~ 0*m_loc
#             m_vic ~~ 0*m_vicrep
#             m_vic ~~ 0*t1
#             m_rep ~~ 0*m_loc
#             m_rep ~~ 0*m_vicrep
#             m_rep ~~ 0*t1
#             m_loc ~~ 0*m_vicrep
#             m_loc ~~ 0*t1
#             m_vicrep ~~ 0*t1
#            ")
# m2_proppers_wo <- bsem(model, data = b2yw_wo,
#                        burnin = 50000, sample = 5000, n.chains = 8)
#
# summary(m2_proppers_wo, standardized = TRUE)
#
#
#
#
#
# # violence
# model <- c("t1 =~ 1*viol.f.wo_15 + 1*viol.p.wo_15 + 1*viol.r.wo_15 + 1*viol.rp.wo_15
#             t2 =~ 1*viol.f.wo_17 + 1*viol.p.wo_17 + 1*viol.r.wo_17 + 1*viol.rp.wo_17
#             t3 =~ 1*viol.f.wo_19 + 1*viol.p.wo_19 + 1*viol.r.wo_19 + 1*viol.rp.wo_19
#
#             t2 ~ t1
#             t3 ~ t2
#
#             m_rep =~ 1*viol.p.wo_15 + 1*viol.p.wo_17 + 1*viol.p.wo_19
#             m_vic =~ 1*viol.r.wo_15 + 1*viol.r.wo_17 + 1*viol.r.wo_19
#             m_loc =~ 1*viol.f.wo_15 + 1*viol.f.wo_17 + 1*viol.f.wo_19
#             m_vicrep =~ 1*viol.rp.wo_15 + 1*viol.rp.wo_17 + 1*viol.rp.wo_19
#
#
#             m_vic ~~ 0*m_rep
#             m_vic ~~ 0*m_loc
#             m_vic ~~ 0*m_vicrep
#             m_vic ~~ 0*t1
#             m_rep ~~ 0*m_loc
#             m_rep ~~ 0*m_vicrep
#             m_rep ~~ 0*t1
#             m_loc ~~ 0*m_vicrep
#             m_loc ~~ 0*t1
#             m_vicrep ~~ 0*t1
#            ")
# m2_viol_wo <- bsem(model, data = b2yw_wo,
#                    burnin = 50000, sample = 5000, n.chains = 8)
#
# summary(m2_viol_wo, standardized = TRUE)
#
#
# qs_m2_wo <- list(m2_propvh_wo, m2_proppers_wo, m2_viol_wo)
#
# save(qs_m2_wo, file = "./output/qs_m2_wo.RData")



load("./output/qs_m2_wo.RData")



summary(qs_m2_wo[[3]], standardized = T)


qual_qsm2_wo <- qs_m2_wo %>%
  map_df(function(x) {
    parameterestimates(x, standardized = T) %>%
      filter(op == "=~" | op == "~~") %>%
      filter(!std.all %in% c(0, 1)) %>%
      filter(!(op == "~~" & str_detect(lhs, "^m_|t[1-9]")))  %>%
      mutate(
        group = case_when(
          str_detect(rhs, "\\.r\\.") ~ "Victim residence",
          str_detect(rhs, "\\.p") ~ "Offence location (report)",
          str_detect(rhs, "\\.f") ~ "Offence location",
          str_detect(rhs, "\\.rp") ~ "Victim residence (report)",
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
                           str_detect(rhs, "vh") ~ "Property (household/vehicle)",
                           str_detect(rhs, "pers") ~ "Property (personal)"),
         year = case_when(year == "15" ~ "15/16",
                          year == "17" ~ "17/18",
                          year == "19" ~ "19/20"))




qual_qsm2_wo %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  ggplot(aes(year, qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_grid(Trait~group) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Time",
       fill = "Source") +
  theme_bw()

ggsave("./output/figs/qsm2_quality_wo.png", height = 6)

qual_qsm2_wo %>%
  group_by(group, source) %>%
  summarise(Quality = mean(qual) %>% round(2)) %>%
  rename(Measure = group, Component = source) %>%
  mutate(Component = as.factor(Component) %>%
           fct_relevel("Trait")) %>%
  arrange(Measure, Component) %>%
  write_csv("./output/tabs/sum_qs_m2_wo.csv")


var_qs_m2_wo <- qs_m2_wo %>%
  map_df(function(x) {
    parameterestimates(x, se = T) %>%
      filter(op == "~~", str_detect(lhs, "m_|t")) %>%
      filter(!est %in% c(0, 1))
  }) %>%
  mutate(Dimension = rep(c("Property (household/vehicle)",
                            "Property (personal)",
                            "Violence"),
                          each = 7),
         Variable = case_when(
           str_detect(lhs, "m_vic$") ~ "M: Victim residence",
           str_detect(lhs, "m_rep") ~ "M: Offence location (report)",
           str_detect(lhs, "m_loc") ~ "M: Offence location",
           str_detect(lhs, "m_vicrep") ~ "M: Victim residence (report)",
           str_detect(lhs, "t1") ~ "T1 (15/16)",
           str_detect(lhs, "t2") ~ "T2 (17/18)",
           str_detect(lhs, "t3") ~ "T3 (19/20)"
         )) %>%
  select(Dimension, Variable, est, se, ci.lower, ci.upper)

var_qs_m2_wo %>% write_csv("./output/tabs/vars_qs_m2_wo.csv")


# Quasi-simplex -----------------------------------------------------------

# qs <- function(var) {
#   model <- str_c("t1 =~ ", var, "_15\n",
#                  "t2 =~ ", var, "_17\n",
#                  "t3 =~ ", var, "_19\n\n",
#                  "t2 ~ t1 \nt3 ~ t2\n\n",
#                  var, "_15 ~~ a*", var, "_15\n",
#                  var, "_17 ~~ a*", var, "_17\n",
#                  var, "_19 ~~ a*", var, "_19")
#
#   bsem(model, data = b2yw_wo,
#        burnin = 50000, sample = 5000, n.chains = 8)
# }
#
# res_qs_wo <- map(vars_int2, qs)
# save(res_qs_wo, file = "./output/quasi_simple_wo.RData")

load("./output/quasi_simple_wo.RData")


qs_rel_wo <- map(res_qs_wo, function(x) summary(x, standardized = TRUE) %>%
                   .[1:3, 7] %>% as.numeric()
) %>%
  reduce(cbind) %>%
  as_tibble() %>%
  setNames(vars_int2) %>%
  mutate(year = c(15, 17, 19))


qs_rel_wo %>%
  gather(-year, key = key, value = value) %>%
  filter(str_detect(key, "\\.f|\\.r|\\.p")) %>%
  mutate(group = case_when(str_detect(key, "\\.r\\.") ~ "Victim residence",
                           str_detect(key, "\\.p") ~ "Offence location (report)",
                           str_detect(key, "\\.f") ~ "Offence location",
                           str_detect(key, "\\.rp") ~ "Victim residence (report)",
                           TRUE ~ "Police"),
         var = str_remove_all(key, "\\.f|\\.p|\\.rp|\\.r"),
         topic = case_when(
           str_detect(key, "prop_pers") ~ "Property (personal)",
           str_detect(key, "prop_vh") ~ "Property (household/vehicle)",
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


ggsave("./output/figs/reliability_survey_qs_wo.png")
