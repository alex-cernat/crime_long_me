################################
#
# MTMM in Barcelona
#
# Data cleaning
#
################################

rm(list=ls())

#load libraries
library(here)
library(haven)
library(ggplot2)
library(ggpubr)
library(dplyr)

#load police data
police_data <- read.csv(here("data/raw/police_crime.csv"))

#crime types table
crime <- police_data %>%
  group_by(Delicte) %>%
  summarise(n = n()) %>%
  mutate(ID.crime = 1:n()) %>%
  select(-n)

#merge crime ID to original data
police_data <- police_data %>%
  left_join(crime, by = "Delicte")

#create crime groups
police_data <- police_data %>%
  mutate(group = ifelse(ID.crime < 6, 'other', NA),
         group = ifelse(ID.crime >= 6 & ID.crime <= 12, 'violence', group),
         group = ifelse(ID.crime >= 13 & ID.crime <= 18, 'other', group),
         group = ifelse(ID.crime >= 18 & ID.crime <= 23, 'violence', group),
         group = ifelse(ID.crime >= 24 & ID.crime <= 25, 'property_personal', group),
         group = ifelse(ID.crime == 26, 'other', group),
         group = ifelse(ID.crime >= 27 & ID.crime <= 28, 'violence', group),
         group = ifelse(ID.crime >= 29 & ID.crime <= 34, 'other', group),
         group = ifelse(ID.crime >= 35 & ID.crime <= 39, 'violence', group),
         group = ifelse(ID.crime >= 40 & ID.crime <= 42, 'other', group),
         group = ifelse(ID.crime >= 43 & ID.crime <= 45, 'violence', group),
         group = ifelse(ID.crime >= 46 & ID.crime <= 69, 'other', group),
         group = ifelse(ID.crime >= 70 & ID.crime <= 74, 'damage', group),
         group = ifelse(ID.crime >= 75 & ID.crime <= 94, 'other', group),
         group = ifelse(ID.crime >= 95 & ID.crime <= 97, 'property_personal', group),
         group = ifelse(ID.crime >= 98 & ID.crime <= 100, 'other', group),
         group = ifelse(ID.crime == 101, 'violence', group),
         group = ifelse(ID.crime >= 102 & ID.crime <= 111, 'other', group),
         group = ifelse(ID.crime == 112, 'violence', group),
         group = ifelse(ID.crime >= 113 & ID.crime <= 115, 'other', group),
         group = ifelse(ID.crime >= 116 & ID.crime <= 117, 'property_personal', group),
         group = ifelse(ID.crime >= 118 & ID.crime <= 121, 'other', group),
         group = ifelse(ID.crime == 122, 'violence', group),
         group = ifelse(ID.crime >= 123 & ID.crime <= 124, 'damage', group),
         group = ifelse(ID.crime >= 125 & ID.crime <= 128, 'other', group),
         group = ifelse(ID.crime == 129, 'property_personal', group),
         group = ifelse(ID.crime >= 130 & ID.crime <= 131, 'violence', group),
         group = ifelse(ID.crime >= 132 & ID.crime <= 135, 'other', group),
         group = ifelse(ID.crime == 136, 'property_vh', group),
         group = ifelse(ID.crime >= 137 & ID.crime <= 139, 'other', group),
         group = ifelse(ID.crime >= 140 & ID.crime <= 141, 'property_personal', group),
         group = ifelse(ID.crime >= 142 & ID.crime <= 143, 'property_vh', group),
         group = ifelse(ID.crime == 144, 'other', group),
         group = ifelse(ID.crime >= 145 & ID.crime <= 147, 'violence', group),
         group = ifelse(ID.crime >= 148 & ID.crime <= 150, 'other', group),
         group = ifelse(ID.crime >= 151 & ID.crime <= 154, 'damage', group),
         group = ifelse(ID.crime >= 155 & ID.crime <= 163, 'other', group),
         group = ifelse(ID.crime >= 164 & ID.crime <= 171, 'violence', group),
         group = ifelse(ID.crime >= 172 & ID.crime <= 174, 'other', group),
         group = ifelse(ID.crime == 175, 'violence', group),
         group = ifelse(ID.crime >= 176 & ID.crime <= 209, 'other', group),
         group = ifelse(ID.crime == 210, 'property_personal', group),
         group = ifelse(ID.crime == 211, 'property_vh', group),
         group = ifelse(ID.crime == 212, 'violence', group),
         group = ifelse(ID.crime == 213, 'property_vh', group),
         group = ifelse(ID.crime >= 214 & ID.crime <= 216, 'property_vh', group),
         group = ifelse(ID.crime == 217, 'violence', group),
         group = ifelse(ID.crime >= 218 & ID.crime <= 228, 'other', group),
         group = ifelse(ID.crime == 229, 'violence', group),
         group = ifelse(ID.crime == 230, 'other', group),
         group = ifelse(ID.crime == 231, 'violence', group),
         group = ifelse(ID.crime >= 232 & ID.crime <= 255, 'other', group),
         group = ifelse(ID.crime == 256, 'violence', group),
         ciber = ifelse(ID.crime == 72 | ID.crime == 97 | ID.crime == 161, 'yes', 'no')) %>%
  mutate(Count = ifelse(Any == 2020, Count*(365/349), Count))

#aggregate by years
years <- police_data %>%
  group_by(Any) %>%
  summarise(violence = sum(Count[group == 'violence']),
            property_vh = sum(Count[group == 'property_vh']),
            property_personal = sum(Count[group == 'property_personal']),
            damage = sum(Count[group == 'damage']),
            other = sum(Count[group == 'other']),
            ciber = sum(Count[ciber == 'yes']))

#merge two neighbourhoods to match Barcelona open data registry
police_data <- police_data %>%
  mutate(Barri = recode(Barri,
                        '11 - el Poble Sec' = '11 - el Poble Sec-Parc Montjuic',
                        'PM - Parc Montju?c' = '11 - el Poble Sec-Parc Montjuic',
                        '12 - la Marina del Prat Vermell' = '12 - la Marina del Prat Vermell-Zona Franca',
                        'FP - Zona Franca - Port' = '12 - la Marina del Prat Vermell-Zona Franca'))

#remove crimes without neighbourhood
police_data <- police_data %>%
  filter(Barri != "Barri de Barcelona")

#aggregate by neighbourhoods and years
barris <- police_data %>%
  group_by(Barri, Any) %>%
  summarise(all = sum(Count),
            violence = sum(Count[group == 'violence']),
            property_vh = sum(Count[group == 'property_vh']),
            property_personal = sum(Count[group == 'property_personal']),
            damage = sum(Count[group == 'damage']),
            other = sum(Count[group == 'other']))

#create new variable of groups of two years
police_data <- police_data %>%
  mutate(Any2 = ifelse(Any == 2015 | Any == 2016, '2015-16', NA),
         Any2 = ifelse(Any == 2017 | Any == 2018, '2017-18', Any2),
         Any2 = ifelse(Any == 2019 | Any == 2020, '2019-20', Any2))

#aggregate by neighbourhoods and years (groups of 2)
barris2 <- police_data %>%
  group_by(Barri, Any2) %>%
  summarise(all = sum(Count) / 2,
            violence = sum(Count[group == 'violence']) / 2,
            property_vh = sum(Count[group == 'property_vh']) / 2,
            property_personal = sum(Count[group == 'property_personal']) / 2,
            damage = sum(Count[group == 'damage']) / 2,
            other = sum(Count[group == 'other']) / 2) %>%
  filter(!is.na(Any2)) %>%
  rename(Any = Any2)

#aggregate by neighbourhoods across all years
barris3 <- police_data %>%
  group_by(Barri) %>%
  summarise(all = sum(Count) / 6,
            violence = sum(Count[group == 'violence']) / 6,
            property_vh = sum(Count[group == 'property_vh']) / 6,
            property_personal = sum(Count[group == 'property_personal']) / 6,
            damage = sum(Count[group == 'damage']) / 6,
            other = sum(Count[group == 'other']) / 6,
            Any = '2015-20')

#merge crime aggregates every two years and every year
barris <- barris %>%
  mutate(Any = as.character(Any)) %>%
  rbind(barris2) %>%
  rbind(barris3)

#load area population size
pop <- read.csv(here("data/raw/Population_2019.csv"))

#remove first 5 characters from name of areas in crime count dataset 
#remove first 4 characters and last 2 characters from name of areas in population size dataset 
barris <- barris %>%
  mutate(Barri = sub('.....', '', Barri))
pop <- pop %>%
  mutate(Barri = sub('....', '', Barri),
         Barri = gsub('.{2}$', '', Barri))

#recode some names of neighbourhoods
pop <- pop %>%
  mutate(Barri = recode(Barri,
                        'el Poble Sec - AEI Parc Montju?c' = 'el Poble Sec-Parc Montjuic',
                        'la Marina del Prat Vermell - AEI Zona Franca' = 'la Marina del Prat Vermell-Zona Franca',
                        'Sants - Badal' = 'Sants-Badal',
                        'Sant Gervasi - la Bonanova' = 'Sant Gervasi-la Bonanova',
                        'Sant Gervasi - Galvany' = 'Sant Gervasi-Galvany'))

#merge population size with crime count
barris <- barris %>%
  left_join(pop, by = "Barri")

#calculate crime rates times 1000
barris <- barris %>%
  mutate(all_rate = (all / TOTAL) * 1000,
         violence_rate = (violence / TOTAL) * 1000,
         property_vh_rate = (property_vh / TOTAL) * 1000,
         property_personal_rate = (property_personal / TOTAL) * 1000,
         damage_rate = (damage / TOTAL) * 1000,
         other_rate = (other / TOTAL) * 1000)

#select single years for plot
barris.plot <- barris %>%
  filter(Any == "2014" | Any == "2015" | Any == "2016" | Any == "2017" |
           Any == "2018" | Any == "2019" | Any == "2020")

#create plot graphs
line.a <- ggplot(data = barris.plot, aes(x = Any, y = all_rate, group = Barri)) + 
  geom_line(color = "grey") +
  ggtitle("All crime") +
  geom_text(x = 5.5, y = 880, label = "Barri G?tic", color = "darkgrey", size = 3, angle = -33) +
  geom_text(x = 5.4, y = 775, label = "Marina Prat Vermell-ZF", color = "darkgrey", size = 2.6, angle = -29) +
  geom_text(x = 3, y = 610, label = "Barceloneta", color = "darkgrey", size = 3, angle = 19) +
  geom_text(x = 2, y = 450, label = "Dreta de l'Eixample", color = "darkgrey", size = 3, angle = 3) +
  xlab("Year") +
  ylab("Crime rate") +
  theme_classic()

line.b <- ggplot(data = barris.plot, aes(x = Any, y = property_personal_rate, group = Barri)) + 
  geom_line(color = "grey") +
  ggtitle("Property (personal)") +
  theme_classic() + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

line.c <- ggplot(data = barris.plot, aes(x = Any, y = property_vh_rate, group = Barri)) + 
  geom_line(color = "grey") +
  ggtitle("Property (vehicle and household)") +
  theme_classic() + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

line.d <- ggplot(data = barris.plot, aes(x = Any, y = violence_rate, group = Barri)) + 
  geom_line(color = "grey") +
  ggtitle("Violence") +
  theme_classic() + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# plot for paper
ggarrange(line.a,
          ggarrange(line.b, line.c, line.d, nrow = 3),
          nrow = 1,
          widths = c(1.3, 0.7)
          ) 

#load survey data
survey_2014.i <- read_sav(here("data/raw/EVAMB 2014_2016_Base individus.sav"))
survey_2015.i <- read_sav(here("data/raw/r15020_Victimització_Basededades Persones.sav"))
survey_2015.c <- read_sav(here("data/raw/r15020_Victimització_Basededades Fets.sav"))
survey_2016.i <- read_sav(here("data/raw/r16007_Victimització_Persones_BDD_V_1_0.sav"))
survey_2016.c <- read_sav(here("data/raw/r16007_Victimització_Fets_BDD_V_1_0.sav"))
survey_2017.i <- read_sav(here("data/raw/r17019_Victimització_Persones_BDD_V_1_0.sav"))
survey_2017.c <- read_sav(here("data/raw/r17019_Victimització_Fets_BDD_V_1_0.sav"))
survey_2018.i <- read_sav(here("data/raw/r18022_EVB_BDD_Persones_V_1_0.sav"))
survey_2018.c <- read_sav(here("data/raw/r18022_EVB_BDD_Fets_V_1_0.sav"))
survey_2018.g <- read_sav(here("data/raw/EVAMB18_Persones_BarriBCN.sav"))
survey_2019.i <- read_sav(here("data/raw/r19017_EVB_BDD_Persones_v1_0.sav"))
survey_2019.c <- read_sav(here("data/raw/r19017_EVB_BDD_Fets_v1_0.sav"))
survey_2019.g <- read_sav(here("data/raw/EVAMB19_PERSONES_barriBCN.sav"))
survey_2020.i <- read_sav(here("data/raw/r20002_EVB_BDD_Persones_v1_0.sav"))
survey_2020.c <- read_sav(here("data/raw/r20002_EVB_BDD_Fets_v1_0.sav"))
survey_2020.g <- read_sav(here("data/raw/EVAMB20_PERSONES_barriBCN.sav"))

#add geographical information in 2018, 2019 and 2020
survey_2018.i <- survey_2018.i %>%
  left_join(survey_2018.g, by = "NUMQ")
survey_2019.i <- survey_2019.i %>%
  cbind(survey_2019.g)
survey_2020.i <- survey_2020.i %>%
  left_join(survey_2020.g, by = "NUMQ")

#recode crime types (capped at 5)
survey_2014.i <- survey_2014.i %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(violence = zz21 + zz22 + pp31 + pp32 + pp41 + pp42 + pp43,
         violence = ifelse(violence > 5, 5, violence),
         property_personal = pp21 + pp22 + pp11 + pp12 + pp51 + pp52 + PP53 + PP54,
         property_personal = ifelse(property_personal > 5, 5, property_personal),
         property_vh = xx2 + xx3 + xx11 + xx12 + yy11 + yy12 + rr11 + rr12 + zz11 + zz12,
         property_vh = ifelse(property_vh > 5, 5, property_vh),
         damage = dxx + dyy + drr + dzz + dcc2,
         damage = ifelse(damage > 5, 5, damage))
survey_2015.i <- survey_2015.i %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(violence = ZZ21 + ZZ22 + PP31 + PP32 + PP41 + PP42 + PP43,
         violence = ifelse(violence > 5, 5, violence),
         property_personal = PP21 + PP22 + PP11 + PP12 + PP51 + PP52 + PP53 + PP54,
         property_personal = ifelse(property_personal > 5, 5, property_personal),
         property_vh = XX2 + XX3 + XX11 + XX12 + YY11 + YY12 + RR11 + RR12 + ZZ11 + ZZ12,
         property_vh = ifelse(property_vh > 5, 5, property_vh))
survey_2016.i <- survey_2016.i %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(violence = ZZ21 + ZZ22 + PP31 + PP32 + PP41 + PP42 + PP43,
         violence = ifelse(violence > 5, 5, violence),
         property_personal = PP21 + PP22 + PP11 + PP12 + PP51 + PP52 + PP53 + PP54,
         property_personal = ifelse(property_personal > 5, 5, property_personal),
         property_vh = XX2 + XX3 + XX11 + XX12 + YY11 + YY12 + RR11 + RR12 + ZZ11 + ZZ12,
         property_vh = ifelse(property_vh > 5, 5, property_vh))
survey_2017.i <- survey_2017.i %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(violence = ZZ21 + ZZ22 + PP31 + PP32 + PP41 + PP42 + PP43,
         violence = ifelse(violence > 5, 5, violence),
         property_personal = PP21 + PP22 + PP11 + PP12 + PP51 + PP52 + PP53 + PP54,
         property_personal = ifelse(property_personal > 5, 5, property_personal),
         property_vh = XX2 + XX3 + XX11 + XX12 + YY11 + YY12 + RR11 + RR12 + ZZ11 + ZZ12,
         property_vh = ifelse(property_vh > 5, 5, property_vh))
survey_2018.i <- survey_2018.i %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(violence = ZZ21 + ZZ22 + PP31 + PP32 + PP41 + PP42 + PP43,
         violence = ifelse(violence > 5, 5, violence),
         property_personal = PP21 + PP22 + PP11 + PP12 + PP51 + PP52 + PP53 + PP54,
         property_personal = ifelse(property_personal > 5, 5, property_personal),
         property_vh = XX2 + XX3 + XX11 + XX12 + YY11 + YY12 + RR11 + RR12 + ZZ11 + ZZ12,
         property_vh = ifelse(property_vh > 5, 5, property_vh))
colnames(survey_2019.i) <- make.unique(names(survey_2019.i))
survey_2019.i <- survey_2019.i %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(violence = ZZ21 + ZZ22 + PP31 + PP32 + PP41 + PP42 + PP43,
         violence = ifelse(violence > 5, 5, violence),
         property_personal = PP21 + PP22 + PP11 + PP12 + PP51 + PP52 + PP53 + PP54,
         property_personal = ifelse(property_personal > 5, 5, property_personal),
         property_vh = XX2 + XX3 + XX11 + XX12 + YY11 + YY12 + RR11 + RR12 + ZZ11 + ZZ12,
         property_vh = ifelse(property_vh > 5, 5, property_vh))
survey_2020.i <- survey_2020.i %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(violence = ZZ21 + ZZ22 + PP31 + PP32 + PP41 + PP42 + PP43,
         violence = ifelse(violence > 5, 5, violence),
         property_personal = PP21 + PP22 + PP11 + PP12 + PP51 + PP52 + PP53 + PP54,
         property_personal = ifelse(property_personal > 5, 5, property_personal),
         property_vh = XX2 + XX3 + XX11 + XX12 + YY11 + YY12 + RR11 + RR12 + ZZ11 + ZZ12,
         property_vh = ifelse(property_vh > 5, 5, property_vh))

# prepare to recalibrate weights in each edition
weights_barris.2014 <- survey_2014.i %>%
  group_by(as_factor(CodMunBa16)) %>%
  summarise(sum_weights = sum(pesmos)) %>%
  rename('CodMunBa16' = 'as_factor(CodMunBa16)') %>%
  filter(stringr::str_detect(CodMunBa16, "Barcelona:")) %>%
  mutate(CodMunBa16 = sub('...........', '', CodMunBa16)) %>%
  mutate(CodMunBa16 = tolower(CodMunBa16))
weights_barris.2015 <- survey_2015.i %>%
  group_by(as_factor(Q2)) %>%
  summarise(sum_weights = sum(PES)) %>%
  rename('CodMunBa16' = 'as_factor(Q2)') %>%
  mutate(CodMunBa16 = tolower(CodMunBa16))
weights_barris.2016 <- survey_2016.i %>%
  group_by(as_factor(BARRI)) %>%
  summarise(sum_weights = sum(PES)) %>%
  rename('CodMunBa16' = 'as_factor(BARRI)') %>%
  mutate(CodMunBa16 = tolower(CodMunBa16))
weights_barris.2017 <- survey_2017.i %>%
  group_by(as_factor(BARRI)) %>%
  summarise(sum_weights = sum(PES)) %>%
  rename('CodMunBa16' = 'as_factor(BARRI)') %>%
  mutate(CodMunBa16 = tolower(CodMunBa16))
weights_barris.2018 <- survey_2018.i %>%
  group_by(as_factor(CodMunBa16_R)) %>%
  summarise(sum_weights = sum(PES)) %>%
  rename('CodMunBa16' = 'as_factor(CodMunBa16_R)') %>%
  mutate(CodMunBa16 = sub('...........', '', CodMunBa16)) %>%
  mutate(CodMunBa16 = tolower(CodMunBa16))
weights_barris.2019 <- survey_2019.i %>%
  group_by(as_factor(CodMunBa16_R)) %>%
  summarise(sum_weights = sum(PES)) %>%
  rename('CodMunBa16' = 'as_factor(CodMunBa16_R)') %>%
  mutate(CodMunBa16 = sub('...........', '', CodMunBa16)) %>%
  mutate(CodMunBa16 = tolower(CodMunBa16))
weights_barris.2020 <- survey_2020.i %>%
  group_by(as_factor(CodMunBa16_R)) %>%
  summarise(sum_weights = sum(PES)) %>%
  rename('CodMunBa16' = 'as_factor(CodMunBa16_R)') %>%
  mutate(CodMunBa16 = sub('...........', '', CodMunBa16)) %>%
  mutate(CodMunBa16 = tolower(CodMunBa16))

#add area population sizes to sum of weights
pop <- pop %>%
  mutate(Barri = tolower(Barri))
weights_barris.2014 <- weights_barris.2014 %>%
  mutate(CodMunBa16 = recode(CodMunBa16,
                             'el poble sec' = 'el poble sec-parc montjuic',
                             'la marina del prat vermell' = 'la marina del prat vermell-zona franca',
                             'sants - badal' = 'sants-badal',
                             'sant gervasi - la bonanova' = 'sant gervasi-la bonanova',
                             'sant gervasi - galvany' = 'sant gervasi-galvany')) %>%
  left_join(pop, by = c("CodMunBa16" =  "Barri"))
weights_barris.2015 <- weights_barris.2015 %>%
  mutate(CodMunBa16 = recode(CodMunBa16,
                             'el poble sec' = 'el poble sec-parc montjuic',
                             'la marina del prat vermell' = 'la marina del prat vermell-zona franca',
                             'sants - badal' = 'sants-badal',
                             'sant gervasi - la bonanova' = 'sant gervasi-la bonanova',
                             'sant gervasi - galvany' = 'sant gervasi-galvany')) %>%
  left_join(pop, by = c("CodMunBa16" =  "Barri"))
weights_barris.2016 <- weights_barris.2016 %>%
  mutate(CodMunBa16 = recode(CodMunBa16,
                             'el poble sec' = 'el poble sec-parc montjuic',
                             'la marina del prat vermell' = 'la marina del prat vermell-zona franca',
                             'sants - badal' = 'sants-badal',
                             'sant gervasi - la bonanova' = 'sant gervasi-la bonanova',
                             'sant gervasi - galvany' = 'sant gervasi-galvany')) %>%
  left_join(pop, by = c("CodMunBa16" =  "Barri"))
weights_barris.2017 <- weights_barris.2017 %>%
  mutate(CodMunBa16 = recode(CodMunBa16,
                             'el poble sec' = 'el poble sec-parc montjuic',
                             'la marina del prat vermell' = 'la marina del prat vermell-zona franca',
                             'sants - badal' = 'sants-badal',
                             'sant gervasi - la bonanova' = 'sant gervasi-la bonanova',
                             'sant gervasi - galvany' = 'sant gervasi-galvany')) %>%
  left_join(pop, by = c("CodMunBa16" =  "Barri"))
weights_barris.2018 <- weights_barris.2018 %>%
  mutate(CodMunBa16 = recode(CodMunBa16,
                             'el poble sec' = 'el poble sec-parc montjuic',
                             'la marina del prat vermell' = 'la marina del prat vermell-zona franca',
                             'sants - badal' = 'sants-badal',
                             'sant gervasi - la bonanova' = 'sant gervasi-la bonanova',
                             'sant gervasi - galvany' = 'sant gervasi-galvany')) %>%
  left_join(pop, by = c("CodMunBa16" =  "Barri"))
weights_barris.2019 <- weights_barris.2019 %>%
  mutate(CodMunBa16 = recode(CodMunBa16,
                             'el poble sec' = 'el poble sec-parc montjuic',
                             'la marina del prat vermell' = 'la marina del prat vermell-zona franca',
                             'sants - badal' = 'sants-badal',
                             'sant gervasi - la bonanova' = 'sant gervasi-la bonanova',
                             'sant gervasi - galvany' = 'sant gervasi-galvany')) %>%
  left_join(pop, by = c("CodMunBa16" =  "Barri"))
weights_barris.2020 <- weights_barris.2020 %>%
  mutate(CodMunBa16 = recode(CodMunBa16,
                             'el poble sec' = 'el poble sec-parc montjuic',
                             'la marina del prat vermell' = 'la marina del prat vermell-zona franca',
                             'sants - badal' = 'sants-badal',
                             'sant gervasi - la bonanova' = 'sant gervasi-la bonanova',
                             'sant gervasi - galvany' = 'sant gervasi-galvany')) %>%
  left_join(pop, by = c("CodMunBa16" =  "Barri"))

#add area information to survey
survey_2014.i <- survey_2014.i %>%
  mutate(CodMunBa16 = as_factor(CodMunBa16)) %>%
  filter(stringr::str_detect(CodMunBa16, "Barcelona:")) %>%
  mutate(CodMunBa16 = sub('...........', '', CodMunBa16)) %>%
  mutate(CodMunBa16 = tolower(CodMunBa16)) %>%
  mutate(CodMunBa16 = recode(CodMunBa16,
                             'el poble sec' = 'el poble sec-parc montjuic',
                             'la marina del prat vermell' = 'la marina del prat vermell-zona franca',
                             'sants - badal' = 'sants-badal',
                             'sant gervasi - la bonanova' = 'sant gervasi-la bonanova',
                             'sant gervasi - galvany' = 'sant gervasi-galvany')) %>%
  left_join(weights_barris.2014, by = "CodMunBa16")
survey_2015.i <- survey_2015.i %>%
  mutate(CodMunBa16 = as_factor(Q2)) %>%
  mutate(CodMunBa16 = tolower(CodMunBa16)) %>%
  mutate(CodMunBa16 = recode(CodMunBa16,
                             'el poble sec' = 'el poble sec-parc montjuic',
                             'la marina del prat vermell' = 'la marina del prat vermell-zona franca',
                             'sants - badal' = 'sants-badal',
                             'sant gervasi - la bonanova' = 'sant gervasi-la bonanova',
                             'sant gervasi - galvany' = 'sant gervasi-galvany')) %>%
  left_join(weights_barris.2015, by = "CodMunBa16")
survey_2016.i <- survey_2016.i %>%
  mutate(CodMunBa16 = as_factor(BARRI)) %>%
  mutate(CodMunBa16 = tolower(CodMunBa16)) %>%
  mutate(CodMunBa16 = recode(CodMunBa16,
                             'el poble sec' = 'el poble sec-parc montjuic',
                             'la marina del prat vermell' = 'la marina del prat vermell-zona franca',
                             'sants - badal' = 'sants-badal',
                             'sant gervasi - la bonanova' = 'sant gervasi-la bonanova',
                             'sant gervasi - galvany' = 'sant gervasi-galvany')) %>%
  left_join(weights_barris.2016, by = "CodMunBa16")
survey_2017.i <- survey_2017.i %>%
  mutate(CodMunBa16 = as_factor(BARRI)) %>%
  mutate(CodMunBa16 = tolower(CodMunBa16)) %>%
  mutate(CodMunBa16 = recode(CodMunBa16,
                             'el poble sec' = 'el poble sec-parc montjuic',
                             'la marina del prat vermell' = 'la marina del prat vermell-zona franca',
                             'sants - badal' = 'sants-badal',
                             'sant gervasi - la bonanova' = 'sant gervasi-la bonanova',
                             'sant gervasi - galvany' = 'sant gervasi-galvany')) %>%
  left_join(weights_barris.2017, by = "CodMunBa16")
survey_2018.i <- survey_2018.i %>%
  mutate(CodMunBa16 = as_factor(CodMunBa16_R)) %>%
  mutate(CodMunBa16 = sub('...........', '', CodMunBa16)) %>%
  mutate(CodMunBa16 = tolower(CodMunBa16)) %>%
  mutate(CodMunBa16 = recode(CodMunBa16,
                             'el poble sec' = 'el poble sec-parc montjuic',
                             'la marina del prat vermell' = 'la marina del prat vermell-zona franca',
                             'sants - badal' = 'sants-badal',
                             'sant gervasi - la bonanova' = 'sant gervasi-la bonanova',
                             'sant gervasi - galvany' = 'sant gervasi-galvany')) %>%
  left_join(weights_barris.2018, by = "CodMunBa16")
survey_2019.i <- survey_2019.i %>%
  mutate(CodMunBa16 = as_factor(CodMunBa16_R)) %>%
  mutate(CodMunBa16 = sub('...........', '', CodMunBa16)) %>%
  mutate(CodMunBa16 = tolower(CodMunBa16)) %>%
  mutate(CodMunBa16 = recode(CodMunBa16,
                             'el poble sec' = 'el poble sec-parc montjuic',
                             'la marina del prat vermell' = 'la marina del prat vermell-zona franca',
                             'sants - badal' = 'sants-badal',
                             'sant gervasi - la bonanova' = 'sant gervasi-la bonanova',
                             'sant gervasi - galvany' = 'sant gervasi-galvany')) %>%
  left_join(weights_barris.2019, by = "CodMunBa16")
survey_2020.i <- survey_2020.i %>%
  mutate(CodMunBa16 = as_factor(CodMunBa16_R)) %>%
  mutate(CodMunBa16 = sub('...........', '', CodMunBa16)) %>%
  mutate(CodMunBa16 = tolower(CodMunBa16)) %>%
  mutate(CodMunBa16 = recode(CodMunBa16,
                             'el poble sec' = 'el poble sec-parc montjuic',
                             'la marina del prat vermell' = 'la marina del prat vermell-zona franca',
                             'sants - badal' = 'sants-badal',
                             'sant gervasi - la bonanova' = 'sant gervasi-la bonanova',
                             'sant gervasi - galvany' = 'sant gervasi-galvany')) %>%
  left_join(weights_barris.2020, by = "CodMunBa16")

#adjust survey weights to population totals in each area
survey_2014.i <- survey_2014.i %>%
  mutate(new_weight = (pesmos * TOTAL) / sum_weights)
survey_2015.i <- survey_2015.i %>%
  mutate(new_weight = (PES * TOTAL) / sum_weights)
survey_2016.i <- survey_2016.i %>%
  mutate(new_weight = (PES * TOTAL) / sum_weights)
survey_2017.i <- survey_2017.i %>%
  mutate(new_weight = (PES * TOTAL) / sum_weights)
survey_2018.i <- survey_2018.i %>%
  mutate(new_weight = (PES * TOTAL) / sum_weights)
survey_2019.i <- survey_2019.i %>%
  mutate(new_weight = (PES * TOTAL) / sum_weights)
survey_2020.i <- survey_2020.i %>%
  mutate(new_weight = (PES * TOTAL) / sum_weights)

#estimate crimes where victims live
area_residence_2014 <- survey_2014.i %>%
  group_by(CodMunBa16) %>%
  summarise(year = 2014,
            sample.r = n(),
            TOTAL = mean(TOTAL),
            violence.r = sum(violence),
            violence.r.w = sum(violence * new_weight),
            violence.r.wo = sum(violence * pesmos),
            property_personal.r = sum(property_personal),
            property_personal.r.w = sum(property_personal * new_weight),
            property_personal.r.wo = sum(property_personal * pesmos),
            property_vh.r = sum(property_vh),
            property_vh.r.w = sum(property_vh * new_weight),
            property_vh.r.wo = sum(property_vh * pesmos),
            damage.r = sum(damage),
            damage.r.w = sum(damage * new_weight),
            damage.r.wo = sum(damage * pesmos)) %>%
  mutate(#violence.r = (violence.r / TOTAL) * 1000,
         violence.r.w = (violence.r.w / TOTAL) * 1000,
         #property_personal.r = (property_personal.r / TOTAL) * 1000,
         property_personal.r.w = (property_personal.r.w / TOTAL) * 1000,
         #property_vh.r = (property_vh.r / TOTAL) * 1000,
         property_vh.r.w = (property_vh.r.w / TOTAL) * 1000,
         #damage.r = (damage.r / TOTAL) * 1000,
         damage.r.w = (damage.r.w / TOTAL) * 1000)
area_residence_2015 <- survey_2015.i %>%
  group_by(CodMunBa16) %>%
  summarise(year = 2015,
            sample.r = n(),
            TOTAL = mean(TOTAL),
            violence.r = sum(violence),
            violence.r.w = sum(violence * new_weight),
            violence.r.wo = sum(violence * PES),
            property_personal.r = sum(property_personal),
            property_personal.r.w = sum(property_personal * new_weight),
            property_personal.r.wo = sum(property_personal * PES),
            property_vh.r = sum(property_vh),
            property_vh.r.w = sum(property_vh * new_weight),
            property_vh.r.wo = sum(property_vh * PES)) %>%
  mutate(#violence.r = (violence.r / TOTAL) * 1000,
         violence.r.w = (violence.r.w / TOTAL) * 1000,
         #property_personal.r = (property_personal.r / TOTAL) * 1000,
         property_personal.r.w = (property_personal.r.w / TOTAL) * 1000,
         #property_vh.r = (property_vh.r / TOTAL) * 1000,
         property_vh.r.w = (property_vh.r.w / TOTAL) * 1000)
area_residence_2016 <- survey_2016.i %>%
  group_by(CodMunBa16) %>%
  summarise(year = 2016,
            sample.r = n(),
            TOTAL = mean(TOTAL),
            violence.r = sum(violence),
            violence.r.w = sum(violence * new_weight),
            violence.r.wo = sum(violence * PES),
            property_personal.r = sum(property_personal),
            property_personal.r.w = sum(property_personal * new_weight),
            property_personal.r.wo = sum(property_personal * PES),
            property_vh.r = sum(property_vh),
            property_vh.r.w = sum(property_vh * new_weight),
            property_vh.r.wo = sum(property_vh * PES)) %>%
  mutate(#violence.r = (violence.r / TOTAL) * 1000,
         violence.r.w = (violence.r.w / TOTAL) * 1000,
         #property_personal.r = (property_personal.r / TOTAL) * 1000,
         property_personal.r.w = (property_personal.r.w / TOTAL) * 1000,
         #property_vh.r = (property_vh.r / TOTAL) * 1000,
         property_vh.r.w = (property_vh.r.w / TOTAL) * 1000)
area_residence_2017 <- survey_2017.i %>%
  group_by(CodMunBa16) %>%
  summarise(year = 2017,
            sample.r = n(),
            TOTAL = mean(TOTAL),
            violence.r = sum(violence),
            violence.r.w = sum(violence * new_weight),
            violence.r.wo = sum(violence * PES),
            property_personal.r = sum(property_personal),
            property_personal.r.w = sum(property_personal * new_weight),
            property_personal.r.wo = sum(property_personal * PES),
            property_vh.r = sum(property_vh),
            property_vh.r.w = sum(property_vh * new_weight),
            property_vh.r.wo = sum(property_vh * PES)) %>%
  mutate(#violence.r = (violence.r / TOTAL) * 1000,
         violence.r.w = (violence.r.w / TOTAL) * 1000,
         #property_personal.r = (property_personal.r / TOTAL) * 1000,
         property_personal.r.w = (property_personal.r.w / TOTAL) * 1000,
         #property_vh.r = (property_vh.r / TOTAL) * 1000,
         property_vh.r.w = (property_vh.r.w / TOTAL) * 1000)
area_residence_2018 <- survey_2018.i %>%
  group_by(CodMunBa16) %>%
  summarise(year = 2018,
            sample.r = n(),
            TOTAL = mean(TOTAL),
            violence.r = sum(violence),
            violence.r.w = sum(violence * new_weight),
            violence.r.wo = sum(violence * PES),
            property_personal.r = sum(property_personal),
            property_personal.r.w = sum(property_personal * new_weight),
            property_personal.r.wo = sum(property_personal * PES),
            property_vh.r = sum(property_vh),
            property_vh.r.w = sum(property_vh * new_weight),
            property_vh.r.wo = sum(property_vh * PES)) %>%
  mutate(#violence.r = (violence.r / TOTAL) * 1000,
         violence.r.w = (violence.r.w / TOTAL) * 1000,
         #property_personal.r = (property_personal.r / TOTAL) * 1000,
         property_personal.r.w = (property_personal.r.w / TOTAL) * 1000,
         #property_vh.r = (property_vh.r / TOTAL) * 1000,
         property_vh.r.w = (property_vh.r.w / TOTAL) * 1000)
area_residence_2019 <- survey_2019.i %>%
  group_by(CodMunBa16) %>%
  summarise(year = 2019,
            sample.r = n(),
            TOTAL = mean(TOTAL),
            violence.r = sum(violence),
            violence.r.w = sum(violence * new_weight),
            violence.r.wo = sum(violence * PES),
            property_personal.r = sum(property_personal),
            property_personal.r.w = sum(property_personal * new_weight),
            property_personal.r.wo = sum(property_personal * PES),
            property_vh.r = sum(property_vh),
            property_vh.r.w = sum(property_vh * new_weight),
            property_vh.r.wo = sum(property_vh * PES)) %>%
  mutate(#violence.r = (violence.r / TOTAL) * 1000,
         violence.r.w = (violence.r.w / TOTAL) * 1000,
         #property_personal.r = (property_personal.r / TOTAL) * 1000,
         property_personal.r.w = (property_personal.r.w / TOTAL) * 1000,
         #property_vh.r = (property_vh.r / TOTAL) * 1000,
         property_vh.r.w = (property_vh.r.w / TOTAL) * 1000)
area_residence_2020 <- survey_2020.i %>%
  group_by(CodMunBa16) %>%
  summarise(year = 2020,
            sample.r = n(),
            TOTAL = mean(TOTAL),
            violence.r = sum(violence),
            violence.r.w = sum(violence * new_weight),
            violence.r.wo = sum(violence * PES),
            property_personal.r = sum(property_personal),
            property_personal.r.w = sum(property_personal * new_weight),
            property_personal.r.wo = sum(property_personal * PES),
            property_vh.r = sum(property_vh),
            property_vh.r.w = sum(property_vh * new_weight),
            property_vh.r.wo = sum(property_vh * PES)) %>%
  mutate(#violence.r = (violence.r / TOTAL) * 1000,
         violence.r.w = (violence.r.w / TOTAL) * 1000,
         #property_personal.r = (property_personal.r / TOTAL) * 1000,
         property_personal.r.w = (property_personal.r.w / TOTAL) * 1000,
         #property_vh.r = (property_vh.r / TOTAL) * 1000,
         property_vh.r.w = (property_vh.r.w / TOTAL) * 1000)

#add year to all editions
survey_2015.i <- survey_2015.i %>%
  mutate(year = 2015)
survey_2016.i <- survey_2016.i %>%
  mutate(year = 2016)
survey_2017.i <- survey_2017.i %>%
  mutate(year = 2017)
survey_2018.i <- survey_2018.i %>%
  mutate(year = 2018)
survey_2019.i <- survey_2019.i %>%
  mutate(year = 2019)
survey_2020.i <- survey_2020.i %>%
  mutate(year = 2020)

#merge survey by two years and all years
survey_2015_16.i <- rbind(survey_2015.i[c("year", "NUMQ", "PES", "CodMunBa16",
                                          "violence", "property_personal", "property_vh")],
                          survey_2016.i[c("year", "NUMQ", "PES", "CodMunBa16",
                                          "violence", "property_personal", "property_vh")])
survey_2017_18.i <- rbind(survey_2017.i[c("year", "NUMQ", "PES", "CodMunBa16",
                                          "violence", "property_personal", "property_vh")],
                          survey_2018.i[c("year", "NUMQ", "PES", "CodMunBa16",
                                          "violence", "property_personal", "property_vh")])
survey_2019_20.i <- rbind(survey_2019.i[c("year", "NUMQ", "PES", "CodMunBa16",
                                          "violence", "property_personal", "property_vh")],
                          survey_2020.i[c("year", "NUMQ", "PES", "CodMunBa16",
                                          "violence", "property_personal", "property_vh")])
survey_2015_20.i <- rbind(survey_2015.i[c("year", "NUMQ", "PES", "CodMunBa16",
                                          "violence", "property_personal", "property_vh")],
                          survey_2016.i[c("year", "NUMQ", "PES", "CodMunBa16",
                                          "violence", "property_personal", "property_vh")],
                          survey_2017.i[c("year", "NUMQ", "PES", "CodMunBa16",
                                          "violence", "property_personal", "property_vh")],
                          survey_2018.i[c("year", "NUMQ", "PES", "CodMunBa16",
                                          "violence", "property_personal", "property_vh")],
                          survey_2019.i[c("year", "NUMQ", "PES", "CodMunBa16",
                                          "violence", "property_personal", "property_vh")],
                          survey_2020.i[c("year", "NUMQ", "PES", "CodMunBa16",
                                          "violence", "property_personal", "property_vh")])

#check number of victims more three crimes
more3_15 <- survey_2015.i %>%
  mutate(more3 = ifelse(ZZ21 > 3 | ZZ22 > 3 | PP31 > 3 | PP32 > 3 |
                          PP41 > 3 | PP42 > 3 | PP43 > 3 | PP21 > 3 |
                          PP22 > 3 | PP11 > 3 | PP12 > 3 | PP51 > 3 |
                          PP52 > 3 | PP53 > 3 | PP54 > 3 | XX2 > 3 |
                          XX3 > 3 | XX11 > 3 | XX12 > 3 | YY11 > 3 |
                          YY12 > 3 | RR11 > 3 | RR12 > 3 | ZZ11 > 3 |
                          ZZ12 > 3, 1, 0),
         atleast1 = ifelse(violence > 0 | property_personal > 0 |
                             property_vh > 0, 1, 0)) %>%
  count(more3, atleast1) %>%
  group_by(atleast1) %>%
  mutate(count = prop.table(n)*100)
more3_16 <- survey_2016.i %>%
  mutate(more3 = ifelse(ZZ21 > 3 | ZZ22 > 3 | PP31 > 3 | PP32 > 3 |
                          PP41 > 3 | PP42 > 3 | PP43 > 3 | PP21 > 3 |
                          PP22 > 3 | PP11 > 3 | PP12 > 3 | PP51 > 3 |
                          PP52 > 3 | PP53 > 3 | PP54 > 3 | XX2 > 3 |
                          XX3 > 3 | XX11 > 3 | XX12 > 3 | YY11 > 3 |
                          YY12 > 3 | RR11 > 3 | RR12 > 3 | ZZ11 > 3 |
                          ZZ12 > 3, 1, 0),
         atleast1 = ifelse(violence > 0 | property_personal > 0 |
                             property_vh > 0, 1, 0)) %>%
  count(more3, atleast1) %>%
  group_by(atleast1) %>%
  mutate(count = prop.table(n)*100)
more3_17 <- survey_2017.i %>%
  mutate(more3 = ifelse(ZZ21 > 3 | ZZ22 > 3 | PP31 > 3 | PP32 > 3 |
                          PP41 > 3 | PP42 > 3 | PP43 > 3 | PP21 > 3 |
                          PP22 > 3 | PP11 > 3 | PP12 > 3 | PP51 > 3 |
                          PP52 > 3 | PP53 > 3 | PP54 > 3 | XX2 > 3 |
                          XX3 > 3 | XX11 > 3 | XX12 > 3 | YY11 > 3 |
                          YY12 > 3 | RR11 > 3 | RR12 > 3 | ZZ11 > 3 |
                          ZZ12 > 3, 1, 0),
         atleast1 = ifelse(violence > 0 | property_personal > 0 |
                             property_vh > 0, 1, 0)) %>%
  count(more3, atleast1) %>%
  group_by(atleast1) %>%
  mutate(count = prop.table(n)*100)
more3_18 <- survey_2018.i %>%
  mutate(more3 = ifelse(ZZ21 > 3 | ZZ22 > 3 | PP31 > 3 | PP32 > 3 |
                          PP41 > 3 | PP42 > 3 | PP43 > 3 | PP21 > 3 |
                          PP22 > 3 | PP11 > 3 | PP12 > 3 | PP51 > 3 |
                          PP52 > 3 | PP53 > 3 | PP54 > 3 | XX2 > 3 |
                          XX3 > 3 | XX11 > 3 | XX12 > 3 | YY11 > 3 |
                          YY12 > 3 | RR11 > 3 | RR12 > 3 | ZZ11 > 3 |
                          ZZ12 > 3, 1, 0),
         atleast1 = ifelse(violence > 0 | property_personal > 0 |
                             property_vh > 0, 1, 0)) %>%
  count(more3, atleast1) %>%
  group_by(atleast1) %>%
  mutate(count = prop.table(n)*100)
more3_19 <- survey_2019.i %>%
  mutate(more3 = ifelse(ZZ21 > 3 | ZZ22 > 3 | PP31 > 3 | PP32 > 3 |
                          PP41 > 3 | PP42 > 3 | PP43 > 3 | PP21 > 3 |
                          PP22 > 3 | PP11 > 3 | PP12 > 3 | PP51 > 3 |
                          PP52 > 3 | PP53 > 3 | PP54 > 3 | XX2 > 3 |
                          XX3 > 3 | XX11 > 3 | XX12 > 3 | YY11 > 3 |
                          YY12 > 3 | RR11 > 3 | RR12 > 3 | ZZ11 > 3 |
                          ZZ12 > 3, 1, 0),
         atleast1 = ifelse(violence > 0 | property_personal > 0 |
                             property_vh > 0, 1, 0)) %>%
  count(more3, atleast1) %>%
  group_by(atleast1) %>%
  mutate(count = prop.table(n)*100)
more3_20 <- survey_2020.i %>%
  mutate(more3 = ifelse(ZZ21 > 3 | ZZ22 > 3 | PP31 > 3 | PP32 > 3 |
                          PP41 > 3 | PP42 > 3 | PP43 > 3 | PP21 > 3 |
                          PP22 > 3 | PP11 > 3 | PP12 > 3 | PP51 > 3 |
                          PP52 > 3 | PP53 > 3 | PP54 > 3 | XX2 > 3 |
                          XX3 > 3 | XX11 > 3 | XX12 > 3 | YY11 > 3 |
                          YY12 > 3 | RR11 > 3 | RR12 > 3 | ZZ11 > 3 |
                          ZZ12 > 3, 1, 0),
         atleast1 = ifelse(violence > 0 | property_personal > 0 |
                             property_vh > 0, 1, 0)) %>%
  count(more3, atleast1) %>%
  group_by(atleast1) %>%
  mutate(count = prop.table(n)*100)

(more3_15[3, 3] + more3_16[3, 3] + more3_17[3, 3] + more3_18[3, 3] +
    more3_19[3, 3] + more3_20[3, 3]) / (more3_15[2, 3] + more3_15[3, 3] +
    more3_16[2, 3] + more3_16[3, 3] + more3_17[2, 3] + more3_17[3, 3] +
    more3_18[2, 3] + more3_18[3, 3] + more3_19[2, 3] + more3_19[3, 3] +
    more3_20[2, 3] + more3_20[3, 3]) * 100

(more3_15[3, 3] + more3_16[3, 3] + more3_17[3, 3] + more3_18[3, 3] +
    more3_19[3, 3] + more3_20[3, 3]) / (nrow(survey_2015.i) + nrow(survey_2016.i) +
    nrow(survey_2017.i) + nrow(survey_2018.i) + nrow(survey_2019.i) +
    nrow(survey_2020.i)) * 100

# prepare to recalibrate weights in each edition
weights_barris.2015_16 <- survey_2015_16.i %>%
  group_by(as_factor(CodMunBa16)) %>%
  summarise(sum_weights = sum(PES)) %>%
  rename(CodMunBa16 = 'as_factor(CodMunBa16)')
weights_barris.2017_18 <- survey_2017_18.i %>%
  group_by(as_factor(CodMunBa16)) %>%
  summarise(sum_weights = sum(PES)) %>%
  rename(CodMunBa16 = 'as_factor(CodMunBa16)')
weights_barris.2019_20 <- survey_2019_20.i %>%
  group_by(as_factor(CodMunBa16)) %>%
  summarise(sum_weights = sum(PES)) %>%
  rename(CodMunBa16 = 'as_factor(CodMunBa16)')
weights_barris.2015_20 <- survey_2015_20.i %>%
  group_by(as_factor(CodMunBa16)) %>%
  summarise(sum_weights = sum(PES)) %>%
  rename(CodMunBa16 = 'as_factor(CodMunBa16)')

#add area population sizes to sum of weights
weights_barris.2015_16 <- weights_barris.2015_16 %>%
  left_join(pop, by = c("CodMunBa16" =  "Barri"))
weights_barris.2017_18 <- weights_barris.2017_18 %>%
  left_join(pop, by = c("CodMunBa16" =  "Barri"))
weights_barris.2019_20 <- weights_barris.2019_20 %>%
  left_join(pop, by = c("CodMunBa16" =  "Barri"))
weights_barris.2015_20 <- weights_barris.2015_20 %>%
  left_join(pop, by = c("CodMunBa16" =  "Barri"))

#add area information to survey
survey_2015_16.i <- survey_2015_16.i %>%
  left_join(weights_barris.2015_16, by = "CodMunBa16")
survey_2017_18.i <- survey_2017_18.i %>%
  left_join(weights_barris.2017_18, by = "CodMunBa16")
survey_2019_20.i <- survey_2019_20.i %>%
  left_join(weights_barris.2019_20, by = "CodMunBa16")
survey_2015_20.i <- survey_2015_20.i %>%
  left_join(weights_barris.2015_20, by = "CodMunBa16")

#adjust survey weights to population totals in each area
survey_2015_16.i <- survey_2015_16.i %>%
  mutate(new_weight = (PES * TOTAL) / sum_weights)
survey_2017_18.i <- survey_2017_18.i %>%
  mutate(new_weight = (PES * TOTAL) / sum_weights)
survey_2019_20.i <- survey_2019_20.i %>%
  mutate(new_weight = (PES * TOTAL) / sum_weights)
survey_2015_20.i <- survey_2015_20.i %>%
  mutate(new_weight = (PES * TOTAL) / sum_weights)

#estimate crimes where victims live
area_residence_2015_16 <- survey_2015_16.i %>%
  group_by(CodMunBa16) %>%
  summarise(year = '2015-16',
            sample.r = n(),
            TOTAL = mean(TOTAL),
            violence.r = sum(violence),
            violence.r.w = sum(violence * new_weight),
            violence.r.wo = sum(violence * PES),
            property_personal.r = sum(property_personal),
            property_personal.r.w = sum(property_personal * new_weight),
            property_personal.r.wo = sum(property_personal * PES),
            property_vh.r = sum(property_vh),
            property_vh.r.w = sum(property_vh * new_weight),
            property_vh.r.wo = sum(property_vh * PES)) %>%
  mutate(#violence.r = (violence.r / TOTAL) * 1000,
         violence.r.w = (violence.r.w / TOTAL) * 1000,
         #property_personal.r = (property_personal.r / TOTAL) * 1000,
         property_personal.r.w = (property_personal.r.w / TOTAL) * 1000,
         #property_vh.r = (property_vh.r / TOTAL) * 1000,
         property_vh.r.w = (property_vh.r.w / TOTAL) * 1000)
area_residence_2017_18 <- survey_2017_18.i %>%
  group_by(CodMunBa16) %>%
  summarise(year = '2017-18',
            sample.r = n(),
            TOTAL = mean(TOTAL),
            violence.r = sum(violence),
            violence.r.w = sum(violence * new_weight),
            violence.r.wo = sum(violence * PES),
            property_personal.r = sum(property_personal),
            property_personal.r.w = sum(property_personal * new_weight),
            property_personal.r.wo = sum(property_personal * PES),
            property_vh.r = sum(property_vh),
            property_vh.r.w = sum(property_vh * new_weight),
            property_vh.r.wo = sum(property_vh * PES)) %>%
  mutate(#violence.r = (violence.r / TOTAL) * 1000,
         violence.r.w = (violence.r.w / TOTAL) * 1000,
         #property_personal.r = (property_personal.r / TOTAL) * 1000,
         property_personal.r.w = (property_personal.r.w / TOTAL) * 1000,
         #property_vh.r = (property_vh.r / TOTAL) * 1000,
         property_vh.r.w = (property_vh.r.w / TOTAL) * 1000)
area_residence_2019_20 <- survey_2019_20.i %>%
  group_by(CodMunBa16) %>%
  summarise(year = '2019-20',
            sample.r = n(),
            TOTAL = mean(TOTAL),
            violence.r = sum(violence),
            violence.r.w = sum(violence * new_weight),
            violence.r.wo = sum(violence * PES),
            property_personal.r = sum(property_personal),
            property_personal.r.w = sum(property_personal * new_weight),
            property_personal.r.wo = sum(property_personal * PES),
            property_vh.r = sum(property_vh),
            property_vh.r.w = sum(property_vh * new_weight),
            property_vh.r.wo = sum(property_vh * PES)) %>%
  mutate(#violence.r = (violence.r / TOTAL) * 1000,
         violence.r.w = (violence.r.w / TOTAL) * 1000,
         #property_personal.r = (property_personal.r / TOTAL) * 1000,
         property_personal.r.w = (property_personal.r.w / TOTAL) * 1000,
         #property_vh.r = (property_vh.r / TOTAL) * 1000,
         property_vh.r.w = (property_vh.r.w / TOTAL) * 1000)
area_residence_2015_20 <- survey_2015_20.i %>%
  group_by(CodMunBa16) %>%
  summarise(year = '2015-20',
            sample.r = n(),
            TOTAL = mean(TOTAL),
            violence.r = sum(violence),
            violence.r.w = sum(violence * new_weight),
            violence.r.wo = sum(violence * PES),
            property_personal.r = sum(property_personal),
            property_personal.r.w = sum(property_personal * new_weight),
            property_personal.r.wo = sum(property_personal * PES),
            property_vh.r = sum(property_vh),
            property_vh.r.w = sum(property_vh * new_weight),
            property_vh.r.wo = sum(property_vh * PES)) %>%
  mutate(#violence.r = (violence.r / TOTAL) * 1000,
         violence.r.w = (violence.r.w / TOTAL) * 1000,
         #property_personal.r = (property_personal.r / TOTAL) * 1000,
         property_personal.r.w = (property_personal.r.w / TOTAL) * 1000,
         #property_vh.r = (property_vh.r / TOTAL) * 1000,
         property_vh.r.w = (property_vh.r.w / TOTAL) * 1000)

#merge with neighbourhood police crime counts
barris <- barris %>%
  mutate(Barri = tolower(Barri))
area_residence <- rbind(area_residence_2014[,1:13], area_residence_2015, area_residence_2016,
                        area_residence_2017, area_residence_2018, area_residence_2019,
                        area_residence_2020, area_residence_2015_16, area_residence_2017_18,
                        area_residence_2019_20, area_residence_2015_20)
area_residence <- area_residence %>%
  mutate(year = as.character(year))
area_residence_2014 <- area_residence_2014 %>%
  mutate(year = as.character(year))
barris <- barris %>%
  left_join(area_residence, c("Barri" = "CodMunBa16", "Any" = "year")) %>%
  mutate(sample.r = ifelse(is.na(sample.r), 0, sample.r),
         violence.r = ifelse(is.na(violence.r), 0, violence.r),
         violence.r.w = ifelse(is.na(violence.r.w), 0, violence.r.w),
         property_personal.r = ifelse(is.na(property_personal.r), 0, property_personal.r),
         property_personal.r.w = ifelse(is.na(property_personal.r.w), 0, property_personal.r.w),
         property_vh.r = ifelse(is.na(property_vh.r), 0, property_vh.r),
         property_vh.r.w = ifelse(is.na(property_vh.r.w), 0, property_vh.r.w),
         TOTAL.y = ifelse(is.na(TOTAL.y), TOTAL.x, TOTAL.y)) %>%
  left_join(area_residence_2014[,c(1,2,14,15,16)], c("Barri" = "CodMunBa16", "Any" = "year"))

#join unit-level survey data to crime survey data
survey_2015.c <- survey_2015.c %>%
  left_join(survey_2015.i, by = "NUMQ")
survey_2016.c <- survey_2016.c %>%
  left_join(survey_2016.i, by = "NUMQ")
survey_2017.c <- survey_2017.c %>%
  left_join(survey_2017.i, by = "NUMQ")
survey_2018.c <- survey_2018.c %>%
  left_join(survey_2018.i, by = "NUMQ")
survey_2019.c <- survey_2019.c %>%
  left_join(survey_2019.i, by = "NUMQ")
survey_2020.c <- survey_2020.c %>%
  left_join(survey_2020.i, by = "NUMQ")

#recode crime types
survey_2015.c <- survey_2015.c %>%
  mutate(FET = as_factor(FET),
         FET = recode(FET,
                      "ROBATORI OBJECTES PERSONALS D'INTERIOR DEL VEHICLE" = 'property_vh',
                      "ROBATORI ACCESSORIS/PART DEL VEHICLE" = 'property_vh',
                      "ROBATORI VEHICLE SENCER" = "property_vh",
                      "INTENT ROBATORI VEHICLE SENCER/PARTS/OBJECTES PERSONALS D'INTERIOR" = 'property_vh',
                      "ROBATORI DOMICILI" = 'property_vh',
                      "INTENT ROBATORI DOMICILI" = 'property_vh',
                      "ROBATORI SEGONA RESID?NCIA" = 'property_vh',
                      "INTENT ROBATORI SEGONA RESID?NCIA" = 'property_vh',
                      "ATRACAMENT AMB VIOL?NCIA NEGOCI" = 'violence',
                      "INTENT ATRACAMENT AMB VIOL?NCIA NEGOCI" = 'violence',
                      "ROBATORI SENSE VIOL?NCIA NEGOCI" = 'property_vh',
                      "INTENT ROBATORI SENSE VIOL?NCIA NEGOCI" = 'property_vh',
                      "ATRACAMENT AMB AMENACES/VIOL?NCIA" = 'violence',
                      "INTENT ATRACAMENT AMB AMENACES/VIOL?NCIA" = 'violence',
                      "ESTREBADA" = 'property_personal',
                      "INTENT ESTREBADA" = 'property_personal',
                      "ROBATORI BOSSA/CARTERA/ALTRES OBJECTES SENSE AMENACES/VIOL?NCIA" = 'property_personal',
                      "INTENT ROBATORI BOSSA/CARTERA/ALTRES OBJECTES SENSE AMENACES/VIOL?NCIA" = 'property_personal',
                      "ROBATORI TEL?FON M?BIL" = 'property_personal',
                      "INTENT ROBATORI TEL?FON M?BIL" = 'property_personal',
                      "ROBATORI ALTRES DISPOSITIUS ELECTR?NICS" = 'property_personal',
                      "INTENT ROBATORI ALTRES DISPOSITIUS ELECTR?NICS" = 'property_personal',
                      "AGRESSI? F?SICA" = 'violence',
                      "INTENT AGRESSI? F?SICA" = 'violence',
                      "INTIMIDACI?, COACCI? O AMENA?A" = 'violence'))
survey_2016.c <- survey_2016.c %>%
  mutate(FET = as_factor(FET),
         FET = recode(FET,
                      "ROBATORI/S D'OBJECTES DE L'INTERIOR DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S D'ACCESSORIS O PARTS DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S DEL VEHICLE SENCER" = "property_vh",
                      "INTENT/S DE ROBATORI DEL VEHICLE SENCER O PARTS DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S AL DOMICILI" = 'property_vh',
                      "INTENT/S DE ROBATORI DOMICILI" = 'property_vh',
                      "ROBATORI/S SEGONA RESID?NCIA" = 'property_vh',
                      "INTENT/S DE ROBATORI SEGONA RESID?NCIA" = 'property_vh',
                      "ATRACAMENT AMB VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'violence',
                      "INTENT/S D'ATRACAMENT AMB VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'violence',
                      "ROBATORI/S SENSE VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'property_vh',
                      "INTENT/S DE ROBATORI SENSE VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'property_vh',
                      "ATRACAMENT AMB AMENACES/VIOL?NCIA" = 'violence',
                      "INTENT/S D'ATRACAMENT AMB AMENACES/VIOL?NCIA" = 'violence',
                      "ESTREBADA" = 'property_personal',
                      "INTENT/S ESTREBADA" = 'property_personal',
                      "ROBATORI BOSSA/CARTERA/ALTRES OBJECTES SENSE AMENACES/VIOL?NCIA" = 'property_personal',
                      "INTENT/S DE ROBATORI BOSSA/CARTERA/ALTRES OBJECTES SENSE AMENACES/VIOL?NCIA" = 'property_personal',
                      "ROBATORI/S TEL?FON M?BIL" = 'property_personal',
                      "INTENT/S DE ROBATORI TEL?FON M?BIL" = 'property_personal',
                      "ROBATORI/S ALTRES DISPOSITIUS ELECTR?NICS" = 'property_personal',
                      "INTENT/S DE ROBATORI ALTRES DISPOSITIUS ELECTR?NICS" = 'property_personal',
                      "AGRESSI? F?SICA" = 'violence',
                      "INTENT/S D'AGRESSI? F?SICA" = 'violence',
                      "INTIMIDACIONS, COACCIONS O AMENACES" = 'violence'))
survey_2017.c <- survey_2017.c %>%
  mutate(FET = as_factor(FET),
         FET = recode(FET,
                      "ROBATORI/S D'OBJECTES DE L'INTERIOR DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S D'ACCESSORIS O PARTS DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S DEL VEHICLE SENCER" = "property_vh",
                      "INTENT/S DE ROBATORI DEL VEHICLE SENCER O PARTS DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S AL SEU DOMICILI" = 'property_vh',
                      "INTENT/S DE ROBATORI AL SEU DOMICILI" = 'property_vh',
                      "ROBATORI/S A LA SEGONA RESID?NCIA" = 'property_vh',
                      "INTENT/S DE ROBATORI A LA SEGONA RESID?NCIA" = 'property_vh',
                      "ATRACAMENT AMB VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'violence',
                      "INTENT/S D'ATRACAMENT AMB VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'violence',
                      "ROBATORI/S SENSE VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'property_vh',
                      "INTENT/S DE ROBATORI SENSE VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'property_vh',
                      "ATRACAMENT AMB AMENACES O VIOL?NCIA" = 'violence',
                      "INTENT/S D'ATRACAMENT AMB AMENACES O VIOL?NCIA" = 'violence',
                      "ESTREBADA" = 'property_personal',
                      "INTENT/S D'ESTREBADA" = 'property_personal',
                      "ROBATORI/S DE LA BOSSA/CARTERA/ALTRES OBJECTES SENSE AMENACES O VIOL?NCIA" = 'property_personal',
                      "INTENT/S DE ROBATORI DE LA BOSSA/CARTERA/ALTRES OBJECTES SENSE AMENACES O VIOL?NCIA" = 'property_personal',
                      "ROBATORI/S DEL TEL?FON M?BIL" = 'property_personal',
                      "INTENT/S DE ROBATORI DEL TEL?FON M?BIL" = 'property_personal',
                      "ROBATORI/S D'ALTRES DISPOSITIUS ELECTR?NICS" = 'property_personal',
                      "INTENT/S DE ROBATORI D'ALTRES DISPOSITIUS ELECTR?NICS" = 'property_personal',
                      "AGRESSI? F?SICA" = 'violence',
                      "AGRESSI? O INTENT D'AGRESSI? SEXUAL" = 'violence',
                      "C?RRECS INDEGUTS EN LA TARGETA BANC?RIA" = 'property_personal',
                      "C?RRECS O TREURE DINERS DELS SEUS COMPTES BANCARIS SENSE EL SEU PERM?S" = 'property_personal',
                      "COBRAR INDEGUDAMENT INSPECCIONS DE GAS, LLUM O ALTRES SERVEIS" = 'property_personal',
                      "ALTRA ESTAFA O FRAU" = 'property_personal',
                      "INTENT/S D'AGRESSI? F?SICA" = 'violence',
                      "INTIMIDACIONS, COACCIONS O AMENACES" = 'violence'))
survey_2018.c <- survey_2018.c %>%
  mutate(FET = as_factor(FET),
         FET = recode(FET,
                      "ROBATORI/S D'OBJECTES DE L'INTERIOR DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S D'ACCESSORIS O PARTS DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S DEL VEHICLE SENCER" = "property_vh",
                      "INTENT/S DE ROBATORI DEL VEHICLE SENCER O PARTS DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S AL SEU DOMICILI" = 'property_vh',
                      "INTENT/S DE ROBATORI AL SEU DOMICILI" = 'property_vh',
                      "ROBATORI/S A LA SEGONA RESID?NCIA" = 'property_vh',
                      "INTENT/S DE ROBATORI A LA SEGONA RESID?NCIA" = 'property_vh',
                      "ATRACAMENT AMB VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'violence',
                      "INTENT/S D'ATRACAMENT AMB VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'violence',
                      "ROBATORI/S SENSE VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'property_vh',
                      "INTENT/S DE ROBATORI SENSE VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'property_vh',
                      "ATRACAMENT AMB AMENACES O VIOL?NCIA" = 'violence',
                      "INTENT/S D'ATRACAMENT AMB AMENACES O VIOL?NCIA" = 'violence',
                      "ESTREBADA" = 'property_personal',
                      "INTENT/S D'ESTREBADA" = 'property_personal',
                      "ROBATORI/S DE LA BOSSA/CARTERA/ALTRES OBJECTES SENSE AMENACES O VIOL?NCIA" = 'property_personal',
                      "INTENT/S DE ROBATORI DE LA BOSSA/CARTERA/ALTRES OBJECTES SENSE AMENACES O VIOL?NCIA" = 'property_personal',
                      "ROBATORI/S DEL TEL?FON M?BIL" = 'property_personal',
                      "INTENT/S DE ROBATORI DEL TEL?FON M?BIL" = 'property_personal',
                      "ROBATORI/S D'ALTRES DISPOSITIUS ELECTR?NICS" = 'property_personal',
                      "INTENT/S DE ROBATORI D'ALTRES DISPOSITIUS ELECTR?NICS" = 'property_personal',
                      "AGRESSI? F?SICA" = 'violence',
                      "AGRESSI? O INTENT D'AGRESSI? SEXUAL" = 'violence',
                      "C?RRECS INDEGUTS EN LA TARGETA BANC?RIA" = 'property_personal',
                      "C?RRECS O TREURE DINERS DELS SEUS COMPTES BANCARIS SENSE EL SEU PERM?S" = 'property_personal',
                      "COBRAR INDEGUDAMENT INSPECCIONS DE GAS, LLUM O ALTRES SERVEIS" = 'property_personal',
                      "ALTRA ESTAFA O FRAU" = 'property_personal',
                      "INTENT/S D'AGRESSI? F?SICA" = 'violence',
                      "INTIMIDACIONS, COACCIONS O AMENACES" = 'violence'))
survey_2019.c <- survey_2019.c %>%
  mutate(FET = as_factor(FET),
         FET = recode(FET,
                      "ROBATORI/S D'OBJECTES DE L'INTERIOR DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S D'ACCESSORIS O PARTS DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S DEL VEHICLE SENCER" = "property_vh",
                      "INTENT/S DE ROBATORI DEL VEHICLE SENCER O PARTS DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S AL SEU DOMICILI" = 'property_vh',
                      "INTENT/S DE ROBATORI AL SEU DOMICILI" = 'property_vh',
                      "ROBATORI/S A LA SEGONA RESID?NCIA" = 'property_vh',
                      "INTENT/S DE ROBATORI A LA SEGONA RESID?NCIA" = 'property_vh',
                      "ATRACAMENT AMB VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'violence',
                      "INTENT/S D'ATRACAMENT AMB VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'violence',
                      "ROBATORI/S SENSE VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'property_vh',
                      "INTENT/S DE ROBATORI SENSE VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'property_vh',
                      "ATRACAMENT AMB AMENACES O VIOL?NCIA" = 'violence',
                      "INTENT/S D'ATRACAMENT AMB AMENACES O VIOL?NCIA" = 'violence',
                      "ESTREBADA" = 'property_personal',
                      "INTENT/S D'ESTREBADA" = 'property_personal',
                      "ROBATORI/S DE LA BOSSA/CARTERA/ALTRES OBJECTES SENSE AMENACES O VIOL?NCIA" = 'property_personal',
                      "INTENT/S DE ROBATORI DE LA BOSSA/CARTERA/ALTRES OBJECTES SENSE AMENACES O VIOL?NCIA" = 'property_personal',
                      "ROBATORI/S DEL TEL?FON M?BIL" = 'property_personal',
                      "INTENT/S DE ROBATORI DEL TEL?FON M?BIL" = 'property_personal',
                      "ROBATORI/S D'ALTRES DISPOSITIUS ELECTR?NICS" = 'property_personal',
                      "INTENT/S DE ROBATORI D'ALTRES DISPOSITIUS ELECTR?NICS" = 'property_personal',
                      "AGRESSI? F?SICA" = 'violence',
                      "AGRESSI? O INTENT D'AGRESSI? SEXUAL" = 'violence',
                      "C?RRECS INDEGUTS EN LA TARGETA BANC?RIA" = 'property_personal',
                      "C?RRECS O TREURE DINERS DELS SEUS COMPTES BANCARIS SENSE EL SEU PERM?S" = 'property_personal',
                      "COBRAR INDEGUDAMENT INSPECCIONS DE GAS, LLUM O ALTRES SERVEIS" = 'property_personal',
                      "ALTRA ESTAFA O FRAU" = 'property_personal',
                      "INTENT/S D'AGRESSI? F?SICA" = 'violence',
                      "INTIMIDACIONS, COACCIONS O AMENACES" = 'violence'))
survey_2020.c <- survey_2020.c %>%
  mutate(FET = as_factor(FET),
         FET = recode(FET,
                      "ROBATORI/S D'OBJECTES DE L'INTERIOR DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S D'ACCESSORIS O PARTS DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S DEL VEHICLE SENCER" = "property_vh",
                      "INTENT/S DE ROBATORI DEL VEHICLE SENCER O PARTS DEL VEHICLE" = 'property_vh',
                      "ROBATORI/S AL SEU DOMICILI" = 'property_vh',
                      "INTENT/S DE ROBATORI AL SEU DOMICILI" = 'property_vh',
                      "ROBATORI/S A LA SEGONA RESID?NCIA" = 'property_vh',
                      "INTENT/S DE ROBATORI A LA SEGONA RESID?NCIA" = 'property_vh',
                      "ATRACAMENT AMB VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'violence',
                      "INTENT/S D'ATRACAMENT AMB VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'violence',
                      "ROBATORI/S SENSE VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'property_vh',
                      "INTENT/S DE ROBATORI SENSE VIOL?NCIA A LA SEVA BOTIGA O NEGOCI" = 'property_vh',
                      "ATRACAMENT AMB AMENACES O VIOL?NCIA" = 'violence',
                      "INTENT/S D'ATRACAMENT AMB AMENACES O VIOL?NCIA" = 'violence',
                      "ESTREBADA" = 'property_personal',
                      "INTENT/S D'ESTREBADA" = 'property_personal',
                      "ROBATORI/S DE LA BOSSA/CARTERA/ALTRES OBJECTES SENSE AMENACES O VIOL?NCIA" = 'property_personal',
                      "INTENT/S DE ROBATORI DE LA BOSSA/CARTERA/ALTRES OBJECTES SENSE AMENACES O VIOL?NCIA" = 'property_personal',
                      "ROBATORI/S DEL TEL?FON M?BIL" = 'property_personal',
                      "INTENT/S DE ROBATORI DEL TEL?FON M?BIL" = 'property_personal',
                      "ROBATORI/S D'ALTRES DISPOSITIUS ELECTR?NICS" = 'property_personal',
                      "INTENT/S DE ROBATORI D'ALTRES DISPOSITIUS ELECTR?NICS" = 'property_personal',
                      "AGRESSI? F?SICA" = 'violence',
                      "AGRESSI? O INTENT D'AGRESSI? SEXUAL" = 'violence',
                      "C?RRECS INDEGUTS A LA TARGETA BANC?RIA O EXTRACCI? DE COMPTES BANCARIS" = 'property_personal',
                      "C?RRECS O TREURE DINERS DELS SEUS COMPTES BANCARIS SENSE EL SEU PERM?S" = 'property_personal',
                      "COBRAR INDEGUDAMENT INSPECCIONS DE SERVEIS" = 'property_personal',
                      "ALTRA ESTAFA O FRAU" = 'property_personal',
                      "INTENT/S D'AGRESSI? F?SICA" = 'violence',
                      "INTIMIDACIONS, COACCIONS O AMENACES" = 'violence'))

#recode neighbourhoods
survey_2015.c <- survey_2015.c %>%
  mutate(CodMunBa16.f = as_factor(BARRI),
         CodMunBa16.f = tolower(CodMunBa16.f),
         CodMunBa16.f = ifelse(stringr::str_detect(CodMunBa16.f, "(gb)") == TRUE, sub('.....', '', CodMunBa16.f), CodMunBa16.f),
         CodMunBa16.f = ifelse(CodMunBa16.f == "no ubicable" | CodMunBa16.f == "no contesta" |
                                 CodMunBa16.f == "residents bcn - no codificats (passen a amb)" |
                                 CodMunBa16.f == "transport p?blic (dins vag? o autob?s, estaci? metro/fcg)" |
                                 CodMunBa16.f == "trucada telef?nica o internet", NA, CodMunBa16.f),
         CodMunBa16.f = ifelse(stringr::str_detect(CodMunBa16.f, "(dist)") == TRUE, NA, CodMunBa16.f),
         CodMunBa16.f = ifelse(CodMunBa16.f == "la font de la guatlla, hostafrancs i la bordeta" |
                                 CodMunBa16.f == "la sagrera, el congr?s i navas" |
                                 CodMunBa16.f == "sant mart?, la verneda i la pau" |
                                 CodMunBa16.f == "sants i sants-badal" |
                                 CodMunBa16.f == "sarri?, les tres torres i vallvidrera", NA, CodMunBa16.f),
         CodMunBa16.f = recode(CodMunBa16.f,
                               'la marina' = 'la marina de port',
                               'el poble sec' = 'el poble sec-parc montjuic',
                               'la marina del prat vermell' = 'la marina del prat vermell-zona franca'))
survey_2016.c <- survey_2016.c %>%
  mutate(CodMunBa16.f = as_factor(BARRI.x),
         CodMunBa16.f = tolower(CodMunBa16.f),
         CodMunBa16.f = ifelse(CodMunBa16.f == "no ho sap" | CodMunBa16.f == "no ubicable" |
                                 CodMunBa16.f == "transport p?blic (dins vag? o autob?s, estaci? metro/fcg)" |
                                 CodMunBa16.f == "trucada telef?nica, d'internet o whatsapp", NA, CodMunBa16.f),
         CodMunBa16.f = recode(CodMunBa16.f,
                               '(gb) la marina' = 'la marina de port',
                               "el poble sec" = "el poble sec-parc montjuic",
                               "la marina del prat vermell" = "la marina del prat vermell-zona franca"))
survey_2017.c <- survey_2017.c %>%
  mutate(CodMunBa16.f = as_factor(BARRI.x),
         CodMunBa16.f = tolower(CodMunBa16.f),
         CodMunBa16.f = ifelse(CodMunBa16.f == "no ho sap" | CodMunBa16.f == "no ubicable" |
                                 CodMunBa16.f == "transport p?blic (dins vag? o autob?s, estaci? metro/fcg)" |
                                 CodMunBa16.f == "trucada telef?nica, d'internet o whatsapp", NA, CodMunBa16.f),
         CodMunBa16.f = recode(CodMunBa16.f,
                               "el poble sec" = "el poble sec-parc montjuic",
                               "la marina del prat vermell" = "la marina del prat vermell-zona franca"))
survey_2018.c <- survey_2018.c %>%
  mutate(CodMunBa16.f = as_factor(BARRI),
         CodMunBa16.f = tolower(CodMunBa16.f),
         BARRI_FET_ONP12 = as_factor(BARRI_FET_ONP12),
         BARRI_FET_ONP12 = tolower(BARRI_FET_ONP12),
         CodMunBa16.f = ifelse(CodMunBa16.f == "transport p?blic (dins vag? o autob?s, estaci? metro/fcg)", BARRI_FET_ONP12, CodMunBa16.f),
         CodMunBa16.f = ifelse(stringr::str_detect(CodMunBa16.f, "(dist)") == TRUE, NA, CodMunBa16.f),
         CodMunBa16.f = ifelse(stringr::str_detect(CodMunBa16.f, "(gb)") == TRUE, sub('.....', '', CodMunBa16.f), CodMunBa16.f),
         CodMunBa16.f = ifelse(CodMunBa16.f == "no ho sap" | CodMunBa16.f == "no ubicable" |
                                 CodMunBa16.f == "no contesta" |
                                 CodMunBa16.f == "trucada telef?nica, d'internet o whatsapp", NA, CodMunBa16.f),
         CodMunBa16.f = ifelse(CodMunBa16.f == "sants i sants-badal" | CodMunBa16.f == "el bes?s, el maresme i proven?als" |
                                 CodMunBa16.f == "el clot i el camp de l'arpa" |
                                 CodMunBa16.f == "el poblenou i diagonal mar" |
                                 CodMunBa16.f == "la font de la guatlla, hostafrancs i la bordeta" |
                                 CodMunBa16.f == "la guineueta, verdun i la prosperitat" |
                                 CodMunBa16.f == "la sagrera, el congr?s i navas" |
                                 CodMunBa16.f == "sant gervasi-la bonanova i el putxet", NA, CodMunBa16.f),
         CodMunBa16.f = recode(CodMunBa16.f,
                               "els barris de la vall d'hebron" = "la vall d'hebron"),
         CodMunBa16.f = recode(CodMunBa16.f,
                               "el poble sec" = "el poble sec-parc montjuic",
                               "la marina del prat vermell" = "la marina del prat vermell-zona franca"))
survey_2019.c <- survey_2019.c %>%
  mutate(CodMunBa16.f = as_factor(BARRI),
         CodMunBa16.f = tolower(CodMunBa16.f),
         PARADA = as_factor(PARADA),
         PARADA = tolower(PARADA),
         CodMunBa16.f = ifelse(CodMunBa16.f == "transport p?blic (dins vag? o autob?s, estaci? metro/fcg)", PARADA, CodMunBa16.f),
         CodMunBa16.f = recode(CodMunBa16.f,
                               "catalunya (metro, renfe, fgc)" = "la dreta de l'eixample",
                               "espanya" = "el poble sec-parc montjuic",
                               "estaci? del nord (estaci? autobusos)" = "el fort pienc",
                               "l1 - navas" = "navas",
                               "l1 - rocafort" = "la nova esquerra de l'eixample",
                               "l1 - urgell" = "l'antiga esquerra de l'eixample",
                               "l1 - arc del triomf" = "sant pere, santa caterina i la ribera",
                               "l11 - torre bar? / vallbona" = "torre bar?",
                               "l3 - drassanes" = "el raval",
                               "l3 - fontana" = "la vila de gr?cia",
                               "l3 - les corts" = "les corts",
                               "l3 - lesseps" = "la vila de gr?cia",
                               "l3 - maria cristina" = "la maternitat i sant ramon",
                               "l3 - poble sec" = "el poble sec-parc montjuic",
                               "l3 - roquetes" = "les roquetes",
                               "l4 - barceloneta" = "la barceloneta",
                               "l4 - ciutadella / vila ol?mpica" = "la vila ol?mpica del poblenou",
                               "l4 - el maresme / f?rum" = "el bes?s i el maresme",
                               "l4 - girona" = "la dreta de l'eixample",
                               "l4 - jaume i" = "el barri g?tic",
                               "l4 - llacuna" = "el poblenou",
                               "l5 - badal" = "sants-badal",
                               "l5 - hospital cl?nic" = "l'antiga esquerra de l'eixample",
                               "l5 - vilapicina" = "el tur? de la peira",
                               "maragall" = "el congr?s i els indians",
                               "paral?lel" = "el raval",
                               "passeig de gr?cia (metro, renfe)" = "la dreta de l'eixample",
                               "proven?a fgc" = "l'antiga esquerra de l'eixample",
                               "diagonal" = "l'antiga esquerra de l'eixample",
                               "sants estaci? / estaci? de sants (metro, renfe)" = "sants",
                               "sarria fgc" = "sarri?",
                               "universitat" = "sant antoni",
                               "urquinaona" = "la dreta de l'eixample",
                               "verdaguer" = "la dreta de l'eixample",
                               "clot" = "el clot",
                               "sagrada fam?lia" = "la sagrada fam?lia",
                               "sagrera" = "la sagrera",
                               "vall d'hebron" = "la vall d'hebron"),
         CodMunBa16.f = ifelse(CodMunBa16.f == "l?nia 1" | CodMunBa16.f == "l?nia 2" |
                                 CodMunBa16.f == "l?nia 3" | CodMunBa16.f == "l?nia 4" |
                                 CodMunBa16.f == "l?nia 5" | CodMunBa16.f == "r2" |
                                 CodMunBa16.f == "l?nies d'autob?s" |
                                 CodMunBa16.f == "no contesta" | CodMunBa16.f == "no ho sap" |
                                 CodMunBa16.f == "no ho sap / no ho recorda" |
                                 CodMunBa16.f == "no ubicable" | 
                                 CodMunBa16.f == "trucada telef?nica, d'internet o whatsapp",
                               NA, CodMunBa16.f),
         CodMunBa16.f = recode(CodMunBa16.f,
                               "el poble sec" = "el poble sec-parc montjuic",
                               "la marina del prat vermell" = "la marina del prat vermell-zona franca"))
survey_2020.c <- survey_2020.c %>%
  mutate(CodMunBa16.f = as_factor(BARRI_FET),
         CodMunBa16.f = tolower(CodMunBa16.f),
         ESTACIO_FET = as_factor(ESTACIO_FET),
         ESTACIO_FET = tolower(ESTACIO_FET),
         CodMunBa16.f = ifelse(CodMunBa16.f == "transport p?blic (dins vag? o autob?s, estaci? metro/fcg)", ESTACIO_FET, CodMunBa16.f),
         CodMunBa16.f = ifelse(stringr::str_detect(CodMunBa16.f, "(dist)") == TRUE, NA, CodMunBa16.f),
         CodMunBa16.f = ifelse(stringr::str_detect(CodMunBa16.f, "(gb)") == TRUE, sub('.....', '', CodMunBa16.f), CodMunBa16.f),
         CodMunBa16.f = ifelse(CodMunBa16.f == "altres" | CodMunBa16.f == "l2 - sense especificar l'estaci?" |
                                 CodMunBa16.f == "l3 - sense especificar l'estaci?" |
                                 CodMunBa16.f == "l4 - sense especificar l'estaci?" |
                                 CodMunBa16.f == "l5 - sense especificar l'estaci?" |
                                 CodMunBa16.f == "no contesta" | CodMunBa16.f == "no ho sap" |
                                 CodMunBa16.f == "no ho sap / no ho recorda" |
                                 CodMunBa16.f == "no ubicable" |
                                 CodMunBa16.f == "trucada telef?nica, d'internet o whatsapp", NA, CodMunBa16.f),
         CodMunBa16.f = recode(CodMunBa16.f,
                               "arc del triomf (metro, renfe)" = "sant pere, santa caterina i la ribera",
                               "catalunya (metro, renfe, fgc)" = "la dreta de l'eixample",
                               "clot (metro, renfe)" = "el clot",
                               "diagonal" = "l'antiga esquerra de l'eixample",
                               "espanya (metro, fgc)" = "el poble sec-parc montjuic",
                               "fabra i puig (metro, renfe)" = "sant andreu",
                               "l1 - gl?ries" = "el parc i la llacuna del poblenou",
                               "l1 - marina" = "el parc i la llacuna del poblenou",
                               "l1 - mercat nou" = "sants",
                               "l1 - navas" = "navas",
                               "l1 - rocafort" = "la nova esquerra de l'eixample",
                               "l1 - torras i bages" = "sant andreu",
                               "l2 - bac de roda" = "sant mart? de proven?als",
                               "l2 - encants" = "el camp de l'arpa del clot",
                               "l2 - monumental" = "el fort pienc",
                               "l2 - sant antoni" = "sant antoni",
                               "l2 - sant mart?" = "sant mart? de proven?als",
                               "l2 - tetuan" = "la dreta de l'eixample",
                               "l3 - canyelles" = "canyelles",
                               "l3 - drassanes" = "el raval",
                               "l3 - fontana" = "la vila de gr?cia",
                               "l3 - les corts" = "les corts",
                               "l3 - lesseps" = "la vila de gr?cia",
                               "l3 - liceu" = "el raval",
                               "l3 - maria cristina" = "la maternitat i sant ramon",
                               "l3 - poble sec" = "el poble sec-parc montjuic",
                               "l3 - roquetes" = "les roquetes",
                               "l3 - tarragona" = "hostafrancs",
                               "l4 - barceloneta" = "la barceloneta",
                               "l4 - bes?s" = "el bes?s i el maresme",
                               "l4 - bes?s mar" = "el bes?s i el maresme",
                               "l4 - bogatell" = "el parc i la llacuna del poblenou",
                               "l4 - ciutadella / vila ol?mpica" = "la vila ol?mpica del poblenou",
                               "l4 - girona" = "la dreta de l'eixample",
                               "l4 - jaume i" = "el barri g?tic",
                               "l4 - joanic" = "el camp d'en grassot i gr?cia nova",
                               "l4 - lluchmajor" = "porta",
                               "l4 - selva de mar" = "diagonal mar i el front mar?tim del poblenou",
                               "l4 - via j?lia" = "verdun",
                               "l5 - badal" = "sants-badal",
                               "l5 - enten?a" = "la nova esquerra de l'eixample",
                               "l5 - horta" = "horta",
                               "l5 - hospital cl?nic" = "l'antiga esquerra de l'eixample",
                               "l5 - sant pau/dos de maig" = "la sagrada fam?lia",
                               "l5 - vilapicina" = "el tur? de la peira",
                               "la pau" = "la verneda i la pau",
                               "maragall" = "el congr?s i els indians",
                               "paral?lel" = "el raval",
                               "passeig de gr?cia (metro, renfe)" = "la dreta de l'eixample",
                               "pla?a de sants" = "sants",
                               "pla?a molina fgc" = "sant gervasi-galvany",
                               "proven?a fgc" = "l'antiga esquerra de l'eixample",
                               "sagrada fam?lia" = "la sagrada fam?lia",
                               "sagrera (metro, renfe)" = "la sagrera",
                               "sants estaci? / estaci? de sants (metro, renfe)" = "sants",
                               "universitat" = "sant antoni",
                               "urquinaona" = "la dreta de l'eixample",
                               "vall d'hebron" = "la vall d'hebron",
                               "verdaguer" = "la dreta de l'eixample"),
         CodMunBa16.f = ifelse(CodMunBa16.f == "canyelles, les roquetes i la trinitat nova" |
                                 CodMunBa16.f == "el bes?s, el maresme i proven?als" |
                                 CodMunBa16.f == "el carmel i can bar?" |
                                 CodMunBa16.f == "el clot i el camp de l'arpa" |
                                 CodMunBa16.f == "el parc, la llacuna i la vila ol?mpica" |
                                 CodMunBa16.f == "el poblenou i diagonal mar" |
                                 CodMunBa16.f == "la font de la guatlla, hostafrancs i la bordeta" |
                                 CodMunBa16.f == "la guineueta, verdun i la prosperitat" |
                                 CodMunBa16.f == "la sagrera, el congr?s i navas" |
                                 CodMunBa16.f == "sant gervasi-la bonanova i el putxet" |
                                 CodMunBa16.f == "sant mart?, la verneda i la pau" |
                                 CodMunBa16.f == "sants i sants-badal" |
                                 CodMunBa16.f == "sarri?, les tres torres i vallvidrera" |
                                 CodMunBa16.f == "vilapicina, porta, el tur? de la peira i can peguera",
                               NA, CodMunBa16.f),
         CodMunBa16.f = recode(CodMunBa16.f,
                               "el poble sec" = "el poble sec-parc montjuic",
                               "els barris de la vall d'hebron" = "la vall d'hebron",
                               "la marina del prat vermell" = "la marina del prat vermell-zona franca"))

#create new variable per crime type
survey_2015.c <- survey_2015.c %>%
  mutate(violence.f = ifelse(FET == "violence", 1, 0),
         property_personal.f = ifelse(FET == "property_personal", 1, 0),
         property_vh.f = ifelse(FET == "property_vh", 1, 0))
survey_2016.c <- survey_2016.c %>%
  mutate(violence.f = ifelse(FET == "violence", 1, 0),
         property_personal.f = ifelse(FET == "property_personal", 1, 0),
         property_vh.f = ifelse(FET == "property_vh", 1, 0))
survey_2017.c <- survey_2017.c %>%
  mutate(violence.f = ifelse(FET == "violence", 1, 0),
         property_personal.f = ifelse(FET == "property_personal", 1, 0),
         property_vh.f = ifelse(FET == "property_vh", 1, 0))
survey_2018.c <- survey_2018.c %>%
  mutate(violence.f = ifelse(FET == "violence", 1, 0),
         property_personal.f = ifelse(FET == "property_personal", 1, 0),
         property_vh.f = ifelse(FET == "property_vh", 1, 0))
survey_2019.c <- survey_2019.c %>%
  mutate(violence.f = ifelse(FET == "violence", 1, 0),
         property_personal.f = ifelse(FET == "property_personal", 1, 0),
         property_vh.f = ifelse(FET == "property_vh", 1, 0))
survey_2020.c <- survey_2020.c %>%
  mutate(violence.f = ifelse(FET == "violence", 1, 0),
         property_personal.f = ifelse(FET == "property_personal", 1, 0),
         property_vh.f = ifelse(FET == "property_vh", 1, 0))

#create new weight by dividing weights by number of crimes per respondent
survey_2015.c <- survey_2015.c %>%
  group_by(NUMQ) %>%
  mutate(num.crimes = n()) %>%
  mutate(new_weight2 = new_weight / num.crimes)
survey_2016.c <- survey_2016.c %>%
  group_by(NUMQ) %>%
  mutate(num.crimes = n()) %>%
  mutate(new_weight2 = new_weight / num.crimes)
survey_2017.c <- survey_2017.c %>%
  group_by(NUMQ) %>%
  mutate(num.crimes = n()) %>%
  mutate(new_weight2 = new_weight / num.crimes)
survey_2018.c <- survey_2018.c %>%
  group_by(NUMQ) %>%
  mutate(num.crimes = n()) %>%
  mutate(new_weight2 = new_weight / num.crimes)
survey_2019.c <- survey_2019.c %>%
  group_by(NUMQ) %>%
  mutate(num.crimes = n()) %>%
  mutate(new_weight2 = new_weight / num.crimes)
survey_2020.c <- survey_2020.c %>%
  group_by(NUMQ) %>%
  mutate(num.crimes = n()) %>%
  mutate(new_weight2 = new_weight / num.crimes)

#estimate crimes where they happen
area_happen_2015 <- survey_2015.c %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = 2015,
            sample.f = n(),
            violence.f.nw = sum(violence.f),
            violence.f.w = sum(violence.f * new_weight),
            violence.f.w2 = sum(violence.f * new_weight2),
            violence.f.wo = sum(violence.f * PES.x),
            property_personal.f.nw = sum(property_personal.f),
            property_personal.f.w = sum(property_personal.f * new_weight),
            property_personal.f.w2 = sum(property_personal.f * new_weight2),
            property_personal.f.wo = sum(property_personal.f * PES.x),
            property_vh.f.nw = sum(property_vh.f),
            property_vh.f.w = sum(property_vh.f * new_weight),
            property_vh.f.w2 = sum(property_vh.f * new_weight2),
            property_vh.f.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.f.nw = (violence.f.nw / TOTAL) * 1000,
         violence.f.w = (violence.f.w / TOTAL) * 1000,
         violence.f.w2 = (violence.f.w2 / TOTAL) * 1000,
         #property_personal.f.nw = (property_personal.f.nw / TOTAL) * 1000,
         property_personal.f.w = (property_personal.f.w / TOTAL) * 1000,
         property_personal.f.w2 = (property_personal.f.w2 / TOTAL) * 1000,
         #property_vh.f.nw = (property_vh.f.nw / TOTAL) * 1000,
         property_vh.f.w = (property_vh.f.w / TOTAL) * 1000,
         property_vh.f.w2 = (property_vh.f.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_happen_2016 <- survey_2016.c %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = 2016,
            sample.f = n(),
            violence.f.nw = sum(violence.f),
            violence.f.w = sum(violence.f * new_weight),
            violence.f.w2 = sum(violence.f * new_weight2),
            violence.f.wo = sum(violence.f * PES.x),
            property_personal.f.nw = sum(property_personal.f),
            property_personal.f.w = sum(property_personal.f * new_weight),
            property_personal.f.w2 = sum(property_personal.f * new_weight2),
            property_personal.f.wo = sum(property_personal.f * PES.x),
            property_vh.f.nw = sum(property_vh.f),
            property_vh.f.w = sum(property_vh.f * new_weight),
            property_vh.f.w2 = sum(property_vh.f * new_weight2),
            property_vh.f.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.f.nw = (violence.f.nw / TOTAL) * 1000,
    violence.f.w = (violence.f.w / TOTAL) * 1000,
    violence.f.w2 = (violence.f.w2 / TOTAL) * 1000,
    #property_personal.f.nw = (property_personal.f.nw / TOTAL) * 1000,
    property_personal.f.w = (property_personal.f.w / TOTAL) * 1000,
    property_personal.f.w2 = (property_personal.f.w2 / TOTAL) * 1000,
    #property_vh.f.nw = (property_vh.f.nw / TOTAL) * 1000,
    property_vh.f.w = (property_vh.f.w / TOTAL) * 1000,
    property_vh.f.w2 = (property_vh.f.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_happen_2017 <- survey_2017.c %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = 2017,
            sample.f = n(),
            violence.f.nw = sum(violence.f),
            violence.f.w = sum(violence.f * new_weight),
            violence.f.w2 = sum(violence.f * new_weight2),
            violence.f.wo = sum(violence.f * PES.x),
            property_personal.f.nw = sum(property_personal.f),
            property_personal.f.w = sum(property_personal.f * new_weight),
            property_personal.f.w2 = sum(property_personal.f * new_weight2),
            property_personal.f.wo = sum(property_personal.f * PES.x),
            property_vh.f.nw = sum(property_vh.f),
            property_vh.f.w = sum(property_vh.f * new_weight),
            property_vh.f.w2 = sum(property_vh.f * new_weight2),
            property_vh.f.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.f.nw = (violence.f.nw / TOTAL) * 1000,
    violence.f.w = (violence.f.w / TOTAL) * 1000,
    violence.f.w2 = (violence.f.w2 / TOTAL) * 1000,
    #property_personal.f.nw = (property_personal.f.nw / TOTAL) * 1000,
    property_personal.f.w = (property_personal.f.w / TOTAL) * 1000,
    property_personal.f.w2 = (property_personal.f.w2 / TOTAL) * 1000,
    #property_vh.f.nw = (property_vh.f.nw / TOTAL) * 1000,
    property_vh.f.w = (property_vh.f.w / TOTAL) * 1000,
    property_vh.f.w2 = (property_vh.f.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_happen_2018 <- survey_2018.c %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = 2018,
            sample.f = n(),
            violence.f.nw = sum(violence.f),
            violence.f.w = sum(violence.f * new_weight),
            violence.f.w2 = sum(violence.f * new_weight2),
            violence.f.wo = sum(violence.f * PES.x),
            property_personal.f.nw = sum(property_personal.f),
            property_personal.f.w = sum(property_personal.f * new_weight),
            property_personal.f.w2 = sum(property_personal.f * new_weight2),
            property_personal.f.wo = sum(property_personal.f * PES.x),
            property_vh.f.nw = sum(property_vh.f),
            property_vh.f.w = sum(property_vh.f * new_weight),
            property_vh.f.w2 = sum(property_vh.f * new_weight2),
            property_vh.f.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.f.nw = (violence.f.nw / TOTAL) * 1000,
         violence.f.w = (violence.f.w / TOTAL) * 1000,
         violence.f.w2 = (violence.f.w2 / TOTAL) * 1000,
         #property_personal.f.nw = (property_personal.f.nw / TOTAL) * 1000,
         property_personal.f.w = (property_personal.f.w / TOTAL) * 1000,
         property_personal.f.w2 = (property_personal.f.w2 / TOTAL) * 1000,
         #property_vh.f.nw = (property_vh.f.nw / TOTAL) * 1000,
         property_vh.f.w = (property_vh.f.w / TOTAL) * 1000,
         property_vh.f.w2 = (property_vh.f.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_happen_2019 <- survey_2019.c %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = 2019,
            sample.f = n(),
            violence.f.nw = sum(violence.f),
            violence.f.w = sum(violence.f * new_weight),
            violence.f.w2 = sum(violence.f * new_weight2),
            violence.f.wo = sum(violence.f * PES.x),
            property_personal.f.nw = sum(property_personal.f),
            property_personal.f.w = sum(property_personal.f * new_weight),
            property_personal.f.w2 = sum(property_personal.f * new_weight2),
            property_personal.f.wo = sum(property_personal.f * PES.x),
            property_vh.f.nw = sum(property_vh.f),
            property_vh.f.w = sum(property_vh.f * new_weight),
            property_vh.f.w2 = sum(property_vh.f * new_weight2),
            property_vh.f.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.f.nw = (violence.f.nw / TOTAL) * 1000,
    violence.f.w = (violence.f.w / TOTAL) * 1000,
    violence.f.w2 = (violence.f.w2 / TOTAL) * 1000,
    #property_personal.f.nw = (property_personal.f.nw / TOTAL) * 1000,
    property_personal.f.w = (property_personal.f.w / TOTAL) * 1000,
    property_personal.f.w2 = (property_personal.f.w2 / TOTAL) * 1000,
    #property_vh.f.nw = (property_vh.f.nw / TOTAL) * 1000,
    property_vh.f.w = (property_vh.f.w / TOTAL) * 1000,
    property_vh.f.w2 = (property_vh.f.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_happen_2020 <- survey_2020.c %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = 2020,
            sample.f = n(),
            violence.f.nw = sum(violence.f),
            violence.f.w = sum(violence.f * new_weight),
            violence.f.w2 = sum(violence.f * new_weight2),
            violence.f.wo = sum(violence.f * PES.x),
            property_personal.f.nw = sum(property_personal.f),
            property_personal.f.w = sum(property_personal.f * new_weight),
            property_personal.f.w2 = sum(property_personal.f * new_weight2),
            property_personal.f.wo = sum(property_personal.f * PES.x),
            property_vh.f.nw = sum(property_vh.f),
            property_vh.f.w = sum(property_vh.f * new_weight),
            property_vh.f.w2 = sum(property_vh.f * new_weight2),
            property_vh.f.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.f.nw = (violence.f.nw / TOTAL) * 1000,
    violence.f.w = (violence.f.w / TOTAL) * 1000,
    violence.f.w2 = (violence.f.w2 / TOTAL) * 1000,
    #property_personal.f.nw = (property_personal.f.nw / TOTAL) * 1000,
    property_personal.f.w = (property_personal.f.w / TOTAL) * 1000,
    property_personal.f.w2 = (property_personal.f.w2 / TOTAL) * 1000,
    #property_vh.f.nw = (property_vh.f.nw / TOTAL) * 1000,
    property_vh.f.w = (property_vh.f.w / TOTAL) * 1000,
    property_vh.f.w2 = (property_vh.f.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)

#recode whether crime was reported to the police or not
survey_2015.c <- survey_2015.c %>%
  mutate(SIG = as_factor(SIG),
         SIG = tolower(SIG),
         SIG = recode(SIG,
                      "no contesta" = "NA",
                      "no ho sap / no ho recorda" = "NA",
                      "no, no ho va denunciar" = "no",
                      "sí, ho va denunciar i va firmar un document" = "yes",
                      "sí, ho va denunciar però no va arribar a firmar cap document" = "yes"))
survey_2016.c <- survey_2016.c %>%
  mutate(SIG = as_factor(SIG),
         SIG = tolower(SIG),
         SIG = recode(SIG,
                      "no contesta" = "NA",
                      "no ho sap / no ho recorda" = "NA",
                      "no, no ho va denunciar" = "no",
                      "sí, ho va denunciar i va firmar un document" = "yes",
                      "sí, ho va denunciar però no va arribar a firmar cap document" = "yes"))
survey_2017.c <- survey_2017.c %>%
  mutate(SIG = as_factor(SIG),
         SIG = tolower(SIG),
         SIG = recode(SIG,
                      "no contesta" = "NA",
                      "no ho sap / no ho recorda" = "NA",
                      "no, no ho va denunciar" = "no",
                      "sí, ho va denunciar i va firmar un document" = "yes",
                      "sí, ho va denunciar però no va arribar a firmar cap document" = "yes"))
survey_2018.c <- survey_2018.c %>%
  mutate(SIG = as_factor(SIG),
         SIG = tolower(SIG),
         SIG = recode(SIG,
                      "no contesta" = "NA",
                      "no ho sap / no ho recorda" = "NA",
                      "no, no ho va denunciar" = "no",
                      "sí, ho va denunciar i va firmar un document" = "yes",
                      "sí, ho va denunciar però no va arribar a firmar cap document" = "yes"))
survey_2019.c <- survey_2019.c %>%
  mutate(SIG = as_factor(SIG),
         SIG = tolower(SIG),
         SIG = recode(SIG,
                      "no contesta" = "NA",
                      "no ho sap / no ho recorda" = "NA",
                      "no, no ho va denunciar" = "no",
                      "sí, ho va denunciar i va firmar un document" = "yes",
                      "sí, ho va denunciar però no va arribar a firmar cap document" = "yes"))
survey_2020.c <- survey_2020.c %>%
  mutate(SIG = as_factor(SIG),
         SIG = tolower(SIG),
         SIG = recode(SIG,
                      "no contesta" = "NA",
                      "no ho sap / no ho recorda" = "NA",
                      "no, no ho va denunciar" = "no",
                      "sí, ho va denunciar i va firmar un document" = "yes",
                      "sí, ho va denunciar però no va arribar a firmar cap document" = "yes"))

#merge survey by two years and all
survey_2015_16.c <- rbind(survey_2015.c[c("year", "NUMQ", "FET", "CodMunBa16.f", "SIG",
                                          "violence.f", "property_personal.f", "property_vh.f")],
                          survey_2016.c[c("year", "NUMQ", "FET", "CodMunBa16.f", "SIG",
                                          "violence.f", "property_personal.f", "property_vh.f")])
survey_2017_18.c <- rbind(survey_2017.c[c("year", "NUMQ", "FET", "CodMunBa16.f", "SIG",
                                          "violence.f", "property_personal.f", "property_vh.f")],
                          survey_2018.c[c("year", "NUMQ", "FET", "CodMunBa16.f", "SIG",
                                          "violence.f", "property_personal.f", "property_vh.f")])
survey_2019_20.c <- rbind(survey_2019.c[c("year", "NUMQ", "FET", "CodMunBa16.f", "SIG",
                                          "violence.f", "property_personal.f", "property_vh.f")],
                          survey_2020.c[c("year", "NUMQ", "FET", "CodMunBa16.f", "SIG",
                                          "violence.f", "property_personal.f", "property_vh.f")])
survey_2015_20.c <- rbind(survey_2015.c[c("year", "NUMQ", "FET", "CodMunBa16.f", "SIG",
                                          "violence.f", "property_personal.f", "property_vh.f")],
                          survey_2016.c[c("year", "NUMQ", "FET", "CodMunBa16.f", "SIG",
                                          "violence.f", "property_personal.f", "property_vh.f")],
                          survey_2017.c[c("year", "NUMQ", "FET", "CodMunBa16.f", "SIG",
                                          "violence.f", "property_personal.f", "property_vh.f")],
                          survey_2018.c[c("year", "NUMQ", "FET", "CodMunBa16.f", "SIG",
                                          "violence.f", "property_personal.f", "property_vh.f")],
                          survey_2019.c[c("year", "NUMQ", "FET", "CodMunBa16.f", "SIG",
                                          "violence.f", "property_personal.f", "property_vh.f")],
                          survey_2020.c[c("year", "NUMQ", "FET", "CodMunBa16.f", "SIG",
                                          "violence.f", "property_personal.f", "property_vh.f")])

#sample size of crimes with known address
table(!is.na(survey_2015.c$CodMunBa16.f))
table(!is.na(survey_2016.c$CodMunBa16.f))
table(!is.na(survey_2017.c$CodMunBa16.f))      
table(!is.na(survey_2018.c$CodMunBa16.f))
table(!is.na(survey_2019.c$CodMunBa16.f))
table(!is.na(survey_2020.c$CodMunBa16.f))

#join unit-level survey data to crime survey data
survey_2015_16.c <- survey_2015_16.c %>%
  left_join(survey_2015_16.i, by = c("NUMQ", "year"))
survey_2017_18.c <- survey_2017_18.c %>%
  left_join(survey_2017_18.i, by = c("NUMQ", "year"))
survey_2019_20.c <- survey_2019_20.c %>%
  left_join(survey_2019_20.i, by = c("NUMQ", "year"))
survey_2015_20.c <- survey_2015_20.c %>%
  left_join(survey_2015_20.i, by = c("NUMQ", "year"))

#create new weight by dividing weights by number of crimes per respondent
survey_2015_16.c <- survey_2015_16.c %>%
  group_by(NUMQ) %>%
  mutate(num.crimes = n()) %>%
  mutate(new_weight2 = new_weight / num.crimes)
survey_2017_18.c <- survey_2017_18.c %>%
  group_by(NUMQ) %>%
  mutate(num.crimes = n()) %>%
  mutate(new_weight2 = new_weight / num.crimes)
survey_2019_20.c <- survey_2019_20.c %>%
  group_by(NUMQ) %>%
  mutate(num.crimes = n()) %>%
  mutate(new_weight2 = new_weight / num.crimes)
survey_2015_20.c <- survey_2015_20.c %>%
  group_by(NUMQ) %>%
  mutate(num.crimes = n()) %>%
  mutate(new_weight2 = new_weight / num.crimes)

#estimate crimes where they happen
area_happen_2015_16 <- survey_2015_16.c %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = '2015-16',
            sample.f = n(),
            violence.f.nw = sum(violence.f),
            violence.f.w = sum(violence.f * new_weight),
            violence.f.w2 = sum(violence.f * new_weight2),
            violence.f.wo = sum(violence.f * PES),
            property_personal.f.nw = sum(property_personal.f),
            property_personal.f.w = sum(property_personal.f * new_weight),
            property_personal.f.w2 = sum(property_personal.f * new_weight2),
            property_personal.f.wo = sum(property_personal.f * PES),
            property_vh.f.nw = sum(property_vh.f),
            property_vh.f.w = sum(property_vh.f * new_weight),
            property_vh.f.w2 = sum(property_vh.f * new_weight2),
            property_vh.f.wo = sum(property_vh.f * PES)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.f.nw = (violence.f.nw / TOTAL) * 1000,
         violence.f.w = (violence.f.w / TOTAL) * 1000,
         violence.f.w2 = (violence.f.w2 / TOTAL) * 1000,
         #property_personal.f.nw = (property_personal.f.nw / TOTAL) * 1000,
         property_personal.f.w = (property_personal.f.w / TOTAL) * 1000,
         property_personal.f.w2 = (property_personal.f.w2 / TOTAL) * 1000,
         #property_vh.f.nw = (property_vh.f.nw / TOTAL) * 1000,
         property_vh.f.w = (property_vh.f.w / TOTAL) * 1000,
         property_vh.f.w2 = (property_vh.f.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_happen_2017_18 <- survey_2017_18.c %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = '2017-18',
            sample.f = n(),
            violence.f.nw = sum(violence.f),
            violence.f.w = sum(violence.f * new_weight),
            violence.f.w2 = sum(violence.f * new_weight2),
            violence.f.wo = sum(violence.f * PES),
            property_personal.f.nw = sum(property_personal.f),
            property_personal.f.w = sum(property_personal.f * new_weight),
            property_personal.f.w2 = sum(property_personal.f * new_weight2),
            property_personal.f.wo = sum(property_personal.f * PES),
            property_vh.f.nw = sum(property_vh.f),
            property_vh.f.w = sum(property_vh.f * new_weight),
            property_vh.f.w2 = sum(property_vh.f * new_weight2),
            property_vh.f.wo = sum(property_vh.f * PES)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.f.nw = (violence.f.nw / TOTAL) * 1000,
    violence.f.w = (violence.f.w / TOTAL) * 1000,
    violence.f.w2 = (violence.f.w2 / TOTAL) * 1000,
    #property_personal.f.nw = (property_personal.f.nw / TOTAL) * 1000,
    property_personal.f.w = (property_personal.f.w / TOTAL) * 1000,
    property_personal.f.w2 = (property_personal.f.w2 / TOTAL) * 1000,
    #property_vh.f.nw = (property_vh.f.nw / TOTAL) * 1000,
    property_vh.f.w = (property_vh.f.w / TOTAL) * 1000,
    property_vh.f.w2 = (property_vh.f.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_happen_2019_20 <- survey_2019_20.c %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = '2019-20',
            sample.f = n(),
            violence.f.nw = sum(violence.f),
            violence.f.w = sum(violence.f * new_weight),
            violence.f.w2 = sum(violence.f * new_weight2),
            violence.f.wo = sum(violence.f * PES),
            property_personal.f.nw = sum(property_personal.f),
            property_personal.f.w = sum(property_personal.f * new_weight),
            property_personal.f.w2 = sum(property_personal.f * new_weight2),
            property_personal.f.wo = sum(property_personal.f * PES),
            property_vh.f.nw = sum(property_vh.f),
            property_vh.f.w = sum(property_vh.f * new_weight),
            property_vh.f.w2 = sum(property_vh.f * new_weight2),
            property_vh.f.wo = sum(property_vh.f * PES)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.f.nw = (violence.f.nw / TOTAL) * 1000,
    violence.f.w = (violence.f.w / TOTAL) * 1000,
    violence.f.w2 = (violence.f.w2 / TOTAL) * 1000,
    #property_personal.f.nw = (property_personal.f.nw / TOTAL) * 1000,
    property_personal.f.w = (property_personal.f.w / TOTAL) * 1000,
    property_personal.f.w2 = (property_personal.f.w2 / TOTAL) * 1000,
    #property_vh.f.nw = (property_vh.f.nw / TOTAL) * 1000,
    property_vh.f.w = (property_vh.f.w / TOTAL) * 1000,
    property_vh.f.w2 = (property_vh.f.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_happen_2015_20 <- survey_2015_20.c %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = '2015-20',
            sample.f = n(),
            violence.f.nw = sum(violence.f),
            violence.f.w = sum(violence.f * new_weight),
            violence.f.w2 = sum(violence.f * new_weight2),
            violence.f.wo = sum(violence.f * PES),
            property_personal.f.nw = sum(property_personal.f),
            property_personal.f.w = sum(property_personal.f * new_weight),
            property_personal.f.w2 = sum(property_personal.f * new_weight2),
            property_personal.f.wo = sum(property_personal.f * PES),
            property_vh.f.nw = sum(property_vh.f),
            property_vh.f.w = sum(property_vh.f * new_weight),
            property_vh.f.w2 = sum(property_vh.f * new_weight2),
            property_vh.f.wo = sum(property_vh.f * PES)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.f.nw = (violence.f.nw / TOTAL) * 1000,
    violence.f.w = (violence.f.w / TOTAL) * 1000,
    violence.f.w2 = (violence.f.w2 / TOTAL) * 1000,
    #property_personal.f.nw = (property_personal.f.nw / TOTAL) * 1000,
    property_personal.f.w = (property_personal.f.w / TOTAL) * 1000,
    property_personal.f.w2 = (property_personal.f.w2 / TOTAL) * 1000,
    #property_vh.f.nw = (property_vh.f.nw / TOTAL) * 1000,
    property_vh.f.w = (property_vh.f.w / TOTAL) * 1000,
    property_vh.f.w2 = (property_vh.f.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)

#merge with neighbourhood police crime counts
area_happen <- rbind(area_happen_2015, area_happen_2016,
                     area_happen_2017, area_happen_2018, 
                     area_happen_2019, area_happen_2020,
                     area_happen_2015_16, area_happen_2017_18,
                     area_happen_2019_20, area_happen_2015_20)
barris <- barris %>%
  left_join(area_happen, c("Barri" = "CodMunBa16.f", "Any" = "year"))

#replace NAs with 0
barris <- barris %>%
  mutate(sample.f = ifelse(is.na(sample.f) & Any != "2014", 0, sample.f),
         violence.f.nw = ifelse(is.na(violence.f.nw) & Any != "2014", 0, violence.f.nw),
         violence.f.w = ifelse(is.na(violence.f.w) & Any != "2014", 0, violence.f.w),
         property_personal.f.nw = ifelse(is.na(property_personal.f.nw) & Any != "2014", 0, property_personal.f.nw),
         property_personal.f.w = ifelse(is.na(property_personal.f.w) & Any != "2014", 0, property_personal.f.w),
         property_vh.f.nw = ifelse(is.na(property_vh.f.nw) & Any != "2014", 0, property_vh.f.nw),
         property_vh.f.w = ifelse(is.na(property_vh.f.w) & Any != "2014", 0, property_vh.f.w))

#check reporting rates by crime type
survey_2015_20.c %>%
  filter(violence.f == 1 & !is.na(CodMunBa16.f)) %>%
  group_by(SIG) %>%
  summarise(n = n())  %>%
  mutate(freq = n / sum(n) * 100)
survey_2015_20.c %>%
  filter(property_personal.f == 1 & !is.na(CodMunBa16.f)) %>%
  group_by(SIG) %>%
  summarise(n = n())  %>%
  mutate(freq = n / sum(n) * 100)
survey_2015_20.c %>%
  filter(property_vh.f == 1 & !is.na(CodMunBa16.f)) %>%
  group_by(SIG) %>%
  summarise(n = n())  %>%
  mutate(freq = n / sum(n) * 100)

#check when crime happen in area of residence
survey_2015_20.c %>%
  filter(violence.f == 1 & !is.na(CodMunBa16.f)) %>%
  group_by(CodMunBa16 == CodMunBa16.f) %>%
  summarise(n = n())  %>%
  mutate(freq = n / sum(n) * 100)
survey_2015_20.c %>%
  filter(property_personal.f == 1 & !is.na(CodMunBa16.f)) %>%
  group_by(CodMunBa16 == CodMunBa16.f) %>%
  summarise(n = n())  %>%
  mutate(freq = n / sum(n) * 100)
survey_2015_20.c %>%
  filter(property_vh.f == 1 & !is.na(CodMunBa16.f)) %>%
  group_by(CodMunBa16 == CodMunBa16.f) %>%
  summarise(n = n())  %>%
  mutate(freq = n / sum(n) * 100)

#check if reporting is larger when crime happen in local area
survey_2015_20.c %>%
  filter(violence.f == 1 & !is.na(CodMunBa16.f)) %>%
  group_by(CodMunBa16 == CodMunBa16.f, SIG) %>%
  filter(SIG != 'NA') %>%
  summarise(n = n())  %>%
  mutate(freq = n / sum(n) * 100)
survey_2015_20.c %>%
  filter(property_personal.f == 1 & !is.na(CodMunBa16.f)) %>%
  group_by(CodMunBa16 == CodMunBa16.f, SIG) %>%
  filter(SIG != 'NA') %>%
  summarise(n = n())  %>%
  mutate(freq = n / sum(n) * 100)
survey_2015_20.c %>%
  filter(property_vh.f == 1 & !is.na(CodMunBa16.f)) %>%
  group_by(CodMunBa16 == CodMunBa16.f, SIG) %>%
  filter(SIG != 'NA') %>%
  summarise(n = n())  %>%
  mutate(freq = n / sum(n) * 100)

#select those that were reported to the police
survey_2015.p <- survey_2015.c %>%
  filter(SIG == "yes")
survey_2016.p <- survey_2016.c %>%
  filter(SIG == "yes")
survey_2017.p <- survey_2017.c %>%
  filter(SIG == "yes")
survey_2018.p <- survey_2018.c %>%
  filter(SIG == "yes")
survey_2019.p <- survey_2019.c %>%
  filter(SIG == "yes")
survey_2020.p <- survey_2020.c %>%
  filter(SIG == "yes")
survey_2015_16.p <- survey_2015_16.c %>%
  filter(SIG == "yes")
survey_2017_18.p <- survey_2017_18.c %>%
  filter(SIG == "yes")
survey_2019_20.p <- survey_2019_20.c %>%
  filter(SIG == "yes")
survey_2015_20.p <- survey_2015_20.c %>%
  filter(SIG == "yes")

#create new weight by dividing weights by number of crimes per respondent
survey_2015.p <- survey_2015.p %>%
  group_by(NUMQ) %>%
  mutate(num.crimes.p = n()) %>%
  mutate(new_weight3 = new_weight / num.crimes.p)
survey_2016.p <- survey_2016.p %>%
  group_by(NUMQ) %>%
  mutate(num.crimes.p = n()) %>%
  mutate(new_weight3 = new_weight / num.crimes.p)
survey_2017.p <- survey_2017.p %>%
  group_by(NUMQ) %>%
  mutate(num.crimes.p = n()) %>%
  mutate(new_weight3 = new_weight / num.crimes.p)
survey_2018.p <- survey_2018.p %>%
  group_by(NUMQ) %>%
  mutate(num.crimes.p = n()) %>%
  mutate(new_weight3 = new_weight / num.crimes.p)
survey_2019.p <- survey_2019.p %>%
  group_by(NUMQ) %>%
  mutate(num.crimes.p = n()) %>%
  mutate(new_weight3 = new_weight / num.crimes.p)
survey_2020.p <- survey_2020.p %>%
  group_by(NUMQ) %>%
  mutate(num.crimes.p = n()) %>%
  mutate(new_weight3 = new_weight / num.crimes.p)
survey_2015_16.p <- survey_2015_16.p %>%
  group_by(NUMQ) %>%
  mutate(num.crimes.p = n()) %>%
  mutate(new_weight3 = new_weight / num.crimes.p)
survey_2017_18.p <- survey_2017_18.p %>%
  group_by(NUMQ) %>%
  mutate(num.crimes.p = n()) %>%
  mutate(new_weight3 = new_weight / num.crimes.p)
survey_2019_20.p <- survey_2019_20.p %>%
  group_by(NUMQ) %>%
  mutate(num.crimes.p = n()) %>%
  mutate(new_weight3 = new_weight / num.crimes.p)
survey_2015_20.p <- survey_2015_20.p %>%
  group_by(NUMQ) %>%
  mutate(num.crimes.p = n()) %>%
  mutate(new_weight3 = new_weight / num.crimes.p)

#estimate crimes known to police where they happen
area_police_2015 <- survey_2015.p %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = 2015,
            sample.p = n(),
            violence.p = sum(violence.f),
            violence.p.w = sum(violence.f * new_weight),
            violence.p.w2 = sum(violence.f * new_weight3),
            violence.p.wo = sum(violence.f * PES.x),
            property_personal.p = sum(property_personal.f),
            property_personal.p.w = sum(property_personal.f * new_weight),
            property_personal.p.w2 = sum(property_personal.f * new_weight3),
            property_personal.p.wo = sum(property_personal.f * PES.x),
            property_vh.p = sum(property_vh.f),
            property_vh.p.w = sum(property_vh.f * new_weight),
            property_vh.p.w2 = sum(property_vh.f * new_weight3),
            property_vh.p.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.p = (violence.p / TOTAL) * 1000,
         violence.p.w = (violence.p.w / TOTAL) * 1000,
         violence.p.w2 = (violence.p.w2 / TOTAL) * 1000,
         #property_personal.p = (property_personal.p / TOTAL) * 1000,
         property_personal.p.w = (property_personal.p.w / TOTAL) * 1000,
         property_personal.p.w2 = (property_personal.p.w2 / TOTAL) * 1000,
         #property_vh.p = (property_vh.p / TOTAL) * 1000,
         property_vh.p.w = (property_vh.p.w / TOTAL) * 1000,
         property_vh.p.w2 = (property_vh.p.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_police_2016 <- survey_2016.p %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = 2016,
            sample.p = n(),
            violence.p = sum(violence.f),
            violence.p.w = sum(violence.f * new_weight),
            violence.p.w2 = sum(violence.f * new_weight3),
            violence.p.wo = sum(violence.f * PES.x),
            property_personal.p = sum(property_personal.f),
            property_personal.p.w = sum(property_personal.f * new_weight),
            property_personal.p.w2 = sum(property_personal.f * new_weight3),
            property_personal.p.wo = sum(property_personal.f * PES.x),
            property_vh.p = sum(property_vh.f),
            property_vh.p.w = sum(property_vh.f * new_weight),
            property_vh.p.w2 = sum(property_vh.f * new_weight3),
            property_vh.p.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.p = (violence.p / TOTAL) * 1000,
    violence.p.w = (violence.p.w / TOTAL) * 1000,
    violence.p.w2 = (violence.p.w2 / TOTAL) * 1000,
    #property_personal.p = (property_personal.p / TOTAL) * 1000,
    property_personal.p.w = (property_personal.p.w / TOTAL) * 1000,
    property_personal.p.w2 = (property_personal.p.w2 / TOTAL) * 1000,
    #property_vh.p = (property_vh.p / TOTAL) * 1000,
    property_vh.p.w = (property_vh.p.w / TOTAL) * 1000,
    property_vh.p.w2 = (property_vh.p.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_police_2017 <- survey_2017.p %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = 2017,
            sample.p = n(),
            violence.p = sum(violence.f),
            violence.p.w = sum(violence.f * new_weight),
            violence.p.w2 = sum(violence.f * new_weight3),
            violence.p.wo = sum(violence.f * PES.x),
            property_personal.p = sum(property_personal.f),
            property_personal.p.w = sum(property_personal.f * new_weight),
            property_personal.p.w2 = sum(property_personal.f * new_weight3),
            property_personal.p.wo = sum(property_personal.f * PES.x),
            property_vh.p = sum(property_vh.f),
            property_vh.p.w = sum(property_vh.f * new_weight),
            property_vh.p.w2 = sum(property_vh.f * new_weight3),
            property_vh.p.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.p = (violence.p / TOTAL) * 1000,
    violence.p.w = (violence.p.w / TOTAL) * 1000,
    violence.p.w2 = (violence.p.w2 / TOTAL) * 1000,
    #property_personal.p = (property_personal.p / TOTAL) * 1000,
    property_personal.p.w = (property_personal.p.w / TOTAL) * 1000,
    property_personal.p.w2 = (property_personal.p.w2 / TOTAL) * 1000,
    #property_vh.p = (property_vh.p / TOTAL) * 1000,
    property_vh.p.w = (property_vh.p.w / TOTAL) * 1000,
    property_vh.p.w2 = (property_vh.p.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_police_2018 <- survey_2018.p %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = 2018,
            sample.p = n(),
            violence.p = sum(violence.f),
            violence.p.w = sum(violence.f * new_weight),
            violence.p.w2 = sum(violence.f * new_weight3),
            violence.p.wo = sum(violence.f * PES.x),
            property_personal.p = sum(property_personal.f),
            property_personal.p.w = sum(property_personal.f * new_weight),
            property_personal.p.w2 = sum(property_personal.f * new_weight3),
            property_personal.p.wo = sum(property_personal.f * PES.x),
            property_vh.p = sum(property_vh.f),
            property_vh.p.w = sum(property_vh.f * new_weight),
            property_vh.p.w2 = sum(property_vh.f * new_weight3),
            property_vh.p.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.p = (violence.p / TOTAL) * 1000,
    violence.p.w = (violence.p.w / TOTAL) * 1000,
    violence.p.w2 = (violence.p.w2 / TOTAL) * 1000,
    #property_personal.p = (property_personal.p / TOTAL) * 1000,
    property_personal.p.w = (property_personal.p.w / TOTAL) * 1000,
    property_personal.p.w2 = (property_personal.p.w2 / TOTAL) * 1000,
    #property_vh.p = (property_vh.p / TOTAL) * 1000,
    property_vh.p.w = (property_vh.p.w / TOTAL) * 1000,
    property_vh.p.w2 = (property_vh.p.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_police_2019 <- survey_2019.p %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = 2019,
            sample.p = n(),
            violence.p = sum(violence.f),
            violence.p.w = sum(violence.f * new_weight),
            violence.p.w2 = sum(violence.f * new_weight3),
            violence.p.wo = sum(violence.f * PES.x),
            property_personal.p = sum(property_personal.f),
            property_personal.p.w = sum(property_personal.f * new_weight),
            property_personal.p.w2 = sum(property_personal.f * new_weight3),
            property_personal.p.wo = sum(property_personal.f * PES.x),
            property_vh.p = sum(property_vh.f),
            property_vh.p.w = sum(property_vh.f * new_weight),
            property_vh.p.w2 = sum(property_vh.f * new_weight3),
            property_vh.p.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.p = (violence.p / TOTAL) * 1000,
    violence.p.w = (violence.p.w / TOTAL) * 1000,
    violence.p.w2 = (violence.p.w2 / TOTAL) * 1000,
    #property_personal.p = (property_personal.p / TOTAL) * 1000,
    property_personal.p.w = (property_personal.p.w / TOTAL) * 1000,
    property_personal.p.w2 = (property_personal.p.w2 / TOTAL) * 1000,
    #property_vh.p = (property_vh.p / TOTAL) * 1000,
    property_vh.p.w = (property_vh.p.w / TOTAL) * 1000,
    property_vh.p.w2 = (property_vh.p.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_police_2020 <- survey_2020.p %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = 2020,
            sample.p = n(),
            violence.p = sum(violence.f),
            violence.p.w = sum(violence.f * new_weight),
            violence.p.w2 = sum(violence.f * new_weight3),
            violence.p.wo = sum(violence.f * PES.x),
            property_personal.p = sum(property_personal.f),
            property_personal.p.w = sum(property_personal.f * new_weight),
            property_personal.p.w2 = sum(property_personal.f * new_weight3),
            property_personal.p.wo = sum(property_personal.f * PES.x),
            property_vh.p = sum(property_vh.f),
            property_vh.p.w = sum(property_vh.f * new_weight),
            property_vh.p.w2 = sum(property_vh.f * new_weight3),
            property_vh.p.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.p = (violence.p / TOTAL) * 1000,
    violence.p.w = (violence.p.w / TOTAL) * 1000,
    violence.p.w2 = (violence.p.w2 / TOTAL) * 1000,
    #property_personal.p = (property_personal.p / TOTAL) * 1000,
    property_personal.p.w = (property_personal.p.w / TOTAL) * 1000,
    property_personal.p.w2 = (property_personal.p.w2 / TOTAL) * 1000,
    #property_vh.p = (property_vh.p / TOTAL) * 1000,
    property_vh.p.w = (property_vh.p.w / TOTAL) * 1000,
    property_vh.p.w2 = (property_vh.p.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_police_2015_16 <- survey_2015_16.p %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = '2015-16',
            sample.p = n(),
            violence.p = sum(violence.f),
            violence.p.w = sum(violence.f * new_weight),
            violence.p.w2 = sum(violence.f * new_weight3),
            violence.p.wo = sum(violence.f * PES),
            property_personal.p = sum(property_personal.f),
            property_personal.p.w = sum(property_personal.f * new_weight),
            property_personal.p.w2 = sum(property_personal.f * new_weight3),
            property_personal.p.wo = sum(property_personal.f * PES),
            property_vh.p = sum(property_vh.f),
            property_vh.p.w = sum(property_vh.f * new_weight),
            property_vh.p.w2 = sum(property_vh.f * new_weight3),
            property_vh.p.wo = sum(property_vh.f * PES)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.p = (violence.p / TOTAL) * 1000,
    violence.p.w = (violence.p.w / TOTAL) * 1000,
    violence.p.w2 = (violence.p.w2 / TOTAL) * 1000,
    #property_personal.p = (property_personal.p / TOTAL) * 1000,
    property_personal.p.w = (property_personal.p.w / TOTAL) * 1000,
    property_personal.p.w2 = (property_personal.p.w2 / TOTAL) * 1000,
    #property_vh.p = (property_vh.p / TOTAL) * 1000,
    property_vh.p.w = (property_vh.p.w / TOTAL) * 1000,
    property_vh.p.w2 = (property_vh.p.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_police_2017_18 <- survey_2017_18.p %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = '2017-18',
            sample.p = n(),
            violence.p = sum(violence.f),
            violence.p.w = sum(violence.f * new_weight),
            violence.p.w2 = sum(violence.f * new_weight3),
            violence.p.wo = sum(violence.f * PES),
            property_personal.p = sum(property_personal.f),
            property_personal.p.w = sum(property_personal.f * new_weight),
            property_personal.p.w2 = sum(property_personal.f * new_weight3),
            property_personal.p.wo = sum(property_personal.f * PES),
            property_vh.p = sum(property_vh.f),
            property_vh.p.w = sum(property_vh.f * new_weight),
            property_vh.p.w2 = sum(property_vh.f * new_weight3),
            property_vh.p.wo = sum(property_vh.f * PES)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.p = (violence.p / TOTAL) * 1000,
    violence.p.w = (violence.p.w / TOTAL) * 1000,
    violence.p.w2 = (violence.p.w2 / TOTAL) * 1000,
    #property_personal.p = (property_personal.p / TOTAL) * 1000,
    property_personal.p.w = (property_personal.p.w / TOTAL) * 1000,
    property_personal.p.w2 = (property_personal.p.w2 / TOTAL) * 1000,
    #property_vh.p = (property_vh.p / TOTAL) * 1000,
    property_vh.p.w = (property_vh.p.w / TOTAL) * 1000,
    property_vh.p.w2 = (property_vh.p.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_police_2019_20 <- survey_2019_20.p %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = '2019-20',
            sample.p = n(),
            violence.p = sum(violence.f),
            violence.p.w = sum(violence.f * new_weight),
            violence.p.w2 = sum(violence.f * new_weight3),
            violence.p.wo = sum(violence.f * PES),
            property_personal.p = sum(property_personal.f),
            property_personal.p.w = sum(property_personal.f * new_weight),
            property_personal.p.w2 = sum(property_personal.f * new_weight3),
            property_personal.p.wo = sum(property_personal.f * PES),
            property_vh.p = sum(property_vh.f),
            property_vh.p.w = sum(property_vh.f * new_weight),
            property_vh.p.w2 = sum(property_vh.f * new_weight3),
            property_vh.p.wo = sum(property_vh.f * PES)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.p = (violence.p / TOTAL) * 1000,
    violence.p.w = (violence.p.w / TOTAL) * 1000,
    violence.p.w2 = (violence.p.w2 / TOTAL) * 1000,
    #property_personal.p = (property_personal.p / TOTAL) * 1000,
    property_personal.p.w = (property_personal.p.w / TOTAL) * 1000,
    property_personal.p.w2 = (property_personal.p.w2 / TOTAL) * 1000,
    #property_vh.p = (property_vh.p / TOTAL) * 1000,
    property_vh.p.w = (property_vh.p.w / TOTAL) * 1000,
    property_vh.p.w2 = (property_vh.p.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_police_2015_20 <- survey_2015_20.p %>%
  group_by(CodMunBa16.f) %>%
  summarise(year = '2015-20',
            sample.p = n(),
            violence.p = sum(violence.f),
            violence.p.w = sum(violence.f * new_weight),
            violence.p.w2 = sum(violence.f * new_weight3),
            violence.p.wo = sum(violence.f * PES),
            property_personal.p = sum(property_personal.f),
            property_personal.p.w = sum(property_personal.f * new_weight),
            property_personal.p.w2 = sum(property_personal.f * new_weight3),
            property_personal.p.wo = sum(property_personal.f * PES),
            property_vh.p = sum(property_vh.f),
            property_vh.p.w = sum(property_vh.f * new_weight),
            property_vh.p.w2 = sum(property_vh.f * new_weight3),
            property_vh.p.wo = sum(property_vh.f * PES)) %>%
  left_join(pop, by = c("CodMunBa16.f" = "Barri")) %>%
  mutate(#violence.p = (violence.p / TOTAL) * 1000,
    violence.p.w = (violence.p.w / TOTAL) * 1000,
    violence.p.w2 = (violence.p.w2 / TOTAL) * 1000,
    #property_personal.p = (property_personal.p / TOTAL) * 1000,
    property_personal.p.w = (property_personal.p.w / TOTAL) * 1000,
    property_personal.p.w2 = (property_personal.p.w2 / TOTAL) * 1000,
    #property_vh.p = (property_vh.p / TOTAL) * 1000,
    property_vh.p.w = (property_vh.p.w / TOTAL) * 1000,
    property_vh.p.w2 = (property_vh.p.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)

#merge with neighbourhood police crime counts
area_police <- rbind(area_police_2015, area_police_2016,
                     area_police_2017, area_police_2018, 
                     area_police_2019, area_police_2020,
                     area_police_2015_16, area_police_2017_18,
                     area_police_2019_20, area_police_2015_20)
barris <- barris %>%
  left_join(area_police, c("Barri" = "CodMunBa16.f", "Any" = "year"))

#replace NAs with 0
barris <- barris %>%
  mutate(sample.p = ifelse(is.na(sample.p) & Any != "2014", 0, sample.p),
         violence.p = ifelse(is.na(violence.p) & Any != "2014", 0, violence.p),
         violence.p.w = ifelse(is.na(violence.p.w) & Any != "2014", 0, violence.p.w),
         violence.p.w2 = ifelse(is.na(violence.p.w2) & Any != "2014", 0, violence.p.w2),
         violence.p.wo = ifelse(is.na(violence.p.wo) & Any != "2014", 0, violence.p.wo),
         violence.f.w2 = ifelse(is.na(violence.f.w2) & Any != "2014", 0, violence.f.w2),
         violence.f.wo = ifelse(is.na(violence.f.wo) & Any != "2014", 0, violence.f.wo),
         property_personal.p = ifelse(is.na(property_personal.p) & Any != "2014", 0, property_personal.p),
         property_personal.p.w = ifelse(is.na(property_personal.p.w) & Any != "2014", 0, property_personal.p.w),
         property_personal.p.w2 = ifelse(is.na(property_personal.p.w2) & Any != "2014", 0, property_personal.p.w2),
         property_personal.p.wo = ifelse(is.na(property_personal.p.wo) & Any != "2014", 0, property_personal.p.wo),
         property_personal.f.w2 = ifelse(is.na(property_personal.f.w2) & Any != "2014", 0, property_personal.f.w2),
         property_personal.f.wo = ifelse(is.na(property_personal.f.wo) & Any != "2014", 0, property_personal.f.wo),
         property_vh.p = ifelse(is.na(property_vh.p) & Any != "2014", 0, property_vh.p),
         property_vh.p.w = ifelse(is.na(property_vh.p.w) & Any != "2014", 0, property_vh.p.w),
         property_vh.p.w2 = ifelse(is.na(property_vh.p.w2) & Any != "2014", 0, property_vh.p.w2),
         property_vh.p.wo = ifelse(is.na(property_vh.p.wo) & Any != "2014", 0, property_vh.p.wo),
         property_vh.f.w2 = ifelse(is.na(property_vh.f.w2) & Any != "2014", 0, property_vh.f.w2),
         property_vh.f.wo = ifelse(is.na(property_vh.f.wo) & Any != "2014", 0, property_vh.f.wo))

#estimate crimes known to police where victims live
area_rp_2015 <- survey_2015.p %>%
  group_by(CodMunBa16) %>%
  summarise(year = 2015,
            sample.rp = n(),
            violence.rp = sum(violence.f),
            violence.rp.w = sum(violence.f * new_weight),
            violence.rp.w2 = sum(violence.f * new_weight3),
            violence.rp.wo = sum(violence.f * PES.x),
            property_personal.rp = sum(property_personal.f),
            property_personal.rp.w = sum(property_personal.f * new_weight),
            property_personal.rp.w2 = sum(property_personal.f * new_weight3),
            property_personal.rp.wo = sum(property_personal.f * PES.x),
            property_vh.rp = sum(property_vh.f),
            property_vh.rp.w = sum(property_vh.f * new_weight),
            property_vh.rp.w2 = sum(property_vh.f * new_weight3),
            property_vh.rp.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16" = "Barri")) %>%
  mutate(#violence.rp = (violence.rp / TOTAL) * 1000,
    violence.rp.w = (violence.rp.w / TOTAL) * 1000,
    violence.rp.w2 = (violence.rp.w2 / TOTAL) * 1000,
    #property_personal.rp = (property_personal.rp / TOTAL) * 1000,
    property_personal.rp.w = (property_personal.rp.w / TOTAL) * 1000,
    property_personal.rp.w2 = (property_personal.rp.w2 / TOTAL) * 1000,
    #property_vh.rp = (property_vh.rp / TOTAL) * 1000,
    property_vh.rp.w = (property_vh.rp.w / TOTAL) * 1000,
    property_vh.rp.w2 = (property_vh.rp.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_rp_2016 <- survey_2016.p %>%
  group_by(CodMunBa16) %>%
  summarise(year = 2016,
            sample.rp = n(),
            violence.rp = sum(violence.f),
            violence.rp.w = sum(violence.f * new_weight),
            violence.rp.w2 = sum(violence.f * new_weight3),
            violence.rp.wo = sum(violence.f * PES.x),
            property_personal.rp = sum(property_personal.f),
            property_personal.rp.w = sum(property_personal.f * new_weight),
            property_personal.rp.w2 = sum(property_personal.f * new_weight3),
            property_personal.rp.wo = sum(property_personal.f * PES.x),
            property_vh.rp = sum(property_vh.f),
            property_vh.rp.w = sum(property_vh.f * new_weight),
            property_vh.rp.w2 = sum(property_vh.f * new_weight3),
            property_vh.rp.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16" = "Barri")) %>%
  mutate(#violence.rp = (violence.rp / TOTAL) * 1000,
    violence.rp.w = (violence.rp.w / TOTAL) * 1000,
    violence.rp.w2 = (violence.rp.w2 / TOTAL) * 1000,
    #property_personal.rp = (property_personal.rp / TOTAL) * 1000,
    property_personal.rp.w = (property_personal.rp.w / TOTAL) * 1000,
    property_personal.rp.w2 = (property_personal.rp.w2 / TOTAL) * 1000,
    #property_vh.rp = (property_vh.rp / TOTAL) * 1000,
    property_vh.rp.w = (property_vh.rp.w / TOTAL) * 1000,
    property_vh.rp.w2 = (property_vh.rp.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_rp_2017 <- survey_2017.p %>%
  group_by(CodMunBa16) %>%
  summarise(year = 2017,
            sample.rp = n(),
            violence.rp = sum(violence.f),
            violence.rp.w = sum(violence.f * new_weight),
            violence.rp.w2 = sum(violence.f * new_weight3),
            violence.rp.wo = sum(violence.f * PES.x),
            property_personal.rp = sum(property_personal.f),
            property_personal.rp.w = sum(property_personal.f * new_weight),
            property_personal.rp.w2 = sum(property_personal.f * new_weight3),
            property_personal.rp.wo = sum(property_personal.f * PES.x),
            property_vh.rp = sum(property_vh.f),
            property_vh.rp.w = sum(property_vh.f * new_weight),
            property_vh.rp.w2 = sum(property_vh.f * new_weight3),
            property_vh.rp.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16" = "Barri")) %>%
  mutate(#violence.rp = (violence.rp / TOTAL) * 1000,
    violence.rp.w = (violence.rp.w / TOTAL) * 1000,
    violence.rp.w2 = (violence.rp.w2 / TOTAL) * 1000,
    #property_personal.rp = (property_personal.rp / TOTAL) * 1000,
    property_personal.rp.w = (property_personal.rp.w / TOTAL) * 1000,
    property_personal.rp.w2 = (property_personal.rp.w2 / TOTAL) * 1000,
    #property_vh.rp = (property_vh.rp / TOTAL) * 1000,
    property_vh.rp.w = (property_vh.rp.w / TOTAL) * 1000,
    property_vh.rp.w2 = (property_vh.rp.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_rp_2018 <- survey_2018.p %>%
  group_by(CodMunBa16) %>%
  summarise(year = 2018,
            sample.rp = n(),
            violence.rp = sum(violence.f),
            violence.rp.w = sum(violence.f * new_weight),
            violence.rp.w2 = sum(violence.f * new_weight3),
            violence.rp.wo = sum(violence.f * PES.x),
            property_personal.rp = sum(property_personal.f),
            property_personal.rp.w = sum(property_personal.f * new_weight),
            property_personal.rp.w2 = sum(property_personal.f * new_weight3),
            property_personal.rp.wo = sum(property_personal.f * PES.x),
            property_vh.rp = sum(property_vh.f),
            property_vh.rp.w = sum(property_vh.f * new_weight),
            property_vh.rp.w2 = sum(property_vh.f * new_weight3),
            property_vh.rp.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16" = "Barri")) %>%
  mutate(#violence.rp = (violence.rp / TOTAL) * 1000,
    violence.rp.w = (violence.rp.w / TOTAL) * 1000,
    violence.rp.w2 = (violence.rp.w2 / TOTAL) * 1000,
    #property_personal.rp = (property_personal.rp / TOTAL) * 1000,
    property_personal.rp.w = (property_personal.rp.w / TOTAL) * 1000,
    property_personal.rp.w2 = (property_personal.rp.w2 / TOTAL) * 1000,
    #property_vh.rp = (property_vh.rp / TOTAL) * 1000,
    property_vh.rp.w = (property_vh.rp.w / TOTAL) * 1000,
    property_vh.rp.w2 = (property_vh.rp.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_rp_2019 <- survey_2019.p %>%
  group_by(CodMunBa16) %>%
  summarise(year = 2019,
            sample.rp = n(),
            violence.rp = sum(violence.f),
            violence.rp.w = sum(violence.f * new_weight),
            violence.rp.w2 = sum(violence.f * new_weight3),
            violence.rp.wo = sum(violence.f * PES.x),
            property_personal.rp = sum(property_personal.f),
            property_personal.rp.w = sum(property_personal.f * new_weight),
            property_personal.rp.w2 = sum(property_personal.f * new_weight3),
            property_personal.rp.wo = sum(property_personal.f * PES.x),
            property_vh.rp = sum(property_vh.f),
            property_vh.rp.w = sum(property_vh.f * new_weight),
            property_vh.rp.w2 = sum(property_vh.f * new_weight3),
            property_vh.rp.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16" = "Barri")) %>%
  mutate(#violence.rp = (violence.rp / TOTAL) * 1000,
    violence.rp.w = (violence.rp.w / TOTAL) * 1000,
    violence.rp.w2 = (violence.rp.w2 / TOTAL) * 1000,
    #property_personal.rp = (property_personal.rp / TOTAL) * 1000,
    property_personal.rp.w = (property_personal.rp.w / TOTAL) * 1000,
    property_personal.rp.w2 = (property_personal.rp.w2 / TOTAL) * 1000,
    #property_vh.rp = (property_vh.rp / TOTAL) * 1000,
    property_vh.rp.w = (property_vh.rp.w / TOTAL) * 1000,
    property_vh.rp.w2 = (property_vh.rp.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_rp_2020 <- survey_2020.p %>%
  group_by(CodMunBa16) %>%
  summarise(year = 2020,
            sample.rp = n(),
            violence.rp = sum(violence.f),
            violence.rp.w = sum(violence.f * new_weight),
            violence.rp.w2 = sum(violence.f * new_weight3),
            violence.rp.wo = sum(violence.f * PES.x),
            property_personal.rp = sum(property_personal.f),
            property_personal.rp.w = sum(property_personal.f * new_weight),
            property_personal.rp.w2 = sum(property_personal.f * new_weight3),
            property_personal.rp.wo = sum(property_personal.f * PES.x),
            property_vh.rp = sum(property_vh.f),
            property_vh.rp.w = sum(property_vh.f * new_weight),
            property_vh.rp.w2 = sum(property_vh.f * new_weight3),
            property_vh.rp.wo = sum(property_vh.f * PES.x)) %>%
  left_join(pop, by = c("CodMunBa16" = "Barri")) %>%
  mutate(#violence.rp = (violence.rp / TOTAL) * 1000,
    violence.rp.w = (violence.rp.w / TOTAL) * 1000,
    violence.rp.w2 = (violence.rp.w2 / TOTAL) * 1000,
    #property_personal.rp = (property_personal.rp / TOTAL) * 1000,
    property_personal.rp.w = (property_personal.rp.w / TOTAL) * 1000,
    property_personal.rp.w2 = (property_personal.rp.w2 / TOTAL) * 1000,
    #property_vh.rp = (property_vh.rp / TOTAL) * 1000,
    property_vh.rp.w = (property_vh.rp.w / TOTAL) * 1000,
    property_vh.rp.w2 = (property_vh.rp.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_rp_2015_16 <- survey_2015_16.p %>%
  group_by(CodMunBa16) %>%
  summarise(year = '2015-16',
            sample.rp = n(),
            violence.rp = sum(violence.f),
            violence.rp.w = sum(violence.f * new_weight),
            violence.rp.w2 = sum(violence.f * new_weight3),
            violence.rp.wo = sum(violence.f * PES),
            property_personal.rp = sum(property_personal.f),
            property_personal.rp.w = sum(property_personal.f * new_weight),
            property_personal.rp.w2 = sum(property_personal.f * new_weight3),
            property_personal.rp.wo = sum(property_personal.f * PES),
            property_vh.rp = sum(property_vh.f),
            property_vh.rp.w = sum(property_vh.f * new_weight),
            property_vh.rp.w2 = sum(property_vh.f * new_weight3),
            property_vh.rp.wo = sum(property_vh.f * PES)) %>%
  left_join(pop, by = c("CodMunBa16" = "Barri")) %>%
  mutate(#violence.rp = (violence.rp / TOTAL) * 1000,
    violence.rp.w = (violence.rp.w / TOTAL) * 1000,
    violence.rp.w2 = (violence.rp.w2 / TOTAL) * 1000,
    #property_personal.rp = (property_personal.rp / TOTAL) * 1000,
    property_personal.rp.w = (property_personal.rp.w / TOTAL) * 1000,
    property_personal.rp.w2 = (property_personal.rp.w2 / TOTAL) * 1000,
    #property_vh.rp = (property_vh.rp / TOTAL) * 1000,
    property_vh.rp.w = (property_vh.rp.w / TOTAL) * 1000,
    property_vh.rp.w2 = (property_vh.rp.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_rp_2017_18 <- survey_2017_18.p %>%
  group_by(CodMunBa16) %>%
  summarise(year = '2017-18',
            sample.rp = n(),
            violence.rp = sum(violence.f),
            violence.rp.w = sum(violence.f * new_weight),
            violence.rp.w2 = sum(violence.f * new_weight3),
            violence.rp.wo = sum(violence.f * PES),
            property_personal.rp = sum(property_personal.f),
            property_personal.rp.w = sum(property_personal.f * new_weight),
            property_personal.rp.w2 = sum(property_personal.f * new_weight3),
            property_personal.rp.wo = sum(property_personal.f * PES),
            property_vh.rp = sum(property_vh.f),
            property_vh.rp.w = sum(property_vh.f * new_weight),
            property_vh.rp.w2 = sum(property_vh.f * new_weight3),
            property_vh.rp.wo = sum(property_vh.f * PES)) %>%
  left_join(pop, by = c("CodMunBa16" = "Barri")) %>%
  mutate(#violence.rp = (violence.rp / TOTAL) * 1000,
    violence.rp.w = (violence.rp.w / TOTAL) * 1000,
    violence.rp.w2 = (violence.rp.w2 / TOTAL) * 1000,
    #property_personal.rp = (property_personal.rp / TOTAL) * 1000,
    property_personal.rp.w = (property_personal.rp.w / TOTAL) * 1000,
    property_personal.rp.w2 = (property_personal.rp.w2 / TOTAL) * 1000,
    #property_vh.rp = (property_vh.rp / TOTAL) * 1000,
    property_vh.rp.w = (property_vh.rp.w / TOTAL) * 1000,
    property_vh.rp.w2 = (property_vh.rp.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_rp_2019_20 <- survey_2019_20.p %>%
  group_by(CodMunBa16) %>%
  summarise(year = '2019-20',
            sample.rp = n(),
            violence.rp = sum(violence.f),
            violence.rp.w = sum(violence.f * new_weight),
            violence.rp.w2 = sum(violence.f * new_weight3),
            violence.rp.wo = sum(violence.f * PES),
            property_personal.rp = sum(property_personal.f),
            property_personal.rp.w = sum(property_personal.f * new_weight),
            property_personal.rp.w2 = sum(property_personal.f * new_weight3),
            property_personal.rp.wo = sum(property_personal.f * PES),
            property_vh.rp = sum(property_vh.f),
            property_vh.rp.w = sum(property_vh.f * new_weight),
            property_vh.rp.w2 = sum(property_vh.f * new_weight3),
            property_vh.rp.wo = sum(property_vh.f * PES)) %>%
  left_join(pop, by = c("CodMunBa16" = "Barri")) %>%
  mutate(#violence.rp = (violence.rp / TOTAL) * 1000,
    violence.rp.w = (violence.rp.w / TOTAL) * 1000,
    violence.rp.w2 = (violence.rp.w2 / TOTAL) * 1000,
    #property_personal.rp = (property_personal.rp / TOTAL) * 1000,
    property_personal.rp.w = (property_personal.rp.w / TOTAL) * 1000,
    property_personal.rp.w2 = (property_personal.rp.w2 / TOTAL) * 1000,
    #property_vh.rp = (property_vh.rp / TOTAL) * 1000,
    property_vh.rp.w = (property_vh.rp.w / TOTAL) * 1000,
    property_vh.rp.w2 = (property_vh.rp.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)
area_rp_2015_20 <- survey_2015_20.p %>%
  group_by(CodMunBa16) %>%
  summarise(year = '2015-20',
            sample.rp = n(),
            violence.rp = sum(violence.f),
            violence.rp.w = sum(violence.f * new_weight),
            violence.rp.w2 = sum(violence.f * new_weight3),
            violence.rp.wo = sum(violence.f * PES),
            property_personal.rp = sum(property_personal.f),
            property_personal.rp.w = sum(property_personal.f * new_weight),
            property_personal.rp.w2 = sum(property_personal.f * new_weight3),
            property_personal.rp.wo = sum(property_personal.f * PES),
            property_vh.rp = sum(property_vh.f),
            property_vh.rp.w = sum(property_vh.f * new_weight),
            property_vh.rp.w2 = sum(property_vh.f * new_weight3),
            property_vh.rp.wo = sum(property_vh.f * PES)) %>%
  left_join(pop, by = c("CodMunBa16" = "Barri")) %>%
  mutate(#violence.rp = (violence.rp / TOTAL) * 1000,
    violence.rp.w = (violence.rp.w / TOTAL) * 1000,
    violence.rp.w2 = (violence.rp.w2 / TOTAL) * 1000,
    #property_personal.rp = (property_personal.rp / TOTAL) * 1000,
    property_personal.rp.w = (property_personal.rp.w / TOTAL) * 1000,
    property_personal.rp.w2 = (property_personal.rp.w2 / TOTAL) * 1000,
    #property_vh.rp = (property_vh.rp / TOTAL) * 1000,
    property_vh.rp.w = (property_vh.rp.w / TOTAL) * 1000,
    property_vh.rp.w2 = (property_vh.rp.w2 / TOTAL) * 1000) %>%
  filter(!is.na(TOTAL)) %>%
  dplyr::select(-TOTAL)

#merge with neighbourhood police crime counts
area_rp <- rbind(area_rp_2015, area_rp_2016,
                     area_rp_2017, area_rp_2018, 
                     area_rp_2019, area_rp_2020,
                     area_rp_2015_16, area_rp_2017_18,
                     area_rp_2019_20, area_rp_2015_20)
barris <- barris %>%
  left_join(area_rp, c("Barri" = "CodMunBa16", "Any" = "year"))

#replace NAs with 0
barris <- barris %>%
  mutate(sample.rp = ifelse(is.na(sample.rp) & Any != "2014", 0, sample.rp),
         violence.rp = ifelse(is.na(violence.rp) & Any != "2014", 0, violence.rp),
         violence.rp.w = ifelse(is.na(violence.rp.w) & Any != "2014", 0, violence.rp.w),
         violence.rp.w2 = ifelse(is.na(violence.rp.w2) & Any != "2014", 0, violence.rp.w2),
         violence.rp.wo = ifelse(is.na(violence.rp.wo) & Any != "2014", 0, violence.rp.wo),
         property_personal.rp = ifelse(is.na(property_personal.rp) & Any != "2014", 0, property_personal.rp),
         property_personal.rp.w = ifelse(is.na(property_personal.rp.w) & Any != "2014", 0, property_personal.rp.w),
         property_personal.rp.w2 = ifelse(is.na(property_personal.rp.w2) & Any != "2014", 0, property_personal.rp.w2),
         property_personal.rp.wo = ifelse(is.na(property_personal.rp.wo) & Any != "2014", 0, property_personal.rp.wo),
         property_vh.rp = ifelse(is.na(property_vh.rp) & Any != "2014", 0, property_vh.rp),
         property_vh.rp.w = ifelse(is.na(property_vh.rp.w) & Any != "2014", 0, property_vh.rp.w),
         property_vh.rp.w2 = ifelse(is.na(property_vh.rp.w2) & Any != "2014", 0, property_vh.rp.w2),
         property_vh.rp.wo = ifelse(is.na(property_vh.rp.wo) & Any != "2014", 0, property_vh.rp.wo))

#check correlation matrix
cor(barris[c("violence_rate", "violence.r.w", "violence.f.w", "violence.p.w", "violence.rp.w")], use = "pairwise.complete.obs")
cor(barris[c("violence_rate", "violence.r.w", "violence.f.w2", "violence.p.w2", "violence.rp.w2")], use = "pairwise.complete.obs")
cor(barris[c("violence", "violence.r.wo", "violence.f.wo", "violence.p.wo", "violence.rp.wo")], use = "pairwise.complete.obs")
cor(barris[c("property_personal_rate", "property_personal.r.w", "property_personal.f.w", "property_personal.p.w", "property_personal.rp.w")], use = "pairwise.complete.obs")
cor(barris[c("property_personal_rate", "property_personal.r.w", "property_personal.f.w2", "property_personal.p.w2", "property_personal.rp.w2")], use = "pairwise.complete.obs")
cor(barris[c("property_personal", "property_personal.r.wo", "property_personal.f.wo", "property_personal.p.wo", "property_personal.rp.wo")], use = "pairwise.complete.obs")
cor(barris[c("property_vh_rate", "property_vh.r.w", "property_vh.f.w", "property_vh.p.w", "property_vh.rp.w")], use = "pairwise.complete.obs")
cor(barris[c("property_vh_rate", "property_vh.r.w", "property_vh.f.w2", "property_vh.p.w2", "property_vh.rp.w2")], use = "pairwise.complete.obs")
cor(barris[c("property_vh", "property_vh.r.wo", "property_vh.f.wo", "property_vh.p.wo", "property_vh.rp.wo")], use = "pairwise.complete.obs")

#save file
write.csv(barris, here("data/clean/neighbourhoods.csv"))

#select data for two years
barris2 <- barris %>%
  filter(Any == "2015-16" | Any == "2017-18" | Any == "2019-20")

#check correlation matrix
cor(barris2[c("violence_rate", "violence.r.w", "violence.f.w", "violence.p.w", "violence.rp.w")], use = "pairwise.complete.obs")
cor(barris2[c("violence_rate", "violence.r.w", "violence.f.w2", "violence.p.w2", "violence.rp.w2")], use = "pairwise.complete.obs")
cor(barris2[c("violence", "violence.r.wo", "violence.f.wo", "violence.p.wo", "violence.rp.wo")], use = "pairwise.complete.obs")
cor(barris2[c("property_personal_rate", "property_personal.r.w", "property_personal.f.w", "property_personal.p.w", "property_personal.rp.w")], use = "pairwise.complete.obs")
cor(barris2[c("property_personal_rate", "property_personal.r.w", "property_personal.f.w2", "property_personal.p.w2", "property_personal.rp.w2")], use = "pairwise.complete.obs")
cor(barris2[c("property_personal", "property_personal.r.wo", "property_personal.f.wo", "property_personal.p.wo", "property_personal.rp.wo")], use = "pairwise.complete.obs")
cor(barris2[c("property_vh_rate", "property_vh.r.w", "property_vh.f.w", "property_vh.p.w", "property_vh.rp.w")], use = "pairwise.complete.obs")
cor(barris2[c("property_vh_rate", "property_vh.r.w", "property_vh.f.w2", "property_vh.p.w2", "property_vh.rp.w2")], use = "pairwise.complete.obs")
cor(barris2[c("property_vh", "property_vh.r.wo", "property_vh.f.wo", "property_vh.p.wo", "property_vh.rp.wo")], use = "pairwise.complete.obs")

cor(barris2[c("violence", "violence.r", "violence.f.nw", "violence.p", "violence.rp")], use = "pairwise.complete.obs")
cor(barris2[c("property_personal", "property_personal.r", "property_personal.f.nw", "property_personal.p", "property_personal.rp")], use = "pairwise.complete.obs")
cor(barris2[c("property_vh", "property_vh.r", "property_vh.f.nw", "property_vh.p", "property_vh.rp")], use = "pairwise.complete.obs")

#save file
write.csv(barris2, here("data/clean/neighbourhoods_2years.csv"))

#select data for two years
barris3 <- barris %>%
  filter(Any == "2015-20")

#check correlation matrix
cor(barris3[c("violence_rate", "violence.r.w", "violence.f.w", "violence.p.w", "violence.rp.w")], use = "pairwise.complete.obs")
cor(barris3[c("violence_rate", "violence.r.w", "violence.f.w2", "violence.p.w2", "violence.rp.w2")], use = "pairwise.complete.obs")
cor(barris3[c("violence", "violence.r.wo", "violence.f.wo", "violence.p.wo", "violence.rp.wo")], use = "pairwise.complete.obs")
cor(barris3[c("property_personal_rate", "property_personal.r.w", "property_personal.f.w", "property_personal.p.w", "property_personal.rp.w")], use = "pairwise.complete.obs")
cor(barris3[c("property_personal_rate", "property_personal.r.w", "property_personal.f.w2", "property_personal.p.w2", "property_personal.rp.w2")], use = "pairwise.complete.obs")
cor(barris3[c("property_personal", "property_personal.r.wo", "property_personal.f.wo", "property_personal.p.wo", "property_personal.rp.wo")], use = "pairwise.complete.obs")
cor(barris3[c("property_vh_rate", "property_vh.r.w", "property_vh.f.w", "property_vh.p.w", "property_vh.rp.w")], use = "pairwise.complete.obs")
cor(barris3[c("property_vh_rate", "property_vh.r.w", "property_vh.f.w2", "property_vh.p.w2", "property_vh.rp.w2")], use = "pairwise.complete.obs")
cor(barris3[c("property_vh", "property_vh.r.wo", "property_vh.f.wo", "property_vh.p.wo", "property_vh.rp.wo")], use = "pairwise.complete.obs")

#save file
write.csv(barris3, here("data/clean/neighbourhoods_6years.csv"))

#load packages for mapping
library(sf)

# load shapefile
shapefile <- st_read(here("data/shapefile/AMB_and_barris.shp"), quiet=TRUE)

#remove cities
shapefile <- shapefile %>%
  filter(!is.na(c_distri)) %>%
  mutate(n_barri = tolower(n_barri))

#recode some neighbourhood names
shapefile <- shapefile %>%
  mutate(n_barri = recode(n_barri,
                          "sant gervasi - la bonanova" = "sant gervasi-la bonanova",
                          "el poble sec" = "el poble sec-parc montjuic",
                          "sant mart? de provenals" = "sant mart? de proven?als",
                          "provenals del poblenou" = "proven?als del poblenou",
                          "la marina del prat vermell" = "la marina del prat vermell-zona franca", 
                          "sants - badal" = "sants-badal",
                          "sant gervasi - galvany" = "sant gervasi-galvany"))

#add crime data and select 2017-18 data
#shapefile <- shapefile %>%
#  left_join(barris2, by = c("n_barri" = "Barri")) %>%
#  filter(Any == "2019-20")
shapefile <- shapefile %>%
  left_join(barris3, by = c("n_barri" = "Barri"))

#apply linear transformation to crime counts
shapefile <- shapefile %>%
  mutate(violence = ((violence - min(violence)) / (max(violence) - min(violence)))*100,
         violence.r.wo = ((violence.r.wo - min(violence.r.wo)) / (max(violence.r.wo) - min(violence.r.wo)))*100,
         violence.f.wo = ((violence.f.wo - min(violence.f.wo)) / (max(violence.f.wo) - min(violence.f.wo)))*100,
         violence.p.wo = ((violence.p.wo - min(violence.p.wo)) / (max(violence.p.wo) - min(violence.p.wo)))*100,
         violence.rp.wo = ((violence.rp.wo - min(violence.rp.wo)) / (max(violence.rp.wo) - min(violence.rp.wo)))*100,
         property_personal = ((property_personal - min(property_personal)) / (max(property_personal) - min(property_personal)))*100,
         property_personal.r.wo = ((property_personal.r.wo - min(property_personal.r.wo)) / (max(property_personal.r.wo) - min(property_personal.r.wo)))*100,
         property_personal.f.wo = ((property_personal.f.wo - min(property_personal.f.wo)) / (max(property_personal.f.wo) - min(property_personal.f.wo)))*100,
         property_personal.p.wo = ((property_personal.p.wo - min(property_personal.p.wo)) / (max(property_personal.p.wo) - min(property_personal.p.wo)))*100,
         property_personal.rp.wo = ((property_personal.rp.wo - min(property_personal.rp.wo)) / (max(property_personal.rp.wo) - min(property_personal.rp.wo)))*100,
         property_vh = ((property_vh - min(property_vh)) / (max(property_vh) - min(property_vh)))*100,
         property_vh.r.wo = ((property_vh.r.wo - min(property_vh.r.wo)) / (max(property_vh.r.wo) - min(property_vh.r.wo)))*100,
         property_vh.f.wo = ((property_vh.f.wo - min(property_vh.f.wo)) / (max(property_vh.f.wo) - min(property_vh.f.wo)))*100,
         property_vh.p.wo = ((property_vh.p.wo - min(property_vh.p.wo)) / (max(property_vh.p.wo) - min(property_vh.p.wo)))*100,
         property_vh.rp.wo = ((property_vh.rp.wo - min(property_vh.rp.wo)) / (max(property_vh.rp.wo) - min(property_vh.rp.wo)))*100)

#maps for article
options(scipen=999)

M.violence.p <- ggplot(data = shapefile)+
  labs(title = "Violence", subtitle = "Police data")+
  geom_sf(aes(fill = violence), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Transformed\n count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(face = "bold"))

M.violence.r <- ggplot(data = shapefile)+
  labs(title = "Victim residence")+
  geom_sf(aes(fill = violence.r.wo), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

M.violence.f <- ggplot(data = shapefile)+
  labs(title = "Offence location")+
  geom_sf(aes(fill = violence.f.wo), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

M.violence.fp <- ggplot(data = shapefile)+
  labs(title = "Offence location (reported)")+
  geom_sf(aes(fill = violence.p.wo), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

M.violence.rp <- ggplot(data = shapefile)+
  labs(title = "Victim residence (reported)")+
  geom_sf(aes(fill = violence.rp.wo), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

# plot for paper
ggarrange(M.violence.p,
          ggarrange(M.violence.r, M.violence.rp,
                    M.violence.f, M.violence.fp, 
                    nrow = 2, ncol = 2),
          nrow = 1,
          widths = c(1.1, 0.9)
          ) 

M.property_personal.p <- ggplot(data = shapefile)+
  labs(title = "Property (personal)", subtitle = "Police data")+
  geom_sf(aes(fill = property_personal), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Transformed\n count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(face = "bold"))

M.property_personal.r <- ggplot(data = shapefile)+
  labs(title = "Victim residence")+
  geom_sf(aes(fill = property_personal.r.wo), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

M.property_personal.f <- ggplot(data = shapefile)+
  labs(title = "Offence location")+
  geom_sf(aes(fill = property_personal.f.wo), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

M.property_personal.fp <- ggplot(data = shapefile)+
  labs(title = "Offence location (reported)")+
  geom_sf(aes(fill = property_personal.p.wo), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

M.property_personal.rp <- ggplot(data = shapefile)+
  labs(title = "Victim residence (reported)")+
  geom_sf(aes(fill = property_personal.rp.wo), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

# plot for paper
ggarrange(M.property_personal.p,
          ggarrange(M.property_personal.r, M.property_personal.rp, 
                    M.property_personal.f, M.property_personal.fp, 
                    nrow = 2, ncol = 2),
          nrow = 1,
          widths = c(1.1, 0.9)
          ) 

M.property_vh.p <- ggplot(data = shapefile)+
  labs(title = "Property (vehicle and household)", subtitle = "Police data")+
  geom_sf(aes(fill = property_vh), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Transformed\n count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(face = "bold"))

M.property_vh.r <- ggplot(data = shapefile)+
  labs(title = "Victim residence")+
  geom_sf(aes(fill = property_vh.r.wo), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

M.property_vh.f <- ggplot(data = shapefile)+
  labs(title = "Offence location")+
  geom_sf(aes(fill = property_vh.f.wo), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

M.property_vh.fp <- ggplot(data = shapefile)+
  labs(title = "Offence location (reported)")+
  geom_sf(aes(fill = property_vh.p.wo), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

M.property_vh.rp <- ggplot(data = shapefile)+
  labs(title = "Victim residence (reported)")+
  geom_sf(aes(fill = property_vh.rp.wo), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, name = "Count") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

# plot for paper
ggarrange(M.property_vh.p,
          ggarrange(M.property_vh.r, M.property_vh.rp,
                    M.property_vh.f, M.property_vh.fp, 
                    nrow = 2, ncol = 2),
          nrow = 1,
          widths = c(1.1, 0.9)
          ) 

#calculate mean and confidence intervals to plot
confint_df <- rbind(
  c("Violence", "Offence location", mean(barris3$violence.f.wo), t.test(barris3$violence.f.wo)$"conf.int"[1], t.test(barris3$violence.f.wo)$"conf.int"[2], 1),
  c("Violence", "Offence location (report)", mean(barris3$violence.p.wo), t.test(barris3$violence.p.wo)$"conf.int"[1], t.test(barris3$violence.p.wo)$"conf.int"[2], 2),
  c("Violence", "Victim residence", mean(barris3$violence.r.wo), t.test(barris3$violence.r.wo)$"conf.int"[1], t.test(barris3$violence.r.wo)$"conf.int"[2], 3),
  c("Violence", "Victim residence (report)", mean(barris3$violence.rp.wo), t.test(barris3$violence.rp.wo)$"conf.int"[1], t.test(barris3$violence.rp.wo)$"conf.int"[2], 4),
  c("Property (personal)", "Offence location", mean(barris3$property_personal.f.wo), t.test(barris3$property_personal.f.wo)$"conf.int"[1], t.test(barris3$property_personal.f.wo)$"conf.int"[2], 1),
  c("Property (personal)", "Offence location (report)", mean(barris3$property_personal.p.wo), t.test(barris3$property_personal.p.wo)$"conf.int"[1], t.test(barris3$property_personal.p.wo)$"conf.int"[2], 2),
  c("Property (personal)", "Victim residence", mean(barris3$property_personal.r.wo), t.test(barris3$property_personal.r.wo)$"conf.int"[1], t.test(barris3$property_personal.r.wo)$"conf.int"[2], 3),
  c("Property (personal)", "Victim residence (report)", mean(barris3$property_personal.rp.wo), t.test(barris3$property_personal.rp.wo)$"conf.int"[1], t.test(barris3$property_personal.rp.wo)$"conf.int"[2], 4),
  c("Property (household/vehicle)", "Offence location", mean(barris3$property_vh.f.wo), t.test(barris3$property_vh.f.wo)$"conf.int"[1], t.test(barris3$property_vh.f.wo)$"conf.int"[2], 1),
  c("Property (household/vehicle)", "Offence location (report)", mean(barris3$property_vh.p.wo), t.test(barris3$property_vh.p.wo)$"conf.int"[1], t.test(barris3$property_vh.p.wo)$"conf.int"[2], 2),
  c("Property (household/vehicle)", "Victim residence", mean(barris3$property_vh.r.wo), t.test(barris3$property_vh.r.wo)$"conf.int"[1], t.test(barris3$property_vh.r.wo)$"conf.int"[2], 3),
  c("Property (household/vehicle)", "Victim residence (report)", mean(barris3$property_vh.rp.wo), t.test(barris3$property_vh.rp.wo)$"conf.int"[1], t.test(barris3$property_vh.rp.wo)$"conf.int"[2], 4)
) 

confint_df <- confint_df %>%
  as_tibble() %>%
  rename(trait = 'V1',
         method = 'V2',
         mean = 'V3',
         LI = 'V4',
         UI = 'V5',
         num = 'V6') %>%
  mutate(mean = as.numeric(mean),
         LI = as.numeric(LI),
         UI = as.numeric(UI),
         num = as.numeric(num))

confint_yr_df <- rbind(
  c("Violence", "15/16", mean(barris2$violence.f.wo[barris2$Any == "2015-16"] +
                              barris2$violence.p.wo[barris2$Any == "2015-16"] +
                              barris2$violence.r.wo[barris2$Any == "2015-16"] +
                              barris2$violence.rp.wo[barris2$Any == "2015-16"]),
    t.test(barris2$violence.f.wo[barris2$Any == "2015-16"] +
             barris2$violence.p.wo[barris2$Any == "2015-16"] +
             barris2$violence.r.wo[barris2$Any == "2015-16"] +
             barris2$violence.rp.wo[barris2$Any == "2015-16"])$"conf.int"[1],
    t.test(barris2$violence.f.wo[barris2$Any == "2015-16"] +
             barris2$violence.p.wo[barris2$Any == "2015-16"] +
             barris2$violence.r.wo[barris2$Any == "2015-16"] +
             barris2$violence.rp.wo[barris2$Any == "2015-16"])$"conf.int"[2],
    1),
  c("Violence", "17/18", mean(barris2$violence.f.wo[barris2$Any == "2017-18"] +
                                barris2$violence.p.wo[barris2$Any == "2017-18"] +
                                barris2$violence.r.wo[barris2$Any == "2017-18"] +
                                barris2$violence.rp.wo[barris2$Any == "2017-18"]),
    t.test(barris2$violence.f.wo[barris2$Any == "2017-18"] +
             barris2$violence.p.wo[barris2$Any == "2017-18"] +
             barris2$violence.r.wo[barris2$Any == "2017-18"] +
             barris2$violence.rp.wo[barris2$Any == "2017-18"])$"conf.int"[1],
    t.test(barris2$violence.f.wo[barris2$Any == "2017-18"] +
             barris2$violence.p.wo[barris2$Any == "2017-18"] +
             barris2$violence.r.wo[barris2$Any == "2017-18"] +
             barris2$violence.rp.wo[barris2$Any == "2017-18"])$"conf.int"[2],
    2),
  c("Violence", "19/20", mean(barris2$violence.f.wo[barris2$Any == "2019-20"] +
                                barris2$violence.p.wo[barris2$Any == "2019-20"] +
                                barris2$violence.r.wo[barris2$Any == "2019-20"] +
                                barris2$violence.rp.wo[barris2$Any == "2019-20"]),
    t.test(barris2$violence.f.wo[barris2$Any == "2019-20"] +
             barris2$violence.p.wo[barris2$Any == "2019-20"] +
             barris2$violence.r.wo[barris2$Any == "2019-20"] +
             barris2$violence.rp.wo[barris2$Any == "2019-20"])$"conf.int"[1],
    t.test(barris2$violence.f.wo[barris2$Any == "2019-20"] +
             barris2$violence.p.wo[barris2$Any == "2019-20"] +
             barris2$violence.r.wo[barris2$Any == "2019-20"] +
             barris2$violence.rp.wo[barris2$Any == "2019-20"])$"conf.int"[2],
    3),
  c("Property (personal)", "15/16", mean(barris2$property_personal.f.wo[barris2$Any == "2015-16"] +
                                         barris2$property_personal.p.wo[barris2$Any == "2015-16"] +
                                         barris2$property_personal.r.wo[barris2$Any == "2015-16"] +
                                         barris2$property_personal.rp.wo[barris2$Any == "2015-16"]),
    t.test(barris2$property_personal.f.wo[barris2$Any == "2015-16"] +
             barris2$property_personal.p.wo[barris2$Any == "2015-16"] +
             barris2$property_personal.r.wo[barris2$Any == "2015-16"] +
             barris2$property_personal.rp.wo[barris2$Any == "2015-16"])$"conf.int"[1],
    t.test(barris2$property_personal.f.wo[barris2$Any == "2015-16"] +
             barris2$property_personal.p.wo[barris2$Any == "2015-16"] +
             barris2$property_personal.r.wo[barris2$Any == "2015-16"] +
             barris2$property_personal.rp.wo[barris2$Any == "2015-16"])$"conf.int"[2],
    1),
  c("Property (personal)", "17/18", mean(barris2$property_personal.f.wo[barris2$Any == "2017-18"] +
                                         barris2$property_personal.p.wo[barris2$Any == "2017-18"] +
                                         barris2$property_personal.r.wo[barris2$Any == "2017-18"] +
                                         barris2$property_personal.rp.wo[barris2$Any == "2017-18"]),
    t.test(barris2$property_personal.f.wo[barris2$Any == "2017-18"] +
             barris2$property_personal.p.wo[barris2$Any == "2017-18"] +
             barris2$property_personal.r.wo[barris2$Any == "2017-18"] +
             barris2$property_personal.rp.wo[barris2$Any == "2017-18"])$"conf.int"[1],
    t.test(barris2$property_personal.f.wo[barris2$Any == "2017-18"] +
             barris2$property_personal.p.wo[barris2$Any == "2017-18"] +
             barris2$property_personal.r.wo[barris2$Any == "2017-18"] +
             barris2$property_personal.rp.wo[barris2$Any == "2017-18"])$"conf.int"[2],
    2),
  c("Property (personal)", "19/20", mean(barris2$property_personal.f.wo[barris2$Any == "2019-20"] +
                                         barris2$property_personal.p.wo[barris2$Any == "2019-20"] +
                                         barris2$property_personal.r.wo[barris2$Any == "2019-20"] +
                                         barris2$property_personal.rp.wo[barris2$Any == "2019-20"]),
    t.test(barris2$property_personal.f.wo[barris2$Any == "2019-20"] +
             barris2$property_personal.p.wo[barris2$Any == "2019-20"] +
             barris2$property_personal.r.wo[barris2$Any == "2019-20"] +
             barris2$property_personal.rp.wo[barris2$Any == "2019-20"])$"conf.int"[1],
    t.test(barris2$property_personal.f.wo[barris2$Any == "2019-20"] +
             barris2$property_personal.p.wo[barris2$Any == "2019-20"] +
             barris2$property_personal.r.wo[barris2$Any == "2019-20"] +
             barris2$property_personal.rp.wo[barris2$Any == "2019-20"])$"conf.int"[2],
    3),
  c("Property (household/vehicle)", "15/16", mean(barris2$property_vh.f.wo[barris2$Any == "2015-16"] +
                                   barris2$property_vh.p.wo[barris2$Any == "2015-16"] +
                                   barris2$property_vh.r.wo[barris2$Any == "2015-16"] +
                                   barris2$property_vh.rp.wo[barris2$Any == "2015-16"]),
    t.test(barris2$property_vh.f.wo[barris2$Any == "2015-16"] +
             barris2$property_vh.p.wo[barris2$Any == "2015-16"] +
             barris2$property_vh.r.wo[barris2$Any == "2015-16"] +
             barris2$property_vh.rp.wo[barris2$Any == "2015-16"])$"conf.int"[1],
    t.test(barris2$property_vh.f.wo[barris2$Any == "2015-16"] +
             barris2$property_vh.p.wo[barris2$Any == "2015-16"] +
             barris2$property_vh.r.wo[barris2$Any == "2015-16"] +
             barris2$property_vh.rp.wo[barris2$Any == "2015-16"])$"conf.int"[2],
    1),
  c("Property (household/vehicle)", "17/18", mean(barris2$property_vh.f.wo[barris2$Any == "2017-18"] +
                                   barris2$property_vh.p.wo[barris2$Any == "2017-18"] +
                                   barris2$property_vh.r.wo[barris2$Any == "2017-18"] +
                                   barris2$property_vh.rp.wo[barris2$Any == "2017-18"],),
    t.test(barris2$property_vh.f.wo[barris2$Any == "2017-18"] +
             barris2$property_vh.p.wo[barris2$Any == "2017-18"] +
             barris2$property_vh.r.wo[barris2$Any == "2017-18"] +
             barris2$property_vh.rp.wo[barris2$Any == "2017-18"])$"conf.int"[1],
    t.test(barris2$property_vh.f.wo[barris2$Any == "2017-18"] +
             barris2$property_vh.p.wo[barris2$Any == "2017-18"] +
             barris2$property_vh.r.wo[barris2$Any == "2017-18"] +
             barris2$property_vh.rp.wo[barris2$Any == "2017-18"])$"conf.int"[2],
    2),
  c("Property (household/vehicle)", "19/20", mean(barris2$property_vh.f.wo[barris2$Any == "2019-20"] +
                                   barris2$property_vh.p.wo[barris2$Any == "2019-20"] +
                                   barris2$property_vh.r.wo[barris2$Any == "2019-20"] +
                                   barris2$property_vh.rp.wo[barris2$Any == "2019-20"]),
    t.test(barris2$property_vh.f.wo[barris2$Any == "2019-20"] +
             barris2$property_vh.p.wo[barris2$Any == "2019-20"] +
             barris2$property_vh.r.wo[barris2$Any == "2019-20"] +
             barris2$property_vh.rp.wo[barris2$Any == "2019-20"])$"conf.int"[1],
    t.test(barris2$property_vh.f.wo[barris2$Any == "2019-20"] +
             barris2$property_vh.p.wo[barris2$Any == "2019-20"] +
             barris2$property_vh.r.wo[barris2$Any == "2019-20"] +
             barris2$property_vh.rp.wo[barris2$Any == "2019-20"])$"conf.int"[2],
    3)
  )
  
confint_yr_df <- confint_yr_df %>%
  as_tibble() %>%
  rename(trait = 'V1',
         year = 'V2',
         mean = 'V3',
         LI = 'V4',
         UI = 'V5',
         num = 'V6') %>%
  mutate(mean = as.numeric(mean),
         LI = as.numeric(LI),
         UI = as.numeric(UI),
         num = as.numeric(num))

p.method <- ggplot(confint_df, aes(y = reorder(method, -num),
                       x = mean, xmin = LI, xmax = UI)) +
  facet_grid(trait ~ .) +
  xlim(0, 106) +
  geom_pointrange() +
  xlab('') +
  ylab('') +
  theme_bw()

p.year <- ggplot(confint_yr_df, aes(y = reorder(year, -num),
                       x = mean, xmin = LI, xmax = UI)) +
  facet_grid(trait ~ .) +
  xlim(0, 82) +
  geom_pointrange() +
  xlab('') +
  ylab('') +
  theme_bw()

ggarrange(p.method, p.year,
          ncol = 2,
          widths = c(1.1, 0.9)) 
