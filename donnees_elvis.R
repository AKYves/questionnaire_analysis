# set the working directory to the source file location.
# loading the required packages
# disclaimer: all my functions are named with a dot

#data manipulation
library(dplyr)
library(tibble)
library(tidyr)
#functions
library(purrr)
#visualisation
library(ggplot2)
library(scales)
#pipes
library(magrittr)
#output
library(openxlsx)
library(googledrive)
library(googlesheets4)

source("functions_elvis.R")

# loading the database
#dt <- read.xlsx("enquete_elvis_tic.xlsx")
dt <- drive_get("enquete_elvis_tic") %>% sheets_read()

  #cleaning the dataset.
colnames(dt) <- c("hur", "mail", "scr", "thm", "oth_thm",
                  "wrks", "sex", "age", "cntry", "whats")


#this is to keep tracking of new data comming in
#nrow(mpl_dt)
#31

#-- dealing with the double observations.
mpl_dt_tr <- mpl_dt %>%
  mutate(nb_nas = apply(mpl_dt, 1, nb.nas)) %>%
  #filtering out non answers // all nas on the line, first 3 columns are
  # filled
  filt(nb_nas < (ncol(dt) - 3)) %>%
  #dealing with others
  mutate(thm = asc.low(thm)) %>%
  mutate(oth_thm = oth.fct(oth_thm)) %>%
  #removing those unwanted variables
  selt(-c(hur, nb_nas)) %>%
  #At this step I have distinct multiples, cleaned from non answers
  distinct() %>%
  #keep those with long thm number of characters
  rmv.small(mail, thm) %>%
  #filter again those with others_thm number of characters
  rmv.small(mail, oth_thm)

# Now I think I will keep it like that and manage (and split the clean part)
new_dt <- sgl_dt %>%
  selt(-hur) %>%
  mutate(thm = asc.low(thm), oth_thm = oth.fct(oth_thm)) %>%
  bind_rows(mpl_dt_tr)

# applying the dicotomization on the dataframe of single observations/
new_dt <- new_dt %>%
  bind_cols(dthm.dt(new_dt, thm))

# cleaning the works.
new_dt <- new_dt %>%
  mutate(wrks = asc.low(wrks, regex = "\\d"))

# we can take a look at unique values now:
unique(new_dt$wrks)
sci_words <- c("scientifique", "para medical", "engenierie",
               "medecine", "bioethique", "technologie", "recherche", "enseignant")
com_words <- c("communication", "communication/marketing/journalisme")
art_words <- c("artisanat", "btp", "architecture immobilier", "stylisme",
                "transport")
adm_words <- c("technique adminitrative", "diplomatie", "securite publique")
eco_words <- c("commerce/gestion/economie", "audit et comptabilite")
lit_words <-c("litteraire/droit/langue", "droit et langue")
oth_words <- c("eleve", "etudiant")

#just a simple test to be sure everything is ok.
tst_vc <-  grep("words$", ls(), value = TRUE) %>% lapply(function(x) get(x))

#This should return TRUE
length(unlist(tst_vc)) == (length(unique(new_dt$wrks)) - 1)

values <- c("administration",  "artisanat et immobilier",
            "communication et marketing",  "commerce, economie et comptabilite",
            "litterature et droit", "autres", "scientifique")

values <- map2(values, tst_vc, rep.lgth) %>% unlist()

# Now it is time to recode everything.
cod_dt <- data.frame(wrks = unlist(tst_vc), wrks_rec = values, stringsAsFactors = F)
new_dt <- new_dt %>% left_join(cod_dt, by = "wrks")

# I will begin the analysis by precising the number of unique entries.

n <- nrow(new_dt) # respondents

# graphing the themes:
main_thm <- comp.thmprop(new_dt, thm1:thm14)



p_thm_perc

# On here is the table
main_thm %>%
  mutate(perc = 100 * round(perc, 5)) %>%
  arrange(desc(perc)) %>%
  set_colnames(c("Thèmes", paste("%de choix (n=", n, ")", sep = ""))) %>%
  knitr::kable()

#preferences for themes:

# Now computing scoring for each themes (preferences)
pref_thm <- comp.pref(new_dt, thm1:thm14)

nthmlbl <- paste(themes_names[order(-pref_thm$wgt)], collapse = "\n")

ggplot(pref_thm, aes(x = reorder(themes, -wgt))) +
  geom_segment(aes(yend = wgt, xend = reorder(themes, -wgt), y=0),
               colour = "blue", size = 1) +
  geom_point(aes(y = wgt), colour = "blue", size = 3)+
  labs(x = "Différents Thèmes", y = "Préférences des personnes enquêtées")+
  scale_y_continuous(breaks = seq(0, 30, by = 1))+
  theme_gray() +
  annotate("label", x = 9, y= 20, label = nthmlbl, hjust = 0,
           colour = "blue", size = 3.5)

knitr::kable(pref_thm %>% arrange(desc(wgt)))

# structure des personnes enquetees:
age_table <- new_dt %>%
  count(sex, age) %>%
  filt(!is.na(sex)) %>%
  spread(key = sex, value = n)
# 2 individus n'ont pas renseignes leur sexe

knitr::kable(age_table)
#

# I will now focus on sex and themes choosen/ and weight of themes.

manm_thm <- new_dt %>%
  filt(sex == "Homme") %>%
  comp.thmprop(thm1:thm14) %>%
  mutate(sex = "Femme")

femm_thm <- new_dt %>%
  filt(sex == "Femme") %>%
  comp.thmprop(thm1:thm14) %>%
  mutate(sex = "Homme")

sex_thm <- bind_cols(manm_thm, femm_thm)
