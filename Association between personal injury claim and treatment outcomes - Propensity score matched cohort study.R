# Association between personal injury claim and outcomes after treatment for hand or wrist disorders
##########################################################################################
# Code written by L. Hoogendam for the Hand Wrist Study Group, 
# Erasmus Medical Center, Rotterdam
##########################################################################################
# 
# This script prepares data for analysis and performs the analyses. 
# Note: this script is specific to our data. For this script to be used on different data set some 
# adaptation will be needed.

rm(list = ls()) # Clear working directory

# Loading data
setwd("/Volumes/T7/Research_exports/20210906")
load("Pols_lang20210906.Rdata")
load("Pols_kort20210906.Rdata")
load("Duim_kort20210906.Rdata")
load("Duim_lang20210906.Rdata")
load("Vinger_kort20210906.Rdata")
load("Vinger_lang20210906.Rdata")
load("Dupuytren20210906.Rdata")
load("Zenuwdecompressie20210906.Rdata")

# Loading packages and functions
library(tidyverse)
library(openxlsx)
library(pwr)
library(MatchIt)
library(survival)
library(survminer)
select <- dplyr::select

source("~/Documents/GitHub/PulseR/Selection/Functies_Lisa.R")
RTW_bewerking <- function(DF){
  #DF = RTW dataframe
  for(i in 1:nrow(DF)){
    if(is.na(DF$werkzaam[i])){
      DF$werkzaam[i] <- "Nee, als gevolg van de hand/polsklacht waarvoor u wordt behandeld"
    }
  }
  
  
  DF_short <- DF %>% 
    select(c(Patient.traject.ID,
             werkzaam,
             aangepastWerk, 
             urenNormaal, 
             urenNu, 
             normaalWerk, 
             rounddescription)) %>%
    filter(urenNormaal > 0) %>%
    filter(urenNormaal < 80) %>%
    filter(werkzaam != "Nee, als gevolg van iets anders") %>%
    select(-werkzaam)
  
  DF_short <- DF_short %>% 
    mutate(Percentage_werk = urenNu/urenNormaal)
  DF_short <- DF_short %>% 
    mutate(Event = ifelse(is.na(aangepastWerk),0,
                          ifelse(aangepastWerk == "Oorspronkelijke werkzaamheden",
                                 ifelse(Percentage_werk > 0.5, 1,0), 0))) %>%
    mutate(EventTime = normaalWerk)
  
  for(i in 1:nrow(DF_short)){
    if(is.na(DF_short$EventTime[i])){
      if(DF_short$rounddescription[i] == "6 weken"){
        DF_short$EventTime[i] <- 6
      }
      if(DF_short$rounddescription[i] == "3 maanden"){
        DF_short$EventTime[i] <- 13
      }
      if(DF_short$rounddescription[i] == "6 maanden"){
        DF_short$EventTime[i] <- 26
      }
      if(DF_short$rounddescription[i] == "12 maanden"){
        DF_short$EventTime[i] <- 52
      }
    }
  }
  
  DF_wide_6 <- DF_short %>% 
    filter(rounddescription == "6 weken") %>% 
    select(-rounddescription)
  
  DF_wide_6_w <- DF_wide_6 %>% 
    filter(Event == 1)
  DF_wide_6_gw <- DF_wide_6 %>% 
    filter(Event == 0)
  
  
  DF_wide_13 <- DF_short %>% 
    filter(rounddescription == "3 maanden") %>% 
    select(-rounddescription) %>%
    anti_join(DF_wide_6_w, by = "Patient.traject.ID")
  
  DF_wide_13_w <- DF_wide_13 %>%
    filter(Event == 1)
  DF_wide_13_gw <- DF_wide_13 %>% 
    filter(Event == 0)
  
  
  DF_wide_26 <- DF_short %>% 
    filter(rounddescription == "6 maanden") %>% 
    select(-rounddescription) %>%
    anti_join(DF_wide_6_w, by = "Patient.traject.ID") %>%
    anti_join(DF_wide_13_w, by = "Patient.traject.ID")
  
  DF_wide_26_w <- DF_wide_26 %>%
    filter(Event == 1)
  DF_wide_26_gw <- DF_wide_26 %>% 
    filter(Event == 0)
  
  
  DF_wide_52 <- DF_short %>% 
    filter(rounddescription == "12 maanden") %>% 
    select(-rounddescription) %>%
    anti_join(DF_wide_6_w, by = "Patient.traject.ID") %>%
    anti_join(DF_wide_13_w, by = "Patient.traject.ID") %>% 
    anti_join(DF_wide_26_w, by = "Patient.traject.ID")
  
  DF_wide_52_w <- DF_wide_52 %>%
    filter(Event == 1)
  DF_wide_52_gw <- DF_wide_52 %>% 
    filter(Event == 0)
  
  
  DF_wide_gw <- DF_wide_52_gw
  
  PtID <- DF_wide_gw %>% 
    select(`Patient.traject.ID`)
  DF_wide_26_gw <- anti_join(DF_wide_26_gw, PtID) %>%
    anti_join(DF_wide_52_w, by = "Patient.traject.ID")
  DF_wide_gw <- rbind(DF_wide_gw, DF_wide_26_gw)
  
  PtID <- DF_wide_gw %>% 
    select(`Patient.traject.ID`)
  DF_wide_13_gw <- anti_join(DF_wide_13_gw, PtID) %>%
    anti_join(DF_wide_52_w, by = "Patient.traject.ID") %>%
    anti_join(DF_wide_26_w, by = "Patient.traject.ID")
  DF_wide_gw <- rbind(DF_wide_gw, DF_wide_13_gw)
  
  PtID <- DF_wide_gw %>% 
    select(`Patient.traject.ID`)
  DF_wide_6_gw <- anti_join(DF_wide_6_gw, PtID) %>%
    anti_join(DF_wide_52_w, by = "Patient.traject.ID") %>%
    anti_join(DF_wide_26_w, by = "Patient.traject.ID") %>%
    anti_join(DF_wide_13_w, by = "Patient.traject.ID")
  DF_wide_gw <- rbind(DF_wide_gw, DF_wide_6_gw)
  
  DF_new <- rbind(DF_wide_6_w, DF_wide_13_w, DF_wide_26_w, DF_wide_52_w, DF_wide_gw)
  DF_new_a <- DF_new %>% filter(EventTime <= 52) #remove missing
  DF_new_a$Event[is.na(DF_new_a$Event)] <- 0
  
  rm(DF_wide_13, DF_wide_13_gw, DF_wide_13_w, DF_wide_26, DF_wide_26_gw, DF_wide_26_w, DF_wide_6, DF_wide_6_gw, DF_wide_6_w, DF_wide_52, DF_wide_52_gw, DF_wide_52_w, DF_wide_gw)
  return(DF_new_a)
}

# Combining data per track into one dataset
# Treatment date
behandeldatum_totaal <- plyr::rbind.fill(Pols_kort$Behandeldatum, 
                                         Pols_lang$Behandeldatum, 
                                         Duim_kort$Behandeldatum, 
                                         Duim_lang$Behandeldatum, 
                                         Vinger_kort$Behandeldatum, 
                                         Vinger_lang$Behandeldatum, 
                                         Zenuwdecompressie$Behandeldatum, 
                                         Dupuytren$Behandeldatum)

# Information on personal injury claim
Intake_totaal <- plyr::rbind.fill(Pols_kort$Intake_oud, 
                                 Pols_lang$Intake_oud, 
                                 Duim_kort$Intake_oud, 
                                 Duim_lang$Intake_oud, 
                                 Vinger_kort$Intake_oud, 
                                 Vinger_lang$Intake_oud, 
                                 Zenuwdecompressie$Intake_oud, 
                                 Dupuytren$Intake_oud)

# Visual Analog Scales
VAS_totaal <- plyr::rbind.fill(Pols_kort$VAS, 
                               Pols_lang$VAS, 
                               Duim_kort$VAS, 
                               Duim_lang$VAS, 
                               Vinger_kort$VAS, 
                               Vinger_lang$VAS, 
                               Zenuwdecompressie$VAS, 
                               Dupuytren$VAS) %>%
  select(Patient.traject.ID, Respondent.ID, rounddescription, Invuldatum, vasPijnGemiddeld_1:vasTevredenheid_1) %>%
  na.omit() # Remove missings within questionnaire
sapply(VAS_totaal, function(x) sum(is.na(x)))

# Return to work
RTW_totaal <- plyr::rbind.fill(Pols_kort$RTW, 
                               Pols_lang$RTW, 
                               Duim_kort$RTW, 
                               Duim_lang$RTW, 
                               Vinger_kort$RTW, 
                               Vinger_lang$RTW, 
                               Zenuwdecompressie$RTW, 
                               Dupuytren$RTW)

# Include patients with measurement track, known treatment date, and no missings in patient demographics
# Add type of treatment (nonsurgical, minor surgery, major surgery)
InV_totaal <- plyr::rbind.fill(Pols_kort$InV_Pols_kort %>%
                                 mutate(Track = "Pols_kort"), Pols_lang$InV_Pols_lang %>%
                                 mutate(Track = "Pols_lang"), Duim_kort$InV_Duim_kort %>%
                                 mutate(Track = "Duim_kort"), Duim_lang$InV_Duim_lang %>%
                                 mutate(Track = "Duim_lang"), Vinger_kort$InV_Vinger_kort %>%
                                 mutate(Track = "Vinger_kort"), Vinger_lang$InV_Vinger_lang %>%
                                 mutate(Track = "Vinger_lang"), Zenuwdecompressie$InV_Zenuwdecompressie %>%
                                 mutate(Track = "Zenuwdecompressie"), Dupuytren$InV_Dupuytren%>%
                                 mutate(Track = "Dupuytren")) %>%
  mutate(Lang_kort = case_when(
    grepl("lang", Track)==TRUE | Track == "Dupuytren" ~ "Lang", 
    grepl("kort", Track)==TRUE | Track == "Zenuwdecompressie" ~ "Kort"
  )) %>%
  mutate(Kort_cons_chi = case_when(
    Lang_kort == "Kort" & grepl("conservatief", behandeling, ignore.case = TRUE)==TRUE ~ "Kort_cons",
    Lang_kort == "Kort" & grepl("conservatief", behandeling, ignore.case = TRUE)==FALSE ~ "Kort_chi"
  )) %>%
  mutate(Type_treatment = case_when(
    Lang_kort == "Lang" & behandeling != "Collagenase" & grepl("Naaldfasciotomie", behandeling)==FALSE & grepl("conservatief", behandeling)==FALSE ~ "Major surgery",
    (Lang_kort == "Kort" & Kort_cons_chi == "Kort_chi") | grepl("Naaldfasciotomie", behandeling)==TRUE  ~ "Minor surgery",
    (Lang_kort == "Kort" & Kort_cons_chi == "Kort_cons") | behandeling == "Collagenase" | grepl("conservatief", behandeling)==TRUE ~ "Conservative treatment"
  )) %>%
  mutate(Cons_chi = case_when(
    Type_treatment == "Major surgery" | Type_treatment == "Minor surgery" ~ "Chirurgie",
    Type_treatment == "Conservative treatment" ~ "Conservatief"
  )) %>% 
  mutate(Beroep = case_when(
    zwaarteBeroep == "Geen betaalde arbeid (o.a. uitkering gepensioneerd: return to work vragenlijst vervalt)"  | zwaarteBeroep == "Geen betaalde arbeid (o.a. uitkering, gepensioneerd: return to work vragenlijst vervalt)" ~ "Geen betaalde arbeid", 
    zwaarteBeroep == "Licht fysieke arbeid (bijv. kantoorwerk)" | zwaarteBeroep == "Licht fysiek arbeid (bijv. kantoorwerk)" ~ "Licht fysieke arbeid", 
    zwaarteBeroep == "Matig fysieke arbeid (bijv. werken in een winkel)" ~ "Matig fysieke arbeid", 
    zwaarteBeroep == "Zwaar fysieke arbeid (bijv. in de bouw, stratenmaker)" ~ "Zwaar fysieke arbeid"
  )) %>%
  inner_join(behandeldatum_totaal, by=c("Patient.traject.ID", "Respondent.ID")) %>%
  filter(!is.na(dominant) & !is.na(Leeftijd) & !is.na(Geslacht) & !is.na(behandeling)) %>%
  remove_duplicates_2(grouping_col = "Patient.traject.ID")
InV_totaal$behandeling <- tolower(InV_totaal$behandeling)
InV_totaal$behandelingother <- tolower(InV_totaal$behandelingother)
InV_totaal$Beroep <- factor(InV_totaal$Beroep, c("Geen betaalde arbeid", "Licht fysieke arbeid", "Matig fysieke arbeid", "Zwaar fysieke arbeid"))
InV_totaal$Geslacht <- factor(InV_totaal$Geslacht)
length(unique(InV_totaal$Patient.traject.ID))
sapply(InV_totaal, function(x) sum(is.na(x)))

# Include patients with known treatment and for whom involvement in a personal injury claim is known  
Letselschade <- inner_join(InV_totaal, Intake_totaal, by=c("Patient.traject.ID", "Respondent.ID"), suffix = c("_InV", "_Intake")) %>%
  select(Patient.traject.ID, Respondent.ID, letselschade, behandeling, Track, Lang_kort, Kort_cons_chi, Type_treatment) %>%
  filter(behandeling != "overige" & behandeling != "nog niet bekend") %>%
  filter(!is.na(letselschade))
length(unique(Letselschade$Patient.traject.ID)) 

# Include treatments that occur at least 20 times in the dataset
Letselschade_aandoeningen <- Letselschade %>%
  group_by(behandeling) %>%
  dplyr::summarise(n = n()) %>%
  filter(n > 20) 

# Final dataset -----
Letselschade_aantallen <- Letselschade %>%
  filter(behandeling %in% Letselschade_aandoeningen$behandeling)
Letselschade_aantallen$behandeling <- factor(Letselschade_aantallen$behandeling)
sapply(Letselschade_aantallen, function(x) sum(is.na(x)))
length(unique(Letselschade_aantallen$Patient.traject.ID))

# Overview of type of treatment, track, and involvement in a personal injury claim
table(Letselschade_aantallen$Lang_kort)
table(Letselschade_aantallen$Kort_cons_chi)
table(Letselschade_aantallen$Type_treatment)
table(Letselschade_aantallen$letselschade)
816/nrow(Letselschade_aantallen)*100  #2.34% of all included patients is involved in a personal injury claim
overview_treatment_classification <- as.data.frame(table(Letselschade_aantallen$Type_treatment, Letselschade_aantallen$behandeling)) %>% 
  filter(Freq != 0)

Traject_aantallen <- Letselschade_aantallen %>%
  group_by(Track) %>%
  dplyr::summarise(n = n()) 

Letselschade_traject <- Letselschade_aantallen %>%
  group_by(Track, letselschade) %>%
  dplyr::summarise(n = n()) %>%
  inner_join(Traject_aantallen, by="Track", suffix=c("", "_totaal")) %>%
  mutate(Percentage = round(n/n_totaal*100, 2)) %>%
  filter(letselschade == "Ja")

# Involvement in a personal injury claim per treatment------
Letselschade_aantallen_per_behandeling <- Letselschade_aantallen %>%
  group_by(behandeling, letselschade, Track) %>%
  dplyr::summarise(n = n()) %>%
  inner_join(Letselschade_aandoeningen, by="behandeling", suffix=c("", "_totaal")) %>%
  mutate(Percentage = round(n/n_totaal*100, 2)) %>%
  filter(letselschade == "Ja")

# Flowchart----
nrow(Letselschade)
length(unique(Letselschade$Patient.traject.ID))
nrow(Letselschade_aantallen)
length(unique(Letselschade_aantallen$Patient.traject.ID))
nrow(Letselschade)- nrow(Letselschade_aantallen)
Letselschade_aantallen_flowchart <- inner_join(Letselschade_aantallen, VAS_totaal %>%
                                                 filter(rounddescription == "Intake") %>% 
                                                 group_by(Patient.traject.ID, Respondent.ID) %>% 
                                                 slice(1) %>% 
                                                 ungroup(), by=c("Patient.traject.ID", "Respondent.ID")) %>%
 inner_join(InV_totaal, by=c("Patient.traject.ID", "Respondent.ID")) 
nrow(Letselschade_aantallen_flowchart)
length(unique(Letselschade_aantallen_flowchart$Patient.traject.ID))
nrow(Letselschade_aantallen) - nrow(Letselschade_aantallen_flowchart)
table(Letselschade_aantallen_flowchart$Type_treatment.x, useNA = "ifany")

# Table 1-Patient and treatment characteristics
names(Letselschade_aantallen_flowchart)
vars <- c("Leeftijd", "Geslacht", "hoeLangKlacht", "Beroep", "dominant",  "Type_treatment.x",
          "vasPijnBelasten_1","vasFunctie_1", "behandeling.x")
table1_totaal <- tableone::CreateTableOne(vars = vars, data = Letselschade_aantallen_flowchart,
                                   factorVars = c("Geslacht", "Beroep", "dominant"),
                                   strata = "letselschade", 
                                   smd = TRUE, 
                                   includeNA = TRUE)
x <- as.data.frame(print(table1_totaal, smd = TRUE, nonnormal = "hoeLangKlacht"))

SMD_vars <- table1_totaal$MetaData$vars
SMD_totale_set <- x$SMD[x$SMD != ""]

# VAS data
VAS_Intake <- VAS_totaal %>% 
  filter(rounddescription == "Intake") %>% 
  group_by(Patient.traject.ID, Respondent.ID) %>% 
  slice(1) %>% 
  ungroup()
nrow(VAS_Intake) == length(unique(VAS_Intake$Patient.traject.ID))
VAS_3_maanden <- VAS_totaal %>% 
  filter(rounddescription == "3 maanden") %>% 
  group_by(Patient.traject.ID, Respondent.ID) %>% 
  slice(1) %>% 
  ungroup()
nrow(VAS_3_maanden) == length(unique(VAS_3_maanden$Patient.traject.ID))
VAS_12_maanden <- VAS_totaal %>% 
  filter(rounddescription == "12 maanden") %>% 
  group_by(Patient.traject.ID, Respondent.ID) %>% 
  slice(1) %>% 
  ungroup()
nrow(VAS_12_maanden) == length(unique(VAS_12_maanden$Patient.traject.ID))

# Creating separate datasets for each subcohort-----
# Nonsurgical treatment dataset------
Kort_cons <- Letselschade_aantallen %>%
  filter(Type_treatment == "Conservative treatment") %>%
  inner_join(VAS_Intake, by=c("Patient.traject.ID", "Respondent.ID")) %>%
  inner_join(VAS_3_maanden, by=c("Patient.traject.ID", "Respondent.ID"), suffix=c("_Int", "_3m")) %>% 
  inner_join(InV_totaal, by=c("Patient.traject.ID", "Respondent.ID")) 
nrow(Kort_cons)
nrow(Kort_cons) == length(unique(Kort_cons$Patient.traject.ID))
table(Kort_cons$letselschade)

RTW_kort_cons <- Letselschade_aantallen %>%
  filter(Type_treatment == "Conservative treatment") %>%
  select(Patient.traject.ID, Respondent.ID, behandeling, Track) %>%
  inner_join(RTW_totaal) %>%
  RTW_bewerking()
length(unique(RTW_kort_cons$Patient.traject.ID))

# Minor surgery dataset----------
Kort_chi <- Letselschade_aantallen %>%
  filter(Type_treatment == "Minor surgery") %>%
  inner_join(VAS_Intake, by=c("Patient.traject.ID", "Respondent.ID")) %>%
  inner_join(VAS_3_maanden, by=c("Patient.traject.ID", "Respondent.ID"), suffix=c("_Int", "_3m")) %>%   
  inner_join(InV_totaal, by=c("Patient.traject.ID", "Respondent.ID")) 
nrow(Kort_chi)
nrow(Kort_chi)==length(unique(Kort_chi$Patient.traject.ID))
table(Kort_chi$letselschade)

RTW_kort_chi <- Letselschade_aantallen %>%
  filter(Type_treatment == "Minor surgery") %>%
  select(Patient.traject.ID, Respondent.ID, behandeling, Track) %>%
  inner_join(RTW_totaal) %>%
  RTW_bewerking()
length(unique(RTW_kort_chi$Patient.traject.ID))

# Major surgery dataset------
Lang <- Letselschade_aantallen %>%
  filter(Type_treatment == "Major surgery") %>%
  inner_join(VAS_Intake, by=c("Patient.traject.ID", "Respondent.ID")) %>%
  inner_join(VAS_12_maanden, by=c("Patient.traject.ID", "Respondent.ID"), suffix=c("_Int", "_12m")) %>%
  inner_join(InV_totaal, by=c("Patient.traject.ID", "Respondent.ID")) 
nrow(Lang)
nrow(Lang) ==length(unique(Lang$Patient.traject.ID))
sapply(Lang, function(x) sum(is.na(x)))
min(Lang$Invuldatum_Int)
max(Lang$Invuldatum_12m, na.rm=TRUE)
table(Lang$letselschade, useNA = "ifany")

RTW_lang <- Letselschade_aantallen_flowchart %>%
  filter(Type_treatment.x == "Major surgery") %>%
  select(Patient.traject.ID, Respondent.ID, behandeling.x, Track.x) %>%
  inner_join(RTW_totaal) %>%
  RTW_bewerking()
length(unique(RTW_lang$Patient.traject.ID))

# Power analysis-----
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2962254/ advises 1:1 or 2:1 propensity score matching (PSM)
# Calculating difference in power based on number of matches

# Based on the smallest number of personal injury claims for the VAS analyses (nonsurgical treatment, n=115), with 80% power and a significance level corrected for 3 tests per subcohort
pwr.t2n.test(n1=115, n2=115, power=0.8, sig.level = 0.05/3) #D=0.43
pwr.t2n.test(n1=115, n2=230, power=0.8, sig.level = 0.05/3) #D=0.37
# For each subcohort we have enough power for the analyses
# We can detect a smaller effect (D=0.37) using 2:1 PSM --> This will be used throughout the analyses

# Matching for VAS analyses - Nonsurgical treatment -------
# Preparing data
kort.cons.match <- Kort_cons %>%
  select(c(letselschade, Leeftijd, Geslacht, Beroep, zijde, dominant, hoeLangKlacht, behandeling.x,
           vasPijnBelasten_1_Int:vasFunctie_1_Int, 
           vasPijnBelasten_1_3m:vasFunctie_1_3m)) %>%
  na.omit() 
sapply(kort.cons.match, function(x) sum(is.na(x)))
kort.cons.match$letselschade <- factor(kort.cons.match$letselschade, c("Nee", "Ja"))
kort.cons.match$letselschade <- as.logical(as.numeric(kort.cons.match$letselschade)-1)
table(kort.cons.match$letselschade)

# Extract standardized mean difference prior to matching
table1 <- tableone::CreateTableOne(vars = c("Leeftijd", "Geslacht", "hoeLangKlacht", "Beroep", "dominant", "vasPijnBelasten_1_Int", "vasFunctie_1_Int", "behandeling.x"), data = kort.cons.match,
                                   factorVars = c("Geslacht", "Beroep", "dominant"),
                                   strata = "letselschade", 
                                   smd = TRUE)
VAS_kort_cons_voor_match <- as.data.frame(print(table1, smd = TRUE)) 

SMD_vars_VAS_kort_cons_voor_match <- table1$MetaData$vars
SMD_VAS_kort_cons_voor_match <- VAS_kort_cons_voor_match$SMD[VAS_kort_cons_voor_match$SMD != ""]

# Matching
set.seed(123)
matchited <- matchit(letselschade ~ Leeftijd + Geslacht + Beroep + dominant +
                        vasPijnBelasten_1_Int  + vasFunctie_1_Int  + 
                       behandeling.x,
                     data = kort.cons.match, method = "nearest", ratio = 2)

df.matched <- match.data(matchited)[1:ncol(kort.cons.match)]
table(df.matched$letselschade) # Evaluate whether all personal injury claim patients were matched

# Extract standardized mean difference after matching
table1 <- tableone::CreateTableOne(vars = c("Leeftijd", "Geslacht", "hoeLangKlacht", "Beroep", "dominant", 
                                            "vasPijnBelasten_1_Int", "vasFunctie_1_Int", "behandeling.x"), data = df.matched,
                                   factorVars = c("Geslacht", "Beroep", "dominant"),
                                   strata = "letselschade", 
                                   smd = TRUE)
VAS_kort_cons_na_match <- as.data.frame(print(table1, smd = TRUE))

SMD_vars_VAS_kort_cons_na_match <- table1$MetaData$vars
SMD_VAS_kort_cons_na_match <- VAS_kort_cons_na_match$SMD[VAS_kort_cons_na_match$SMD != ""]

Table_SMD <- data.frame(Var = SMD_vars_VAS_kort_cons_voor_match, SMD = as.numeric(SMD_VAS_kort_cons_voor_match), Type = "Voor matching")
Table_SMD_matched <- data.frame(Var = SMD_vars_VAS_kort_cons_na_match, SMD = as.numeric(SMD_VAS_kort_cons_na_match), Type = "Na matching")
Table_SMD_kort_cons_VAS <- rbind(Table_SMD, Table_SMD_matched)
Table_SMD_kort_cons_VAS$Type <- factor(Table_SMD_kort_cons_VAS$Type, c("Voor matching", "Na matching"))

# Figure of standardized mean difference prior to and after matching
Table_SMD_kort_cons_VAS$Var <- rep(c("Age", "Gender", "Duration of symptoms", "Type of work", "Dominant side", "VAS pain during load", "VAS hand function", "Treatment"))
Table_SMD_kort_cons_VAS$Type <- rep(c("Prior to matching", "After matching"), each = 8)
Table_SMD_kort_cons_VAS$Type <- factor(Table_SMD_kort_cons_VAS$Type, c("Prior to matching", "After matching"))
ggplot(Table_SMD_kort_cons_VAS, aes(x = SMD, y = Var, colour = Type)) +
  geom_vline(xintercept = 0.1, linetype = 2) +
  geom_point() +
  geom_point() +
  theme_classic() +
  xlab("Standardized Mean Difference") +
  ylab("") +
  labs(colour = "")
ggsave(paste("/Users/lisa/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/", 
             "Supplemental_figure_1-Matching resultaat VAS conservatief",
             ".tiff", sep = ""))

# Evaluate outcomes VAS scores over time - Nonsurgical treatment ------
fit <- lm(vasPijnBelasten_1_3m ~ letselschade + dominant + Geslacht + behandeling.x, df.matched)
summary(fit) # Pain during load
fit <- lm(vasFunctie_1_3m ~ letselschade + dominant + Geslacht + behandeling.x, df.matched)
summary(fit) # Hand function

# Evaluate outcomes VAS scores over time - Nonsurgical treatment ------
# Preparing data
originele_data <- Kort_cons %>%
  select(contains(vars), Patient.traject.ID, vasPijnBelasten_1_3m, vasFunctie_1_3m, letselschade) %>% 
  na.omit() 
rowid_cases <- rownames(matchited$match.matrix)
rowid_matches <- matchited$match.matrix %>% 
  cbind() %>% 
  as.vector()
test_cases <- originele_data[rowid_cases, ]
test_matches <- originele_data[rowid_matches, ]
ID_letselschade <- test_cases %>%
  select(Patient.traject.ID)
ID_controle <- test_matches %>%
  select(Patient.traject.ID)
long_format <- VAS_totaal %>%
  inner_join(Kort_cons %>%
               filter(!is.na(letselschade))%>%
               select(Patient.traject.ID, letselschade, behandeling.x)) %>%
  filter(behandeling.x != "Overige") %>%
  mutate(Indicator = case_when(
    Patient.traject.ID %in% ID_letselschade$Patient.traject.ID ~ "Personal injury claim",
    Patient.traject.ID %in% ID_controle$Patient.traject.ID ~ "Matched control",
    !Patient.traject.ID %in% test2$Patient.traject.ID ~ "Unmatched"
  )) %>%
  filter(rounddescription == "Intake" | rounddescription == "3 maanden")
long_format$Indicator <- factor(long_format$Indicator, c("Personal injury claim", "Matched control", "Unmatched"))
long_format$rounddescription <- factor(long_format$rounddescription, c("Intake", "3 maanden"))

# Data for VAS pain during load over time - Nonsurgical treatment------
grouped_VAS_kort_cons <- long_format %>% 
  group_by(Indicator, rounddescription) %>% 
  dplyr::summarize(N = n(), 
                   mean = mean(vasPijnBelasten_1), 
                   sd = sd(vasPijnBelasten_1), 
                   ymin = min(vasPijnBelasten_1), 
                   ymax = max(vasPijnBelasten_1)) %>%
  mutate(se = sd/sqrt(N))
# Figure of VAS pain during load over time - Nonsurgical treatment------
ggplot(grouped_VAS_kort_cons, aes(x=rounddescription, y=mean, group = Indicator)) +
  geom_line(aes(colour = Indicator)) +
  geom_errorbar(width=.1, aes(ymin=mean - 1.96*se, ymax=mean + 1.96*se, colour = Indicator), size = 1) +
  geom_point(aes(colour = Indicator), shape=21, size=1.7) +  
  xlab("Time") +
  ylab("VAS pain during load") + 
  ylim(0, 100) +
  labs(colour = "") + 
  theme_classic()
ggsave(file=paste("~/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/Figure_2a_VAS_pain_load_conservative", ".tiff", sep = ""))

# Data for VAS hand function over time - Nonsurgical treatment------
grouped_VAS_kort_cons <- long_format %>% 
  group_by(Indicator, rounddescription) %>% 
  dplyr::summarize(N = n(), 
                   mean = mean(vasFunctie_1), 
                   sd = sd(vasFunctie_1), 
                   ymin = min(vasFunctie_1), 
                   ymax = max(vasFunctie_1)) %>%
  mutate(se = sd/sqrt(N))
# Figure of VAS hand function over time - Nonsurgical treatment------
ggplot(grouped_VAS_kort_cons, aes(x=rounddescription, y=mean, group = Indicator)) +
  geom_line(aes(colour = Indicator)) +
  geom_errorbar(width=.1, aes(ymin=mean - 1.96*se, ymax=mean + 1.96*se, colour = Indicator), size = 1) +
  geom_point(aes(colour = Indicator), shape=21, size=1.7) +  
  xlab("Time") +
  ylab("VAS function") + 
  ylim(0, 100) +
  labs(colour = "") + 
  theme_classic()
ggsave(file=paste("~/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/Figure_2b_VAS_function_conservative", ".tiff", sep = ""))

# Matching for RTW analysis - Nonsurgical treatment -------
# Preparing data
RTW_df_cons <- inner_join(RTW_kort_cons, InV_totaal, by=c("Patient.traject.ID")) %>%
  inner_join(VAS_Intake, by="Patient.traject.ID") %>%
  inner_join(Intake_totaal %>%
               select(letselschade, Patient.traject.ID))

RTW_match_kort_cons <- RTW_df_cons %>%
  select(c(letselschade, Leeftijd, Geslacht, Beroep, zijde, dominant, hoeLangKlacht, behandeling,
           vasPijnBelasten_1, vasFunctie_1, Patient.traject.ID)) %>%
  na.omit() %>%
  droplevels()
nrow(RTW_match_kort_cons)
nrow(RTW_match_kort_cons)==length(unique(RTW_match_kort_cons$Patient.traject.ID))
sapply(RTW_match_kort_cons, function(x) sum(is.na(x)))
table(RTW_match_kort_cons$letselschade)
RTW_match_kort_cons$letselschade <- factor(RTW_match_kort_cons$letselschade, c("Nee", "Ja"))
RTW_match_kort_cons$letselschade <- as.logical(as.numeric(RTW_match_kort_cons$letselschade)-1)

# Extract standardized mean difference prior to matching
table1_RTW_cons <- tableone::CreateTableOne(vars = c("Leeftijd", "Geslacht", "hoeLangKlacht", "Beroep", "dominant", 
                                                 "vasPijnBelasten_1", "vasFunctie_1", "behandeling"), data = RTW_match_kort_cons,
                                        factorVars = c("Geslacht", "Beroep", "dominant"),
                                        strata = "letselschade", 
                                        smd = TRUE)
RTW_cons_voor_match <- as.data.frame(print(table1_RTW_cons, smd = TRUE)) 

SMD_vars_RTW_cons_voor_match <- table1_RTW_cons$MetaData$vars
SMD_RTW_cons_voor_match <- RTW_cons_voor_match$SMD[RTW_cons_voor_match$SMD != ""]

# Matching 
set.seed(123)
RTW_match_kort_cons$behandeling <- as.factor(RTW_match_kort_cons$behandeling)
matchited <- matchit(letselschade ~ Leeftijd + Geslacht+ Beroep + dominant +
                       vasPijnBelasten_1 + vasFunctie_1  +  behandeling,
                     data = RTW_match_kort_cons, method = "nearest", ratio = 2)

df.matched_RTW <- df.matched <- match.data(matchited)[1:ncol(RTW_match_kort_cons)]
table(df.matched_RTW$letselschade) # Evaluate whether all personal injury claim patients were matched

# Extract outcome data on RTW
rowid_cases <- rownames(matchited$match.matrix)
rowid_matches <- matchited$match.matrix %>% 
  cbind() %>% 
  as.vector()
rowid_all <- c(rowid_cases, rowid_matches)
test2 <- RTW_match_kort_cons[rowid_all, ]

# Extract standardized mean difference after matching
table1 <- tableone::CreateTableOne(vars = c("Leeftijd", "Geslacht", "hoeLangKlacht", "Beroep", "dominant", 
                                             "vasPijnBelasten_1",  "vasFunctie_1", "behandeling"), data = df.matched,
                                   factorVars = c("Geslacht", "Beroep", "dominant"),
                                   strata = "letselschade", 
                                   smd = TRUE)
RTW_cons_na_match <- as.data.frame(print(table1, smd = TRUE)) 

SMD_vars_RTW_cons_na_match <- table1$MetaData$vars
SMD_RTW_cons_na_match <- RTW_cons_na_match$SMD[RTW_cons_na_match$SMD != ""] 

Table_SMD <- data.frame(Var = SMD_vars_RTW_cons_voor_match, SMD = as.numeric(SMD_RTW_cons_voor_match), Type = "Voor matching")
Table_SMD_matched <- data.frame(Var = SMD_vars_RTW_cons_na_match, SMD = as.numeric(SMD_RTW_cons_na_match), Type = "Na matching")
Table_SMD_kort_cons_RTW <- rbind(Table_SMD, Table_SMD_matched)
Table_SMD_kort_cons_RTW$Type <- factor(Table_SMD_kort_cons_RTW$Type, c("Voor matching", "Na matching"))

# Figure of standardized mean difference prior to and after matching
Table_SMD_kort_cons_RTW$Var <- rep(c("Age", "Gender", "Duration of symptoms", "Type of work", "Dominant side", "VAS pain during load",  "VAS hand function", "Treatment"))
Table_SMD_kort_cons_RTW$Type <- rep(c("Prior to matching", "After matching"), each = 8)
Table_SMD_kort_cons_RTW$Type <- factor(Table_SMD_kort_cons_RTW$Type, c("Prior to matching", "After matching"))
ggplot(Table_SMD_kort_cons_RTW, aes(x = SMD, y = Var, colour = Type)) +
  geom_vline(xintercept = 0.1, linetype = 2) +
  geom_point() +
  geom_point() +
  theme_classic() +
  xlab("Standardized Mean Difference") +
  ylab("") +
  labs(colour = "")
ggsave(paste("/Users/lisa/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/", 
             "Supplemental_figure_2-Matching resultaat RTW conservatief",
             ".tiff", sep = ""))

# Add information on matching status
new_RTW_cons <- RTW_kort_cons %>%
  inner_join(test2, by="Patient.traject.ID")
new_RTW_cons$personal_injury_case <- ifelse(new_RTW_cons$letselschade == TRUE, TRUE, FALSE)

# Figure of RTW for matched patients - Nonsurgical treatment -----
KM2 <- survfit(Surv(EventTime, Event) ~ personal_injury_case, data = new_RTW_cons)
ggsurvplot(KM2, fun = "event",
           risk.table = FALSE,
           conf.int = FALSE,
           xlim = c(0, 52),ylim = c(0,1), break.time.by = 2, 
           xlab = "Time (weeks)", ylab = "Return to work",
           surv.median.line = "hv", surv.scale = "percent",
           palette = c("blue", "red"))
ggsave(file=paste("/Users/lisa/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/", 
                  "Figure_3-RTW_letselschade_conservatieve_trajecten",
                  ".tiff", sep=''))

# Evaluate hazard ratios RTW - Nonsurgical treatment ------
CM <- coxph(Surv(EventTime, Event) ~  letselschade + behandeling, 
            data = new_RTW_cons)
a  <- broom::tidy(CM)
round(exp(cbind(HR = coef(CM), confint(CM))), 3)

# Matching for VAS analyses - Minor surgery -------
# Preparing data
kort.chi.match <- Kort_chi %>%
  select(c(letselschade, Leeftijd, Geslacht, Beroep, zijde, dominant, hoeLangKlacht, behandeling.x,
           vasPijnBelasten_1_Int:vasFunctie_1_Int, 
           vasPijnBelasten_1_3m:vasFunctie_1_3m)) %>%
  na.omit() 

kort.chi.match$letselschade <- factor(kort.chi.match$letselschade, c("Nee", "Ja"))
kort.chi.match$letselschade <- as.logical(as.numeric(kort.chi.match$letselschade)-1)
table(kort.chi.match$letselschade)

# Extract standardized mean difference prior to matching
table1_voor_match <- tableone::CreateTableOne(vars = c("Leeftijd", "Geslacht", "hoeLangKlacht", "Beroep", "dominant", 
                                            "vasPijnBelasten_1_Int", "vasFunctie_1_Int", "behandeling.x"), data = kort.chi.match,
                                   factorVars = c("Geslacht", "Beroep", "dominant"),
                                   strata = "letselschade", 
                                   smd = TRUE)
VAS_kort_chi_voor_match <- as.data.frame(print(table1_voor_match, smd = TRUE))

SMD_vars_VAS_kort_chi_voor_match <- table1_voor_match$MetaData$vars
SMD_VAS_kort_chi_voor_match <- VAS_kort_chi_voor_match$SMD[VAS_kort_chi_voor_match$SMD != ""]

# Matching
set.seed(123)
matchited <- matchit(letselschade ~ Leeftijd + Geslacht + Beroep + dominant +
                      vasPijnBelasten_1_Int  + vasFunctie_1_Int  + 
                       behandeling.x,
                     data = kort.chi.match, method = "nearest", ratio = 2)

df.matched <- match.data(matchited)[1:ncol(kort.chi.match)]
table(df.matched$letselschade) # Evaluate whether all personal injury claim patients were matched


# Extract standardized mean difference after matching
table1 <- tableone::CreateTableOne(vars = c("Leeftijd", "Geslacht", "hoeLangKlacht", "Beroep", "dominant", 
                                           "vasPijnBelasten_1_Int",  "vasFunctie_1_Int", "behandeling.x"), data = df.matched,
                                   factorVars = c("Geslacht", "Beroep", "dominant"),
                                   strata = "letselschade", 
                                   smd = TRUE)
VAS_kort_chi_na_match <- as.data.frame(print(table1, smd = TRUE)) 

SMD_vars_VAS_kort_chi_na_match <- table1$MetaData$vars
SMD_VAS_kort_chi_na_match <- VAS_kort_chi_na_match$SMD[VAS_kort_chi_na_match$SMD != ""]

Table_SMD <- data.frame(Var = SMD_vars_VAS_kort_chi_voor_match, SMD = as.numeric(SMD_VAS_kort_chi_voor_match), Type = "Voor matching")
Table_SMD_matched <- data.frame(Var = SMD_vars_VAS_kort_chi_na_match, SMD = as.numeric(SMD_VAS_kort_chi_na_match), Type = "Na matching")
Table_SMD_kort_chi_VAS <- rbind(Table_SMD, Table_SMD_matched)
Table_SMD_kort_chi_VAS$Type <- factor(Table_SMD_kort_chi_VAS$Type, c("Voor matching", "Na matching"))

# Figure of standardized mean difference prior to and after matching
Table_SMD_kort_chi_VAS$Var <- rep(c("Age", "Gender", "Duration of symptoms", "Type of work", "Dominant side", "VAS pain during load", "VAS hand function", "Treatment"))
Table_SMD_kort_chi_VAS$Type <- rep(c("Prior to matching", "After matching"), each = 8)
Table_SMD_kort_chi_VAS$Type <- factor(Table_SMD_kort_chi_VAS$Type, c("Prior to matching", "After matching"))
ggplot(Table_SMD_kort_chi_VAS, aes(x = SMD, y = Var, colour = Type)) +
  geom_vline(xintercept = 0.1, linetype = 2) +
  geom_point() +
  geom_point() +
  theme_classic() +
  xlab("Standardized Mean Difference") +
  ylab("") +
  labs(colour = "")
ggsave(paste("/Users/lisa/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/", 
             "Supplemental_figure_3-Matching resultaat VAS kleine OKs",
             ".tiff", sep = ""))

# Evaluate outcomes VAS scores over time - Minor surgery ------
fit <- lm(vasPijnBelasten_1_3m ~ letselschade + Geslacht + dominant + behandeling.x, data = df.matched)
summary(fit)  # Pain during load
confint(fit)

fit <- lm(vasFunctie_1_3m ~ letselschade + Geslacht + dominant + behandeling.x, data = df.matched)
summary(fit)  # Hand function
confint(fit)

# Figures VAS scores over time - Minor surgery 
# Preparing data
originele_data <- Kort_chi %>%
  select(Leeftijd, Geslacht, hoeLangKlacht, Beroep, dominant, Patient.traject.ID, vasPijnBelasten_1_Int, vasFunctie_1_Int, vasPijnBelasten_1_3m, vasFunctie_1_3m, letselschade) %>% 
  na.omit() 
rowid_cases <- rownames(matchited$match.matrix)
rowid_matches <- matchited$match.matrix %>% 
  cbind() %>% 
  as.vector()
test_cases <- originele_data[rowid_cases, ]
test_matches <- originele_data[rowid_matches, ]
ID_letselschade <- test_cases %>%
  select(Patient.traject.ID)
ID_controle <- test_matches %>%
  select(Patient.traject.ID)
long_format <- VAS_totaal %>%
  inner_join(Kort_chi %>%
               filter(!is.na(letselschade))%>%
               select(Patient.traject.ID, letselschade, behandeling.x)) %>%
  filter(behandeling.x != "Overige") %>%
  mutate(Indicator = case_when(
    Patient.traject.ID %in% ID_letselschade$Patient.traject.ID ~ "Personal injury claim",
    Patient.traject.ID %in% ID_controle$Patient.traject.ID ~ "Matched control",
    !Patient.traject.ID %in% test2$Patient.traject.ID ~ "Unmatched"
  )) %>%
  filter(rounddescription == "Intake" | rounddescription == "3 maanden")
long_format$Indicator <- factor(long_format$Indicator, c("Personal injury claim", "Matched control", "Unmatched"))
long_format$rounddescription <- factor(long_format$rounddescription, c("Intake", "3 maanden"))

# Data for VAS pain during load over time - Minor surgery------
grouped_VAS_kort_chi <- long_format %>% 
  group_by(Indicator, rounddescription) %>% 
  dplyr::summarize(N = n(), 
                   mean = mean(vasPijnBelasten_1), 
                   sd = sd(vasPijnBelasten_1), 
                   ymin = min(vasPijnBelasten_1), 
                   ymax = max(vasPijnBelasten_1)) %>%
  mutate(se = sd/sqrt(N))

# Figure of VAS pain during load over time - Minor surgery------
ggplot(grouped_VAS_kort_chi, aes(x=rounddescription, y=mean, group = Indicator)) +
  geom_line(aes(colour = Indicator)) +
  geom_errorbar(width=.1, aes(ymin=mean - 1.96*se, ymax=mean + 1.96*se, colour = Indicator), size = 1) +
  geom_point(aes(colour = Indicator), shape=21, size=1.7) +  
  xlab("Time") +
  ylab("VAS pain during load") + 
  ylim(0, 100) +
  labs(colour = "") + 
  theme_classic()
ggsave(file=paste("~/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/Figure_4a_VAS_pain_load_minor_surgery", ".tiff", sep = ""))

# Data for VAS hand function over time - Minor surgery------
grouped_VAS_kort_chi <- long_format %>% 
  group_by(Indicator, rounddescription) %>% 
  dplyr::summarize(N = n(), 
                   mean = mean(vasFunctie_1), 
                   sd = sd(vasFunctie_1), 
                   ymin = min(vasFunctie_1), 
                   ymax = max(vasFunctie_1)) %>%
  mutate(se = sd/sqrt(N))

# Figure of VAS hand function over time - Minor surgery------
ggplot(grouped_VAS_kort_chi, aes(x=rounddescription, y=mean, group = Indicator)) +
  geom_line(aes(colour = Indicator)) +
  geom_errorbar(width=.1, aes(ymin=mean - 1.96*se, ymax=mean + 1.96*se, colour = Indicator), size = 1) +
  geom_point(aes(colour = Indicator), shape=21, size=1.7) +  
  xlab("Time") +
  ylab("VAS function") + 
  ylim(0, 100) +
  labs(colour = "") + 
  theme_classic()
ggsave(file=paste("~/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/Figure_4b_VAS_function_minor_surgery", ".tiff", sep = ""))

# Matching for RTW analysis - Minor surgery -------
# Preparing data
RTW_df_chi <- inner_join(RTW_kort_chi, InV_totaal, by=c("Patient.traject.ID")) %>%
  inner_join(VAS_Intake, by="Patient.traject.ID") %>%
  inner_join(Intake_totaal %>%
               select(letselschade, Patient.traject.ID))
table(RTW_df_chi$letselschade)

RTW_match_kort_chi <- RTW_df_chi %>%
  select(c(letselschade, Leeftijd, Geslacht, Beroep, zijde, dominant, hoeLangKlacht, behandeling,
           vasPijnBelasten_1:vasFunctie_1, Patient.traject.ID)) %>%
  na.omit() %>%
  droplevels()
nrow(RTW_match_kort_chi)==length(unique(RTW_match_kort_chi$Patient.traject.ID))
sapply(RTW_match_kort_chi, function(x) sum(is.na(x)))
table(RTW_match_kort_chi$letselschade)
RTW_match_kort_chi$letselschade <- factor(RTW_match_kort_chi$letselschade, c("Nee", "Ja"))
RTW_match_kort_chi$letselschade <- as.logical(as.numeric(RTW_match_kort_chi$letselschade)-1)

# Extract standardized mean difference prior to matching
RTW_kort_chi_voor_match <- tableone::CreateTableOne(vars = c("Leeftijd", "Geslacht", "hoeLangKlacht", "Beroep",  "dominant", "vasPijnBelasten_1", "vasFunctie_1","behandeling"), data = RTW_match_kort_chi,
                                                factorVars = c("Geslacht", "Beroep", "dominant"),
                                                strata = "letselschade", 
                                                smd = TRUE)
RTW_kort_chi_voor_match_data <- as.data.frame(print(RTW_kort_chi_voor_match, smd = TRUE)) 
SMD_vars_RTW_kort_chi_voor_match <- RTW_kort_chi_voor_match$MetaData$vars
SMD_RTW_kort_chi_voor_match <- RTW_kort_chi_voor_match_data$SMD[RTW_kort_chi_voor_match_data$SMD != ""]

# Matching 
set.seed(123)
RTW_match_kort_chi$behandeling <- as.factor(RTW_match_kort_chi$behandeling )
matchited <- matchit(letselschade ~ Leeftijd + Geslacht + hoeLangKlacht + Beroep + dominant +
                     vasPijnBelasten_1 + vasFunctie_1 +  behandeling,
                     data = RTW_match_kort_chi, method = "nearest", ratio = 2)

df.matched_RTW <- df.matched <- match.data(matchited)[1:ncol(RTW_match_kort_chi)]
table(df.matched_RTW$letselschade) # Evaluate whether all personal injury claim patients were matched

# Extract outcome data on RTW
rowid_cases <- rownames(matchited$match.matrix)
rowid_matches <- matchited$match.matrix %>% 
  cbind() %>% 
  as.vector()
rowid_all <- c(rowid_cases, rowid_matches)
test2 <- RTW_match_kort_chi[rowid_all, ]

# Extract standardized mean difference after matching
table1 <- tableone::CreateTableOne(vars = c("Leeftijd", "Geslacht", "Beroep", "dominant", "hoeLangKlacht",
                                            "vasPijnBelasten_1","vasFunctie_1", "behandeling"), data = df.matched,
                                   factorVars = c("Geslacht", "Beroep", "dominant"),
                                   strata = "letselschade", 
                                   smd = TRUE)

RTW_kort_chi_na_match_data <- as.data.frame(print(table1, smd = TRUE)) 
SMD_vars_RTW_kort_chi_na_match <- table1$MetaData$vars
SMD_RTW_kort_chi_na_match <- RTW_kort_chi_na_match_data$SMD[RTW_kort_chi_na_match_data$SMD != ""]

Table_SMD <- data.frame(Var = SMD_vars_RTW_kort_chi_voor_match, SMD = as.numeric(SMD_RTW_kort_chi_voor_match), Type = "Voor matching")
Table_SMD_matched <- data.frame(Var = SMD_vars_RTW_kort_chi_na_match, SMD = as.numeric(SMD_RTW_kort_chi_na_match), Type = "Na matching")
Table_SMD_kort_chi_RTW <- rbind(Table_SMD, Table_SMD_matched)
Table_SMD_kort_chi_RTW$Type <- factor(Table_SMD_kort_chi_RTW$Type, c("Voor matching", "Na matching"))

# Figure of standardized mean difference prior to and after matching
Table_SMD_kort_chi_RTW$Var <- rep(c("Age", "Gender", "Duration of symptoms", "Type of work", "Dominant side", "VAS pain during load","VAS hand function", "Treatment"))
Table_SMD_kort_chi_RTW$Type <- rep(c("Prior to matching", "After matching"), each = 8)
Table_SMD_kort_chi_RTW$Type <- factor(Table_SMD_kort_chi_RTW$Type, c("Prior to matching", "After matching"))
ggplot(Table_SMD_kort_chi_RTW, aes(x = SMD, y = Var, colour = Type)) +
  geom_vline(xintercept = 0.1, linetype = 2) +
  geom_point() +
  geom_point() +
  theme_classic() +
  xlab("Standardized Mean Difference") +
  ylab("") +
  labs(colour = "")
ggsave(paste("/Users/lisa/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/", 
             "Supplemental_figure_4-Matching resultaat RTW kleine OKs",
             ".tiff", sep = ""))

# Add information on matching status
new_RTW_chi <- RTW_kort_chi %>%
  inner_join(test2, by="Patient.traject.ID")
new_RTW_chi$personal_injury_case <- ifelse(new_RTW_chi$letselschade == TRUE, TRUE, FALSE)

# Figure of RTW for matched patients - Minor surgery -----
KM2 <- survfit(Surv(EventTime, Event) ~ personal_injury_case, data = new_RTW_chi)
ggsurvplot(KM2, fun = "event",
           risk.table = FALSE,
           conf.int = FALSE,
           xlim = c(0, 52),ylim = c(0,1), break.time.by = 2, 
           xlab = "Time (weeks)", ylab = "Return to work",
           surv.median.line = "hv", surv.scale = "percent",
           palette = c("blue", "red"))
ggsave(file=paste("/Users/lisa/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/", 
                  "Figure_5-RTW_letselschade_kleine_OKs",
                  ".tiff", sep=''))

surv_diff <- survdiff(Surv(EventTime, Event) ~ letselschade, data = new_RTW_chi)

# Evaluate hazard ratios RTW - Minor surgery ------
CM <- coxph(Surv(EventTime, Event) ~ 
              letselschade + behandeling + Beroep, 
            data = new_RTW_chi)
a  <- broom::tidy(CM)
round(exp(cbind(HR = coef(CM), confint(CM))), 3)

# Matching for VAS analyses - Major surgery -------
# Preparing data
lang.match <- Lang %>%
  select(c(letselschade, Leeftijd, Geslacht, Beroep, zijde, dominant, hoeLangKlacht, behandeling.x,
           vasPijnBelasten_1_Int, vasFunctie_1_Int, 
           vasPijnBelasten_1_12m:vasFunctie_1_12m)) %>%
  na.omit() %>%
  droplevels() 

lang.match$letselschade <- factor(lang.match$letselschade, c("Nee", "Ja"))
lang.match$letselschade <- as.logical(as.numeric(lang.match$letselschade)-1)
table(lang.match$letselschade)

# Extract standardized mean difference prior to matching
vars <- c("Leeftijd", "Geslacht", "hoeLangKlacht", "Beroep", "dominant",
          "vasPijnBelasten_1_Int", "vasFunctie_1_Int", "behandeling.x")
lang_voor_match <- tableone::CreateTableOne(vars = vars, strata = "letselschade", data = lang.match)
table1_lang_voor_match <- as.data.frame(print(lang_voor_match, smd = TRUE))
SMD_vars_lang_voor_match <- lang_voor_match$MetaData$vars
SMD_lang_voor_match <- table1_lang_voor_match$SMD[table1_lang_voor_match$SMD != ""]

# Matching 
set.seed(123)
matchited <- matchit(letselschade ~ Leeftijd + Geslacht + Beroep + dominant +
                       vasPijnBelasten_1_Int + vasFunctie_1_Int + 
                       behandeling.x,
                     data = lang.match, method = "nearest", ratio = 2)

df.matched <- match.data(matchited)[1:ncol(lang.match)]
table(df.matched$letselschade) # Evaluate whether all personal injury claim patients were matched

# Extract standardized mean difference after matching
lang_na_match <- tableone::CreateTableOne(vars = vars, data = df.matched,
                                          factorVars = c("Geslacht", "Beroep", "dominant"),
                                          strata = "letselschade", 
                                          smd = TRUE)
table1_lang_na_match <- as.data.frame(print(lang_na_match, smd = TRUE))

SMD_vars_lang_na_match <- lang_na_match$MetaData$vars
SMD_lang_na_match <- table1_lang_na_match$SMD[table1_lang_na_match$SMD != ""]

Table_SMD <- data.frame(Var = SMD_vars_lang_voor_match, SMD = as.numeric(SMD_lang_voor_match), Type = "Voor matching")
SMD_temp_na_match <- as.numeric(SMD_lang_na_match)
Table_SMD_matched <- data.frame(Var = SMD_vars_lang_na_match, SMD = SMD_temp_na_match, Type = "Na matching")
Table_SMD_lang_VAS <- rbind(Table_SMD, Table_SMD_matched)
Table_SMD_lang_VAS$Type <- factor(Table_SMD_lang_VAS$Type, c("Voor matching", "Na matching"))

# Figure of standardized mean difference prior to and after matching
Table_SMD_lang_VAS$Var <- rep(c("Age", "Gender", "Duration of symptoms", "Type of work", "Dominant side", "VAS pain during load", "VAS hand function", "Treatment"))
Table_SMD_lang_VAS$Type <- rep(c("Prior to matching", "After matching"), each = 8)
Table_SMD_lang_VAS$Type <- factor(Table_SMD_lang_VAS$Type, c("Prior to matching", "After matching"))
ggplot(Table_SMD_lang_VAS, aes(x = SMD, y = Var, colour = Type)) +
  geom_vline(xintercept = 0.1, linetype = 2) +
  geom_point() +
  geom_point() +
  theme_classic() +
  xlab("Standardized Mean Difference") +
  ylab("") +
  labs(colour = "")
ggsave("~/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/Supplemental_figure_5-Matching resultaten VAS grote OK.tiff")

# Evaluate outcomes VAS scores over time - Major surgery ------
fit <- lm(vasPijnBelasten_1_12m ~ letselschade + behandeling.x, df.matched)
summary(fit) # Pain during load
fit <- lm(vasFunctie_1_12m ~ letselschade + behandeling.x, df.matched)
summary(fit) # Hand function

# Figures VAS scores over time - Major surgery 
# Preparing data
originele_data <- Lang %>%
  select(contains(vars), Patient.traject.ID, vasPijnBelasten_1_12m, vasFunctie_1_12m, letselschade) %>% 
  na.omit() 
rowid_cases <- rownames(matchited$match.matrix)
rowid_matches <- matchited$match.matrix %>% 
  cbind() %>% 
  as.vector()
test_cases <- originele_data[rowid_cases, ]
test_matches <- originele_data[rowid_matches, ]
ID_letselschade <- test_cases %>%
  select(Patient.traject.ID)
ID_controle <- test_matches %>%
  select(Patient.traject.ID)
long_format <- VAS_totaal %>%
  inner_join(Lang %>%
               filter(!is.na(letselschade))%>%
               select(Patient.traject.ID, letselschade, behandeling.x)) %>%
  filter(behandeling.x != "Overige") %>%
  mutate(Indicator = case_when(
    Patient.traject.ID %in% ID_letselschade$Patient.traject.ID ~ "Personal injury claim",
    Patient.traject.ID %in% ID_controle$Patient.traject.ID ~ "Matched control",
    !Patient.traject.ID %in% test2$Patient.traject.ID ~ "Unmatched"
  )) %>%
  filter(rounddescription == "Intake" | rounddescription == "12 maanden")
long_format$Indicator <- factor(long_format$Indicator, c("Personal injury claim", "Matched control", "Unmatched"))
long_format$rounddescription <- factor(long_format$rounddescription, c("Intake", "12 maanden"))

# Data for VAS pain during load over time - Major surgery------
grouped_VAS_lang <- long_format %>% 
  group_by(Indicator, rounddescription) %>% 
  dplyr::summarize(N = n(), 
                   mean = mean(vasPijnBelasten_1), 
                   sd = sd(vasPijnBelasten_1), 
                   ymin = min(vasPijnBelasten_1), 
                   ymax = max(vasPijnBelasten_1)) %>%
  mutate(se = sd/sqrt(N))
# Figure of VAS pain during load over time - Major surgery------
ggplot(grouped_VAS_lang, aes(x=rounddescription, y=mean, group = Indicator)) +
  geom_line(aes(colour = Indicator)) +
  geom_errorbar(width=.1, aes(ymin=mean - 1.96*se, ymax=mean + 1.96*se, colour = Indicator), size = 1) +
  geom_point(aes(colour = Indicator), shape=21, size=1.7) +  
  xlab("Time") +
  ylab("VAS pain during load") + 
  ylim(0, 100) +
  labs(colour = "") + 
  theme_classic()
ggsave(file=paste("~/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/Figure_6a_VAS_pain_load_major_surgery", ".tiff", sep = ""))

# Data for VAS hand function over time - Major surgery------
grouped_VAS_lang <- long_format %>% 
  group_by(Indicator, rounddescription) %>% 
  dplyr::summarize(N = n(), 
                   mean = mean(vasFunctie_1), 
                   sd = sd(vasFunctie_1), 
                   ymin = min(vasFunctie_1), 
                   ymax = max(vasFunctie_1)) %>%
  mutate(se = sd/sqrt(N))

# Figure of VAS hand function over time - Major surgery------
ggplot(grouped_VAS_lang, aes(x=rounddescription, y=mean, group = Indicator)) +
  geom_line(aes(colour = Indicator)) +
  geom_errorbar(width=.1, aes(ymin=mean - 1.96*se, ymax=mean + 1.96*se, colour = Indicator), size = 1) +
  geom_point(aes(colour = Indicator), shape=21, size=1.7) +  
  xlab("Time") +
  ylab("VAS function") + 
  ylim(0, 100) +
  labs(colour = "") + 
  theme_classic()
ggsave(file=paste("~/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/Figure_6b_VAS_function_major_surgery", ".tiff", sep = ""))

# Matching for RTW analysis - Major surgery -------
# Preparing data
RTW_df <- inner_join(RTW_lang, InV_totaal, by=c("Patient.traject.ID")) %>%
  inner_join(VAS_Intake, by="Patient.traject.ID") %>% 
  inner_join(Intake_totaal %>%
               select(letselschade, Patient.traject.ID))
table(RTW_df$letselschade, useNA = "ifany")

RTW_match_lang <- RTW_df %>%
  select(c(letselschade, Leeftijd, Geslacht, Beroep, zijde, dominant, hoeLangKlacht, behandeling,
           vasPijnBelasten_1, vasFunctie_1, Patient.traject.ID)) %>%
  na.omit() %>%
  droplevels()
nrow(RTW_match_lang)
sapply(RTW_match_lang, function(x) sum(is.na(x)))
table(RTW_match_lang$letselschade)
RTW_match_lang$letselschade <- factor(RTW_match_lang$letselschade, c("Nee", "Ja"))
RTW_match_lang$letselschade <- as.logical(as.numeric(RTW_match_lang$letselschade)-1)
RTW_match_lang$behandeling <- as.factor(RTW_match_lang$behandeling)

# Extract standardized mean difference prior to matching
vars <-  c("Leeftijd", "Geslacht", "hoeLangKlacht", "Beroep", "dominant",
           "vasPijnBelasten_1", "vasFunctie_1", "behandeling")
RTW_lang_voor_match <- tableone::CreateTableOne(vars = vars, data = RTW_match_lang,
                                                factorVars = c("Geslacht", "Beroep", "dominant"),
                                                strata = "letselschade", 
                                                smd = TRUE)
RTW_lang_voor_match_data <- as.data.frame(print(RTW_lang_voor_match, smd = TRUE))

SMD_vars_RTW_lang_voor_match <- RTW_lang_voor_match$MetaData$vars
SMD_RTW_lang_voor_match <- RTW_lang_voor_match_data$SMD[RTW_lang_voor_match_data$SMD != ""]

# Matching
set.seed(123)
matchited <- matchit(letselschade ~ Leeftijd + Geslacht + Beroep + dominant +
                       vasPijnBelasten_1 + vasFunctie_1 + behandeling,
                     data = RTW_match_lang, method = "nearest", ratio = 2)

df.matched_RTW <- df.matched <- match.data(matchited)[1:ncol(RTW_match_lang)]
table(df.matched$letselschade) # Evaluate whether all personal injury claim patients were matched

# Extract outcome data on RTW
rowid_cases <- rownames(matchited$match.matrix)
rowid_matches <- matchited$match.matrix %>% 
  cbind() %>% 
  as.vector()
rowid_all <- c(rowid_cases, rowid_matches)
test2 <- RTW_match_lang[rowid_all, ]

# Extract standardized mean difference after matching
table1 <- tableone::CreateTableOne(vars = vars, data = df.matched,
                                   factorVars = c("Geslacht", "Beroep", "dominant"),
                                   strata = "letselschade", 
                                   smd = TRUE)
RTW_lang_na_match <- as.data.frame(print(table1, smd = TRUE)) 

SMD_vars_RTW_lang_na_match <- table1$MetaData$vars
SMD_RTW_lang_na_match <- RTW_lang_na_match$SMD[RTW_lang_na_match$SMD != ""]

Table_SMD <- data.frame(Var = SMD_vars_RTW_lang_voor_match, SMD = as.numeric(SMD_RTW_lang_voor_match), Type = "Voor matching")
Table_SMD_matched <- data.frame(Var = SMD_vars_RTW_lang_na_match, SMD = as.numeric(SMD_RTW_lang_na_match), Type = "Na matching")
Table_SMD_lang_RTW <- rbind(Table_SMD, Table_SMD_matched)
Table_SMD_lang_RTW$Type <- factor(Table_SMD_lang_RTW$Type, c("Voor matching", "Na matching"))

# Figure of standardized mean difference prior to and after matching
Table_SMD_lang_RTW$Var <- rep(c("Age", "Gender", "Duration of symptoms", "Type of work", "Dominant side", "VAS pain during load", "VAS hand function", "Treatment"))
Table_SMD_lang_RTW$Type <- rep(c("Prior to matching", "After matching"), each = 8)
Table_SMD_lang_RTW$Type <- factor(Table_SMD_lang_VAS$Type, c("Prior to matching", "After matching"))
ggplot(Table_SMD_lang_RTW, aes(x = SMD, y = Var, colour = Type)) +
  geom_vline(xintercept = 0.1, linetype = 2) +
  geom_point() +
  geom_point() +
  theme_classic() +
  xlab("Standardized Mean Difference") +
  ylab("") +
  labs(colour = "")
ggsave(paste("/Users/lisa/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/", 
             "Supplemental figure 6-Matching_RTW_lang",
             ".tiff", sep = ""))

# Add information on matching status
new_RTW <- RTW_lang %>%
  inner_join(test2, by="Patient.traject.ID")
new_RTW$personal_injury_case <- ifelse(new_RTW$letselschade == TRUE, TRUE, FALSE)

# Figure of RTW for matched patients - Major surgery -----
KM2 <- survfit(Surv(EventTime, Event) ~ personal_injury_case, data = new_RTW)
ggsurvplot(KM2, fun = "event",
           risk.table = FALSE,
           conf.int = FALSE,
           xlim = c(0, 52),ylim = c(0,1), break.time.by = 2, 
           xlab = "Time (weeks)", ylab = "Return to work",
           surv.median.line = "hv", surv.scale = "percent",
           palette = c("blue", "red"))
ggsave(file=paste("/Users/lisa/Documents/Promotie/Projecten Lisa/Letselschade/Letselschade artikel Engels/", 
                  "Figure 7-RTW_letselschade_major_surgery",
                  ".tiff", sep=''))
surv_diff <- survdiff(Surv(EventTime, Event) ~ letselschade, data = new_RTW)

# Evaluate hazard ratios RTW - Major surgery ------
CM <- coxph(Surv(EventTime, Event) ~ 
              letselschade + vasFunctie_1 + hoeLangKlacht + behandeling, 
            data = new_RTW)
a  <- broom::tidy(CM)
round(exp(cbind(HR = coef(CM), confint(CM))), 3)