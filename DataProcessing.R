# Data Processing Script  
# Heloiza de Oliveira Souza -- October 2024


rm(list = ls())

# Bibliotecas ---------------------------------------------------------------

# package to reed the dataset
library(readxl)
# package to data manupulation
library(dplyr)
library(tidyr)
# package to strings manipulation
library(stringr)
# package to graphics production 
library(ggplot2)
# package to run the Tukey test
library(agricolae)
# package to run the Scott-Knott test
library(ExpDes.pt)


# Data Loading ------------------------------------------------------------

# 
# importing harvest data
potato120 <- read_xlsx(path = "Input/Dados pós colheita- campo.xlsx", sheet = 1)
potato150 <- read_xlsx(path = "Input/Dados pós colheita- campo.xlsx", sheet = 2)
potato180 <- read_xlsx(path = "Input/Dados pós colheita- campo.xlsx", sheet = 3)

# importing climate data
climate <- read_xlsx(path = "Input/Dados FAL - Estação Climatológica Automática.xlsx", sheet = 1)

# importing sensory evaluation dataset
sensorial <- read_xlsx(path = "Input/Avaliação sensorial 150D- copia.xlsx", sheet = 1)


# Data Processing ----------------------------------------------------------

# removing useless lines
potato120 <- potato120[-c(1,2),]
potato150 <- potato150[-c(1,2),]
potato180 <- potato180[-c(1,2),]


# renaming columns 
colnames(potato120) <- c('Genotipo', 'Gen', 'Parcela', 'n_Turb_Comerc', 'peso_Comerc',
                         'n_Turb_N_Com', 'peso_N_Com', 'Produtividade', 'Comp1',
                         'Comp2', 'Comp3', 'Comp4', 'Comp5', 'Diam1', 'Diam2', 
                         'Diam3', 'Diam4', 'Diam5', 'Espessura', 'obs', 'nFuros1',
                         'nFuros2', 'nFuros3', 'nFuros4', 'nFuros5', 'Form_Com1',
                         'Form_Com2', 'Form_Com3', 'Form_Com4', 'Form_Com5', 'Form_mapa1',
                         'Form_mapa2', 'Form_mapa3', 'Form_mapa4', 'Form_mapa5', 
                         'cor_Casca', 'cor_Polpa', 'cor_2Casca', 'cor_2Polpa')
colnames(potato150) <- c('Genotipo', 'Gen', 'Parcela', 'n_Turb_Comerc', 'peso_Comerc',
                         'n_Turb_N_Com', 'peso_N_Com', 'Produtividade', 'Comp1',
                         'Comp2', 'Comp3', 'Comp4', 'Comp5', 'Com_Medio', 'Diam1', 'Diam2', 
                         'Diam3', 'Diam4', 'Diam5', 'Diam_Medio', 'Espessura', 'obs', 'nFuros1',
                         'nFuros2', 'nFuros3', 'nFuros4', 'nFuros5', 'n_Furos_Medio', 'Form_mapa1',
                         'Form_mapa2', 'Form_mapa3', 'Form_mapa4', 'Form_mapa5', 
                         'cor_Casca', 'cor_Polpa', 'cor_2Casca', 'cor_2Polpa')
colnames(potato180) <- c('Genotipo', 'Gen', 'Parcela', 'n_Turb_Comerc', 'peso_Comerc',
                         'n_Turb_N_Com', 'peso_N_Com', 'Produtividade', 'Comp1',
                         'Comp2', 'Comp3', 'Comp4', 'Comp5', 'Diam1', 'Diam2', 
                         'Diam3', 'Diam4', 'Diam5', 'Espessura', 'obs', 'nFuros1',
                         'nFuros2', 'nFuros3', 'nFuros4', 'nFuros5', 'Form_mapa1',
                         'Form_mapa2', 'Form_mapa3', 'Form_mapa4', 'Form_mapa5', 
                         'cor_Casca', 'cor_Polpa', 'cor_2Casca', 'cor_2Polpa')


# treatments in the 120-day post-harvest dataset
potato120 <- potato120 %>%
  # transforms all other missing information codes into NA
  mutate(across(everything(), ~ replace(., . %in% c('-9','-7','****'), NA))) %>%
  # transforms numeric and categorical columns
  mutate(across(c(4:19, 21:25), ~ as.numeric(as.character(.))),
         across(c(1:3,36:39), ~as.factor(as.character(.)))) %>%
  # extracting only the numerical part of the color evaluations score 
  mutate(across(starts_with("cor"), ~ str_extract(., "\\d+") %>% as.numeric() %>% factor()))  %>%
  # creating the variable of total produtivity, average length and average diameter
  mutate(Produtividade = ifelse(is.na(peso_Comerc) & is.na(peso_N_Com), NA, 
                                coalesce(peso_Comerc, 0) + coalesce(peso_N_Com, 0)),
         Comp_Medio = rowMeans(select(., Comp1:Comp5), na.rm = TRUE),
         Diam_Medio = rowMeans(select(., Diam1:Diam5), na.rm = TRUE),
         nFuros_Medio = rowMeans(select(., nFuros1:nFuros5), na.rm = TRUE))


# treatments in the 150-day post-harvest dataset
potato150 <- potato150 %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  # transform all -9 values found in the dataset into NA
  mutate(across(everything(), ~ na_if(., '-9'))) %>%
  # transforms numeric and categorical columns
  mutate(across(c(4:21, 23:33), ~ as.numeric(as.character(.))),
         across(c(1:3,34:37), ~as.factor(as.character(.)))) %>%
  # creating the variable of total produtivity, average length and average diameter
  mutate(Produtividade = ifelse(is.na(peso_Comerc) & is.na(peso_N_Com), NA, 
                                coalesce(peso_Comerc, 0) + coalesce(peso_N_Com, 0)),
         Comp_Medio = rowMeans(select(., Comp1:Comp5), na.rm = TRUE),
         Diam_Medio = rowMeans(select(., Diam1:Diam5), na.rm = TRUE),
         nFuros_Medio = rowMeans(select(., nFuros1:nFuros5), na.rm = TRUE))


# treatments in the 180-day post-harvest dataset
potato180 <- potato180 %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  # transform all -9 values found in the dataset into NA
  mutate(across(everything(), ~ na_if(., '-9'))) %>%
  # transforms numeric and categorical columns
  mutate(across(c(4:19, 21:30), ~ as.numeric(as.character(.))),
         across(c(1:3,31:34), ~as.factor(as.character(.)))) %>%
  # creating the variable of total produtivity, average length and average diameter
  mutate(Produtividade = ifelse(is.na(peso_Comerc) & is.na(peso_N_Com), NA, 
                                coalesce(peso_Comerc, 0) + coalesce(peso_N_Com, 0)),
         Comp_Medio = rowMeans(select(., Comp1:Comp5), na.rm = TRUE),
         Diam_Medio = rowMeans(select(., Diam1:Diam5), na.rm = TRUE),
         nFuros_Medio = rowMeans(select(., nFuros1:nFuros5), na.rm = TRUE))

# creating a single data frame
produt_potato <- bind_rows(
  potato120 %>% select(Gen, Parcela, Produtividade, peso_Comerc, Comp_Medio, Diam_Medio, nFuros_Medio) %>% mutate(Colheita = 120),
  potato150 %>% select(Gen, Parcela, Produtividade, peso_Comerc, Comp_Medio, Diam_Medio, nFuros_Medio) %>% mutate(Colheita = 150),
  potato180 %>% select(Gen, Parcela, Produtividade, peso_Comerc, Comp_Medio, Diam_Medio, nFuros_Medio) %>% mutate(Colheita = 180)
)
produt_potato <- produt_potato %>% 
  mutate(Colheita = factor(Colheita, levels = c(120,150,180)),
         Produtiv_T = sqrt(Produtividade+0.0001),
         Peso_Com_T = sqrt(peso_Comerc+0.0001),
         Comp_T = sqrt(Comp_Medio+0.0001),
         n_Fur_T = sqrt(nFuros_Medio+0.0001))


## treatments in the climate dataset
climate <- climate %>% 
  mutate(Data = as.Date(Data, format = "%Y-%m-%d"),
         across(c(2:7), ~ as.numeric(as.character(.))))

# removing useless lines
climate <- climate[-c(30:34, 97:99),]


# treatments in the sensory evaluation dataset
sensorial <- sensorial %>%
  # transforms all 0 into NA
  mutate(across(c(3:11), ~ replace(., 0, NA)))

# filling in the age and gender of missing forms
sensorial_filled <- sensorial %>%
  group_by(Formulário) %>%
  fill(Idade, Gênero, .direction = "down") %>%
  fill(Idade, .direction = "up") %>%
  ungroup() %>% 
# creating age group variable
  mutate(faixa_etaria = as.factor(case_when(
    Idade >= 18 & Idade <= 20 ~ '18-20',
    Idade > 20 & Idade <= 22 ~ '20-22',
    Idade > 22 & Idade <= 24 ~ '22-24',
    Idade >= 25 ~ '25+'
  )))

