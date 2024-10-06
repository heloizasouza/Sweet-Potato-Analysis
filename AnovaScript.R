# análise de dados da batata doce

rm(list = ls())

# Bibliotecas ---------------------------------------------------------------

library(readxl) # para leitura dos dados
library(dplyr) # para processamento dos dados
library(stringr) # para tratamento de strings
library(ggplot2) # para produção gráfica
library(agricolae) # para executar o teste de Tukey
library(ExpDes.pt) # para executar o teste de Scott-Knott


# Carregamento dos dados ------------------------------------------------------------

# importação dos dados de colheita
batata120 <- read_xlsx(path = "Input/Dados pós colheita- campo.xlsx", sheet = 1)
batata150 <- read_xlsx(path = "Input/Dados pós colheita- campo.xlsx", sheet = 2)
batata180 <- read_xlsx(path = "Input/Dados pós colheita- campo.xlsx", sheet = 3)


# Processamento dos dados ----------------------------------------------------------


# removendo linhas desnecessárias
batata120 <- batata120[-c(1,2),]
batata150 <- batata150[-c(1,2),]
batata180 <- batata180[-c(1,2),]

# renomeando colunas
colnames(batata120) <- c('Genotipo', 'Gen', 'Parcela', 'n_Turb_Comerc', 'peso_Comerc',
                         'n_Turb_N_Com', 'peso_N_Com', 'Produtividade', 'Comp1',
                         'Comp2', 'Comp3', 'Comp4', 'Comp5', 'Diam1', 'Diam2', 
                         'Diam3', 'Diam4', 'Diam5', 'Espessura', 'obs', 'nFuros1',
                         'nFuros2', 'nFuros3', 'nFuros4', 'nFuros5', 'Form_Com1',
                         'Form_Com2', 'Form_Com3', 'Form_Com4', 'Form_Com5', 'Form_mapa1',
                         'Form_mapa2', 'Form_mapa3', 'Form_mapa4', 'Form_mapa5', 
                         'cor_Casca', 'cor_Polpa', 'cor_2Casca', 'cor_2Polpa')
colnames(batata150) <- c('Genotipo', 'Gen', 'Parcela', 'n_Turb_Comerc', 'peso_Comerc',
                         'n_Turb_N_Com', 'peso_N_Com', 'Produtividade', 'Comp1',
                         'Comp2', 'Comp3', 'Comp4', 'Comp5', 'Com_Medio', 'Diam1', 'Diam2', 
                         'Diam3', 'Diam4', 'Diam5', 'Diam_Medio', 'Espessura', 'obs', 'nFuros1',
                         'nFuros2', 'nFuros3', 'nFuros4', 'nFuros5', 'n_Furos_Medio', 'Form_mapa1',
                         'Form_mapa2', 'Form_mapa3', 'Form_mapa4', 'Form_mapa5', 
                         'cor_Casca', 'cor_Polpa', 'cor_2Casca', 'cor_2Polpa')
colnames(batata180) <- c('Genotipo', 'Gen', 'Parcela', 'n_Turb_Comerc', 'peso_Comerc',
                         'n_Turb_N_Com', 'peso_N_Com', 'Produtividade', 'Comp1',
                         'Comp2', 'Comp3', 'Comp4', 'Comp5', 'Diam1', 'Diam2', 
                         'Diam3', 'Diam4', 'Diam5', 'Espessura', 'obs', 'nFuros1',
                         'nFuros2', 'nFuros3', 'nFuros4', 'nFuros5', 'Form_mapa1',
                         'Form_mapa2', 'Form_mapa3', 'Form_mapa4', 'Form_mapa5', 
                         'cor_Casca', 'cor_Polpa', 'cor_2Casca', 'cor_2Polpa')


# tratamentos no conjunto de dados pós colheita de 120 dias
batata120 <- batata120 %>%
  # transforma em NA todos os outros códigos de informação faltante
  mutate(across(everything(), ~ replace(., . %in% c('-9','-7','****'), NA))) %>%
  # transforma as colunas numéricas e categóricas
  mutate(across(c(4:19, 21:25), ~ as.numeric(as.character(.))),
         across(c(1:3,36:39), ~as.factor(as.character(.)))) %>%
  # extraindo apenas a parte numérica da nota de avaliação de cor
  mutate(across(starts_with("cor"), ~ str_extract(., "\\d+") %>% as.numeric() %>% factor()))  %>% 
  # criando a variável de produtividade total, comprimento e diâmetro médio
  mutate(Produtividade = ifelse(is.na(peso_Comerc) & is.na(peso_N_Com), NA, 
                                coalesce(peso_Comerc, 0) + coalesce(peso_N_Com, 0)),
         Comp_Medio = sqrt(rowMeans(select(., Comp1:Comp5), na.rm = TRUE)+0.05),
         Diam_Medio = rowMeans(select(., Diam1:Diam5), na.rm = TRUE),
         nFuros_Medio = rowMeans(select(., nFuros1:nFuros5), na.rm = TRUE))


# tratamentos no conjunto de dados pós colheita de 150 dias
batata150 <- batata150 %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  # transforma em NA todos os valores -9 encontrados no df
  mutate(across(everything(), ~ na_if(., '-9'))) %>%
  # transforma as colunas numéricas e categóricas
  mutate(across(c(4:21, 23:33), ~ as.numeric(as.character(.))),
         across(c(1:3,34:37), ~as.factor(as.character(.)))) %>%
  # criando a variável de produtividade total, comprimento e diâmetro médio
  mutate(Produtividade = ifelse(is.na(peso_Comerc) & is.na(peso_N_Com), NA, 
                                coalesce(peso_Comerc, 0) + coalesce(peso_N_Com, 0)),
         Comp_Medio = rowMeans(select(., Comp1:Comp5), na.rm = TRUE),
         Diam_Medio = rowMeans(select(., Diam1:Diam5), na.rm = TRUE),
         nFuros_Medio = rowMeans(select(., nFuros1:nFuros5), na.rm = TRUE))


# tratamentos no conjunto de dados pós colheita de 150 dias
batata180 <- batata180 %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  # transforma em NA todos os valores -9 encontrados no df
  mutate(across(everything(), ~ na_if(., '-9'))) %>%
  # transforma as colunas numéricas e categóricas
  mutate(across(c(4:19, 21:30), ~ as.numeric(as.character(.))),
         across(c(1:3,31:34), ~as.factor(as.character(.)))) %>%
  # criando a variável de produtividade total, comprimento e diâmetro médio
  mutate(Produtividade = ifelse(is.na(peso_Comerc) & is.na(peso_N_Com), NA, 
                                coalesce(peso_Comerc, 0) + coalesce(peso_N_Com, 0)),
         Comp_Medio = rowMeans(select(., Comp1:Comp5), na.rm = TRUE),
         Diam_Medio = rowMeans(select(., Diam1:Diam5), na.rm = TRUE),
         nFuros_Medio = rowMeans(select(., nFuros1:nFuros5), na.rm = TRUE))


# criando um data frame único
produt_batata <- bind_rows(
  batata120 %>% select(Gen, Parcela, Produtividade, peso_Comerc, Comp_Medio, Diam_Medio, nFuros_Medio) %>% mutate(Colheita = 120),
  batata150 %>% select(Gen, Parcela, Produtividade, peso_Comerc, Comp_Medio, Diam_Medio, nFuros_Medio) %>% mutate(Colheita = 150),
  batata180 %>% select(Gen, Parcela, Produtividade, peso_Comerc, Comp_Medio, Diam_Medio, nFuros_Medio) %>% mutate(Colheita = 180)
  )
produt_batata$Colheita <- factor(produt_batata$Colheita, levels = c(120,150,180))


# Análise descritiva ----------------------------------------------------


# Colheita de 120 dias
ggplot(data = batata120, mapping = aes(x = Gen, y = Produtividade)) +
  geom_boxplot() +
  scale_x_discrete(limits = as.character(1:18)) +
  theme_light() +
  labs(x = 'Genótipo', y = "Produtividade Total",
       title = "Produtividade dos genótipos colhidos em 120 dias")


# Colheita de 150 dias
ggplot(data = batata150, mapping = aes(x = Gen, y = Produtividade)) +
  geom_boxplot() +
  scale_x_discrete(limits = as.character(1:18)) +
  theme_light() +
  labs(x = 'Genótipo', y = "Produtividade Total",
       title = "Produtividade dos genótipos colhidos em 150 dias")


# Colheita de 180 dias
ggplot(data = batata180, mapping = aes(x = Gen, y = Produtividade)) +
  geom_boxplot() +
  scale_x_discrete(limits = as.character(1:18)) +
  theme_light() +
  labs(x = 'Genótipo', y = "Produtividade Total",
       title = "Produtividade dos genótipos colhidos em 180 dias")
       
# Produtividade das três colheitas
ggplot(data = produt_batata, mapping = aes(x = Colheita, y = Produtividade)) + 
  geom_boxplot()

# Peso Comercial das três colheitas
ggplot(data = produt_batata, mapping = aes(x = Colheita, y = peso_Comerc)) + 
  geom_boxplot()

# Comprimento Médio das três colheitas
ggplot(data = produt_batata, mapping = aes(x = Colheita, y = Comp_Medio)) + 
  geom_boxplot()

# Diâmetro das três colheitas
ggplot(data = produt_batata, mapping = aes(x = Colheita, y = Diam_Medio)) + 
  geom_boxplot()

# n° de furos médio das três colheitas
ggplot(data = produt_batata, mapping = aes(x = Colheita, y = nFuros_Medio)) + 
  geom_boxplot()


# ANOVA 120 dias -------------------------------------------------------------------


# ANOVA da Produtividade de Colheita em função dos Genótipos
aov_prod_120 <- aov(formula = Produtividade ~ Parcela+Gen, data = batata120)
(summary_aov120 <- summary(aov_prod_120))
# Exportando para um arquivo .txt
capture.output(summary_aov120, file = "teste_F_prod_120.txt")


# ANOVA do Comprimento 
aov_comp_120 <- aov(formula = Comp_Medio ~ Parcela+Gen, data = batata120)
(summary_aov120 <- summary(aov_comp_120))
capture.output(summary_aov120, file = "teste_F_comp_120.txt")


# ANOVA do Diâmetro
aov_diam_120 <- aov(formula = Diam_Medio ~ Parcela+Gen, data = batata120)
(summary_aov120 <- summary(aov_diam_120))
capture.output(summary_aov120, file = "teste_F_diam_120.txt")


# ANOVA do n° de furos
aov_furos_120 <- aov(formula = nFuros_Medio ~ Parcela+Gen, data = batata120)
(summary_aov120 <- summary(aov_furos_120))
capture.output(summary_aov120, file = "teste_F_nFuros_120.txt")


# ANOVA Peso Comercial
aov_pesoCom_120 <- aov(formula = peso_Comerc ~ Parcela+Gen, data = batata120)
(summary_aov120 <- summary(aov_pesoCom_120))
capture.output(summary_aov120, file = "teste_F_pesoCom_120.txt")




# ANOVA 150 dias --------------------------------------------


# ANOVA da Produtividade de Colheita em função dos Genótipos
aov_prod_150 <- aov(formula = Produtividade ~ Parcela+Gen, data = batata150)
(summary_aov150 <- summary(aov_prod_150))
# Exportando para um arquivo .txt
capture.output(summary_aov150, file = "teste_F_prod_150.txt")


# ANOVA do Comprimento 
aov_comp_150 <- aov(formula = Comp_Medio ~ Parcela+Gen, data = batata150)
(summary_aov150 <- summary(aov_comp_150))
capture.output(summary_aov150, file = "teste_F_comp_150.txt")


# ANOVA do Diâmetro
aov_diam_150 <- aov(formula = Diam_Medio ~ Parcela+Gen, data = batata150)
(summary_aov150 <- summary(aov_diam_150))
capture.output(summary_aov150, file = "teste_F_diam_150.txt")


# ANOVA do n° de furos
aov_furos_150 <- aov(formula = nFuros_Medio ~ Parcela+Gen, data = batata150)
(summary_aov150 <- summary(aov_furos_150))
capture.output(summary_aov150, file = "teste_F_nFuros_150.txt")


# ANOVA Peso Comercial
aov_pesoCom_150 <- aov(formula = peso_Comerc ~ Parcela+Gen, data = batata150)
(summary_aov150 <- summary(aov_pesoCom_150))
capture.output(summary_aov150, file = "teste_F_pesoCom_150.txt")




# ANOVA 180 dias ----------------------------------------------------------


# ANOVA da Produtividade de Colheita em função dos Genótipos
aov_prod_180 <- aov(formula = Produtividade ~ Parcela+Gen, data = batata180)
(summary_aov180 <- summary(aov_prod_180))
# Exportando para um arquivo .txt
capture.output(summary_aov180, file = "teste_F_prod_180.txt")


# ANOVA do Comprimento 
aov_comp_180 <- aov(formula = Comp_Medio ~ Parcela+Gen, data = batata180)
(summary_aov180 <- summary(aov_comp_180))
capture.output(summary_aov180, file = "teste_F_comp_180.txt")


# ANOVA do Diâmetro
aov_diam_180 <- aov(formula = Diam_Medio ~ Parcela+Gen, data = batata180)
(summary_aov180 <- summary(aov_diam_180))
capture.output(summary_aov180, file = "teste_F_diam_180.txt")


# ANOVA do n° de furos
aov_furos_180 <- aov(formula = nFuros_Medio ~ Parcela+Gen, data = batata180)
(summary_aov180 <- summary(aov_furos_180))
capture.output(summary_aov180, file = "teste_F_nFuros_180.txt")


# ANOVA Peso Comercial
aov_pesoCom_180 <- aov(formula = peso_Comerc ~ Parcela+Gen, data = batata180)
(summary_aov180 <- summary(aov_pesoCom_180))
capture.output(summary_aov180, file = "teste_F_pesoCom_180.txt")




# ANOVA 2 fatores ---------------------------------------------------------

# ANOVA da Produtividade de Colheita em função dos Genótipos e Colheita
aov_prod <- aov(formula = Produtividade ~ Parcela+Gen*Colheita, data = produt_batata)
(summary_aov <- summary(aov_prod))
# Exportando para um arquivo .txt
capture.output(summary_aov, file = "teste_F_prod.txt")


# ANOVA do Comprimento 
aov_comp <- aov(formula = Comp_Medio ~ Parcela+Gen*Colheita, data = produt_batata)
(summary_aov <- summary(aov_comp))
capture.output(summary_aov, file = "teste_F_comp.txt")


# ANOVA do Diâmetro
aov_diam <- aov(formula = Diam_Medio ~ Parcela+Gen*Colheita, data = produt_batata)
(summary_aov <- summary(aov_diam))
capture.output(summary_aov, file = "teste_F_diam.txt")


# ANOVA do n° de furos
aov_furos <- aov(formula = nFuros_Medio ~ Parcela+Gen+Colheita, data = produt_batata)
(summary_aov <- summary(aov_furos))
capture.output(summary_aov, file = "teste_F_nFuros.txt")


# ANOVA Peso Comercial
aov_pesoCom <- aov(formula = peso_Comerc ~ Parcela+Gen*Colheita, data = produt_batata)
(summary_aov <- summary(aov_pesoCom))
capture.output(summary_aov, file = "teste_F_pesoCom.txt")



# Análise de resíduos -----------------------------------------------------


#### Colheita de 120 dias ####

# COMPRIMENTO MÉDIO -- passou na normalidade com transformação
# normalidade dos resíduos
shapiro.test(residuals(aov_comp_120))
# homocedasticidade das variâncias
car::leveneTest(Comp_Medio ~ Gen, data = batata120)
# independência dos resíduos
lmtest::dwtest(Comp_Medio ~ Gen, data = batata120)

# DIÂMETRO MÉDIO -- passou
# normalidade dos resíduos
shapiro.test(residuals(aov_diam_120))
# homocedasticidade das variâncias
car::leveneTest(Diam_Medio ~ Gen, data = batata120)
# independência dos resíduos
lmtest::dwtest(Diam_Medio ~ Gen, data = batata120)

# N° DE FUROS -- não passou na normalidade
# normalidade dos resíduos
shapiro.test(residuals(aov_furos_120))
# homocedasticidade das variâncias
car::leveneTest(nFuros_Medio ~ Gen, data = batata120)
# independência dos resíduos
lmtest::dwtest(nFuros_Medio ~ Gen, data = batata120)

# PESO COMERCIAL -- passou
# normalidade dos resíduos
shapiro.test(residuals(aov_pesoCom_120))
# homocedasticidade das variâncias
car::leveneTest(peso_Comerc ~ Gen, data = batata120)
# independência dos resíduos
lmtest::dwtest(peso_Comerc ~ Gen, data = batata120)

# PRODUTIVIDADE TOTAL -- passou
# normalidade dos resíduos
shapiro.test(residuals(aov_prod_120))
# homocedasticidade das variâncias
car::leveneTest(Produtividade ~ Gen, data = batata120)
# independência dos resíduos
lmtest::dwtest(Produtividade ~ Gen, data = batata120)


#### Colheita de 150 dias ####

# COMPRIMENTO MÉDIO -- passou
# normalidade dos resíduos
shapiro.test(residuals(aov_comp_150))
# homocedasticidade das variâncias
car::leveneTest(Comp_Medio ~ Gen, data = batata150)
# independência dos resíduos
lmtest::dwtest(Comp_Medio ~ Gen, data = batata150)

# DIÂMETRO MÉDIO -- passou
# normalidade dos resíduos
shapiro.test(residuals(aov_diam_150))
# homocedasticidade das variâncias
car::leveneTest(Diam_Medio ~ Gen, data = batata150)
# independência dos resíduos
lmtest::dwtest(Diam_Medio ~ Gen, data = batata150)

# N° DE FUROS -- não passou na normalidade
# normalidade dos resíduos
shapiro.test(residuals(aov_furos_150))
# homocedasticidade das variâncias
car::leveneTest(nFuros_Medio ~ Gen, data = batata150)
# independência dos resíduos
lmtest::dwtest(nFuros_Medio ~ Gen, data = batata150)

# PESO COMERCIAL -- passou
# normalidade dos resíduos
shapiro.test(residuals(aov_pesoCom_150))
# homocedasticidade das variâncias
car::leveneTest(peso_Comerc ~ Gen, data = batata150)
# independência dos resíduos
lmtest::dwtest(peso_Comerc ~ Gen, data = batata150)

# PRODUTIVIDADE TOTAL -- passou
# normalidade dos resíduos
shapiro.test(residuals(aov_prod_150))
# homocedasticidade das variâncias
car::leveneTest(Produtividade ~ Gen, data = batata150)
# independência dos resíduos
lmtest::dwtest(Produtividade ~ Gen, data = batata150)


#### Colheita de 180 dias ####

# COMPRIMENTO MÉDIO -- passou
# normalidade dos resíduos
shapiro.test(residuals(aov_comp_180))
# homocedasticidade das variâncias
car::leveneTest(Comp_Medio ~ Gen, data = batata180)
# independência dos resíduos
lmtest::dwtest(Comp_Medio ~ Gen, data = batata180)

# DIÂMETRO MÉDIO -- passou
# normalidade dos resíduos
shapiro.test(residuals(aov_diam_180))
# homocedasticidade das variâncias
car::leveneTest(Diam_Medio ~ Gen, data = batata180)
# independência dos resíduos
lmtest::dwtest(Diam_Medio ~ Gen, data = batata180)

# N° DE FUROS -- não passou na normalidade
# normalidade dos resíduos
shapiro.test(residuals(aov_furos_180))
# homocedasticidade das variâncias
car::leveneTest(nFuros_Medio ~ Gen, data = batata180)
# independência dos resíduos
lmtest::dwtest(nFuros_Medio ~ Gen, data = batata180)

# PESO COMERCIAL -- passou
# normalidade dos resíduos
shapiro.test(residuals(aov_pesoCom_180))
# homocedasticidade das variâncias
car::leveneTest(peso_Comerc ~ Gen, data = batata180)
# independência dos resíduos
lmtest::dwtest(peso_Comerc ~ Gen, data = batata180)

# PRODUTIVIDADE TOTAL -- passou
# normalidade dos resíduos
shapiro.test(residuals(aov_prod_180))
# homocedasticidade das variâncias
car::leveneTest(Produtividade ~ Gen, data = batata180)
# independência dos resíduos
lmtest::dwtest(Produtividade ~ Gen, data = batata180)


#### As Três Colheitas ####

# COMPRIMENTO MÉDIO -- não passou na normalidade
# normalidade dos resíduos
shapiro.test(residuals(aov_comp))
# homocedasticidade das variâncias
car::leveneTest(Comp_Medio ~ Gen, data = produt_batata)
# independência dos resíduos
lmtest::dwtest(Comp_Medio ~ Gen, data = produt_batata)

# DIÂMETRO MÉDIO -- não passou na independência
# normalidade dos resíduos
shapiro.test(residuals(aov_diam))
# homocedasticidade das variâncias
car::leveneTest(Diam_Medio ~ Gen, data = produt_batata)
# independência dos resíduos
lmtest::dwtest(Diam_Medio ~ Gen, data = produt_batata)

# N° DE FUROS -- não passou em nada
# normalidade dos resíduos
shapiro.test(residuals(aov_furos))
# homocedasticidade das variâncias
car::leveneTest(nFuros_Medio ~ Gen, data = produt_batata)
# independência dos resíduos
lmtest::dwtest(nFuros_Medio ~ Gen, data = produt_batata)

# PESO COMERCIAL -- não passou em nada
# normalidade dos resíduos
shapiro.test(residuals(aov_pesoCom))
# homocedasticidade das variâncias
car::leveneTest(peso_Comerc ~ Gen, data = produt_batata)
# independência dos resíduos
lmtest::dwtest(peso_Comerc ~ Gen, data = produt_batata)

# PRODUTIVIDADE TOTAL -- não passou em nada
# normalidade dos resíduos
shapiro.test(residuals(aov_prod))
# homocedasticidade das variâncias
car::leveneTest(Produtividade ~ Gen, data = produt_batata)
# independência dos resíduos
lmtest::dwtest(Produtividade ~ Gen, data = produt_batata)


# Comparações múltiplas ---------------------------------------------------


# Teste de Diferenças Significativas de Tukey

# Colheita de 120 dias
HSD.test(y = aov_prod_120, trt = 'Gen', console = TRUE)

# Colheita de 150 dias
HSD.test(y = aov_prod_150, trt = 'Gen', console = TRUE)

# Colheita de 180 dias
HSD.test(y = aov_prod_180, trt = 'Gen', console = TRUE)



# Teste de Scott-Knott


#### Colheita de 120 dias ####

# COMPRIMENTO
capture.output(scottknott(y = batata120$Comp_Medio, trt = batata120$Gen,
                          DFerror = aov_comp_120$df.residual,
                          SSerror = sum(aov_comp_120$residuals^2)),
               file = "Scott_Knott_Comp120.txt")

# DIÂMETRO
capture.output(scottknott(y = batata120$Diam_Medio, trt = batata120$Gen,
                          DFerror = aov_diam_120$df.residual,
                          SSerror = sum(aov_diam_120$residuals^2)),
               file = "Scott_Knott_Diam120.txt")

# PESO COMERCIAL
capture.output(scottknott(y = batata120$peso_Comerc, trt = batata120$Gen,
                          DFerror = aov_pesoCom_120$df.residual,
                          SSerror = sum(aov_pesoCom_120$residuals^2)),
               file = "Scott_Knott_PesoCom120.txt")

# PRODUTIVIDADE TOTAL
capture.output(scottknott(y = batata120$Produtividade, trt = batata120$Gen,
                          DFerror = aov_prod_120$df.residual,
                          SSerror = sum(aov_prod_120$residuals^2)),
               file = "Scott_Knott_Produt120.txt")


#### Colheita de 150 dias ####

# COMPRIMENTO
capture.output(scottknott(y = batata150$Comp_Medio, trt = batata150$Gen,
                          DFerror = aov_comp_150$df.residual,
                          SSerror = sum(aov_comp_150$residuals^2)),
               file = "Scott_Knott_Comp150.txt")

# DIÂMETRO
capture.output(scottknott(y = batata150$Diam_Medio, trt = batata150$Gen,
                          DFerror = aov_diam_150$df.residual,
                          SSerror = sum(aov_diam_150$residuals^2)),
               file = "Scott_Knott_Diam150.txt")

# PESO COMERCIAL
capture.output(scottknott(y = batata150$peso_Comerc, trt = batata150$Gen,
                          DFerror = aov_pesoCom_150$df.residual,
                          SSerror = sum(aov_pesoCom_150$residuals^2)),
               file = "Scott_Knott_PesoCom150.txt")

# PRODUTIVIDADE TOTAL
capture.output(scottknott(y = batata150$Produtividade, trt = batata150$Gen,
                          DFerror = aov_prod_150$df.residual,
                          SSerror = sum(aov_prod_150$residuals^2)),
               file = "Scott_Knott_Produt150.txt")


#### Colheita de 180 dias ####

# COMPRIMENTO
capture.output(scottknott(y = batata180$Comp_Medio, trt = batata180$Gen,
                          DFerror = aov_comp_180$df.residual,
                          SSerror = sum(aov_comp_180$residuals^2)),
               file = "Scott_Knott_Comp180.txt")

# DIÂMETRO
capture.output(scottknott(y = batata180$Diam_Medio, trt = batata180$Gen,
                          DFerror = aov_diam_180$df.residual,
                          SSerror = sum(aov_diam_180$residuals^2)),
               file = "Scott_Knott_Diam180.txt")

# PESO COMERCIAL
capture.output(scottknott(y = batata180$peso_Comerc, trt = batata180$Gen,
                          DFerror = aov_pesoCom_180$df.residual,
                          SSerror = sum(aov_pesoCom_180$residuals^2)),
               file = "Scott_Knott_PesoCom180.txt")

# PRODUTIVIDADE TOTAL
capture.output(scottknott(y = batata180$Produtividade, trt = batata180$Gen,
                          DFerror = aov_prod_180$df.residual,
                          SSerror = sum(aov_prod_180$residuals^2)),
               file = "Scott_Knott_Produt180.txt")

