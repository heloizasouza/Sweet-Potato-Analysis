# Analysis of Variance of sewwt potato
# Heloiza de Oliveira Souza -- October 2024


# Descriptive Analysis ----------------------------------------------------


source(file = "DescriptiveAnalysis.R")


# ANOVA 120-day -------------------------------------------------------------------


# ANOVA of Harvest Productivity as a Function of Genotypes
aov_prod_120 <- aov(formula = Produtividade ~ Parcela+Gen, data = potato120)
(summary_aov120 <- summary(aov_prod_120))
# Exporting to a .txt file
capture.output(summary_aov120, file = "teste_F_prod_120.txt")


# Average Length ANOVA
aov_comp_120 <- aov(formula = Comp_Medio ~ Parcela+Gen, data = potato120)
(summary_aov120 <- summary(aov_comp_120))
capture.output(summary_aov120, file = "teste_F_comp_120.txt")


# Average Diameter ANOVA
aov_diam_120 <- aov(formula = Diam_Medio ~ Parcela+Gen, data = potato120)
(summary_aov120 <- summary(aov_diam_120))
capture.output(summary_aov120, file = "teste_F_diam_120.txt")


# Number os Holls ANOVA
aov_furos_120 <- aov(formula = nFuros_Medio ~ Parcela+Gen, data = potato120)
(summary_aov120 <- summary(aov_furos_120))
capture.output(summary_aov120, file = "teste_F_nFuros_120.txt")


# Comercial Weight ANOVA
aov_pesoCom_120 <- aov(formula = peso_Comerc ~ Parcela+Gen, data = potato120)
(summary_aov120 <- summary(aov_pesoCom_120))
capture.output(summary_aov120, file = "teste_F_pesoCom_120.txt")




# ANOVA 150-day --------------------------------------------


# ANOVA of Harvest Productivity as a Function of Genotypes
aov_prod_150 <- aov(formula = Produtividade ~ Parcela+Gen, data = potato150)
(summary_aov150 <- summary(aov_prod_150))
# Exporting to a .txt file
capture.output(summary_aov150, file = "teste_F_prod_150.txt")


# Average Length ANOVA
aov_comp_150 <- aov(formula = Comp_Medio ~ Parcela+Gen, data = potato150)
(summary_aov150 <- summary(aov_comp_150))
capture.output(summary_aov150, file = "teste_F_comp_150.txt")


# Average Diameter ANOVA
aov_diam_150 <- aov(formula = Diam_Medio ~ Parcela+Gen, data = potato150)
(summary_aov150 <- summary(aov_diam_150))
capture.output(summary_aov150, file = "teste_F_diam_150.txt")


# Number os Holls ANOVA
aov_furos_150 <- aov(formula = nFuros_Medio ~ Parcela+Gen, data = potato150)
(summary_aov150 <- summary(aov_furos_150))
capture.output(summary_aov150, file = "teste_F_nFuros_150.txt")


# Comercial Weight ANOVA
aov_pesoCom_150 <- aov(formula = peso_Comerc ~ Parcela+Gen, data = potato150)
(summary_aov150 <- summary(aov_pesoCom_150))
capture.output(summary_aov150, file = "teste_F_pesoCom_150.txt")




# ANOVA 180-day ----------------------------------------------------------


# ANOVA of Harvest Productivity as a Function of Genotypes
aov_prod_180 <- aov(formula = Produtividade ~ Parcela+Gen, data = potato180)
(summary_aov180 <- summary(aov_prod_180))
# Exporting to a .txt file
capture.output(summary_aov180, file = "teste_F_prod_180.txt")


# Average Length ANOVA
aov_comp_180 <- aov(formula = Comp_Medio ~ Parcela+Gen, data = potato180)
(summary_aov180 <- summary(aov_comp_180))
capture.output(summary_aov180, file = "teste_F_comp_180.txt")


# Average Diameter ANOVA
aov_diam_180 <- aov(formula = Diam_Medio ~ Parcela+Gen, data = potato180)
(summary_aov180 <- summary(aov_diam_180))
capture.output(summary_aov180, file = "teste_F_diam_180.txt")


# Number os Holls ANOVA
aov_furos_180 <- aov(formula = nFuros_Medio ~ Parcela+Gen, data = potato180)
(summary_aov180 <- summary(aov_furos_180))
capture.output(summary_aov180, file = "teste_F_nFuros_180.txt")


# Comercial Weight ANOVA
aov_pesoCom_180 <- aov(formula = peso_Comerc ~ Parcela+Gen, data = potato180)
(summary_aov180 <- summary(aov_pesoCom_180))
capture.output(summary_aov180, file = "teste_F_pesoCom_180.txt")




# Two-Way ANOVA ---------------------------------------------------------

# ANOVA of Harvest Productivity as a Function of Genotypes e Colheita
aov_prod <- aov(formula = Produtividade ~ Parcela+Gen*Colheita, data = produt_potato)
(summary_aov <- summary(aov_prod))
# Exporting to a .txt file
capture.output(summary_aov, file = "teste_F_prod.txt")


# Average Length ANOVA
aov_comp <- aov(formula = Comp_Medio ~ Parcela+Gen*Colheita, data = produt_potato)
(summary_aov <- summary(aov_comp))
capture.output(summary_aov, file = "teste_F_comp.txt")


# Average Diameter ANOVA
aov_diam <- aov(formula = Diam_Medio ~ Parcela+Gen*Colheita, data = produt_potato)
(summary_aov <- summary(aov_diam))
capture.output(summary_aov, file = "teste_F_diam.txt")


# Number os Holls ANOVA
aov_furos <- aov(formula = nFuros_Medio ~ Parcela+Gen*Colheita, data = produt_potato)
(summary_aov <- summary(aov_furos))
capture.output(summary_aov, file = "teste_F_nFuros.txt")


# Comercial Weight ANOVA
aov_pesoCom <- aov(formula = peso_Comerc ~ Parcela+Gen*Colheita, data = produt_potato)
(summary_aov <- summary(aov_pesoCom))
capture.output(summary_aov, file = "teste_F_pesoCom.txt")


# Transformed Productivity ANOVA
aov_prod_T <- aov(formula = Produtiv_T ~ Parcela+Gen*Colheita, data = produt_potato)
(summary_aov <- summary(aov_prod_T))

# Transformed Comercial Weight ANOVA
aov_pesoCom_T <- aov(formula = Peso_Com_T ~ Parcela+Gen*Colheita, data = produt_potato)
(summary_aov <- summary(aov_pesoCom_T))


# ANOVA DO GENÓTIPO NA COLHEITA?
aov_prod_Int <- aov(formula = Produtividade ~ Parcela + Colheita/Gen, data = produt_potato)
summary(aov_prod_Int)




# Residuals Analysis -----------------------------------------------------


#### 120-day Harvest ####

# AVERAGE LENGTH -- passou na normalidade com transformação
# normality of residuals
shapiro.test(residuals(aov_comp_120))
# homoscedasticity of variances
car::leveneTest(Comp_Medio ~ Gen, data = potato120)
# residuals independence
lmtest::dwtest(Comp_Medio ~ Gen, data = potato120)

# AVERAGE DIAMETER -- passou
# normality of residuals
shapiro.test(residuals(aov_diam_120))
# homoscedasticity of variances
car::leveneTest(Diam_Medio ~ Gen, data = potato120)
# residuals independence
lmtest::dwtest(Diam_Medio ~ Gen, data = potato120)

# NUMBER OF HOLLS -- não passou na normalidade
# normality of residuals
shapiro.test(residuals(aov_furos_120))
# homoscedasticity of variances
car::leveneTest(nFuros_Medio ~ Gen, data = potato120)
# residuals independence
lmtest::dwtest(nFuros_Medio ~ Gen, data = potato120)

# COMERCIAL WEIGTH -- passou
# normality of residuals
shapiro.test(residuals(aov_pesoCom_120))
# homoscedasticity of variances
car::leveneTest(peso_Comerc ~ Gen, data = potato120)
# residuals independence
lmtest::dwtest(peso_Comerc ~ Gen, data = potato120)

# TOTAL PRODUCTIVITY -- passou
# normality of residuals
shapiro.test(residuals(aov_prod_120))
# homoscedasticity of variances
car::leveneTest(Produtividade ~ Gen, data = potato120)
# residuals independence
lmtest::dwtest(Produtividade ~ Gen, data = potato120)


#### 150-day Harvest ####

# AVERAGE LENGTH -- passou
# normality of residuals
shapiro.test(residuals(aov_comp_150))
# homoscedasticity of variances
car::leveneTest(Comp_Medio ~ Gen, data = potato150)
# residuals independence
lmtest::dwtest(Comp_Medio ~ Gen, data = potato150)

# AVERAGE DIAMETER -- passou
# normality of residuals
shapiro.test(residuals(aov_diam_150))
# homoscedasticity of variances
car::leveneTest(Diam_Medio ~ Gen, data = potato150)
# residuals independence
lmtest::dwtest(Diam_Medio ~ Gen, data = potato150)

# NUMBER OF HOLLS -- não passou na normalidade
# normality of residuals
shapiro.test(residuals(aov_furos_150))
# homoscedasticity of variances
car::leveneTest(nFuros_Medio ~ Gen, data = potato150)
# residuals independence
lmtest::dwtest(nFuros_Medio ~ Gen, data = potato150)

# COMERCIAL WEIGTH -- passou
# normality of residuals
shapiro.test(residuals(aov_pesoCom_150))
# homoscedasticity of variances
car::leveneTest(peso_Comerc ~ Gen, data = potato150)
# residuals independence
lmtest::dwtest(peso_Comerc ~ Gen, data = potato150)

# TOTAL PRODUCTIVITY -- passou
# normality of residuals
shapiro.test(residuals(aov_prod_150))
# homoscedasticity of variances
car::leveneTest(Produtividade ~ Gen, data = potato150)
# residuals independence
lmtest::dwtest(Produtividade ~ Gen, data = potato150)


#### 180-day Harvest ####

# AVERAGE LENGTH -- passou
# normality of residuals
shapiro.test(residuals(aov_comp_180))
# homoscedasticity of variances
car::leveneTest(Comp_Medio ~ Gen, data = potato180)
# residuals independence
lmtest::dwtest(Comp_Medio ~ Gen, data = potato180)

# AVERAGE DIAMETER -- passou
# normality of residuals
shapiro.test(residuals(aov_diam_180))
# homoscedasticity of variances
car::leveneTest(Diam_Medio ~ Gen, data = potato180)
# residuals independence
lmtest::dwtest(Diam_Medio ~ Gen, data = potato180)

# NUMBER OF HOLLS -- não passou na normalidade
# normality of residuals
shapiro.test(residuals(aov_furos_180))
# homoscedasticity of variances
car::leveneTest(nFuros_Medio ~ Gen, data = potato180)
# residuals independence
lmtest::dwtest(nFuros_Medio ~ Gen, data = potato180)

# COMERCIAL WEIGTH -- passou
# normality of residuals
shapiro.test(residuals(aov_pesoCom_180))
# homoscedasticity of variances
car::leveneTest(peso_Comerc ~ Gen, data = potato180)
# residuals independence
lmtest::dwtest(peso_Comerc ~ Gen, data = potato180)

# TOTAL PRODUCTIVITY -- passou
# normality of residuals
shapiro.test(residuals(aov_prod_180))
# homoscedasticity of variances
car::leveneTest(Produtividade ~ Gen, data = potato180)
# residuals independence
lmtest::dwtest(Produtividade ~ Gen, data = potato180)


#### The Three Harvests ####

# AVERAGE LENGTH -- passou em tudo
# normality of residuals
shapiro.test(residuals(aov_comp))
# homoscedasticity of variances
car::leveneTest(Comp_Medio ~ Gen*Colheita, data = produt_potato)

# AVERAGE DIAMETER -- passou em tudo
# normality of residuals
shapiro.test(residuals(aov_diam))
# homoscedasticity of variances
car::leveneTest(Diam_Medio ~ Gen*Colheita, data = produt_potato)

# NUMBER OF HOLLS -- não passou na normalidade
# normality of residuals
shapiro.test(residuals(aov_furos))
# homoscedasticity of variances
car::leveneTest(nFuros_Medio ~ Gen*Colheita, data = produt_potato)

# COMERCIAL WEIGTH -- não passou na normalidade
# normality of residuals
shapiro.test(residuals(aov_pesoCom))
# homoscedasticity of variances
car::leveneTest(peso_Comerc ~ Gen*Colheita, data = produt_potato)

# TOTAL PRODUCTIVITY -- não passou na normalidade
# normality of residuals
shapiro.test(residuals(aov_prod))
# homoscedasticity of variances
car::leveneTest(Produtividade ~ Gen*Colheita, data = produt_potato)

# TRANSFORMED TOTAL PRODUCTIVITY -- passou em tudo
shapiro.test(residuals(aov_prod_T))
car::leveneTest(Produtiv_T ~ Gen*Colheita, data = produt_potato)

# TRANSFORMED COMERCIAL WEIGTH -- passou em tudo
shapiro.test(residuals(aov_pesoCom_T))
car::leveneTest(Peso_Com_T ~ Gen*Colheita, data = produt_potato)




# Comparações múltiplas ---------------------------------------------------


# Scott-Knott Test


#### 120-day Harvest ####

# AVERAGE LENGTH
capture.output(scottknott(y = potato120$Comp_Medio, trt = potato120$Gen,
                          DFerror = aov_comp_120$df.residual,
                          SSerror = sum(aov_comp_120$residuals^2)),
               file = "Scott_Knott_Comp120.txt")

# AVERAGE DIAMETER
capture.output(scottknott(y = potato120$Diam_Medio, trt = potato120$Gen,
                          DFerror = aov_diam_120$df.residual,
                          SSerror = sum(aov_diam_120$residuals^2)),
               file = "Scott_Knott_Diam120.txt")

# COMERCIAL WEIGTH
capture.output(scottknott(y = potato120$peso_Comerc, trt = potato120$Gen,
                          DFerror = aov_pesoCom_120$df.residual,
                          SSerror = sum(aov_pesoCom_120$residuals^2)),
               file = "Scott_Knott_PesoCom120.txt")

# TOTAL PRODUCTIVITY
capture.output(scottknott(y = potato120$Produtividade, trt = potato120$Gen,
                          DFerror = aov_prod_120$df.residual,
                          SSerror = sum(aov_prod_120$residuals^2)),
               file = "Scott_Knott_Produt120.txt")


#### 150-day Harvest ####

# AVERAGE LENGTH
capture.output(scottknott(y = potato150$Comp_Medio, trt = potato150$Gen,
                          DFerror = aov_comp_150$df.residual,
                          SSerror = sum(aov_comp_150$residuals^2)),
               file = "Scott_Knott_Comp150.txt")

# AVERAGE DIAMETER
capture.output(scottknott(y = potato150$Diam_Medio, trt = potato150$Gen,
                          DFerror = aov_diam_150$df.residual,
                          SSerror = sum(aov_diam_150$residuals^2)),
               file = "Scott_Knott_Diam150.txt")

# COMERCIAL WEIGTH
capture.output(scottknott(y = potato150$peso_Comerc, trt = potato150$Gen,
                          DFerror = aov_pesoCom_150$df.residual,
                          SSerror = sum(aov_pesoCom_150$residuals^2)),
               file = "Scott_Knott_PesoCom150.txt")

# TOTAL PRODUCTIVITY
capture.output(scottknott(y = potato150$Produtividade, trt = potato150$Gen,
                          DFerror = aov_prod_150$df.residual,
                          SSerror = sum(aov_prod_150$residuals^2)),
               file = "Scott_Knott_Produt150.txt")


#### 180-day Harvest ####

# AVERAGE LENGTH
capture.output(scottknott(y = potato180$Comp_Medio, trt = potato180$Gen,
                          DFerror = aov_comp_180$df.residual,
                          SSerror = sum(aov_comp_180$residuals^2)),
               file = "Scott_Knott_Comp180.txt")

# AVERAGE DIAMETER
capture.output(scottknott(y = potato180$Diam_Medio, trt = potato180$Gen,
                          DFerror = aov_diam_180$df.residual,
                          SSerror = sum(aov_diam_180$residuals^2)),
               file = "Scott_Knott_Diam180.txt")

# COMERCIAL WEIGTH
capture.output(scottknott(y = potato180$peso_Comerc, trt = potato180$Gen,
                          DFerror = aov_pesoCom_180$df.residual,
                          SSerror = sum(aov_pesoCom_180$residuals^2)),
               file = "Scott_Knott_PesoCom180.txt")

# TOTAL PRODUCTIVITY
capture.output(scottknott(y = potato180$Produtividade, trt = potato180$Gen,
                          DFerror = aov_prod_180$df.residual,
                          SSerror = sum(aov_prod_180$residuals^2)),
               file = "Scott_Knott_Produt180.txt")


#### The Three Harvests ####

# AVERAGE LENGTH
capture.output(scottknott(y = produt_potato$Comp_Medio, trt = produt_potato$Gen,
                          DFerror = aov_comp$df.residual,
                          SSerror = sum(aov_comp$residuals^2)),
               file = "Scott_Knott_Comp.txt")

# AVERAGE DIAMETER
capture.output(scottknott(y = produt_potato$Diam_Medio, trt = produt_potato$Gen,
                          DFerror = aov_diam$df.residual,
                          SSerror = sum(aov_diam$residuals^2)),
               file = "Scott_Knott_Diam.txt")

# COMERCIAL WEIGTH
capture.output(scottknott(y = produt_potato$peso_Comerc, trt = produt_potato$Gen,
                          DFerror = aov_pesoCom_T$df.residual,
                          SSerror = sum(aov_pesoCom_T$residuals^2)),
               file = "Scott_Knott_PesoComT.txt")

# TOTAL PRODUCTIVITY
capture.output(scottknott(y = produt_potato$Produtividade, trt = produt_potato$Gen,
                          DFerror = aov_prod_T$df.residual,
                          SSerror = sum(aov_prod_T$residuals^2)),
               file = "Scott_Knott_ProdutivT.txt")


# TOTAL PRODUCTIVITY NA 120-day
scottknott(y = produt_potato$Produtividade[produt_potato$Colheita == "120"],
           trt = produt_potato$Gen[produt_potato$Colheita == "120"],
           DFerror = aov_prod_T$df.residual,
           SSerror = sum(aov_prod_T$residuals^2))