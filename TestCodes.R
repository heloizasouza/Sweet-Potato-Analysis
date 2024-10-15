# test codes
# Heloiza de Oliveira Souza -- October 2024



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





# Residuals Analysis ------------------------------------------------------

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



# Multiple Comparisons ----------------------------------------------------

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



