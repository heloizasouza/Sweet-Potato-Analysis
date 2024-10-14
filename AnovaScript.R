# Analysis of Variance of sewwt potato
# Heloiza de Oliveira Souza -- October 2024


# Descriptive Analysis ----------------------------------------------------


source(file = "DescriptiveAnalysis.R")


# Analysis of Variance ----------------------------------------------------


# Two-Way ANOVA ---------------------------------------------------------

# ANOVA of Harvest Productivity as a Function of Genotypes e Colheita
aov_prod <- aov(formula = Produtividade ~ Parcela+Gen*Colheita, data = produt_potato)
(summary_aov <- summary(aov_prod))
# Exporting to a .txt file
capture.output(summary_aov, file = "ANOVA_prod.txt")


# Average Length ANOVA
aov_comp <- aov(formula = Comp_Medio ~ Parcela+Gen*Colheita, data = produt_potato)
(summary_aov <- summary(aov_comp))
capture.output(summary_aov, file = "ANOVA_comp.txt")


# Average Diameter ANOVA
aov_diam <- aov(formula = Diam_Medio ~ Parcela+Gen*Colheita, data = produt_potato)
(summary_aov <- summary(aov_diam))
capture.output(summary_aov, file = "ANOVA_diam.txt")


# Number os Holls ANOVA
aov_furos <- aov(formula = nFuros_Medio ~ Parcela+Gen*Colheita, data = produt_potato)
(summary_aov <- summary(aov_furos))
capture.output(summary_aov, file = "ANOVA_nFuros.txt")


# Comercial Weight ANOVA
aov_pesoCom <- aov(formula = peso_Comerc ~ Parcela+Gen*Colheita, data = produt_potato)
(summary_aov <- summary(aov_pesoCom))
capture.output(summary_aov, file = "ANOVA_pesoCom.txt")


# Transformed Productivity ANOVA
aov_prod_T <- aov(formula = Produtiv_T ~ Parcela+Gen*Colheita, data = produt_potato)
(summary_aov <- summary(aov_prod_T))

# Transformed Comercial Weight ANOVA
aov_pesoCom_T <- aov(formula = Peso_Com_T ~ Parcela+Gen*Colheita, data = produt_potato)
(summary_aov <- summary(aov_pesoCom_T))


# analisando o genótipo dentro da colheita
aov_prod.1 <- aov(formula = Produtividade ~ Colheita/Gen, data = produt_potato)
summary(aov_prod.1)
names(coef(aov_prod.1))
summary(aov_prod.1, split = list('Colheita:Gen' = list('Gen d. Colh 120'=c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49),
                                                       'Gen d. Colh 150'=c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50),
                                                       'Gen d. Colh 180'=c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51))))

# analisando a colheita dentro do genótipo
aov_prod.2 <- aov(formula = Produtividade ~ Gen/Colheita, data = produt_potato)
summary(aov_prod.2)
names(coef(aov_prod.2))
summary(aov_prod.2, split = list('Gen:Colheita' = list('Colh d. Gen 1'=c(1,19),'Colh d. Gen 2'=c(2,20),'Colh d. Gen 3'=c(3,21),
                                                       'Colh d. Gen 4'=c(4,22),'Colh d. Gen 5'=c(5,23),'Colh d. Gen 6'=c(6,24),
                                                       'Colh d. Gen 7'=c(7,25),'Colh d. Gen 8'=c(8,26),'Colh d. Gen 9'=c(9,27),
                                                       'Colh d. Gen 10'=c(10,28),'Colh d. Gen 11'=c(11,29),'Colh d. Gen 12'=c(12,30),
                                                       'Colh d. Gen 13'=c(13,31),'Colh d. Gen 14'=c(14,32),'Colh d. Gen 15'=c(15,33),
                                                       'Colh d. Gen 16'=c(16,34),'Colh d. Gen 17'=c(17,35),'Colh d. Gen 18'=c(18,36))))



# Residuals Analysis -----------------------------------------------------


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




# Multiple Comparisons ---------------------------------------------------


#### Tukey Test ####


# teste de tukey para a colheita de 120 dias
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Colheita == '120'],
                        trt = produt_potato$Gen[produt_potato$Colheita == '120'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Colh120.txt")

# teste de tukey para a colheita de 150 dias
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Colheita == '150'],
                        trt = produt_potato$Gen[produt_potato$Colheita == '150'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Colh150.txt")

# teste de tukey para a colheita de 180 dias
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Colheita == '180'],
                        trt = produt_potato$Gen[produt_potato$Colheita == '180'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Colh180.txt")


# teste de tukey para o genótipo 1
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '1'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '1'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen1.txt")

# teste de tukey para o genótipo 2
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '2'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '2'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen2.txt")

# teste de tukey para o genótipo 3
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '3'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '3'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen3.txt")

# teste de tukey para o genótipo 4
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '4'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '4'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen4.txt")

# teste de tukey para o genótipo 5
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '5'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '5'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen5.txt")

# teste de tukey para o genótipo 6
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '6'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '6'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen6.txt")

# teste de tukey para o genótipo 7
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '7'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '7'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen7.txt")

# teste de tukey para o genótipo 8
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '8'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '8'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen8.txt")

# teste de tukey para o genótipo 9
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '9'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '9'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen9.txt")

# teste de tukey para o genótipo 10
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '10'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '10'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen10.txt")

# teste de tukey para o genótipo 11
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '11'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '1'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen11.txt")

# teste de tukey para o genótipo 12
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '12'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '12'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen12.txt")

# teste de tukey para o genótipo 13
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '13'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '13'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen13.txt")

# teste de tukey para o genótipo 14
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '14'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '14'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen14.txt")

# teste de tukey para o genótipo 15
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '15'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '15'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen15.txt")

# teste de tukey para o genótipo 16
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '16'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '16'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen16.txt")

# teste de tukey para o genótipo 17
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '17'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '17'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen17.txt")

# teste de tukey para o genótipo 18
capture.output(HSD.test(y = produt_potato$Produtividade[produt_potato$Gen == '18'],
                        trt = produt_potato$Colheita[produt_potato$Gen == '18'],
                        DFerror = aov_prod$df.residual,
                        MSerror = summary(aov_prod)[[1]][5,3],
                        console = TRUE),
               file = "Tukey_Prod_Gen18.txt")



#### Scott-Knott Test ####

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
                          DFerror = aov_pesoCom$df.residual,
                          SSerror = sum(aov_pesoCom$residuals^2)),
               file = "Scott_Knott_PesoCom.txt")

# TOTAL PRODUCTIVITY
capture.output(scottknott(y = produt_potato$Produtividade, trt = produt_potato$Gen,
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2)),
               file = "Scott_Knott_Produtiv.txt")



# teste de scott-knot para a colheita de 120 dias
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Colheita == '120'],
                          trt = produt_potato$Gen[produt_potato$Colheita == '120'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para Colheita de 120 dias'),
               file = "SK_Produt_colh120.txt")

# teste de scott-knot para a colheita de 150 dias
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Colheita == '150'],
                          trt = produt_potato$Gen[produt_potato$Colheita == '150'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para Colheita de 120 dias'),
               file = "SK_Produt_colh150.txt")

# teste de scott-knot para a colheita de 180 dias
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Colheita == '180'],
                          trt = produt_potato$Gen[produt_potato$Colheita == '180'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott paraa Colheita de 180 dias'),
               file = "SK_Produt_colh180.txt")


# teste de scott-knot para o genótipo 1
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Gen == '1'],
                          trt = produt_potato$Colheita[produt_potato$Gen == '1'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 1'),
               file = "SK_Produt_Gen1.txt")

# teste de scott-knot para o genótipo 2
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Gen == '2'],
                          trt = produt_potato$Colheita[produt_potato$Gen == '2'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 2'),
               file = "SK_Produt_Gen2.txt")

# teste de scott-knot para o genótipo 3
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Gen == '3'],
                          trt = produt_potato$Colheita[produt_potato$Gen == '3'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 3'),
               file = "SK_Produt_Gen3.txt")

# teste de scott-knot para o genótipo 4
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Gen == '4'],
                          trt = produt_potato$Colheita[produt_potato$Gen == '4'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 4'),
               file = "SK_Produt_Gen4.txt")

# teste de scott-knot para o genótipo 5
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Gen == '5'],
                          trt = produt_potato$Colheita[produt_potato$Gen == '5'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 5'),
               file = "SK_Produt_Gen5.txt")

# teste de scott-knot para o genótipo 6
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Gen == '6'],
                          trt = produt_potato$Colheita[produt_potato$Gen == '6'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 6'),
               file = "SK_Produt_Gen6.txt")

# teste de scott-knot para o genótipo 7
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Gen == '7'],
                          trt = produt_potato$Colheita[produt_potato$Gen == '7'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 7'),
               file = "SK_Produt_Gen7.txt")

# teste de scott-knot para o genótipo 8
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Gen == '8'],
                          trt = produt_potato$Colheita[produt_potato$Gen == '8'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 8'),
               file = "SK_Produt_Gen8.txt")

# teste de scott-knot para o genótipo 9
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Gen == '9'],
                          trt = produt_potato$Colheita[produt_potato$Gen == '9'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 9'),
               file = "SK_Produt_Gen9.txt")

# teste de scott-knot para o genótipo 10
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Gen == '10'],
                          trt = produt_potato$Colheita[produt_potato$Gen == '10'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 10'),
               file = "SK_Produt_Gen10.txt")

# teste de scott-knot para o genótipo 11
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Gen == '11'],
                          trt = produt_potato$Colheita[produt_potato$Gen == '1'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 11'),
               file = "SK_Produt_Gen11.txt")

# teste de scott-knot para o genótipo 12
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Gen == '12'],
                          trt = produt_potato$Colheita[produt_potato$Gen == '12'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 12'),
               file = "SK_Produt_Gen12.txt")

# teste de scott-knot para o genótipo 13
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Gen == '13'],
                          trt = produt_potato$Colheita[produt_potato$Gen == '13'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 13'),
               file = "SK_Produt_Gen13.txt")

# teste de scott-knot para o genótipo 14
capture.output(scottknott(y = produt_potato$Produtividade[produt_potato$Gen == '14'],
                          trt = produt_potato$Colheita[produt_potato$Gen == '14'],
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 14'),
               file = "SK_Produt_Gen14.txt")

# teste de scott-knot para o genótipo 15

# Filtrando os dados para remover NAs
dados_filtrados <- produt_potato[produt_potato$Gen == '15' & 
                                   !is.na(produt_potato$Produtividade) & 
                                   !is.na(produt_potato$Colheita), ]

capture.output(scottknott(y = dados_filtrados$Produtividade,
                          trt = dados_filtrados$Colheita,
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 15'),
               file = "SK_Produt_Gen15.txt")

# teste de scott-knot para o genótipo 16

# Filtrando os dados para remover NAs
dados_filtrados <- produt_potato[produt_potato$Gen == '16' & 
                                   !is.na(produt_potato$Produtividade) & 
                                   !is.na(produt_potato$Colheita), ]

capture.output(scottknott(y = dados_filtrados$Produtividade,
                          trt = dados_filtrados$Colheita,
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 16'),
               file = "SK_Produt_Gen16.txt")

# teste de scott-knot para o genótipo 17

# Filtrando os dados para remover NAs
dados_filtrados <- produt_potato[produt_potato$Gen == '17' & 
                                   !is.na(produt_potato$Produtividade) & 
                                   !is.na(produt_potato$Colheita), ]

capture.output(scottknott(y = dados_filtrados$Produtividade,
                          trt = dados_filtrados$Colheita,
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 17'),
               file = "SK_Produt_Gen17.txt")

# teste de scott-knot para o genótipo 18

# Filtrando os dados para remover NAs
dados_filtrados <- produt_potato[produt_potato$Gen == '18' & 
                                   !is.na(produt_potato$Produtividade) & 
                                   !is.na(produt_potato$Colheita), ]

capture.output(scottknott(y = dados_filtrados$Produtividade,
               trt = dados_filtrados$Colheita,
                          DFerror = aov_prod$df.residual,
                          SSerror = sum(aov_prod$residuals^2),
                          main = 'Teste de Scott-Knott para o Genótipo 18'),
               file = "SK_Produt_Gen18.txt")

