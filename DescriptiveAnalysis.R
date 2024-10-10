# Descriptive Analysis Script  
# Heloiza de Oliveira Souza -- October 2024



# Data Processing ---------------------------------------------------------

source(file = "DataProcessing.R")



# Descriptive Analysis ----------------------------------------------------

# graphics list
graphics <- list()

# BoxPlot of 120-day Harvest
(graphics[[1]] <- ggplot(data = potato120, mapping = aes(x = Gen, y = Produtividade)) +
    geom_boxplot() +
    scale_x_discrete(limits = as.character(1:18)) +
    theme_light() +
    labs(x = 'Genótipo', y = "Produtividade Total",
         title = "Produtividade dos genótipos colhidos em 120 dias") )

# BoxPlot of 150-day Harvest
(graphics[[2]] <- ggplot(data = potato150, mapping = aes(x = Gen, y = Produtividade)) +
    geom_boxplot() +
    scale_x_discrete(limits = as.character(1:18)) +
    theme_light() +
    labs(x = 'Genótipo', y = "Produtividade Total",
         title = "Produtividade dos genótipos colhidos em 150 dias"))

# BoxPlot of 180-day Harvest
(graphics[[3]] <- ggplot(data = potato180, mapping = aes(x = Gen, y = Produtividade)) +
    geom_boxplot() +
    scale_x_discrete(limits = as.character(1:18)) +
    theme_light() +
    labs(x = 'Genótipo', y = "Produtividade Total",
         title = "Produtividade dos genótipos colhidos em 180 dias")
)


# Productivity of all genotypes in the three harvests
(graphics[[4]] <- ggplot(data = produt_potato, mapping = aes(x = Gen, y = Produtividade)) + 
    geom_boxplot() + facet_grid(rows = vars(Colheita)) +
    scale_x_discrete(limits = as.character(1:18)) +
    theme_light() +
    labs(x = 'Genótipo', y = "Produtividade Total",
         title = "Produtividade total dos genótipos colhidos em 120, 150 e 180 dias")
)

# Commercial Weight of all genotypes in the three harvests
(graphics[[5]] <- ggplot(data = produt_potato, mapping = aes(x = Gen, y = peso_Comerc)) + 
    geom_boxplot() + facet_grid(rows = vars(Colheita)) +
    scale_x_discrete(limits = as.character(1:18)) +
    theme_light() +
    labs(x = 'Genótipo', y = "Peso Comercial",
         title = "Peso Comercial dos genótipos colhidos em 120, 150 e 180 dias")
)

# Average Length of all genotypes in the three harvests
(graphics[[6]] <- ggplot(data = produt_potato, mapping = aes(x = Gen, y = Comp_Medio)) + 
    geom_boxplot() + facet_grid(rows = vars(Colheita)) +
    scale_x_discrete(limits = as.character(1:18)) +
    theme_light() +
    labs(x = 'Genótipo', y = "Comprimento Médio",
         title = "Comprimento Médio dos genótipos colhidos em 120, 150 e 180 dias"))

# Average Diameter of all genotypes in the three harvests
(graphics[[7]] <- ggplot(data = produt_potato, mapping = aes(x = Gen, y = Diam_Medio)) + 
    geom_boxplot() + facet_grid(rows = vars(Colheita)) +
    scale_x_discrete(limits = as.character(1:18)) +
    theme_light() +
    labs(x = 'Genótipo', y = "Diâmetro Médio",
         title = "Diâmetro Médio dos genótipos colhidos em 120, 150 e 180 dias")
)

# average number of holes of all genotypes in the three harvests
(graphics[[8]] <- ggplot(data = produt_potato, mapping = aes(x = Gen, y = nFuros_Medio)) + 
    geom_boxplot() + facet_grid(rows = vars(Colheita)) +
    scale_x_discrete(limits = as.character(1:18)) +
    theme_light() +
    labs(x = 'Genótipo', y = "N° de Furos Médio",
         title = "N° de Furos Médio dos genótipos colhidos em 120, 150 e 180 dias")
)

# exporting the graphics
for (i in 1:length(graphics)) {
  ggsave(filename = paste0("grafico_", i, ".png"), plot = graphics[[i]], width = 6, height = 4)
}

# BarPlot of Productivity of all genotypes in the three harvests
ggplot(data = produt_potato, mapping = aes(x = Gen, y = Produtividade, fill = Parcela)) +
  geom_col() + facet_grid(rows = vars(Colheita)) +
  scale_x_discrete(limits = as.character(1:18)) +
  theme_light() +
  theme(legend.position = "top") + 
  labs(x = 'Genótipo', y = "Produtividade Total",
       title = "Produtividade Total dos genótipos colhidos em 120, 150 e 180 dias")

# DotPlot of Productivity of all genotypes in the three harvests
ggplot(data = produt_potato, mapping = aes(x = Gen, y = Produtividade, colour = Parcela)) + 
  geom_point() + facet_grid(rows = vars(Colheita)) +
  scale_x_discrete(limits = as.character(1:18)) +
  theme_light() +
  theme(legend.position = "top") + 
  labs(x = 'Genótipo', y = "Produtividade Total",
       title = "Produtividade Total dos genótipos colhidos em 120, 150 e 180 dias")
