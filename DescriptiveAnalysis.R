# Descriptive Analysis Script  
# Heloiza de Oliveira Souza -- October 2024



# Data Processing ---------------------------------------------------------

source(file = "DataProcessing.R")

# Descriptive Analysis ----------------------------------------------------

# graphics list
graphics <- list()


##### Harvest Graphs #####


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


##### Climate Graphs #####

# BarPlot of Productivity of all genotypes in the three harvests
(graphics[[9]] <- ggplot(data = produt_potato, mapping = aes(x = Gen, y = Produtividade, fill = Parcela)) +
    geom_col() + facet_grid(rows = vars(Colheita)) +
    scale_x_discrete(limits = as.character(1:18)) +
    theme_light() +
    theme(legend.position = "top") + 
    labs(x = 'Genótipo', y = "Produtividade Total",
         title = "Produtividade Total dos genótipos colhidos em 120, 150 e 180 dias")
)

# DotPlot of Productivity of all genotypes in the three harvests
(graphics[[10]] <- ggplot(data = produt_potato, mapping = aes(x = Gen, y = Produtividade, colour = Parcela)) + 
    geom_point() + facet_grid(rows = vars(Colheita)) +
    scale_x_discrete(limits = as.character(1:18)) +
    theme_light() +
    theme(legend.position = "top") + 
    labs(x = 'Genótipo', y = "Produtividade Total",
         title = "Produtividade Total dos genótipos colhidos em 120, 150 e 180 dias")
)

# precipitation line graph with label on graph
(graphics[[11]] <- ggplot(data = climate, mapping = aes(x = Data, y = `Prec.(mm)`)) + 
    geom_line() + 
    scale_x_date(date_breaks = "4 week", date_labels = "%b/%Y") + 
    geom_vline(xintercept = as.Date('2024-04-12'), colour = 'red', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-04-12') + 10, y = 75, label = '1° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) +
    geom_vline(xintercept = as.Date('2024-05-12'), colour = 'blue', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-05-12')+10, y = 75, label = '2° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) +
    geom_vline(xintercept = as.Date('2024-06-12'), colour = 'green4', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-06-12')+10, y = 75, label = '3° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) + 
    theme_light() +
    labs(title = "Gráfico de linhas da Precipitação média ao longo do período de cultivo",
         x = "Data", y = "Precipitação (mm)")
)

# line graph of global radiation with label on graph
(graphics[[12]] <- ggplot(data = climate[-c(37:52),], mapping = aes(x = Data, y = `Rad. global (MJ/m2 d)`)) + 
    geom_line(na.rm = TRUE) + 
    scale_x_date(date_breaks = "4 week", date_labels = "%b/%Y") + 
    geom_vline(xintercept = as.Date('2024-04-12'), colour = 'red', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-04-12') + 10, y = 20, label = '1° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) +
    geom_vline(xintercept = as.Date('2024-05-12'), colour = 'blue', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-05-12')+10, y = 20, label = '2° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) +
    geom_vline(xintercept = as.Date('2024-06-12'), colour = 'green4', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-06-12')+10, y = 20, label = '3° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) + 
    theme_light() +
    labs(title = "Gráfico de linhas da Radiação global ao longo do período de cultivo",
         x = "Data", y = "Radiação global (MJ/m2 d)")
)

# line graph of average temperature with label on graph
(graphics[[13]] <- ggplot(data = climate, mapping = aes(x = Data, y = `T med (0º)`)) + 
    geom_line() + 
    scale_x_date(date_breaks = "4 week", date_labels = "%b/%Y") + 
    geom_vline(xintercept = as.Date('2024-04-12'), colour = 'red', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-04-12') + 10, y = 24, label = '1° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) +
    geom_vline(xintercept = as.Date('2024-05-12'), colour = 'blue', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-05-12')+10, y = 24, label = '2° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) +
    geom_vline(xintercept = as.Date('2024-06-12'), colour = 'green4', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-06-12')+10, y = 24, label = '3° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) + 
    theme_light() +
    labs(title = "Gráfico de linhas da Temperatura média ao longo do período de cultivo",
         x = "Data", y = "Temperatura média (ºC)")
)

# Maximum temperature line graph with label on graph
(graphics[[14]] <- ggplot(data = climate, mapping = aes(x = Data, y = `T max 0º C`)) + 
    geom_line() + 
    scale_x_date(date_breaks = "4 week", date_labels = "%b/%Y") + 
    geom_vline(xintercept = as.Date('2024-04-12'), colour = 'red', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-04-12') + 10, y = 31, label = '1° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) +
    geom_vline(xintercept = as.Date('2024-05-12'), colour = 'blue', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-05-12')+10, y = 31, label = '2° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) +
    geom_vline(xintercept = as.Date('2024-06-12'), colour = 'green4', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-06-12')+10, y = 31, label = '3° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) + 
    theme_light() +
    labs(title = "Gráfico de linhas da Temperatura máxima ao longo do período de cultivo",
         x = "Data", y = "Temperatura max (ºC)")
)

# Minimum temperature line graph with label on graph
(graphics[[15]] <- ggplot(data = climate, mapping = aes(x = Data, y = `T min (0 º)`)) + 
    geom_line() + 
    scale_x_date(date_breaks = "4 week", date_labels = "%b/%Y") + 
    geom_vline(xintercept = as.Date('2024-04-12'), colour = 'red', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-04-12') + 10, y = 18.75, label = '1° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) +
    geom_vline(xintercept = as.Date('2024-05-12'), colour = 'blue', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-05-12')+10, y = 18.75, label = '2° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) +
    geom_vline(xintercept = as.Date('2024-06-12'), colour = 'green4', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-06-12')+10, y = 18.75, label = '3° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) + 
    theme_light() +
    labs(title = "Gráfico de linhas da Temperatura mínima ao longo do período de cultivo",
         x = "Data", y = "Temperatura min (ºC)")
)

# relative humidity line graph with label in the Graph
(graphics[[16]] <- ggplot(data = climate, mapping = aes(x = Data, y = `UR med (%)`)) + 
    geom_line() + 
    scale_x_date(date_breaks = "4 week", date_labels = "%b/%Y") + 
    geom_vline(xintercept = as.Date('2024-04-12'), colour = 'red', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-04-12') + 10, y = 85, label = '1° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) +
    geom_vline(xintercept = as.Date('2024-05-12'), colour = 'blue', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-05-12')+10, y = 85, label = '2° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) +
    geom_vline(xintercept = as.Date('2024-06-12'), colour = 'green4', linetype = 'dashed') +
    annotate(geom = 'label', x = as.Date('2024-06-12')+10, y = 85, label = '3° Colheita',
             colour = 'black', fill = 'white', label.size = 0.2, vjust = -0.5) + 
    theme_light() +
    labs(title = "Gráfico de linhas da Umidade Relativa média ao longo do período de cultivo",
         x = "Data", y = "UR med (%)")
)





# End Code ----------------------------------------------------------------


# exporting the graphics
for (i in 1:length(graphics)) {
  ggsave(filename = paste0("grafico_", i, ".png"), plot = graphics[[i]], width = 6, height = 4)
}