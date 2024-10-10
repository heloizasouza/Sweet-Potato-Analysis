
# sensory evaluation analysis
# Heloiza de Oliveira Souza -- October 2024



# Data Processing ---------------------------------------------------------

source(file = "DataProcessing.R")


# Analysis ----------------------------------------------------------------


# Genótipos que os entrevistados comprariam por gênero e faixa etária
compraria <- sensorial_filled %>% 
  filter(compraria == 1) %>%  # Filtrar apenas quem respondeu "Sim"
  group_by(Gênero, faixa_etaria, genótipo) %>%
  summarise(total_compraria = n(), .groups = 'drop') %>%
  arrange(Gênero, faixa_etaria, desc(total_compraria))


# gráfico da Contagem de Compras por Espécie de Batata Doce
compraria %>% 
  filter(!is.na(faixa_etaria)) %>% 
  ggplot(aes(x = genótipo, y = total_compraria, fill = as.factor(Gênero))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ faixa_etaria) +  # Criar facetas para cada faixa etária
  theme_minimal() +
  labs(title = "Contagem de Compras por Espécie de Batata Doce",
       x = "Espécie de Batata Doce",
       y = "Total que Comprariam")


# Função para obter a característica com a maior média e a nota correspondente
caracteristica_mais_bem_avaliada <- function(df) {
  caracteristicas <- c("aparencia", "Aroma", "sabor", "textura", "doçura", "geral")
  medias <- sapply(caracteristicas, function(col) mean(df[[col]], na.rm = TRUE))
  max_index <- which.max(medias)
  data.frame(
    caracteristica = caracteristicas[max_index],
    nota = medias[max_index]
  )
}

# Resumo das características mais bem avaliadas por gênero e faixa etária
caracteristicas_avaliadas <- sensorial_filled %>%
  group_by(Gênero, faixa_etaria) %>%
  summarise(
    caracteristica_info = list(caracteristica_mais_bem_avaliada(cur_data())),
    .groups = 'drop'
  ) %>%
  unnest_wider(caracteristica_info)  # Expandir a lista em colunas separadas

# Ver os resultados
print(caracteristicas_avaliadas)


