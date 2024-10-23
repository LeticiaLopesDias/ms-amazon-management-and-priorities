###
# Analysis of the article "Mismatch between investment in management and 
# priorities for biodiversity conservation in Amazon protected areas"
###

# Packages 
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  readxl,
  tidyverse,
  tidySEM,
  lavaan,
  piecewiseSEM,
  semPlot,
  nlme,
  DiagrammeR,
  DiagrammeRsvg,
  multcompView
)

data <- readxl::read_xlsx("data_sem_model.xlsx")
dplyr::glimpse(data)

# Arrumar variáveis categóricas
# Binárias: 0 e 1
# Ordinais: enumerar
# Categóricas: deixar como caracter (ver e-book)
unique(dados$arpa)
unique(dados$esfera)
unique(dados$rec_hum)
unique(dados$grupo_manejo)

dados <- dados |> 
  mutate(
    arpa = if_else(arpa == "Sem ARPA", 0, 1),
    esfera = if_else(esfera == "Federal", 0, 1),
    grupo_manejo = if_else(grupo_manejo == "US", 0, 1)
  )

# Padronizar variáveis numéricas
dados_m <- dados |>
  select(-c(desm_1km, desm_5km, ano_criacao, area_ha)) |> 
  mutate(
    # padronizar variáveis contínuas
    across(where(is.double), \(x) (x-mean(x))/sd(x))
    )

glimpse(dados_m)
# Traduzir para o inglês

dados_ing <- dados_m |> 
  rename(
    sphere = esfera,
    group = grupo_manejo,
    manag_index = indimapa,
    hq_dist = dist_sedes,
    ext_def = desm_10km,
    in_def = desm_interno,
    fin_res = rec_fin,
    hum_res = rec_hum
  )
glimpse(dados_ing)
writexl::write_xlsx(dados_ing, "data_sem_model.xlsx")
# Para o artigo, fazer script em inglês

mod1 <- 'indimapa ~ rec_hum + rec_fin + arpa + grupo_manejo + desm_interno
          desm_interno ~ desm_10km + dist_sedes + grupo_manejo + indimapa
          rec_hum ~ esfera
          rec_fin ~ esfera + arpa
          desm_10km ~ dist_sedes + desm_interno
          grupo_manejo ~ desm_10km
         
          rec_hum ~~ rec_fin
         '

fit <- sem(mod1, data = dados_m)
summary(fit, standardized = T)
modindices(fit, sort = T)


semPaths(fit, what = 'std', 
         residuals = F,
         nCharNodes = 8, sizeMan = 7, 
         edge.color = "black",
         edge.label.cex = 1, 
         layoutSplit = T
)


# Possibilidade de simplificar o modelo:
# Testar modelo com grupo de manejo como variável aleatória

mod2 <- psem(
  lme(indimapa ~ rec_hum + rec_fin + arpa, random = list(grupo_manejo = ~1), dados_m),
  lme(desm_interno ~ desm_10km + dist_sedes + indimapa, random = list(grupo_manejo = ~1), dados_m),
  lme(rec_hum ~ esfera, random = list(grupo_manejo = ~1), dados_m),
  lme(rec_fin ~ esfera + arpa, random = list(grupo_manejo = ~1), dados_m),
  lme(desm_10km ~ dist_sedes, random = list(grupo_manejo = ~1), dados_m),
  
  rec_hum %~~% rec_fin
  )

summary(mod2)
# Sem as contínuas padronizadas, o modelo não converge. Por que será?
# Testar com variáveis de distância ^2

dados_m2 <- dados_m |> 
  mutate(
    desm_10km = desm_10km^2,
    dist_sedes = dist_sedes^2
  )

hist(dados_m$desm_10km)
hist(dados_m2$desm_10km)

mod3 <- psem(
  lme(indimapa ~ rec_hum + rec_fin + arpa, random = list(grupo_manejo = ~1), dados_m2),
  lme(desm_interno ~ desm_10km + dist_sedes + indimapa, random = list(grupo_manejo = ~1), dados_m2),
  lme(rec_hum ~ esfera, random = list(grupo_manejo = ~1), dados_m2),
  lme(rec_fin ~ esfera + arpa, random = list(grupo_manejo = ~1), dados_m2),
  lme(desm_10km ~ dist_sedes, random = list(grupo_manejo = ~1), dados_m2),
  
  rec_hum %~~% rec_fin
)

summary(mod3)
# mod2 teve melhor ajuste

# Compute partial residuals of y ~ x1
# Use partialResid
presid1 <- partialResid(desm_interno ~ indimapa, mod2)
plot(presid1)
head(presid1)

ggplot(presid1,
       aes(x = xresid, y = yresid)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = lm, se = F, color = "grey20") +
  labs(x = "indimapa",
       y = "inner deforestation") +
  theme_classic()

presid2 <- partialResid(desm_interno ~ desm_10km, mod2)
plot(presid2)

ggplot(presid2,
       aes(x = xresid, y = yresid)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = lm, se = F, color = "grey20") +
  labs(x = "surrounding deforestation",
       y = "inner deforestation") +
  theme_classic()


# Adicionar R2 no modelo
rsquared(mod2)

# Criando o diagrama manualmente com os rótulos personalizados (incluindo os valores de R²)
graph_code <- "
  digraph SEM {
    graph [layout = dot, rankdir = LR]

    # Definir nós
    node [shape = box, fontsize = 10]
    
    # Agrupar nós no mesmo nível
{ rank=same; esfera;  arpa }
{ rank=same; dist_sedes; desm_interno }
{ rank=same; rec_fin; rec_hum }
    
    
    indimapa [label = 'Management index\\nR² = 0.62']  
    desm_interno [label = 'Inner deforestation\\nR² = 0.51']  
    desm_10km [label = 'Surrounding deforestation\\nR² = 0.39'] 
    rec_hum [label = 'Human resources\\nR² = 0.02']
    rec_fin [label = 'Finantial resources\\nR² = 0.34']
    dist_sedes [label = 'Municipal seats\\ndistance']
    esfera [label = 'Sphere']
    arpa [label = 'ARPA support']

    # Definir arestas (relações entre variáveis)
    rec_hum -> indimapa [label = '0.40', penwidth = 1.4]
    rec_fin -> indimapa [label = '0.36', penwidth = 1.36]
    arpa -> indimapa [label = '0.29', penwidth = 1.29]
    desm_10km -> desm_interno [label = '0.67', penwidth = 1.67]
    dist_sedes -> desm_interno [style=dashed]  # tracejada para não significativa
    indimapa -> desm_interno [label = '-0.11', penwidth = 1.11]
    esfera -> rec_hum [xlabel = '-0.13', penwidth = 1.13]
    esfera -> rec_fin [label = '-0.40', penwidth = 1.40]
    arpa -> rec_fin [label = '0.35', penwidth = 1.35]
    dist_sedes -> desm_10km [label = '-0.63', penwidth = 1.63]
    rec_hum -> rec_fin [dir=both, color=gray, label = '0.41', penwidth = 1.41]
  }
"

# Renderizando o gráfico
p <- grViz(graph_code)
svg_graph <- export_svg(p)

# Salvar o arquivo SVG
writeLines(svg_graph, "diagrama_sem.svg")



# Testar outro sentido da relação efetividade > desmatamento
mod3 <- psem(
  lme(indimapa ~ rec_hum + rec_fin + arpa + desm_10km, random = list(grupo_manejo = ~1), dados_m),
  lme(desm_interno ~ desm_10km + dist_sedes, random = list(grupo_manejo = ~1), dados_m),
  lme(rec_hum ~ esfera, random = list(grupo_manejo = ~1), dados_m),
  lme(rec_fin ~ esfera + arpa, random = list(grupo_manejo = ~1), dados_m),
  lme(desm_10km ~ dist_sedes, random = list(grupo_manejo = ~1), dados_m),
  
  rec_hum %~~% rec_fin
)

plot(mod3)
summary(mod3)
# Não teve ajuste
