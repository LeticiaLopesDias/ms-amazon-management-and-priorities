###
# Analysis of the article "Mismatch between investment in management and 
# priorities for biodiversity conservation in Amazon protected areas"
###


### Packages 
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
  multcompView, 
  patchwork
)


### Data

data <- readxl::read_xlsx("data_sem_model.xlsx")
dplyr::glimpse(data)

unique(data$arpa)
# 0 = No ARPA support; 1 = ARPA support
unique(data$sphere)
# 0 = Federal; 1 = State
unique(data$group)
# 0 = Sustainable use; 1 = Strict protection


### Models

# First,
mod1 <- 'manag_index ~ hum_res + fin_res + arpa + group
          in_def ~ ext_def + hq_dist + group + manag_index
          hum_res ~ sphere
          fin_res ~ sphere + arpa
          ext_def ~ hq_dist
          group ~ ext_def
         
          hum_res ~~ fin_res
         '

fit <- sem(mod1, data = data)
summary(fit, standardized = T)
modindices(fit, sort = T)

semPaths(fit, what = 'std', 
         residuals = F,
         nCharNodes = 8, sizeMan = 7, 
         edge.color = "black",
         edge.label.cex = 1, 
         layoutSplit = T
)

# Test if management and inner deforestation are resulting from an external factor
# i.e. their not explained variation are correlated

mod2 <- 'manag_index ~ hum_res + fin_res + arpa + group
          in_def ~ ext_def + hq_dist + group + manag_index
          hum_res ~ sphere
          fin_res ~ sphere + arpa
          ext_def ~ hq_dist 
          group ~ ext_def
         
          hum_res ~~ fin_res
          manag_index ~~ in_def
         '

fit2 <- sem(mod2, data = data)
summary(fit2, standardized = T)


# Possibility of simplifying the model:
# Test model with management group as a random variable

mod3 <- psem(
  lme(manag_index ~ hum_res + fin_res + arpa, random = list(group = ~1), data),
  lme(in_def ~ ext_def + hq_dist + manag_index, random = list(group = ~1), data),
  lme(hum_res ~ sphere, random = list(group = ~1), data),
  lme(fin_res ~ sphere + arpa, random = list(group = ~1), data),
  lme(ext_def ~ hq_dist, random = list(group = ~1), data),
  
  hum_res %~~% fin_res
  )

summary(mod3)



# Test non linear relations

data2 <- data |> 
  mutate(
    ext_def = ext_def^2,
    hq_dist = hq_dist^2
  )

hist(data$ext_def)
hist(data2$ext_def)

mod4 <- psem(
  lme(manag_index ~ hum_res + fin_res + arpa, random = list(group = ~1), data2),
  lme(in_def ~ ext_def + hq_dist + manag_index, random = list(group = ~1), data2),
  lme(hum_res ~ sphere, random = list(group = ~1), data2),
  lme(fin_res ~ sphere + arpa, random = list(group = ~1), data2),
  lme(ext_def ~ hq_dist, random = list(group = ~1), data2),
  
  hum_res %~~% fin_res
)

summary(mod4)
# Bad fit


# Test another direction of the relationship management > deforestation
mod5 <- psem(
  lme(manag_index ~ hum_res + fin_res + arpa + in_def, random = list(group = ~1), data),
  lme(in_def ~ ext_def + hq_dist, random = list(group = ~1), data),
  lme(hum_res ~ sphere, random = list(group = ~1), data),
  lme(fin_res ~ sphere + arpa, random = list(group = ~1), data),
  lme(ext_def ~ hq_dist, random = list(group = ~1), data),
  
  hum_res %~~% fin_res
)

summary(mod5)
# Higher AIC than mod3
plot(mod5)



### Figures

rsquared(mod3)

# Creating the diagram manually with custom labels (including R² values)
graph_code <- "
  digraph SEM {
    graph [layout = dot, rankdir = LR]

    # Define nodes
    node [shape = box, fontsize = 10]
    
    # Group nodes
{ rank=same; sphere;  arpa }
{ rank=same; hq_dist; in_def }
{ rank=same; fin_res; hum_res }
    
    
    manag_index [label = 'Management index\\nR² = 0.64']  
    in_def [label = 'Inner deforestation\\nR² = 0.51']  
    ext_def [label = 'Surrounding deforestation\\nR² = 0.39'] 
    hum_res [label = 'Human resources\\nR² = 0.02']
    fin_res [label = 'Finantial resources\\nR² = 0.34']
    hq_dist [label = 'Municipal seats\\ndistance']
    sphere [label = 'Sphere']
    arpa [label = 'ARPA support']

    # Define edges
    hum_res -> manag_index [label = '0.40', penwidth = 1.4]
    fin_res -> manag_index [label = '0.35', penwidth = 1.35]
    arpa -> manag_index [label = '0.50', penwidth = 1.50]
    ext_def -> in_def [label = '0.71', penwidth = 1.71]
    hq_dist -> in_def [style=dashed]  # non-significant
    manag_index -> in_def [label = '-0.11', penwidth = 1.11]
    sphere -> hum_res [xlabel = '-0.26', penwidth = 1.26]
    sphere -> fin_res [label = '-0.79', penwidth = 1.79]
    arpa -> fin_res [label = '0.70', penwidth = 1.70]
    hq_dist -> ext_def [label = '-0.63', penwidth = 1.63]
    hum_res -> fin_res [dir=both, color=gray, label = '0.41', penwidth = 1.41] # correlation
  }
"

p <- grViz(graph_code)
svg_graph <- export_svg(p)

# Save as svg
writeLines(svg_graph, "Fig1.svg")


# Compute partial residuals

presid1 <- partialResid(in_def ~ manag_index, mod3)
plot(presid1)
head(presid1)

g1 <- 
  ggplot(presid1,
         aes(x = xresid, y = yresid)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = lm, se = F, color = "grey20") +
  labs(x = "Management index",
       y = "Inner deforestation") +
  theme_classic()


presid2 <- partialResid(in_def ~ ext_def, mod3)
plot(presid2)

g2 <- 
  ggplot(presid2,
         aes(x = xresid, y = yresid)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = lm, se = F, color = "grey20") +
  labs(x = "Surrounding deforestation",
       y = element_blank()) +
  theme_classic()

g1 + g2 + plot_annotation(tag_levels = 'A')

ggsave("Fig2.png", width = 8, height = 3.5)
