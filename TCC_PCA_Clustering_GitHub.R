
# Análise Fatorial por Componentes Principais (PCA)

# Curso: MBA DSA (USP ESALQ)
# Marcelo A. dos Santos Jr
# Biólogo, Mestre em Ecologia,
# Doutorando em Ciência do Sistema Terrestre,
# MBA em Data Science & Analytics e 
# Especialista em Geoprocessamento.
# Lattes: http://lattes.cnpq.br/0538254452248765
# ORCID: https://orcid.org/0000-0002-5325-8203
# Researchgate: https://www.researchgate.net/profile/Marcelo-Santos-Junior
# https://github.com/brasabio/TCC_MBA_DSA.git
# 24/04/2024

# Pacotes necessários
pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "PerformanceAnalytics", #função 'chart.Correlation' para plotagem
             "psych", #elaboração da fatorial e estatísticas
             "ltm", #determinação do alpha de Cronbach pela função 'cronbach.alpha'
             "Hmisc", # matriz de correlações com p-valor
             "readxl","factoextra","sp","tmap") # leitura de dados em Excel

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Selecionar diretório
setwd("C:/#GIS/Projetos/R")

# carregar base de dados final
load("TCC_PCA_Cluster_30_03_2024")

# salvar projeto
save.image(file="TCC_PCA_Cluster_30_03_2024")



# Carregamento da base de dados
segmenta <- read_excel("C:/#GIS/Projetos/R/MRDSA_variaveis.XLSX",sheet = "Tab_Final")


# Separar as variáveis quanto à componente do risco
# A = ameaça; E = exposição; S = Sensibilidade; C = Capacidade Adaptativa/Resiliência
segmenta_quantiE0 <- segmenta[,c(52,53,54,55,56,57,58)]
segmenta_quantiS0 <- segmenta[,c(59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82)]
segmenta_quantiC0 <- segmenta[,c(83,84,85,86,87,88,89,90,91,92,93,94,95)]
segmenta_quantiA0 <- segmenta[,c(96,98,99,101,102,104,105,107,108,110,111,113,116,119,122,125)]

# Padronizar as variáveis por meio do método ZScore
segmenta_quantiA <- as.data.frame(scale(segmenta_quantiA0))
segmenta_quantiE <- as.data.frame(scale(segmenta_quantiE0))
segmenta_quantiS <- as.data.frame(scale(segmenta_quantiS0))
segmenta_quantiC <- as.data.frame(scale(segmenta_quantiC0))

round(mean(segmenta_quantiE$IS01_1), 3)
round(mean(segmenta_quantiE$IS01_2), 3)
round(mean(segmenta_quantiE$IS01_3), 3)
round(mean(segmenta_quantiE$IS02), 3)
round(mean(segmenta_quantiE$IS03_1), 3)
round(mean(segmenta_quantiE$IS03_2), 3)
round(mean(segmenta_quantiE$IS03_3), 3)

round(sd(segmenta_quantiE$IS01_1), 3)
round(sd(segmenta_quantiE$IS01_2), 3)
round(sd(segmenta_quantiE$IS01_3), 3)
round(sd(segmenta_quantiE$IS02), 3)
round(sd(segmenta_quantiE$IS03_1), 3)
round(sd(segmenta_quantiE$IS03_2), 3)
round(sd(segmenta_quantiE$IS03_3), 3)

round(mean(segmenta_quantiS$IS04), 3)
round(mean(segmenta_quantiS$IS05), 3)
round(mean(segmenta_quantiS$IS06), 3)
round(mean(segmenta_quantiS$IS07), 3)
round(mean(segmenta_quantiS$IS08), 3)
round(mean(segmenta_quantiS$IS09), 3)
round(mean(segmenta_quantiS$IS10), 3)
round(mean(segmenta_quantiS$IS11), 3)
round(mean(segmenta_quantiS$IS12), 3)
round(mean(segmenta_quantiS$IS13), 3)
round(mean(segmenta_quantiS$IS14), 3)
round(mean(segmenta_quantiS$IS15), 3)
round(mean(segmenta_quantiS$IS16), 3)
round(mean(segmenta_quantiS$IS17), 3)
round(mean(segmenta_quantiS$IS18), 3)
round(mean(segmenta_quantiS$IS19), 3)
round(mean(segmenta_quantiS$IS20), 3)
round(mean(segmenta_quantiS$IS21), 3)
round(mean(segmenta_quantiS$IS22), 3)
round(mean(segmenta_quantiS$IS23), 3)
round(mean(segmenta_quantiS$IS24), 3)
round(mean(segmenta_quantiS$IS25), 3)
round(mean(segmenta_quantiS$IS26), 3)
round(mean(segmenta_quantiS$IS27), 3)

round(sd(segmenta_quantiS$IS04), 3)
round(sd(segmenta_quantiS$IS05), 3)
round(sd(segmenta_quantiS$IS06), 3)
round(sd(segmenta_quantiS$IS07), 3)
round(sd(segmenta_quantiS$IS08), 3)
round(sd(segmenta_quantiS$IS09), 3)
round(sd(segmenta_quantiS$IS10), 3)
round(sd(segmenta_quantiS$IS11), 3)
round(sd(segmenta_quantiS$IS12), 3)
round(sd(segmenta_quantiS$IS13), 3)
round(sd(segmenta_quantiS$IS14), 3)
round(sd(segmenta_quantiS$IS15), 3)
round(sd(segmenta_quantiS$IS16), 3)
round(sd(segmenta_quantiS$IS17), 3)
round(sd(segmenta_quantiS$IS18), 3)
round(sd(segmenta_quantiS$IS19), 3)
round(sd(segmenta_quantiS$IS20), 3)
round(sd(segmenta_quantiS$IS21), 3)
round(sd(segmenta_quantiS$IS22), 3)
round(sd(segmenta_quantiS$IS23), 3)
round(sd(segmenta_quantiS$IS24), 3)
round(sd(segmenta_quantiS$IS25), 3)
round(sd(segmenta_quantiS$IS26), 3)
round(sd(segmenta_quantiS$IS27), 3)

round(mean(segmenta_quantiC$IS28), 3)
round(mean(segmenta_quantiC$IS29), 3)
round(mean(segmenta_quantiC$IS30), 3)
round(mean(segmenta_quantiC$IS31), 3)
round(mean(segmenta_quantiC$IS32), 3)
round(mean(segmenta_quantiC$IS33), 3)
round(mean(segmenta_quantiC$IS34), 3)
round(mean(segmenta_quantiC$IS35), 3)
round(mean(segmenta_quantiC$IS36), 3)
round(mean(segmenta_quantiC$IS37), 3)
round(mean(segmenta_quantiC$IS38), 3)
round(mean(segmenta_quantiC$IS39), 3)
round(mean(segmenta_quantiC$IS40), 3)

round(sd(segmenta_quantiC$IS28), 3)
round(sd(segmenta_quantiC$IS29), 3)
round(sd(segmenta_quantiC$IS30), 3)
round(sd(segmenta_quantiC$IS31), 3)
round(sd(segmenta_quantiC$IS32), 3)
round(sd(segmenta_quantiC$IS33), 3)
round(sd(segmenta_quantiC$IS34), 3)
round(sd(segmenta_quantiC$IS35), 3)
round(sd(segmenta_quantiC$IS36), 3)
round(sd(segmenta_quantiC$IS37), 3)
round(sd(segmenta_quantiC$IS38), 3)
round(sd(segmenta_quantiC$IS39), 3)
round(sd(segmenta_quantiC$IS40), 3)

round(mean(segmenta_quantiA$IS41_1), 3)
round(mean(segmenta_quantiA$IS41_3), 3)
round(mean(segmenta_quantiA$IS42_1), 3)
round(mean(segmenta_quantiA$IS42_3), 3)
round(mean(segmenta_quantiA$IS43_1), 3)
round(mean(segmenta_quantiA$IS43_3), 3)
round(mean(segmenta_quantiA$IS44_1), 3)
round(mean(segmenta_quantiA$IS44_3), 3)
round(mean(segmenta_quantiA$IS45_1), 3)
round(mean(segmenta_quantiA$IS45_3), 3)
round(mean(segmenta_quantiA$IS46_1), 3)
round(mean(segmenta_quantiA$IS46_3), 3)
round(mean(segmenta_quantiA$IS47_3), 3)
round(mean(segmenta_quantiA$IS48_3), 3)
round(mean(segmenta_quantiA$IS49_3), 3)
round(mean(segmenta_quantiA$IS50_3), 3)

round(sd(segmenta_quantiA$IS41_1), 3)
round(sd(segmenta_quantiA$IS41_3), 3)
round(sd(segmenta_quantiA$IS42_1), 3)
round(sd(segmenta_quantiA$IS42_3), 3)
round(sd(segmenta_quantiA$IS43_1), 3)
round(sd(segmenta_quantiA$IS43_3), 3)
round(sd(segmenta_quantiA$IS44_1), 3)
round(sd(segmenta_quantiA$IS44_3), 3)
round(sd(segmenta_quantiA$IS45_1), 3)
round(sd(segmenta_quantiA$IS45_3), 3)
round(sd(segmenta_quantiA$IS46_1), 3)
round(sd(segmenta_quantiA$IS46_3), 3)
round(sd(segmenta_quantiA$IS47_3), 3)
round(sd(segmenta_quantiA$IS48_3), 3)
round(sd(segmenta_quantiA$IS49_3), 3)
round(sd(segmenta_quantiA$IS50_3), 3)

# Estatísticas descritivas dos dados
summary(segmenta_quantiA)
summary(segmenta_quantiE)
summary(segmenta_quantiS)
summary(segmenta_quantiC)

# Estabelecendo uma matriz de correlações de Pearson
rhoA <- cor(segmenta_quantiA)
rhoE <- cor(segmenta_quantiE)
rhoS <- cor(segmenta_quantiS)
rhoC <- cor(segmenta_quantiC)

# Elaborando um mapa de calor das correlações
segmenta_quantiA %>% 
  cor() %>% 
  melt() %>% 
  rename(Correlação = value) %>%
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
  geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 2)),
            size = 3) +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

segmenta_quantiE %>% 
  cor() %>% 
  melt() %>% 
  rename(Correlação = value) %>%
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
  geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 2)),
            size = 3) +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

segmenta_quantiS %>% 
  cor() %>% 
  melt() %>% 
  rename(Correlação = value) %>%
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
  geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 2)),
            size = 3) +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

segmenta_quantiC %>% 
  cor() %>% 
  melt() %>% 
  rename(Correlação = value) %>%
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
  geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 2)),
            size = 3) +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


# Teste da adequabilidade dos dados para PCA (Teste de esfericidade de Bartlett)
cortest.bartlett(segmenta_quantiA)
cortest.bartlett(segmenta_quantiE)
cortest.bartlett(segmenta_quantiS)
cortest.bartlett(segmenta_quantiC)

# Análise Fatorial por Componentes Principais (PCA)
fatorialA <- principal(segmenta_quantiA,
                       nfactors = length(segmenta_quantiA),
                       rotate = "none",
                       scores = TRUE)

fatorialE <- principal(segmenta_quantiE,
                       nfactors = length(segmenta_quantiE),
                       rotate = "none",
                       scores = TRUE)

fatorialS <- principal(segmenta_quantiS,
                       nfactors = length(segmenta_quantiS),
                       rotate = "none",
                       scores = TRUE)

fatorialC <- principal(segmenta_quantiC,
                       nfactors = length(segmenta_quantiC),
                       rotate = "none",
                       scores = TRUE)

# Identificação inicial de todos os autovalores
eigenvaluesA <- round(fatorialA$values, 5)
print(eigenvaluesA)
sum(eigenvaluesA)

eigenvaluesE <- round(fatorialE$values, 5)
print(eigenvaluesE)
sum(eigenvaluesE)

eigenvaluesS <- round(fatorialS$values, 5)
print(eigenvaluesS)
sum(eigenvaluesS)

eigenvaluesC <- round(fatorialC$values, 5)
print(eigenvaluesC)
sum(eigenvaluesC)

# Quantidade de autovalores maiores que 1 (critério de Kaiser)
kA <- sum(eigenvaluesA > 1)
print(kA)

kE <- sum(eigenvaluesE > 1)
print(kE)

kS <- sum(eigenvaluesS > 1)
print(kS)

kC <- sum(eigenvaluesC > 1)
print(kC)

# Definindo a Análise Fatorial por Componentes Principais (PCA) p/ 2 fatores
fatorialA <- principal(segmenta_quantiA,
                       nfactors = kA,
                       rotate = "none",
                       scores = TRUE)

fatorialE <- principal(segmenta_quantiE,
                       nfactors = kE,
                       rotate = "none",
                       scores = TRUE)

fatorialS <- principal(segmenta_quantiS,
                       nfactors = kS,
                       rotate = "none",
                       scores = TRUE)

fatorialC <- principal(segmenta_quantiC,
                       nfactors = kC,
                       rotate = "none",
                       scores = TRUE)

# Identificação da variância compartilhada em cada fator extraído
variancia_compartilhadaA <- as.data.frame(fatorialA$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhadaA) <- c("Autovalores",
                                        "Prop. da Variância",
                                        "Prop. da Variância Acumulada")

variancia_compartilhadaE <- as.data.frame(fatorialE$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhadaE) <- c("Autovalores",
                                        "Prop. da Variância",
                                        "Prop. da Variância Acumulada")

variancia_compartilhadaS <- as.data.frame(fatorialS$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhadaS) <- c("Autovalores",
                                        "Prop. da Variância",
                                        "Prop. da Variância Acumulada")

variancia_compartilhadaC <- as.data.frame(fatorialC$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhadaC) <- c("Autovalores",
                                        "Prop. da Variância",
                                        "Prop. da Variância Acumulada")

# Scree Plot com a proporção da variância compartilhada em cada fator
variancia_compartilhadaA %>%
  slice(2) %>% 
  melt() %>% 
  ggplot(aes(x = variable, 
             y = value)) + 
  geom_col(fill = "orange", color = "black") +
  geom_text(aes(label = paste0(round(value*100, 2),"%") , vjust = -0.1))+
  labs(x = "Fatores",
       y = "Variância Compartilhada Ameaça") +
  theme_bw()

variancia_compartilhadaE %>%
  slice(2) %>% 
  melt() %>% 
  ggplot(aes(x = variable, 
             y = value)) + 
  geom_col(fill = "orange", color = "black") +
  geom_text(aes(label = paste0(round(value*100, 2),"%") , vjust = -0.1))+
  labs(x = "Fatores",
       y = "Variância Compartilhada Exposição") +
  theme_bw()

variancia_compartilhadaS %>%
  slice(2) %>% 
  melt() %>% 
  ggplot(aes(x = variable, 
             y = value)) + 
  geom_col(fill = "orange", color = "black") +
  geom_text(aes(label = paste0(round(value*100, 2),"%") , vjust = -0.1))+
  labs(x = "Fatores",
       y = "Variância Compartilhada Sensibilidade") +
  theme_bw()

variancia_compartilhadaC %>%
  slice(2) %>% 
  melt() %>% 
  ggplot(aes(x = variable, 
             y = value)) + 
  geom_col(fill = "orange", color = "black") +
  geom_text(aes(label = paste0(round(value*100, 2),"%") , vjust = -0.1))+
  labs(x = "Fatores",
       y = "Variância Compartilhada Cap. Adaptativa / Resiliência") +
  theme_bw()

# Extraindo as Cargas Fatoriais
cargas_fatoriaisA <- as.data.frame(unclass(fatorialA$loadings))

cargas_fatoriaisE <- as.data.frame(unclass(fatorialE$loadings))

cargas_fatoriaisS <- as.data.frame(unclass(fatorialS$loadings))

cargas_fatoriaisC <- as.data.frame(unclass(fatorialC$loadings))

# Visualizando as cargas fatoriais
cargas_fatoriaisA %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

cargas_fatoriaisE %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

cargas_fatoriaisS %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

cargas_fatoriaisC %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Extraindo as Comunalidades
comunalidadesA <- as.data.frame(unclass(fatorialA$communality)) %>%
  rename(comunalidades = 1)

comunalidadesE <- as.data.frame(unclass(fatorialE$communality)) %>%
  rename(comunalidades = 1)

comunalidadesS <- as.data.frame(unclass(fatorialS$communality)) %>%
  rename(comunalidades = 1)

comunalidadesC <- as.data.frame(unclass(fatorialC$communality)) %>%
  rename(comunalidades = 1)

# Visualizando as Comunalidades
comunalidadesA %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

comunalidadesE %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

comunalidadesS %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

comunalidadesC %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Relatório das cargas fatoriais e das comunalidades
cargas_fatoriaisA %>%
  mutate(ComunalidadesA = rowSums(cargas_fatoriaisA ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

cargas_fatoriaisE %>%
  mutate(ComunalidadesE = rowSums(cargas_fatoriaisE ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

cargas_fatoriaisS %>%
  mutate(ComunalidadesS = rowSums(cargas_fatoriaisS ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

cargas_fatoriaisC %>%
  mutate(ComunalidadesC = rowSums(cargas_fatoriaisC ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Plotagem das Cargas Fatoriais
cargas_fatoriaisA %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, color = "darkorchid") +
  geom_vline(xintercept = 0, color = "darkorchid") +
  geom_text_repel(label = row.names(cargas_fatoriaisA)) +
  theme_bw()

cargas_fatoriaisE %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, color = "darkorchid") +
  geom_vline(xintercept = 0, color = "darkorchid") +
  geom_text_repel(label = row.names(cargas_fatoriaisE)) +
  theme_bw()

cargas_fatoriaisS %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, color = "darkorchid") +
  geom_vline(xintercept = 0, color = "darkorchid") +
  geom_text_repel(label = row.names(cargas_fatoriaisS)) +
  theme_bw()

cargas_fatoriaisC %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, color = "darkorchid") +
  geom_vline(xintercept = 0, color = "darkorchid") +
  geom_text_repel(label = row.names(cargas_fatoriaisC)) +
  theme_bw()

# Identificação dos Scores Fatoriais
scores_fatoriaisA <- as.data.frame(fatorialA$weights)
scores_fatoriaisE <- as.data.frame(fatorialE$weights)
scores_fatoriaisS <- as.data.frame(fatorialS$weights)
scores_fatoriaisC <- as.data.frame(fatorialC$weights)

# Visualizando os Scores Fatoriais
scores_fatoriaisA %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

scores_fatoriaisE %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

scores_fatoriaisS %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

scores_fatoriaisC %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Cálculo dos fatores extraídos
fatoresA <- as.data.frame(fatorialA$scores)

segmenta_quantiA <- bind_cols(segmenta_quantiA,
                              "fator_1A" = fatoresA$PC1, 
                              "fator_2A" = fatoresA$PC2)

segmenta_quantiA[,c(2, 17, 18)] %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

fatoresE <- as.data.frame(fatorialE$scores)

segmenta_quantiE <- bind_cols(segmenta_quantiE,
                              "fator_1E" = fatoresE$PC1, 
                              "fator_2E" = fatoresE$PC2)

segmenta_quantiE[,c(2, 8, 9)] %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

fatoresS <- as.data.frame(fatorialS$scores)

segmenta_quantiS <- bind_cols(segmenta_quantiS,
                              "fator_1S" = fatoresS$PC1, 
                              "fator_2S" = fatoresS$PC2)

segmenta_quantiS[,c(2, 25, 26)] %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

fatoresC <- as.data.frame(fatorialC$scores)

segmenta_quantiC <- bind_cols(segmenta_quantiC,
                              "fator_1C" = fatoresC$PC1, 
                              "fator_2C" = fatoresC$PC2)

segmenta_quantiC[,c(2, 14, 15)] %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Proposta de construção de um ranking
# Assumindo-se apenas o F1 como indicador, calcula-se a "pontuação"
# Trata-se do fator * variância compartilhada por aquele fator
segmenta_quantiA <- segmenta_quantiA %>% 
  mutate(pontuacaoA = fator_1A * variancia_compartilhadaA$PC1[2])

segmenta_quantiE <- segmenta_quantiE %>% 
  mutate(pontuacaoE = fator_1E * variancia_compartilhadaE$PC1[2])

segmenta_quantiS <- segmenta_quantiS %>% 
  mutate(pontuacaoS = fator_1S * variancia_compartilhadaS$PC1[2])

segmenta_quantiC <- segmenta_quantiC %>% 
  mutate(pontuacaoC = fator_1C * variancia_compartilhadaC$PC1[2])

# Visualizando o ranking final em ordem decrescente
segmenta_quantiA %>%
  arrange(desc(pontuacaoA)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

segmenta$pontuacaoA <- as.numeric(segmenta_quantiA$pontuacaoA)

segmenta_quantiE %>%
  arrange(desc(pontuacaoE)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

segmenta$pontuacaoE <- as.numeric(segmenta_quantiE$pontuacaoE)

segmenta_quantiS %>%
  arrange(desc(pontuacaoS)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

segmenta$pontuacaoS <- as.numeric(segmenta_quantiS$pontuacaoS)

segmenta_quantiC %>%
  arrange(desc(pontuacaoC)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# salvar tabela com resultados
write.table(segmenta, file = "segmentaP.xls", row.names = FALSE, sep = "\t", dec = ".")


# salvar projeto 
save.image(file="TCC_PCA_Cluster_30_03_2024")

############################################################################################
# Análise de Cluster

# Curso: MBA DSA (USP ESALQ)
# Marcelo A. dos Santos Jr
# Biólogo, Mestre em Ecologia,
# Doutorando em Ciência do Sistema Terrestre,
# MBA em Data Science & Analytics e 
# Especialista em Geoprocessamento.
# Lattes: http://lattes.cnpq.br/0538254452248765
# ORCID: https://orcid.org/0000-0002-5325-8203
# Researchgate: https://www.researchgate.net/profile/Marcelo-Santos-Junior
# https://github.com/brasabio/TCC_MBA_DSA.git
# 24/04/2024

# Instalação e carregamento dos pacotes utilizados
pacotes2 <- c("plotly", 
              "tidyverse", 
              "ggrepel",
              "knitr", "kableExtra", 
              "sjPlot", 
              "FactoMineR", 
              "amap", 
              "ade4",
              "cluster",
              "factoextra",
              "dplyr",
              "readxl",
              "WriteXLS",
              "readr")

if(sum(as.numeric(!pacotes2 %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes2, require, character = T) 
} else {
  sapply(pacotes2, require, character = T) 
}

# Importando a base de dados
# Selecionar diretório

setwd("C:/#GIS/Projetos/R")

# carregar base de dados final
load("TCC_PCA_Cluster_30_03_2024")

# salvar projeto 
save.image(file="TCC_PCA_Cluster_30_03_2024")

# Selecioanr as variáveis quanto à pontuação
# A = ameaça; E = exposição; S = Sensibilidade; C = Capacidade Adaptativa/Resiliência
segmenta_quanti2 <- segmenta[,c(130,131,132,133)]

# Estatísticas descritivas dos dados
summary(segmenta_quanti2)

# Padronizar as variáveis por meio do método ZScore
segm_pad <- as.data.frame(scale(segmenta_quanti2))

round(mean(segm_pad$pontuacaoA), 3)
round(sd(segm_pad$pontuacaoA), 3)
round(mean(segm_pad$pontuacaoE), 3)
round(sd(segm_pad$pontuacaoE), 3)
round(mean(segm_pad$pontuacaoS), 3)
round(sd(segm_pad$pontuacaoS), 3)
round(mean(segm_pad$pontuacaoC), 3)
round(sd(segm_pad$pontuacaoC), 3)

# Estatísticas descritivas
summary(segm_pad)

#---------- Esquema de aglomeração hierárquica ---------------------
# Matriz de dissimilaridades
matriz_D <- segm_pad %>% 
  dist(method = "euclidean")

# 1º Teste: Elaboração da clusterização hierárquica como "single linkage"
cluster_hier_single <- agnes(x = matriz_D, method = "single")

# Construção do dendrograma "single linkage"
#dev.off()
dendrosingle <- fviz_dend(x = cluster_hier_single, show_labels = F)
dendrosingle

## O método de encadeamento single linkage não permite uma clusterização útil
## Pode-se interpretar que as observações estão muito próximas umas das outras

# 2º Teste: Elaboração da clusterização hierárquica como "complete linkage"
cluster_hier_complete <- agnes(x = matriz_D, method = "complete")

# Construção do dendrograma "complete linkage"
dendrocomplete <- fviz_dend(x = cluster_hier_complete, show_labels = F)
dendrocomplete

## O método de encadeamento complete linkage melhora significativamente

# 3º Teste: Elaboração da clusterização hierárquica como "average linkage"
cluster_hier_average <- agnes(x = matriz_D, method = "average")

# Construção do dendrograma "average linkage"
dendroaverage <- fviz_dend(x = cluster_hier_average, show_labels = F)
dendroaverage

# salvar projeto 
save.image(file="TCC_PCA_Cluster_30_03_2024")



## Opção escolhida foi complete linkage

# Dendrograma com visualização dos clusters (selecionando por "altura = 4.5")
dendrocomplete_alt <- fviz_dend(x = cluster_hier_complete,
                                h = 4.5,
                                color_labels_by_k = T,
                                rect = T,
                                rect_fill = T,
                                rect_border = "black",
                                lwd = 1,
                                show_labels = F,
                                ggtheme = theme_bw())

dendrocomplete_alt

# Detalhamento do esquema hierárquico
coeficientes <- sort(cluster_hier_complete$height, decreasing = FALSE) 
esquema <- as.data.frame(cbind(cluster_hier_complete$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

# Salvar nova tabela
write.table(esquema, file = "esquema.xls", row.names = FALSE, sep = "\t", dec = ".")

## Portanto, foi gerada uma variável indicando 4 clusters
segm_pad$cluster_Hp <- as.numeric(cutree(tree = cluster_hier_complete, k = 4))
segmenta_quanti2$cluster_Hp <- as.numeric(cutree(tree = cluster_hier_complete, k = 4))
segmenta$cluster_Hp <- as.numeric(cutree(tree = cluster_hier_complete, k = 4))

# Verificar se todas as variáveis ajudam na formação dos grupos
summary(anova_pontuacaoA <- aov(formula = pontuacaoA ~ cluster_Hp, data = segm_pad))
summary(anova_pontuacaoE <- aov(formula = pontuacaoE ~ cluster_Hp, data = segm_pad))
summary(anova_pontuacaoS <- aov(formula = pontuacaoS ~ cluster_Hp, data = segm_pad))
summary(anova_pontuacaoC <- aov(formula = pontuacaoC ~ cluster_Hp, data = segm_pad))

## Todas auxiliam na formação de pelo menos um cluster
# O que os cluster indicam? Interpretação a parir das variáveis médias:

análise <- group_by(segm_pad, cluster_Hp) %>%
  summarise(pontuacaoA = mean(pontuacaoA , na.rm = TRUE),
            pontuacaoE = mean(pontuacaoE , na.rm = TRUE),
            pontuacaoS = mean(pontuacaoS , na.rm = TRUE),
            pontuacaoC = mean(pontuacaoC , na.rm = TRUE))


análise

# Salvar nova tabela
write.table(análise, file = "analise.xls", row.names = FALSE, sep = "\t", dec = ".")

# salvar projeto 
save.image(file="TCC_PCA_Cluster_30_03_2024")

#---------- Esquema de aglomeração não hierárquico K-MEANS ---------------------
segm_pad2 <- as.data.frame(scale(segmenta_quanti2))

round(mean(segm_pad2$pontuacaoA), 3)
round(sd(segm_pad2$pontuacaoA), 3)
round(mean(segm_pad2$pontuacaoE), 3)
round(sd(segm_pad2$pontuacaoE), 3)
round(mean(segm_pad2$pontuacaoS), 3)
round(sd(segm_pad2$pontuacaoS), 3)
round(mean(segm_pad2$pontuacaoC), 3)
round(sd(segm_pad2$pontuacaoC), 3)
round(mean(segm_pad2$cluster_Hp), 3)
round(sd(segm_pad2$cluster_Hp), 3)

# Estatísticas descritivas
summary(segm_pad2)

# Método de Elbow para identificação do número ótimo de clusters
## Apresenta a variação total dentro dos clusters para várias nº de clusters
## Em geral, quando há a dobra é um indício do número ótimo de clusters
Elbow_kmeans2 <- fviz_nbclust(segm_pad2, kmeans, method = "wss", k.max = 15)

Elbow_kmeans2

# Elaboração da clusterização não hieráquica k-means
## centers: parametrização da quantidade de clusters
cluster_kmeans <- kmeans(segm_pad2,
                         centers = 8)

# Criando variável categórica para indicação do cluster no banco de dados
segmenta$cluster_Kp <- as.numeric(cluster_kmeans$cluster)
segmenta_quanti2$cluster_Kp <- as.numeric(cluster_kmeans$cluster)
segm_pad2$cluster_Kp <- as.numeric(cluster_kmeans$cluster)

# Visualização da base de dados
segm_pad2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Comparando os resultados dos esquemas hierárquico e não hierárquico
segmenta %>%
  select(pontuacaoA, cluster_Hp, cluster_Kp) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

## Nem todas auxiliam na formação de pelo menos um cluster
# O que os cluster indicam? Interpretação a partir das variáveis médias:

análise2 <- group_by(segm_pad2, cluster_Kp) %>%
  summarise(pontuacaoA = mean(pontuacaoA , na.rm = TRUE),
            pontuacaoE = mean(pontuacaoE , na.rm = TRUE),
            pontuacaoS = mean(pontuacaoS , na.rm = TRUE),
            pontuacaoC = mean(pontuacaoC , na.rm = TRUE),
            cluster_Hp = mean(cluster_Hp, na.rm = TRUE))


análise2

# Salvar nova tabela
write.table(análise2, file = "analise2.xls", row.names = FALSE, sep = "\t", dec = ".")


# Análise de variância de um fator (ANOVA). Interpretação do output:
## Mean Sq do cluster_K: indica a variabilidade entre grupos
## Mean Sq dos Residuals: indica a variabilidade dentro dos grupos
## F value: estatística de teste (Sum Sq do cluster_K / Sum Sq dos Residuals)
## Pr(>F): p-valor da estatística 
## p-valor < 0.05: pelo menos um cluster apresenta média estatisticamente diferente dos demais
## A variável mais discriminante dos grupos contém maior estatística F (e significativa)
# ANOVA da variável 'matematica'
summary(anova_pontuacaoA <- aov(formula = pontuacaoA ~ cluster_Kp, data = segm_pad2))
summary(anova_pontuacaoE <- aov(formula = pontuacaoE ~ cluster_Kp, data = segm_pad2))
summary(anova_pontuacaoS <- aov(formula = pontuacaoS ~ cluster_Kp, data = segm_pad2))
summary(anova_pontuacaoC <- aov(formula = pontuacaoC ~ cluster_Kp, data = segm_pad2))
summary(Anova_cluster_Hp <- aov(formula = cluster_Hp ~ cluster_Kp, data = segm_pad2))

# salvar projeto 
save.image(file="TCC_PCA_Cluster_30_03_2024")

# Estatísticas descritivas para as variáveis originais 
group_by(segmenta, cluster_Kp) %>%
  summarise(
    mean = mean(pontuacaoA, na.rm = TRUE),
    sd = sd(pontuacaoA, na.rm = TRUE),
    min = min(pontuacaoA, na.rm = TRUE),
    max = max(pontuacaoA, na.rm = TRUE),
    obs = n())

group_by(segmenta, cluster_Kp) %>%
  summarise(
    mean = mean(pontuacaoE, na.rm = TRUE),
    sd = sd(pontuacaoE, na.rm = TRUE),
    min = min(pontuacaoE, na.rm = TRUE),
    max = max(pontuacaoE, na.rm = TRUE),
    obs = n())

group_by(segmenta, cluster_Kp) %>%
  summarise(
    mean = mean(pontuacaoS, na.rm = TRUE),
    sd = sd(pontuacaoS, na.rm = TRUE),
    min = min(pontuacaoS, na.rm = TRUE),
    max = max(pontuacaoS, na.rm = TRUE),
    obs = n())

group_by(segmenta, cluster_Kp) %>%
  summarise(
    mean = mean(pontuacaoC, na.rm = TRUE),
    sd = sd(pontuacaoC, na.rm = TRUE),
    min = min(pontuacaoC, na.rm = TRUE),
    max = max(pontuacaoC, na.rm = TRUE),
    obs = n())

group_by(segmenta, cluster_Kp) %>%
  summarise(
    mean = mean(cluster_Hp, na.rm = TRUE),
    sd = sd(cluster_Hp, na.rm = TRUE),
    min = min(cluster_Hp, na.rm = TRUE),
    max = max(cluster_Hp, na.rm = TRUE),
    obs = n())

# salvar projeto 
save.image(file="TCC_PCA_Cluster_30_03_2024")

# Salvar nova tabela
write.table(segmenta_quanti2, file = "segmenta_quanti2.xls", row.names = FALSE, sep = "\t", dec = ".")
write.table(segmenta_quantiA, file = "segmenta_quantiS.xls", row.names = FALSE, sep = "\t", dec = ".")
write.table(segmenta_quantiE, file = "segmenta_quantiE.xls", row.names = FALSE, sep = "\t", dec = ".")
write.table(segmenta_quantiS, file = "segmenta_quantiS.xls", row.names = FALSE, sep = "\t", dec = ".")
write.table(segmenta_quantiC, file = "segmenta_quantiC.xls", row.names = FALSE, sep = "\t", dec = ".")

write.table(segmenta, file = "segmenta_PHK.xls", row.names = FALSE, sep = "\t", dec = ".")

# salvar projeto 
save.image(file="TCC_PCA_Cluster_30_03_2024")

summary_quanti_T<-summary(segmenta_quanti2)
write.table(summary_quanti_T, file = "summary_quanti_T.xls", row.names = FALSE, sep = "\t", dec = ".")

segm_quanti2_K1 <- segmenta_quanti2[which(segmenta_quanti2$cluster_Kp == 1),1:6]
summary_quanti_K1<-summary(segm_quanti2_K1)
write.table(summary_quanti_K1, file = "summary_quanti_K1.xls", row.names = FALSE, sep = "\t", dec = ".")

segm_quanti2_K2 <- segmenta_quanti2[which(segmenta_quanti2$cluster_Kp == 2),1:6]
summary_quanti_K2<-summary(segm_quanti2_K2)
write.table(summary_quanti_K2, file = "summary_quanti_K2.xls", row.names = FALSE, sep = "\t", dec = ".")

segm_quanti2_K3 <- segmenta_quanti2[which(segmenta_quanti2$cluster_Kp == 3),1:6]
summary_quanti_K3<-summary(segm_quanti2_K3)
write.table(summary_quanti_K3, file = "summary_quanti_K3.xls", row.names = FALSE, sep = "\t", dec = ".")

segm_quanti2_K4 <- segmenta_quanti2[which(segmenta_quanti2$cluster_Kp == 4),1:6]
summary_quanti_K4<-summary(segm_quanti2_K4)
write.table(summary_quanti_K4, file = "summary_quanti_K4.xls", row.names = FALSE, sep = "\t", dec = ".")

segm_quanti2_K5 <- segmenta_quanti2[which(segmenta_quanti2$cluster_Kp == 5),1:6]
summary_quanti_K5<-summary(segm_quanti2_K5)
write.table(summary_quanti_K5, file = "summary_quanti_K5.xls", row.names = FALSE, sep = "\t", dec = ".")

segm_quanti2_K6 <- segmenta_quanti2[which(segmenta_quanti2$cluster_Kp == 6),1:6]
summary_quanti_K6<-summary(segm_quanti2_K6)
write.table(summary_quanti_K6, file = "summary_quanti_K6.xls", row.names = FALSE, sep = "\t", dec = ".")

segm_quanti2_K7 <- segmenta_quanti2[which(segmenta_quanti2$cluster_Kp == 7),1:6]
summary_quanti_K7<-summary(segm_quanti2_K7)
write.table(summary_quanti_K7, file = "summary_quanti_K7.xls", row.names = FALSE, sep = "\t", dec = ".")

segm_quanti2_K8 <- segmenta_quanti2[which(segmenta_quanti2$cluster_Kp == 8),1:6]
summary_quanti_K8<-summary(segm_quanti2_K8)
write.table(summary_quanti_K8, file = "summary_quanti_K8.xls", row.names = FALSE, sep = "\t", dec = ".")

# salvar projeto 
save.image(file="TCC_PCA_Cluster_30_03_2024")
