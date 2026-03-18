# ----------------------------------------
# ANÁLISE ESTATÍSTICA - DIABETES
# ----------------------------------------
options(scipen = 999)#pra nao ficar com notacao cientifica
# ----------------------------
# 1. Bibliotecas
# ----------------------------

pacotes <- c("readr", "dplyr", "ggplot2", "randomForest", "reshape2")

for (p in pacotes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# ----------------------------
# 2. Carregamento dos dados
# ----------------------------

dados <- read_csv("diabetes.csv")

# Visualização inicial
head(dados)
str(dados)
summary(dados)

# ----------------------------
# 3. Limpeza dos dados
# ----------------------------

# Selecionar colunas de interesse
dados <- dados %>%
  dplyr::select(Pregnancies, Glucose, BloodPressure, SkinThickness,
                Insulin, BMI, DiabetesPedigreeFunction, Age, Outcome)

# Colunas que não podem ter valor zero
cols_zeros <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")

# Substituir 0 por NA
dados[cols_zeros] <- lapply(dados[cols_zeros], function(x) {
  ifelse(x == 0, NA, x)
})

# Garantir que são numéricas
dados[cols_zeros] <- lapply(dados[cols_zeros], function(x) {
  as.numeric(as.character(x))
})

# ----------------------------
# Imputação pela mediana por grupo (CORRIGIDA)
# ----------------------------

for (col in cols_zeros) {
  
  mediana_0 <- median(dados[[col]][dados$Outcome == 0], na.rm = TRUE)
  mediana_1 <- median(dados[[col]][dados$Outcome == 1], na.rm = TRUE)
  
  # fallback global se necessário
  if (is.na(mediana_0)) {
    mediana_0 <- median(dados[[col]], na.rm = TRUE)
  }
  
  if (is.na(mediana_1)) {
    mediana_1 <- median(dados[[col]], na.rm = TRUE)
  }
  
  # imputação
  dados[[col]][dados$Outcome == 0 & is.na(dados[[col]])] <- mediana_0
  dados[[col]][dados$Outcome == 1 & is.na(dados[[col]])] <- mediana_1
}

# Verificação final
colSums(is.na(dados))

# ----------------------------
# 4. Estatística descritiva
# ----------------------------

summary(dados)

dados %>%
  summarise(across(everything(),
                   list(media = mean, sd = sd),
                   na.rm = TRUE))

# ----------------------------
# 5. Gráficos
# ----------------------------

# Histograma
ggplot(dados, aes(x = Glucose)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  ggtitle("Distribuição da Glicose")

# Dispersão
ggplot(dados, aes(x = BMI, y = Glucose, color = as.factor(Outcome))) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("blue", "red"), name = "Diabetes", 
                     labels = c("Não", "Sim")) +
  ggtitle("BMI vs Glicose por Diagnóstico")

# ----------------------------
# 6. Correlação
# ----------------------------

correlacao <- cor(dados[, -9])  # excluindo Outcome (categórica)
print(round(correlacao, 2))

library(reshape2)
cor_long <- melt(correlacao)


ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Matriz de Correlação", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =====================================================
# 7. TESTES DE HIPÓTESES OBRIGATÓRIOS
# =====================================================

# 7.1 TESTE QUI-QUADRADO DE INDEPENDÊNCIA
# ----------------------------------------
# Criar variáveis categóricas para teste

# Categorizar idade (jovem: <30, adulto: 30-50, idoso: >50)
dados$faixa_etaria <- cut(dados$Age, 
                          breaks = c(0, 30, 50, 100), 
                          labels = c("Jovem", "Adulto", "Idoso"))

# Categorizar glicose (normal: <140, alterada: >=140)
dados$glicose_cat <- ifelse(dados$Glucose < 140, "Normal", "Alterada")

# Teste Qui-Quadrado: Faixa etária vs Outcome
tabela_idade <- table(dados$faixa_etaria, dados$Outcome)
print("Tabela de contingência - Idade vs Diabetes:")
print(tabela_idade)

qui_quadrado_idade <- chisq.test(tabela_idade)
print("Teste Qui-Quadrado - Idade vs Diabetes:")
print(qui_quadrado_idade)

# Teste Qui-Quadrado: Glicose categorizada vs Outcome
tabela_glicose <- table(dados$glicose_cat, dados$Outcome)
print("Tabela de contingência - Glicose vs Diabetes:")
print(tabela_glicose)

qui_quadrado_glicose <- chisq.test(tabela_glicose)
print("Teste Qui-Quadrado - Glicose vs Diabetes:")
print(qui_quadrado_glicose)

# 7.2 COMPARAÇÃO DE VARIÂNCIAS (TESTE F)
# ----------------------------------------
# Comparar variância da glicose entre diabéticos e não diabéticos

teste_variancia_glucose <- var.test(Glucose ~ Outcome, data = dados)
print("Teste F para comparação de variâncias - Glicose:")
print(teste_variancia_glucose)

# Comparar variância do BMI entre diabéticos e não diabéticos
teste_variancia_bmi <- var.test(BMI ~ Outcome, data = dados)
print("Teste F para comparação de variâncias - BMI:")
print(teste_variancia_bmi)

# Comparar variância da idade entre diabéticos e não diabéticos
teste_variancia_idade <- var.test(Age ~ Outcome, data = dados)
print("Teste F para comparação de variâncias - Idade:")
print(teste_variancia_idade)

# 7.3 COMPARAÇÃO DE PROPORÇÕES
# ----------------------------------------
# Comparar proporção de diabéticos entre diferentes grupos

# Proporção de diabéticos por faixa etária
tabela_prop_idade <- prop.table(tabela_idade, 1) * 100
print("Proporção de diabéticos por faixa etária (%):")
print(round(tabela_prop_idade, 2))

# Teste de proporções - Jovens vs Adultos
# Extrair contagens
jovens_diabetes <- tabela_idade["Jovem", "1"]
jovens_total <- sum(tabela_idade["Jovem", ])
adultos_diabetes <- tabela_idade["Adulto", "1"]
adultos_total <- sum(tabela_idade["Adulto", ])

teste_prop_jovens_adultos <- prop.test(x = c(jovens_diabetes, adultos_diabetes),
                                       n = c(jovens_total, adultos_total))
print("Teste de proporções - Jovens vs Adultos (diabetes):")
print(teste_prop_jovens_adultos)

# Teste de proporções - Jovens vs Idosos
idosos_diabetes <- tabela_idade["Idoso", "1"]
idosos_total <- sum(tabela_idade["Idoso", ])

teste_prop_jovens_idosos <- prop.test(x = c(jovens_diabetes, idosos_diabetes),
                                      n = c(jovens_total, idosos_total))
print("Teste de proporções - Jovens vs Idosos (diabetes):")
print(teste_prop_jovens_idosos)

# Proporção por gênero (usando Pregnancies como proxy para gênero feminino)
# Assumindo que Pregnancies > 0 indica sexo feminino
dados$genero <- ifelse(dados$Pregnancies > 0, "Feminino", "Masculino")

tabela_genero <- table(dados$genero, dados$Outcome)
print("Tabela de contingência - Gênero vs Diabetes:")
print(tabela_genero)

teste_prop_genero <- prop.test(x = c(tabela_genero["Feminino", "1"], 
                                     tabela_genero["Masculino", "1"]),
                               n = c(sum(tabela_genero["Feminino", ]),
                                     sum(tabela_genero["Masculino", ])))
print("Teste de proporções - Feminino vs Masculino (diabetes):")
print(teste_prop_genero)

# =====================================================
# 8. RESUMO DOS TESTES ESTATÍSTICOS
# =====================================================

cat("\n", "=========================================\n")
cat("RESUMO DOS TESTES ESTATÍSTICOS REALIZADOS\n")
cat("=========================================\n\n")

cat("1. TESTE QUI-QUADRADO DE INDEPENDÊNCIA:\n")
cat("   - Idade vs Diabetes: p-valor =", round(qui_quadrado_idade$p.value, 5), 
    ifelse(qui_quadrado_idade$p.value < 0.05, "(SIGNIFICATIVO)", "(NÃO significativo)"), "\n")
cat("   - Glicose (categorizada) vs Diabetes: p-valor =", round(qui_quadrado_glicose$p.value, 5), 
    ifelse(qui_quadrado_glicose$p.value < 0.05, "(SIGNIFICATIVO)", "(NÃO significativo)"), "\n\n")

cat("2. TESTE F PARA COMPARAÇÃO DE VARIÂNCIAS:\n")
cat("   - Glicose (Diabéticos vs Não diabéticos): p-valor =", round(teste_variancia_glucose$p.value, 5), 
    ifelse(teste_variancia_glucose$p.value < 0.05, "(SIGNIFICATIVO)", "(NÃO significativo)"), "\n")
cat("   - BMI (Diabéticos vs Não diabéticos): p-valor =", round(teste_variancia_bmi$p.value, 5), 
    ifelse(teste_variancia_bmi$p.value < 0.05, "(SIGNIFICATIVO)", "(NÃO significativo)"), "\n")
cat("   - Idade (Diabéticos vs Não diabéticos): p-valor =", round(teste_variancia_idade$p.value, 5), 
    ifelse(teste_variancia_idade$p.value < 0.05, "(SIGNIFICATIVO)", "(NÃO significativo)"), "\n\n")

cat("3. TESTE DE COMPARAÇÃO DE PROPORÇÕES:\n")
cat("   - Jovens vs Adultos (diabetes): p-valor =", round(teste_prop_jovens_adultos$p.value, 5), 
    ifelse(teste_prop_jovens_adultos$p.value < 0.05, "(SIGNIFICATIVO)", "(NÃO significativo)"), "\n")
cat("   - Jovens vs Idosos (diabetes): p-valor =", round(teste_prop_jovens_idosos$p.value, 5), 
    ifelse(teste_prop_jovens_idosos$p.value < 0.05, "(SIGNIFICATIVO)", "(NÃO significativo)"), "\n")
cat("   - Feminino vs Masculino (diabetes): p-valor =", round(teste_prop_genero$p.value, 5), 
    ifelse(teste_prop_genero$p.value < 0.05, "(SIGNIFICATIVO)", "(NÃO significativo)"), "\n")

cat("=========================================\n\n")

# =====================================================
# 9. Regressão Linear
# =====================================================

modelo_lm <- lm(Glucose ~ BMI, data = dados)

summary(modelo_lm)

# Gráfico com regressão
ggplot(dados, aes(x = BMI, y = Glucose)) +
  geom_point(aes(color = as.factor(Outcome)), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_color_manual(values = c("blue", "red"), name = "Diabetes", 
                     labels = c("Não", "Sim")) +
  ggtitle("Regressão Linear: Glicose ~ BMI")

# =====================================================
# 10. Regressão Logística (modelo principal)
# =====================================================

# Garantir variável resposta como fator
dados$Outcome <- as.factor(dados$Outcome)

# ----------------------------
# Separar X e y
# ----------------------------

X <- dados %>% dplyr::select(Pregnancies, Glucose, BloodPressure, SkinThickness,
                             Insulin, BMI, DiabetesPedigreeFunction, Age)
y <- dados$Outcome

# Converter para numérico
X <- data.frame(lapply(X, as.numeric))

# ----------------------------
# Divisão treino/teste (80/20)
# ----------------------------

set.seed(42)

amostra <- sample(1:nrow(dados), 0.8 * nrow(dados))

X_train <- X[amostra, ]
X_test  <- X[-amostra, ]

y_train <- y[amostra]
y_test  <- y[-amostra]

# ----------------------------
# Normalização
# ----------------------------

# calcular média e desvio do treino
media <- apply(X_train, 2, mean)
desvio <- apply(X_train, 2, sd)

# padronizar
X_train_scaled <- scale(X_train, center = media, scale = desvio)
X_test_scaled  <- scale(X_test, center = media, scale = desvio)

# ----------------------------
# Modelo de Regressão Logística
# ----------------------------

modelo_log <- glm(y_train ~ ., 
                  data = as.data.frame(X_train_scaled), 
                  family = binomial)

summary(modelo_log)

# ----------------------------
# Predição
# ----------------------------

probabilidades <- predict(modelo_log, 
                          newdata = as.data.frame(X_test_scaled), 
                          type = "response")

# Classificação (threshold = 0.5)
predicoes <- ifelse(probabilidades > 0.5, 1, 0)
predicoes <- as.factor(predicoes)

# ----------------------------
# Avaliação
# ----------------------------

matriz <- table(Predito = predicoes, Real = y_test)

print(matriz)

acuracia <- sum(diag(matriz)) / sum(matriz)

cat("Acurácia do modelo (Regressão Logística):", round(acuracia * 100, 2), "%\n")