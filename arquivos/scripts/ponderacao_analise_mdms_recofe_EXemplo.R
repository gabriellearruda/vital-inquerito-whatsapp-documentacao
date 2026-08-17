# =============================================================
# PONDERAÇÃO E ANÁLISE COM AMOSTRA COMPLEXA
# Avaliação de Acesso e Qualidade da APS — Recife (USF vs USF+)
# Pacote: survey (Thomas Lumley)
# =============================================================

# --- 0. Instalação e carregamento dos pacotes ---
# install.packages("survey")
library(survey)
library(dplyr)

# --- 1. Preparação da base de dados ---
# Supondo que 'dados' é o data.frame com as respostas dos usuários,
# contendo ao menos as seguintes variáveis:
#
#   id_usuario      : identificador único do respondente
#   id_unidade      : identificador da USF/USF+ (UPA - 1º estágio)
#   tipo_unidade    : "USF" ou "USF+"
#   distrito        : Distrito Sanitário (DS I a DS VIII)
#   estrato         : combinação distrito + tipo_unidade (ex: "DS_I_USF")
#   peso_amostral   : peso final calculado (ver seção 1.1 abaixo)
#   escore_acesso   : escore de acesso (ex: PCATool)
#   escore_qualidade: escore de qualidade (ex: PCATool)
#   sexo, idade, raca_cor, escolaridade: covariáveis sociodemográficas

# --- 1.1 Cálculo dos pesos amostrais ---
# O peso amostral de cada usuário é o inverso do produto das
# probabilidades de seleção nos dois estágios, ajustado pela
# taxa de resposta no estrato.
#
# Peso = (1 / P1) * (1 / P2) * (1 / taxa_resposta_estrato)
#
# Onde:
#   P1 = prob. de seleção da unidade no estrato
#      = (n_unidades_selecionadas_estrato * cadastrados_unidade) /
#        cadastrados_total_estrato
#   P2 = prob. de seleção do usuário na unidade
#      = n_sorteados_unidade / cadastrados_unidade

dados <- dados %>%
  mutate(
    # Probabilidade de seleção da unidade (PPT)
    prob_estagio1 = (n_unidades_sel_estrato * cadastrados_unidade) /
      cadastrados_total_estrato,
    # Probabilidade de seleção do usuário dentro da unidade
    prob_estagio2 = n_sorteados_unidade / cadastrados_unidade,
    # Peso amostral básico (inverso do produto das probabilidades)
    peso_amostral = 1 / (prob_estagio1 * prob_estagio2)
  )
# Nota: o ajuste de não resposta NÃO entra no peso básico.
# Eventuais desequilíbrios no perfil dos respondentes serão
# corrigidos pela pós-estratificação (seção seguinte).

# --- 2. Declaração do desenho amostral complexo ---
# id    = variável que identifica o conglomerado (1º estágio = unidade)
# strata = variável que identifica o estrato
# weights = peso amostral final
# nest   = TRUE indica que os ids de conglomerado são aninhados nos estratos

desenho <- svydesign(
  id      = ~id_unidade,
  strata  = ~estrato,
  weights = ~peso_amostral,
  nest    = TRUE,
  data    = dados
)

# --- 3. Pós-estratificação (opcional) ---
# Ajusta os pesos para que a distribuição amostral de sexo e faixa etária
# corresponda à distribuição populacional do IBGE (Censo 2022).
# 'pop_ibge' é um data.frame com as frequências populacionais por grupo.

# Exemplo de tabela populacional (substituir pelos dados reais do IBGE):
pop_ibge <- data.frame(
  sexo       = c("M", "M", "M", "F", "F", "F"),
  faixa_etaria = c("18-39", "40-59", "60+", "18-39", "40-59", "60+"),
  Freq       = c(195000, 155000, 85000, 220000, 175000, 105000)
)

# Criar a variável de faixa etária na base
dados$faixa_etaria <- cut(
  dados$idade,
  breaks = c(18, 39, 59, Inf),
  labels = c("18-39", "40-59", "60+"),
  right  = TRUE
)

# Atualizar o objeto de desenho com a nova variável
desenho <- update(desenho, faixa_etaria = dados$faixa_etaria)

# Aplicar pós-estratificação
desenho_pos <- postStratify(
  design   = desenho,
  strata   = ~sexo + faixa_etaria,
  population = pop_ibge
)

# --- 4. Análises descritivas ponderadas ---

# 4.1 Média ponderada do escore de acesso por tipo de unidade
svyby(~escore_acesso, ~tipo_unidade, desenho_pos, svymean, na.rm = TRUE)

# 4.2 Média ponderada do escore de qualidade por tipo de unidade
svyby(~escore_qualidade, ~tipo_unidade, desenho_pos, svymean, na.rm = TRUE)

# 4.3 Média por Distrito Sanitário e tipo de unidade
svyby(~escore_acesso, ~distrito + tipo_unidade, desenho_pos, svymean, na.rm = TRUE)

# 4.4 Proporções ponderadas (ex: satisfação alta)
dados$satisfacao_alta <- ifelse(dados$escore_qualidade >= 6.6, 1, 0)
desenho_pos <- update(desenho_pos, satisfacao_alta = dados$satisfacao_alta)
svyby(~satisfacao_alta, ~tipo_unidade, desenho_pos, svymean, na.rm = TRUE)

# --- 5. Testes de hipótese ---

# 5.1 Teste t para diferença de médias (USF vs USF+)
svyttest(escore_acesso ~ tipo_unidade, design = desenho_pos)
svyttest(escore_qualidade ~ tipo_unidade, design = desenho_pos)

# 5.2 Qui-quadrado ponderado (variáveis categóricas)
svychisq(~satisfacao_alta + tipo_unidade, design = desenho_pos)

# --- 6. Modelos de regressão ajustados ---

# 6.1 Regressão linear — escore de acesso ajustado por covariáveis
modelo_acesso <- svyglm(
  escore_acesso ~ tipo_unidade + sexo + faixa_etaria + raca_cor + escolaridade,
  design = desenho_pos
)
summary(modelo_acesso)

# 6.2 Regressão linear — escore de qualidade ajustado por covariáveis
modelo_qualidade <- svyglm(
  escore_qualidade ~ tipo_unidade + sexo + faixa_etaria + raca_cor + escolaridade,
  design = desenho_pos
)
summary(modelo_qualidade)

# 6.3 Regressão logística — probabilidade de satisfação alta
modelo_logistico <- svyglm(
  satisfacao_alta ~ tipo_unidade + sexo + faixa_etaria + raca_cor + escolaridade,
  design = desenho_pos,
  family = quasibinomial()
)
summary(modelo_logistico)

# --- 7. Efeito do desenho (deff) ---
# Útil para reportar e para ajustar cálculos amostrais futuros

svymean(~escore_acesso, desenho_pos, deff = TRUE)
svymean(~escore_qualidade, desenho_pos, deff = TRUE)