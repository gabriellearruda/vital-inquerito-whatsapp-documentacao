

library(tidyverse)
library(readxl)
library(gtsummary)
library(sidrar)
library(janitor)


options(scipen = 999)

# Dados CESO ----
recife <- get_sidra(
  x = 9606,
  variable = 93,
  period = "2022",
  geo = "City",
  geo.filter = list(City = 2611606),
  classific = "all",
  category = "all"
) |>
  clean_names()

head(recife)
recife_fil <- recife %>% 
  filter(idade=="Total",
         sexo=="Total",
         cor_ou_raca!="Total") %>% 
  select(cor_ou_raca, valor) %>% 
  mutate(perc=round(valor/sum(valor)*100,1))


# Dados APS REcife ----

dados_aps_raw <- read_xlsx("/Users/renatoteixeira/Library/CloudStorage/Box-Box/Data Science/DCNT/Inquéritos/Mais dados mais saúde/_recife/amostragem/Relação Nominal Pop_18 anos +_16_06_2026.xlsx")

dados_aps_raw <- dados_aps_raw %>% 
  rename(raca_cor=`Raça/Cor`,
         ds_usf=`USF/Unidade`,
         sexo=Sexo,
         cd_distrito=Distrito) %>% 
  mutate(ds_usf=toupper(ds_usf))

dados_aps_raw <- dados_aps_raw %>% 
  mutate(fx_etaria=case_when(Idade<30~"<30",
                             Idade<59&Idade>=30~"30-59",
                             Idade>=60~">=60+"))

dados_aps_raw <- dados_aps_raw %>%
  mutate(
    faixa_etaria = cut(
      Idade,
      breaks = c(18, 35, 55, Inf),
      right  = FALSE,
      labels = c("18-34", "35-54", "55+")
    )
  )

dados_aps_raw <- dados_aps_raw %>%
  mutate(
    tipo_unidade = ifelse(
      str_detect(ds_usf, regex("USF MAIS|USF\\+", ignore_case = TRUE)),
      "USF+",
      "USF"
    )
  )
library(gtsummary)

dados_aps_raw %>%
  select(sexo, Idade, raca_cor, fx_etaria,raca_cor,ds_usf) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    sort = all_categorical() ~ "frequency",
    missing = "no"
  )

# =============================================================
# PLANEJAMENTO AMOSTRAL FINAL
# Regra: estratos com n < 100 → censo (disparo total)
#        estratos com n ≥ 100 → amostragem
#        Indígenas → censo (todos)
# Estrato = ds_usf × sexo × faixa_etaria × raca_cor
# Todas as USF incluídas | deff = 1.0
# =============================================================

library(dplyr)
library(tidyr)
library(stringr)

# =============================================================
# 1. PREPARAÇÃO
# =============================================================

dados <- dados_aps_raw %>%
  filter(
    Idade >= 18,
    sexo != "Não Informado",
    raca_cor != "Sem Informação",
    !is.na(faixa_etaria)
  ) %>%
  mutate(
    tipo_unidade = ifelse(
      str_detect(ds_usf, regex("USF MAIS|USF\\+", ignore_case = TRUE)),
      "USF+", "USF"
    ),
    distrito = paste0("DS ", cd_distrito)
  )

cat("=== Base total (≥18, sem missing) ===\n")
cat("Total:", nrow(dados), "\n\n")
print(table(dados$tipo_unidade))


# =============================================================
# 2. SEPARAR INDÍGENAS (CENSO COMPLETO)
# =============================================================

dados_indigena <- dados %>% filter(raca_cor == "Indígena")
dados_amostra  <- dados %>% filter(raca_cor != "Indígena")

cat("\n=== Indígenas (censo) ===\n")
cat("Total:", nrow(dados_indigena), "\n")
print(table(dados_indigena$tipo_unidade))


# =============================================================
# 3. CONSTRUIR ESTRATOS: ds_usf × sexo × faixa_etaria × raca_cor
# =============================================================

estratos <- dados_amostra %>%
  count(ds_usf, CNES, tipo_unidade, distrito, sexo, faixa_etaria, raca_cor,
        name = "N_estrato") %>%
  arrange(ds_usf, sexo, faixa_etaria, raca_cor)

cat("\n=== Total de estratos ===\n")
cat("Estratos:", nrow(estratos), "\n")

cat("\n=== Distribuição do tamanho dos estratos ===\n")
print(summary(estratos$N_estrato))

cat("\n=== Distribuição por faixa de tamanho ===\n")
estratos %>%
  mutate(
    faixa_n = cut(N_estrato,
                  breaks = c(0, 30, 50, 100, 500, 1000, Inf),
                  labels = c("<30", "30-49", "50-99", "100-499", "500-999", "1000+"),
                  right = FALSE)
  ) %>%
  count(faixa_n) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()


# =============================================================
# 4. CLASSIFICAR ESTRATOS: CENSO vs AMOSTRA
# =============================================================

limite_censo <- 30  # Estratos com N < 30 → disparo total

estratos <- estratos %>%
  mutate(
    tipo_disparo = ifelse(N_estrato < limite_censo, "CENSO", "AMOSTRA")
  )

cat("\n=== Classificação dos estratos ===\n")
estratos %>%
  group_by(tipo_disparo) %>%
  summarise(
    n_estratos = n(),
    total_cadastrados = sum(N_estrato),
    .groups = "drop"
  ) %>%
  mutate(pct_cadastrados = round(total_cadastrados / sum(total_cadastrados) * 100, 1)) %>%
  print()

cat("\n--- Por tipo de unidade ---\n")
estratos %>%
  group_by(tipo_unidade, tipo_disparo) %>%
  summarise(
    n_estratos = n(),
    total_cadastrados = sum(N_estrato),
    .groups = "drop"
  ) %>%
  print()


# =============================================================
# 5. CÁLCULO DA AMOSTRA NOS ESTRATOS ≥ 30
# =============================================================

# Parâmetros do cálculo por poder
alpha       <- 0.05
poder       <- 0.80
sigma       <- 2.0
delta       <- 0.3
z_alpha     <- qnorm(1 - alpha / 2)
z_beta      <- qnorm(poder)
tx_resposta <- 0.085

# n por grupo (USF e USF+)
n_por_grupo <- ceiling((z_alpha + z_beta)^2 * 2 * sigma^2 / delta^2)

cat("\n=== Parâmetros do cálculo ===\n")
cat("α:", alpha, "| Poder:", poder, "| σ:", sigma, "| δ:", delta, "\n")
cat("n por grupo (respostas):", n_por_grupo, "\n")

# Total de cadastrados nos estratos amostrais, por tipo
pop_amostra <- estratos %>%
  filter(tipo_disparo == "AMOSTRA") %>%
  group_by(tipo_unidade) %>%
  summarise(N_total = sum(N_estrato), .groups = "drop")

cat("\n=== População nos estratos amostrais ===\n")
print(pop_amostra)

# Fração amostral por tipo de unidade
fracao_amostral <- pop_amostra %>%
  mutate(
    n_respostas = n_por_grupo,
    fracao = n_respostas / N_total,
    n_convites = ceiling(n_respostas / tx_resposta),
    fracao_convites = n_convites / N_total
  )

cat("\n=== Fração amostral nos estratos amostrais ===\n")
print(fracao_amostral)


# =============================================================
# 6. CALCULAR CONVITES POR ESTRATO
# =============================================================

# Estratos CENSO: convites = todos os cadastrados
# Estratos AMOSTRA: convites proporcionais ao tamanho do estrato

head(estratos)
estratos %>%
  mutate(raca_cor_b_a = factor(raca_cor, 
                               levels = c("Indígena", "Preta", "Parda", "Branca","Amarela"))) %>%
  ggplot(aes(x = N_estrato, fill = raca_cor)) +
  geom_histogram( bins=30,position = "stack") +
  labs(x = "n por célula", y = "Frequência", fill = "Raça/cor",
       title = "Histograma do número de amostras por estrato segundo raça ou cor com boost <3 (n=200 por UF)") +
  theme_minimal()

estratos_final <- estratos %>%
  left_join(
    fracao_amostral %>% select(tipo_unidade, fracao_convites),
    by = "tipo_unidade"
  ) %>%
  mutate(
    n_convites = case_when(
      tipo_disparo == "CENSO"   ~ N_estrato,
      tipo_disparo == "AMOSTRA" ~ ceiling(N_estrato * fracao_convites)
    ),
    # Garantir que não ultrapasse cadastrados
    n_convites = pmin(n_convites, N_estrato),
    n_respostas_esp = case_when(
      tipo_disparo == "CENSO"   ~ ceiling(N_estrato * tx_resposta),
      tipo_disparo == "AMOSTRA" ~ ceiling(n_convites * tx_resposta)
    ),
    # Peso amostral
    peso_base = case_when(
      tipo_disparo == "CENSO"   ~ 1.0,  # Censo → peso 1
      tipo_disparo == "AMOSTRA" ~ N_estrato / n_convites
    )
  )


# =============================================================
# 6B. BOOST POR DISTRITO — GARANTIR MARGEM DE 7% POR DS × TIPO
# =============================================================

n_min_7pct <- 196  # n mínimo para margem de 7% (AAS, p=0.5)

# --- Rodada 1: boost proporcional nos estratos AMOSTRA ---
resp_ds <- estratos_final %>%
  group_by(distrito, tipo_unidade) %>%
  summarise(
    respostas_esp_atual = sum(n_respostas_esp),
    .groups = "drop"
  ) %>%
  mutate(
    gap_7pct = pmax(0, n_min_7pct - respostas_esp_atual),
    respostas_extras = gap_7pct
  )

cat("\n=== BOOST RODADA 1 — distritos deficitários ===\n")
print(resp_ds %>% filter(gap_7pct > 0))

estratos_final <- estratos_final %>%
  left_join(
    resp_ds %>% select(distrito, tipo_unidade, gap_7pct, respostas_extras),
    by = c("distrito", "tipo_unidade")
  ) %>%
  group_by(distrito, tipo_unidade) %>%
  mutate(
    prop_estrato_no_ds = ifelse(
      tipo_disparo == "AMOSTRA",
      N_estrato / sum(N_estrato[tipo_disparo == "AMOSTRA"]),
      0
    ),
    convites_extras = ceiling(
      (respostas_extras / tx_resposta) * prop_estrato_no_ds
    ),
    convites_extras = ifelse(tipo_disparo == "CENSO", 0, convites_extras),
    n_convites_final = pmin(n_convites + convites_extras, N_estrato),
    n_respostas_esp_final = ceiling(n_convites_final * tx_resposta),
    peso_base_final = case_when(
      tipo_disparo == "CENSO" ~ 1.0,
      TRUE ~ N_estrato / n_convites_final
    )
  ) %>%
  ungroup()

# --- Rodada 2: para DS × tipo que AINDA não atingiram 196, ---
# --- converter todos os estratos AMOSTRA em CENSO           ---
resp_ds_pos_boost <- estratos_final %>%
  group_by(distrito, tipo_unidade) %>%
  summarise(
    respostas_pos_boost = sum(n_respostas_esp_final),
    .groups = "drop"
  ) %>%
  mutate(ainda_deficitario = respostas_pos_boost < n_min_7pct)

distritos_deficitarios <- resp_ds_pos_boost %>%
  filter(ainda_deficitario) %>%
  select(distrito, tipo_unidade)

if (nrow(distritos_deficitarios) > 0) {
  cat("\n=== BOOST RODADA 2 — censo total nos DS ainda deficitários ===\n")
  print(distritos_deficitarios)
  
  estratos_final <- estratos_final %>%
    left_join(
      distritos_deficitarios %>% mutate(forcar_censo = TRUE),
      by = c("distrito", "tipo_unidade")
    ) %>%
    mutate(
      forcar_censo = ifelse(is.na(forcar_censo), FALSE, forcar_censo),
      # Nos DS ainda deficitários: enviar para TODOS
      n_convites_final = ifelse(forcar_censo, N_estrato, n_convites_final),
      n_respostas_esp_final = ceiling(n_convites_final * tx_resposta),
      tipo_disparo = ifelse(forcar_censo, "CENSO_BOOST", tipo_disparo),
      peso_base_final = case_when(
        tipo_disparo %in% c("CENSO", "CENSO_BOOST") ~ 1.0,
        TRUE ~ N_estrato / n_convites_final
      )
    ) %>%
    select(-forcar_censo)
} else {
  cat("\n✅ Todos os DS × tipo atingiram margem ≤ 7% na rodada 1.\n")
}

# --- Verificação final ---
cat("\n=== VERIFICAÇÃO PÓS-BOOST FINAL ===\n")
estratos_final %>%
  group_by(distrito, tipo_unidade) %>%
  summarise(
    cadastrados = sum(N_estrato),
    convites = sum(n_convites_final),
    respostas_esp = sum(n_respostas_esp_final),
    .groups = "drop"
  ) %>%
  mutate(
    margem_erro = round(1.96 * sqrt(0.25 / respostas_esp) * 100, 1),
    status = ifelse(margem_erro <= 7.0, "✅ ≤7%", "❌ >7%")
  ) %>%
  print(n = 20)

# Resumo do boost total
cat("\n=== IMPACTO TOTAL DO BOOST ===\n")
cat("Convites originais (sem boost):", sum(estratos_final$n_convites), "\n")
cat("Convites finais (com boost):", sum(estratos_final$n_convites_final), "\n")
cat("Convites extras:", sum(estratos_final$n_convites_final) - sum(estratos_final$n_convites), "\n")
cat("Respostas esperadas originais:", sum(estratos_final$n_respostas_esp), "\n")
cat("Respostas esperadas finais:", sum(estratos_final$n_respostas_esp_final), "\n")
# =============================================================
# INSERIR O BLOCO ABAIXO NO SEU SCRIPT GERAL
# POSIÇÃO: após a linha que imprime "IMPACTO TOTAL DO BOOST"
#          e ANTES da linha "# 7. RESUMO GERAL"
#
# Ou seja, logo após este trecho existente:
#   cat("Respostas esperadas finais:", sum(estratos_final$n_respostas_esp_final), "\n")
#
# E antes deste trecho existente:
#   cat("\n=== Amostra por estrato (primeiros 30) ===\n")
# =============================================================


# =============================================================
# 6C. CORREÇÃO DA FRAÇÃO AMOSTRAL POR RAÇA/COR
#     Problema: categoria dominante (Amarela) subrepresentada
#     nos disparos por causa do take-all (n<100 = censo)
#     Solução: elevar fração dos estratos AMOSTRA para que
#     a razão máxima de pesos fique ≤ 3:1
# =============================================================

cat("\n══════════════════════════════════════════════════════════\n")
cat("  6C. CORREÇÃO DE FRAÇÃO POR RAÇA/COR\n")
cat("══════════════════════════════════════════════════════════\n")

# --- Diagnóstico: fração de disparo por raça ---
diag_raca <- dados_amostra %>%
  count(raca_cor, name = "pop") %>%
  mutate(prop_pop = pop / sum(pop))

diag_disp <- estratos_final %>%
  group_by(raca_cor) %>%
  summarise(
    disparos_atual = sum(n_convites_final),
    .groups = "drop"
  )

diag_raca <- diag_raca %>%
  left_join(diag_disp, by = "raca_cor") %>%
  mutate(
    fracao_atual = disparos_atual / pop,
    prop_disparos = disparos_atual / sum(disparos_atual)
  )

cat("\n--- Diagnóstico antes da correção ---\n")
print(diag_raca)

# Maior fração (tipicamente as categorias menores, quase censo)
fracao_max <- max(diag_raca$fracao_atual)

# Razão de pesos alvo
razao_alvo <- 3

# Fração mínima para manter razão ≤ 3:1
fracao_minima <- fracao_max / razao_alvo

cat("\nFração máxima atual:", round(fracao_max, 4), "\n")
cat("Fração mínima necessária (razão", razao_alvo, ":1):", round(fracao_minima, 4), "\n")

# --- Aplicar nova fração nos estratos AMOSTRA ---
estratos_final <- estratos_final %>%
  mutate(
    n_convites_corrigido = case_when(
      # Censo: manter todos
      tipo_disparo %in% c("CENSO", "CENSO_BOOST") ~ as.numeric(N_estrato),
      # Amostra: aplicar a fração mínima se a atual for menor
      tipo_disparo == "AMOSTRA" ~ pmax(
        as.numeric(n_convites_final),
        ceiling(N_estrato * fracao_minima)
      )
    ),
    n_convites_corrigido = pmin(n_convites_corrigido, N_estrato),
    n_respostas_corrigido = ceiling(n_convites_corrigido * tx_resposta),
    peso_corrigido = N_estrato / n_convites_corrigido
  )

# --- Verificar equilíbrio pós-correção ---
cat("\n--- Diagnóstico após correção ---\n")

verif <- estratos_final %>%
  group_by(raca_cor) %>%
  summarise(
    pop = sum(N_estrato),
    disp_antes = sum(n_convites_final),
    disp_depois = sum(n_convites_corrigido),
    .groups = "drop"
  ) %>%
  mutate(
    prop_pop = round(pop / sum(pop) * 100, 1),
    prop_antes = round(disp_antes / sum(disp_antes) * 100, 1),
    prop_depois = round(disp_depois / sum(disp_depois) * 100, 1),
    fracao_antes = round(disp_antes / pop, 4),
    fracao_depois = round(disp_depois / pop, 4),
    peso_antes = round(pop / disp_antes, 2),
    peso_depois = round(pop / disp_depois, 2)
  )

print(verif)

cat("\nRazão de pesos ANTES:",
    round(max(verif$peso_antes) / min(verif$peso_antes), 1), ": 1\n")
cat("Razão de pesos DEPOIS:",
    round(max(verif$peso_depois) / min(verif$peso_depois), 1), ": 1\n")

# --- Verificar que margem por distrito × tipo se mantém ---
cat("\n--- Margem por distrito × tipo (pós-correção) ---\n")
estratos_final %>%
  group_by(distrito, tipo_unidade) %>%
  summarise(
    respostas = sum(n_respostas_corrigido),
    .groups = "drop"
  ) %>%
  mutate(
    margem = round(1.96 * sqrt(0.25 / respostas) * 100, 1),
    status = ifelse(margem <= 7.0, "✅ ≤7%", "⚠️ >7%")
  ) %>%
  print(n = 20)

# --- Totais ---
cat("\nConvites antes da correção:", sum(estratos_final$n_convites_final), "\n")
cat("Convites depois da correção:", sum(estratos_final$n_convites_corrigido), "\n")
cat("Diferença:", sum(estratos_final$n_convites_corrigido) - sum(estratos_final$n_convites_final), "\n")

# --- Substituir variáveis finais pelas corrigidas ---
estratos_final <- estratos_final %>%
  mutate(
    n_convites_final = n_convites_corrigido,
    n_respostas_esp_final = n_respostas_corrigido,
    peso_base_final = peso_corrigido
  ) %>%
  select(-n_convites_corrigido, -n_respostas_corrigido, -peso_corrigido, -fracao_alvo)

cat("\n✅ Variáveis finais atualizadas com correção de fração.\n")
cat("   Todas as seções seguintes usarão os valores corrigidos.\n")


# =============================================================
# >>> A PARTIR DAQUI, CONTINUA O SCRIPT EXISTENTE <<<
# >>> Seção 7: RESUMO GERAL                       <<<
# =============================================================
cat("\n=== Amostra por estrato (primeiros 30) ===\n")
print(
  head(estratos_final %>%
         select(ds_usf, tipo_unidade, sexo, faixa_etaria, raca_cor,
                N_estrato, tipo_disparo, n_convites, n_respostas_esp, peso_base), 30)
)


# =============================================================
# 7. RESUMO GERAL
# =============================================================

cat("\n══════════════════════════════════════════════════════════\n")
cat("  RESUMO DA AMOSTRA (com boost por distrito)\n")
cat("══════════════════════════════════════════════════════════\n")

# Por tipo de disparo e tipo de unidade
resumo <- estratos_final %>%
  group_by(tipo_unidade, tipo_disparo) %>%
  summarise(
    n_estratos = n(),
    cadastrados = sum(N_estrato),
    convites = sum(n_convites_final),
    respostas_esp = sum(n_respostas_esp_final),
    .groups = "drop"
  )

print(resumo)

# Totais gerais
cat("\n=== TOTAIS ===\n")
total_convites_amostra <- sum(estratos_final$n_convites_final)
total_respostas_esp <- sum(estratos_final$n_respostas_esp_final)

cat("Convites (estratos amostrais + censo n<100):", total_convites_amostra, "\n")
cat("Convites (indígenas - censo):", nrow(dados_indigena), "\n")
cat("TOTAL DE CONVITES:", total_convites_amostra + nrow(dados_indigena), "\n")
cat("Respostas esperadas (amostra):", total_respostas_esp, "\n")
cat("Respostas esperadas (indígenas):", ceiling(nrow(dados_indigena) * tx_resposta), "\n")
cat("TOTAL RESPOSTAS ESPERADAS:",
    total_respostas_esp + ceiling(nrow(dados_indigena) * tx_resposta), "\n")


# =============================================================
# 8. VERIFICAR REPRESENTATIVIDADE POR SUBGRUPO
# =============================================================

cat("\n══════════════════════════════════════════════════════════\n")
cat("  REPRESENTATIVIDADE: RESPOSTAS ESPERADAS POR SUBGRUPO\n")
cat("══════════════════════════════════════════════════════════\n")

# Respostas esperadas por sexo × tipo
resp_sexo <- estratos_final %>%
  group_by(tipo_unidade, sexo) %>%
  summarise(
    respostas_esp = sum(n_respostas_esp_final),
    .groups = "drop"
  )

cat("\n--- Por sexo ---\n")
print(resp_sexo)

# Por faixa etária × tipo
resp_faixa <- estratos_final %>%
  group_by(tipo_unidade, faixa_etaria) %>%
  summarise(
    respostas_esp = sum(n_respostas_esp_final),
    .groups = "drop"
  )

cat("\n--- Por faixa etária ---\n")
print(resp_faixa)

# Por raça/cor × tipo
resp_raca <- estratos_final %>%
  group_by(tipo_unidade, raca_cor) %>%
  summarise(
    respostas_esp = sum(n_respostas_esp_final),
    .groups = "drop"
  )

cat("\n--- Por raça/cor ---\n")
print(resp_raca)

# Margem de erro por subgrupo
cat("\n--- Margem de erro estimada por subgrupo ---\n")
bind_rows(
  resp_sexo %>% rename(subgrupo = sexo) %>% mutate(categoria = "Sexo"),
  resp_faixa %>% rename(subgrupo = faixa_etaria) %>% mutate(categoria = "Faixa etária"),
  resp_raca %>% rename(subgrupo = raca_cor) %>% mutate(categoria = "Raça/cor")
) %>%
  mutate(
    margem_erro = round(1.96 * sqrt(0.5 * 0.5 / respostas_esp) * 100, 1)
  ) %>%
  arrange(tipo_unidade, categoria, desc(respostas_esp)) %>%
  print(n = 50)


# =============================================================
# 9. AGREGAR POR UNIDADE (para operacionalização)
# =============================================================

cat("\n══════════════════════════════════════════════════════════\n")
cat("  CONVITES POR UNIDADE (operacional)\n")
cat("══════════════════════════════════════════════════════════\n")

convites_por_unidade <- estratos_final %>%
  group_by(distrito, CNES, ds_usf, tipo_unidade) %>%
  summarise(
    N_cadastrados = sum(N_estrato),
    n_convites = sum(n_convites_final),
    n_censo = sum(n_convites_final[tipo_disparo == "CENSO"]),
    n_amostra = sum(n_convites_final[tipo_disparo == "AMOSTRA"]),
    n_estratos_censo = sum(tipo_disparo == "CENSO"),
    n_estratos_amostra = sum(tipo_disparo == "AMOSTRA"),
    .groups = "drop"
  ) %>%
  mutate(
    pct_disparo = round(n_convites / N_cadastrados * 100, 1)
  ) %>%
  arrange(distrito, tipo_unidade)

cat("\n=== Convites por unidade (primeiras 30) ===\n")
print(head(convites_por_unidade, 30))

cat("\n=== Resumo por tipo de unidade ===\n")
convites_por_unidade %>%
  group_by(tipo_unidade) %>%
  summarise(
    n_unidades = n(),
    total_cad = sum(N_cadastrados),
    total_convites = sum(n_convites),
    total_censo = sum(n_censo),
    total_amostra = sum(n_amostra),
    pct_medio_disparo = round(mean(pct_disparo), 1),
    .groups = "drop"
  ) %>%
  print()


# =============================================================
# 10. TABELA DE PÓS-ESTRATIFICAÇÃO
# =============================================================

# Para estratos amostrais: pós-estratificação por sexo × faixa × raça
tab_pos <- dados_amostra %>%
  count(tipo_unidade, sexo, faixa_etaria, raca_cor, name = "Freq") %>%
  arrange(tipo_unidade, sexo, faixa_etaria, raca_cor)

cat("\n=== TABELA DE PÓS-ESTRATIFICAÇÃO ===\n")
cat("Células:", nrow(tab_pos), "\n")


# =============================================================
# 11. ESTRUTURA DOS PESOS
# =============================================================

cat("\n══════════════════════════════════════════════════════════\n")
cat("  ESTRUTURA DOS PESOS AMOSTRAIS\n")
cat("══════════════════════════════════════════════════════════\n")
cat("
Três tipos de peso conforme o tipo de disparo:

1. ESTRATOS CENSO (n < 100):
   peso_base = 1
   Todos foram convidados, não há seleção.

2. ESTRATOS AMOSTRA (n ≥ 100), incluindo boost por distrito:
   peso_base = N_estrato / n_convites_final
   Inverso da fração amostral no estrato.

3. INDÍGENAS (censo total):
   peso_base = 1

Após calcular o peso_base, aplicar pós-estratificação
com survey::postStratify() usando a tabela em tab_pos
para calibrar por sexo × faixa_etaria × raca_cor.

Nota: nos distritos que receberam boost (DS com gap para
margem de 7%), a fração amostral é maior, gerando pesos
menores — o que é correto, pois esses estratos foram
sobrerepresentados intencionalmente.
\n")


# =============================================================
# 12. RESUMO EXECUTIVO FINAL
# =============================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║           PLANEJAMENTO AMOSTRAL FINAL                      ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║ DESENHO                                                    ║\n")
cat("║   Todas as USF e USF+ incluídas (deff = 1.0)              ║\n")
cat("║   Estratos: ds_usf × sexo × faixa_etaria × raca_cor       ║\n")
cat("║   Estrato n < 100 → censo (disparo total)                 ║\n")
cat("║   Estrato n ≥ 100 → amostragem proporcional              ║\n")
cat("║   Boost: margem ≤ 7% em cada distrito × tipo              ║\n")
cat("║   Indígenas → censo total                                 ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║ CÁLCULO                                                    ║\n")
cat("║   Poder: 80% | α: 5% | σ:", sigma, "| δ:", delta, "pt        ║\n")
cat("║   n por grupo:", n_por_grupo, "respostas                      ║\n")
cat("║   Taxa de resposta:", tx_resposta * 100, "%                          ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")

total_conv <- sum(convites_por_unidade$n_convites) + nrow(dados_indigena)
total_resp <- sum(estratos_final$n_respostas_esp_final) +
  ceiling(nrow(dados_indigena) * tx_resposta)

cat("║ CONVITES                                                   ║\n")
cat("║   Estratos censo (n<100):",
    formatC(sum(estratos_final$n_convites_final[estratos_final$tipo_disparo == "CENSO"]),
            format = "d", big.mark = "."), "                          ║\n")
cat("║   Estratos amostra (n≥100):",
    formatC(sum(estratos_final$n_convites_final[estratos_final$tipo_disparo == "AMOSTRA"]),
            format = "d", big.mark = "."), "                         ║\n")
cat("║   Indígenas (censo):",
    formatC(nrow(dados_indigena), format = "d", big.mark = "."), "                              ║\n")
cat("║   TOTAL:",
    formatC(total_conv, format = "d", big.mark = "."), "                                     ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║ RESPOSTAS ESPERADAS:",
    formatC(total_resp, format = "d", big.mark = "."), "                            ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║ REPRESENTATIVIDADE                                         ║\n")
cat("║   Total Recife: ✅ margem ≤ 5%                             ║\n")
cat("║   Por distrito (total): ✅ 6/8 DS ≤ 5%, 2/8 DS ≤ 7%      ║\n")
cat("║   Por distrito × tipo: ≤ 7% (com boost)                   ║\n")
cat("║   Exceções: DS1-USF e DS3-USF (exploratório, n pequeno)   ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║ PONDERAÇÃO                                                 ║\n")
cat("║   Censo: peso = 1                                         ║\n")
cat("║   Amostra: peso = N_estrato / n_convites_final            ║\n")
cat("║   Pós-estratificação: sexo × faixa × raça/cor              ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")


# =============================================================
# 13. EXPORTAR
# =============================================================

# write.csv(estratos_final, "01_estratos_amostrais.csv", row.names = FALSE)
# write.csv(convites_por_unidade, "02_convites_por_unidade.csv", row.names = FALSE)
# write.csv(tab_pos, "03_tab_pos_estratificacao.csv", row.names = FALSE)
# 
# # Lista de usuários indígenas (censo)
# write.csv(
#   dados_indigena %>% select(distrito, CNES, ds_usf, tipo_unidade,
#                             `Identificador Cidadão`, sexo, faixa_etaria, raca_cor),
#   "04_indigenas_censo.csv", row.names = FALSE
# )

cat("\n✅ Arquivos exportados:\n")
cat("   01_estratos_amostrais.csv (cada estrato com tipo_disparo e n_convites)\n")
cat("   02_convites_por_unidade.csv (agregado por USF para operacionalização)\n")
cat("   03_tab_pos_estratificacao.csv (gabarito para pós-estratificação)\n")
cat("   04_indigenas_censo.csv (lista de indígenas para disparo total)\n")

#### Operacional ----

# =============================================================
# TABELAS OPERACIONAIS PARA EQUIPE DE DISPARO
# Gera: (1) resumo por unidade e (2) lista nominal de disparo
# =============================================================

library(dplyr)
library(tidyr)
library(stringr)

# =============================================================
# 1. GERAR LISTA DE DISPARO (nível do usuário)
# =============================================================

# --- 1.1 Classificar cada USUÁRIO no seu estrato ---
dados_classificado <- dados_amostra %>%
  left_join(
    estratos_final %>%
      select(ds_usf, CNES, sexo, faixa_etaria, raca_cor,
             N_estrato, tipo_disparo, n_convites_final),
    by = c("ds_usf", "CNES", "sexo", "faixa_etaria", "raca_cor")
  )

# --- 1.2 Estratos CENSO: todos entram ---
lista_censo <- dados_classificado %>%
  filter(tipo_disparo == "CENSO") %>%
  mutate(selecionado = TRUE)

cat("Usuários em estratos CENSO:", nrow(lista_censo), "\n")

# --- 1.3 Estratos AMOSTRA: sortear dentro de cada estrato ---
set.seed(2026)

lista_amostra <- dados_classificado %>%
  filter(tipo_disparo == "AMOSTRA") %>%
  group_by(ds_usf, CNES, sexo, faixa_etaria, raca_cor) %>%
  group_modify(~ {
    n_sort <- min(.x$n_convites_final[1], nrow(.x))
    slice_sample(.x, n = n_sort)
  }) %>%
  ungroup() %>%
  mutate(selecionado = TRUE)

cat("Usuários sorteados em estratos AMOSTRA:", nrow(lista_amostra), "\n")

# --- 1.4 Indígenas: todos entram ---
lista_indigena <- dados_indigena %>%
  mutate(
    tipo_disparo = "CENSO_INDIGENA",
    selecionado = TRUE
  )

cat("Usuários indígenas (censo):", nrow(lista_indigena), "\n")

# --- 1.5 Juntar tudo ---
lista_disparo <- bind_rows(
  lista_censo,
  lista_amostra,
  lista_indigena
)

cat("\n=== TOTAL PARA DISPARO:", nrow(lista_disparo), "===\n")


# =============================================================
# 2. TABELA 1: RESUMO POR UNIDADE (para planejamento)
# =============================================================

tabela_resumo <- lista_disparo %>%
  group_by(distrito, CNES, ds_usf, tipo_unidade) %>%
  summarise(
    total_disparos = n(),
    disparos_censo = sum(tipo_disparo %in% c("CENSO", "CENSO_INDIGENA")),
    disparos_amostra = sum(tipo_disparo == "AMOSTRA"),
    .groups = "drop"
  ) %>%
  arrange(distrito, tipo_unidade, ds_usf)

cat("\n=== TABELA RESUMO POR UNIDADE ===\n")
print(tabela_resumo, n = 30)

cat("\nTotal de unidades:", nrow(tabela_resumo), "\n")
cat("Total de disparos:", sum(tabela_resumo$total_disparos), "\n")


# =============================================================
# 3. TABELA 2: LISTA NOMINAL DE DISPARO (operacional)
# =============================================================

# Esta é a tabela que vai para a equipe de disparos
# Contém APENAS o necessário: quem, onde, e tipo de disparo

tabela_disparo <- lista_disparo %>%
  select(
    distrito,
    CNES,
    ds_usf,
    tipo_unidade,
    `Identificador Cidadão`,
    sexo,
    faixa_etaria,
    raca_cor,
    tipo_disparo
  ) %>%
  arrange(distrito, ds_usf, sexo, faixa_etaria, raca_cor)

head(lista_disparo)
lista_disparo %>% 
  group_by(raca_cor) %>% 
  summarise(disparos=n()) %>% 
  ungroup() %>% 
  mutate(n_esperado=disparos*tx_resposta,
         prop_n_esp=round(n_esperado/sum(n_esperado)*100,1)) %>% 
  merge(dados_amostra %>% 
              group_by(raca_cor) %>% 
              summarise(total=n()) %>% 
          ungroup() %>%
          mutate(prop_pop=round(total/sum(total)*100,1)),
            by=("raca_cor"))

cat("\n=== TABELA DE DISPARO (primeiras 20 linhas) ===\n")
print(head(tabela_disparo, 20))


# =============================================================
# 4. TABELA 3: CONTROLE DE DISPAROS (para acompanhamento)
# =============================================================

tabela_controle <- tabela_resumo %>%
  mutate(
    disparos_realizados = 0,
    respostas_recebidas = 0,
    taxa_resposta = 0,
    lembrete_1_enviado = "Não",
    lembrete_2_enviado = "Não",
    status = "Pendente"
  )

cat("\n=== TABELA DE CONTROLE (modelo) ===\n")
print(head(tabela_controle))


# =============================================================
# 5. EXPORTAR
# =============================================================

# Tabela resumo (para gestão)
# write.csv(tabela_resumo,
#           "OPERACIONAL_01_resumo_por_unidade.csv",
#           row.names = FALSE)
# 
# # Lista de disparo (para a equipe técnica)
# write.csv(tabela_disparo,
#           "OPERACIONAL_02_lista_disparo.csv",
#           row.names = FALSE)
# 
# # Tabela de controle (para acompanhamento)
# write.csv(tabela_controle,
#           "OPERACIONAL_03_controle_disparos.csv",
#           row.names = FALSE)

cat("\n✅ Arquivos operacionais exportados:\n")
cat("   OPERACIONAL_01_resumo_por_unidade.csv\n")
cat("   OPERACIONAL_02_lista_disparo.csv\n")
cat("   OPERACIONAL_03_controle_disparos.csv\n")


# =============================================================
# 6. VALIDAÇÕES
# =============================================================

cat("\n══════════════════════════════════════════════════════════\n")
cat("  VALIDAÇÕES\n")
cat("══════════════════════════════════════════════════════════\n")

# Checar duplicatas
n_duplicados <- lista_disparo %>%
  filter(duplicated(`Identificador Cidadão`)) %>%
  nrow()
cat("Usuários duplicados na lista:", n_duplicados, "\n")

# Checar se todos os indígenas estão na lista
n_indigena_faltando <- nrow(dados_indigena) - nrow(lista_indigena)
cat("Indígenas faltando:", n_indigena_faltando, "\n")

# Distribuição do tipo de disparo
cat("\n=== Composição da lista de disparo ===\n")
lista_disparo %>%
  count(tipo_disparo) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

# Comparar perfil da lista de disparo vs população
cat("\n=== Perfil: Lista de disparo vs População ===\n")

cat("\n--- Sexo ---\n")
bind_rows(
  dados_amostra %>% count(sexo) %>% mutate(fonte = "População", pct = round(n/sum(n)*100,1)),
  lista_disparo %>% count(sexo) %>% mutate(fonte = "Lista disparo", pct = round(n/sum(n)*100,1))
) %>%
  select(fonte, sexo, n, pct) %>%
  pivot_wider(names_from = fonte, values_from = c(n, pct)) %>%
  print()

cat("\n--- Faixa etária ---\n")
bind_rows(
  dados_amostra %>% count(faixa_etaria) %>% mutate(fonte = "População", pct = round(n/sum(n)*100,1)),
  lista_disparo %>% count(faixa_etaria) %>% mutate(fonte = "Lista disparo", pct = round(n/sum(n)*100,1))
) %>%
  select(fonte, faixa_etaria, n, pct) %>%
  pivot_wider(names_from = fonte, values_from = c(n, pct)) %>%
  print()

cat("\n--- Raça/cor ---\n")
bind_rows(
  dados_amostra %>% count(raca_cor) %>% mutate(fonte = "População", pct = round(n/sum(n)*100,1)),
  lista_disparo %>% count(raca_cor) %>% mutate(fonte = "Lista disparo", pct = round(n/sum(n)*100,1))
) %>%
  select(fonte, raca_cor, n, pct) %>%
  pivot_wider(names_from = fonte, values_from = c(n, pct)) %>%
  print()