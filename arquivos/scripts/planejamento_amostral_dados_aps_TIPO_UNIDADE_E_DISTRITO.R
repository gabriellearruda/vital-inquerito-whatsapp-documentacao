# PLANEJAMENTO AMOSTRAL ----

# =============================================================
# PESQUISA APS RECIFE — USF vs USF+
# Script completo: preparação → plano amostral → operacional
# =============================================================
library(writexl)
library(tidyverse)
library(readxl)
library(gtsummary)
library(sidrar)
library(janitor)
library(scales)
options(scipen = 999)


# =============================================================
# PARTE 1: PREPARAÇÃO DOS DADOS
# =============================================================

# --- 1.1 Dados IBGE (Censo 2022) ---
recife <- get_sidra(
  x = 9606, variable = 93, period = "2022",
  geo = "City", geo.filter = list(City = 2611606),
  classific = "all", category = "all"
) |> clean_names()

recife_fil <- recife %>%
  filter(idade == "Total", sexo == "Total", cor_ou_raca != "Total") %>%
  select(cor_ou_raca, valor) %>%
  mutate(perc = round(valor / sum(valor) * 100, 1))

# --- 1.2 Dados APS Recife ---
dados_aps_raw <- read_xlsx(
  '/Users/renatoteixeira/Library/CloudStorage/Box-Box/Data Science/DCNT/Inquéritos/Mais dados mais saúde/_recife/amostragem/Relação_Nominal_18 anos+_28_07_2026.xlsx'
)


dados_aps_raw <- dados_aps_raw %>%
  rename(
    raca_cor = `Raça/Cor`,
    ds_usf = `USF/Unidade`,
    sexo = Sexo,
    cd_distrito = DS,
    dt_ultimo_atend=`Data último atendimento`
  ) %>%
  mutate(
    ds_usf = toupper(ds_usf),
    faixa_etaria = cut(
      Idade,
      breaks = c(18, 35, 55, Inf),
      right = FALSE,
      labels = c("18-34", "35-54", "55+")
    ),
    tipo_unidade = ifelse(
      str_detect(ds_usf, regex("USF MAIS|USF\\+", ignore_case = TRUE)),
      "USF+", "USF"
    )
  )


### Descritiva da data do último atendimento---



# Data de referência
data_referencia <- "2026-08-03"
class(Sys.Date())

# Preparar os dados
dados_aps_raw <- dados_aps_raw |>
  mutate(
    # Converter POSIXct/POSIXt para Date
    dt_ultimo_atend = as.Date(dt_ultimo_atend),
    
    # Tempo desde a última consulta
    tempo_dias = as.numeric(
      difftime(
        data_referencia,
        dt_ultimo_atend,
        units = "days"
      )
    ),
    
    tempo_meses = tempo_dias / 30.4375,
    tempo_anos  = tempo_dias / 365.25,
    
    # Categorias de tempo
    faixa_tempo = case_when(
      is.na(tempo_dias)       ~ "Sem informação",
      tempo_dias < 0          ~ "Data futura",
      tempo_dias <= 30        ~ "Até 30 dias",
      tempo_dias <= 90        ~ "31 a 90 dias",
      tempo_dias <= 180       ~ "91 a 180 dias",
      tempo_dias <= 365       ~ "181 dias a 1 ano",
      tempo_dias <= 730       ~ "Mais de 1 a 2 anos",
      tempo_dias <= 1826      ~ "Mais de 2 a 5 anos",
      TRUE                    ~ "Mais de 5 anos"
    ),
    
    faixa_tempo = factor(
      faixa_tempo,
      levels = c(
        "Até 30 dias",
        "31 a 90 dias",
        "91 a 180 dias",
        "181 dias a 1 ano",
        "Mais de 1 a 2 anos",
        "Mais de 2 a 5 anos",
        "Mais de 5 anos",
        "Data futura",
        "Sem informação"
      )
    )
  )


dados_aps_raw <- dados_aps_raw %>%
  mutate(
    tempo_atend = case_when(
      faixa_tempo %in% c("Até 30 dias", "31 a 90 dias")         ~ "Recente (≤90d)",
      faixa_tempo %in% c("91 a 180 dias", "181 dias a 1 ano")   ~ "Intermediário (91d-1a)",
      faixa_tempo %in% c("Mais de 1 a 2 anos", 
                         "Mais de 2 a 5 anos", 
                         "Mais de 5 anos")                       ~ "Antigo (>1a)",
      TRUE                                                        ~ "Sem info"
    ),
    tempo_atend = factor(tempo_atend,
                         levels = c("Recente (≤90d)", "Intermediário (91d-1a)",
                                    "Antigo (>1a)", "Sem info"))
  )

cat("\n=== Tempo desde último atendimento ===\n")

dados_aps_raw %>%
  count(tempo_atend) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

tab_tempo <- dados_aps_raw %>%
  count(faixa_tempo, name = "n") %>%
  mutate(
    percentual = n / sum(n) * 100,
    rotulo = paste0(format(n, big.mark = "."), " (", round(percentual, 1), "%)"),
    faixa_tempo = reorder(faixa_tempo, percentual)
  )

fig_tempo_consulta <- ggplot(
  tab_tempo,
  aes(
    x = percentual,
    y = faixa_tempo
  )
) +
  geom_col(width = 0.7, fill = "#2878B5") +
  geom_text(aes(label = rotulo), hjust = -0.1, size = 3.5) +
  scale_x_continuous(
    labels = scales::label_percent(scale = 1, decimal.mark = ","),
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "Tempo desde a última consulta",
    subtitle = "Tempo calculado até 03/08/2026",
    x = "Percentual de registros",
    y = NULL,
    caption = "Registros sem data foram mantidos como 'Sem informação'."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(colour = "grey20")
  )

fig_tempo_consulta


# --- 1.3 Descritiva rápida ---
dados_aps_raw %>%
  select(sexo, Idade, raca_cor, faixa_etaria, faixa_tempo,ds_usf) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    sort = all_categorical() ~ "frequency",
    missing = "no"
  )

df_usf_aps <- dados_aps_raw |> 
  group_by(ds_usf) |> 
  summarise(n=n())

# =============================================================
# PARTE 2: PLANO AMOSTRAL
# Representativo por DS × tipo_unidade
# Estratos = DS × tipo (até 16)
# Todas as USF incluídas | deff = 1.0
# Indígenas → censo
# =============================================================

# --- 2.1 Filtrar elegíveis ---
dados_pre <- dados_aps_raw %>%
  filter(
    Idade >= 18,
    sexo != "Não Informado",
    raca_cor != "Sem Informação",
    !is.na(faixa_etaria),
    !is.na(Telefone)
  ) %>%
  mutate(distrito = paste0("DS ", cd_distrito))


df_tel <- dados_pre |> 
  mutate(tel_nchar=nchar(as.character(Telefone)),
         tel_2pri=substr(as.character(Telefone),1,2),
         tel_ddd=substr(as.character(Telefone),3,4))



df_tel_n <- as.data.frame(table(dados_pre$Telefone)) |> 
  rename(tel_repet=Freq,
         Telefone=Var1)

dados_pre <- merge(dados_pre,
               df_tel_n,by="Telefone")

dados <- dados_pre |> 
  filter(tel_repet<6)


cat("=== Base elegível ===\n")
cat("Total:", nrow(dados), "\n\n")
print(table(dados$tipo_unidade))

# --- 2.2 Separar indígenas (censo) ---
dados_indigena <- dados %>% filter(raca_cor == "Indígena")
dados_amostra  <- dados %>% filter(raca_cor != "Indígena")

cat("\n=== Indígenas (censo) ===\n")
cat("Total:", nrow(dados_indigena), "\n")

# --- 2.3 Construir estratos: DS × tipo ---
estratos <- dados_amostra %>%
  count(distrito, tipo_unidade, name = "N_estrato") %>%
  arrange(distrito, tipo_unidade)

cat("\n=== ESTRATOS ===\n")
print(estratos)

# USFs dentro de cada estrato
usf_por_estrato <- dados_amostra %>%
  count(distrito, tipo_unidade, ds_usf, name = "N_cad_usf") %>%
  arrange(distrito, tipo_unidade, desc(N_cad_usf))

# --- 2.4 Tamanho da amostra ---
tx_resposta   <- 0.075
n_por_estrato <- ceiling(1.96^2 * 0.5 * 0.5 / 0.05^2)  # 385

cat("\n=== TAMANHO DA AMOSTRA ===\n")
cat("n por estrato:", n_por_estrato, "respostas (margem ≤ 5%)\n")
cat("Total esperado:", n_por_estrato * nrow(estratos), "respostas\n")
cat("Taxa de resposta:", tx_resposta * 100, "%\n")

# Poder implícito (informativo)
delta_por_ds <- round(
  (qnorm(0.975) + qnorm(0.80)) * sqrt(2 * 2.0^2 / n_por_estrato), 2
)
cat("Poder implícito: detecta δ =", delta_por_ds, "pt por DS\n")

# --- 2.5 Alocação por estrato ---
alocacao_estratos <- estratos %>%
  mutate(
    n_respostas = n_por_estrato,
    n_convites = ceiling(n_respostas / tx_resposta),
    n_convites = pmin(n_convites, N_estrato),
    tipo_disparo = ifelse(n_convites >= N_estrato, "CENSO", "AMOSTRA"),
    n_convites = ifelse(tipo_disparo == "CENSO", N_estrato, n_convites),
    n_respostas_esp = ceiling(n_convites * tx_resposta),
    margem_erro = round(1.96 * sqrt(0.25 / n_respostas_esp) * 100, 1),
    fracao = round(n_convites / N_estrato, 4),
    peso_base = round(N_estrato / n_convites, 4)
  )

cat("\n=== ALOCAÇÃO POR ESTRATO ===\n")
print(alocacao_estratos)
cat("\nTodos ≤ 5%?", all(alocacao_estratos$margem_erro <= 5.0), "\n")

# --- 2.6 Tabela de pós-estratificação ---
tab_pos <- dados_amostra %>%
  count(tipo_unidade, sexo, faixa_etaria, raca_cor, name = "Freq") %>%
  arrange(tipo_unidade, sexo, faixa_etaria, raca_cor)

# --- 2.7 Resumo ---
total_conv <- sum(alocacao_estratos$n_convites) + nrow(dados_indigena)
total_resp <- sum(alocacao_estratos$n_respostas_esp) +
  ceiling(nrow(dados_indigena) * tx_resposta)

cat("\n")
cat("╔══════════════════════════════════════════════════════════╗\n")
cat("║         PLANO AMOSTRAL                                 ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║ Estratos: DS × tipo (", nrow(estratos), ")                       ║\n")
cat("║ n por estrato:", n_por_estrato, "(margem ≤ 5%)              ║\n")
cat("║ δ detectável por DS:", delta_por_ds, "pt                    ║\n")
cat("║ Taxa resposta:", tx_resposta * 100, "%                           ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║ Convites:", formatC(total_conv, format = "d", big.mark = "."),"║\n")
cat("║ Respostas esperadas:", formatC(total_resp, format = "d", big.mark = "."),"║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")


# =============================================================
# PARTE 3: OPERACIONAL
# Alocação direcionada por sexo × faixa × raça/cor
# =============================================================

# --- 3.1 Cotas demográficas dentro de cada estrato ---
cotas <- dados_amostra %>%
  count(distrito, tipo_unidade, sexo, faixa_etaria, raca_cor,
        name = "N_celula") %>%
  left_join(
    alocacao_estratos %>%
      select(distrito, tipo_unidade, N_estrato, n_convites,
             tipo_disparo, peso_base),
    by = c("distrito", "tipo_unidade")
  ) %>%
  mutate(
    prop_celula = N_celula / N_estrato,
    n_convites_celula = case_when(
      tipo_disparo == "CENSO" ~ N_celula,
      TRUE ~ pmax(1, ceiling(prop_celula * n_convites))
    ),
    n_convites_celula = pmin(n_convites_celula, N_celula)
  )

cat("\n=== COTAS DEMOGRÁFICAS ===\n")
cat("Células:", nrow(cotas), "\n")

# Verificação
cotas %>%
  group_by(distrito, tipo_unidade) %>%
  summarise(
    alocado = sum(n_convites_celula),
    planejado = first(n_convites),
    .groups = "drop"
  ) %>%
  mutate(diff = alocado - planejado) %>%
  print(n = 20)

# --- 3.2 Distribuir cotas pelas USFs (com correção de totais) ---
cotas_usf <- dados_amostra %>%
  count(distrito, tipo_unidade, ds_usf,
        sexo, faixa_etaria, raca_cor, name = "N_usf_celula") %>%
  left_join(
    cotas %>%
      select(distrito, tipo_unidade, sexo, faixa_etaria, raca_cor,
             N_celula, n_convites_celula, tipo_disparo),
    by = c("distrito", "tipo_unidade", "sexo", "faixa_etaria", "raca_cor")
  ) %>%
  mutate(
    prop_usf = N_usf_celula / N_celula,
    n_convites_usf = case_when(
      tipo_disparo == "CENSO" ~ N_usf_celula,
      TRUE ~ pmax(1, round(prop_usf * n_convites_celula))
    ),
    n_convites_usf = pmin(n_convites_usf, N_usf_celula)
  )

# Correção: ajustar para que o total por estrato bata com o planejado
cotas_usf <- cotas_usf %>%
  left_join(
    alocacao_estratos %>% select(distrito, tipo_unidade, n_convites_estrato = n_convites),
    by = c("distrito", "tipo_unidade")
  ) %>%
  group_by(distrito, tipo_unidade) %>%
  mutate(
    total_alocado = sum(n_convites_usf),
    deficit = n_convites_estrato - total_alocado,
    # Distribuir o déficit proporcionalmente (prioriza células maiores)
    folga = N_usf_celula - n_convites_usf,  # quanto cada célula pode absorver
    pode_receber = folga > 0 & tipo_disparo == "AMOSTRA",
    prop_redistribuir = ifelse(pode_receber, N_usf_celula / sum(N_usf_celula[pode_receber]), 0),
    ajuste = ifelse(deficit > 0 & pode_receber,
                    pmin(round(prop_redistribuir * deficit), folga),
                    0),
    n_convites_usf = n_convites_usf + ajuste
  ) %>%
  ungroup() %>%
  select(-total_alocado, -deficit, -folga, -pode_receber, -prop_redistribuir, -ajuste)

# Verificar
cat("\n--- Verificação pós-correção ---\n")

cotas_usf %>%
  group_by(distrito, tipo_unidade) %>%
  summarise(
    alocado = sum(n_convites_usf),
    planejado = first(n_convites_estrato),
    .groups = "drop"
  ) %>%
  mutate(diff = alocado - planejado) %>%
  print(n = 20)

# Recuperar ds_usf para uso posterior (1 nome por CNES)
# nomes_usf <- dados_amostra %>%
#   distinct(CNES, ds_usf)
# 
# cotas_usf <- cotas_usf %>%
#   left_join(nomes_usf, by = "CNES")


# DIAGNÓSTICO: onde se perdem os convites de DS 8 × USF+?

# 1. O que cotas_usf diz?
cat("cotas_usf DS8 USF+:", 
    cotas_usf %>% 
      filter(distrito == "DS 8", tipo_unidade == "USF+") %>% 
      pull(n_convites_usf) %>% sum(), "\n")



# # =============================================================
# # 3.2b — LISTA DO PILOTO (com estratificação por tempo_atend)
# # Testa: versão A/B + impacto do tempo desde último atendimento
# # =============================================================
# 
# n_piloto_total <- 3000
# ds_piloto <- c("DS 1", "DS 2","DS 3")
# 
# cat("\n=== PILOTO ===\n")
# cat("DS:", paste(ds_piloto, collapse = ", "), "\n")
# cat("Teste 1: questionário completo (A) vs reduzido (B)\n")
# cat("Teste 2: taxa de resposta por tempo desde último atendimento\n")
# 
# # --- Calcular convites por estrato × tempo_atend ---
# # Distribuir o n do piloto proporcionalmente,
# # mas garantir n mínimo por categoria de tempo
# 
# table(dados_amostra$cd_distrito,dados_amostra$tipo_unidade)
# 
# pool_piloto <- dados_amostra %>%
#   filter(distrito %in% ds_piloto)
# 
# 
# # --- Sortear piloto com alocação por tempo_atend ---
# # Garantir n mínimo de 50 convites por categoria de tempo × tipo
# # para ter precisão mínima na estimativa da taxa
# 
# set.seed(2025)
# 
# # Calcular fração do piloto
# fracao_piloto <- n_piloto_total / nrow(pool_piloto)
# 
# lista_piloto <- pool_piloto %>%
#   group_by(distrito, tipo_unidade, ds_usf, sexo, faixa_etaria, raca_cor, tempo_atend) %>%
#   group_modify(~ {
#     n_sort <- min(pmax(1, round(nrow(.x) * fracao_piloto)), nrow(.x))
#     slice_sample(.x, n = n_sort)
#   }) %>%
#   ungroup()
# 
# # Ajustar total por estrato
# convites_piloto_estrato <- pool_piloto %>%
#   count(distrito, tipo_unidade, name = "N_estrato") %>%
#   mutate(
#     prop = N_estrato / sum(N_estrato),
#     n_alvo = pmax(100, round(prop * n_piloto_total))
#   )
# 
# lista_piloto <- lista_piloto %>%
#   left_join(
#     convites_piloto_estrato %>% select(distrito, tipo_unidade, n_alvo),
#     by = c("distrito", "tipo_unidade")
#   ) %>%
#   group_by(distrito, tipo_unidade) %>%
#   mutate(rank = row_number(sample(n()))) %>%
#   filter(rank <= first(n_alvo)) %>%
#   ungroup() %>%
#   select(-rank, -n_alvo)
# 
# # --- Atribuir versão A/B ---
# # Balanceado por estrato × tempo_atend × sexo
# lista_piloto <- lista_piloto %>%
#   group_by(distrito, tipo_unidade, tempo_atend, sexo) %>%
#   mutate(
#     versao_ab = sample(rep(c("A_completo", "B_reduzido"), length.out = n()))
#   ) %>%
#   ungroup()
# 
# # --- Validações ---
# cat("\n--- Piloto gerado ---\n")
# cat("Total:", nrow(lista_piloto), "\n")
# 
# cat("\nPor versão:\n")
# lista_piloto %>% count(versao_ab) %>% print()
# 
# cat("\nPor estrato × versão:\n")
# lista_piloto %>%
#   count(distrito, tipo_unidade, versao_ab) %>%
#   pivot_wider(names_from = versao_ab, values_from = n, values_fill = 0) %>%
#   print()
# 
# cat("\nPor tempo_atend × versão:\n")
# lista_piloto %>%
#   count(tempo_atend, versao_ab) %>%
#   pivot_wider(names_from = versao_ab, values_from = n, values_fill = 0) %>%
#   mutate(total = A_completo + B_reduzido) %>%
#   print()
# 
# cat("\nPor tempo_atend × tipo_unidade:\n")
# lista_piloto %>%
#   count(tempo_atend, tipo_unidade) %>%
#   pivot_wider(names_from = tipo_unidade, values_from = n, values_fill = 0) %>%
#   print()
# 
# cat("\nPerfil sexo por versão:\n")
# lista_piloto %>%
#   group_by(versao_ab) %>%
#   summarise(
#     Fem = round(mean(sexo == "Feminino") * 100, 1),
#     Masc = round(mean(sexo == "Masculino") * 100, 1),
#     .groups = "drop") %>% print()
# 
# cat("\nPerfil faixa por versão:\n")
# lista_piloto %>%
#   group_by(versao_ab) %>%
#   summarise(
#     `18-34` = round(mean(faixa_etaria == "18-34") * 100, 1),
#     `35-54` = round(mean(faixa_etaria == "35-54") * 100, 1),
#     `55+` = round(mean(faixa_etaria == "55+") * 100, 1),
#     .groups = "drop") %>% print()
# 
# # --- O que o piloto vai responder ---
# cat("\n=== O QUE O PILOTO MEDE ===\n")
# cat("1. Taxa de resposta geral\n")
# cat("2. Taxa por versão A vs B (tamanho do questionário)\n")
# cat("3. Taxa por tempo desde último atendimento:\n")
# cat("   - Recente (≤90d): espera-se a maior taxa\n")
# cat("   - Intermediário (91d-1a): taxa média\n")
# cat("   - Antigo (>1a): espera-se a menor taxa\n")
# cat("   - Sem info: imprevisível\n")
# cat("4. Interação: tempo × versão (ex: antigos respondem mais ao curto?)\n")
# cat("5. Taxa por USF vs USF+\n\n")
# 
# n_por_versao <- nrow(lista_piloto) / 2
# cat("Precisão da taxa: ±",
#     round(1.96 * sqrt(0.075 * 0.925 / n_por_versao) * 100, 1), "pp por versão\n")
# 
# # --- Guardar IDs ---
# ids_piloto <- lista_piloto %>% pull(`Identificador Cidadão`)
# 
# 
# cat("IDs reservados para piloto:", length(ids_piloto), "\n")
# 
# # --- Exportar ---
# path_file <- '/Users/renatoteixeira/Library/CloudStorage/Box-Box/Data Science/DCNT/Inquéritos/Mais dados mais saúde/_recife/amostragem/planejamento_amostral_operacional/'
# write_xlsx(lista_piloto,
#            paste0(path_file, "00_PILOTO_lista_disparo_ds123.xlsx"))
# 
# cat("✅ Exportado: 00_PILOTO_lista_disparo.xlsx\n")
# 
# 
# 
# # --- Guardar IDs do piloto para excluir das ondas ---
# ids_piloto <- lista_piloto %>% pull(`Identificador Cidadão`)
# 
# cat("\nIDs reservados para piloto:", length(ids_piloto), "\n")
# 
# 







# =============================================================
# 3.2b — LISTA DO PILOTO
# 2.400 convites: 3 DS × 2 tipos × 400 por estrato
# Teste A/B + tempo desde último atendimento
# =============================================================

ds_piloto <- c("DS 1", "DS 2", "DS 3")
n_por_estrato_piloto <- 400
n_piloto_total <- n_por_estrato_piloto * length(ds_piloto) * 2  # 2.400

cat("\n══════════════════════════════════════════════════════════\n")
cat("  PILOTO\n")
cat("══════════════════════════════════════════════════════════\n")
cat("DS:", paste(ds_piloto, collapse = " + "), "\n")
cat("n por estrato:", n_por_estrato_piloto, "\n")
cat("Total:", n_piloto_total, "\n")

# --- Pool do piloto ---
pool_piloto <- dados_amostra %>%
  filter(distrito %in% ds_piloto)

cat("Pool disponível:", nrow(pool_piloto), "\n")

# --- Verificar viabilidade ---
viabilidade <- pool_piloto %>%
  count(distrito, tipo_unidade, name = "N_cad") %>%
  mutate(
    convites = n_por_estrato_piloto,
    pct_pop = round(convites / N_cad * 100, 1),
    viavel = ifelse(N_cad >= convites, "✅", "❌")
  )

cat("\n--- Viabilidade por estrato ---\n")
print(viabilidade)

# --- Calcular fração por estrato ---
fracoes_piloto <- viabilidade %>%
  mutate(fracao = convites / N_cad) %>%
  select(distrito, tipo_unidade, N_cad, convites, fracao)

# --- Sortear piloto com alocação direcionada ---
set.seed(2025)

lista_piloto <- pool_piloto %>%
  left_join(
    fracoes_piloto %>% select(distrito, tipo_unidade, fracao),
    by = c("distrito", "tipo_unidade")
  ) %>%
  group_by(distrito, tipo_unidade, ds_usf, sexo, faixa_etaria, raca_cor, tempo_atend) %>%
  group_modify(~ {
    n_sort <- max(1, round(nrow(.x) * .x$fracao[1]))
    n_sort <- min(n_sort, nrow(.x))
    slice_sample(.x, n = n_sort)
  }) %>%
  ungroup()

# Ajustar total por estrato para exatamente 400
lista_piloto <- lista_piloto %>%
  group_by(distrito, tipo_unidade) %>%
  mutate(rank = row_number(sample(n()))) %>%
  filter(rank <= n_por_estrato_piloto) %>%
  ungroup() %>%
  select(-rank, -fracao)

cat("\nSorteados:", nrow(lista_piloto), "\n")

# --- Verificar total por estrato ---
cat("\n--- Total por estrato ---\n")
lista_piloto %>%
  count(distrito, tipo_unidade, name = "n") %>%
  print()

# --- Atribuir versão A/B ---
# Balanceado por estrato × sexo × tempo_atend
lista_piloto <- lista_piloto %>%
  group_by(distrito, tipo_unidade, sexo, tempo_atend) %>%
  mutate(
    versao_ab = sample(rep(c("A_completo", "B_reduzido"), length.out = n()))
  ) %>%
  ungroup()

# =============================================================
# VALIDAÇÕES
# =============================================================

cat("\n══════════════════════════════════════════════════════════\n")
cat("  VALIDAÇÕES DO PILOTO\n")
cat("══════════════════════════════════════════════════════════\n")

cat("\n--- Total ---\n")
cat("Convites:", nrow(lista_piloto), "\n")
cat("Respostas esperadas (7,5%):", ceiling(nrow(lista_piloto) * 0.075), "\n")

cat("\n--- Por versão ---\n")
lista_piloto %>% count(versao_ab) %>% print()

cat("\n--- Por estrato × versão ---\n")
lista_piloto %>%
  count(distrito, tipo_unidade, versao_ab) %>%
  pivot_wider(names_from = versao_ab, values_from = n, values_fill = 0) %>%
  mutate(Total = A_completo + B_reduzido) %>%
  print()

cat("\n--- Por tempo_atend × versão ---\n")
lista_piloto %>%
  count(tempo_atend, versao_ab) %>%
  pivot_wider(names_from = versao_ab, values_from = n, values_fill = 0) %>%
  mutate(Total = A_completo + B_reduzido) %>%
  print()

cat("\n--- Por tempo_atend × tipo_unidade ---\n")
lista_piloto %>%
  count(tempo_atend, tipo_unidade) %>%
  pivot_wider(names_from = tipo_unidade, values_from = n, values_fill = 0) %>%
  print()

cat("\n--- USFs no piloto ---\n")
lista_piloto %>%
  count(distrito, tipo_unidade, ds_usf, name = "convites") %>%
  arrange(distrito, tipo_unidade, desc(convites)) %>%
  print(n = 40)

# Perfil demográfico
pop_ds_piloto <- dados_amostra %>%
  filter(distrito %in% ds_piloto)

cat("\n--- Perfil: Piloto vs População DS ---\n")

cat("\nSexo:\n")
bind_rows(
  pop_ds_piloto %>% count(sexo) %>%
    mutate(fonte = "Pop DS", pct = round(n / sum(n) * 100, 1)),
  lista_piloto %>% count(sexo) %>%
    mutate(fonte = "Piloto", pct = round(n / sum(n) * 100, 1))
) %>%
  select(fonte, sexo, n, pct) %>%
  pivot_wider(names_from = fonte, values_from = c(n, pct)) %>%
  print()

cat("\nFaixa etária:\n")
bind_rows(
  pop_ds_piloto %>% count(faixa_etaria) %>%
    mutate(fonte = "Pop DS", pct = round(n / sum(n) * 100, 1)),
  lista_piloto %>% count(faixa_etaria) %>%
    mutate(fonte = "Piloto", pct = round(n / sum(n) * 100, 1))
) %>%
  select(fonte, faixa_etaria, n, pct) %>%
  pivot_wider(names_from = fonte, values_from = c(n, pct)) %>%
  print()

cat("\nRaça/cor:\n")
bind_rows(
  pop_ds_piloto %>% count(raca_cor) %>%
    mutate(fonte = "Pop DS", pct = round(n / sum(n) * 100, 1)),
  lista_piloto %>% count(raca_cor) %>%
    mutate(fonte = "Piloto", pct = round(n / sum(n) * 100, 1))
) %>%
  select(fonte, raca_cor, n, pct) %>%
  pivot_wider(names_from = fonte, values_from = c(n, pct)) %>%
  print()

# Balanceamento A/B
cat("\n--- Balanceamento A/B por sexo ---\n")
lista_piloto %>%
  count(sexo, versao_ab) %>%
  pivot_wider(names_from = versao_ab, values_from = n) %>%
  print()

cat("\n--- Balanceamento A/B por faixa ---\n")
lista_piloto %>%
  count(faixa_etaria, versao_ab) %>%
  pivot_wider(names_from = versao_ab, values_from = n) %>%
  print()

cat("\n--- Balanceamento A/B por tempo_atend ---\n")
lista_piloto %>%
  count(tempo_atend, versao_ab) %>%
  pivot_wider(names_from = versao_ab, values_from = n) %>%
  print()

# Métricas esperadas
cat("\n=== MÉTRICAS ESPERADAS ===\n")
cat("Por estrato:", n_por_estrato_piloto, "convites →",
    ceiling(n_por_estrato_piloto * 0.075), "respostas\n")
cat("Por versão × tipo (6 estratos × 200):",
    "200 convites →", ceiling(200 * 0.075), "respostas\n")
cat("Precisão da taxa por estrato: ±",
    round(1.96 * sqrt(0.075 * 0.925 / n_por_estrato_piloto) * 100, 1), "pp\n")
cat("Detecta diferença A/B por tipo: ≥",
    round((qnorm(0.975) + qnorm(0.80)) *
            sqrt(2 * 0.075 * 0.925 / (n_por_estrato_piloto / 2)) * 100, 1),
    "pp (80% poder)\n")

# --- Guardar IDs ---
ids_piloto <- lista_piloto %>% pull(`Identificador Cidadão`)
cat("\nIDs reservados para piloto:", length(ids_piloto), "\n")

# --- Exportar ---
write_xlsx(lista_piloto,
           paste0(path_file, "00_PILOTO_lista_disparo_ds123.xlsx"))

cat("✅ Exportado: 00_PILOTO_lista_disparo.xlsx\n")

# =============================================================
# RESUMO
# =============================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════╗\n")
cat("║         RESUMO DO PILOTO                               ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║ DS: 1 + 2 + 3                                         ║\n")
cat("║ Convites: 2.400 (400 por estrato)                     ║\n")
cat("║ Estratos: 6 (3 DS × 2 tipos)                          ║\n")
cat("║ Versão A: 1.200 | Versão B: 1.200                     ║\n")
cat("║ Respostas esperadas: ~180 (30 por estrato)             ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║ Hipóteses:                                             ║\n")
cat("║  1. Taxa geral de resposta                             ║\n")
cat("║  2. Versão A (completo) vs B (reduzido)                ║\n")
cat("║  3. Taxa por tempo desde último atendimento            ║\n")
cat("║  4. Interação tempo × versão                           ║\n")
cat("║  5. Taxa USF vs USF+                                   ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")


table(dados_amostra$cd_distrito,
      dados_amostra$tipo_unidade)

####### Piloto com


# =============================================================
# 3.3 — SORTEAR LISTA PRINCIPAL (excluindo piloto)
# =============================================================

# EXCLUIR IDs do piloto antes de classificar e sortear
dados_classificado <- dados_amostra %>%
  filter(!`Identificador Cidadão` %in% ids_piloto) %>%
  left_join(
    cotas_usf %>%
      select(distrito, tipo_unidade, ds_usf, sexo, faixa_etaria, raca_cor,
             N_usf_celula, n_convites_usf, tipo_disparo),
    by = c("distrito", "tipo_unidade", "ds_usf", "sexo", "faixa_etaria", "raca_cor")
  )

cat("\n=== SORTEIO PRINCIPAL (excluídos", length(ids_piloto), "do piloto) ===\n")

lista_censo <- dados_classificado %>%
  filter(tipo_disparo == "CENSO")

set.seed(2026)

lista_amostra <- dados_classificado %>%
  filter(tipo_disparo == "AMOSTRA") %>%
  group_by(distrito, tipo_unidade, ds_usf, sexo, faixa_etaria, raca_cor) %>%
  group_modify(~ {
    n_sort <- min(.x$n_convites_usf[1], nrow(.x))
    slice_sample(.x, n = n_sort)
  }) %>%
  ungroup()

lista_indigena <- dados_indigena %>%
  filter(!`Identificador Cidadão` %in% ids_piloto) %>%
  mutate(tipo_disparo = "CENSO_INDIGENA")

lista_disparo <- bind_rows(lista_censo, lista_amostra, lista_indigena)

# Verificar que nenhum ID do piloto está nas ondas
overlap_piloto <- sum(lista_disparo$`Identificador Cidadão` %in% ids_piloto)
cat("IDs do piloto na lista de ondas:", overlap_piloto, "(deve ser 0)\n")



# =============================================================
# ATRIBUIR ONDAS (1 a 5) À LISTA DE DISPARO
# Cada onda = 20%, proporcional por estrato e célula demográfica
# =============================================================

set.seed(2026)

lista_disparo_onda <- lista_disparo %>%
  group_by(distrito, tipo_unidade, ds_usf, sexo, faixa_etaria, raca_cor) %>%
  mutate(
    # Embaralhar dentro de cada grupo e atribuir onda sequencialmente
    ordem = sample(n()),
    onda = ceiling(ordem / n() * 5),
    # Garantir que onda fique entre 1 e 5
    onda = pmin(onda, 5)
  ) %>%
  ungroup() %>%
  select(-ordem)

# =============================================================
# VALIDAÇÕES DAS ONDAS
# =============================================================

cat("=== DISTRIBUIÇÃO POR ONDA ===\n")
lista_disparo_onda %>%
  count(onda) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

cat("\n--- Por onda × tipo_unidade ---\n")
lista_disparo_onda %>%
  count(onda, tipo_unidade) %>%
  pivot_wider(names_from = tipo_unidade, values_from = n) %>%
  print()

cat("\n--- Por onda × distrito (total) ---\n")
lista_disparo_onda %>%
  count(onda, distrito) %>%
  pivot_wider(names_from = onda, values_from = n, names_prefix = "Onda_") %>%
  print()

cat("\n--- Perfil demográfico por onda (%) ---\n")

cat("\nSexo:\n")
lista_disparo_onda %>%
  group_by(onda) %>%
  summarise(
    Feminino = round(mean(sexo == "Feminino") * 100, 1),
    Masculino = round(mean(sexo == "Masculino") * 100, 1),
    .groups = "drop"
  ) %>%
  print()

cat("\nFaixa etária:\n")
lista_disparo_onda %>%
  group_by(onda) %>%
  summarise(
    `18-34` = round(mean(faixa_etaria == "18-34") * 100, 1),
    `35-54` = round(mean(faixa_etaria == "35-54") * 100, 1),
    `55+` = round(mean(faixa_etaria == "55+") * 100, 1),
    .groups = "drop"
  ) %>%
  print()

cat("\nRaça/cor:\n")
lista_disparo_onda %>%
  group_by(onda) %>%
  summarise(
    Parda = round(mean(raca_cor == "Parda", na.rm = TRUE) * 100, 1),
    Branca = round(mean(raca_cor == "Branca", na.rm = TRUE) * 100, 1),
    Preta = round(mean(raca_cor == "Preta", na.rm = TRUE) * 100, 1),
    Amarela = round(mean(raca_cor == "Amarela", na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  print()

cat("\n--- Respostas esperadas por onda (tx =", tx_resposta * 100, "%) ---\n")
lista_disparo_onda %>%
  count(onda, name = "convites") %>%
  mutate(respostas_esp = ceiling(convites * tx_resposta)) %>%
  print()
# Validar


write_xlsx(lista_disparo_onda,
           "/Users/renatoteixeira/Library/CloudStorage/Box-Box/Data Science/DCNT/Inquéritos/Mais dados mais saúde/_recife/amostragem/planejamento_amostral_operacional/lista_disparo_onda_tel.xlsx")

lista_disparo_onda_primeira <- lista_disparo_onda |> 
  filter(onda==1)

write_xlsx(lista_disparo_onda_primeira,
           "/Users/renatoteixeira/Library/CloudStorage/Box-Box/Data Science/DCNT/Inquéritos/Mais dados mais saúde/_recife/amostragem/planejamento_amostral_operacional/lista_disparo_onda_primeira.xlsx")


a <- lista_disparo_onda |> 
  select(Equipe) |> 
  unique()
lista_disparo_onda_agregada <- lista_disparo_onda |> 
  group_by(distrito, ds_usf,sexo,faixa_etaria,tipo_unidade,onda ) |> 
  summarise(n_convites=n())

write_xlsx(lista_disparo_onda_agregada,
           "/Users/renatoteixeira/Library/CloudStorage/Box-Box/Data Science/DCNT/Inquéritos/Mais dados mais saúde/_recife/amostragem/planejamento_amostral_operacional/lista_disparo_onda_agregada.xlsx")




cat("\n=== LISTA DE DISPARO ===\n")
cat("Censo:", nrow(lista_censo), "\n")
cat("Amostra:", nrow(lista_amostra), "\n")
cat("Indígenas:", nrow(lista_indigena), "\n")
cat("TOTAL:", nrow(lista_disparo), "\n")

# --- 3.4 Validações ---
cat("\n══════════════════════════════════════════════════════════\n")
cat("  VALIDAÇÕES\n")
cat("══════════════════════════════════════════════════════════\n")

cat("Duplicatas:", sum(duplicated(lista_disparo$`Identificador Cidadão`)), "\n")

cat("\n--- Convites por estrato ---\n")


lista_disparo %>%
  filter(tipo_disparo != "CENSO_INDIGENA") %>%
  count(distrito, tipo_unidade, name = "realizado") %>%
  left_join(
    alocacao_estratos %>% select(distrito, tipo_unidade, n_convites),
    by = c("distrito", "tipo_unidade")
  ) %>%
  rename(planejado = n_convites) %>%
  mutate(diff = realizado - planejado) %>%
  print(n = 20)



cat("\n--- Perfil: Lista vs População ---\n")

cat("\nSexo:\n")
bind_rows(
  dados_amostra %>% count(sexo) %>%
    mutate(fonte = "Pop", pct = round(n / sum(n) * 100, 1)),
  lista_disparo %>% count(sexo) %>%
    mutate(fonte = "Lista", pct = round(n / sum(n) * 100, 1))
) %>%
  select(fonte, sexo, pct) %>%
  pivot_wider(names_from = fonte, values_from = pct) %>%
  print()

cat("\nFaixa etária:\n")
bind_rows(
  dados_amostra %>% count(faixa_etaria) %>%
    mutate(fonte = "Pop", pct = round(n / sum(n) * 100, 1)),
  lista_disparo %>% count(faixa_etaria) %>%
    mutate(fonte = "Lista", pct = round(n / sum(n) * 100, 1))
) %>%
  select(fonte, faixa_etaria, pct) %>%
  pivot_wider(names_from = fonte, values_from = pct) %>%
  print()

cat("\nRaça/cor:\n")
bind_rows(
  dados_amostra %>% count(raca_cor) %>%
    mutate(fonte = "Pop", pct = round(n / sum(n) * 100, 1)),
  lista_disparo %>% count(raca_cor) %>%
    mutate(fonte = "Lista", pct = round(n / sum(n) * 100, 1))
) %>%
  select(fonte, raca_cor, pct) %>%
  pivot_wider(names_from = fonte, values_from = pct) %>%
  print()


# =============================================================
# TABELA COMPARATIVA: POPULAÇÃO vs AMOSTRA (n e %)
# =============================================================

tabela_perfil <- bind_rows(
  # Sexo
  bind_rows(
    dados_amostra %>% count(sexo) %>%
      mutate(fonte = "Pop", pct = round(n / sum(n) * 100, 1)) %>%
      rename(categoria = sexo) %>% mutate(variavel = "Sexo"),
    lista_disparo %>% count(sexo) %>%
      mutate(fonte = "Amostra", pct = round(n / sum(n) * 100, 1)) %>%
      rename(categoria = sexo) %>% mutate(variavel = "Sexo")
  ),
  # Faixa etária
  bind_rows(
    dados_amostra %>% count(faixa_etaria) %>%
      mutate(fonte = "Pop", pct = round(n / sum(n) * 100, 1)) %>%
      rename(categoria = faixa_etaria) %>% mutate(variavel = "Faixa etária"),
    lista_disparo %>% count(faixa_etaria) %>%
      mutate(fonte = "Amostra", pct = round(n / sum(n) * 100, 1)) %>%
      rename(categoria = faixa_etaria) %>% mutate(variavel = "Faixa etária")
  ),
  # Raça/cor
  bind_rows(
    dados_amostra %>% count(raca_cor) %>%
      mutate(fonte = "Pop", pct = round(n / sum(n) * 100, 1)) %>%
      rename(categoria = raca_cor) %>% mutate(variavel = "Raça/Cor"),
    lista_disparo %>% count(raca_cor) %>%
      mutate(fonte = "Amostra", pct = round(n / sum(n) * 100, 1)) %>%
      rename(categoria = raca_cor) %>% mutate(variavel = "Raça/Cor")
  )
) %>%
  pivot_wider(
    names_from = fonte,
    values_from = c(n, pct),
    names_glue = "{fonte}_{.value}"
  ) %>%
  select(variavel, categoria,
         Pop_n, Pop_pct,
         Amostra_n, Amostra_pct) %>%
  rename(
    `Variável` = variavel,
    Categoria = categoria,
    `Pop (n)` = Pop_n,
    `Pop (%)` = Pop_pct,
    `Amostra (n)` = Amostra_n,
    `Amostra (%)` = Amostra_pct
  )

print(tabela_perfil, n = 30)

# Exportar
write.csv(tabela_perfil, "tabela_perfil_pop_vs_amostra.csv", row.names = FALSE)



# --- 3.5 Tabelas de saída ---

# Resumo por USF
tabela_resumo <- lista_disparo %>%
  group_by(distrito, ds_usf, tipo_unidade) %>%
  summarise(total_disparos = n(), .groups = "drop") %>%
  left_join(
    usf_por_estrato %>% select(distrito, tipo_unidade, ds_usf, N_cad_usf),
    by = c("distrito", "tipo_unidade", "ds_usf")
  ) %>%
  mutate(pct_disparo = round(total_disparos / N_cad_usf * 100, 1)) %>%
  arrange(distrito, tipo_unidade, ds_usf)

# Lista nominal
tabela_disparo <- lista_disparo %>%
  select(distrito, ds_usf, tipo_unidade,
         `Identificador Cidadão`, Telefone,sexo, faixa_etaria, raca_cor,
         tipo_disparo) %>%
  arrange(distrito, ds_usf, sexo, faixa_etaria, raca_cor)

# Controle
tabela_controle <- tabela_resumo %>%
  mutate(
    disparos_realizados = 0,
    respostas_recebidas = 0,
    taxa_resposta = 0,
    lembrete_1 = "Não",
    lembrete_2 = "Não",
    status = "Pendente"
  )

# --- 3.6 Resumo final ---
cat("\n")
cat("╔══════════════════════════════════════════════════════════╗\n")
cat("║         RESUMO OPERACIONAL                             ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║ Estratos: DS × tipo | Cotas: sexo × faixa × raça      ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║ Disparos:", formatC(nrow(lista_disparo), format = "d", big.mark = "."),
    "                                    ║\n")
cat("║   Censo:", formatC(nrow(lista_censo), format = "d", big.mark = "."),
    "                                     ║\n")
cat("║   Amostra:", formatC(nrow(lista_amostra), format = "d", big.mark = "."),
    "                                   ║\n")
cat("║   Indígenas:", formatC(nrow(lista_indigena), format = "d", big.mark = "."),
    "                                    ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║ Respostas esperadas:",
    formatC(ceiling(nrow(lista_disparo) * tx_resposta),
            format = "d", big.mark = "."),
    "                          ║\n")
cat("║ Unidades:", nrow(tabela_resumo), "                                    ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")
paste0()

path_file <- '/Users/renatoteixeira/Library/CloudStorage/Box-Box/Data Science/DCNT/Inquéritos/Mais dados mais saúde/_recife/amostragem/planejamento_amostral_operacional/'
# --- 3.7 Exportar ---
write.csv(alocacao_estratos, paste0(path_file,"01_estratos_ds_tipo.csv"), row.names = FALSE)
write.csv(tab_pos, paste0(path_file,"02_tab_pos_estratificacao.csv"), row.names = FALSE)
write.csv(cotas, paste0(path_file,"03_cotas_demograficas.csv"), row.names = FALSE)
write.csv(tabela_resumo, paste0(path_file,"04_resumo_por_unidade.csv"), row.names = FALSE)
write.csv(tabela_disparo, paste0(path_file,"05_lista_disparo.csv"), row.names = FALSE)
write.csv(tabela_controle, paste0(path_file,"06_controle_disparos.csv"), row.names = FALSE)









cat("\n✅ Arquivos exportados:\n")
cat("   01_estratos_ds_tipo.csv\n")
cat("   02_tab_pos_estratificacao.csv\n")
cat("   03_cotas_demograficas.csv\n")
cat("   04_resumo_por_unidade.csv\n")
cat("   05_lista_disparo.csv\n")
cat("   06_controle_disparos.csv\n")






# =============================================================
# VALIDAÇÃO: ADERÊNCIA DEMOGRÁFICA POR ESTRATO (DS × tipo)
# =============================================================

cat("\n══════════════════════════════════════════════════════════\n")
cat("  ADERÊNCIA DEMOGRÁFICA POR ESTRATO\n")
cat("══════════════════════════════════════════════════════════\n")

# Comparar proporções: população vs lista de disparo
aderencia <- bind_rows(
  # População
  dados_amostra %>%
    count(distrito, tipo_unidade, sexo, faixa_etaria, raca_cor, name = "n") %>%
    group_by(distrito, tipo_unidade) %>%
    mutate(pct = round(n / sum(n) * 100, 2), fonte = "Pop") %>%
    ungroup(),
  # Lista de disparo (sem indígenas)
  lista_disparo %>%
    filter(tipo_disparo != "CENSO_INDIGENA") %>%
    count(distrito, tipo_unidade, sexo, faixa_etaria, raca_cor, name = "n") %>%
    group_by(distrito, tipo_unidade) %>%
    mutate(pct = round(n / sum(n) * 100, 2), fonte = "Lista") %>%
    ungroup()
) %>%
  select(distrito, tipo_unidade, sexo, faixa_etaria, raca_cor, fonte, pct) %>%
  pivot_wider(names_from = fonte, values_from = pct, values_fill = 0) %>%
  mutate(diff = round(Lista - Pop, 2))

# Resumo: maior desvio por estrato
cat("\n--- Maior desvio absoluto por estrato ---\n")
aderencia %>%
  group_by(distrito, tipo_unidade) %>%
  summarise(
    max_diff = max(abs(diff)),
    celula_max_diff = paste(
      sexo[which.max(abs(diff))],
      faixa_etaria[which.max(abs(diff))],
      raca_cor[which.max(abs(diff))]
    ),
    .groups = "drop"
  ) %>%
  mutate(status = ifelse(max_diff <= 1.0, "✅ ≤1pp", "⚠️ >1pp")) %>%
  print(n = 20)

# Detalhe: todas as células com desvio > 0.5pp
cat("\n--- Células com desvio > 0.5pp ---\n")
aderencia %>%
  filter(abs(diff) > 0.5) %>%
  arrange(desc(abs(diff))) %>%
  print(n = 30)

# Resumo por variável isolada (agregando estratos)
cat("\n--- Aderência por sexo (todos os estratos) ---\n")
bind_rows(
  dados_amostra %>%
    count(distrito, tipo_unidade, sexo, name = "n") %>%
    group_by(distrito, tipo_unidade) %>%
    mutate(pct = round(n / sum(n) * 100, 1), fonte = "Pop") %>% ungroup(),
  lista_disparo %>%
    filter(tipo_disparo != "CENSO_INDIGENA") %>%
    count(distrito, tipo_unidade, sexo, name = "n") %>%
    group_by(distrito, tipo_unidade) %>%
    mutate(pct = round(n / sum(n) * 100, 1), fonte = "Lista") %>% ungroup()
) %>%
  select(distrito, tipo_unidade, sexo, fonte, pct) %>%
  pivot_wider(names_from = fonte, values_from = pct, values_fill = 0) %>%
  mutate(diff = Lista - Pop) %>%
  print(n = 20)

cat("\n--- Aderência por faixa etária (todos os estratos) ---\n")
bind_rows(
  dados_amostra %>%
    count(distrito, tipo_unidade, faixa_etaria, name = "n") %>%
    group_by(distrito, tipo_unidade) %>%
    mutate(pct = round(n / sum(n) * 100, 1), fonte = "Pop") %>% ungroup(),
  lista_disparo %>%
    filter(tipo_disparo != "CENSO_INDIGENA") %>%
    count(distrito, tipo_unidade, faixa_etaria, name = "n") %>%
    group_by(distrito, tipo_unidade) %>%
    mutate(pct = round(n / sum(n) * 100, 1), fonte = "Lista") %>% ungroup()
) %>%
  select(distrito, tipo_unidade, faixa_etaria, fonte, pct) %>%
  pivot_wider(names_from = fonte, values_from = pct, values_fill = 0) %>%
  mutate(diff = Lista - Pop) %>%
  print(n = 30)

cat("\n--- Aderência por raça/cor (todos os estratos) ---\n")
bind_rows(
  dados_amostra %>%
    count(distrito, tipo_unidade, raca_cor, name = "n") %>%
    group_by(distrito, tipo_unidade) %>%
    mutate(pct = round(n / sum(n) * 100, 1), fonte = "Pop") %>% ungroup(),
  lista_disparo %>%
    filter(tipo_disparo != "CENSO_INDIGENA") %>%
    count(distrito, tipo_unidade, raca_cor, name = "n") %>%
    group_by(distrito, tipo_unidade) %>%
    mutate(pct = round(n / sum(n) * 100, 1), fonte = "Lista") %>% ungroup()
) %>%
  select(distrito, tipo_unidade, raca_cor, fonte, pct) %>%
  pivot_wider(names_from = fonte, values_from = pct, values_fill = 0) %>%
  mutate(diff = Lista - Pop) %>%
  print(n = 40)



# =============================================================
# VALIDAÇÃO: ADERÊNCIA DO CRUZAMENTO sexo × faixa × raça
# (por estrato DS × tipo_unidade)
# =============================================================

cat("\n══════════════════════════════════════════════════════════\n")
cat("  ADERÊNCIA: sexo × faixa_etaria × raca_cor por estrato\n")
cat("══════════════════════════════════════════════════════════\n")

aderencia_cruzada <- bind_rows(
  dados_amostra %>%
    count(distrito, tipo_unidade, sexo, faixa_etaria, raca_cor, name = "n") %>%
    group_by(distrito, tipo_unidade) %>%
    mutate(pct = round(n / sum(n) * 100, 2), fonte = "Pop") %>%
    ungroup(),
  lista_disparo %>%
    filter(tipo_disparo != "CENSO_INDIGENA") %>%
    count(distrito, tipo_unidade, sexo, faixa_etaria, raca_cor, name = "n") %>%
    group_by(distrito, tipo_unidade) %>%
    mutate(pct = round(n / sum(n) * 100, 2), fonte = "Lista") %>%
    ungroup()
) %>%
  select(distrito, tipo_unidade, sexo, faixa_etaria, raca_cor, fonte, pct) %>%
  pivot_wider(names_from = fonte, values_from = pct, values_fill = 0) %>%
  mutate(diff = round(Lista - Pop, 2))

# --- Resumo geral ---
cat("\nTotal de células cruzadas:", nrow(aderencia_cruzada), "\n")
cat("Desvio médio absoluto:", round(mean(abs(aderencia_cruzada$diff)), 3), "pp\n")
cat("Desvio máximo absoluto:", round(max(abs(aderencia_cruzada$diff)), 3), "pp\n")
cat("Células com desvio ≤ 0.5pp:",
    sum(abs(aderencia_cruzada$diff) <= 0.5), "/", nrow(aderencia_cruzada),
    "(", round(mean(abs(aderencia_cruzada$diff) <= 0.5) * 100, 1), "%)\n")

# --- Maior desvio por estrato ---
cat("\n--- Maior desvio por estrato ---\n")
aderencia_cruzada %>%
  group_by(distrito, tipo_unidade) %>%
  summarise(
    n_celulas = n(),
    desvio_medio = round(mean(abs(diff)), 3),
    desvio_max = round(max(abs(diff)), 3),
    celula_pior = paste(
      sexo[which.max(abs(diff))],
      faixa_etaria[which.max(abs(diff))],
      raca_cor[which.max(abs(diff))],
      sep = " | "
    ),
    .groups = "drop"
  ) %>%
  mutate(status = ifelse(desvio_max <= 1.0, "✅", "⚠️")) %>%
  print(n = 20)

# --- Top 20 maiores desvios ---
cat("\n--- 20 maiores desvios ---\n")
aderencia_cruzada %>%
  arrange(desc(abs(diff))) %>%
  head(20) %>%
  print()

# --- Visualização compacta por estrato (Pop vs Lista lado a lado) ---
cat("\n--- Exemplo detalhado: primeiro estrato ---\n")
primeiro <- aderencia_cruzada %>%
  filter(distrito == first(distrito), tipo_unidade == first(tipo_unidade))

cat("Estrato:", primeiro$distrito[1], "×", primeiro$tipo_unidade[1], "\n\n")
primeiro %>%
  arrange(sexo, faixa_etaria, raca_cor) %>%
  print(n = 50)




# Resultados resumidos para relatório ----

# =============================================================
# GERAR NÚMEROS PARA O RELATÓRIO
# =============================================================

cat("\n=== NÚMEROS PARA O RELATÓRIO ===\n")
cat("N_TOTAL:", nrow(dados), "\n")
cat("N_INDIGENA:", nrow(dados_indigena), "\n")
cat("N_AMOSTRA:", nrow(dados_amostra), "\n")
cat("N_USF:", sum(dados_amostra$tipo_unidade == "USF"), "\n")
cat("N_USF_MAIS:", sum(dados_amostra$tipo_unidade == "USF+"), "\n")
cat("delta_por_ds:", delta_por_ds, "\n")
sum(dados_amostra$tipo_unidade == "USF+")/(sum(dados_amostra$tipo_unidade == "USF")+sum(dados_amostra$tipo_unidade == "USF+"))
cat("\n--- Tabela de estratos ---\n")
alocacao_estratos %>%
  select(distrito, tipo_unidade, N_estrato, n_convites,
         tipo_disparo, n_respostas_esp, margem_erro, peso_base) %>%
  print(n = 20)

cat("\n--- Totais por tipo de disparo ---\n")
alocacao_estratos %>%
  group_by(tipo_disparo) %>%
  summarise(
    convites = sum(n_convites),
    respostas = sum(n_respostas_esp),
    .groups = "drop"
  ) %>%
  print()

cat("\nN_TOTAL_POR_GRUPO_USF:",
    sum(alocacao_estratos$n_respostas_esp[alocacao_estratos$tipo_unidade == "USF"]), "\n")
cat("N_TOTAL_POR_GRUPO_USF+:",
    sum(alocacao_estratos$n_respostas_esp[alocacao_estratos$tipo_unidade == "USF+"]), "\n")





# Lista RESERVA ----
# =============================================================
# LISTA DE RESERVA
# 50% da lista principal, mesmas proporções demográficas
# Requer: rodar o script completo (plano + operacional) antes
# =============================================================

library(dplyr)

# =============================================================
# 1. IDENTIFICAR USUÁRIOS DISPONÍVEIS PARA RESERVA
# =============================================================

# IDs já selecionados na lista principal
ids_disparo <- lista_disparo %>%
  pull(`Identificador Cidadão`)

# Usuários elegíveis que NÃO estão na lista principal nem são indígenas
disponiveis <- dados_amostra %>%
  filter(!`Identificador Cidadão` %in% ids_disparo)

cat("=== POOL DISPONÍVEL PARA RESERVA ===\n")
cat("Total na lista principal:", length(ids_disparo), "\n")
cat("Total disponível para reserva:", nrow(disponiveis), "\n\n")

# Verificar disponibilidade por estrato
disp_estrato <- disponiveis %>%
  count(distrito, tipo_unidade, name = "disponiveis") %>%
  left_join(
    lista_disparo %>%
      filter(tipo_disparo != "CENSO_INDIGENA") %>%
      count(distrito, tipo_unidade, name = "na_principal"),
    by = c("distrito", "tipo_unidade")
  ) %>%
  mutate(
    reserva_alvo = ceiling(na_principal * 0.5),
    reserva_possivel = pmin(reserva_alvo, disponiveis),
    cobertura = round(reserva_possivel / reserva_alvo * 100, 1)
  )

cat("--- Disponibilidade por estrato ---\n")
print(disp_estrato)

# =============================================================
# 2. CALCULAR COTAS DA RESERVA POR CÉLULA DEMOGRÁFICA
# =============================================================

# Cotas da lista principal por célula (para manter proporções)
cotas_principal <- lista_disparo %>%
  filter(tipo_disparo != "CENSO_INDIGENA") %>%
  count(distrito, tipo_unidade, ds_usf, sexo, faixa_etaria, raca_cor,
        name = "n_principal")

# Cotas da reserva = 50% da principal, por célula
cotas_reserva <- cotas_principal %>%
  mutate(
    n_reserva_alvo = pmax(1, ceiling(n_principal * 0.5))
  )

# Verificar disponibilidade por célula
disponiveis_celula <- disponiveis %>%
  count(distrito, tipo_unidade, ds_usf, sexo, faixa_etaria, raca_cor,
        name = "n_disponivel")

cotas_reserva <- cotas_reserva %>%
  left_join(disponiveis_celula,
            by = c("distrito", "tipo_unidade", "ds_usf",
                   "sexo", "faixa_etaria", "raca_cor")) %>%
  mutate(
    n_disponivel = ifelse(is.na(n_disponivel), 0, n_disponivel),
    n_reserva = pmin(n_reserva_alvo, n_disponivel)
  )

cat("\n=== COTAS DA RESERVA ===\n")
cat("Células com reserva disponível:",
    sum(cotas_reserva$n_reserva > 0), "/", nrow(cotas_reserva), "\n")
cat("Células sem reserva (pool esgotado):",
    sum(cotas_reserva$n_disponivel == 0), "\n")

# =============================================================
# 3. SORTEAR LISTA DE RESERVA
# =============================================================

set.seed(2027)  # Semente diferente da lista principal

lista_reserva <- disponiveis %>%
  left_join(
    cotas_reserva %>%
      select(distrito, tipo_unidade, ds_usf, sexo, faixa_etaria, raca_cor,
             n_reserva),
    by = c("distrito", "tipo_unidade", "ds_usf",
           "sexo", "faixa_etaria", "raca_cor")
  ) %>%
  filter(!is.na(n_reserva), n_reserva > 0) %>%
  group_by(ds_usf, sexo, faixa_etaria, raca_cor) %>%
  group_modify(~ {
    n_sort <- min(.x$n_reserva[1], nrow(.x))
    slice_sample(.x, n = n_sort)
  }) %>%
  ungroup()

cat("\n=== LISTA DE RESERVA GERADA ===\n")
cat("Total:", nrow(lista_reserva), "\n")
cat("Proporção da principal:", round(nrow(lista_reserva) / length(ids_disparo) * 100, 1), "%\n")

# =============================================================
# 4. VALIDAÇÕES
# =============================================================

cat("\n══════════════════════════════════════════════════════════\n")
cat("  VALIDAÇÕES DA RESERVA\n")
cat("══════════════════════════════════════════════════════════\n")

# Sem sobreposição com a principal
overlap <- sum(lista_reserva$`Identificador Cidadão` %in% ids_disparo)
cat("Sobreposição com lista principal:", overlap, "\n")

# Sem duplicatas internas
dup <- sum(duplicated(lista_reserva$`Identificador Cidadão`))
cat("Duplicatas internas:", dup, "\n")

# Reserva por estrato
cat("\n--- Reserva por estrato ---\n")
lista_reserva %>%
  count(distrito, tipo_unidade, name = "reserva") %>%
  left_join(
    lista_disparo %>%
      filter(tipo_disparo != "CENSO_INDIGENA") %>%
      count(distrito, tipo_unidade, name = "principal"),
    by = c("distrito", "tipo_unidade")
  ) %>%
  mutate(pct = round(reserva / principal * 100, 1)) %>%
  print(n = 20)

# Aderência demográfica: reserva vs principal
cat("\n--- Perfil: Reserva vs Principal ---\n")

cat("\nSexo:\n")
bind_rows(
  lista_disparo %>%
    filter(tipo_disparo != "CENSO_INDIGENA") %>%
    count(sexo) %>% mutate(fonte = "Principal", pct = round(n/sum(n)*100,1)),
  lista_reserva %>%
    count(sexo) %>% mutate(fonte = "Reserva", pct = round(n/sum(n)*100,1))
) %>%
  select(fonte, sexo, pct) %>%
  pivot_wider(names_from = fonte, values_from = pct) %>%
  print()

cat("\nFaixa etária:\n")
bind_rows(
  lista_disparo %>%
    filter(tipo_disparo != "CENSO_INDIGENA") %>%
    count(faixa_etaria) %>% mutate(fonte = "Principal", pct = round(n/sum(n)*100,1)),
  lista_reserva %>%
    count(faixa_etaria) %>% mutate(fonte = "Reserva", pct = round(n/sum(n)*100,1))
) %>%
  select(fonte, faixa_etaria, pct) %>%
  pivot_wider(names_from = fonte, values_from = pct) %>%
  print()

cat("\nRaça/cor:\n")
bind_rows(
  lista_disparo %>%
    filter(tipo_disparo != "CENSO_INDIGENA") %>%
    count(raca_cor) %>% mutate(fonte = "Principal", pct = round(n/sum(n)*100,1)),
  lista_reserva %>%
    count(raca_cor) %>% mutate(fonte = "Reserva", pct = round(n/sum(n)*100,1))
) %>%
  select(fonte, raca_cor, pct) %>%
  pivot_wider(names_from = fonte, values_from = pct) %>%
  print()

# =============================================================
# 5. TABELA OPERACIONAL DA RESERVA
# =============================================================

tabela_reserva <- lista_reserva %>%
  select(
    distrito,
    ds_usf,
    tipo_unidade,
    `Identificador Cidadão`,
    sexo,
    faixa_etaria,
    raca_cor
  ) %>%
  mutate(tipo_disparo = "RESERVA") %>%
  arrange(distrito, ds_usf, sexo, faixa_etaria, raca_cor)

# Resumo por USF
resumo_reserva <- lista_reserva %>%
  group_by(distrito, ds_usf, tipo_unidade) %>%
  summarise(reservas = n(), .groups = "drop") %>%
  arrange(distrito, tipo_unidade)

cat("\n=== RESUMO DA RESERVA POR USF (primeiras 20) ===\n")
print(head(resumo_reserva, 20))

# =============================================================
# 6. RESUMO FINAL
# =============================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════╗\n")
cat("║         LISTA DE RESERVA                               ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║ Total:", formatC(nrow(lista_reserva), format="d", big.mark="."),
    "pessoas                                ║\n")
cat("║ Proporção da principal:",
    round(nrow(lista_reserva) / length(ids_disparo) * 100, 1),
    "%                          ║\n")
cat("║ Sobreposição: 0 | Duplicatas: 0                       ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║ Uso: ativar nas ondas 2-5 quando estrato/célula        ║\n")
cat("║ estiver abaixo da meta de respostas                    ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")

# =============================================================
# 7. EXPORTAR
# =============================================================

write.csv(tabela_reserva,
          paste0(path_file,"07_lista_reserva.csv"),
          row.names = FALSE)

write.csv(resumo_reserva,
          "08_resumo_reserva_por_unidade.csv",
          row.names = FALSE)

cat("\n✅ Arquivos exportados:\n")
cat("   07_lista_reserva.csv\n")
cat("   08_resumo_reserva_por_unidade.csv\n")
