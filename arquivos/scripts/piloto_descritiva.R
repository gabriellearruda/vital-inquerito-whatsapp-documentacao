# =============================================================
# DESCRITIVA DA LISTA DO PILOTO
# =============================================================

library(dplyr)
library(tidyr)

cat("══════════════════════════════════════════════════════════\n")
cat("  DESCRITIVA — LISTA DO PILOTO\n")
cat("══════════════════════════════════════════════════════════\n")

# --- 1. Visão geral ---
cat("\n=== 1. VISÃO GERAL ===\n")
cat("Total de convites:", nrow(lista_piloto), "\n")
cat("DS incluídos:", paste(unique(lista_piloto$distrito), collapse = ", "), "\n")
cat("USFs envolvidas:", n_distinct(lista_piloto$ds_usf), "\n")
cat("Respostas esperadas (7,5%):", ceiling(nrow(lista_piloto) * 0.075), "\n")

# --- 2. Por estrato (DS × tipo) ---
cat("\n=== 2. POR ESTRATO ===\n")
lista_piloto %>%
  group_by(distrito, tipo_unidade) %>%
  summarise(
    n = n(),
    usfs = n_distinct(ds_usf),
    resp_esp = ceiling(n() * 0.075),
    .groups = "drop"
  ) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

# --- 3. Por versão A/B ---
cat("\n=== 3. TESTE A/B ===\n")
lista_piloto %>%
  count(versao_ab, name = "n") %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    resp_esp = ceiling(n * 0.075)
  ) %>%
  print()

cat("\nPor versão × tipo_unidade:\n")
lista_piloto %>%
  count(versao_ab, tipo_unidade) %>%
  pivot_wider(names_from = tipo_unidade, values_from = n, values_fill = 0) %>%
  mutate(Total = USF + `USF+`) %>%
  print()

cat("\nPor versão × distrito:\n")
lista_piloto %>%
  count(versao_ab, distrito) %>%
  pivot_wider(names_from = distrito, values_from = n, values_fill = 0) %>%
  print()

# --- 4. Perfil demográfico ---
cat("\n=== 4. PERFIL DEMOGRÁFICO ===\n")

# Tabela cruzada: população do piloto vs população geral (nos DS do piloto)
pop_ds_piloto <- dados_amostra %>%
  filter(distrito %in% unique(lista_piloto$distrito))

perfil <- bind_rows(
  # Sexo
  bind_rows(
    pop_ds_piloto %>% count(sexo) %>%
      mutate(fonte = "Pop DS", pct = round(n / sum(n) * 100, 1)) %>%
      rename(categoria = sexo) %>% mutate(variavel = "Sexo"),
    lista_piloto %>% count(sexo) %>%
      mutate(fonte = "Piloto", pct = round(n / sum(n) * 100, 1)) %>%
      rename(categoria = sexo) %>% mutate(variavel = "Sexo")
  ),
  # Faixa etária
  bind_rows(
    pop_ds_piloto %>% count(faixa_etaria) %>%
      mutate(fonte = "Pop DS", pct = round(n / sum(n) * 100, 1)) %>%
      rename(categoria = faixa_etaria) %>% mutate(variavel = "Faixa etária"),
    lista_piloto %>% count(faixa_etaria) %>%
      mutate(fonte = "Piloto", pct = round(n / sum(n) * 100, 1)) %>%
      rename(categoria = faixa_etaria) %>% mutate(variavel = "Faixa etária")
  ),
  # Raça/cor
  bind_rows(
    pop_ds_piloto %>% count(raca_cor) %>%
      mutate(fonte = "Pop DS", pct = round(n / sum(n) * 100, 1)) %>%
      rename(categoria = raca_cor) %>% mutate(variavel = "Raça/cor"),
    lista_piloto %>% count(raca_cor) %>%
      mutate(fonte = "Piloto", pct = round(n / sum(n) * 100, 1)) %>%
      rename(categoria = raca_cor) %>% mutate(variavel = "Raça/cor")
  ),
  # Tempo atendimento
  bind_rows(
    pop_ds_piloto %>% count(tempo_atend) %>%
      mutate(fonte = "Pop DS", pct = round(n / sum(n) * 100, 1)) %>%
      rename(categoria = tempo_atend) %>% mutate(variavel = "Tempo atend."),
    lista_piloto %>% count(tempo_atend) %>%
      mutate(fonte = "Piloto", pct = round(n / sum(n) * 100, 1)) %>%
      rename(categoria = tempo_atend) %>% mutate(variavel = "Tempo atend.")
  )
) %>%
  select(variavel, categoria, fonte, n, pct) %>%
  pivot_wider(names_from = fonte, values_from = c(n, pct),
              names_glue = "{fonte}_{.value}") %>%
  mutate(diff_pp = `Piloto_pct` - `Pop DS_pct`) %>%
  select(variavel, categoria,
         `Pop DS_n`, `Pop DS_pct`, `Piloto_n`, `Piloto_pct`, diff_pp)

print(perfil, n = 30)

# --- 5. Tempo desde último atendimento ---
cat("\n=== 5. TEMPO ÚLTIMO ATENDIMENTO ===\n")

cat("\nPor tempo_atend × tipo_unidade:\n")
lista_piloto %>%
  count(tempo_atend, tipo_unidade) %>%
  pivot_wider(names_from = tipo_unidade, values_from = n, values_fill = 0) %>%
  mutate(Total = USF + `USF+`) %>%
  print()

cat("\nPor tempo_atend × versão A/B:\n")
lista_piloto %>%
  count(tempo_atend, versao_ab) %>%
  pivot_wider(names_from = versao_ab, values_from = n, values_fill = 0) %>%
  mutate(Total = A_completo + B_reduzido) %>%
  print()

cat("\nPor tempo_atend × tipo × versão (tabela completa):\n")
lista_piloto %>%
  count(tempo_atend, tipo_unidade, versao_ab) %>%
  pivot_wider(names_from = c(tipo_unidade, versao_ab),
              values_from = n, values_fill = 0) %>%
  print()

# --- 6. Por USF ---
cat("\n=== 6. POR USF ===\n")
lista_piloto %>%
  group_by(distrito, tipo_unidade, ds_usf) %>%
  summarise(
    convites = n(),
    versao_A = sum(versao_ab == "A_completo"),
    versao_B = sum(versao_ab == "B_reduzido"),
    pct_recente = round(mean(tempo_atend == "Recente (≤90d)") * 100, 1),
    pct_antigo = round(mean(tempo_atend == "Antigo (>1a)") * 100, 1),
    pct_sem_info = round(mean(tempo_atend == "Sem info") * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(distrito, tipo_unidade, desc(convites)) %>%
  print(n = 30)

# --- 7. Balanceamento do A/B ---
cat("\n=== 7. BALANCEAMENTO A/B ===\n")

cat("\nPor sexo × versão:\n")
lista_piloto %>%
  count(sexo, versao_ab) %>%
  pivot_wider(names_from = versao_ab, values_from = n) %>%
  print()

cat("\nPor faixa × versão:\n")
lista_piloto %>%
  count(faixa_etaria, versao_ab) %>%
  pivot_wider(names_from = versao_ab, values_from = n) %>%
  print()

cat("\nPor raça × versão:\n")
lista_piloto %>%
  count(raca_cor, versao_ab) %>%
  pivot_wider(names_from = versao_ab, values_from = n) %>%
  print()

cat("\nPor tempo_atend × versão:\n")
lista_piloto %>%
  count(tempo_atend, versao_ab) %>%
  pivot_wider(names_from = versao_ab, values_from = n) %>%
  print()

# --- 8. Resumo ---
cat("\n")
cat("╔══════════════════════════════════════════════════════════╗\n")
cat("║         RESUMO DO PILOTO                               ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║ Convites:", formatC(nrow(lista_piloto), format="d", big.mark="."),
    "                                     ║\n")
cat("║ Versão A (completo):",
    sum(lista_piloto$versao_ab == "A_completo"), "                          ║\n")
cat("║ Versão B (reduzido):",
    sum(lista_piloto$versao_ab == "B_reduzido"), "                          ║\n")
cat("║ Respostas esperadas:",
    ceiling(nrow(lista_piloto) * 0.075), "                              ║\n")
cat("╠══════════════════════════════════════════════════════════╣\n")
cat("║ O piloto mede:                                         ║\n")
cat("║  1. Taxa geral de resposta                             ║\n")
cat("║  2. Taxa por versão A vs B                             ║\n")
cat("║  3. Taxa por tempo desde último atendimento            ║\n")
cat("║  4. Interação tempo × versão                           ║\n")
cat("║  5. Taxa por USF vs USF+                               ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")