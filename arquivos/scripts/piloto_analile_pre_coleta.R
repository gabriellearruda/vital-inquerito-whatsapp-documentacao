
# =============================================================
# ANÁLISE PÓS-PILOTO (rodar após receber as respostas)
# =============================================================

# Supondo que 'respostas_piloto' tem os IDs que responderam
# respostas_piloto <- read_xlsx("respostas_piloto.xlsx")

lista_piloto <- lista_piloto %>%
  mutate(respondeu = `Identificador Cidadão` %in% respostas_piloto$id)

cat("=== RESULTADOS DO PILOTO ===\n")

cat("\n--- Taxa geral ---\n")
cat("Taxa:", round(mean(lista_piloto$respondeu) * 100, 1), "%\n")

cat("\n--- Taxa por versão A/B ---\n")
lista_piloto %>%
  group_by(versao_ab) %>%
  summarise(
    n = n(), resp = sum(respondeu),
    taxa = round(resp / n * 100, 1),
    .groups = "drop") %>% print()

cat("\n--- Taxa por tempo_atend ---\n")
lista_piloto %>%
  group_by(tempo_atend) %>%
  summarise(
    n = n(), resp = sum(respondeu),
    taxa = round(resp / n * 100, 1),
    .groups = "drop") %>% print()

cat("\n--- Taxa por tempo_atend × versão ---\n")
lista_piloto %>%
  group_by(tempo_atend, versao_ab) %>%
  summarise(
    n = n(), resp = sum(respondeu),
    taxa = round(resp / n * 100, 1),
    .groups = "drop") %>%
  pivot_wider(names_from = versao_ab, values_from = c(n, resp, taxa)) %>%
  print()

cat("\n--- Taxa por tipo_unidade ---\n")
lista_piloto %>%
  group_by(tipo_unidade) %>%
  summarise(
    n = n(), resp = sum(respondeu),
    taxa = round(resp / n * 100, 1),
    .groups = "drop") %>% print()