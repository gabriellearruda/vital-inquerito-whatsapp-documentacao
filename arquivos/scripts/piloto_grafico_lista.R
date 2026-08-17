path_file <- '/Users/renatoteixeira/Library/CloudStorage/Box-Box/Data Science/DCNT/Inquéritos/Mais dados mais saúde/_recife/amostragem/planejamento_amostral_operacional/'
lista_piloto <- read_xlsx(paste0(path_file, "00_PILOTO_lista_disparo_ds123.xlsx"))# =============================================================

# GRÁFICOS DESCRITIVOS DO PILOTO
# Exporta PNG para compartilhar com parceiros
# =============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)  # install.packages("patchwork")

# Tema padrão
tema <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#333333"),
    plot.subtitle = element_text(size = 9, color = "#888888"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

cores_tipo <- c("USF" = "#4A6A9A", "USF+" = "#7F77DD")
cores_ab <- c("A_completo" = "#4A8A7A", "B_reduzido" = "#E8913A")
cores_tempo <- c("Recente (≤90d)" = "#4CAF50", "Intermediário (91d-1a)" = "#FFC107",
                 "Antigo (>1a)" = "#F44336", "Sem info" = "#9E9E9E")

pop_piloto <- dados_amostra %>%
  filter(distrito %in% unique(lista_piloto$distrito))

# =============================================================
# G1 — Convites por estrato
# =============================================================

g1 <- lista_piloto %>%
  count(distrito, tipo_unidade, name = "n") %>%
  mutate(estrato = paste(distrito, "×", tipo_unidade)) %>%
  ggplot(aes(x = reorder(estrato, n), y = n, fill = tipo_unidade)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = format(n, big.mark = ".")), hjust = -0.1, size = 3.5) +
  coord_flip(ylim = c(0, max(table(lista_piloto$distrito, lista_piloto$tipo_unidade)) * 1.15)) +
  scale_fill_manual(values = cores_tipo) +
  labs(title = "Convites por estrato",
       subtitle = paste("Total:", format(nrow(lista_piloto), big.mark = "."), "convites"),
       x = NULL, y = "Convites") +
  tema

# =============================================================
# G2 — Teste A/B por tipo de unidade
# =============================================================

g2 <- lista_piloto %>%
  count(tipo_unidade, versao_ab, name = "n") %>%
  ggplot(aes(x = tipo_unidade, y = n, fill = versao_ab)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = n), position = position_dodge(0.6), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = cores_ab,
                    labels = c("A — Completo", "B — Reduzido")) +
  labs(title = "Teste A/B por tipo de unidade",
       subtitle = "Versão do questionário",
       x = NULL, y = "Convites") +
  tema

# =============================================================
# G3 — Perfil: Piloto vs População (sexo, faixa, raça)
# =============================================================

perfil_comp <- bind_rows(
  bind_rows(
    pop_piloto %>% count(sexo) %>%
      mutate(fonte = "População DS", pct = n / sum(n) * 100) %>%
      rename(categoria = sexo) %>% mutate(var = "Sexo"),
    lista_piloto %>% count(sexo) %>%
      mutate(fonte = "Piloto", pct = n / sum(n) * 100) %>%
      rename(categoria = sexo) %>% mutate(var = "Sexo")
  ),
  bind_rows(
    pop_piloto %>% count(faixa_etaria) %>%
      mutate(fonte = "População DS", pct = n / sum(n) * 100) %>%
      rename(categoria = faixa_etaria) %>% mutate(var = "Faixa etária"),
    lista_piloto %>% count(faixa_etaria) %>%
      mutate(fonte = "Piloto", pct = n / sum(n) * 100) %>%
      rename(categoria = faixa_etaria) %>% mutate(var = "Faixa etária")
  ),
  bind_rows(
    pop_piloto %>% count(raca_cor) %>%
      mutate(fonte = "População DS", pct = n / sum(n) * 100) %>%
      rename(categoria = raca_cor) %>% mutate(var = "Raça/cor"),
    lista_piloto %>% count(raca_cor) %>%
      mutate(fonte = "Piloto", pct = n / sum(n) * 100) %>%
      rename(categoria = raca_cor) %>% mutate(var = "Raça/cor")
  )
)

g3 <- perfil_comp %>%
  ggplot(aes(x = categoria, y = pct, fill = fonte)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(0.6), vjust = -0.5, size = 2.8) +
  facet_wrap(~var, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = c("População DS" = "#CCCCCC", "Piloto" = "#7F77DD")) +
  labs(title = "Perfil demográfico: Piloto vs População dos DS",
       subtitle = "Aderência da alocação direcionada",
       x = NULL, y = "%") +
  tema +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))

# =============================================================
# G4 — Tempo desde último atendimento por tipo
# =============================================================

g4 <- lista_piloto %>%
  count(tempo_atend, tipo_unidade) %>%
  group_by(tipo_unidade) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = tempo_atend, y = pct, fill = tipo_unidade)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(0.6), vjust = -0.5, size = 3) +
  scale_fill_manual(values = cores_tipo) +
  labs(title = "Tempo desde último atendimento por tipo de unidade",
       subtitle = "Distribuição no piloto",
       x = NULL, y = "%") +
  tema +
  theme(axis.text.x = element_text(size = 9))

# =============================================================
# G5 — Tempo × versão A/B
# =============================================================

g5 <- lista_piloto %>%
  count(tempo_atend, versao_ab) %>%
  ggplot(aes(x = tempo_atend, y = n, fill = versao_ab)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = n), position = position_dodge(0.6), vjust = -0.5, size = 3) +
  scale_fill_manual(values = cores_ab,
                    labels = c("A — Completo", "B — Reduzido")) +
  labs(title = "Tempo último atendimento × Versão A/B",
       subtitle = "Balanceamento do teste",
       x = NULL, y = "Convites") +
  tema +
  theme(axis.text.x = element_text(size = 9))

# =============================================================
# G6 — Convites por USF (top 20)
# =============================================================

g6 <- lista_piloto %>%
  count(distrito, tipo_unidade, ds_usf, name = "n") %>%
  arrange(desc(n)) %>%
  head(20) %>%
  mutate(label = paste0(substr(ds_usf, 1, 35), " (", distrito, ")")) %>%
  ggplot(aes(x = reorder(label, n), y = n, fill = tipo_unidade)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3) +
  coord_flip(ylim = c(0, max(table(lista_piloto$ds_usf)) * 1.2)) +
  scale_fill_manual(values = cores_tipo) +
  labs(title = "Top 20 USFs — Convites no piloto",
       x = NULL, y = "Convites") +
  tema +
  theme(axis.text.y = element_text(size = 7))

# =============================================================
# PAINEL COMPOSTO (A4 landscape)
# =============================================================

painel_superior <- g1 + g2 + plot_layout(widths = c(1, 1))
painel_meio <- g3
painel_inferior <- g4 + g5 + plot_layout(widths = c(1, 1))

painel_final <- (painel_superior / painel_meio / painel_inferior) +
  plot_annotation(
    title = "Piloto — Pesquisa APS Recife (USF vs USF+)",
    subtitle = paste0("DS selecionados: ", paste(unique(lista_piloto$distrito), collapse = " + "),
                      " | ", format(nrow(lista_piloto), big.mark = "."), " convites",
                      " | Versões A/B balanceadas"),
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, color = "#333333"),
      plot.subtitle = element_text(size = 10, color = "#888888")
    )
  )

# Salvar painel
ggsave(paste0(path_file, "piloto_painel_descritivo.png"),
       painel_final, width = 14, height = 12, dpi = 300, bg = "white")

cat("✅ Exportado: piloto_painel_descritivo.png (14×12in, 300dpi)\n")

# Salvar gráfico de USFs separado (muito largo para o painel)
ggsave(paste0(path_file, "piloto_top20_usfs.png"),
       g6, width = 10, height = 6, dpi = 300, bg = "white")

cat("✅ Exportado: piloto_top20_usfs.png\n")

# =============================================================
# SALVAR GRÁFICOS INDIVIDUAIS (opcional)
# =============================================================
graficos[3]
graficos <- list(g1, g2, g3, g4, g5, g6)
nomes <- c("convites_estrato", "teste_ab_tipo", "perfil_demografico",
           "tempo_atend_tipo", "tempo_ab", "top20_usfs")

for (i in seq_along(graficos)) {
  ggsave(paste0(path_file, "piloto_", nomes[i], ".png"),
         graficos[[i]], width = 8, height = 5, dpi = 300, bg = "white")
}

cat("✅ Exportados 6 gráficos individuais\n")