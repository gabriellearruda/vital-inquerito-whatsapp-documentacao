

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
# PLANEJAMENTO AMOSTRAL FINAL ----
# Regra: estratos com n < 100 → censo (disparo total)
#        estratos com n ≥ 100 → amostragem
#        Indígenas → censo (todos)
# Estrato = ds_usf × sexo × faixa_etaria × raca_cor
# Todas as USF incluídas | deff = 1.0
# =============================================================

library(dplyr)
library(tidyr)
library(stringr)

# 
# 1. PREPARAÇÃO
# 

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


# 
# 2. SEPARAR INDÍGENAS (CENSO COMPLETO)
# 

dados_indigena <- dados %>% filter(raca_cor == "Indígena")
dados_amostra  <- dados %>% filter(raca_cor != "Indígena")

cat("\n=== Indígenas (censo) ===\n")
cat("Total:", nrow(dados_indigena), "\n")
print(table(dados_indigena$tipo_unidade))


# 
# 3. CONSTRUIR ESTRATOS: ds_usf × sexo × faixa_etaria × raca_cor
# 

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


# 
# 4. CLASSIFICAR ESTRATOS: CENSO vs AMOSTRA
# 

limite_censo <- 50  # Estratos com N < 100 → disparo total

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


# 
# 5. CÁLCULO DA AMOSTRA NOS ESTRATOS ≥ 100
# 

# Parâmetros do cálculo por poder
alpha       <- 0.05
poder       <- 0.80
sigma       <- 2.0
delta       <- 0.3
z_alpha     <- qnorm(1 - alpha / 2)
z_beta      <- qnorm(poder)
tx_resposta <- 0.05

# n por grupo (USF e USF+)
n_por_grupo <- ceiling((z_alpha + z_beta)^2 * 2 * sigma^2 / delta^2)
n_por_grupo

df_resumo_texto <- data.frame(Resumo="Resumo amostragem MDMS-Recife APS")

df_resumo_texto <- rbind(df_resumo_texto,data.frame(Resumo=paste0("Parâmetros do cálculo para tamanho de amostra: α=", alpha, ", Poder=", poder, ", σ=", sigma, ", δ=", delta)))
df_resumo_texto <- rbind(df_resumo_texto,data.frame(Resumo=paste0("Taxa de resposta considerada:", round(tx_resposta*100,1), "%")))

df_resumo_texto <- rbind(df_resumo_texto,data.frame(Resumo=paste0("n por tipo de USF (respostas):", n_por_grupo)))

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


# 
# 6. CALCULAR CONVITES POR ESTRATO
# 

# Estratos CENSO: convites = todos os cadastrados
# Estratos AMOSTRA: convites proporcionais ao tamanho do estrato

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

cat("\n=== Amostra por estrato (primeiros 30) ===\n")
print(
  head(estratos_final %>%
         select(ds_usf, tipo_unidade, sexo, faixa_etaria, raca_cor,
                N_estrato, tipo_disparo, n_convites, n_respostas_esp, peso_base), 30)
)


hist(estratos_final$n_respostas_esp)

# 
# 7. RESUMO GERAL
# 

cat("\n══════════════════════════════════════════════════════════\n")
cat("  RESUMO DA AMOSTRA\n")
cat("══════════════════════════════════════════════════════════\n")

# Por tipo de disparo e tipo de unidade
resumo <- estratos_final %>%
  group_by(tipo_unidade, tipo_disparo) %>%
  summarise(
    n_estratos = n(),
    cadastrados = sum(N_estrato),
    convites = sum(n_convites),
    respostas_esp = sum(n_respostas_esp),
    .groups = "drop"
  )

print(resumo)

# Totais gerais
cat("\n=== TOTAIS ===\n")
total_convites_amostra <- sum(estratos_final$n_convites)
total_respostas_esp <- sum(estratos_final$n_respostas_esp)

cat("Convites (estratos amostrais + censo n<100):", total_convites_amostra, "\n")
cat("Convites (indígenas - censo):", nrow(dados_indigena), "\n")
cat("TOTAL DE CONVITES:", total_convites_amostra + nrow(dados_indigena), "\n")
cat("Respostas esperadas (amostra):", total_respostas_esp, "\n")
cat("Respostas esperadas (indígenas):", ceiling(nrow(dados_indigena) * tx_resposta), "\n")
cat("TOTAL RESPOSTAS ESPERADAS:",
    total_respostas_esp + ceiling(nrow(dados_indigena) * tx_resposta), "\n")

df_resumo_texto <- rbind(df_resumo_texto,data.frame(Resumo=paste0("Convites (estratos amostrais + censo n<100):", total_convites_amostra)))
df_resumo_texto <- rbind(df_resumo_texto,data.frame(Resumo=paste0("Convites (indígenas - censo):", nrow(dados_indigena))))
df_resumo_texto <- rbind(df_resumo_texto,data.frame(Resumo=paste0("TOTAL DE CONVITES:", total_convites_amostra + nrow(dados_indigena))))
df_resumo_texto <- rbind(df_resumo_texto,data.frame(Resumo=paste0("Respostas esperadas (amostra):", total_respostas_esp)))
df_resumo_texto <- rbind(df_resumo_texto,data.frame(Resumo=paste0("Respostas esperadas (indígenas):", ceiling(nrow(dados_indigena) * tx_resposta))))
df_resumo_texto <- rbind(df_resumo_texto,data.frame(Resumo=paste0("TOTAL RESPOSTAS ESPERADAS:",
                                                                  total_respostas_esp + ceiling(nrow(dados_indigena) * tx_resposta))))
df_resumo_texto
# 
# 8. VERIFICAR REPRESENTATIVIDADE POR SUBGRUPO
# 

cat("\n══════════════════════════════════════════════════════════\n")
cat("  REPRESENTATIVIDADE: RESPOSTAS ESPERADAS POR SUBGRUPO\n")
cat("══════════════════════════════════════════════════════════\n")

# Respostas esperadas por sexo × tipo
resp_sexo <- estratos_final %>%
  group_by(tipo_unidade, sexo) %>%
  summarise(
    respostas_esp = sum(n_respostas_esp),
    .groups = "drop"
  )

cat("\n--- Por sexo ---\n")
print(resp_sexo)

# Por faixa etária × tipo
resp_faixa <- estratos_final %>%
  group_by(tipo_unidade, faixa_etaria) %>%
  summarise(
    respostas_esp = sum(n_respostas_esp),
    .groups = "drop"
  )

cat("\n--- Por faixa etária ---\n")
print(resp_faixa)

# Por raça/cor × tipo
resp_raca <- estratos_final %>%
  group_by(tipo_unidade, raca_cor) %>%
  summarise(
    respostas_esp = sum(n_respostas_esp),
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


# 
# 9. AGREGAR POR UNIDADE (para operacionalização)
# 

cat("\n══════════════════════════════════════════════════════════\n")
cat("  CONVITES POR UNIDADE (operacional)\n")
cat("══════════════════════════════════════════════════════════\n")

convites_por_unidade <- estratos_final %>%
  group_by(distrito, CNES, ds_usf, tipo_unidade) %>%
  summarise(
    N_cadastrados = sum(N_estrato),
    n_convites = sum(n_convites),
    n_censo = sum(n_convites[tipo_disparo == "CENSO"]),
    n_amostra = sum(n_convites[tipo_disparo == "AMOSTRA"]),
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
    total_censo = sum(n_censo,n.rm=T),
    total_amostra = sum(n_amostra,na.rm=T),
    pct_medio_disparo = round(mean(pct_disparo), 1),
    .groups = "drop"
  ) %>%
  print()


# 
# 10. TABELA DE PÓS-ESTRATIFICAÇÃO
# 

# Para estratos amostrais: pós-estratificação por sexo × faixa × raça
tab_pos <- dados_amostra %>%
  count(tipo_unidade, sexo, faixa_etaria, raca_cor, name = "Freq") %>%
  arrange(tipo_unidade, sexo, faixa_etaria, raca_cor)

cat("\n=== TABELA DE PÓS-ESTRATIFICAÇÃO ===\n")
cat("Células:", nrow(tab_pos), "\n")


# 
# 11. ESTRUTURA DOS PESOS
# 

cat("\n══════════════════════════════════════════════════════════\n")
cat("  ESTRUTURA DOS PESOS AMOSTRAIS\n")
cat("══════════════════════════════════════════════════════════\n")
cat("
Três tipos de peso conforme o tipo de disparo:

1. ESTRATOS CENSO (n < 100):
   peso_base = 1
   Todos foram convidados, não há seleção.

2. ESTRATOS AMOSTRA (n ≥ 100):
   peso_base = N_estrato / n_convites
   Inverso da fração amostral no estrato.

3. INDÍGENAS (censo total):
   peso_base = 1

Após calcular o peso_base, aplicar pós-estratificação
com survey::postStratify() usando a tabela em tab_pos
para calibrar por sexo × faixa_etaria × raca_cor.
\n")


# 
# 12. RESUMO EXECUTIVO FINAL
# 

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║           PLANEJAMENTO AMOSTRAL FINAL                      ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║ DESENHO                                                    ║\n")
cat("║   Todas as USF e USF+ incluídas (deff = 1.0)              ║\n")
cat("║   Estratos: ds_usf × sexo × faixa_etaria × raca_cor       ║\n")
cat("║   Estrato n < 100 → censo (disparo total)                 ║\n")
cat("║   Estrato n ≥ 100 → amostragem proporcional              ║\n")
cat("║   Indígenas → censo total                                 ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║ CÁLCULO                                                    ║\n")
cat("║   Poder: 80% | α: 5% | σ:", sigma, "| δ:", delta, "pt        ║\n")
cat("║   n por grupo:", n_por_grupo, "respostas                      ║\n")
cat("║   Taxa de resposta:", tx_resposta * 100, "%                          ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")

total_conv <- sum(convites_por_unidade$n_convites) + nrow(dados_indigena)
total_resp <- sum(estratos_final$n_respostas_esp) +
  ceiling(nrow(dados_indigena) * tx_resposta)

cat("║ CONVITES                                                   ║\n")
cat("║   Estratos censo (n<100):",
    formatC(sum(estratos_final$n_convites[estratos_final$tipo_disparo == "CENSO"]),
            format = "d", big.mark = "."), "                          ║\n")
cat("║   Estratos amostra (n≥100):",
    formatC(sum(estratos_final$n_convites[estratos_final$tipo_disparo == "AMOSTRA"]),
            format = "d", big.mark = "."), "                         ║\n")
cat("║   Indígenas (censo):",
    formatC(nrow(dados_indigena), format = "d", big.mark = "."), "                              ║\n")
cat("║   TOTAL:",
    formatC(total_conv, format = "d", big.mark = "."), "                                     ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║ RESPOSTAS ESPERADAS:",
    formatC(total_resp, format = "d", big.mark = "."), "                            ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║ PONDERAÇÃO                                                 ║\n")
cat("║   Censo: peso = 1                                         ║\n")
cat("║   Amostra: peso = N_estrato / n_convites                  ║\n")
cat("║   Pós-estratificação: sexo × faixa × raça/cor              ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")


# 
# 13. EXPORTAR
# 

write.csv(estratos_final, "01_estratos_amostrais.csv", row.names = FALSE)
write.csv(convites_por_unidade, "02_convites_por_unidade.csv", row.names = FALSE)
write.csv(tab_pos, "03_tab_pos_estratificacao.csv", row.names = FALSE)

# Lista de usuários indígenas (censo)
write.csv(
  dados_indigena %>% select(distrito, CNES, ds_usf, tipo_unidade,
                            `Identificador Cidadão`, sexo, faixa_etaria, raca_cor),
  "04_indigenas_censo.csv", row.names = FALSE
)

cat("\n✅ Arquivos exportados:\n")
cat("   01_estratos_amostrais.csv (cada estrato com tipo_disparo e n_convites)\n")
cat("   02_convites_por_unidade.csv (agregado por USF para operacionalização)\n")
cat("   03_tab_pos_estratificacao.csv (gabarito para pós-estratificação)\n")
cat("   04_indigenas_censo.csv (lista de indígenas para disparo total)\n")







#### Operacional ----

# 
# TABELAS OPERACIONAIS PARA EQUIPE DE DISPARO
# Gera: (1) resumo por unidade e (2) lista nominal de disparo
# 

library(dplyr)
library(tidyr)
library(stringr)

# 
# 1. GERAR LISTA DE DISPARO (nível do usuário)
# 

# --- 1.1 Classificar cada USUÁRIO no seu estrato ---
dados_classificado <- dados_amostra %>%
  left_join(
    estratos_final %>%
      select(ds_usf, CNES, sexo, faixa_etaria, raca_cor,
             N_estrato, tipo_disparo, n_convites),
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
    n_sort <- min(.x$n_convites[1], nrow(.x))
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


# 
# 2. TABELA 1: RESUMO POR UNIDADE (para planejamento)
# 

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


# 
# 3. TABELA 2: LISTA NOMINAL DE DISPARO (operacional)
# 

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

cat("\n=== TABELA DE DISPARO (primeiras 20 linhas) ===\n")
print(head(tabela_disparo, 20))


# 
# 4. TABELA 3: CONTROLE DE DISPAROS (para acompanhamento)
# 

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


# 
# 5. EXPORTAR
# 

# Tabela resumo (para gestão)
write.csv(tabela_resumo,
          "OPERACIONAL_01_resumo_por_unidade.csv",
          row.names = FALSE)

# Lista de disparo (para a equipe técnica)
write.csv(tabela_disparo,
          "OPERACIONAL_02_lista_disparo.csv",
          row.names = FALSE)

# Tabela de controle (para acompanhamento)
write.csv(tabela_controle,
          "OPERACIONAL_03_controle_disparos.csv",
          row.names = FALSE)

cat("\n✅ Arquivos operacionais exportados:\n")
cat("   OPERACIONAL_01_resumo_por_unidade.csv\n")
cat("   OPERACIONAL_02_lista_disparo.csv\n")
cat("   OPERACIONAL_03_controle_disparos.csv\n")


#
# 6. VALIDAÇÕES
#

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