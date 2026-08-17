#####################################################################

# Planeja,memtop amostral MDMS WhatsApp em Recife

#####################################################################



library(tidyverse)
library(sidrar)


# install.packages("sidrar")
library(sidrar)

# Tabela 4714 - População residente, por situação do domicílio e sexo
# Censo 2022
# Nível territorial: Município (código de Recife = 2611606)
# Primeiro, verificar o que essa tabela oferece
info_sidra(9514)

library(sidrar)

# Usando a string da API do SIDRA diretamente
dados <- get_sidra(api = "/t/9514/n6/2611606/v/93/p/last/c2/4,5/c287/all")

head(dados)




##### Dados mais desagregados

# install.packages("censobr")
library(censobr)


# Dados de população agregados por setor censitário (Censo 2022)
setores <- read_tracts(
  year = 2022,
  dataset = "Basico"   # dados básicos por setor censitário
)

# Filtrar para Recife (código do município: 2611606)
recife <- setores |>
  filter(code_muni == 2611606)

# Variável Descrição
# V0001 População residente
# V0002 Total de domicílios particulares
# V0003 Domicílios particulares ocupados
# V0004 Domicílios particulares não ocupados
# V0005 Média de moradores por domicílio particular ocupado
# V0006 Densidade demográfica (habitantes por hectare)
# V0007 Domicílios particulares ocupados com responsável do sexo masculino

#
# Dados de pessoa por setor censitário
pessoa <- read_tracts(year = 2022, dataset = "Pessoas")

# Ver as colunas disponíveis
names(pessoa)

# Ou o schema completo
schema(pessoa)


pessoa_recife <- pessoa |>
  filter(code_muni == 2611606) |>
  collect()

unique(pessoa_recife$name_subdistrict)

data_dictionary(year = 2022, dataset = "population")
data_dictionary(year = 2022, dataset = "households")
dic <- data_dictionary(year = 2022, dataset = "tracts")

# Ver o conteúdo
View(dic)

# Ou
print(dic, n = 100)
