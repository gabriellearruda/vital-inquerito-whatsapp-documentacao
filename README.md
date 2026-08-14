# Inquérito APS Recife — Documentação do Projeto

Documentação metodológica do inquérito de avaliação de acesso e qualidade da Atenção Primária à Saúde (APS) em Recife, conduzido pelo programa **Mais Dados Mais Saúde (MDMS)** via WhatsApp.

🔗 **Site:** [gabriellearruda.github.io/vital-inquerito-whatsapp-documentacao](https://gabriellearruda.github.io/vital-inquerito-whatsapp-documentacao/)

---

## Sobre o projeto

O estudo compara Unidades de Saúde da Família convencionais (USF) e expandidas (USF+) no município de Recife, avaliando diferenças na percepção dos usuários sobre acesso e qualidade do cuidado. O inquérito é entregue via WhatsApp, usando a base cadastral do e-SUS APS como frame amostral — uma metodologia inédita no município que reduz custos e ciclos de coleta em relação a pesquisas telefônicas ou presenciais tradicionais.

## Parceiros

| Organização | Papel |
|---|---|
| Umane | Coordenação executiva |
| Vital Strategies | Parceria técnica principal |
| UFPel | Metodologia e análise estatística |
| Instituto Devive | Apoio institucional |
| Resolve to Save Lives | Apoio institucional |
| SMS Recife | Dados e parceria local |

## Estrutura da documentação

O site de documentação está organizado em seis seções:

1. **Contexto e Objetivos** — escopo, parceiros, perguntas de pesquisa e linha do tempo
2. **Aspectos Técnicos** — arquitetura, fluxo do questionário, plano amostral e pipeline R
3. **Governança e Ética** — CEP, LGPD, TCLE e protocolo para casos sensíveis
4. **Operação e Campo** — estratégia de ondas, monitoramento e suporte
5. **Arquivos de Referência** — documentos, scripts R e dados
6. **Desafios e Aprendizados** — o que funcionou, desafios e recomendações

## Repositório de scripts

Os scripts R de planejamento amostral, análise descritiva do piloto e ponderação estão no repositório de scripts do projeto (acesso restrito):

```
planejamento_amostral_dados_aps.R
planejamento_amostral_dados_aps_TIPO_UNIDADE_E_DISTRITO.R
planejamento_amostral_dados_aps_USF.R
piloto_analile_pre_coleta.R
piloto_descritiva.R
piloto_grafico_lista.R
ponderacao_analise_mdms_recife_EXemplo.R
dados_censo_2022_RECIFE.R
```

## Como ativar o GitHub Pages

1. Acesse **Settings → Pages** no repositório
2. Em *Branch*, selecione `main` e pasta `/root`
3. Clique em **Save**

O site ficará disponível em `https://gabriellearruda.github.io/vital-inquerito-whatsapp-documentacao/`

---

*Mais Dados Mais Saúde · Recife · 2025*
