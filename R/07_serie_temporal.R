################################################################################
# 07_serie_temporal.R
#
# Para CADA EVENTO de CADA PROCESSO (nao um snapshot de "situacao hoje"),
# classifica se a atividade minerária estava, naquele momento da linha do
# tempo, PERMITIDA (por classe de titulo), NAO_PERMITIDA ou NEUTRA.
#
# Duas saidas SEPARADAS (decisao 2026-08: uma nao sobrescreve a outra):
#
#   1) 07_serie_temporal_eventos  -- 1 linha por evento, com rotulo_permissao:
#        PERMITIDA_CONC_LAV / PERMITIDA_LICEN / PERMITIDA_PLG /
#        PERMITIDA_REG_EXT / PERMITIDA_PESQUISA / NAO_PERMITIDA / NEUTRO
#
#   2) 07_motivos_fechamento -- SO os eventos de fechamento/suspensao
#      (papel FECHA ou SUSPENDE), com o motivo especifico (caducidade,
#      vencimento, decisao judicial, etc) e a origem (judicial/administrativo)
#
# Depende de:
#   - data/result_db/microdados/micro_processo_evento.parquet (saida do 04)
#   - R/dicionario_eventos_classificado_v2.csv (saida de
#     checks/gerar_dicionario_enriquecido.R -- roda ele ANTES deste script
#     sempre que o dicionario base mudar)
#
# ESCOPO (decisao 2026-08): este e o "07 novo", cobrindo SO a classificacao
# de permissao por evento + motivo de fechamento -- os demais blocos do 07
# antigo (vigencia de titular/substancia, licenca/GU como serie temporal,
# situacao documental, propriedade do solo, protecao art 211/213, despacho
# judicial em texto livre, penalidades) NAO estao aqui. Ver mensagem
# separada com a lista completa do que fica de fora, pra decisao conjunta.
################################################################################

rm(list = ls(all.names = TRUE))
options(scipen = 999)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(stringi)
  library(arrow)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(sf)
  library(here)
})

source(here::here("R", "utils.R"))

MICRO_DIR <- here::here("data", "result_db", "microdados")
DIC_PATH  <- here::here("data", "_qa", "classificacao_temporal_permissao", "dicionario_eventos_classificado_v2.csv")
QA_DIR    <- here::here("data", "_qa", "07_serie_temporal")
SHINY_DIR <- here::here("shiny_dashboard")
dir.create(QA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SHINY_DIR, recursive = TRUE, showWarnings = FALSE)

FASES_QUE_OPERAM  <- c("CONC LAV", "LICEN", "PLG", "REG EXT")
FASES_DE_PESQUISA <- c("AUT PESQ")

# =============================================================================
# BLOCO 0 — CARREGA DICIONARIO V2 + EVENTOS, JUNTA
# =============================================================================

dic <- readr::read_csv(DIC_PATH, show_col_types = FALSE) |>
  dplyr::mutate(idevento = as.character(idevento))

ev_raw <- arrow::read_parquet(file.path(MICRO_DIR, "micro_processo_evento.parquet"))

eventos <- ev_raw |>
  dplyr::mutate(
    idevento = as.character(as.integer(idevento)),
    dtevento  = as.Date(dtevento)
  ) |>
  dplyr::left_join(
    dic |> dplyr::select(idevento, dsevento, tipo_proc, sufixo, papel,
                         categ_licenca, categ_gu, categ_barragem,
                         categ_acidente, categ_espacial, categ_judicial),
    by = "idevento"
  )
n_sem_data  <- sum(is.na(eventos$dtevento))
n_sem_match <- sum(is.na(eventos$papel))
message(sprintf("[07][0] eventos totais: %d | sem dtevento (excluidos): %d | sem match no dicionario: %d (%.2f%%)",
                nrow(eventos), n_sem_data, n_sem_match, 100 * n_sem_match / nrow(eventos)))
if (n_sem_match > 0) {
  readr::write_csv(
    eventos |> dplyr::filter(is.na(papel)) |> dplyr::count(idevento, sort = TRUE),
    file.path(QA_DIR, "00_ideventos_sem_match.csv")
  )
}

eventos <- eventos |> dplyr::filter(!is.na(dtevento)) |> dplyr::arrange(processo, dtevento)

# =============================================================================
# BLOCO A — MAQUINA DE ESTADOS: rotulo_permissao por evento
# =============================================================================
# RETOMA reverte tanto SUSPENDE quanto FECHA (o proprio classificador do
# dicionario trata TORNA S/EFEITO de CADUCID/INDEFER/CANCEL/CASSA/DECAIMENTO/
# RENUNCIA/DESISTENCIA como RETOMA, nao so suspensao judicial) -- por isso
# guardamos "rotulo antes do ultimo impedimento", nao so "antes da suspensao".

classificar_processo_fast <- function(dt, papel, tp, sufixo, categ_licenca) {
  n <- length(dt)
  ord <- order(dt)
  papel <- papel[ord]; tp <- tp[ord]; sufixo <- sufixo[ord]; categ_licenca <- categ_licenca[ord]

  rotulo <- character(n)
  atual <- "NEUTRO"
  rotulo_pre_impedimento <- NA_character_
  licenca_vista <- FALSE

  for (i in seq_len(n)) {
    p <- papel[i]
    if (is.na(p)) p <- "NEUTRO_outros"

    if (!is.na(categ_licenca[i]) && categ_licenca[i] && !is.na(sufixo[i]) && sufixo[i] == "PROTOC") {
      licenca_vista <- TRUE
    }

    if (p == "MUDA_FASE") {
      tpi <- tp[i]
      if (!is.na(tpi) && tpi %in% FASES_QUE_OPERAM) {
        novo <- paste0("PERMITIDA_", gsub(" ", "_", tpi, fixed = TRUE))
      } else if (!is.na(tpi) && tpi %in% FASES_DE_PESQUISA) {
        novo <- "PERMITIDA_PESQUISA"
      } else {
        novo <- "NEUTRO"
      }
    } else if (p == "FECHA" || p == "SUSPENDE") {
      novo <- "NAO_PERMITIDA"
    } else if (p == "RETOMA") {
      novo <- if (!is.na(rotulo_pre_impedimento)) rotulo_pre_impedimento else atual
    } else {
      novo <- atual
    }

    if ((p == "FECHA" || p == "SUSPENDE") && atual != "NAO_PERMITIDA") rotulo_pre_impedimento <- atual

    atual <- novo
    rotulo[i] <- novo
  }

  out <- character(n)
  out[ord] <- rotulo
  out
}

t0 <- Sys.time()
by_proc <- split(
  eventos |> dplyr::select(dtevento, papel, tipo_proc, sufixo, categ_licenca),
  eventos$processo
)
resultados <- vector("list", length(by_proc))
nomes <- names(by_proc)
for (k in seq_along(by_proc)) {
  g <- by_proc[[k]]
  resultados[[k]] <- classificar_processo_fast(g$dtevento, g$papel, g$tipo_proc, g$sufixo, g$categ_licenca)
}
eventos_por_proc <- split(seq_len(nrow(eventos)), eventos$processo)
eventos$rotulo_permissao <- NA_character_
for (k in seq_along(by_proc)) {
  eventos$rotulo_permissao[eventos_por_proc[[nomes[k]]]] <- resultados[[k]]
}
t1 <- Sys.time()
message(sprintf("[07][A] maquina de estados rodou em %.1f segundos | %d processos | %d eventos",
                as.numeric(difftime(t1, t0, units = "secs")), length(by_proc), nrow(eventos)))

message("\n[07][A] distribuicao geral do rotulo:")
dist_geral <- dplyr::count(eventos, rotulo_permissao, sort = TRUE) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 2))
print(dist_geral, n = Inf)
readr::write_csv(dist_geral, file.path(QA_DIR, "01_distribuicao_geral_rotulo.csv"))

save_ckpt(eventos, "07_serie_temporal_eventos")
message("[07][A] checkpoint salvo: 07_serie_temporal_eventos (", nrow(eventos), " linhas)")

# =============================================================================
# BLOCO B — TABELA SEPARADA: MOTIVO DE FECHAMENTO/SUSPENSAO
# =============================================================================
# So os eventos com papel FECHA ou SUSPENDE. NAO sobrescreve o bloco A --
# checkpoint proprio.

fechamentos <- eventos |>
  dplyr::filter(papel %in% c("FECHA", "SUSPENDE")) |>
  dplyr::mutate(
    resto = dplyr::if_else(
      stringr::str_starts(dsevento, stringr::fixed("TORNA S/EFEITO")) | !stringr::str_detect(dsevento, "/"),
      dsevento,
      stringr::str_trim(stringr::str_remove(dsevento, "^[^/]+/"))
    ),
    S = stringi::stri_trans_general(toupper(resto), "Latin-ASCII")
  ) |>
  dplyr::mutate(
    motivo = dplyr::case_when(
      # --- dentro de SUSPENDE ---
      papel == "SUSPENDE" & stringr::str_detect(S, "SUSPENSAO JUDICIAL") ~ "suspensao_judicial",
      papel == "SUSPENDE" & stringr::str_detect(S, "BLOQUEAD[AO] JUDICIALMENTE") ~ "bloqueio_judicial",
      papel == "SUSPENDE" & stringr::str_detect(S, "INTERDICAO") ~ "interdicao",
      papel == "SUSPENDE" & stringr::str_detect(S, "\\bEMBARGO\\b") ~ "embargo",
      papel == "SUSPENDE" & stringr::str_detect(S, "GUIA UTILIZACAO SUSPENSA|\\bGU\\b.*SUSPENSA") ~ "gu_suspensa",
      papel == "SUSPENDE" ~ "suspensao_administrativa_trabalhos",

      # --- dentro de FECHA ---
      stringr::str_detect(S, "CADUCID|CADUCAD[OA]|CADUCO") ~ "caducidade",
      stringr::str_detect(S, "(RENOVACAO|PRORROGACAO).*INDEFER|INDEFER.*(RENOVACAO|PRORROGACAO)") ~ "indeferimento_renovacao",
      stringr::str_detect(S, "\\bVENCID[OA]\\b") ~ "vencimento",
      stringr::str_detect(S, "NULIDADE|DECLARAD[OA] NUL[OA]") ~ "nulidade",
      stringr::str_detect(S, "REVOGACAO|REVOGAD[OA]") ~ "revogacao",
      stringr::str_detect(S, "CASSAD[OA]|CASSACAO") ~ "cassacao",
      stringr::str_detect(S, "EXTINCAO|EXTINT[OA]") ~ "extincao",
      stringr::str_detect(S, "RENUNCIA|DESISTENCIA") ~ "renuncia_desistencia",
      stringr::str_detect(S, "DECAIMENTO") ~ "decaimento_snuc",
      stringr::str_detect(S, "AREA BLOQUEADA") ~ "area_bloqueada",
      stringr::str_detect(S, "RELATORIO PESQUISA") & stringr::str_detect(S, "NAO APROVADO|ARQUIVADO") ~ "relatorio_reprovado_arquivado",
      stringr::str_detect(S, "CANCELAD[OA]|CANCELAMENTO") ~ "cancelamento",
      stringr::str_detect(S, "BAIXA TRANSCRI") ~ "baixa_transcricao",
      stringr::str_detect(S, "\\bARQUIVAMENTO\\b") ~ "arquivamento",
      stringr::str_detect(S, "INDEFERIMENTO") ~ "indeferimento",
      TRUE ~ "outro"
    ),
    origem = dplyr::case_when(
      stringr::str_detect(S, "JUDICIAL") ~ "judicial",
      motivo %in% c("suspensao_judicial", "bloqueio_judicial") ~ "judicial",
      TRUE ~ "administrativo"
    )
  ) |>
  dplyr::select(processo, dtevento, dsevento, tipo_proc, sufixo, papel, motivo, origem)

message("\n[07][B] distribuicao de motivo (so eventos FECHA/SUSPENDE):")
dist_motivo <- dplyr::count(fechamentos, papel, motivo, origem, sort = TRUE)
print(dist_motivo, n = Inf)
readr::write_csv(dist_motivo, file.path(QA_DIR, "02_distribuicao_motivo.csv"))

n_outro <- sum(fechamentos$motivo == "outro")
if (n_outro > 0) {
  message(sprintf("[07][B] %d evento(s) com motivo = 'outro' -- amostra em 03_motivo_outro_amostra.csv", n_outro))
  readr::write_csv(
    fechamentos |> dplyr::filter(motivo == "outro") |> dplyr::count(dsevento, sort = TRUE),
    file.path(QA_DIR, "03_motivo_outro_amostra.csv")
  )
}

save_ckpt(fechamentos, "07_motivos_fechamento")
message("[07][B] checkpoint salvo: 07_motivos_fechamento (", nrow(fechamentos), " linhas)")

# =============================================================================
# BLOCO C — CAMADA SEPARADA: PROTECAO DE VENCIMENTO POR RENOVACAO/PRORROGACAO
# =============================================================================
# NAO mistura com rotulo_permissao (Bloco A) nem motivos_fechamento (Bloco B)
# -- checkpoint proprio (decisao 2026-08: camada a parte).
#
# Pergunta que essa camada responde: pra um processo com titulo VENCIDO POR
# DATA (dt_vencimento < hoje, direto da ProcessoTitulo) e que o Bloco A AINDA
# mostra como PERMITIDA_* (nenhum evento FECHA/SUSPENDE capturou isso), existe
# protocolo de renovacao/prorrogacao ANTES do vencimento, sem indeferimento
# posterior? Se sim, esta PROTEGIDO -- nao deveria ser tratado como vencido
# de verdade, mesmo sem evento explicito de "renovado" publicado ainda.
#
# Fonte: Consolidacao Normativa DNPM 155/2016 (art. 211/213 p/ PLG; art.
# 182/196 p/ LICEN, confirmado por jurisprudencia TRF-4 -- protocolo TARDIO
# (apos o vencimento) NAO protege, so conta se vier antes).
#
# CONC LAV e caso especial: NAO tem vencimento por prazo fixo (Codigo de
# Mineracao -- prazo indeterminado, vinculado a exaustao da jazida,
# confirmado por doutrina/jurisprudencia). Um processo com registro tipo=4
# (a concessao em si) e vencimento NULO esta sempre "sem vencimento
# aplicavel" -- mesmo que um registro mais antigo (ex: alvara pre-concessao)
# mostre data vencida. Achado 2026-08: 99,6% do que parecia "gap" em
# CONC_LAV era esse artefato (script pegando o registro antigo por engano).
# =============================================================================

titulo_path <- file.path(MICRO_DIR, "micro_processo_titulo.parquet")

if (!file.exists(titulo_path)) {
  message("[07][C] micro_processo_titulo.parquet ausente -- pulando camada de protecao de vencimento.")
} else {

  titulo <- arrow::read_parquet(titulo_path) |>
    dplyr::mutate(dtvencimento = as.Date(dtvencimento), dtpublicacao = as.Date(dtpublicacao))

  HOJE <- Sys.Date()

  situacao_hoje <- eventos |>
    dplyr::group_by(processo) |>
    dplyr::slice_max(dtevento, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(processo, rotulo_permissao_hoje = rotulo_permissao)

  # --- CONC LAV sem vencimento por lei (registro tipo=4, vencimento nulo) ---
  conclav_sem_vencimento <- titulo |>
    dplyr::filter(idtipodocumentolegal == 4, is.na(dtvencimento)) |>
    dplyr::distinct(processo) |>
    dplyr::pull(processo)

  message(sprintf("[07][C] CONC_LAV com registro de titulo sem vencimento (sem prazo por lei): %d processos",
                  length(conclav_sem_vencimento)))

  # --- vencidos por data, excluindo os CONC_LAV ja resolvidos acima ---
  vencidos <- titulo |>
    dplyr::filter(!is.na(dtvencimento), dtvencimento < HOJE, !processo %in% conclav_sem_vencimento) |>
    dplyr::group_by(processo) |>
    dplyr::slice_max(dtvencimento, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(processo, dt_vencimento = dtvencimento) |>
    dplyr::left_join(situacao_hoje, by = "processo") |>
    dplyr::filter(stringr::str_starts(rotulo_permissao_hoje, "PERMITIDA"))

  message(sprintf("[07][C] processos vencidos por data, ainda PERMITIDA_* segundo o Bloco A: %d", nrow(vencidos)))

  REGRAS_RENOVACAO <- list(
    PERMITIDA_PLG      = list(protoc = 521, concedida = 523,                                    indef = 522),
    PERMITIDA_LICEN    = list(protoc = 755, concedida = 742,                                    indef = 744),
    PERMITIDA_REG_EXT  = list(protoc = 937, concedida = c(926, 927, 939, 940, 941),              indef = 938),
    PERMITIDA_PESQUISA = list(protoc = 265, concedida = c(271, 272, 275, 277, 324, 325, 326, 965), indef = c(197, 267))
  )

  ids_todos_relevantes <- as.character(unlist(lapply(REGRAS_RENOVACAO, function(r) c(r$protoc, r$concedida, r$indef))))
  ev_idx <- eventos |>
    dplyr::filter(idevento %in% ids_todos_relevantes) |>
    dplyr::select(processo, idevento, dtevento)
  ev_idx_por_proc <- split(ev_idx, ev_idx$processo)

  checar_protecao <- function(p, dt_venc, rotulo) {
    regra <- REGRAS_RENOVACAO[[rotulo]]
    if (is.null(regra)) return(list(protegido = NA, motivo = "sem_regra_para_este_tipo", dt_protocolo = as.Date(NA)))
    ev_p <- ev_idx_por_proc[[p]]
    if (is.null(ev_p)) return(list(protegido = FALSE, motivo = "sem_protocolo_antes_do_vencimento", dt_protocolo = as.Date(NA)))
    protocs <- ev_p$dtevento[ev_p$idevento %in% as.character(c(regra$protoc, regra$concedida))]
    indefs  <- ev_p$dtevento[ev_p$idevento %in% as.character(regra$indef)]
    protocs_antes <- protocs[protocs <= dt_venc]
    if (length(protocs_antes) == 0) return(list(protegido = FALSE, motivo = "sem_protocolo_antes_do_vencimento", dt_protocolo = as.Date(NA)))
    dt_ultimo <- max(protocs_antes)
    if (length(indefs) == 0 || !any(indefs > dt_ultimo)) {
      return(list(protegido = TRUE, motivo = "protocolo_antes_sem_indeferimento_posterior", dt_protocolo = dt_ultimo))
    }
    list(protegido = FALSE, motivo = "protocolo_indeferido_apos", dt_protocolo = dt_ultimo)
  }

  t0 <- Sys.time()
  resultado <- purrr::pmap_dfr(
    list(vencidos$processo, vencidos$dt_vencimento, vencidos$rotulo_permissao_hoje),
    function(p, dt, r) {
      res <- checar_protecao(p, dt, r)
      tibble::tibble(processo = p, protegido = as.logical(res$protegido), motivo_protecao = as.character(res$motivo),
                     dt_protocolo_renovacao = as.Date(res$dt_protocolo))
    }
  )
  t1 <- Sys.time()
  message(sprintf("[07][C] checagem de protecao rodou em %.1f segundos", as.numeric(difftime(t1, t0, units = "secs"))))

  protecao_vencimento <- vencidos |> dplyr::left_join(resultado, by = "processo")

  conclav_df <- tibble::tibble(
    processo = conclav_sem_vencimento,
    dt_vencimento = as.Date(NA),
    rotulo_permissao_hoje = "PERMITIDA_CONC_LAV",
    protegido = NA,
    motivo_protecao = "sem_vencimento_por_lei_prazo_indeterminado",
    dt_protocolo_renovacao = as.Date(NA)
  )

  protecao_vencimento <- dplyr::bind_rows(protecao_vencimento, conclav_df)

  message("\n[07][C] distribuicao de protecao (so quem estava PERMITIDA_* com titulo vencido por data):")
  dist_protecao <- dplyr::count(protecao_vencimento, rotulo_permissao_hoje, protegido, motivo_protecao, sort = TRUE)
  print(dist_protecao, n = Inf)
  readr::write_csv(dist_protecao, file.path(QA_DIR, "04_distribuicao_protecao_vencimento.csv"))

  n_gap_real <- sum(protecao_vencimento$protegido == FALSE, na.rm = TRUE)
  message(sprintf("\n[07][C] gap real (vencido, sem protecao de renovacao, ainda PERMITIDA_*): %d de %d avaliados (excluindo CONC_LAV sem prazo)",
                  n_gap_real, sum(!is.na(protecao_vencimento$protegido))))

  save_ckpt(protecao_vencimento, "07_protecao_vencimento_titulo")
  message("[07][C] checkpoint salvo: 07_protecao_vencimento_titulo (", nrow(protecao_vencimento), " linhas)")

  # ===========================================================================
  # BLOCO D — LINHA DO TEMPO AJUSTADA (Possibilidade A confirmada, 2026-08):
  # vencimento por data SEM protecao = fecha de verdade, a partir da DATA de
  # vencimento (nao antes). Insere um evento SINTETICO (marcado como tal,
  # nunca se confunde com evento real da ANM) e reprocessa so os processos
  # afetados pela mesma maquina de estados do Bloco A.
  #
  # NAO sobrescreve 07_serie_temporal_eventos (continua 100% evento real da
  # ANM, para auditoria). Este e um checkpoint NOVO -- a versao recomendada
  # pra responder "pode operar hoje?" na pratica (ex: aba 4).
  # ===========================================================================

  afetados <- protecao_vencimento |>
    dplyr::filter(protegido == FALSE) |>
    dplyr::select(processo, dt_vencimento)

  message(sprintf("\n[07][D] processos com vencimento sem protecao (evento sintetico sera inserido): %d", nrow(afetados)))

  eventos_sinteticos <- afetados |>
    dplyr::mutate(
      idevento = NA_character_,
      dtevento = dt_vencimento,
      dsevento = "VENCIMENTO POR DATA SEM RENOVACAO PROTOCOLADA (EVENTO SINTETICO)",
      tipo_proc = NA_character_,
      sufixo = NA_character_,
      papel = "FECHA",
      categ_licenca = FALSE, categ_gu = FALSE, categ_barragem = FALSE,
      categ_acidente = FALSE, categ_espacial = FALSE, categ_judicial = FALSE,
      evento_sintetico = TRUE
    ) |>
    dplyr::select(-dt_vencimento)

  eventos_processos_afetados <- eventos |>
    dplyr::filter(processo %in% afetados$processo) |>
    dplyr::mutate(evento_sintetico = FALSE) |>
    dplyr::bind_rows(eventos_sinteticos) |>
    dplyr::arrange(processo, dtevento)

  by_proc_d <- split(
    eventos_processos_afetados |> dplyr::select(dtevento, papel, tipo_proc, sufixo, categ_licenca),
    eventos_processos_afetados$processo
  )
  nomes_d <- names(by_proc_d)
  resultados_d <- vector("list", length(by_proc_d))
  for (k in seq_along(by_proc_d)) {
    g <- by_proc_d[[k]]
    resultados_d[[k]] <- classificar_processo_fast(g$dtevento, g$papel, g$tipo_proc, g$sufixo, g$categ_licenca)
  }
  idx_d <- split(seq_len(nrow(eventos_processos_afetados)), eventos_processos_afetados$processo)
  eventos_processos_afetados$rotulo_permissao <- NA_character_
  for (k in seq_along(by_proc_d)) {
    eventos_processos_afetados$rotulo_permissao[idx_d[[nomes_d[k]]]] <- resultados_d[[k]]
  }

  eventos_ajustados <- eventos |>
    dplyr::mutate(evento_sintetico = FALSE) |>
    dplyr::filter(!processo %in% afetados$processo) |>
    dplyr::bind_rows(eventos_processos_afetados) |>
    dplyr::arrange(processo, dtevento)

  message("[07][D] distribuicao geral do rotulo, na linha do tempo ajustada:")
  dist_ajustada <- dplyr::count(eventos_ajustados, rotulo_permissao, sort = TRUE) |>
    dplyr::mutate(pct = round(100 * n / sum(n), 2))
  print(dist_ajustada, n = Inf)
  readr::write_csv(dist_ajustada, file.path(QA_DIR, "05_distribuicao_rotulo_ajustada.csv"))

  saveRDS(eventos_ajustados, file.path(SHINY_DIR, "eventos_serie.rds"))
  save_ckpt(eventos_ajustados, "07_serie_temporal_eventos_ajustada")
  message(sprintf("[07][D] checkpoint salvo: 07_serie_temporal_eventos_ajustada (%d linhas, %d sinteticas)",
                  nrow(eventos_ajustados), sum(eventos_ajustados$evento_sintetico)))
  message("[07][D] exportado tambem: shiny_dashboard/eventos_serie.rds")
}

# =============================================================================
# BLOCO E — CRUZA CFEM (POR DECLARACAO) COM ROTULO_PERMISSAO NA DATA
# =============================================================================
# Decisao 2026-08: substitui segmentos_aptidao_processo() antigo (5 tabelas
# cruzadas com logica de intervalo complexa -- fonte de bastante erro).
# Fonte UNICA agora: rotulo_permissao por evento (Bloco A, ou Bloco D quando
# disponivel, que ja inclui a correcao de vencimento por data), convertido
# em faixas continuas e casado com a data de cada declaracao CFEM (1o dia
# do mes/ano declarado). Nao recria situacao documental nem cruza licenca/
# GU/211-213 separadamente -- tudo isso ja esta dentro do rotulo_permissao.
# =============================================================================

CFEM_PATH <- here::here("data", "result_shiny", "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv")

if (!file.exists(CFEM_PATH)) {
  message("[07][E] CFEM nao encontrado em ", CFEM_PATH, " -- pulando cruzamento CFEM x aptidao.")
} else {

  cfem <- readr::read_csv(CFEM_PATH, show_col_types = FALSE) |>
    dplyr::mutate(
      PROCESSO = as.character(PROCESSO),
      ANO = as.integer(ANO), MES = as.integer(MES),
      data_cfem = as.Date(sprintf("%04d-%02d-01", ANO, MES)),
      cfem_id = dplyr::row_number()
    )

  # usa a linha do tempo AJUSTADA (com vencimento corrigido) quando existe;
  # senao cai pra pura (evento real).
  fonte_faixas <- if (exists("eventos_ajustados")) eventos_ajustados else eventos

  construir_faixas_processo <- function(dtevento, rotulo_permissao) {
    ord <- order(dtevento)
    dt <- dtevento[ord]; rot <- rotulo_permissao[ord]
    n <- length(dt)
    if (n == 0) return(NULL)
    tibble::tibble(xmin = dt, xmax = c(dt[-1], Sys.Date()), rotulo_permissao = rot)
  }

  processos_cfem <- unique(cfem$PROCESSO)
  fonte_cfem <- fonte_faixas |>
    dplyr::filter(processo %in% processos_cfem) |>
    dplyr::select(processo, dtevento, rotulo_permissao)

  by_proc_e <- split(fonte_cfem, fonte_cfem$processo)
  faixas_todos <- purrr::imap_dfr(by_proc_e, function(g, p) {
    f <- construir_faixas_processo(g$dtevento, g$rotulo_permissao)
    if (is.null(f)) return(NULL)
    f$processo <- p
    f
  })

  match_cfem <- cfem |>
    dplyr::select(cfem_id, processo = PROCESSO, data_cfem) |>
    dplyr::inner_join(faixas_todos, by = "processo", relationship = "many-to-many") |>
    dplyr::filter(data_cfem >= xmin, data_cfem < xmax) |>
    dplyr::distinct(cfem_id, .keep_all = TRUE) |>
    dplyr::select(cfem_id, rotulo_permissao_na_data = rotulo_permissao)

  cfem_declaracoes_dossie <- cfem |>
    dplyr::left_join(match_cfem, by = "cfem_id") |>
    dplyr::mutate(
      tem_serie_temporal = PROCESSO %in% fonte_cfem$processo,
      apto_na_data = dplyr::case_when(
        is.na(rotulo_permissao_na_data) ~ NA,
        startsWith(rotulo_permissao_na_data, "PERMITIDA") ~ TRUE,
        TRUE ~ FALSE
      )
    )

  n_com_faixa <- sum(!is.na(cfem_declaracoes_dossie$rotulo_permissao_na_data))
  n_sem_serie  <- sum(!cfem_declaracoes_dossie$tem_serie_temporal)
  n_sem_faixa_com_serie <- sum(is.na(cfem_declaracoes_dossie$rotulo_permissao_na_data) & cfem_declaracoes_dossie$tem_serie_temporal)

  message(sprintf(
    "[07][E] declaracoes CFEM: %d | com faixa (rotulo na data): %d | sem serie temporal p/ processo: %d | com serie mas sem faixa correspondente: %d | fora de periodo permitido: %d",
    nrow(cfem_declaracoes_dossie), n_com_faixa, n_sem_serie, n_sem_faixa_com_serie,
    sum(cfem_declaracoes_dossie$apto_na_data == FALSE, na.rm = TRUE)
  ))

  dist_rotulo_cfem <- dplyr::count(cfem_declaracoes_dossie, rotulo_permissao_na_data, sort = TRUE)
  print(dist_rotulo_cfem, n = Inf)
  readr::write_csv(dist_rotulo_cfem, file.path(QA_DIR, "06_distribuicao_rotulo_cfem.csv"))

  save_ckpt(cfem_declaracoes_dossie, "07_cfem_declaracoes_dossie")
  message(sprintf("[07][E] checkpoint salvo: 07_cfem_declaracoes_dossie (%d linhas)", nrow(cfem_declaracoes_dossie)))
}

# =============================================================================
# BLOCO F — SITUACAO_ATUAL POR PROCESSO (fonte principal da caixa do dossie
# na aba 4 -- 1 linha por processo, existe pra TODO processo, com ou sem CFEM)
# =============================================================================
# Junta o que ja calculamos (rotulo_permissao_hoje, motivo mais recente,
# protecao de renovacao) com fase/titular do PMA/SIGMINE (result_shiny do
# 06 -- e a "foto atual", ja pronta, nao precisa reconstruir vigencia
# historica de titular pra isso).
# =============================================================================

PMA_PATH <- here::here("data", "result_shiny", "pma_amzl_ALLminerals_final.geojson")

if (!file.exists(PMA_PATH)) {
  message("[07][F] PMA nao encontrado em ", PMA_PATH, " -- pulando situacao_atual.")
} else {

  pma_atual <- sf::st_read(PMA_PATH, quiet = TRUE) |>
    sf::st_drop_geometry() |>
    dplyr::distinct(PROCESSO, .keep_all = TRUE) |>  # titular/fase sao iguais entre as linhas de foco do mesmo processo
    dplyr::transmute(processo = as.character(PROCESSO), fase_pma = FASE, titular_atual = TITULAR)

  fonte_situacao_f <- if (exists("eventos_ajustados")) eventos_ajustados else eventos

  situacao_hoje_f <- fonte_situacao_f |>
    dplyr::group_by(processo) |>
    dplyr::slice_max(dtevento, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      processo,
      rotulo_permissao_hoje = rotulo_permissao,
      apto_operar = as.character(startsWith(rotulo_permissao, "PERMITIDA")),
      fase_evento = dplyr::if_else(
        startsWith(rotulo_permissao, "PERMITIDA_"),
        stringr::str_remove(rotulo_permissao, "^PERMITIDA_"),
        rotulo_permissao
      )
    )

  motivo_recente <- if (exists("fechamentos")) {
    fechamentos |>
      dplyr::group_by(processo) |>
      dplyr::slice_max(dtevento, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(processo, motivo_nao_apto = motivo, motivo_origem = origem)
  } else {
    tibble::tibble(processo = character(0), motivo_nao_apto = character(0), motivo_origem = character(0))
  }

  protecao_f <- if (exists("protecao_vencimento")) {
    protecao_vencimento |>
      dplyr::select(processo, protegido_renovacao = protegido, dt_vencimento_titulo = dt_vencimento,
                    motivo_protecao, dt_protocolo_renovacao)
  } else {
    tibble::tibble(processo = character(0), protegido_renovacao = logical(0),
                   dt_vencimento_titulo = as.Date(character(0)), motivo_protecao = character(0),
                   dt_protocolo_renovacao = as.Date(character(0)))
  }

  # ultimo protocolo de licenca ambiental (categ_licenca, sufixo PROTOC) --
  # so pra fase PLG, que e onde a caixa mostra esse campo.
  ultimo_protocolo_licenca <- fonte_situacao_f |>
    dplyr::filter(categ_licenca, sufixo == "PROTOC") |>
    dplyr::group_by(processo) |>
    dplyr::summarise(dt_ultimo_protocolo_licenca_ambiental = max(dtevento), .groups = "drop")

  situacao_atual <- situacao_hoje_f |>
    dplyr::left_join(pma_atual, by = "processo") |>
    dplyr::left_join(motivo_recente, by = "processo") |>
    dplyr::left_join(protecao_f, by = "processo") |>
    dplyr::left_join(ultimo_protocolo_licenca, by = "processo") |>
    dplyr::mutate(
      # cuidado: vocabulario de fase pode diferir entre evento ANM e PMA/
      # SIGMINE (ex: "PLG" vs "PERMISSAO DE LAVRA GARIMPEIRA") -- comparacao
      # simples de string, pode dar falso positivo de divergencia. Revisar
      # amostra antes de confiar cegamente nesse flag.
      # CORRECAO (2026-08): PMA/SIGMINE usa nome por extenso ("LAVRA
      # GARIMPEIRA", "AUTORIZAÇÃO DE PESQUISA"), nosso fase_evento usa codigo
      # curto ("PLG", "PESQUISA") -- comparar direto (versao anterior) nunca
      # batia, sempre marcava divergencia falsa. Mapeamento confirmado pros
      # 4 primeiros via FASES_CORR_PADRAO (utils.R); CONC_LAV/REG_EXT sao
      # melhor palpite, nao confirmados contra dado real -- se o nome exato
      # do PMA for diferente, esses dois simplesmente nao geram comparacao
      # (fase_diverge_pma fica FALSE), nao um falso positivo.
      fase_pma_mapeada = dplyr::case_when(
        toupper(fase_pma) == "LAVRA GARIMPEIRA"                  ~ "PLG",
        toupper(fase_pma) == "LICENCIAMENTO"                     ~ "LICEN",
        toupper(fase_pma) == "AUTORIZAÇÃO DE PESQUISA"            ~ "PESQUISA",
        toupper(fase_pma) == "CONCESSÃO DE LAVRA"                 ~ "CONC_LAV",
        toupper(fase_pma) == "REGISTRO DE EXTRAÇÃO"               ~ "REG_EXT",
        TRUE ~ NA_character_
      ),
      fase_diverge_pma = !is.na(fase_pma_mapeada) & !is.na(fase_evento) & fase_pma_mapeada != fase_evento
    )

  message(sprintf("[07][F] situacao_atual: %d processos | apto_operar TRUE: %d",
                  nrow(situacao_atual), sum(situacao_atual$apto_operar == "TRUE", na.rm = TRUE)))

  saveRDS(situacao_atual, file.path(SHINY_DIR, "situacao_atual.rds"))
  save_ckpt(situacao_atual, "07_situacao_atual")
  message("[07][F] exportado: shiny_dashboard/situacao_atual.rds + checkpoint 07_situacao_atual")
}

# =============================================================================
# BLOCO G — RESUMO DE CFEM POR PROCESSO x FOCO (enriquecimento OPCIONAL da
# caixa do dossie -- so existe pra quem declarou CFEM)
# =============================================================================

if (exists("cfem_declaracoes_dossie")) {

  cfem_com_foco <- cfem_declaracoes_dossie |>
    dplyr::mutate(processo = PROCESSO, foco = categorizar_foco(SUBSarr, SUBSarrSIM))

  n_por_processo <- cfem_com_foco |>
    dplyr::group_by(processo) |>
    dplyr::summarise(
      n_declaracoes           = dplyr::n(),
      n_declaracoes_fora_vig  = sum(apto_na_data == FALSE, na.rm = TRUE),
      .groups = "drop"
    )

  dossie_resumo_processo <- cfem_com_foco |>
    dplyr::group_by(processo, foco) |>
    dplyr::summarise(
      valor_total   = sum(VALORarr,      na.rm = TRUE),
      peso_total_kg = sum(PESO_KG_final, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(n_por_processo, by = "processo")

  message(sprintf("[07][G] dossie_resumo_processo: %d linhas (processo x foco)", nrow(dossie_resumo_processo)))

  saveRDS(dossie_resumo_processo, file.path(SHINY_DIR, "dossie_resumo_processo.rds"))
  save_ckpt(dossie_resumo_processo, "07_dossie_resumo_processo")
  message("[07][G] exportado: shiny_dashboard/dossie_resumo_processo.rds + checkpoint 07_dossie_resumo_processo")
} else {
  message("[07][G] cfem_declaracoes_dossie indisponivel (Bloco E pulado) -- pulando resumo de CFEM.")
}

message("\n=== 07_serie_temporal.R — CONCLUIDO ===")

# =============================================================================
# BLOCO H — TABELA "FASES DO PROCESSO" (resumo pra aba 4, layout: Status |
# Fase | Data Inicio | Data Fim | Evento Inicio | Evento Fim)
# =============================================================================
# Design NOVO (2026-08), inspirado no formato que ja existia no app, mas
# reconstruido em cima do rotulo_permissao/papel -- nao e replica byte-a-
# byte da logica antiga (que eu nao tenho visibilidade completa), e um
# desenho novo com o mesmo espirito. Duas "trilhas" combinadas:
#
#   1) trilha do TITULO (banda continua) -- todo evento MUDA_FASE/FECHA/
#      SUSPENDE/RETOMA cujo tipo_proc esteja nas fases que operam ou em
#      pesquisa. Status deriva do papel; Data Fim = data do PROXIMO evento
#      dessa mesma trilha (ou "Atual" se for o ultimo).
#   2) trilha da LICENCA AMBIENTAL (pontual, sem banda) -- todo evento com
#      categ_licenca & sufixo PROTOC vira 1 linha isolada, Status =
#      "PROTOCOLADA", sem Data Fim (nao faz sentido "fechar" um protocolo).
#
# Uma linha sintetica extra ("PRE_AUTORIZACAO / SEM REGISTRO") cobre o
# periodo antes do primeiro evento conhecido do processo -- mesmo raciocinio
# do Bloco D pra vencimento: nao inventamos o que aconteceu antes, so
# marcamos que e um periodo sem informacao.
# =============================================================================

fonte_h <- if (exists("eventos_ajustados")) eventos_ajustados else eventos

status_trilha_titulo <- function(papel) {
  dplyr::case_when(
    papel %in% c("MUDA_FASE", "RETOMA") ~ "ATIVA",
    papel == "PROTOC"                   ~ "PROTOCOLO",
    papel == "FECHA"                    ~ "ENCERRADA",
    papel == "SUSPENDE"                 ~ "SUSPENSA",
    TRUE                                ~ NA_character_
  )
}

trilha_titulo <- fonte_h |>
  dplyr::filter(
    (papel == "MUDA_FASE" & tipo_proc %in% c(FASES_QUE_OPERAM, FASES_DE_PESQUISA)) |
    papel %in% c("FECHA", "SUSPENDE", "RETOMA") |
    (papel == "PROTOC" & tipo_proc %in% c(FASES_QUE_OPERAM, FASES_DE_PESQUISA) &
       stringr::str_detect(stringi::stri_trans_general(toupper(dsevento), "Latin-ASCII"), "RENOVA|PRORROGA"))
  ) |>
  dplyr::mutate(
    status = status_trilha_titulo(papel)
  ) |>
  dplyr::filter(!is.na(status)) |>
  dplyr::arrange(processo, dtevento) |>
  dplyr::group_by(processo) |>
  dplyr::mutate(dt_fim = dplyr::lead(dtevento)) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    processo, status, fase = tipo_proc,
    dt_inicio = dtevento, dt_fim,
    evento_inicio = dsevento, evento_fim = NA_character_
  )

trilha_licenca <- fonte_h |>
  dplyr::filter(categ_licenca, sufixo == "PROTOC") |>
  dplyr::transmute(
    processo, status = "PROTOCOLO", fase = "LIC AMB",
    dt_inicio = dtevento, dt_fim = as.Date(NA),
    evento_inicio = dsevento, evento_fim = NA_character_
  )

primeiro_evento_h <- fonte_h |>
  dplyr::group_by(processo) |>
  dplyr::summarise(dt_primeiro = min(dtevento), .groups = "drop")

trilha_pre_autorizacao <- primeiro_evento_h |>
  dplyr::transmute(
    processo, status = "PRE_AUTORIZACAO", fase = "SEM REGISTRO",
    dt_inicio = as.Date(NA), dt_fim = dt_primeiro,
    evento_inicio = NA_character_, evento_fim = NA_character_
  )

fases_processo_tabela <- dplyr::bind_rows(trilha_pre_autorizacao, trilha_titulo, trilha_licenca) |>
  dplyr::arrange(processo, dplyr::coalesce(dt_inicio, dt_fim))

message(sprintf("[07][H] fases_processo_tabela: %d linhas (%d processos)",
                nrow(fases_processo_tabela), dplyr::n_distinct(fases_processo_tabela$processo)))

saveRDS(fases_processo_tabela, file.path(SHINY_DIR, "fases_processo_tabela.rds"))
save_ckpt(fases_processo_tabela, "07_fases_processo_tabela")
message("[07][H] exportado: shiny_dashboard/fases_processo_tabela.rds + checkpoint 07_fases_processo_tabela")

# =============================================================================
# BLOCO I — TABELA "MULTAS E INFRACOES" (Tipo | Data | Evento | Descricao)
# =============================================================================

multas_infracoes_tabela <- fonte_h |>
  dplyr::mutate(
    S_h = stringi::stri_trans_general(toupper(dsevento), "Latin-ASCII"),
    tipo = dplyr::case_when(
      papel == "SUSPENDE" ~ "SUSPENSAO/EMBARGO",
      papel == "RETOMA"   ~ "RETOMADA/DESEMBARGO",
      papel == "NEUTRO_financeiro" & stringr::str_detect(S_h, "INFRACAO|\\bMULTA\\b") ~ "INFRACAO",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(tipo)) |>
  dplyr::transmute(processo, tipo, data = dtevento, evento = dsevento, descricao_publicacao = dspublicacaodou) |>
  dplyr::arrange(processo, data)

message(sprintf("[07][I] multas_infracoes_tabela: %d linhas (%d processos)",
                nrow(multas_infracoes_tabela), dplyr::n_distinct(multas_infracoes_tabela$processo)))

saveRDS(multas_infracoes_tabela, file.path(SHINY_DIR, "multas_infracoes_tabela.rds"))
save_ckpt(multas_infracoes_tabela, "07_multas_infracoes_tabela")
message("[07][I] exportado: shiny_dashboard/multas_infracoes_tabela.rds + checkpoint 07_multas_infracoes_tabela")

message("\n=== Blocos H e I — CONCLUIDO ===")