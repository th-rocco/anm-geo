# app.R
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(sf)
  library(DT); library(plotly); library(leaflet)
  library(bslib); library(shinyWidgets); library(networkD3)
  library(ggplot2); library(scales); library(readr); library(writexl); library(digest); library(stringi)
})

options(scipen = 999)
options(shiny.maxRequestSize = 50 * 1024^2)

res_dir <- normalizePath(".", winslash = "/")

data_atualizacao <- format(file.info(file.path(res_dir, "cfem.rds"))$mtime, "%d %B %Y")
if (is.na(data_atualizacao)) data_atualizacao <- "Data não disponível"

.read_rds <- function(name) readRDS(file.path(res_dir, name))
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ==============================================================================
# FUNCOES DO GRAFICO DE HISTORICO POR PROCESSO (Peca C)
# ==============================================================================
# EMBUTIDAS AQUI DE PROPOSITO (nao via source() de arquivo externo): o deploy
# no droplet usa scp so para "app.R" e "*.rds" (ver comando de deploy do
# usuario) — qualquer outro .R solto dentro de shiny_dashboard NAO sobe para
# o servidor. Por isso este bloco nao pode depender de um source() externo.
#
# FONTE DA VERDADE / ONDE EDITAR: R/graficos_historico.R, no repositorio do
# pipeline (o mesmo arquivo que utils.R usa via source()). Este bloco aqui e
# uma COPIA MANUAL. Se alterar a logica do grafico em graficos_historico.R,
# tem que colar a mudanca aqui tambem — nao ha jeito de automatizar isso sem
# reintroduzir uma dependencia de arquivo externo no deploy. Isso e uma
# excecao deliberada a regra de nao duplicar codigo, forcada pela restricao
# real do scp, nao uma escolha de conveniencia.
# ------------------------------------------------------------------------------

formata_num_br <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

tema_historico_processo <- ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())

# Cores das faixas "nao apto" no grafico plotly (aba 4), por motivo — mesma
# paleta de cores_status usada na tabela "Historico de autorizacoes" mais
# abaixo, pra ficar consistente entre tabela e grafico (achado 2026-07).
CORES_MOTIVO_NAO_APTO_RGBA <- c(
  "fase_de_tramitacao_ou_pesquisa"       = "rgba(108,117,125,0.15)",
  "titulo_vencido_renovacao_protocolada" = "rgba(243,156,18,0.18)"
)
CORES_MOTIVO_NAO_APTO_RGBA_DEFAULT <- "rgba(192,57,43,0.12)"
COR_APTO_RGBA <- "rgba(39,174,96,0.12)"

calcular_gaps_titulo <- function(inicio, fim, data_referencia = Sys.Date()) {
  if (length(inicio) == 0) {
    return(tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character())))
  }

  ord <- order(inicio)
  inicio <- inicio[ord]; fim <- fim[ord]

  intervalos_ini <- c(); intervalos_fim <- c()
  cur_ini <- inicio[1]; cur_fim <- fim[1]
  if (length(inicio) > 1) {
    for (i in 2:length(inicio)) {
      if (inicio[i] <= cur_fim) {
        cur_fim <- max(cur_fim, fim[i])
      } else {
        intervalos_ini <- c(intervalos_ini, cur_ini)
        intervalos_fim <- c(intervalos_fim, cur_fim)
        cur_ini <- inicio[i]; cur_fim <- fim[i]
      }
    }
  }
  intervalos_ini <- c(intervalos_ini, cur_ini)
  intervalos_fim <- c(intervalos_fim, cur_fim)

  gaps_ini <- c(); gaps_fim <- c()
  if (length(intervalos_ini) > 1) {
    for (i in 1:(length(intervalos_ini) - 1)) {
      gaps_ini <- c(gaps_ini, intervalos_fim[i])
      gaps_fim <- c(gaps_fim, intervalos_ini[i + 1])
    }
  }

  ultimo_fim <- intervalos_fim[length(intervalos_fim)]
  if (ultimo_fim < data_referencia) {
    gaps_ini <- c(gaps_ini, ultimo_fim)
    gaps_fim <- c(gaps_fim, data_referencia)
  }

  tibble::tibble(xmin = as.Date(gaps_ini, origin = "1970-01-01"),
                 xmax = as.Date(gaps_fim, origin = "1970-01-01"))
}

gaps_vigencia_titulo <- function(situacao_documental, processos = NULL) {
  d <- situacao_documental |>
    dplyr::filter(!is.na(dt_publicacao), !is.na(dt_vencimento))
  if (!is.null(processos)) d <- d |> dplyr::filter(processo %in% processos)
  if (nrow(d) == 0) {
    return(tibble::tibble(processo = character(), xmin = as.Date(character()), xmax = as.Date(character())))
  }
  d |>
    dplyr::group_by(processo) |>
    dplyr::group_modify(~ calcular_gaps_titulo(.x$dt_publicacao, .x$dt_vencimento)) |>
    dplyr::ungroup()
}

# --- Reconstrucao completa de aptidao como intervalos (fase/status + licenca
# + titulo) — usada na faixa vermelha do grafico. Ver graficos_historico.R
# para os comentarios completos / fonte da verdade.
unir_intervalos <- function(inicio, fim) {
  if (length(inicio) == 0) {
    return(tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character())))
  }
  ord <- order(inicio)
  inicio <- inicio[ord]; fim <- fim[ord]
  ini_out <- c(); fim_out <- c()
  cur_ini <- inicio[1]; cur_fim <- fim[1]
  if (length(inicio) > 1) {
    for (i in 2:length(inicio)) {
      if (inicio[i] <= cur_fim) {
        cur_fim <- max(cur_fim, fim[i])
      } else {
        ini_out <- c(ini_out, cur_ini); fim_out <- c(fim_out, cur_fim)
        cur_ini <- inicio[i]; cur_fim <- fim[i]
      }
    }
  }
  ini_out <- c(ini_out, cur_ini); fim_out <- c(fim_out, cur_fim)
  tibble::tibble(xmin = as.Date(ini_out, origin = "1970-01-01"),
                 xmax = as.Date(fim_out, origin = "1970-01-01"))
}

complementar_intervalos <- function(intervalos, inicio_janela, fim_janela) {
  if (is.null(intervalos) || nrow(intervalos) == 0) {
    return(tibble::tibble(xmin = inicio_janela, xmax = fim_janela))
  }
  intervalos <- intervalos[order(intervalos$xmin), , drop = FALSE]
  gaps_ini <- c(); gaps_fim <- c()
  cursor <- inicio_janela
  for (i in seq_len(nrow(intervalos))) {
    if (intervalos$xmin[i] > cursor) {
      gaps_ini <- c(gaps_ini, cursor); gaps_fim <- c(gaps_fim, intervalos$xmin[i] - 1)
    }
    cursor <- max(cursor, intervalos$xmax[i] + 1)
  }
  if (cursor <= fim_janela) {
    gaps_ini <- c(gaps_ini, cursor); gaps_fim <- c(gaps_fim, fim_janela)
  }
  if (length(gaps_ini) == 0) {
    return(tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character())))
  }
  tibble::tibble(xmin = as.Date(gaps_ini, origin = "1970-01-01"),
                 xmax = as.Date(gaps_fim, origin = "1970-01-01"))
}

segmentos_aptidao_processo <- function(processo_alvo, serie_fase_status, situacao_documental,
                                        protocolos_licenca_ambiental, eventos_renovacao_plg = NULL,
                                        intervalos_gu_aut_pesq = NULL,
                                        data_referencia = Sys.Date()) {
  processo_alvo <- as.character(processo_alvo)[1]
  FASES_QUE_OPERAM <- c("CONC LAV", "LICEN", "PLG", "REG EXT")

  fs <- if (!is.null(serie_fase_status)) serie_fase_status[serie_fase_status$processo == processo_alvo, , drop = FALSE] else NULL
  if (is.null(fs) || nrow(fs) == 0) return(NULL)

  doc <- if (!is.null(situacao_documental)) situacao_documental[situacao_documental$processo == processo_alvo, , drop = FALSE] else NULL
  lic <- if (!is.null(protocolos_licenca_ambiental)) protocolos_licenca_ambiental[protocolos_licenca_ambiental$processo == processo_alvo, , drop = FALSE] else NULL
  dt_primeiro_protocolo_lic <- if (!is.null(lic) && nrow(lic) > 0) min(lic$dt_protocolo, na.rm = TRUE) else as.Date(NA)

  # Protecao Art. 211/213 (Portaria DNPM 155/2016) — ver graficos_historico.R
  # para a fonte da verdade / comentarios completos.
  ev_renov <- if (!is.null(eventos_renovacao_plg)) eventos_renovacao_plg[eventos_renovacao_plg$processo == processo_alvo, , drop = FALSE] else NULL
  protocolos_renov <- if (!is.null(ev_renov)) sort(ev_renov$dtevento[ev_renov$idevento == "521"]) else as.Date(character())
  indeferimentos_renov <- if (!is.null(ev_renov)) sort(ev_renov$dtevento[ev_renov$idevento == "522"]) else as.Date(character())
  buraco_protegido_211_213 <- function(dt_vencimento_buraco) {
    candidatos <- protocolos_renov[protocolos_renov <= dt_vencimento_buraco]
    if (length(candidatos) == 0) return(FALSE)
    dt_protocolo <- max(candidatos)
    !any(indeferimentos_renov > dt_protocolo)
  }

  # GU (Guia de Utilizacao) para AUT PESQ — ver graficos_historico.R para a
  # fonte da verdade / comentarios completos.
  gu_proc <- if (!is.null(intervalos_gu_aut_pesq)) intervalos_gu_aut_pesq[intervalos_gu_aut_pesq$processo == processo_alvo, , drop = FALSE] else NULL
  intervalos_gu <- if (!is.null(gu_proc) && nrow(gu_proc) > 0) gu_proc[, c("xmin", "xmax")] else tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character()))

  intervalos_titulo <- if (!is.null(doc)) {
    doc_v <- doc[!is.na(doc$dt_publicacao) & !is.na(doc$dt_vencimento), , drop = FALSE]
    unir_intervalos(doc_v$dt_publicacao, doc_v$dt_vencimento)
  } else {
    tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character()))
  }

  fs2 <- fs
  fs2$dt_fim_efetivo <- dplyr::coalesce(fs2$dt_fim, data_referencia)

  segmentos <- list()
  for (i in seq_len(nrow(fs2))) {
    ini <- fs2$dt_inicio[i]; fim <- fs2$dt_fim_efetivo[i]
    if (is.na(ini) || is.na(fim) || fim < ini) next
    fase_i <- fs2$fase[i]; status_i <- fs2$status[i]

    fase_ok    <- !is.na(fase_i) && (fase_i %in% FASES_QUE_OPERAM)
    status_ok  <- !is.na(status_i) && identical(status_i, "ATIVA")
    licenca_ok <- !is.na(dt_primeiro_protocolo_lic)

    # GU para AUT PESQ — SINCRONIZADO com R/graficos_historico.R (fonte da
    # verdade). Particiona [ini, fim] por cobertura de GU valida quando a
    # fase e AUT PESQ; qualquer outra fase segue com 1 unico sub-intervalo
    # (comportamento identico ao de antes desta mudanca).
    if (!is.na(fase_i) && identical(fase_i, "AUT PESQ") && nrow(intervalos_gu) > 0) {
      cobertura_gu <- unir_intervalos(pmax(intervalos_gu$xmin, ini), pmin(intervalos_gu$xmax, fim))
      cobertura_gu <- cobertura_gu[cobertura_gu$xmin <= cobertura_gu$xmax, , drop = FALSE]
    } else {
      cobertura_gu <- tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character()))
    }
    buracos_gu <- complementar_intervalos(cobertura_gu, ini, fim)
    subintervalos_fase <- dplyr::bind_rows(
      if (nrow(cobertura_gu) > 0) tibble::tibble(xmin = cobertura_gu$xmin, xmax = cobertura_gu$xmax, fase_ok_j = TRUE) else NULL,
      if (nrow(buracos_gu) > 0)   tibble::tibble(xmin = buracos_gu$xmin,   xmax = buracos_gu$xmax,   fase_ok_j = fase_ok) else NULL
    )
    if (nrow(subintervalos_fase) == 0) subintervalos_fase <- tibble::tibble(xmin = ini, xmax = fim, fase_ok_j = fase_ok)
    subintervalos_fase <- subintervalos_fase[order(subintervalos_fase$xmin), , drop = FALSE]

    for (j in seq_len(nrow(subintervalos_fase))) {
      ini_j <- subintervalos_fase$xmin[j]; fim_j <- subintervalos_fase$xmax[j]
      fase_ok_efetivo <- subintervalos_fase$fase_ok_j[j]

      # AJUSTE (2026-07, flags binarias independentes — filtro "Tipo de alerta"
      # da aba 4 passa a refletir colunas binarias, nao so a flag encadeada).
      # SINCRONIZADO com R/graficos_historico.R (fonte da verdade) — ver
      # comentarios completos la. Titulo sempre particionado dentro de
      # [ini_j, fim_j], independente de fase/status/licenca ja terem falhado;
      # cascata apto_na_data/motivo_nao_apto_na_data com o MESMO resultado
      # de sempre (so muda a forma de derivar).
      if (fase_i %in% "CONC LAV") {
        titulo_part <- tibble::tibble(xmin = ini_j, xmax = fim_j, titulo_valido = TRUE, motivo_titulo = NA_character_)
      } else if (nrow(intervalos_titulo) == 0) {
        titulo_part <- tibble::tibble(
          xmin = ini_j, xmax = fim_j, titulo_valido = FALSE, motivo_titulo = "vencimento_sem_data_a_revisar")
      } else {
        cobertura <- unir_intervalos(pmax(intervalos_titulo$xmin, ini_j), pmin(intervalos_titulo$xmax, fim_j))
        cobertura <- cobertura[cobertura$xmin <= cobertura$xmax, , drop = FALSE]
        parte_cobertura <- if (nrow(cobertura) > 0) {
          tibble::tibble(xmin = cobertura$xmin, xmax = cobertura$xmax, titulo_valido = TRUE, motivo_titulo = NA_character_)
        } else {
          tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character()), titulo_valido = logical(), motivo_titulo = character())
        }
        buracos <- complementar_intervalos(cobertura, ini_j, fim_j)
        parte_buracos <- if (nrow(buracos) > 0) {
          motivo_buraco <- vapply(buracos$xmin, function(x0) {
            if (buraco_protegido_211_213(x0 - 1)) "titulo_vencido_renovacao_protocolada" else "titulo_vencido"
          }, character(1))
          tibble::tibble(xmin = buracos$xmin, xmax = buracos$xmax, titulo_valido = FALSE, motivo_titulo = motivo_buraco)
        } else {
          tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character()), titulo_valido = logical(), motivo_titulo = character())
        }
        titulo_part <- dplyr::bind_rows(parte_cobertura, parte_buracos)
        titulo_part <- titulo_part[order(titulo_part$xmin), , drop = FALSE]
      }
      if (nrow(titulo_part) == 0) next

      for (k in seq_len(nrow(titulo_part))) {
        tk <- titulo_part[k, ]

        flag_fase_nao_operacional                 <- !fase_ok_efetivo
        flag_status_nao_ativo                     <- !status_ok
        flag_sem_licenca_ambiental_previa         <- fase_ok_efetivo && !licenca_ok
        flag_vencimento_sem_data_a_revisar        <- identical(tk$motivo_titulo, "vencimento_sem_data_a_revisar")
        flag_titulo_vencido                       <- identical(tk$motivo_titulo, "titulo_vencido")
        flag_titulo_vencido_renovacao_protocolada <- identical(tk$motivo_titulo, "titulo_vencido_renovacao_protocolada")

        if (flag_fase_nao_operacional) {
          apto_k <- "em_analise"; motivo_k <- "fase_de_tramitacao_ou_pesquisa"
        } else if (flag_status_nao_ativo) {
          apto_k <- "FALSE"; motivo_k <- "suspensa_ou_encerrada"
        } else if (flag_sem_licenca_ambiental_previa) {
          apto_k <- "FALSE"; motivo_k <- "sem_licenca_ambiental_previa"
        } else if (!tk$titulo_valido && identical(tk$motivo_titulo, "titulo_vencido_renovacao_protocolada")) {
          apto_k <- "TRUE"; motivo_k <- NA_character_
        } else if (!tk$titulo_valido) {
          apto_k <- "FALSE"; motivo_k <- tk$motivo_titulo
        } else {
          apto_k <- "TRUE"; motivo_k <- NA_character_
        }

        segmentos[[length(segmentos) + 1]] <- tibble::tibble(
          xmin = tk$xmin, xmax = tk$xmax, apto_na_data = apto_k, motivo_nao_apto_na_data = motivo_k,
          flag_fase_nao_operacional                 = flag_fase_nao_operacional,
          flag_status_nao_ativo                     = flag_status_nao_ativo,
          flag_sem_licenca_ambiental_previa         = flag_sem_licenca_ambiental_previa,
          flag_vencimento_sem_data_a_revisar        = flag_vencimento_sem_data_a_revisar,
          flag_titulo_vencido                       = flag_titulo_vencido,
          flag_titulo_vencido_renovacao_protocolada = flag_titulo_vencido_renovacao_protocolada)
      }
    }
  }

  if (length(segmentos) == 0) return(NULL)
  out <- dplyr::bind_rows(segmentos)
  out$processo <- processo_alvo
  out[order(out$xmin), c("processo", "xmin", "xmax", "apto_na_data", "motivo_nao_apto_na_data",
                          "flag_fase_nao_operacional", "flag_status_nao_ativo",
                          "flag_sem_licenca_ambiental_previa", "flag_vencimento_sem_data_a_revisar",
                          "flag_titulo_vencido", "flag_titulo_vencido_renovacao_protocolada")]
}

periodos_nao_apto_processo <- function(processo_alvo, serie_fase_status, situacao_documental,
                                        protocolos_licenca_ambiental, eventos_renovacao_plg = NULL,
                                        intervalos_gu_aut_pesq = NULL,
                                        data_referencia = Sys.Date()) {
  seg <- segmentos_aptidao_processo(processo_alvo, serie_fase_status, situacao_documental,
                                     protocolos_licenca_ambiental, eventos_renovacao_plg,
                                     intervalos_gu_aut_pesq, data_referencia)
  if (is.null(seg)) return(NULL)
  nao_apto <- seg[seg$apto_na_data != "TRUE", , drop = FALSE]
  if (nrow(nao_apto) == 0) {
    return(tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character()),
                           motivo_nao_apto_na_data = character()))
  }
  dplyr::bind_rows(lapply(split(nao_apto, nao_apto$motivo_nao_apto_na_data), function(df) {
    u <- unir_intervalos(df$xmin, df$xmax)
    u$motivo_nao_apto_na_data <- df$motivo_nao_apto_na_data[1]
    u
  })) |> dplyr::arrange(xmin)
}

# NOVO (achado 2026-07, pedido direto): versao COMPLETA — inclui tambem os
# periodos APTOS (TRUE), pra sombrear de verde o "periodo outorgado real,
# sem problemas" no grafico (aba 4), nao so os periodos problematicos.
periodos_aptidao_processo <- function(processo_alvo, serie_fase_status, situacao_documental,
                                       protocolos_licenca_ambiental, eventos_renovacao_plg = NULL,
                                       intervalos_gu_aut_pesq = NULL,
                                       data_referencia = Sys.Date()) {
  seg <- segmentos_aptidao_processo(processo_alvo, serie_fase_status, situacao_documental,
                                     protocolos_licenca_ambiental, eventos_renovacao_plg,
                                     intervalos_gu_aut_pesq, data_referencia)
  if (is.null(seg) || nrow(seg) == 0) {
    return(tibble::tibble(xmin = as.Date(character()), xmax = as.Date(character()),
                           apto_na_data = character(), motivo_nao_apto_na_data = character()))
  }
  grupo <- ifelse(seg$apto_na_data == "TRUE", "__APTO__", seg$motivo_nao_apto_na_data)
  dplyr::bind_rows(lapply(split(seg, grupo), function(df) {
    u <- unir_intervalos(df$xmin, df$xmax)
    u$apto_na_data <- df$apto_na_data[1]
    u$motivo_nao_apto_na_data <- df$motivo_nao_apto_na_data[1]
    u
  })) |> dplyr::arrange(xmin)
}

camada_marcacao <- function(dados, col_data, cor, col_label = NULL,
                             label_italico = FALSE, mostrar_texto = TRUE) {
  if (is.null(dados) || nrow(dados) == 0) return(list())

  dados <- dados |>
    dplyr::mutate(.label = if (!is.null(col_label))
      paste0(.data[[col_label]], " ", format(.data[[col_data]], "%d/%m/%Y"))
      else format(.data[[col_data]], "%d/%m/%Y"))

  camadas <- list(
    ggplot2::geom_vline(
      data = dados, ggplot2::aes(xintercept = .data[[col_data]]),
      color = cor, linetype = "dashed", linewidth = 0.4
    )
  )
  if (mostrar_texto) {
    camadas <- c(camadas, list(
      ggplot2::geom_text(
        data = dados, ggplot2::aes(x = .data[[col_data]], y = Inf, label = .label),
        inherit.aes = FALSE, color = cor, size = 2,
        fontface = if (label_italico) "italic" else "plain",
        angle = 90, hjust = 1.1, vjust = -0.4
      )
    ))
  }
  camadas
}

eventos_marcacao <- function(eventos_classificados, papeis, processos = NULL) {
  if (is.null(eventos_classificados)) return(NULL)
  d <- eventos_classificados |> dplyr::filter(papel %in% papeis)
  if (!is.null(processos)) d <- d |> dplyr::filter(processo %in% processos)
  d
}

# -----------------------------------------------------------------------------
# VERSAO PLOTLY NATIVA (nao ggplotly) — usada na aba 4 do Shiny. Mesmo padrao
# do app.R original (plot_ly() direto + hovertemplate + shapes de layout),
# por isso funciona bem: nunca passa pela conversao ggplot2->plotly que
# quebrava a faixa vermelha e os labels. Eventos entram como tracinhos
# coloridos na base (symbol "line-ns-open") com texto no HOVER.
grafico_historico_processo_plotly <- function(processo_alvo,
                                               dados_cfem = NULL,
                                               situacao_documental = NULL,
                                               protocolos_licenca_ambiental = NULL,
                                               eventos_classificados = NULL,
                                               serie_fase_status = NULL,
                                               eventos_renovacao_plg = NULL,
                                               intervalos_gu_aut_pesq = NULL,
                                               variavel = c("valor", "peso"),
                                               cores_evento = list()) {

  variavel <- match.arg(variavel)
  processo_alvo <- as.character(processo_alvo)[1]

  cores <- utils::modifyList(list(
    linha = "#1B4332", ponto_ok = "#2D6A4F", ponto_alerta = "#C0392B",
    publicacao = "#1B7A3D", vencimento = "#C0392B", protocolo = "#2C3E50",
    suspensao  = "#B9770E", retomada = "#1F618D", anulacao = "#7B241C",
    protocolo_renovacao_plg = "#16A085", baixa = "#8E44AD"
  ), cores_evento)

  col_y <- if (variavel == "valor") "VALORarr" else "PESO_KG_final_limpo"
  eixo_y_titulo <- if (variavel == "valor") "R$" else "kg"

  cfem_p <- NULL
  if (!is.null(dados_cfem)) {
    cfem_p <- dados_cfem |>
      dplyr::mutate(PROCESSO = as.character(PROCESSO)) |>
      dplyr::filter(PROCESSO == processo_alvo) |>
      dplyr::mutate(data_cfem = if ("data_cfem" %in% names(dados_cfem)) data_cfem
                                 else as.Date(sprintf("%04d-%02d-01", ANO, MES))) |>
      dplyr::arrange(data_cfem)
  }
  tem_cfem <- !is.null(cfem_p) && nrow(cfem_p) > 0

  doc_p <- if (!is.null(situacao_documental)) situacao_documental |> dplyr::filter(processo == processo_alvo) else NULL
  lic_p <- if (!is.null(protocolos_licenca_ambiental)) protocolos_licenca_ambiental |> dplyr::filter(processo == processo_alvo) else NULL
  ev_p  <- if (!is.null(eventos_classificados)) eventos_classificados |> dplyr::filter(processo == processo_alvo) else NULL

  publicacao_p <- if (!is.null(doc_p)) doc_p |> dplyr::filter(!is.na(dt_publicacao)) |> dplyr::distinct(dt_publicacao, dssituacaodocumentolegal) else NULL
  vencimento_p <- if (!is.null(doc_p)) doc_p |> dplyr::filter(!is.na(dt_vencimento)) |> dplyr::distinct(dt_vencimento, dssituacaodocumentolegal) else NULL

  # CORRECAO (achado real, processo 850292/2016): a faixa vermelha nao pode
  # olhar so vencimento NOMINAL do titulo — um titulo pode morrer ANTES do
  # vencimento nominal por renuncia/cassacao/nulidade/revogacao (evento
  # FECHA). periodos_nao_apto_processo() cruza fase/status + licenca +
  # titulo, os mesmos 3 criterios ja aprovados, como intervalos.
  gaps_p <- periodos_aptidao_processo(processo_alvo, serie_fase_status, situacao_documental,
                                       protocolos_licenca_ambiental, eventos_renovacao_plg, intervalos_gu_aut_pesq)

  suspensao_p <- eventos_marcacao(ev_p, papeis = "SUSPENDE")
  retomada_p  <- eventos_marcacao(ev_p, papeis = "RETOMA")
  anulacao_p_bruto <- eventos_marcacao(ev_p, papeis = "FECHA")

  # NOVO (achado 2026-07): separa "BAIXA TRANSCRICAO" (Art. 216 e
  # equivalentes) do restante dos fechamentos — marcador proprio.
  baixa_p    <- NULL
  anulacao_p <- anulacao_p_bruto
  if (!is.null(anulacao_p_bruto) && nrow(anulacao_p_bruto) > 0) {
    eh_baixa <- stringr::str_detect(anulacao_p_bruto$dsevento, "BAIXA TRANSCRI")
    if (any(eh_baixa))  baixa_p    <- anulacao_p_bruto[eh_baixa, , drop = FALSE]
    if (any(!eh_baixa)) anulacao_p <- anulacao_p_bruto[!eh_baixa, , drop = FALSE] else anulacao_p <- NULL
  }

  # NOVO: protocolo de renovacao da PLG (idevento 521, Art. 211) — vem de
  # eventos_renovacao_plg (papel PROTOC, fora de eventos_classificados).
  renovacao_plg_p <- if (!is.null(eventos_renovacao_plg)) {
    d <- eventos_renovacao_plg |> dplyr::filter(processo == processo_alvo, idevento == "521")
    if (nrow(d) > 0) d else NULL
  } else NULL

  shapes <- list()
  if (!is.null(gaps_p) && nrow(gaps_p) > 0) {
    for (i in seq_len(nrow(gaps_p))) {
      if (!is.na(gaps_p$apto_na_data[i]) && gaps_p$apto_na_data[i] == "TRUE") {
        cor_i <- COR_APTO_RGBA
      } else {
        cor_i <- CORES_MOTIVO_NAO_APTO_RGBA[gaps_p$motivo_nao_apto_na_data[i]]
        if (is.na(cor_i)) cor_i <- CORES_MOTIVO_NAO_APTO_RGBA_DEFAULT
      }
      shapes[[length(shapes) + 1]] <- list(
        type = "rect", xref = "x", yref = "paper",
        x0 = as.character(gaps_p$xmin[i]), x1 = as.character(gaps_p$xmax[i]),
        y0 = 0, y1 = 1, fillcolor = unname(cor_i), line = list(width = 0), layer = "below"
      )
    }
  }

  marcas <- list()
  empilhar_marca <- function(datas, rotulos, cor) {
    if (is.null(datas) || length(datas) == 0) return(invisible(NULL))
    marcas[[length(marcas) + 1]] <<- data.frame(x = as.Date(datas), texto = rotulos, cor = cor, stringsAsFactors = FALSE)
  }
  if (!is.null(publicacao_p) && nrow(publicacao_p) > 0)
    empilhar_marca(publicacao_p$dt_publicacao, paste0(publicacao_p$dssituacaodocumentolegal, " — ", format(publicacao_p$dt_publicacao, "%d/%m/%Y")), cores$publicacao)
  if (!is.null(vencimento_p) && nrow(vencimento_p) > 0)
    empilhar_marca(vencimento_p$dt_vencimento, paste0(vencimento_p$dssituacaodocumentolegal, " — ", format(vencimento_p$dt_vencimento, "%d/%m/%Y")), cores$vencimento)
  if (!is.null(lic_p) && nrow(lic_p) > 0)
    empilhar_marca(lic_p$dt_protocolo, paste0("Lic Amb Protoc — ", format(lic_p$dt_protocolo, "%d/%m/%Y")), cores$protocolo)
  if (!is.null(suspensao_p) && nrow(suspensao_p) > 0)
    empilhar_marca(suspensao_p$dtevento, paste0(suspensao_p$dsevento, " — ", format(suspensao_p$dtevento, "%d/%m/%Y")), cores$suspensao)
  if (!is.null(retomada_p) && nrow(retomada_p) > 0)
    empilhar_marca(retomada_p$dtevento, paste0(retomada_p$dsevento, " — ", format(retomada_p$dtevento, "%d/%m/%Y")), cores$retomada)
  if (!is.null(anulacao_p) && nrow(anulacao_p) > 0)
    empilhar_marca(anulacao_p$dtevento, paste0(anulacao_p$dsevento, " — ", format(anulacao_p$dtevento, "%d/%m/%Y")), cores$anulacao)
  if (!is.null(baixa_p) && nrow(baixa_p) > 0)
    empilhar_marca(baixa_p$dtevento, paste0(baixa_p$dsevento, " — ", format(baixa_p$dtevento, "%d/%m/%Y")), cores$baixa)
  if (!is.null(renovacao_plg_p) && nrow(renovacao_plg_p) > 0)
    empilhar_marca(renovacao_plg_p$dtevento, paste0("Protocolo renovacao PLG — ", format(renovacao_plg_p$dtevento, "%d/%m/%Y")), cores$protocolo_renovacao_plg)
  marcas_df <- if (length(marcas) > 0) dplyr::bind_rows(marcas) else NULL

  if (tem_cfem) {
    cor_ponto <- ifelse(!is.na(cfem_p$apto_na_data) & cfem_p$apto_na_data != "TRUE",
                         cores$ponto_alerta, cores$ponto_ok)
    p <- plotly::plot_ly(
      cfem_p, x = ~data_cfem, y = stats::as.formula(paste0("~", col_y)),
      type = "scatter", mode = "lines+markers",
      line = list(color = cores$linha, width = 2),
      marker = list(color = cor_ponto, size = 7),
      hovertemplate = paste0("%{x|%d/%m/%Y}<br>",
                             if (variavel == "valor") "R$ %{y:,.2f}" else "%{y:,.2f} kg",
                             "<extra></extra>"),
      name = ""
    )
  } else {
    datas_disponiveis <- c(
      if (!is.null(publicacao_p)) publicacao_p$dt_publicacao,
      if (!is.null(vencimento_p)) vencimento_p$dt_vencimento,
      if (!is.null(lic_p))        lic_p$dt_protocolo,
      if (!is.null(ev_p))         ev_p$dtevento
    )
    datas_disponiveis <- as.Date(datas_disponiveis, origin = "1970-01-01")
    if (length(datas_disponiveis) == 0) datas_disponiveis <- c(Sys.Date() - 3650, Sys.Date())
    p <- plotly::plot_ly(
      x = c(min(datas_disponiveis), max(datas_disponiveis)), y = c(0, 0),
      type = "scatter", mode = "markers", opacity = 0, hoverinfo = "none", showlegend = FALSE
    )
  }

  if (!is.null(marcas_df) && nrow(marcas_df) > 0) {
    p <- p |> plotly::add_markers(
      data = marcas_df, x = ~x, y = 0, inherit = FALSE, showlegend = FALSE,
      marker = list(color = marcas_df$cor, size = 10, symbol = "line-ns-open", line = list(width = 2)),
      text = ~texto, hovertemplate = "%{text}<extra></extra>"
    )
  }

  p |> plotly::layout(
    shapes = shapes,
    xaxis = list(title = ""), yaxis = list(title = eixo_y_titulo),
    margin = list(l = 60, r = 20, t = 10, b = 30),
    showlegend = FALSE
  )
}
# ------------------------------------------------------------------------------
# FIM DO BLOCO EMBUTIDO (Peca C)
# ==============================================================================

# ---- Dados tabulares ----
cfem        <- .read_rds("cfem.rds")
cfem_anual  <- .read_rds("cfem_anual.rds")
cfem_mensal <- .read_rds("cfem_mensal.rds")
pma_simpl   <- .read_rds("pma_simpl.rds")

# ---- Geometrias ----
ti  <- .read_rds("ti_simpl.rds")
uc  <- .read_rds("uc_simpl.rds")
qui <- .read_rds("qui_simpl.rds")

# ---- Choices iniciais ----
anos_all          <- sort(unique(cfem$ANO))
subs_all_grupo    <- sort(unique(cfem$SUBSarrSIM))
subs_all_original <- sort(unique(cfem$SUBSarr))
ufs_all           <- sort(na.omit(unique(cfem$abbrev_state)))
muns_all          <- sort(na.omit(unique(cfem$name_muni)))
fases_all         <- sort(na.omit(unique(cfem$FASE)))
procs_all         <- sort(na.omit(unique(cfem$PROCESSO)))
tits_all          <- sort(na.omit(unique(cfem$TITULAR)))
decl_all          <- sort(na.omit(unique(cfem$NOME_arr)))
map_subs          <- cfem |> dplyr::distinct(SUBSarrSIM, SUBSarr)

# ---- Choices da aba Consulta (PMA — nomes originais) ----
cp_ufs_all   <- sort(na.omit(unique(pma_simpl$uf)))
cp_muns_all  <- sort(na.omit(unique(pma_simpl$munic)))
cp_subs_grp  <- sort(na.omit(unique(pma_simpl$SUBSpmaGRP)))
cp_subs_det  <- sort(na.omit(unique(pma_simpl$SUBS)))
cp_fases_all <- sort(na.omit(unique(pma_simpl$FASE)))
pma_attrs_cp <- sf::st_drop_geometry(pma_simpl)
pma_attrs_cp$PROCESSO <- as.character(pma_attrs_cp$PROCESSO)
# NOTA (2026-07-20): pma_simpl agora tem 1-3 linhas por processo (1 por foco
# -- ver 05_integracao_final.R). pma_attrs_cp alimenta so filtros/lista de
# processos (UF, municipio, fase, substancia) -- nao usa nenhum numero
# especifico de foco (isso fica em dossie_resumo_processo, cp_dossie_box).
# distinct(PROCESSO) evita o processo aparecer duplicado na lista/picker.
pma_attrs_cp <- pma_attrs_cp |> dplyr::distinct(PROCESSO, .keep_all = TRUE)
map_subs_pma <- pma_attrs_cp |>
  dplyr::distinct(SUBSpmaGRP, SUBS) |>
  dplyr::filter(!is.na(SUBSpmaGRP), !is.na(SUBS))
cp_map_mun   <- pma_attrs_cp |>
  dplyr::distinct(uf, munic) |>
  dplyr::filter(!is.na(uf), !is.na(munic))

# ---- Lookups para filtros encadeados ----
lk_mun      <- .read_rds("lk_mun_tab1.rds")
lk_tit_proc <- .read_rds("lk_tit_proc_tab1.rds")
lk_decl     <- .read_rds("lk_decl_tab1.rds")

lk_mun_tab2      <- .read_rds("lk_mun_tab2.rds")
lk_tit_proc_tab2 <- .read_rds("lk_tit_proc_tab2.rds")
lk_decl_tab2     <- .read_rds("lk_decl_tab2.rds")

# ---- Microdados SCM (aba Consulta) ----
.read_rds_opt <- function(name) {
  p <- file.path(res_dir, name)
  if (file.exists(p)) readRDS(p) else NULL
}
micro_processos     <- .read_rds_opt("micro_processos.rds")
micro_eventos       <- .read_rds_opt("micro_eventos.rds")
micro_pessoas       <- .read_rds_opt("micro_pessoas.rds")
micro_pessoa_resumo <- .read_rds_opt("micro_pessoa_resumo.rds")
micro_substancias   <- .read_rds_opt("micro_substancias.rds")
micro_titulos       <- .read_rds_opt("micro_titulos.rds")
micro_municipios    <- .read_rds_opt("micro_municipios.rds")
micro_documentacao  <- .read_rds_opt("micro_documentacao.rds")
micro_associacoes   <- .read_rds_opt("micro_associacoes.rds")
micro_propsolo      <- .read_rds_opt("micro_propsolo.rds")
micro_ok            <- !is.null(micro_processos)
micro_proc_choices  <- if (micro_ok) sort(unique(micro_processos$processo)) else character(0)

# ---- Dossie da aba 4 (07_proc_shiny_dossie.R) — granularidade individual,
# sem agregacao: 1 declaracao de CFEM = 1 linha, 1 evento = 1 linha ----
dossie_resumo_processo      <- .read_rds_opt("dossie_resumo_processo.rds")
cfem_declaracoes_dossie     <- .read_rds_opt("cfem_declaracoes_dossie.rds")
cfem_motivo_ref             <- .read_rds_opt("cfem_motivo_ref.rds")
cfem_eventos_ref            <- .read_rds_opt("cfem_eventos_ref.rds")

# ---- Fontes do grafico historico por processo (08_proc_shiny_geo.R e
# 07_proc_shiny_dossie.R ja deixam tudo em .rds dentro de shiny_dashboard;
# nao ha dependencia de arrow/parquet em producao) ----
situacao_documental          <- .read_rds_opt("situacao_documental.rds")
protocolos_licenca_ambiental <- .read_rds_opt("protocolos_licenca_ambiental.rds")
eventos_classificados        <- .read_rds_opt("eventos_classificados.rds")
situacao_atual               <- .read_rds_opt("situacao_atual.rds")
serie_fase_status            <- .read_rds_opt("serie_fase_status.rds")
# NOVO (Art. 211/213, Portaria DNPM 155/2016 — renovacao de PLG protocolada):
# protocolo (idevento 521) e indeferimento (idevento 522) de renovacao de
# PLG por processo. Opcional (.read_rds_opt) — se ausente (deploy antigo,
# 06/07 nao rodados de novo ainda), volta ao comportamento anterior a esta
# mudanca (sem protecao), NULL e aceito em todas as funcoes que o usam.
eventos_renovacao_plg        <- .read_rds_opt("eventos_renovacao_plg_211_213.rds")
# NOVO (Bloco J do 06, achado 2026-07-21 — GU para AUT PESQ): janelas de
# validade de Guia de Utilizacao por processo. Opcional (.read_rds_opt),
# mesmo padrao do eventos_renovacao_plg acima.
intervalos_gu_aut_pesq       <- .read_rds_opt("intervalos_gu_aut_pesq.rds")
# NOVO (Bloco I do 06, Fase 3 — historico de penalidades/ocorrencias): multa/
# infracao/advertencia + suspensao/retomada administrativa + suspensao/
# termino/anulacao judicial, ja consolidadas num produto so. Opcional, mesmo
# padrao do eventos_renovacao_plg acima.
eventos_penalidades_ocorrencias <- .read_rds_opt("eventos_penalidades_ocorrencias.rds")

dossie_ok  <- !is.null(dossie_resumo_processo)
inapto_ok  <- dossie_ok  # nome mantido por compatibilidade com o resto do server abaixo

inapto_cats <- if (!is.null(cfem_motivo_ref)) sort(unique(na.omit(cfem_motivo_ref$rotulo))) else character(0)

# AJUSTE (2026-07, flags binarias): mapeamento motivo -> coluna tem_flag_*
# em dossie_resumo_processo (agregada no 07_proc_shiny_dossie.R a partir das
# flags independentes de segmentos_aptidao_processo). O picker continua
# mostrando os MESMOS rotulos de sempre (via cfem_motivo_ref) — so a fonte
# do filtro por tras muda de string-match em motivos_periodo_nao_apto
# (flag encadeada, mascarava condicoes simultaneas) para OR entre colunas
# binarias independentes.
MOTIVO_TO_FLAGCOL <- c(
  fase_de_tramitacao_ou_pesquisa        = "tem_flag_fase_nao_operacional",
  suspensa_ou_encerrada                 = "tem_flag_status_nao_ativo",
  sem_licenca_ambiental_previa          = "tem_flag_sem_licenca_ambiental_previa",
  vencimento_sem_data_a_revisar         = "tem_flag_vencimento_sem_data_a_revisar",
  titulo_vencido                        = "tem_flag_titulo_vencido",
  titulo_vencido_renovacao_protocolada  = "tem_flag_titulo_vencido_renovacao_protocolada"
)
# NOVO (achado 2026-07): mesma tabela, agora apontando pras colunas de
# situacao_atual (flag_*, sem o prefixo "tem_" que so existia em
# dossie_resumo_processo/CFEM) — usada no cp_lista_df unificado, que passa
# a ler a base completa em vez do produto CFEM-gated.
MOTIVO_TO_FLAGCOL_SA <- sub("^tem_", "", MOTIVO_TO_FLAGCOL)

# ---- Colunas + rótulos (aba Tabela) ----
cols_visible <- c(
  "SUBSarrSIM", "SUBSarr", "PROCESSO", "AREA_HA", "ANO", "MES",
  "abbrev_state", "name_muni", "TITULAR", "CPF_CNPJcm", "NOME_arr",
  "CPF_CNPJarr", "VALORarr", "VALORtot", "PESO_G", "PESO_KG",
  "preco_g_orig", "corr", "PESO_G_final_limpo", "PESO_KG_final_limpo", "preco_g_final",
  "FASE", "ULT_EV_DES", "ULT_EV_DAT", "UCname", "TIname", "QUIname"
)
cols_labels <- c(
  SUBSarrSIM = "Grupo", SUBSarr = "Substância", PROCESSO = "Processo",
  AREA_HA = "Área proc.(ha)", ANO = "Ano", MES = "Mês",
  abbrev_state = "UF", name_muni = "Município", TITULAR = "Titular",
  CPF_CNPJcm = "CPF-CNPJ (titular)", NOME_arr = "Parte declarante",
  CPF_CNPJarr = "CPF-CNPJ (declarante)", VALORarr = "Valor Recolhido (R$)",
  VALORtot = "Valor Total (R$)", PESO_G = "Peso orig (g)", PESO_KG = "Peso orig (Kg)",
  preco_g_orig = "R$/g (orig)", corr = "Peso corrigido?",
  PESO_G_final_limpo = "Peso final (g)", PESO_KG_final_limpo = "Peso final (kg)",
  preco_g_final = "R$/g (final)", FASE = "Fase Processo",
  ULT_EV_DES = "Último evento", ULT_EV_DAT = "Data último evento",
  UCname = "UC", TIname = "TI", QUIname = "QUI"
)

# ---- Tema ----
primary_color <- "#1B4332"
accent_color  <- "#2D6A4F"
theme <- bs_theme(
  version = 5,
  base_font = font_google("Inter"), heading_font = font_google("Inter"),
  primary = primary_color, info = accent_color, bg = "#ffffff", fg = "#212529"
)

picker_opts <- list(
  `actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE,
  `noneSelectedText` = "Todos", `selectedTextFormat` = "count > 2"
)

# ---- Relatório de seleção ----
relatorio_selecao <- function(df, mensal = TRUE, list_cap = 10) {
  if (is.null(df) || nrow(df) == 0) return("Nenhum dado encontrado com os filtros aplicados.")
  stopifnot("PESO_KG_final_limpo" %in% names(df))
  peso_total <- sum(df$PESO_KG_final_limpo, na.rm = TRUE)
  anos <- range(na.omit(df$ANO))
  fmt_num_br <- function(x) format(round(x, 2), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  fmt_cur_br <- function(x) paste0("R$ ", format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2))
  showv <- function(v) paste(c(utils::head(v, list_cap), if (length(v) > list_cap) "…"), collapse = ", ")
  subs_u <- sort(unique(na.omit(df$SUBSarr)))
  grps_u <- sort(unique(na.omit(df$SUBSarrSIM)))

  linhas <- c(
    paste0("Período: ", anos[1], "–", anos[2]),
    paste0("UF (", dplyr::n_distinct(df$abbrev_state), "): ",
           paste(sort(unique(na.omit(df$abbrev_state))), collapse = ", ")),
    paste0("Município (", dplyr::n_distinct(df$name_muni), "): ",
           paste(utils::head(sort(unique(na.omit(df$name_muni))), 20), collapse = ", "),
           if (dplyr::n_distinct(df$name_muni) > 20) ", …" else ""),
    paste0("Substância - Grupo (", length(grps_u), "): ", showv(grps_u)),
    paste0("Substância - Detalhe (", length(subs_u), "): ", showv(subs_u)),
    paste0("Fase (", dplyr::n_distinct(df$FASE), "): ",
           paste(sort(unique(na.omit(df$FASE))), collapse = ", "))
  )

  proc_u <- sort(unique(na.omit(df$PROCESSO)))
  tit_u  <- sort(unique(na.omit(df$TITULAR)))
  dec_u  <- sort(unique(na.omit(df$NOME_arr)))
  linhas_listas <- c(
    paste0("Processos únicos (", length(proc_u), "):\n  ", showv(proc_u)),
    paste0("Titulares únicos (", length(tit_u), "):\n  ", showv(tit_u)),
    paste0("Partes declarantes únicas (", length(dec_u), "):\n  ", showv(dec_u))
  )

  area_total <- NA_real_; n_area_ok <- 0L; linha_area <- NULL
  if ("AREA_HA" %in% names(df)) {
    area_info <- df |>
      dplyr::select(PROCESSO, AREA_HA) |>
      dplyr::filter(!is.na(AREA_HA)) |>
      dplyr::group_by(PROCESSO) |>
      dplyr::summarise(area_ha = dplyr::first(AREA_HA), .groups = "drop")
    area_total <- sum(area_info$area_ha, na.rm = TRUE)
    n_area_ok  <- nrow(area_info)
    linha_area <- if (n_area_ok > 0) {
      paste0("Área total (ha): ", fmt_num_br(area_total), " [", n_area_ok, " processos]")
    } else {
      "Área total (ha): não disponível (sem valores de área na seleção)."
    }
  }

  linha_ratio <- NULL
  if (is.finite(area_total) && !is.na(area_total) && area_total > 0) {
    kg_ha <- peso_total / area_total
    linha_ratio <- paste0("Relação (Kg/ha): ", fmt_num_br(kg_ha))
  }

  linhas2 <- c(
    paste0("Total Declarações CFEM: ", format(nrow(df), big.mark = ".", decimal.mark = ",")),
    paste0("Total Valor Recolhido: ", fmt_cur_br(sum(df$VALORarr, na.rm = TRUE))),
    paste0("Total Peso declarado (Kg): ", fmt_num_br(peso_total)),
    linha_area, linha_ratio
  )

  add_ov <- function(flag, namecol, rotulo) {
    if (flag %in% names(df) && any(df[[flag]] == 1, na.rm = TRUE)) {
      nomes <- sort(unique(na.omit(df[[namecol]][ df[[flag]] == 1 ])))
      paste0("- ", rotulo, " (", length(nomes), "): ",
             paste(utils::head(nomes, 15), collapse = ", "),
             if (length(nomes) > 15) ", …" else "")
    } else NULL
  }
  bloco_ov <- c(
    "Sobreposição com Territórios Protegidos:",
    add_ov("TIov",  "TIname",  "Terras Indígenas"),
    add_ov("UCov",  "UCname",  "Unidades de Conservação"),
    add_ov("QUIov", "QUIname", "Comunidades Quilombolas")
  )
  if (identical(bloco_ov[-1], list(NULL, NULL, NULL))) bloco_ov <- "Sobreposição com Territórios Protegidos: Nenhuma."
  bloco_buf <- c(
    "Proximidade (10 km):",
    add_ov("TIov10km",  "TIname",  "Terras Indígenas"),
    add_ov("UCov10km",  "UCname",  "Unidades de Conservação"),
    add_ov("QUIov10km", "QUIname", "Comunidades Quilombolas")
  )
  if (identical(bloco_buf[-1], list(NULL, NULL, NULL))) bloco_buf <- "Proximidade (10 km): Não."

  paste(c(
    linhas, "",
    linhas_listas[1], "", linhas_listas[2], "", linhas_listas[3], "",
    linhas2, "", bloco_ov, "", bloco_buf
  ), collapse = "\n")
}

# ---- UI ----
ui <- page_navbar(
  title = "Arrecadação de CFEM (2003-2026)",
  theme = theme,
  header = tags$head(
    tags$style(HTML("
      body { font-size: 12px; color: #212529; }
      h1, h2 { color: #2C3E50; font-weight: 600; }
      h3 { font-size: 16px; font-weight: 600; margin-top: 10px; margin-bottom: 8px; color: #2C3E50; }
      h4 { font-size: 14px; font-weight: 600; margin-top: 14px; margin-bottom: 6px; color: #2C3E50; }
      .app-subtitle { font-size: 14px; color: #6c757d; line-height: 1.5; margin-bottom: 8px; }
      .note-text {font-size: 14px; color: #6c757d; font-style: italic; margin-top: -4px; margin-bottom: 10px;}
      .filters-card { background: #F8F9FA; border: 1px solid #E1E5EB; border-radius: 6px; padding: 10px; }
      .filters-card .form-control, .filters-card .selectpicker, .filters-card .form-select { font-size: 10px; height: calc(1.8em + 0.75rem + 2px); }
      .filters-card .shiny-input-container { margin-bottom: 10px; width: 100%; }
      .filters-card .btn { width: 100%; font-size: 11px; }
      .bootstrap-select .bs-actionsbox { padding: 4px 8px !important; }
      .bootstrap-select .bs-actionsbox .btn-group { display: flex !important; width: auto !important; gap: 6px; }
      .bootstrap-select .bs-actionsbox .btn-group .btn { flex: 0 0 auto !important; width: auto !important; padding: 2px 6px !important; font-size: 10px !important; line-height: 1.2 !important; }
      .bootstrap-select .dropdown-menu li a span.text { font-size: 12px !important; }
      .bootstrap-select .dropdown-menu { max-height: 70vh !important; z-index: 3000 !important; }
      .bootstrap-select .dropdown-menu .inner { max-height: 64vh !important; }
      .summary-box { background: #F8F9FA; border: 1px solid #D6D8DB; border-radius: 8px; padding: 12px; margin-bottom: 10px; }
      .summary-title { font-weight: 600; font-size: 14px; color: #2C3E50; margin-bottom: 6px; }
      .btn-light { border: 1px solid #ced4da; color: #2C3E50; }
      .dt-buttons .dt-button { font-size: 8px !important; padding: 1px 8px !important; border-radius: 4px !important; background-color: #343a40 !important; color: white !important; border: none !important; margin-right: 5px; }
      .dataTables_wrapper .dataTables_paginate { float: left; }
      .dt-buttons .btn:hover { background-color: #495057 !important; color: white !important; }
      .dataTables_filter { display: none !important; }
      #sankeyPlot { height: 1300px !important; }
      #relatorio_tab1, #relatorio_tab2, #relatorio_tab3 { white-space: pre-wrap; font-size: 12px; }
      #tabela_dt { width: 100% !important; margin: 0 auto; }
      .dataTables_wrapper { width: 100% !important; overflow-x: auto !important; position: relative; }
      .dataTables_scrollBody { overflow-x: auto !important; max-width: 100% !important; }
      .dataTables_scrollHead { overflow: hidden !important; }
      table.dataTable { width: auto !important; margin-bottom: 0 !important; }
      table.dataTable td, table.dataTable th { white-space: nowrap !important; vertical-align: middle !important; padding: 8px 12px !important; }
      table.dataTable thead th { position: sticky !important; top: 0 !important; background-color: #f8f9fa !important; z-index: 10 !important; }
      table.dataTable td:not(.dt-wrap), table.dataTable th:not(.dt-wrap) { white-space: nowrap !important; }
      table.dataTable td.dt-wrap, table.dataTable th.dt-wrap { white-space: normal !important; word-break: break-word; overflow-wrap: break-word; line-height: 1.25; min-width: 200px; max-width: 320px; }
      .dataTables_paginate { margin-top: 10px !important; }
      ::-webkit-scrollbar { height: 8px; width: 8px; }
      ::-webkit-scrollbar-track { background: #f1f1f1; }
      ::-webkit-scrollbar-thumb { background: #888; border-radius: 4px; }
      ::-webkit-scrollbar-thumb:hover { background: #555; }
      @media screen and (max-width: 767px) {
        .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate { float: none !important; text-align: center !important; }
        .dataTables_wrapper .dataTables_paginate { margin-top: 0.5em !important; }
      }
      .tab-pane { height: calc(100vh - 120px) !important; display: flex; flex-direction: column; }
      .fluid-row { display: flex; flex: 1; min-height: 0; }
      .col-sm-3 { overflow: visible; padding-bottom: 20px; }
      .col-sm-9 { height: 100%; display: flex; flex-direction: column; }
      .dataTables_wrapper { flex: 1; display: flex; flex-direction: column; min-height: 0; }
      .dataTables_scrollBody { flex: 1; min-height: 0; }
      div.dt-buttons { display: inline-flex !important; gap: 6px; margin: 0 0 8px 0; }
      div.dt-buttons .dt-button, div.dt-buttons .btn { font-size: 8px !important; line-height: 1.2 !important; padding: 4px 10px !important; border-radius: 4px !important; width: auto !important; flex: 0 0 auto !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button { font-size: 6px !important; padding: 2px 6px !important; min-width: 10px !important; margin: 0 1px !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current { font-size: 6px !important; padding: 2px 6px !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button.previous, .dataTables_wrapper .dataTables_paginate .paginate_button.next { font-size: 6px !important; padding: 2px 6px !important; }
      .btn-group > .btn { margin: 2px 3px; }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover { background: #e9ecef !important; border: 1px solid #dee2e6 !important; }
      #ov_flags_tab1 .btn, #ov_flags_tab2 .btn, #ov_flags_tab3 .btn { font-size: 11px !important; padding: 2px 10px !important; }
    "))
  ),

  # ---- Aba 1 – Tabela ----
  nav_panel("Tabela de Dados",
    tags$p(class = "app-subtitle",
      "Explore os registros mensais da arrecadação da Compensação Financeira pela Exploração Mineral (CFEM) vinculados a processos minerários ativos do SIGMINE/ANM. ",
      "Filtre por substância (grupo/detalhe), fase, UF, município, processo, titular e parte declarante. ",
      "Os valores estão em R$ e as quantidades em kg e g. ",
      "Dados: ",
      tags$a("dados.gov.br/sistema-arrecadacao", href = "https://dados.gov.br/dados/conjuntos-dados/sistema-arrecadacao", target = "_blank"), ". ",
      "Fonte: Sistema de Arrecadação (download em ", data_atualizacao, "). "),
    tags$p(class = "note-text",
      "Nota - Alíquota CFEM utilizada para obtenção da coluna 'Valor Final': até 31/10/2017 (Lei 8.001/1990), adotamos: ouro em PLG = 0,2%; ouro fora de PLG = 2%; diamante em PLG = 0,2%; nióbio = 3%; e 2% para as demais substâncias aqui analisadas. A partir de 01/11/2017 (Lei 13.540/2017), as alíquotas passam a ouro = 1,5%, diamante = 2%, nióbio = 3% e 2% para todas as demais. O valor total é então calculado por 'Valor Arrecadado' ÷ alíquota vigente, por competência (ANO/MÊS) e por substância."),
    fluidRow(
      column(width = 3,
        div(class = "filters-card",
          tags$div(class = "mb-2", tags$strong("Filtros")),
          pickerInput("subs_tab1", "Substância(s) (grupo):",
                      choices = subs_all_grupo, selected = "OURO", multiple = TRUE, options = picker_opts),
          pickerInput("subs_det_tab1", "Substância(s) (detalhadas):",
                      choices = subs_all_original, selected = c("OURO", "MINÉRIO DE OURO", "OURO NATIVO"),
                      multiple = TRUE, options = picker_opts),
          pickerInput("fases_tab1", "Fase(s):",
                      choices = fases_all, selected = c("LAVRA GARIMPEIRA", "REQUERIMENTO DE LAVRA GARIMPEIRA"),
                      multiple = TRUE, options = picker_opts),
          checkboxGroupButtons("ov_flags_tab1", "Territórios Protegidos:",
                      choices = c("UC" = "UCov", "TI" = "TIov", "QUI" = "QUIov",
                                  "UC (10 km)" = "UCov10km", "TI (10 km)" = "TIov10km", "QUI (10 km)" = "QUIov10km"),
                      selected = c(), direction = "horizontal",
                      checkIcon = list(yes = icon("check"), no = icon("minus")), size = "sm", status = "light"),
          sliderInput("periodo_tab1", "Período (anos):",
                      min = min(anos_all), max = max(anos_all), value = c(2025, 2026), step = 1, sep = "", ticks = FALSE),
          pickerInput("ufs_tab1", "UF(s):", choices = ufs_all, selected = ufs_all, multiple = TRUE, options = picker_opts),
          pickerInput("muns_tab1", "Município(s):", choices = muns_all, multiple = TRUE, options = picker_opts),
          pickerInput("procs_tab1", "Processo(s):", choices = procs_all, multiple = TRUE, options = picker_opts),
          pickerInput("tits_tab1", "Titular(es):", choices = tits_all, multiple = TRUE, options = picker_opts),
          pickerInput("decl_tab1", "Parte(s) Declarante(s):", choices = decl_all, multiple = TRUE, options = picker_opts),
          tags$hr(),
          div(class = "d-grid gap-2 mt-1", actionButton("reset_tab1", "Resetar filtros", class = "btn btn-light btn-sm")),
          tags$hr(),
          div(class = "mb-0 d-flex gap-0", downloadButton("baixar_csv", "CSV"), downloadButton("baixar_xlsx", "Excel")),
          tags$hr(),
          downloadButton("baixar_pma_sel_tab1", "PMAs (seleção) .shp", class = "btn btn-light"),
          downloadButton("baixar_pma_titular_tab1", "PMAs (mesmo titular) .shp", class = "btn btn-light"),
          downloadButton("baixar_pma_declarante_tab1", "PMAs (mesma declarante) .shp", class = "btn btn-light")
        )
      ),
      column(width = 9,
        div(style = "overflow-x: auto;", DTOutput("tabela_dt", height = "100%")),
        br(),
        div(class = "summary-box",
          div(class = "summary-title", "Resumo da seleção"),
          verbatimTextOutput("relatorio_tab1", placeholder = TRUE))
      )
    )
  ),

  # ---- Aba 2 – Fluxo Sankey ----
  nav_panel("Fluxo Anual de Arrecadação",
    tags$p(class = "app-subtitle",
      "Fluxo anual da CFEM (R$ ou kg) entre os níveis: UF → Município → Titular → Processo → Parte declarante. ",
      "Ajuste “Máx. de nós por nível” para manter a legibilidade e refine com os filtros laterais. ",
      "Dados: ",
      tags$a("dados.gov.br/sistema-arrecadacao", href = "https://dados.gov.br/dados/conjuntos-dados/sistema-arrecadacao", target = "_blank"), ".",
      "Fonte: Sistema de Arrecadação (download em ", data_atualizacao, "). "),
    fluidRow(
      column(width = 3,
        div(class = "filters-card", tags$div(class = "mb-2", tags$strong("Filtros")),
          numericInput("max_nodes_sankey", "Máx. de nós por nível:", value = 10, min = 5, max = 200, step = 5),
          radioButtons("variavel_fluxo_tab2", "Métrica do fluxo:",
                       choices = c("Valor Recolhido (R$)" = "VALORarr", "Quantidade (Kg líquido)" = "PESO_KG_final_limpo"),
                       selected = "VALORarr"),
          pickerInput("subs_tab2", "Substância(s) (grupo):",
                      choices = subs_all_grupo, selected = "OURO", multiple = TRUE, options = picker_opts),
          pickerInput("subs_det_tab2", "Substância(s) (detalhadas):",
                      choices = subs_all_original, selected = c("OURO", "MINÉRIO DE OURO", "OURO NATIVO"),
                      multiple = TRUE, options = picker_opts),
          pickerInput("fases_tab2", "Fase(s):",
                      choices = fases_all, multiple = TRUE,
                      selected = c("LAVRA GARIMPEIRA", "REQUERIMENTO DE LAVRA GARIMPEIRA"), options = picker_opts),
          checkboxGroupButtons("ov_flags_tab2", "Territórios Protegidos:",
                      choices = c("UC" = "UCov", "TI" = "TIov", "QUI" = "QUIov",
                                  "UC (10 km)" = "UCov10km", "TI (10 km)" = "TIov10km", "QUI (10 km)" = "QUIov10km"),
                      selected = c(), direction = "horizontal",
                      checkIcon = list(yes = icon("check"), no = icon("minus")), size = "sm", status = "light"),
          sliderInput("periodo_tab2", "Período (anos):",
                      min = min(anos_all), max = max(anos_all), value = c(2025, 2026), step = 1, sep = "", ticks = FALSE),
          pickerInput("ufs_tab2", "UF(s):", choices = ufs_all, selected = ufs_all, multiple = TRUE, options = picker_opts),
          pickerInput("muns_tab2", "Município(s):", choices = muns_all, multiple = TRUE, options = picker_opts),
          pickerInput("procs_tab2", "Processo(s):", choices = procs_all, multiple = TRUE, options = picker_opts),
          pickerInput("tits_tab2", "Titular(es):", choices = tits_all, multiple = TRUE, options = picker_opts),
          pickerInput("decl_tab2", "Parte(s) Declarante(s):", choices = decl_all, multiple = TRUE, options = picker_opts),
          tags$hr(),
          div(class = "d-grid gap-2 mt-1", actionButton("reset_tab2", "Resetar filtros", class = "btn btn-light btn-sm")),
          tags$hr(),
          div(class = "mb-0 d-flex gap-0", downloadButton("baixar_csv_tab2", "CSV"), downloadButton("baixar_xlsx_tab2", "Excel")),
          tags$hr(),
          downloadButton("baixar_pma_sel_tab2", "PMAs (seleção) .shp", class = "btn btn-light"),
          downloadButton("baixar_pma_titular_tab2", "PMAs (mesmo titular) .shp", class = "btn btn-light"),
          downloadButton("baixar_pma_declarante_tab2", "PMAs (mesma declarante) .shp", class = "btn btn-light")
        )
      ),
      column(width = 9, sankeyNetworkOutput("sankeyPlot", height = "800px"),
        br(),
        div(class = "summary-box",
          div(class = "summary-title", "Resumo da seleção"),
          verbatimTextOutput("relatorio_tab2", placeholder = TRUE)))
    )
  ),

  # ---- Aba 3 – Série Temporal e Mapa ----
  nav_panel("Série Temporal e Mapa Processos Minerários",
    tags$p(class = "app-subtitle",
      "Série mensal da CFEM (R$ ou kg) conforme os filtros. ",
      "Veja a curva geral ou separe por Processo, Titular, Parte Declarante, Substância, Grupo ou Fase. ",
      "Defina o intervalo de anos e meses; pontos acima de 1,5×IQR são destacados como outliers. ",
      "Dados: ",
      tags$a("dados.gov.br/sistema-arrecadacao", href = "https://dados.gov.br/dados/conjuntos-dados/sistema-arrecadacao", target = "_blank"), ".",
      "Fonte: Sistema de Arrecadação (download em ", data_atualizacao, "). "),
    tags$p(class = "note-text",
      "Nota: os pontos destacados como outliers são calculados por grupo (Processo, Titular, etc.) ",
      "com base no critério do boxplot: valores acima de Q3 + 1,5 × IQR são considerados atípicos. ",
      "Quando o intervalo interquartílico (IQR) é nulo, aplica-se um ajuste usando o desvio padrão da série."),
    fluidRow(
      column(width = 3,
        div(class = "filters-card", tags$div(class = "mb-2", tags$strong("Filtros")),
          radioButtons("variavel_fluxo_tab3", "Métrica do fluxo:",
                       choices = c("Valor Recolhido (R$)" = "VALORarr", "Quantidade (Kg líquido)" = "PESO_KG_final_limpo"),
                       selected = "VALORarr"),
          pickerInput("subs_tab3", "Substância(s) (grupo):",
                      choices = subs_all_grupo, selected = "OURO", multiple = TRUE, options = picker_opts),
          pickerInput("subs_det_tab3", "Substância(s) (detalhadas):",
                      choices = subs_all_original, selected = c("OURO", "OURO NATIVO", "MINÉRIO DE OURO"),
                      multiple = TRUE, options = picker_opts),
          pickerInput("fases_tab3", "Fase(s):",
                      choices = fases_all, selected = c("LAVRA GARIMPEIRA", "REQUERIMENTO DE LAVRA GARIMPEIRA"),
                      multiple = TRUE, options = picker_opts),
          checkboxGroupButtons("ov_flags_tab3", "Territórios Protegidos:",
                      choices = c("UC" = "UCov", "TI" = "TIov", "QUI" = "QUIov",
                                  "UC (10 km)" = "UCov10km", "TI (10 km)" = "TIov10km", "QUI (10 km)" = "QUIov10km"),
                      selected = c(), direction = "horizontal",
                      checkIcon = list(yes = icon("check"), no = icon("minus")), size = "sm", status = "light"),
          selectInput("agrupamento_tab3", "Visualiza linhas por:",
                      choices = c("Geral" = "geral", "Processo" = "PROCESSO", "Titular" = "TITULAR",
                                  "Parte Declarante" = "NOME_arr", "Substância" = "SUBSarr",
                                  "Grupo (subs)" = "SUBSarrSIM", "Fase" = "FASE")),
          sliderInput("periodo_tab3", "Período (anos):",
                      min = min(anos_all), max = max(anos_all), value = c(2025, 2026), step = 1, sep = "", ticks = FALSE),
          sliderInput("meses_tab3", "Meses:", min = 1, max = 12, value = c(1, 12), step = 1, sep = "", ticks = FALSE),
          pickerInput("ufs_tab3", "UF(s):", choices = ufs_all, selected = ufs_all, multiple = TRUE, options = picker_opts),
          pickerInput("muns_tab3", "Município(s):", choices = muns_all, multiple = TRUE, options = picker_opts),
          pickerInput("procs_tab3", "Processo(s):", choices = procs_all, multiple = TRUE, options = picker_opts),
          pickerInput("tits_tab3", "Titular(es):", choices = tits_all, multiple = TRUE, options = picker_opts),
          pickerInput("decl_tab3", "Parte(s) Declarante(s):", choices = decl_all, multiple = TRUE, options = picker_opts),
          tags$hr(),
          div(class = "d-grid gap-2 mt-1", actionButton("reset_tab3", "Resetar filtros", class = "btn btn-light btn-sm")),
          tags$hr(),
          div(class = "mt-2 mb-2 d-flex gap-0", downloadButton("baixar_csv_tab3", "CSV"), downloadButton("baixar_xlsx_tab3", "Excel")),
          tags$hr(),
          downloadButton("baixar_pma_sel_tab3", "Download PMAs (seleção) .shp", class = "btn btn-light"),
          downloadButton("baixar_pma_titular_tab3", "Download PMAs (mesmo titular) .shp", class = "btn btn-light"),
          downloadButton("baixar_pma_declarante_tab3", "Download PMAs (mesma declarante) .shp", class = "btn btn-light")
        )
      ),
      column(width = 9,
        leafletOutput("mapa_cfem_pma_tab3", height = "525px"),
        br(),
        plotlyOutput("serie_temporal", height = "400px"),
        br(),
        plotlyOutput("grafico_outliers", height = "400px"),
        br(),
        div(class = "summary-box",
          div(class = "summary-title", "Resumo da seleção"),
          verbatimTextOutput("relatorio_tab3", placeholder = TRUE))
      )
    )
  ),

  # ---- Aba 4 – Consulta de Processos (Dossiê + Inaptos CFEM) ----
  nav_panel("Consulta de Processos",
    tags$p(class = "app-subtitle",
      "Consulta de processos minerários(Amazônia Legal). ",
      "Fonte: SIGMINE e Microdados/ANM (download em ", data_atualizacao, ")."),
    fluidRow(
      column(width = 4,
        div(class = "filters-card",
          tags$div(class = "mb-2", tags$strong("Filtros")),
          textInput("cp_busca", "Buscar (processo ou titular):", placeholder = "ex.: 850123/2016 ou nome..."),
          pickerInput("cp_subs_grp", "Substância (grupo):",
                      choices = cp_subs_grp, selected = "OURO", multiple = TRUE, options = picker_opts),
          pickerInput("cp_subs_det", "Substância (detalhe):",
                      choices = cp_subs_det,
                      selected = intersect(c("OURO", "MINÉRIO DE OURO", "OURO NATIVO"), cp_subs_det),
                      multiple = TRUE, options = picker_opts),
          pickerInput("cp_fase", "Fase do processo:",
                      choices = cp_fases_all,
                      selected = intersect(c("LAVRA GARIMPEIRA", "REQUERIMENTO DE LAVRA GARIMPEIRA"), cp_fases_all),
                      multiple = TRUE, options = picker_opts),
          pickerInput("cp_uf", "UF(s):",
                      choices = cp_ufs_all, selected = cp_ufs_all, multiple = TRUE, options = picker_opts),
          pickerInput("cp_mun", "Município(s):",
                      choices = cp_muns_all, multiple = TRUE, options = picker_opts),
          # ATUALIZADO (achado 2026-07): "Tipo de alerta" deixa de depender do
          # modo "inaptos" (que nao existe mais) — filtra a base completa
          # direto, via flag_* de situacao_atual (ver MOTIVO_TO_FLAGCOL_SA).
          # O slider de "ano da ultima declaracao de CFEM" saiu — era
          # CFEM-especifico e nao fazia sentido pra processo sem declaracao.
          # FIX (achado 2026-07): selected = inapto_cats (todas marcadas) fazia
          # o filtro estar SEMPRE ativo por padrao, mesmo sem o usuario tocar
          # em nada — como ele exige "pelo menos 1 flag de alerta = TRUE",
          # todo processo APTO (sem alerta nenhum) ficava escondido por
          # padrao. selected = character(0): nada marcado = sem filtro = mostra todos.
          pickerInput("cp_categoria", "Tipo de alerta:",
                      choices = inapto_cats, selected = character(0), multiple = TRUE, options = picker_opts),
          tags$hr(),
          uiOutput("cp_info"),
          tags$hr(),
          div(style = "max-height: 460px; overflow-y: auto;", DTOutput("cp_lista", height = "auto")),
          tags$hr(),
          uiOutput("cp_legenda_categorias"),
          tags$hr(),
          tags$details(
            style = "margin-top:4px;",
            tags$summary(
              style = "cursor:pointer; font-weight:600; color:#2C3E50; font-size:12px; padding:4px 0;",
              "Tabela de referência — eventos de aptidão"
            ),
            div(style = "margin-top:8px;", uiOutput("cp_eventos_ref"))
          )
        )
      ),
      column(width = 8,
        leafletOutput("cp_mapa", height = "400px"),
        br(),
        uiOutput("cp_dossie_box"),
        uiOutput("cp_relatorio_ui"),
        uiOutput("cp_grafico_ui"),
        uiOutput("cp_dossie_cabecalho"),
        uiOutput("cp_dossie_corpo")
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {

  # ---- Helpers ----
  filter_in <- function(df, col, sel) {
    if (is.null(sel) || length(sel) == 0) return(df)
    df[df[[col]] %in% sel, , drop = FALSE]
  }

  sync_pair <- function(session, id_group, id_detail, map_df, col_group, col_detail) {
    lock <- reactiveVal(FALSE)
    observeEvent(input[[id_group]], {
      if (lock()) return()
      lock(TRUE); on.exit(lock(FALSE), add = TRUE)
      g_sel <- input[[id_group]]
      valid_choices <- map_df |>
        dplyr::filter(.data[[col_group]] %in% g_sel) |>
        dplyr::pull(.data[[col_detail]]) |> unique() |> sort()
      updatePickerInput(session, id_detail, choices = valid_choices,
                        selected = intersect(isolate(input[[id_detail]]), valid_choices))
    }, ignoreInit = TRUE)
    observeEvent(input[[id_detail]], {
      if (lock()) return()
      lock(TRUE); on.exit(lock(FALSE), add = TRUE)
      d_sel <- input[[id_detail]]
      if (!length(d_sel)) return()
      parent_choices <- map_df |>
        dplyr::filter(.data[[col_detail]] %in% d_sel) |>
        dplyr::pull(.data[[col_group]]) |> unique() |> sort()
      updatePickerInput(session, id_group,
                        choices = sort(unique(map_df[[col_group]])), selected = parent_choices)
    }, ignoreInit = TRUE)
  }

  filtra_sobrepos <- function(df, flags) {
    if (length(flags) == 0) return(df)
    cols_ok <- intersect(flags, names(df))
    if (length(cols_ok) == 0) return(df)
    df |> dplyr::filter(rowSums(dplyr::across(dplyr::all_of(cols_ok), ~ dplyr::coalesce(.x, 0))) >= 1)
  }

  exportar_shapefile <- function(sf_obj, nome_base, temp_dir) {
    stopifnot(inherits(sf_obj, "sf"))
    if (nrow(sf_obj) == 0) stop("Sem feições para exportar.")
    path_base <- file.path(temp_dir, nome_base)
    if (is.na(sf::st_crs(sf_obj))) warning("Objeto sf sem CRS definido; o .prj pode sair vazio.")
    sf::st_write(sf_obj, paste0(path_base, ".shp"), delete_layer = TRUE, quiet = TRUE)
    arquivos <- list.files(temp_dir,
                           pattern = paste0("^", nome_base, "\\.(shp|shx|dbf|prj|cpg|qml|qpj)$"),
                           full.names = TRUE)
    zipfile <- file.path(temp_dir, paste0(nome_base, ".zip"))
    zip::zip(zipfile, files = arquivos, mode = "cherry-pick")
    zipfile
  }

  # NOVO (achado real, crash em producao 2026-07): defesa de ultima linha
  # contra "Don't know how to get polygon data from object of class
  # XY,GEOMETRYCOLLECTION,sfg" no leaflet::addPolygons(). A correcao de raiz
  # e no 08 (re-sanear apos ms_simplify + checagem pre-deploy) — isso aqui
  # e so pra garantir que, se algo mesmo assim escapar, um processo com
  # geometria ruim nao derruba a sessao inteira do usuario (o que
  # provavelmente explicava as quedas de conexao relatadas).
  sanear_geo_para_leaflet <- function(geo) {
    if (is.null(geo) || nrow(geo) == 0) return(geo)
    tipos <- as.character(sf::st_geometry_type(geo))
    ruins <- !(tipos %in% c("POLYGON", "MULTIPOLYGON"))
    if (!any(ruins)) return(geo)
    geo_ok    <- geo[!ruins, , drop = FALSE]
    geo_ruim  <- suppressWarnings(tryCatch(
      sf::st_collection_extract(geo[ruins, , drop = FALSE], "POLYGON"),
      error = function(e) NULL
    ))
    if (!is.null(geo_ruim) && nrow(geo_ruim) > 0) rbind(geo_ok, geo_ruim) else geo_ok
  }

  # ==========================================================================
  # ABA 4 — Consulta de Processos
  # ==========================================================================
  # Cascatas da aba Consulta (PMA)
  sync_pair(session, "cp_subs_grp", "cp_subs_det", map_subs_pma, "SUBSpmaGRP", "SUBS")

  observeEvent(input$cp_uf, {
    muns_disp <- if (length(input$cp_uf) > 0)
      sort(unique(na.omit(cp_map_mun$munic[cp_map_mun$uf %in% input$cp_uf])))
    else cp_muns_all
    updatePickerInput(session, "cp_mun", choices = muns_disp,
                      selected = intersect(isolate(input$cp_mun), muns_disp))
  }, ignoreInit = TRUE)

  cp_pma_filtrado <- reactive({
    df <- pma_attrs_cp
    if (!is.null(input$cp_uf) && length(input$cp_uf) > 0)
      df <- df[df$uf %in% input$cp_uf, , drop = FALSE]
    if (!is.null(input$cp_mun) && length(input$cp_mun) > 0)
      df <- df[df$munic %in% input$cp_mun, , drop = FALSE]
    if (!is.null(input$cp_fase) && length(input$cp_fase) > 0)
      df <- df[df$FASE %in% input$cp_fase, , drop = FALSE]
    if (!is.null(input$cp_subs_grp) && length(input$cp_subs_grp) > 0)
      df <- df[df$SUBSpmaGRP %in% input$cp_subs_grp, , drop = FALSE]
    if (!is.null(input$cp_subs_det) && length(input$cp_subs_det) > 0)
      df <- df[df$SUBS %in% input$cp_subs_det, , drop = FALSE]
    df
  })

  # ATUALIZADO (achado 2026-07): unificado — nao existe mais modo "todos" vs
  # "inaptos". Fonte agora e SEMPRE micro_processos (base completa) enriquecida
  # com situacao_atual (apto_operar/motivo_nao_apto/flag_*) — cobre processo
  # com ou sem CFEM. dossie_resumo_processo (CFEM) nao alimenta mais a lista;
  # continua existindo so como enriquecimento na caixa de detalhe (cp_dossie_box).
  # Exigencia de "filtro ativo" pra mostrar algo tambem caiu — a tabela ja e
  # DT server-side (server=TRUE, linha ~1374), aguenta a base inteira paginada.
  cp_lista_df <- reactive({
    req(micro_ok)
    pma_f <- cp_pma_filtrado()
    procs_filtrados <- pma_f$PROCESSO

    df <- micro_processos[micro_processos$processo %in% procs_filtrados, , drop = FALSE]

    if (!is.null(situacao_atual)) {
      cols_sa <- intersect(c("processo", "apto_operar", "motivo_nao_apto",
                              unname(MOTIVO_TO_FLAGCOL_SA)), names(situacao_atual))
      df <- merge(df, situacao_atual[, cols_sa, drop = FALSE], by = "processo", all.x = TRUE)
    }

    if (!is.null(input$cp_categoria) && length(input$cp_categoria) > 0 && !is.null(cfem_motivo_ref) && !is.null(situacao_atual)) {
      motivos_sel <- cfem_motivo_ref$motivo[cfem_motivo_ref$rotulo %in% input$cp_categoria]
      # OR entre as colunas flag_X correspondentes aos motivos selecionados —
      # nao depende da cascata categorica (motivo_nao_apto so guarda o
      # primeiro motivo na ordem de prioridade, mascarando condicoes
      # simultaneas — mesmo raciocinio ja aplicado no dossie_resumo_processo).
      cols_sel <- intersect(unname(MOTIVO_TO_FLAGCOL_SA[motivos_sel]), names(df))
      if (length(cols_sel) > 0) {
        tem_flag_sel <- Reduce(`|`, lapply(cols_sel, function(cl) !is.na(df[[cl]]) & df[[cl]]))
        df <- df[tem_flag_sel, , drop = FALSE]
      }
    }

    termo <- trimws(input$cp_busca %||% "")
    if (nchar(termo) > 0) {
      hit <- grepl(tolower(termo), tolower(df$processo), fixed = TRUE)
      if (!is.null(micro_pessoas)) {
        tit <- micro_pessoas[grepl("titular", micro_pessoas$relacao, ignore.case = TRUE), , drop = FALSE]
        proc_tit <- unique(tit$processo[grepl(tolower(termo), tolower(tit$nome %||% ""), fixed = TRUE)])
        hit <- hit | (df$processo %in% proc_tit)
      }
      df <- df[hit, , drop = FALSE]
    }

    cols <- intersect(c("processo", "uf", "fase", "municipios", "apto_operar", "motivo_nao_apto"), names(df))
    df[, cols, drop = FALSE]
  }) |> debounce(300)

  output$cp_info <- renderUI({
    if (!micro_ok) return(tags$div(style = "font-size:12px;color:#b02a37;",
      "Microdados não encontrados (rode o script 07)."))
    df <- cp_lista_df()
    if (is.null(df)) return(tags$div(style = "font-size:12px;color:#6c757d;",
      "Digite um processo ou titular para listar."))

    n <- nrow(df)
    fmt_rs <- function(x) paste0("R$ ", format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2))
    fmt_kg <- function(x) paste0(format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2), " kg")

    val_str <- if ("valor_total" %in% names(df)) {
      tot <- sum(df$valor_total, na.rm = TRUE)
      susp <- if ("valor_suspeito" %in% names(df)) sum(df$valor_suspeito, na.rm = TRUE) else NA_real_
      if (!is.na(susp)) paste0(fmt_rs(tot), " total | ", fmt_rs(susp), " suspeito") else fmt_rs(tot)
    } else ""

    kg_str <- if ("peso_total_kg" %in% names(df)) {
      tot <- sum(df$peso_total_kg, na.rm = TRUE)
      susp <- if ("peso_suspeito_kg" %in% names(df)) sum(df$peso_suspeito_kg, na.rm = TRUE) else NA_real_
      if (!is.na(susp)) paste0(fmt_kg(tot), " total | ", fmt_kg(susp), " suspeito") else fmt_kg(tot)
    } else ""

    tags$div(style = "font-size:12px; color:#2C3E50; line-height:1.8;",
      tags$div(tags$strong("Processos: "), format(n, big.mark=".", decimal.mark=",")),
      if (nchar(val_str) > 0) tags$div(tags$strong("Valor CFEM: "), val_str) else NULL,
      if (nchar(kg_str) > 0) tags$div(tags$strong("Peso: "), kg_str) else NULL
    )
  })

  output$cp_lista <- renderDT({
    validate(need(micro_ok, "Dados indisponíveis."))
    df <- cp_lista_df()
    validate(need(!is.null(df), "Use a busca ou os filtros para listar processos."))
    validate(need(nrow(df) > 0, "Nenhum processo encontrado."))

    # Traduz codigos de apto_operar/motivos_periodo_nao_apto para rotulo
    # amigavel antes de exibir (o dado por baixo continua sendo o codigo
    # original). apto_operar aqui e so CONTEXTO ("hoje"); o motivo real de
    # estar na lista e motivos_periodo_nao_apto (historico, declaracao a
    # declaracao) — ver cp_lista_df().
    if ("apto_operar" %in% names(df)) {
      df$apto_operar <- dplyr::case_when(
        df$apto_operar == "TRUE"  ~ "Apto",
        df$apto_operar == "FALSE" ~ "Nao apto",
        df$apto_operar == "em_analise" ~ "Em analise",
        TRUE ~ df$apto_operar
      )
    }
    if ("motivos_periodo_nao_apto" %in% names(df) && !is.null(cfem_motivo_ref)) {
      map_motivo <- setNames(cfem_motivo_ref$rotulo, cfem_motivo_ref$motivo)
      df$motivos_periodo_nao_apto <- vapply(strsplit(df$motivos_periodo_nao_apto, "; ", fixed = TRUE), function(m) {
        if (length(m) == 0 || all(m == "")) return("")
        paste(dplyr::coalesce(unname(map_motivo[m]), m), collapse = "; ")
      }, character(1))
    }

    nm <- names(df); cn <- nm
    cn[nm == "processo"]                      <- "Processo"
    cn[nm == "uf"]                            <- "UF"
    cn[nm == "fase"]                          <- "Fase"
    cn[nm == "apto_operar"]                   <- "Situacao hoje"
    cn[nm == "motivos_periodo_nao_apto"]      <- "Motivo(s) no periodo declarado"
    cn[nm == "valor_total"]                   <- "Valor CFEM (R$)"
    cn[nm == "peso_total_kg"]                 <- "Peso final (kg)"
    cn[nm == "n_declaracoes_periodo_nao_apto"]<- "Declaracoes em periodo nao apto"
    cn[nm == "municipios"]                     <- "Município"
    if ("valor_total" %in% names(df))
      df$valor_total <- paste0("R$ ", format(round(df$valor_total, 2),
                                              big.mark = ".", decimal.mark = ",", nsmall = 2))
    DT::datatable(df, rownames = FALSE, class = "compact", colnames = cn,
      selection = "single", extensions = "Scroller",
      options = list(scrollX = TRUE, dom = "tip", pageLength = 12, deferRender = TRUE,
                     columnDefs = list(list(targets = "_all", className = "dt-left"))))
  }, server = TRUE)

  cp_proc_sel <- reactive({
    s <- input$cp_lista_rows_selected
    df <- cp_lista_df()
    if (is.null(s) || length(s) == 0 || is.null(df) || nrow(df) == 0) return(NULL)
    col_proc <- intersect(c("processo", "PROCESSO"), names(df))[1]
    as.character(df[[col_proc]][s[1]])
  })

  # ---- Legenda de categorias (sempre visível) ----
  output$cp_legenda_categorias <- renderUI({
    if (is.null(cfem_motivo_ref)) return(NULL)
    cores <- c(
      fase_de_tramitacao_ou_pesquisa = "#6C757D",
      suspensa_ou_encerrada          = "#A32D2D",
      sem_licenca_ambiental_previa   = "#8E44AD",
      titulo_vencido                 = "#856404",
      vencimento_sem_data_a_revisar  = "#F9A825"
    )
    itens <- lapply(seq_len(nrow(cfem_motivo_ref)), function(i) {
      r <- cfem_motivo_ref[i, ]
      motivo_str <- as.character(r$motivo)
      cor <- if (!is.na(motivo_str) && motivo_str %in% names(cores)) cores[[motivo_str]] else "#6C757D"
      tags$div(style = "margin-bottom:6px; font-size:11px; line-height:1.3;",
        tags$span(style = paste0("display:inline-block; width:10px; height:10px; border-radius:2px; background:",
                                 cor, "; margin-right:6px;")),
        tags$strong(r$rotulo), tags$span(" — ", r$descricao))
    })
    tags$div(
      tags$div(style = "font-weight:600; font-size:12px; margin-bottom:6px; color:#2C3E50;",
               "Tipos de alerta"),
      itens
    )
  })

  # ---- Tabela de referência de eventos (HTML estática, agrupada por tipo) ----
  output$cp_eventos_ref <- renderUI({
    if (is.null(cfem_eventos_ref)) return(tags$em("Tabela não disponível."))
    df <- cfem_eventos_ref

    papel_cor <- c(MUDA_FASE = "#D4EDDA", FECHA = "#F8D7DA", SUSPENDE = "#FFF3CD", RETOMA = "#D1ECF1")
    papel_tx  <- c(MUDA_FASE = "#1E7E34", FECHA = "#C0392B", SUSPENDE = "#8A6D00", RETOMA = "#0C7C8C")

    badge <- function(papel) {
      cor <- if (!is.na(papel) && papel %in% names(papel_cor)) papel_cor[[papel]] else "#EEE"
      tx  <- if (!is.na(papel) && papel %in% names(papel_tx))  papel_tx[[papel]]  else "#333"
      tags$span(papel, style = paste0(
        "display:inline-block; padding:1px 8px; border-radius:10px; font-size:10px;",
        "font-weight:600; background:", cor, "; color:", tx, ";"))
    }

    tipos <- unique(df$tipo_proc)
    blocos <- lapply(tipos, function(tp) {
      sub <- df[df$tipo_proc == tp, , drop = FALSE]
      linhas <- lapply(seq_len(nrow(sub)), function(i) {
        r <- sub[i, ]
        tags$tr(style = "border-bottom:1px solid #F0F0F0;",
          tags$td(r$idevento, style = "padding:4px 6px; font-size:10px; color:#999; font-family:monospace; vertical-align:top;"),
          tags$td(
            tags$div(r$dsevento, style = "font-size:11px; color:#333;"),
            tags$div(r$descricao, style = "font-size:10px; color:#999;"),
            style = "padding:4px 6px;"),
          tags$td(badge(r$papel), style = "padding:4px 6px; white-space:nowrap; vertical-align:top; text-align:right;"))
      })
      tags$div(style = "margin-bottom:12px;",
        tags$div(tp, style = "font-weight:600; font-size:11px; color:#555; padding:4px 0; letter-spacing:0.3px;"),
        tags$table(style = "width:100%; border-collapse:collapse;",
          tags$tbody(linhas)))
    })

    tags$div(style = "max-height:480px; overflow-y:auto; padding-right:6px;", blocos)
  })

  # ---- Caixa de status do processo (dossie) — apto_operar/motivo_nao_apto,
  # direto do situacao_atual/dossie_resumo_processo, sem reclassificar nada ----
  # ATUALIZADO (achado 2026-07): fonte primaria passa a ser situacao_atual
  # (cobre TODO processo, independente de ter declarado CFEM) em vez de
  # dossie_resumo_processo (que so existe pra quem declarou CFEM). CFEM
  # continua disponivel, mas so como enriquecimento OPCIONAL — se nao
  # existir, a caixa aparece igual, so sem essa parte. tem_evento_suspensao/
  # tem_evento_anulacao deixam de vir pre-agregados do 07 (CFEM-gated) e
  # passam a ser calculados aqui, direto de eventos_classificados, pro
  # processo selecionado — mesma logica, sem depender de CFEM.
  output$cp_dossie_box <- renderUI({
    p <- cp_proc_sel(); if (is.null(p) || is.null(situacao_atual)) return(NULL)
    row <- situacao_atual[situacao_atual$processo == p, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    row <- row[1, ]

    # CFEM como extra opcional — pode nao existir, tudo bem
    # NOTA (2026-07-20): dossie_resumo_processo agora tem 1-3 linhas por
    # processo (1 por foco: ouro/cassiterita/outros, ver 07_proc_shiny_
    # dossie.R). row_cfem_all guarda TODAS; row_cfem = so a primeira, usada
    # para os campos que sao iguais em qualquer linha (n_declaracoes etc,
    # agregados so por processo, nao por foco).
    # NOTA (2026-07-21, defensivo): se dossie_resumo_processo.rds for de uma
    # versao anterior do 07 (sem a coluna foco -- ouro/cassiterita/outros),
    # tem_foco fica FALSE e o bloco de CFEM abaixo cai num fallback
    # combinado, em vez de quebrar a caixa inteira (achado real: warning
    # "Unknown or uninitialised column: foco" -- dado desatualizado, precisa
    # rodar o 07 atualizado pra regerar o rds com a coluna nova).
    row_cfem_all <- if (dossie_ok) dossie_resumo_processo[dossie_resumo_processo$processo == p, , drop = FALSE] else NULL
    tem_cfem <- !is.null(row_cfem_all) && nrow(row_cfem_all) > 0
    tem_foco <- tem_cfem && "foco" %in% names(row_cfem_all)
    row_cfem <- if (tem_cfem) row_cfem_all[1, ] else NULL

    ev_p <- if (!is.null(eventos_classificados)) eventos_classificados[eventos_classificados$processo == p, , drop = FALSE] else NULL
    tem_evento_suspensao <- !is.null(ev_p) && any(ev_p$papel == "SUSPENDE", na.rm = TRUE)
    tem_evento_anulacao  <- !is.null(ev_p) && any(ev_p$papel == "FECHA",    na.rm = TRUE)

    cod <- row$motivo_nao_apto %||% NA_character_
    ref_row <- if (!is.null(cfem_motivo_ref) && !is.na(cod))
      cfem_motivo_ref[cfem_motivo_ref$motivo == cod, , drop = FALSE] else NULL
    rot <- if (!is.null(ref_row) && nrow(ref_row) > 0) ref_row$rotulo[1] else
      if (identical(row$apto_operar, "TRUE")) "Apto" else "Situacao a revisar"
    desc <- if (!is.null(ref_row) && nrow(ref_row) > 0) ref_row$descricao[1] else NULL

    cor_cfg <- if (identical(row$apto_operar, "TRUE")) {
      list(bg = "#EAF6EC", bd = "#2D6A4F", tx = "#1B4332")
    } else if (identical(cod, "suspensa_ou_encerrada")) {
      list(bg = "#FCEBEB", bd = "#A32D2D", tx = "#501313")
    } else if (identical(cod, "sem_licenca_ambiental_previa")) {
      list(bg = "#F4ECF7", bd = "#8E44AD", tx = "#4A235A")
    } else if (identical(cod, "titulo_vencido") || identical(cod, "vencimento_sem_data_a_revisar")) {
      list(bg = "#FFF3CD", bd = "#856404", tx = "#412402")
    } else if (identical(cod, "titulo_vencido_renovacao_protocolada")) {
      list(bg = "#E7F1FA", bd = "#31708F", tx = "#1B4F63")
    } else if (identical(cod, "fase_de_tramitacao_ou_pesquisa")) {
      list(bg = "#E2E3E5", bd = "#6C757D", tx = "#2C2C2C")
    } else {
      list(bg = "#F0F0F0", bd = "#6C757D", tx = "#2C2C2C")
    }

    linha <- function(rotulo, val) if (!is.null(val) && length(val) && !is.na(val) && val != "")
      tags$div(tags$strong(paste0(rotulo, ": ")), val) else NULL
    fmt_rs <- function(x) if (length(x) && !is.na(x))
      paste0("R$ ", format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2)) else NA
    fmt_kg <- function(x) if (length(x) && !is.na(x))
      paste0(format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2), " kg") else NA

    pma_row <- tryCatch({
      pa <- sf::st_drop_geometry(pma_simpl)
      pa[as.character(pa$PROCESSO) == p, , drop = FALSE]
    }, error = function(e) data.frame())
    sobrep_ti  <- nrow(pma_row) > 0 && "TIov10km"   %in% names(pma_row) && isTRUE(as.logical(pma_row$TIov10km[1]))
    sobrep_uc  <- nrow(pma_row) > 0 && "UCov2_10km" %in% names(pma_row) && isTRUE(as.logical(pma_row$UCov2_10km[1]))
    sobrep_qui <- nrow(pma_row) > 0 && "QUIov"      %in% names(pma_row) && isTRUE(as.logical(pma_row$QUIov[1]))

    tags$div(
      style = paste0("background:", cor_cfg$bg, "; border-left:5px solid ", cor_cfg$bd,
                     "; color:", cor_cfg$tx, "; padding:12px 16px; border-radius:6px; margin-bottom:14px;"),
      tags$div(style = "font-weight:600; margin-bottom:2px; text-transform:uppercase;", rot),
      tags$div(style = "font-size:12px; margin-bottom:6px; opacity:0.8;", p),
      if (!is.null(desc)) tags$div(style = "font-size:12px; margin-bottom:8px; font-style:italic;", desc),

      # --- Grupo 1: fase/situacao do processo ---
      linha("Fase (evento ANM)", row$fase_evento),
      linha("Fase (PMA/SIGMINE)", row$fase_pma),
      if (isTRUE(row$fase_diverge_pma))
        tags$div(style = "font-size:11px; color:#721C24;",
                 "Fase do historico de eventos diverge da fase registrada no PMA/SIGMINE"),

      # --- Grupo 2: titular ---
      linha("Titular", row$titular_atual),

      # --- Grupo 3: prazos/pendencias administrativas (mesma familia de
      # informacao — protocolo + tempo decorrido — agrupadas juntas) ---
      # NOVO (achado 2026-07): duracao da protecao Art. 211/213, quando aplicavel
      # — texto informativo (mora administrativa protege o titular, nao e irregularidade)
      if (isTRUE(row$protegido_211_213) && !is.null(row$dt_protocolo_renovacao_plg) && !is.na(row$dt_protocolo_renovacao_plg))
        tags$div(style = "font-size:11px; margin-top:2px;",
                 sprintf("Renovacao solicitada no prazo (Art. 211/213) e sem resposta da ANM ha %s",
                         fmt_duracao_anos_meses(row$dt_protocolo_renovacao_plg))),
      # AJUSTE (pedido usuario): agora com duracao (mesmo estilo da linha
      # acima), nao so a data — "protocolada ha X anos e X meses (DD/MM/AAAA)".
      if (identical(row$fase_evento, "PLG"))
        linha("Última licença ambiental protocolada",
              if (!is.null(row$dt_ultimo_protocolo_licenca_ambiental) && !is.na(row$dt_ultimo_protocolo_licenca_ambiental))
                sprintf("há %s (%s)",
                        fmt_duracao_anos_meses(row$dt_ultimo_protocolo_licenca_ambiental),
                        format(row$dt_ultimo_protocolo_licenca_ambiental, "%d/%m/%Y"))
              else "Nenhuma"),

      # --- Grupo 4: CFEM/financeiro ---
      # AJUSTE (2026-07-20): valor/peso sempre separados por foco (ouro/
      # cassiterita/outros) -- nunca somados juntos, pra nao repetir o
      # problema de misturar substancias de magnitude diferente no mesmo
      # numero (ver nota no 05_integracao_final.R / categorizar_foco()).
      if (tem_cfem) linha("Declaracoes de CFEM", paste0(row_cfem$n_declaracoes, " total | ", row_cfem$n_declaracoes_fora_vig, " fora de vigencia de titulo")),
      if (tem_cfem && tem_foco) {
        rotulo_foco <- c(ouro = "Ouro", cassiterita = "Cassiterita", outros = "Outros")
        tagList(lapply(names(rotulo_foco), function(f) {
          rf <- row_cfem_all[row_cfem_all$foco == f, , drop = FALSE]
          if (nrow(rf) == 0) return(NULL)
          tagList(
            linha(paste0("Valor arrecadado — ", rotulo_foco[[f]]), fmt_rs(rf$valor_total[1])),
            linha(paste0("Peso comercializado — ", rotulo_foco[[f]]), fmt_kg(rf$peso_total_kg[1]))
          )
        }))
      },
      # Fallback (rds desatualizado, sem coluna foco): mostra combinado, sem
      # quebrar a caixa. So aparece se voce ainda nao rodou o 07 atualizado.
      if (tem_cfem && !tem_foco) {
        tagList(
          tags$div(style = "font-size:10px; color:#856404; font-style:italic; margin-bottom:2px;",
                   "Dado desatualizado (rode o 07 atualizado p/ separar por substancia)"),
          linha("Valor arrecadado", fmt_rs(row_cfem$valor_total)),
          linha("Peso comercializado", fmt_kg(row_cfem$peso_total_kg))
        )
      },
      if (!tem_cfem) tags$div(style = "font-size:11px; color:#6C757D; font-style:italic;", "Sem declaracao de CFEM registrada"),

      # --- Grupo 5: alertas administrativos/judiciais ---
      if (tem_evento_suspensao)
        tags$div(style = "margin-top:6px; font-size:11px; color:#721C24;",
                 "Ha pelo menos 1 evento de suspensao no historico administrativo"),
      if (tem_evento_anulacao)
        tags$div(style = "font-size:11px; color:#721C24;",
                 "Ha pelo menos 1 evento de anulacao/encerramento no historico administrativo"),

      # --- Grupo 6: territorial ---
      if (sobrep_ti)  tags$div(style = "font-size:11px; color:#721C24;", "Sobrepoe terra indigena (10 km)"),
      if (sobrep_uc)  tags$div(style = "font-size:11px; color:#721C24;", "Sobrepoe unidade de conservacao (10 km)"),
      if (sobrep_qui) tags$div(style = "font-size:11px; color:#721C24;", "Sobrepoe territorio quilombola")
    )
  })

  # ---- Grafico de historico do processo (Peca C: grafico_historico_processo)
  # Substitui o antigo par cp_grafico_valor/cp_grafico_peso (plotly cru) e a
  # tabela cp_tabela_fases — tudo consolidado num unico grafico ggplot, com ou
  # sem CFEM, evento a evento, sem nenhuma agregacao.
  #
  # RENDER ESTATICO (renderPlot), NAO ggplotly(): geom_rect(ymin/ymax = Inf) e
  # geom_text(y = Inf) — a faixa vermelha de vigencia e os rotulos verticais —
  # nao convertem de forma confiavel para plotly (limitacao conhecida da
  # conversao ggplot2->plotly com extensao infinita). O mesmo grafico, do
  # jeito que esta, ja foi validado visualmente no script da COOGAM rodando
  # como ggplot2 puro — aqui e o mesmo caminho, so que dentro do Shiny.
  # Perde-se zoom/hover interativo; ganha-se ficar igual ao que ja validamos. ----
  output$cp_grafico_ui <- renderUI({
    p <- cp_proc_sel()
    if (is.null(p)) return(NULL)
    tagList(
      tags$div(style = "font-weight:600; margin-bottom:6px; color:#2C3E50;",
               "Historico do processo"),
      plotlyOutput("cp_grafico_valor", height = "260px"),
      div(style = "height:8px;"),
      plotlyOutput("cp_grafico_peso", height = "260px"),
      div(style = "height:14px;"),
      # AJUSTE (pedido direto): as duas tabelas de eventos (fases e
      # multas/infracoes) passam a ficar em abas separadas, em vez de
      # empilhadas — mais facil de navegar quando as duas tem muitas linhas.
      # tabsetPanel/tabPanel (shiny puro) em vez de navset_tab/nav_panel
      # (bslib) -- este bloco e renderizado dinamicamente dentro de
      # renderUI() a cada troca de processo, e o id auto-gerado do
      # navset_tab pode colidir/quebrar a re-renderizacao nesse contexto
      # (achado real: caixa de resumo cp_dossie_box sumiu ao trocar de
      # processo). tabsetPanel nao tem esse problema.
      tabsetPanel(
        tabPanel("Fases do processo", DTOutput("cp_tabela_fases")),
        tabPanel("Multas e infracoes", DTOutput("cp_tabela_penalidades"))
      ),
      div(style = "height:14px;")
    )
  })

  cp_dados_cfem_proc <- reactive({
    p <- cp_proc_sel(); req(p)
    if (is.null(cfem_declaracoes_dossie)) return(NULL)
    d <- cfem_declaracoes_dossie[cfem_declaracoes_dossie$PROCESSO == p, , drop = FALSE]
    if (nrow(d) == 0) NULL else d
  })

  # Versao PLOTLY NATIVA (nao ggplotly) — mesmo padrao do app.R original,
  # com hover e aparencia melhores. Ver graficos_historico.R para a fonte
  # da verdade / comentarios completos.
  cp_grafico_reactivo_plotly <- function(variavel) {
    reactive({
      p <- cp_proc_sel(); req(p)
      grafico_historico_processo_plotly(
        processo_alvo = p,
        dados_cfem = cp_dados_cfem_proc(),
        situacao_documental = situacao_documental,
        protocolos_licenca_ambiental = protocolos_licenca_ambiental,
        eventos_classificados = eventos_classificados,
        serie_fase_status = serie_fase_status,
        eventos_renovacao_plg = eventos_renovacao_plg,
        intervalos_gu_aut_pesq = intervalos_gu_aut_pesq,
        variavel = variavel
      )
    })
  }
  cp_grafico_valor_obj <- cp_grafico_reactivo_plotly("valor")
  cp_grafico_peso_obj  <- cp_grafico_reactivo_plotly("peso")

  output$cp_grafico_valor <- renderPlotly({
    req(cp_grafico_valor_obj())
    cp_grafico_valor_obj() |> plotly::config(displayModeBar = FALSE)
  })
  output$cp_grafico_peso <- renderPlotly({
    req(cp_grafico_peso_obj())
    cp_grafico_peso_obj() |> plotly::config(displayModeBar = FALSE)
  })

  # ---- Historico de autorizacoes (fases do processo) — serie_fase_status
  # (blocos de fase/status, do 06), FILTRADO para so o que responde a
  # pergunta que essa tabela existe pra responder: "esse titulo permitia
  # extracao de fato, e o que aconteceu com ele?" — decisao explicita do
  # usuario: fora requerimento/pesquisa/disponibilidade (REQ PLG, REQ PESQ,
  # AUT PESQ etc.), dentro PRE_AUTORIZACAO (contexto de quando o processo
  # comecou) + as 4 fases que operam (mesmo vocabulario do 06/07) + protocolos
  # de licenca ambiental como linha propria (antes so apareciam no grafico).
  FASES_QUE_OPERAM_TABELA <- c("CONC LAV", "LICEN", "PLG", "REG EXT")  # ver 06_serie_temporal.R / 07_proc_shiny_dossie.R

  # Sentinela pra ordenar com PRE_AUTORIZACAO (dt_inicio = NA) SEMPRE primeiro
  # — arrange()/order() por padrao colocam NA por ultimo, o que jogava
  # PRE_AUTORIZACAO pro fim da tabela (errado: e o inicio da historia).
  ordenar_por_data <- function(df) {
    df[order(dplyr::coalesce(df$dt_inicio, as.Date("1900-01-01"))), , drop = FALSE]
  }

  # NOVO (achado 2026-07, Art. 211/213): formata duracao em "Y anos e X meses"
  # com singular/plural corretos — usado no texto de protocolo de renovacao
  # parado sem resposta da ANM. Nao ha teto legal pra essa espera (Art. 213,
  # paragrafo unico: "permanecera em vigor ate manifestacao definitiva do
  # DNPM") — o texto e informativo (tempo de mora), nao uma acusacao.
  fmt_duracao_anos_meses <- function(data_inicio, data_fim = Sys.Date()) {
    if (is.null(data_inicio) || is.na(data_inicio)) return(NA_character_)
    meses_totais <- (as.integer(format(data_fim, "%Y")) - as.integer(format(data_inicio, "%Y"))) * 12L +
      (as.integer(format(data_fim, "%m")) - as.integer(format(data_inicio, "%m"))) -
      (as.integer(format(data_fim, "%d")) < as.integer(format(data_inicio, "%d")))
    meses_totais <- max(meses_totais, 0L)
    anos  <- meses_totais %/% 12L
    meses <- meses_totais %% 12L
    txt_anos  <- if (anos  == 1) "1 ano"  else sprintf("%d anos", anos)
    txt_meses <- if (meses == 1) "1 mes"  else sprintf("%d meses", meses)
    if (anos > 0 && meses > 0) paste(txt_anos, "e", txt_meses)
    else if (anos > 0)          txt_anos
    else                        txt_meses
  }

  cp_historico_fases_p <- reactive({
    p <- cp_proc_sel(); req(p)
    if (is.null(serie_fase_status)) return(NULL)
    h <- serie_fase_status[serie_fase_status$processo == p, , drop = FALSE]
    if (nrow(h) == 0) return(NULL)

    # Filtro: PRE_AUTORIZACAO sempre entra; o resto so se for uma das 4 fases
    # que de fato autorizam extracao (descarta requerimento/pesquisa/
    # disponibilidade, e qualquer GAP que tenha herdado uma dessas fases).
    h <- h[h$status == "PRE_AUTORIZACAO" | h$fase %in% FASES_QUE_OPERAM_TABELA, , drop = FALSE]
    if (nrow(h) == 0) return(NULL)
    h <- ordenar_por_data(h)
    h$evento_inicio <- NA_character_
    h$evento_fim    <- NA_character_

    ev_p <- if (!is.null(eventos_classificados))
      eventos_classificados[eventos_classificados$processo == p, , drop = FALSE] else NULL

    desc_evento_em <- function(data_alvo) {
      if (is.null(ev_p) || is.na(data_alvo) || nrow(ev_p) == 0) return(NA_character_)
      m <- ev_p[ev_p$dtevento == data_alvo, , drop = FALSE]
      if (nrow(m) == 0) return(NA_character_)
      paste(unique(m$dsevento), collapse = " | ")
    }
    for (i in seq_len(nrow(h))) {
      h$evento_inicio[i] <- desc_evento_em(h$dt_inicio[i])
      h$evento_fim[i]    <- desc_evento_em(h$dt_fim[i])
    }

    # Injecao da fase "VENCIDA": se o titulo mais recente ja venceu e a ultima
    # fase ATIVA (a que esta com dt_fim em aberto) comecou antes desse
    # vencimento, fecha ela na data do vencimento e abre uma nova fase
    # sintetica "VENCIDA" a partir do dia seguinte.
    # ATUALIZADO (Art. 211/213, achado 2026-07): se ha protocolo de renovacao
    # de PLG (idevento 521) feito ate a data de vencimento e sem indeferimento
    # (522) registrado depois dele, o rotulo "sem renovacao" e factualmente
    # errado — vira "VENCIDA (RENOVAÇÃO PROTOCOLADA)", com texto proprio.
    if (!is.null(situacao_documental)) {
      doc_p <- situacao_documental[situacao_documental$processo == p, , drop = FALSE]
      datas_venc <- stats::na.omit(doc_p$dt_vencimento)
      if (length(datas_venc) > 0) {
        max_venc <- max(datas_venc)
        if (max_venc < Sys.Date()) {
          ev_renov_p <- if (!is.null(eventos_renovacao_plg))
            eventos_renovacao_plg[eventos_renovacao_plg$processo == p, , drop = FALSE] else NULL
          protocolos_renov_p <- if (!is.null(ev_renov_p)) sort(ev_renov_p$dtevento[ev_renov_p$idevento == "521"]) else as.Date(character())
          indeferimentos_renov_p <- if (!is.null(ev_renov_p)) sort(ev_renov_p$dtevento[ev_renov_p$idevento == "522"]) else as.Date(character())
          candidatos_protocolo <- protocolos_renov_p[protocolos_renov_p <= max_venc]
          protegido_211_213 <- length(candidatos_protocolo) > 0 &&
            !any(indeferimentos_renov_p > max(candidatos_protocolo))

          idx_ativa <- which(is.na(h$dt_fim) & h$status == "ATIVA" &
                                !is.na(h$dt_inicio) & max_venc >= h$dt_inicio)
          if (length(idx_ativa) > 0) {
            novas <- list()
            for (i in idx_ativa) {
              h$dt_fim[i]      <- max_venc
              h$evento_fim[i]  <- "Vencimento do titulo"
              nl <- h[i, ]
              nl$dt_inicio     <- max_venc + 1
              nl$dt_fim        <- as.Date(NA)
              if (protegido_211_213) {
                nl$status        <- "VENCIDA (RENOVAÇÃO PROTOCOLADA)"
                nl$evento_inicio <- sprintf(
                  "Prazo expirado — renovacao da PLG protocolada em %s, aguardando manifestacao da ANM (Art. 211/213, Portaria DNPM 155/2016)",
                  format(max(candidatos_protocolo), "%d/%m/%Y")
                )
              } else {
                nl$status        <- "VENCIDA"
                nl$evento_inicio <- "Prazo expirado (sem renovacao protocolada)"
              }
              nl$evento_fim    <- NA_character_
              novas[[length(novas) + 1]] <- nl
            }
            h <- dplyr::bind_rows(h, novas)
          }
        }
      }
    }

    # Intercala protocolos de licenca ambiental como linha propria (pedido:
    # "mostrar quando as lic amb foram protocoladas" direto na tabela, nao
    # so no hover do grafico).
    if (!is.null(protocolos_licenca_ambiental)) {
      lic_p <- protocolos_licenca_ambiental[protocolos_licenca_ambiental$processo == p, , drop = FALSE]
      if (nrow(lic_p) > 0) {
        lic_rows <- data.frame(
          processo = p, dt_inicio = lic_p$dt_protocolo, dt_fim = as.Date(NA),
          fase = "LIC AMB", status = "PROTOCOLADA",
          evento_inicio = lic_p$dsevento, evento_fim = NA_character_
        )
        h <- dplyr::bind_rows(h, lic_rows)
      }
    }

    # NOVO (achado 2026-07): mesmo tratamento pro protocolo de renovacao de
    # PLG (idevento 521, Art. 211) — hoje so aparecia no hover do grafico,
    # nao na tabela. Status proprio "RENOVACAO PROTOC" (verde claro,
    # distinto do azul da lic amb — pedido do usuario 2026-07) — mesma
    # semantica (pedido em aberto, PROTOC, aguardando resposta da ANM).
    if (!is.null(eventos_renovacao_plg)) {
      renov_p <- eventos_renovacao_plg[eventos_renovacao_plg$processo == p &
                                          eventos_renovacao_plg$idevento == "521", , drop = FALSE]
      if (nrow(renov_p) > 0) {
        renov_rows <- data.frame(
          processo = p, dt_inicio = renov_p$dtevento, dt_fim = as.Date(NA),
          fase = "PLG", status = "RENOVACAO PROTOC",
          evento_inicio = "PLG/RENOVACAO DO PRAZO PLG PROTOC", evento_fim = NA_character_
        )
        h <- dplyr::bind_rows(h, renov_rows)
      }
    }

    ordenar_por_data(h)
  })

  # NOVO: fatorado do renderDT abaixo pra ser reaproveitado pelos exports
  # (Word e Excel) sem duplicar a logica de formatacao.
  construir_tabela_fases_export <- function(h) {
    fmt_data_fim <- function(x, status) {
      dplyr::case_when(
        !is.na(x)                                        ~ format(x, "%d/%m/%Y"),
        status %in% c("PROTOCOLADA", "RENOVACAO PROTOC")  ~ "—",
        TRUE                                              ~ "Atual"
      )
    }
    data.frame(
      Status          = h$status,
      Fase            = dplyr::coalesce(h$fase, "—"),
      `Data Início`   = ifelse(is.na(h$dt_inicio), "—", format(h$dt_inicio, "%d/%m/%Y")),
      `Data Fim`      = fmt_data_fim(h$dt_fim, h$status),
      `Evento Início` = dplyr::coalesce(h$evento_inicio, "—"),
      `Evento Fim`    = dplyr::coalesce(h$evento_fim, "—"),
      check.names = FALSE
    )
  }

  output$cp_tabela_fases <- renderDT({
    h <- cp_historico_fases_p()
    validate(need(!is.null(h), "Sem historico de fases que autorizem extracao para este processo."))
    tab <- construir_tabela_fases_export(h)

    cores_status <- c(ATIVA = "#D4EDDA", SUSPENSA = "#FFF3CD", ENCERRADA = "#F8D7DA",
                       VENCIDA = "#F8D7DA", `VENCIDA (RENOVAÇÃO PROTOCOLADA)` = "#FFE5B4",
                       GAP = "#E2E3E5", PRE_AUTORIZACAO = "#E2E3E5",
                       PROTOCOLADA = "#D1ECF1", `RENOVACAO PROTOC` = "#B2E4D8")

    # ordering = FALSE: essa tabela e uma narrativa cronologica construida por
    # nos (PRE_AUTORIZACAO sempre primeiro) — deixar o usuario clicar no
    # cabecalho e reordenar por texto quebraria essa leitura.
    DT::datatable(tab, rownames = FALSE, class = "compact", selection = "none",
      options = list(scrollX = TRUE, dom = "t", pageLength = -1, ordering = FALSE)) |>
      DT::formatStyle("Status", backgroundColor = DT::styleEqual(names(cores_status), unname(cores_status)))
  })

  # ---- Historico de penalidades e ocorrencias (Fase 3, Bloco I do 06) ----
  # Consolida 3 fontes ja existentes (categ_penalidade, papel SUSPENDE/
  # RETOMA, classificacao judicial de DESPACHO DIVERSO PUBL) — ver comentario
  # do Bloco I no 06_serie_temporal.R. Mesma logica de reactive/renderDT que
  # a tabela de fases acima, so trocando a fonte.
  cp_penalidades_p <- reactive({
    p <- cp_proc_sel(); req(p)
    if (is.null(eventos_penalidades_ocorrencias)) return(NULL)
    h <- eventos_penalidades_ocorrencias[eventos_penalidades_ocorrencias$processo == p, , drop = FALSE]
    if (nrow(h) == 0) return(NULL)
    dplyr::arrange(h, dtevento)
  })

  # NOVO: fatorado do renderDT abaixo pra ser reaproveitado no export do
  # relatorio docx, mesmo padrao de construir_tabela_fases_export.
  # AJUSTE (pedido usuario): Tipo vira primeira coluna, em CAPSLOCK — mesmo
  # padrao visual da coluna Fase/Status na tabela de historico de fases.
  construir_tabela_penalidades_export <- function(h) {
    data.frame(
      Tipo                    = toupper(h$tipo),
      Data                    = format(h$dtevento, "%d/%m/%Y"),
      Evento                  = dplyr::coalesce(h$dsevento, "—"),
      `Descrição/Publicação`  = dplyr::coalesce(h$dspublicacaodou, "—"),
      check.names = FALSE
    )
  }

  output$cp_tabela_penalidades <- renderDT({
    h <- cp_penalidades_p()
    validate(need(!is.null(h), "Sem penalidades ou ocorrencias registradas para este processo."))
    tab <- construir_tabela_penalidades_export(h)

    # AJUSTE (pedido usuario): paleta redesenhada — vermelho reservado pra
    # suspensao/embargo (administrativo E judicial, e o bloqueio de leilao,
    # que tambem impede operacao). Multa/infracao/advertencia saem do
    # vermelho e ganham tons de laranja/amarelo DISTINTOS entre si (nao mais
    # a mesma cor umas das outras). Retomada/termino/anulacao — reversao de
    # um impedimento — ficam verdes. Chaves em CAPSLOCK pra bater com o
    # toupper() aplicado na coluna Tipo acima.
    cores_tipo <- c(
      "SUSPENSÃO/EMBARGO"                            = "#F5C6CB",
      "SUSPENSÃO JUDICIAL (TÍTULO/LAVRA)"            = "#F5C6CB",
      "SUSPENSÃO/SOBRESTAMENTO JUDICIAL (ANÁLISE)"   = "#F5C6CB",
      "BLOQUEIO JUDICIAL DE LEILÃO/DISPONIBILIDADE"  = "#F5C6CB",
      "MULTA"                                        = "#FFE0B2",
      "INFRAÇÃO"                                     = "#FFCC80",
      "INFRAÇÃO MANTIDA (DEFESA NEGADA)"              = "#FFCC80",
      "ADVERTÊNCIA"                                   = "#FFF3CD",
      "RETOMADA/DESEMBARGO"                          = "#D4EDDA",
      "TÉRMINO DE SUSPENSÃO JUDICIAL"                = "#D4EDDA",
      "ANULAÇÃO JUDICIAL DE CADUCIDADE"              = "#D4EDDA",
      "DECISÃO JUDICIAL (OUTRA)"                      = "#E2E3E5"
    )

    DT::datatable(tab, rownames = FALSE, class = "compact", selection = "none",
      options = list(scrollX = TRUE, dom = "t", pageLength = -1, ordering = FALSE)) |>
      DT::formatStyle("Tipo", backgroundColor = DT::styleEqual(names(cores_tipo), unname(cores_tipo)))
  })

  # ==========================================================================
  # Dossie completo (Excel) — todas as tabelas do dossie, multi-aba.
  # AJUSTE (2026-07): export do relatorio Word removido (so o Excel fica).
  # ==========================================================================
  output$cp_relatorio_ui <- renderUI({
    p <- cp_proc_sel()
    if (is.null(p)) return(NULL)
    tags$div(style = "display:flex; gap:8px; margin-bottom:10px;",
      downloadButton("cp_dossie_xlsx", "Dados compilados", class = "btn btn-light btn-sm")
    )
  })

  output$cp_dossie_xlsx <- downloadHandler(
    filename = function() {
      p <- cp_proc_sel() %||% "processo"
      paste0("dossie_", gsub("[/]", "-", p), "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      p <- cp_proc_sel()
      validate(need(!is.null(p), "Selecione um processo para gerar o dossiê."))
      h <- cp_historico_fases_p()
      hp <- cp_penalidades_p()

      # HistoricoEventos: mesma logica de apresentacao da tela (cp_eventos) —
      # ordenado por data decrescente, coluna tecnica "papel" (usada so pra
      # colorir a linha na tela) fora, nomes traduzidos. "observacao" e
      # "idevento" ja vinham completos via cp_bloco (que so remove
      # "processo"); aqui so alinha ordenacao/rotulos com o que se ve na tela.
      df_eventos <- cp_bloco(micro_eventos, p)
      if (!is.null(df_eventos)) {
        if ("data" %in% names(df_eventos)) df_eventos <- df_eventos[order(df_eventos$data, decreasing = TRUE), , drop = FALSE]
        df_eventos <- df_eventos[, setdiff(names(df_eventos), "papel"), drop = FALSE]
        nm <- names(df_eventos)
        nm[nm == "data"]        <- "Data"
        nm[nm == "evento"]      <- "Evento"
        nm[nm == "observacao"]  <- "Observação"
        nm[nm == "publicacao"]  <- "Publicação (DOU)"
        names(df_eventos) <- nm
      }

      abas <- list(
        Fases             = if (!is.null(h))  construir_tabela_fases_export(h)       else data.frame(),
        Penalidades       = if (!is.null(hp)) construir_tabela_penalidades_export(hp) else data.frame(),
        Substancias       = cp_bloco(micro_substancias,  p),
        Titulos           = cp_bloco(micro_titulos,      p),
        Pessoas           = cp_bloco(micro_pessoas,      p),
        Municipios        = cp_bloco(micro_municipios,   p),
        PropriedadeSolo   = cp_bloco(micro_propsolo,     p),
        HistoricoEventos  = df_eventos,
        Associados        = cp_bloco(micro_associacoes,  p),
        Documentacao      = cp_bloco(micro_documentacao, p)
      )
      abas <- lapply(abas, function(d) if (is.null(d)) data.frame() else d)
      writexl::write_xlsx(abas, path = file)
    }
  )

  output$cp_dossie_cabecalho <- renderUI({
    p <- cp_proc_sel()
    if (is.null(p)) return(tags$div(class = "summary-box", "Selecione um processo."))
    ph <- micro_processos[micro_processos$processo == p, , drop = FALSE]
    if (nrow(ph) == 0) return(tags$div("Processo não encontrado nos microdados."))
    ph <- ph[1, ]
    linha <- function(rot, val) if (!is.null(val) && !is.na(val) && val != "")
      tags$div(tags$strong(paste0(rot, ": ")), val) else NULL
    tags$div(
      tags$h3(paste0("Processo ", p)),
      tags$div(style = "font-size:13px; color:#2C3E50; margin-bottom:14px;",
        linha("NUP", ph$nup),
        linha("Ativo", ph$ativo),
        linha("Tipo requerimento", ph$tipo_requerimento),
        linha("Fase", ph$fase),
        linha("Área (ha)", if (!is.na(ph$area_ha)) format(round(ph$area_ha, 2), big.mark = ".", decimal.mark = ",") else NA),
        linha("UF", if ("abbrev_state" %in% names(ph)) ph$abbrev_state else if ("uf" %in% names(ph)) ph$uf else NA),
        linha("Município(s)", if ("municipios" %in% names(ph)) ph$municipios else NA),
        linha("Protocolo", as.character(ph$dt_protocolo)),
        linha("Prioridade", as.character(ph$dt_prioridade))
      )
    )
  })

  cp_bloco <- function(tbl, p, drop = "processo") {
    if (is.null(tbl)) return(NULL)
    d <- tbl[tbl$processo == p, , drop = FALSE]
    d[, setdiff(names(d), drop), drop = FALSE]
  }

  output$cp_dossie_corpo <- renderUI({
    p <- cp_proc_sel(); if (is.null(p)) return(NULL)
    sec <- function(titulo, id) tagList(tags$h4(titulo), DTOutput(id, height = "auto"))
    tagList(
      sec("Substâncias", "cp_sub"),
      sec("Títulos", "cp_tit"),
      sec("Pessoas relacionadas", "cp_pes"),
      sec("Municípios", "cp_mun_bloco"),
      sec("Propriedade do solo", "cp_solo"),
      sec("Histórico de eventos", "cp_eventos"),
      sec("Processos associados", "cp_assoc"),
      sec("Documentação", "cp_doc")
    )
  })

  .cp_dt <- function(df) {
    if (is.null(df)) df <- data.frame("Aviso" = character(0))
    DT::datatable(df, rownames = FALSE, class = "compact",
      options = list(
        scrollX = TRUE, dom = "tp", pageLength = 5,
        language = list(
          zeroRecords = "Nenhum registro encontrado.",
          emptyTable = "Nenhum registro encontrado.",
          infoEmpty = ""
        ),
        columnDefs = list(list(targets = "_all", className = "dt-left"))
      ),
      selection = "none")
  }

  output$cp_sub       <- renderDT(.cp_dt(cp_bloco(micro_substancias,  cp_proc_sel())), server = TRUE)
  output$cp_pes       <- renderDT(.cp_dt(cp_bloco(micro_pessoas,      cp_proc_sel())), server = TRUE)
  output$cp_tit       <- renderDT(.cp_dt(cp_bloco(micro_titulos,      cp_proc_sel())), server = TRUE)
  output$cp_mun_bloco <- renderDT(.cp_dt(cp_bloco(micro_municipios,   cp_proc_sel())), server = TRUE)
  output$cp_solo      <- renderDT(.cp_dt(cp_bloco(micro_propsolo,     cp_proc_sel())), server = TRUE)
  output$cp_assoc     <- renderDT(.cp_dt(cp_bloco(micro_associacoes,  cp_proc_sel())), server = TRUE)
  output$cp_doc       <- renderDT(.cp_dt(cp_bloco(micro_documentacao, cp_proc_sel())), server = TRUE)

  output$cp_eventos <- renderDT({
    p <- cp_proc_sel()
    df <- cp_bloco(micro_eventos, p)
    
    if (is.null(df)) return(DT::datatable(data.frame(Aviso = "Dados indisponíveis.")))
    if ("data" %in% names(df)) df <- df[order(df$data, decreasing = TRUE), , drop = FALSE]

    # Cores por papel (eventos decisivos na logica de aptidao; NAO_CLASSIFICADO
    # = evento existe no historico bruto mas nao entra na maquina de estado)
    papel_cor <- c(MUDA_FASE = "#D4EDDA", FECHA = "#F8D7DA", SUSPENDE = "#FFF3CD", RETOMA = "#D1ECF1", NAO_CLASSIFICADO = "#F5F5F5")
    papel_tx  <- c(MUDA_FASE = "#155724", FECHA = "#721C24", SUSPENDE = "#856404", RETOMA = "#0C5460", NAO_CLASSIFICADO = "#6C757D")

    # Remove coluna papel antes de exibir (usada só para estilo)
    papel_col <- if ("papel" %in% names(df)) df$papel else rep(NA_character_, nrow(df))
    df_show <- df[, setdiff(names(df), "papel"), drop = FALSE]

    nm <- names(df_show); cn <- nm
    cn[nm == "data"] <- "Data"; cn[nm == "evento"] <- "Evento"
    cn[nm == "observacao"] <- "Observação"; cn[nm == "publicacao"] <- "Publicação (DOU)"

    # Coluna auxiliar oculta com o papel, usada para colorir a linha inteira
    df_show$.papel <- papel_col
    papel_idx <- ncol(df_show) - 1  # índice 0-based da coluna .papel

    dt <- DT::datatable(df_show, rownames = FALSE, class = "compact", colnames = cn,
      extensions = "Scroller",
      options = list(
        scrollX = TRUE, dom = "tip", pageLength = 10, deferRender = TRUE,
        language = list(
          zeroRecords = "Nenhum evento registrado.",
          emptyTable = "Nenhum evento registrado."
        ),
        columnDefs = list(
          list(targets = papel_idx, visible = FALSE),
          list(targets = "_all", className = "dt-left"),
          list(targets = which(nm %in% c("observacao", "publicacao")) - 1,
               className = "dt-wrap")
        )
      )
    )

    # Colore a linha conforme o papel do evento (ABRE/FECHA/SUSPENDE/RETOMA)
    dt |>
      DT::formatStyle(
        ".papel", target = "row",
        backgroundColor = DT::styleEqual(names(papel_cor), unname(papel_cor)),
        color           = DT::styleEqual(names(papel_tx),  unname(papel_tx))
      )
  }, server = TRUE)

  output$cp_mapa <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Satélite") |>
      leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB") |>
      leaflet::addLayersControl(baseGroups = c("Satélite", "CartoDB"),
                                options = leaflet::layersControlOptions(collapsed = FALSE)) |>
      leaflet::setView(lng = -55, lat = -5, zoom = 4)
  })

  observeEvent(cp_proc_sel(), {
    proxy <- leaflet::leafletProxy("cp_mapa")
    proxy |> leaflet::clearGroup("proc")
    p <- cp_proc_sel()
    if (is.null(p)) return()
    geo <- NULL
    if (exists("pma_simpl") && "PROCESSO" %in% names(pma_simpl)) {
      g <- pma_simpl[pma_simpl$PROCESSO == p, , drop = FALSE]
      g <- g |> dplyr::distinct(PROCESSO, .keep_all = TRUE)
      if (nrow(g) > 0) geo <- sanear_geo_para_leaflet(sf::st_transform(g, 4326))
    }
    if (is.null(geo) || nrow(geo) == 0) return()
    bb <- sf::st_bbox(geo)
    tryCatch({
      proxy |>
        leaflet::addPolygons(data = geo, group = "proc",
                             color = "#FF3D00", weight = 2, opacity = 1,
                             fillOpacity = 0.35, smoothFactor = 0.2) |>
        leaflet::fitBounds(lng1 = as.numeric(bb["xmin"]), lat1 = as.numeric(bb["ymin"]),
                           lng2 = as.numeric(bb["xmax"]), lat2 = as.numeric(bb["ymax"]))
    }, error = function(e) {
      message("[cp_mapa] falha ao desenhar geometria do processo ", p, ": ", conditionMessage(e))
    })
  })

  # ==========================================================================
  # ABA 1 — Tabela
  # ==========================================================================
  sync_pair(session, "subs_tab1", "subs_det_tab1", map_subs, "SUBSarrSIM", "SUBSarr")

  observeEvent(list(input$subs_tab1, input$subs_det_tab1, input$ufs_tab1, input$fases_tab1, input$periodo_tab1), {
    df_temp <- lk_mun |>
      dplyr::filter(ANO >= input$periodo_tab1[1], ANO <= input$periodo_tab1[2],
                    FASE %in% input$fases_tab1, abbrev_state %in% input$ufs_tab1)
    if (length(input$subs_det_tab1)) {
      df_temp <- df_temp |> dplyr::filter(SUBSarr %in% input$subs_det_tab1)
    } else {
      df_temp <- df_temp |> dplyr::filter(SUBSarrSIM %in% input$subs_tab1)
    }
    muns_ok <- sort(unique(df_temp$name_muni))
    updatePickerInput(session, "muns_tab1", choices = muns_ok,
                      selected = intersect(isolate(input$muns_tab1), muns_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  observeEvent(list(input$muns_tab1, input$ufs_tab1), {
    df_temp <- lk_tit_proc |> dplyr::filter(abbrev_state %in% input$ufs_tab1, name_muni %in% input$muns_tab1)
    tits_ok  <- sort(unique(df_temp$TITULAR))
    procs_ok <- sort(unique(df_temp$PROCESSO))
    updatePickerInput(session, "tits_tab1", choices = tits_ok,
                      selected = intersect(isolate(input$tits_tab1), tits_ok))
    updatePickerInput(session, "procs_tab1", choices = procs_ok,
                      selected = intersect(isolate(input$procs_tab1), procs_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  observeEvent(list(input$procs_tab1, input$tits_tab1), {
    df_temp <- lk_decl |>
      dplyr::filter(abbrev_state %in% input$ufs_tab1, name_muni %in% input$muns_tab1,
                    TITULAR %in% input$tits_tab1, PROCESSO %in% input$procs_tab1)
    decl_ok <- sort(unique(df_temp$NOME_arr))
    updatePickerInput(session, "decl_tab1", choices = decl_ok,
                      selected = intersect(isolate(input$decl_tab1), decl_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  dados_filtrados <- reactive({
    showNotification("Filtrando dados...", duration = 1, type = "default")
    df <- cfem |>
      dplyr::filter(ANO >= input$periodo_tab1[1], ANO <= input$periodo_tab1[2],
                    FASE %in% input$fases_tab1, abbrev_state %in% input$ufs_tab1)
    if (length(input$subs_det_tab1)) {
      df <- df |> dplyr::filter(SUBSarr %in% input$subs_det_tab1)
    } else {
      df <- df |> dplyr::filter(SUBSarrSIM %in% input$subs_tab1)
    }
    if (length(input$muns_tab1)) df  <- df |> dplyr::filter(name_muni %in% input$muns_tab1)
    if (length(input$procs_tab1)) df <- df |> dplyr::filter(PROCESSO %in% input$procs_tab1)
    if (length(input$tits_tab1)) df  <- df |> dplyr::filter(TITULAR %in% input$tits_tab1)
    if (length(input$decl_tab1)) df  <- df |> dplyr::filter(NOME_arr %in% input$decl_tab1)
    df <- filtra_sobrepos(df, flags = input$ov_flags_tab1)
    df
  }) |> bindCache(
    input$periodo_tab1, input$subs_tab1, input$subs_det_tab1, input$ufs_tab1, input$muns_tab1, input$fases_tab1,
    input$procs_tab1, input$tits_tab1, input$decl_tab1, input$ov_flags_tab1
  ) |> debounce(250)

  observeEvent(input$reset_tab1, {
    updatePickerInput(session, "subs_tab1", choices = subs_all_grupo, selected = subs_all_grupo)
    updateSliderInput(session, "periodo_tab1", value = c(min(anos_all), max(anos_all)))
    updatePickerInput(session, "ufs_tab1", choices = ufs_all, selected = ufs_all)
    updatePickerInput(session, "fases_tab1", choices = fases_all, selected = fases_all)
    updateCheckboxGroupButtons(session, "ov_flags_tab1", selected = c())
    updatePickerInput(session, "subs_det_tab1", choices = subs_all_original, selected = subs_all_original)
    updatePickerInput(session, "muns_tab1", choices = muns_all, selected = muns_all)
    updatePickerInput(session, "tits_tab1", choices = tits_all, selected = tits_all)
    updatePickerInput(session, "procs_tab1", choices = procs_all, selected = character(0))
    updatePickerInput(session, "decl_tab1", choices = decl_all, selected = decl_all)
  })

  output$tabela_dt <- renderDT({
    df <- dados_filtrados()
    validate(need(nrow(df) > 0, "Nenhum dado encontrado com os filtros aplicados."))
    cols_keep  <- intersect(cols_visible, names(df))
    df_display <- df[, cols_keep, drop = FALSE]
    names(df_display) <- unname(cols_labels[cols_keep])
    if ("Peso corrigido?" %in% names(df_display)) {
      df_display[["Peso corrigido?"]] <- tolower(as.character(df_display[["Peso corrigido?"]]))
    } else {
      df_display[["Peso corrigido?"]] <- NA_character_
    }
    num_cols <- intersect(
      c("Peso orig (g)", "Peso orig (Kg)", "Peso final (g)", "Peso final (kg)", "Valor Recolhido (R$)", "Valor Total (R$)"),
      names(df_display))
    totals_raw <- vapply(num_cols, function(nm) sum(df_display[[nm]], na.rm = TRUE), numeric(1))
    fmt_num <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    fmt_cur <- function(x) paste0("R$ ", format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2))
    totals_fmt <- setNames(
      ifelse(grepl("\\(R\\$\\)", names(totals_raw)), fmt_cur(totals_raw), fmt_num(totals_raw)),
      names(totals_raw))
    first_label <- sprintf("TOTAL (linhas: %s)", nrow(df_display))
    sketch <- htmltools::withTags(table(
      thead(
        tr(lapply(seq_along(df_display), function(i) {
          nm <- names(df_display)[i]
          val <- if (i == 1) first_label else if (nm %in% names(totals_fmt)) totals_fmt[[nm]] else ""
          th(style = "background:#F8F9FA;font-weight:700;", val)
        })),
        tr(lapply(names(df_display), th))
      )
    ))
    wrap_cols <- c("Titular", "Parte declarante", "UC", "TI")
    wrap_idx  <- which(names(df_display) %in% wrap_cols) - 1
    dt_obj <- datatable(
      df_display, container = sketch, extensions = c("Scroller"),
      rownames = FALSE, class = "compact",
      options = list(
        scrollX = TRUE, dom = 'ftip', pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'Tudo')),
        columnDefs = list(
          list(targets = "_all", className = "dt-left"),
          list(targets = wrap_idx, className = "dt-wrap"),
          list(targets = wrap_idx, width = "260px")),
        autoWidth = TRUE, deferRender = TRUE)
    ) |>
      formatCurrency("Valor Recolhido (R$)", currency = "R$ ", digits = 2) |>
      formatCurrency("Valor Total (R$)", currency = "R$ ", digits = 2) |>
      formatRound("Peso orig (g)", digits = 2) |> formatRound("Peso orig (Kg)", digits = 2) |>
      formatRound("Peso final (g)", digits = 2) |> formatRound("Peso final (kg)", digits = 2) |>
      formatRound("R$/g (orig)", digits = 1) |> formatRound("R$/g (final)", digits = 1)
    lv <- setdiff(unique(df_display[["Peso corrigido?"]]), "original")
    dt_obj |> formatStyle(columns = names(df_display), valueColumns = "Peso corrigido?",
      backgroundColor = styleEqual(lv, rep("rgba(255,250,205,0.9)", length(lv))))
  }, server = TRUE)

  output$relatorio_tab1 <- renderText({ relatorio_selecao(dados_filtrados(), mensal = TRUE) })

  proxy_tabela <- DT::dataTableProxy("tabela_dt")
  observeEvent(dados_filtrados(), { DT::reloadData(proxy_tabela, resetPaging = TRUE) }, ignoreInit = TRUE)

  prep_export <- function() {
    df <- dados_filtrados()
    cols_keep <- intersect(cols_visible, names(df))
    df_export <- df[, cols_keep, drop = FALSE]
    names(df_export) <- unname(cols_labels[cols_keep])
    df_export
  }
  prep_export_tab2 <- function() {
    df <- dados_selecionados_sankey()
    validate(need(nrow(df) > 0, "Nenhum dado para exportar (aba 2)."))
    cols_keep <- intersect(cols_visible, names(df))
    df_export <- df[, cols_keep, drop = FALSE]
    names(df_export) <- unname(cols_labels[cols_keep])
    df_export
  }
  prep_export_tab3 <- function() {
    df <- dados_mensal()
    validate(need(nrow(df) > 0, "Nenhum dado para exportar (aba 3)."))
    cols_keep <- intersect(cols_visible, names(df))
    df_export <- df[, cols_keep, drop = FALSE]
    names(df_export) <- unname(cols_labels[cols_keep])
    df_export
  }

  output$baixar_csv  <- downloadHandler(filename = function() paste0("cfem_filtrado_", Sys.Date(), ".csv"),
                                        content = function(file) readr::write_csv(prep_export(), file))
  output$baixar_xlsx <- downloadHandler(filename = function() paste0("cfem_filtrado_", Sys.Date(), ".xlsx"),
                                        content = function(file) write_xlsx(prep_export(), path = file))
  output$baixar_csv_tab2  <- downloadHandler(filename = function() paste0("cfem_anual_filtrado_", Sys.Date(), ".csv"),
                                             content = function(file) readr::write_csv(prep_export_tab2(), file))
  output$baixar_xlsx_tab2 <- downloadHandler(filename = function() paste0("cfem_anual_filtrado_", Sys.Date(), ".xlsx"),
                                             content = function(file) writexl::write_xlsx(prep_export_tab2(), path = file))
  output$baixar_csv_tab3  <- downloadHandler(filename = function() paste0("cfem_mensal_filtrado_", Sys.Date(), ".csv"),
                                             content = function(file) readr::write_csv(prep_export_tab3(), file))
  output$baixar_xlsx_tab3 <- downloadHandler(filename = function() paste0("cfem_mensal_filtrado_", Sys.Date(), ".xlsx"),
                                             content = function(file) writexl::write_xlsx(prep_export_tab3(), path = file))

  procs_sel_tab1 <- reactive({ unique(dados_filtrados()$PROCESSO) }) |> bindCache(dados_filtrados()$PROCESSO)
  tits_sel_tab1  <- reactive({ unique(dados_filtrados()$TITULAR) }) |> bindCache(dados_filtrados()$TITULAR)
  decl_sel_tab1  <- reactive({ unique(dados_filtrados()$NOME_arr) }) |> bindCache(dados_filtrados()$NOME_arr)

  pma_sel_tab1 <- reactive({
    procs <- procs_sel_tab1()
    src <- pma_simpl
    if (!length(procs)) return(src[0, ])
    dplyr::filter(src, PROCESSO %in% procs)
  }) |> bindCache(procs_sel_tab1())

  pma_titular_tab1 <- reactive({
    tits  <- tits_sel_tab1()
    procs <- procs_sel_tab1()
    src <- pma_simpl
    if (!length(tits)) return(src[0, ])
    dplyr::filter(src, TITULAR %in% tits, !(PROCESSO %in% procs))
  }) |> bindCache(tits_sel_tab1(), procs_sel_tab1())

  pma_declarante_tab1 <- reactive({
    declarantes <- decl_sel_tab1()
    if (!length(declarantes)) { src <- pma_simpl; return(src[0, ]) }
    procs_declarantes <- cfem |> dplyr::filter(NOME_arr %in% declarantes) |> dplyr::pull(PROCESSO) |> unique()
    src <- pma_simpl
    dplyr::filter(src, PROCESSO %in% procs_declarantes)
  }) |> bindCache(decl_sel_tab1())

  output$baixar_pma_sel_tab1 <- downloadHandler(
    filename = function() paste0("pmas_selecao_tab1_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_sel_tab1(), "pmas_selecao_tab1", temp_dir), file, overwrite = TRUE)
    })
  output$baixar_pma_titular_tab1 <- downloadHandler(
    filename = function() paste0("pmas_titular_tab1_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_titular_tab1(), "pmas_titular_tab1", temp_dir), file, overwrite = TRUE)
    })
  output$baixar_pma_declarante_tab1 <- downloadHandler(
    filename = function() paste0("pmas_declarante_tab1_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_declarante_tab1(), "pmas_declarante_tab1", temp_dir), file, overwrite = TRUE)
    })

  # ==========================================================================
  # ABA 2 — Fluxo Sankey
  # ==========================================================================
  sync_pair(session, "subs_tab2", "subs_det_tab2", map_subs, "SUBSarrSIM", "SUBSarr")

  observeEvent(list(input$subs_tab2, input$subs_det_tab2, input$ufs_tab2, input$fases_tab2, input$periodo_tab2), {
    df_temp <- lk_mun_tab2 |>
      dplyr::filter(ANO >= input$periodo_tab2[1], ANO <= input$periodo_tab2[2],
                    FASE %in% input$fases_tab2, abbrev_state %in% input$ufs_tab2)
    if (length(input$subs_det_tab2)) {
      df_temp <- df_temp |> dplyr::filter(SUBSarr %in% input$subs_det_tab2)
    } else {
      df_temp <- df_temp |> dplyr::filter(SUBSarrSIM %in% input$subs_tab2)
    }
    muns_ok <- sort(unique(df_temp$name_muni))
    updatePickerInput(session, "muns_tab2", choices = muns_ok,
                      selected = intersect(isolate(input$muns_tab2), muns_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  observeEvent(list(input$muns_tab2, input$tits_tab2, input$ufs_tab2), {
    df_temp <- lk_tit_proc_tab2 |> dplyr::filter(abbrev_state %in% input$ufs_tab2, name_muni %in% input$muns_tab2)
    tits_ok <- sort(unique(df_temp$TITULAR)); procs_ok <- sort(unique(df_temp$PROCESSO))
    updatePickerInput(session, "tits_tab2", choices = tits_ok,
                      selected = intersect(isolate(input$tits_tab2), tits_ok))
    updatePickerInput(session, "procs_tab2", choices = procs_ok,
                      selected = intersect(isolate(input$procs_tab2), procs_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  observeEvent(list(input$procs_tab2, input$tits_tab2), {
    df_temp <- lk_decl_tab2 |> dplyr::filter(abbrev_state %in% input$ufs_tab2, name_muni %in% input$muns_tab2,
                                             TITULAR %in% input$tits_tab2, PROCESSO %in% input$procs_tab2)
    decl_ok <- sort(unique(df_temp$NOME_arr))
    updatePickerInput(session, "decl_tab2", choices = decl_ok,
                      selected = intersect(isolate(input$decl_tab2), decl_ok))
    rm(df_temp)
  }, ignoreInit = TRUE)

  dados_selecionados_sankey <- reactive({
    showNotification("Atualizando Sankey...", duration = 1, type = "default")
    df <- cfem_anual |>
      dplyr::filter(ANO >= input$periodo_tab2[1], ANO <= input$periodo_tab2[2],
                    FASE %in% input$fases_tab2, abbrev_state %in% input$ufs_tab2)
    if (length(input$subs_det_tab2)) {
      df <- df |> dplyr::filter(SUBSarr %in% input$subs_det_tab2)
    } else {
      df <- df |> dplyr::filter(SUBSarrSIM %in% input$subs_tab2)
    }
    if (length(input$muns_tab2)) df <- df |> dplyr::filter(name_muni %in% input$muns_tab2)
    if (length(input$tits_tab2)) df <- df |> dplyr::filter(TITULAR %in% input$tits_tab2)
    if (length(input$procs_tab2)) df <- df |> dplyr::filter(PROCESSO %in% input$procs_tab2)
    if (length(input$decl_tab2)) df <- df |> dplyr::filter(NOME_arr %in% input$decl_tab2)
    df <- filtra_sobrepos(df, flags = input$ov_flags_tab2)
    df
  }) |> bindCache(
    input$periodo_tab2, input$ufs_tab2, input$fases_tab2, input$subs_tab2, input$subs_det_tab2,
    input$muns_tab2, input$tits_tab2, input$procs_tab2, input$decl_tab2, input$ov_flags_tab2
  ) |> debounce(250)

  collapse_level <- function(df, col, top_n, label_outros) {
    tot <- df |> dplyr::group_by(.data[[col]]) |>
      dplyr::summarise(total = sum(valor_usado, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(total))
    keep <- head(tot[[col]], top_n)
    df[[col]] <- ifelse(df[[col]] %in% keep, df[[col]], label_outros)
    df
  }

  output$sankeyPlot <- renderSankeyNetwork({
    dados <- dados_selecionados_sankey()
    if (nrow(dados) == 0) { showNotification("Nenhum fluxo encontrado com os filtros aplicados.", type = "warning"); return(NULL) }
    dados <- dados |>
      dplyr::group_by(abbrev_state, name_muni, TITULAR, PROCESSO, NOME_arr) |>
      dplyr::summarise(valor_usado = sum(.data[[input$variavel_fluxo_tab2]], na.rm = TRUE), .groups = "drop") |>
      dplyr::filter(!is.na(NOME_arr), valor_usado > 0)
    top_n <- req(input$max_nodes_sankey)
    dados <- dados |>
      collapse_level("abbrev_state", top_n, "Outros — UFs") |>
      collapse_level("name_muni", top_n, "Outros — Municípios") |>
      collapse_level("TITULAR", top_n, "Outros — Titulares") |>
      collapse_level("PROCESSO", top_n, "Outros — Processos") |>
      collapse_level("NOME_arr", top_n, "Outros — Partes")
    dados2 <- dados |>
      mutate(UF = paste0(abbrev_state), MUN = paste0(name_muni), TIT = paste0(TITULAR),
             PROC = paste0(PROCESSO), DEC = paste0(NOME_arr))
    nodes <- data.frame(name = c(
      sort(unique(dados2$UF)), sort(unique(dados2$MUN)), sort(unique(dados2$TIT)),
      sort(unique(dados2$PROC)), sort(unique(dados2$DEC))))
    criar_links <- function(df, a, b) df |>
      dplyr::group_by(.data[[a]], .data[[b]]) |>
      dplyr::summarise(value = sum(valor_usado, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(source = match(.data[[a]], nodes$name) - 1, target = match(.data[[b]], nodes$name) - 1)
    links <- dplyr::bind_rows(
      criar_links(dados2, "UF", "MUN"), criar_links(dados2, "MUN", "TIT"),
      criar_links(dados2, "TIT", "PROC"), criar_links(dados2, "PROC", "DEC"))
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", Value = "value",
      NodeID = "name", fontSize = 14, nodeWidth = 50, nodePadding = 50, sinksRight = FALSE)
  })

  observeEvent(input$reset_tab2, {
    updateRadioButtons(session, "variavel_fluxo_tab2", selected = "VALORarr")
    updateNumericInput(session, "max_nodes_sankey", value = 10)
    updatePickerInput(session, "subs_tab2", choices = subs_all_grupo, selected = subs_all_grupo)
    updatePickerInput(session, "fases_tab2", choices = fases_all, selected = fases_all)
    updateCheckboxGroupButtons(session, "ov_flags_tab2", selected = c())
    updateSliderInput(session, "periodo_tab2", value = c(min(anos_all), max(anos_all)))
    updatePickerInput(session, "subs_det_tab2", choices = subs_all_original, selected = subs_all_original)
    updatePickerInput(session, "ufs_tab2", choices = ufs_all, selected = ufs_all)
    updatePickerInput(session, "muns_tab2", choices = muns_all, selected = character(0))
    updatePickerInput(session, "tits_tab2", choices = tits_all, selected = character(0))
    updatePickerInput(session, "procs_tab2", choices = procs_all, selected = character(0))
    updatePickerInput(session, "decl_tab2", choices = decl_all, selected = character(0))
  })

  procs_sel_tab2 <- reactive({ unique(dados_selecionados_sankey()$PROCESSO) }) |> bindCache(dados_selecionados_sankey()$PROCESSO)
  tits_sel_tab2  <- reactive({ unique(dados_selecionados_sankey()$TITULAR) })  |> bindCache(dados_selecionados_sankey()$TITULAR)
  decl_sel_tab2  <- reactive({ unique(dados_selecionados_sankey()$NOME_arr) }) |> bindCache(dados_selecionados_sankey()$NOME_arr)

  pma_sel_tab2 <- reactive({
    procs <- procs_sel_tab2()
    src <- pma_simpl
    if (!length(procs)) return(src[0, ])
    dplyr::filter(src, PROCESSO %in% procs)
  }) |> bindCache(procs_sel_tab2())

  pma_titular_tab2 <- reactive({
    tits  <- tits_sel_tab2()
    procs <- procs_sel_tab2()
    src <- pma_simpl
    if (!length(tits)) return(src[0, ])
    dplyr::filter(src, TITULAR %in% tits, !(PROCESSO %in% procs))
  }) |> bindCache(tits_sel_tab2(), procs_sel_tab2())

  pma_declarante_tab2 <- reactive({
    declarantes <- decl_sel_tab2()
    if (!length(declarantes)) { src <- pma_simpl; return(src[0, ]) }
    procs_declarantes <- cfem |> dplyr::filter(NOME_arr %in% declarantes) |> dplyr::pull(PROCESSO) |> unique()
    src <- pma_simpl
    dplyr::filter(src, PROCESSO %in% procs_declarantes)
  }) |> bindCache(decl_sel_tab2())

  output$baixar_pma_sel_tab2 <- downloadHandler(
    filename = function() paste0("pmas_selecao_tab2_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_sel_tab2(), "pmas_selecao_tab2", temp_dir), file, overwrite = TRUE)
    })
  output$baixar_pma_titular_tab2 <- downloadHandler(
    filename = function() paste0("pmas_titular_tab2_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_titular_tab2(), "pmas_titular_tab2", temp_dir), file, overwrite = TRUE)
    })
  output$baixar_pma_declarante_tab2 <- downloadHandler(
    filename = function() paste0("pmas_declarante_tab2_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_declarante_tab2(), "pmas_declarante_tab2", temp_dir), file, overwrite = TRUE)
    })

  output$relatorio_tab2 <- renderText({
    base <- relatorio_selecao(dados_selecionados_sankey(), mensal = FALSE)
    metrica <- if (input$variavel_fluxo_tab2 == "VALORarr") "Valor Recolhido (R$)" else "Quantidade (Kg líquido)"
    paste0(base, "\n\nMétrica no Sankey: ", metrica)
  })

  # ==========================================================================
  # ABA 3 — Série Temporal e Mapa
  # ==========================================================================
  sync_pair(session, "subs_tab3", "subs_det_tab3", map_subs, "SUBSarrSIM", "SUBSarr")

  observeEvent(list(input$subs_tab3, input$subs_det_tab3, input$ufs_tab3, input$fases_tab3, input$periodo_tab3, input$meses_tab3), {
    df_temp <- cfem_mensal |>
      dplyr::filter(ANO >= input$periodo_tab3[1], ANO <= input$periodo_tab3[2],
                    FASE %in% input$fases_tab3, MES >= input$meses_tab3[1], MES <= input$meses_tab3[2],
                    abbrev_state %in% input$ufs_tab3)
    if (length(input$subs_det_tab3)) {
      df_temp <- df_temp |> dplyr::filter(SUBSarr %in% input$subs_det_tab3)
    } else {
      df_temp <- df_temp |> dplyr::filter(SUBSarrSIM %in% input$subs_tab3)
    }
    updatePickerInput(session, "muns_tab3", choices = sort(unique(df_temp$name_muni)),
                      selected = intersect(isolate(input$muns_tab3), sort(unique(df_temp$name_muni))))
    rm(df_temp); gc()
  }, ignoreInit = FALSE)

  observeEvent(list(input$muns_tab3, input$ufs_tab3), {
    df_temp <- cfem_mensal |> filter_in("abbrev_state", input$ufs_tab3) |> filter_in("name_muni", input$muns_tab3)
    updatePickerInput(session, "tits_tab3", choices = sort(unique(df_temp$TITULAR)),
                      selected = intersect(isolate(input$tits_tab3), sort(unique(df_temp$TITULAR))))
    updatePickerInput(session, "procs_tab3", choices = sort(unique(df_temp$PROCESSO)),
                      selected = intersect(isolate(input$procs_tab3), sort(unique(df_temp$PROCESSO))))
    rm(df_temp); gc()
  }, ignoreInit = FALSE)

  observeEvent(list(input$procs_tab3, input$tits_tab3, input$ufs_tab3, input$muns_tab3), {
    df_temp <- cfem_mensal |>
      filter_in("abbrev_state", input$ufs_tab3) |> filter_in("name_muni", input$muns_tab3) |>
      filter_in("TITULAR", input$tits_tab3) |> filter_in("PROCESSO", input$procs_tab3)
    updatePickerInput(session, "decl_tab3", choices = sort(unique(df_temp$NOME_arr)),
                      selected = intersect(isolate(input$decl_tab3), sort(unique(df_temp$NOME_arr))))
    rm(df_temp); gc()
  }, ignoreInit = FALSE)

  dados_mensal <- reactive({
    df <- cfem_mensal |>
      dplyr::filter(ANO >= input$periodo_tab3[1], ANO <= input$periodo_tab3[2],
                    FASE %in% input$fases_tab3, MES >= input$meses_tab3[1], MES <= input$meses_tab3[2],
                    abbrev_state %in% input$ufs_tab3)
    if (length(input$subs_det_tab3)) {
      df <- df |> dplyr::filter(SUBSarr %in% input$subs_det_tab3)
    } else {
      df <- df |> dplyr::filter(SUBSarrSIM %in% input$subs_tab3)
    }
    if (length(input$muns_tab3)) df <- df |> dplyr::filter(name_muni %in% input$muns_tab3)
    if (length(input$tits_tab3)) df <- df |> dplyr::filter(TITULAR %in% input$tits_tab3)
    if (length(input$procs_tab3)) df <- df |> dplyr::filter(PROCESSO %in% input$procs_tab3)
    if (length(input$decl_tab3)) df <- df |> dplyr::filter(NOME_arr %in% input$decl_tab3)
    df <- filtra_sobrepos(df, flags = input$ov_flags_tab3)
    df
  }) |> bindCache(input$periodo_tab3, input$meses_tab3, input$fases_tab3, input$ufs_tab3,
                  input$subs_tab3, input$subs_det_tab3, input$muns_tab3, input$tits_tab3, input$procs_tab3,
                  input$decl_tab3, input$ov_flags_tab3) |> debounce(450)

  observeEvent(input$reset_tab3, {
    updateRadioButtons(session, "variavel_fluxo_tab3", selected = "VALORarr")
    updateSelectInput(session, "agrupamento_tab3", selected = "geral")
    updatePickerInput(session, "subs_tab3", choices = subs_all_grupo, selected = subs_all_grupo)
    updatePickerInput(session, "fases_tab3", choices = fases_all, selected = fases_all)
    updateCheckboxGroupButtons(session, "ov_flags_tab3", selected = c())
    updateSliderInput(session, "periodo_tab3", value = c(min(anos_all), max(anos_all)))
    updateSliderInput(session, "meses_tab3", value = c(1, 12))
    updatePickerInput(session, "subs_det_tab3", choices = subs_all_original, selected = subs_all_original)
    updatePickerInput(session, "ufs_tab3", choices = ufs_all, selected = ufs_all)
    updatePickerInput(session, "muns_tab3", choices = muns_all, selected = muns_all)
    updatePickerInput(session, "tits_tab3", choices = tits_all, selected = tits_all)
    updatePickerInput(session, "procs_tab3", choices = procs_all, selected = procs_all)
    updatePickerInput(session, "decl_tab3", choices = decl_all, selected = decl_all)
  })

  output$serie_temporal <- renderPlotly({
    df <- dados_mensal(); req(nrow(df) > 0)
    variavel <- input$variavel_fluxo_tab3; agrup <- input$agrupamento_tab3
    if (agrup == "geral") {
      df_plot <- df |> group_by(data) |> summarise(valor = sum(.data[[variavel]], na.rm = TRUE), .groups = "drop")
      p <- ggplot(df_plot, aes(x = data, y = valor)) +
        geom_line(color = "tomato", linewidth = 1) + geom_point(color = "tomato", size = 1.5) +
        scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
        scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
        labs(y = ifelse(variavel == "VALORarr", "Valor arrecadado (R$)", "Peso declarado (Kg)"), x = "Data") +
        theme_minimal(base_size = 13) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
      ggplotly(p, tooltip = c("x", "y")) |> config(displayModeBar = FALSE)
    } else {
      df_plot <- df |> group_by(data, grupo = .data[[agrup]]) |> summarise(valor = sum(.data[[variavel]], na.rm = TRUE), .groups = "drop")
      p <- ggplot(df_plot, aes(x = data, y = valor, group = grupo, color = grupo,
                               text = paste0("<b>", grupo, "</b><br>", "Data: ", format(data, "%b/%Y"), "<br>", "valor:", comma(valor)))) +
        geom_line(linewidth = 1) + geom_point(size = 1.5) +
        scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
        scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
        labs(y = ifelse(variavel == "VALORarr", "Valor arrecadado (R$)", "Peso declarado (Kg)"), x = "Data") +
        theme_minimal(base_size = 13) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), legend.position = "none")
      ggplotly(p, tooltip = "text") |> config(displayModeBar = FALSE)
    }
  })

  output$grafico_outliers <- renderPlotly({
    df <- dados_mensal(); req(nrow(df) > 0)
    variavel <- input$variavel_fluxo_tab3; agrup <- input$agrupamento_tab3
    df_plot <- df |>
      mutate(grupo = if (agrup == "geral") "geral" else .data[[agrup]], valor = as.numeric(.data[[variavel]])) |>
      group_by(data, grupo) |> summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    df_plot <- df_plot |>
      group_by(grupo) |>
      mutate(Q1 = quantile(valor, 0.25, na.rm = TRUE), Q3 = quantile(valor, 0.75, na.rm = TRUE),
             IQR = Q3 - Q1, sdv = sd(valor, na.rm = TRUE),
             limite_sup = ifelse(is.finite(IQR) & IQR > 0, Q3 + 1.5 * IQR,
                                 Q3 + 3 * ifelse(is.finite(sdv) & !is.na(sdv), sdv, 0)),
             outlier = valor > limite_sup) |> ungroup()
    message("Outliers totais: ", sum(df_plot$outlier, na.rm = TRUE))
    df_plot <- df_plot |> mutate(outlier = factor(outlier, levels = c(FALSE, TRUE), labels = c("Não", "Sim")))
    dummy_legend <- data.frame(data = as.Date(c(NA, NA)), valor = c(NA_real_, NA_real_),
                               outlier = factor(c("Não", "Sim"), levels = c("Não", "Sim")))
    p <- ggplot(df_plot, aes(x = data, y = valor)) +
      geom_line(aes(group = grupo), alpha = 0.2, color = "gray50", linewidth = 0.6) +
      geom_point(aes(color = outlier), size = 1.6) +
      geom_point(data = dummy_legend, aes(color = outlier), alpha = 0) +
      scale_color_manual(values = c("#212529", "tomato"), breaks = c("Não", "Sim"), drop = FALSE, name = "Outlier") +
      scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      labs(y = ifelse(variavel == "VALORarr", "Valor arrecadado por mês (R$)", "Peso declarado por mês (Kg)"), x = "Data") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", legend.title = element_text(size = 10), legend.text = element_text(size = 10),
            axis.text.x = element_text(angle = 70, hjust = 1, size = 9), axis.text.y = element_text(size = 10))
    ggplotly(p, tooltip = c("x", "y", "color")) |>
      layout(legend = list(orientation = "h", x = 0.1, y = -0.2)) |> config(displayModeBar = FALSE)
  })

  pma_src_tab3 <- reactive(pma_simpl)
  dados_mapa_cfem_tab3 <- reactive({ dados_mensal() })

  # NOTA (2026-07-20): pma_src_tab3() pode ter 1-3 linhas por processo (1 por
  # foco -- ver 05_integracao_final.R). distinct(PROCESSO) aqui e so pra nao
  # desenhar a mesma geometria sobreposta 2-3x no mapa; nenhum numero de foco
  # e usado nesses popups (so PROCESSO/SUBS/FASE/TITULAR).
  pma_filtrado_tab3 <- reactive({
    procs <- unique(dados_mapa_cfem_tab3()$PROCESSO)
    src <- pma_src_tab3()
    if (length(procs) == 0) return(src[0, ])
    src |> dplyr::filter(PROCESSO %in% procs) |> dplyr::distinct(PROCESSO, .keep_all = TRUE)
  }) |> bindCache(dados_mapa_cfem_tab3()$PROCESSO)

  pma_titular_tab3 <- reactive({
    procs_sel <- unique(dados_mapa_cfem_tab3()$PROCESSO)
    tits <- unique(dados_mapa_cfem_tab3()$TITULAR)
    src <- pma_src_tab3()
    if (length(tits) == 0) return(src[0, ])
    src |> dplyr::filter(TITULAR %in% tits, !(PROCESSO %in% procs_sel)) |> dplyr::distinct(PROCESSO, .keep_all = TRUE)
  }) |> bindCache(dados_mapa_cfem_tab3()$TITULAR, dados_mapa_cfem_tab3()$PROCESSO)

  pma_declarante_tab3 <- reactive({
    declarantes <- unique(dados_mapa_cfem_tab3()$NOME_arr)
    src <- pma_src_tab3()
    if (!length(declarantes)) return(src[0, ])
    processos_declarantes <- cfem |> dplyr::filter(NOME_arr %in% declarantes) |> dplyr::pull(PROCESSO) |> unique()
    src |> dplyr::filter(PROCESSO %in% processos_declarantes) |> dplyr::distinct(PROCESSO, .keep_all = TRUE)
  }) |> bindCache(dados_mapa_cfem_tab3()$NOME_arr)

  output$mapa_cfem_pma_tab3 <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2, maxZoom = 18, preferCanvas = TRUE)) |>
      leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB") |>
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Satélite") |>
      leaflet::addPolygons(data = uc, group = "Unidades de Conservação",
                           color = "#78c679", weight = 0.5, opacity = 0.8, fillOpacity = 0.5,
                           popup = ~paste0("<b>UC:</b> ", nome_uc)) |>
      leaflet::addPolygons(data = ti, group = "Terras Indígenas",
                           color = "#006837", weight = 0.5, opacity = 0.8, fillOpacity = 0.5,
                           popup = ~paste0("<b>TI:</b> ", terrai_nom,
                                           if ("fase_ti" %in% names(ti)) paste0("<br><b>Fase:</b> ", fase_ti) else "")) |>
      leaflet::addPolygons(data = qui, group = "Comunidades Quilombolas",
                           color = "#dfc27d", weight = 0.5, opacity = 0.85, fillOpacity = 0.45,
                           popup = ~paste0("<b>Comunidade:</b> ", nm_comunid,
                                           if ("fase" %in% names(qui)) paste0("<br><b>Fase:</b> ", fase) else "")) |>
      leaflet::addLayersControl(
        baseGroups = c("CartoDB", "Satélite"),
        overlayGroups = c("Processos Minerários", "PMAs do mesmo Titular", "PMAs da mesma Parte Declarante",
                          "Unidades de Conservação", "Terras Indígenas", "Comunidades Quilombolas"),
        options = leaflet::layersControlOptions(collapsed = FALSE)) |>
      leaflet::hideGroup(c("PMAs do mesmo Titular", "PMAs da mesma Parte Declarante",
                           "Unidades de Conservação", "Terras Indígenas", "Comunidades Quilombolas"))
  })

  observeEvent(input$ov_flags_tab3, {
    proxy <- leaflet::leafletProxy("mapa_cfem_pma_tab3")
    groups <- c("Unidades de Conservação" = "UCov", "Terras Indígenas" = "TIov", "Comunidades Quilombolas" = "QUIov")
    lapply(names(groups), function(g) proxy |> leaflet::hideGroup(g))
    sel <- input$ov_flags_tab3
    lapply(names(groups)[groups %in% sel], function(g) proxy |> leaflet::showGroup(g))
  })

  prev_hash_tab3 <- reactiveVal(NULL)
  observeEvent(pma_filtrado_tab3(), {
    pm <- pma_filtrado_tab3()
    validate(need(nrow(pm) > 0, "Nenhum processo minerário encontrado com os filtros."))
    h <- digest::digest(list(proc = sort(pm$PROCESSO), src = "simpl"))
    if (is.null(prev_hash_tab3()) || h != prev_hash_tab3()) {
      prev_hash_tab3(h)
      pm <- sanear_geo_para_leaflet(sf::st_transform(pm, 4326))
      if (is.null(pm) || nrow(pm) == 0) return()
      bb <- sf::st_bbox(pm)
      tryCatch({
        leaflet::leafletProxy("mapa_cfem_pma_tab3") |>
          leaflet::clearGroup("Processos Minerários") |>
          leaflet::addPolygons(data = pm, group = "Processos Minerários",
            color = "#FF3D00", weight = 2, opacity = 1, fillOpacity = 0.4, smoothFactor = 0.2,
            popup = ~paste0("<b>Processo:</b> ", PROCESSO, "<br>", "<b>Substância:</b> ", SUBS, "<br>",
                            "<b>Fase:</b> ", FASE, "<br>", "<b>Titular:</b> ", TITULAR)) |>
          leaflet::fitBounds(lng1 = as.numeric(bb["xmin"]), lat1 = as.numeric(bb["ymin"]),
                             lng2 = as.numeric(bb["xmax"]), lat2 = as.numeric(bb["ymax"]))
      }, error = function(e) {
        message("[mapa_cfem_pma_tab3/Processos Minerários] falha ao desenhar: ", conditionMessage(e))
      })
    }
  })

  observeEvent(pma_titular_tab3(), {
    d <- sanear_geo_para_leaflet(pma_titular_tab3())
    if (is.null(d) || nrow(d) == 0) return()
    tryCatch({
      leaflet::leafletProxy("mapa_cfem_pma_tab3") |>
        leaflet::clearGroup("PMAs do mesmo Titular") |>
        leaflet::addPolygons(data = d, group = "PMAs do mesmo Titular",
          color = "#0078FF", weight = 1, opacity = 0.8, fillOpacity = 0.35, smoothFactor = 0.2,
          popup = ~paste0("<b>Processo:</b> ", PROCESSO, "<br>", "<b>Titular:</b> ", TITULAR))
    }, error = function(e) {
      message("[mapa_cfem_pma_tab3/PMAs do mesmo Titular] falha ao desenhar: ", conditionMessage(e))
    })
  })

  observeEvent(pma_declarante_tab3(), {
    d <- sanear_geo_para_leaflet(pma_declarante_tab3())
    if (is.null(d) || nrow(d) == 0) return()
    tryCatch({
      leaflet::leafletProxy("mapa_cfem_pma_tab3") |>
        leaflet::clearGroup("PMAs da mesma Parte Declarante") |>
        leaflet::addPolygons(data = d, group = "PMAs da mesma Parte Declarante",
          color = "#6a3d9a", weight = 1, opacity = 0.8, fillOpacity = 0.35, smoothFactor = 0.2,
          popup = ~paste0("<b>Processo:</b> ", PROCESSO, "<br>", "<b>Titular:</b> ", TITULAR))
    }, error = function(e) {
      message("[mapa_cfem_pma_tab3/PMAs da mesma Parte Declarante] falha ao desenhar: ", conditionMessage(e))
    })
  })

  output$baixar_pma_sel_tab3 <- downloadHandler(
    filename = function() paste0("pmas_selecao_tab3_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_filtrado_tab3(), "pmas_selecao_tab3", temp_dir), file, overwrite = TRUE); gc()
    })
  output$baixar_pma_titular_tab3 <- downloadHandler(
    filename = function() paste0("pmas_titular_tab3_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_titular_tab3(), "pmas_titular_tab3", temp_dir), file, overwrite = TRUE); gc()
    })
  output$baixar_pma_declarante_tab3 <- downloadHandler(
    filename = function() paste0("pmas_declarante_tab3_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_dir <- tempdir()
      file.copy(exportar_shapefile(pma_declarante_tab3(), "pmas_declarante_tab3", temp_dir), file, overwrite = TRUE); gc()
    })

  outputOptions(output, "mapa_cfem_pma_tab3", suspendWhenHidden = FALSE)
  outputOptions(output, "cp_mapa", suspendWhenHidden = FALSE)

  output$relatorio_tab3 <- renderText({
    base <- relatorio_selecao(dados_mensal(), mensal = TRUE)
    metrica <- if (input$variavel_fluxo_tab3 == "VALORarr") "Valor Recolhido (R$)" else "Quantidade (Kg líquido)"
    agr_labels <- c(geral = "Geral", PROCESSO = "Processo", TITULAR = "Titular", NOME_arr = "Parte Declarante",
                    SUBSarr = "Substância", SUBSarrSIM = "Grupo (subs)", FASE = "Fase")
    agr <- agr_labels[[input$agrupamento_tab3]] %||% input$agrupamento_tab3
    df <- dados_mensal()
    if (!nrow(df)) return(paste0(base, "\n\nMétrica nos gráficos: ", metrica, " | Linhas por: ", agr))
    if (!"data" %in% names(df)) df$data <- as.Date(sprintf("%s-%02d-01", df$ANO, df$MES))
    paste0(base, "\n\nMétrica nos gráficos: ", metrica, " | Linhas por: ", agr, "\n")
  })
}

shinyApp(ui, server)