################################################################################
# graficos_historico.R
#
# Copia autocontida da funcao de grafico historico usada na aba 4 do app.R.
# Existe como arquivo separado (nao via source() de outro .R) pelo mesmo
# motivo de sempre: so app.R + *.rds sao enviados ao droplet no deploy --
# este arquivo e so referencia/leitura local, nao e chamado em producao.
#
# REESCRITO POR INTEIRO em 2026-08: a versao anterior (~750 linhas --
# calcular_gaps_titulo, unir_intervalos, complementar_intervalos,
# segmentos_aptidao_processo, periodos_aptidao_processo, camada_marcacao,
# eventos_marcacao) cruzava 6 tabelas na hora de renderizar
# (situacao_documental, protocolos_licenca_ambiental, eventos_classificados,
# serie_fase_status, eventos_renovacao_plg, intervalos_gu_aut_pesq) -- essas
# tabelas nao existem mais no pipeline novo (07_serie_temporal.R). A funcao
# abaixo le direto o rotulo_permissao ja calculado no 07 (uma fonte so, ja
# testada), converte em faixas continuas, plota. Binario verde
# (PERMITIDA_*) / vermelho (resto) -- decisao 2026-08. CFEM entra so como
# linha (sem cor por ponto). AJUSTE 2026-08: titulo + unidade sempre
# visiveis no eixo Y (pedido usuario).
#
# FONTE DA VERDADE: app.R (e a versao que roda de fato). Se editar aqui,
# copie o resultado de volta pro app.R -- nao existe mais geracao
# automatica via 07_proc_shiny_dossie.R (esse script nao existe mais).
################################################################################

# Formata duracao em "Y anos e X meses" com singular/plural corretos — usado
# no texto de protocolo de renovacao/licenca parado sem resposta da ANM.
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


################################################################################
# GRAFICO HISTORICO DO PROCESSO (aba 4) — reescrito 2026-08
#
# Substitui TODA a maquinaria antiga (calcular_gaps_titulo, unir_intervalos,
# complementar_intervalos, segmentos_aptidao_processo, periodos_aptidao_
# processo, periodos_nao_apto_processo, camada_marcacao, eventos_marcacao,
# grafico_historico_processo_plotly — ~565 linhas, cruzando 6 tabelas
# diferentes na hora de renderizar) por uma versao simples: le direto o
# rotulo_permissao ja calculado no 07 (uma fonte so, ja testada), converte
# em faixas continuas, plota. Binario verde (PERMITIDA_*) / vermelho (resto)
# -- decisao 2026-08. CFEM entra so como linha (sem cor por ponto).
################################################################################

COR_VERDE_FAIXA    <- "rgba(45,106,79,0.18)"
COR_VERMELHO_FAIXA <- "rgba(192,57,43,0.18)"
COR_SUSPENSAO_MARCA <- "#B9770E"
COR_RETOMADA_MARCA  <- "#1F618D"
COR_FECHA_MARCA     <- "#7B241C"

construir_faixas_grafico <- function(eventos_processo) {
  ev <- eventos_processo[order(eventos_processo$dtevento), ]
  n <- nrow(ev)
  if (n == 0) return(NULL)
  tibble::tibble(
    xmin = ev$dtevento,
    xmax = c(ev$dtevento[-1], Sys.Date()),
    cor  = ifelse(startsWith(ev$rotulo_permissao, "PERMITIDA"), COR_VERDE_FAIXA, COR_VERMELHO_FAIXA)
  )
}

grafico_historico_processo_plotly <- function(processo_alvo, eventos_serie, dados_cfem = NULL, variavel = "valor") {

  ev_p <- eventos_serie[eventos_serie$processo == processo_alvo, , drop = FALSE]
  ev_p <- ev_p[order(ev_p$dtevento), ]
  if (nrow(ev_p) == 0) return(plotly::plotly_empty(type = "scatter", mode = "markers"))

  faixas <- construir_faixas_grafico(ev_p)
  shapes <- lapply(seq_len(nrow(faixas)), function(i) {
    list(type = "rect", xref = "x", yref = "paper",
         x0 = as.character(faixas$xmin[i]), x1 = as.character(faixas$xmax[i]),
         y0 = 0, y1 = 1, fillcolor = faixas$cor[i], line = list(width = 0), layer = "below")
  })

  marcas <- ev_p[ev_p$papel %in% c("SUSPENDE", "RETOMA", "FECHA"), , drop = FALSE]
  if (nrow(marcas) > 0) {
    marcas$cor <- dplyr::case_when(
      marcas$papel == "SUSPENDE" ~ COR_SUSPENSAO_MARCA,
      marcas$papel == "RETOMA"   ~ COR_RETOMADA_MARCA,
      TRUE                       ~ COR_FECHA_MARCA
    )
    marcas$texto <- paste0(marcas$dsevento, " — ", format(marcas$dtevento, "%d/%m/%Y"))
  }

  tem_cfem <- !is.null(dados_cfem) && nrow(dados_cfem) > 0
  col_y <- if (variavel == "peso") "PESO_KG_final" else "VALORarr"

  if (tem_cfem && col_y %in% names(dados_cfem)) {
    cfem_p <- dados_cfem[order(dados_cfem$data), ]
    p <- plotly::plot_ly(
      cfem_p, x = ~data, y = stats::as.formula(paste0("~", col_y)),
      type = "scatter", mode = "lines+markers",
      line = list(color = "#1B4332", width = 2),
      marker = list(color = "#1B4332", size = 6),
      hovertemplate = if (variavel == "peso") "%{x|%d/%m/%Y}<br>%{y:,.2f} kg<extra></extra>"
                       else "%{x|%d/%m/%Y}<br>R$ %{y:,.2f}<extra></extra>",
      name = ""
    )
  } else {
    datas <- range(ev_p$dtevento)
    p <- plotly::plot_ly(x = datas, y = c(0, 0), type = "scatter", mode = "markers",
                         opacity = 0, hoverinfo = "none", showlegend = FALSE)
  }

  if (nrow(marcas) > 0) {
    p <- p |> plotly::add_markers(
      data = marcas, x = ~dtevento, y = 0, inherit = FALSE, showlegend = FALSE,
      marker = list(color = marcas$cor, size = 10, symbol = "line-ns-open", line = list(width = 2)),
      text = ~texto, hovertemplate = "%{text}<extra></extra>"
    )
  }

  titulo_grafico <- if (variavel == "peso") "Peso comercializado (kg)" else "Valor arrecadado (R$)"

  p |> plotly::layout(
    title = list(text = titulo_grafico, font = list(size = 13), x = 0, xanchor = "left"),
    shapes = shapes,
    xaxis = list(title = ""),
    yaxis = list(title = if (variavel == "peso") "kg" else "R$", visible = TRUE),
    margin = list(l = 60, r = 20, t = 36, b = 30),
    showlegend = FALSE
  )
}