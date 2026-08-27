# === INSERIDO POR 10_export_shinylive.R -- nao editar aqui ===
# bindCache desativado: sem backend de cache no webR.
bindCache <- function(x, ...) x

# app.R
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(sf)
  library(DT); library(plotly); library(leaflet)
  library(bslib); library(shinyWidgets); library(networkD3)
  library(ggplot2); library(scales); library(readr); library(writexl); library(digest); library(stringi)
})

if (identical(Sys.info()[["sysname"]], "Emscripten")) {
  bindCache <- function(x, ...) x
  message("[app] rodando em webR -- bindCache desativado.")
}

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


.read_rds_opt <- function(name) {
  p <- file.path(res_dir, name)
  if (file.exists(p)) readRDS(p) else NULL
}

# isTRUE() so aceita escalar; aqui precisamos do equivalente vetorizado,
# tratando NA como FALSE (nao queremos descartar linha por flag ausente).
isTRUE_vec <- function(x) !is.na(x) & x
micro_processos     <- .read_rds_opt("micro_processos.rds")
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

# ---- Fontes do grafico/tabelas do historico por processo (07_serie_
# temporal.R ja deixa tudo pronto em .rds dentro de shiny_dashboard) ----
eventos_serie                <- .read_rds_opt("eventos_serie.rds")
situacao_atual               <- .read_rds_opt("situacao_atual.rds")
fases_processo_tabela        <- .read_rds_opt("fases_processo_tabela.rds")
multas_infracoes_tabela      <- .read_rds_opt("multas_infracoes_tabela.rds")

# ---- Historico de eventos da aba 4 (F-03, 2026-08-25) -----------------------
# O app carregava "micro_eventos.rds", que NENHUM script do pipeline gerava --
# e o .read_rds_opt devolve NULL em silencio, entao a tabela de historico
# exibia "Dados indisponiveis" de forma permanente e a exportacao do dossie
# saia sem a secao de eventos. Ninguem via porque nada reclamava.
#
# NAO criamos um arquivo novo: eventos_serie.rds (saida do 07) e a MESMA
# tabela de origem (micro_processo_evento) ja cruzada com o dicionario, entao
# ja traz 'papel', que e justamente a coluna que faltava e o motivo de o 08
# nao conseguir gerar isso sozinho. Dois artefatos com a mesma procedencia e
# como eles divergem; no banco isso vira view, nao tabela.
#
# EVENTO SINTETICO FICA DE FORA: o Bloco D do 07 insere um FECHA na data de
# vencimento de titulo sem renovacao protocolada. E inferencia nossa, derivada
# da data, NAO um evento publicado pela ANM. Se aparecesse aqui sem distincao,
# o usuario veria um fechamento que nao encontraria ao conferir no cadastro --
# caro num produto investigativo.
micro_eventos <- if (!is.null(eventos_serie)) {
  ev <- eventos_serie
  if ("evento_sintetico" %in% names(ev)) {
    ev <- ev[!isTRUE_vec(ev$evento_sintetico), , drop = FALSE]
  }
  cols <- intersect(c("processo", "dtevento", "dsevento", "papel",
                      "tipo_proc", "sufixo", "dspublicacaodou"), names(ev))
  out <- ev[, cols, drop = FALSE]
  names(out)[names(out) == "dtevento"]        <- "data"
  names(out)[names(out) == "dsevento"]        <- "evento"
  names(out)[names(out) == "dspublicacaodou"] <- "publicacao_dou"
  out[order(out$processo, out$data), , drop = FALSE]
} else {
  NULL
}

dossie_ok  <- !is.null(dossie_resumo_processo)
inapto_ok  <- dossie_ok  # nome mantido por compatibilidade com o resto do server abaixo

# ---- Filtro de motivo (aba Consulta) -- versao simples (decisao 2026-08):
# so a lista de valores ja legiveis de motivo_nao_apto (07_motivos_
# fechamento: caducidade/indeferimento/judicial/vencimento/etc), sem tabela
# de referencia nem sistema de flags -- e so um pickerInput direto na coluna.
cp_motivos_all <- if (!is.null(situacao_atual)) sort(unique(na.omit(situacao_atual$motivo_nao_apto))) else character(0)

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
    base_font = font_link(
    "Inter",
    href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;700&display=swap"
  ),
  heading_font = font_link(
    "Inter",
    href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;700&display=swap"
  ),
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
  # F-06 (2026-08-25): este bloco anunciava "Proximidade (10 km)" para os tres
  # territorios e lia "UCov10km", que nao existe. Os raios sao DIFERENTES por
  # decisao metodologica (05_integracao_final.R): TI e quilombola a 10 km, UC a
  # 2 km. O texto agora explicita cada raio em vez de afirmar um numero unico.
  bloco_buf <- c(
    "Proximidade de Territórios Protegidos:",
    add_ov("TIov10km",  "TIname_ov",  "Terras Indígenas (10 km)"),
    add_ov("UCov2km",   "UCname_ov",  "Unidades de Conservação (2 km)"),
    add_ov("QUIov10km", "QUIname_ov", "Comunidades Quilombolas (10 km)")
  )
  if (identical(bloco_buf[-1], list(NULL, NULL, NULL))) {
    bloco_buf <- "Proximidade de Territórios Protegidos: Nenhuma."
  }

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
                      # F-06 (2026-08-25): a UI oferecia "UCov10km", que NAO existe
                      # nos dados -- o pipeline produz UCov2km. O filtro era inerte e o
                      # rotulo dizia 10 km quando o raio real da UC e 2 km. Os raios sao
                      # DIFERENTES por decisao metodologica (ver 05_integracao_final.R):
                      # TI e quilombola a 10 km, UC a 2 km.
                      choices = c("UC" = "UCov", "TI" = "TIov", "QUI" = "QUIov",
                                  "UC (2 km)" = "UCov2km", "TI (10 km)" = "TIov10km", "QUI (10 km)" = "QUIov10km"),
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
                      # F-06 (2026-08-25): a UI oferecia "UCov10km", que NAO existe
                      # nos dados -- o pipeline produz UCov2km. O filtro era inerte e o
                      # rotulo dizia 10 km quando o raio real da UC e 2 km. Os raios sao
                      # DIFERENTES por decisao metodologica (ver 05_integracao_final.R):
                      # TI e quilombola a 10 km, UC a 2 km.
                      choices = c("UC" = "UCov", "TI" = "TIov", "QUI" = "QUIov",
                                  "UC (2 km)" = "UCov2km", "TI (10 km)" = "TIov10km", "QUI (10 km)" = "QUIov10km"),
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
                      # F-06 (2026-08-25): a UI oferecia "UCov10km", que NAO existe
                      # nos dados -- o pipeline produz UCov2km. O filtro era inerte e o
                      # rotulo dizia 10 km quando o raio real da UC e 2 km. Os raios sao
                      # DIFERENTES por decisao metodologica (ver 05_integracao_final.R):
                      # TI e quilombola a 10 km, UC a 2 km.
                      choices = c("UC" = "UCov", "TI" = "TIov", "QUI" = "QUIov",
                                  "UC (2 km)" = "UCov2km", "TI (10 km)" = "TIov10km", "QUI (10 km)" = "QUIov10km"),
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
          pickerInput("cp_motivo", "Motivo (quando não apto):",
                      choices = cp_motivos_all, selected = character(0), multiple = TRUE, options = picker_opts),
          pickerInput("cp_uf", "UF(s):",
                      choices = cp_ufs_all, selected = cp_ufs_all, multiple = TRUE, options = picker_opts),
          pickerInput("cp_mun", "Município(s):",
                      choices = cp_muns_all, multiple = TRUE, options = picker_opts),
          tags$hr(),
          uiOutput("cp_info"),
          tags$hr(),
          div(style = "max-height: 460px; overflow-y: auto;", DTOutput("cp_lista", height = "auto"))
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

  # F-02 (2026-08-25): esta funcao devolvia o conjunto INTEIRO quando nenhuma
  # das colunas pedidas existia no data.frame -- sem erro, sem aviso. Foi o que
  # fez os botoes "Territorios Protegidos" das abas 1 e 3 parecerem funcionar
  # sem filtrar nada por meses (o 05 nao propagava as flags para a tabela de
  # CFEM). O intersect tambem engolia o caso PARCIAL: com 3 de 6 colunas
  # presentes, filtrava pelas 3 e nao dizia nada.
  #
  # Agora avisa em qualquer descasamento. Nao usa stop(): derrubar a sessao do
  # usuario por coluna faltante e pior que mostrar o resultado com aviso -- mas
  # o silencio, que foi o problema original, acabou.
  filtra_sobrepos <- function(df, flags) {
    if (length(flags) == 0) return(df)
    cols_ok       <- intersect(flags, names(df))
    cols_faltando <- setdiff(flags, names(df))

    if (length(cols_faltando) > 0) {
      msg <- paste0("[app][filtra_sobrepos] coluna(s) de flag ausente(s) nos dados: ",
                    paste(cols_faltando, collapse = ", "),
                    " -- filtro IGNORADO para essa(s). Conferir a propagacao no ",
                    "05_integracao_final.R (FLAGS_SOBREPOSICAO em R/utils.R).")
      warning(msg, call. = FALSE)
      showNotification(msg, type = "warning", duration = 12)
    }

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
      cols_sa <- intersect(c("processo", "apto_operar", "motivo_nao_apto"), names(situacao_atual))
      df <- merge(df, situacao_atual[, cols_sa, drop = FALSE], by = "processo", all.x = TRUE)
    }

    if (!is.null(input$cp_motivo) && length(input$cp_motivo) > 0 && "motivo_nao_apto" %in% names(df)) {
      df <- df[df$motivo_nao_apto %in% input$cp_motivo, , drop = FALSE]
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

  # ---- Caixa de status do processo (dossie) — apto_operar/motivo_nao_apto,
  # direto do situacao_atual/dossie_resumo_processo, sem reclassificar nada ----
  # ATUALIZADO (achado 2026-07): fonte primaria passa a ser situacao_atual
  # (cobre TODO processo, independente de ter declarado CFEM) em vez de
  # dossie_resumo_processo (que so existe pra quem declarou CFEM). CFEM
  # continua disponivel, mas so como enriquecimento OPCIONAL — se nao
  # existir, a caixa aparece igual, so sem essa parte. tem_evento_suspensao/
  # tem_evento_anulacao deixam de vir pre-agregados do 07 (CFEM-gated) e
  # passam a ser calculados aqui, direto de eventos_serie, pro
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

    ev_p <- if (!is.null(eventos_serie)) eventos_serie[eventos_serie$processo == p, , drop = FALSE] else NULL
    tem_evento_suspensao <- !is.null(ev_p) && any(ev_p$papel == "SUSPENDE", na.rm = TRUE)
    tem_evento_anulacao  <- !is.null(ev_p) && any(ev_p$papel == "FECHA",    na.rm = TRUE)

    cod <- row$motivo_nao_apto %||% NA_character_
    # SEM cfem_motivo_ref (removido — nao faz mais sentido, ver decisao
    # 2026-08): motivo_nao_apto ja vem legivel do 07_motivos_fechamento
    # (caducidade/indeferimento/judicial/etc) -- so formata (maiuscula +
    # espaco em vez de "_"), sem precisar de tabela de traducao.
    rot <- if (identical(row$apto_operar, "TRUE")) "Apto" else
      if (!is.na(cod)) tools::toTitleCase(gsub("_", " ", cod)) else "Situacao a revisar"
    desc <- NULL

    cor_cfg <- if (identical(row$apto_operar, "TRUE")) {
      list(bg = "#EAF6EC", bd = "#2D6A4F", tx = "#1B4332")   # verde -- pode operar
    } else {
      list(bg = "#FCEBEB", bd = "#A32D2D", tx = "#501313")   # vermelho -- nao pode operar
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
    sobrep_uc  <- nrow(pma_row) > 0 && "UCov2km" %in% names(pma_row) && isTRUE(as.logical(pma_row$UCov2km[1]))
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
      # NOVO (achado 2026-07, generalizado 2026-08): duracao da protecao por
      # renovacao/prorrogacao pendente, quando aplicavel -- texto informativo
      # (mora administrativa protege o titular, nao e irregularidade). Cobre
      # PLG, LICEN, REG_EXT e PESQUISA (cada um com base legal propria na
      # Consolidacao Normativa -- nao so art. 211/213, que e especifico de PLG).
      if (isTRUE(row$protegido_renovacao) && !is.null(row$dt_protocolo_renovacao) && !is.na(row$dt_protocolo_renovacao))
        tags$div(style = "font-size:11px; margin-top:2px;",
                 sprintf("Renovacao/prorrogacao solicitada no prazo e sem resposta da ANM ha %s",
                         fmt_duracao_anos_meses(row$dt_protocolo_renovacao))),
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
      tags$div(style = "font-weight:600; font-size:13px; margin-bottom:6px; color:#2C3E50;", "Fases do processo"),
      DTOutput("cp_tabela_fases"),
      div(style = "height:14px;"),
      tags$div(style = "font-weight:600; font-size:13px; margin-bottom:6px; color:#2C3E50;", "Multas e infrações"),
      DTOutput("cp_tabela_penalidades"),
      div(style = "height:14px;")
    )
  })

  cp_dados_cfem_proc <- reactive({
    p <- cp_proc_sel(); req(p)
    if (is.null(cfem_mensal)) return(NULL)
    d <- cfem_mensal[as.character(cfem_mensal$PROCESSO) == p, , drop = FALSE]
    if (nrow(d) == 0) NULL else d
  })

  cp_grafico_reactivo_plotly <- function(variavel) {
    reactive({
      p <- cp_proc_sel(); req(p)
      grafico_historico_processo_plotly(
        processo_alvo = p,
        eventos_serie = eventos_serie,
        dados_cfem = cp_dados_cfem_proc(),
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

  cp_historico_fases_p <- reactive({
    p <- cp_proc_sel(); req(p)
    if (is.null(fases_processo_tabela)) return(NULL)
    h <- fases_processo_tabela[fases_processo_tabela$processo == p, , drop = FALSE]
    if (nrow(h) == 0) NULL else h
  })

  # NOVO: fatorado do renderDT abaixo pra ser reaproveitado pelos exports
  # (Word e Excel) sem duplicar a logica de formatacao.
  construir_tabela_fases_export <- function(h) {
    fmt_data_fim <- function(x, status) {
      dplyr::case_when(
        !is.na(x)             ~ format(x, "%d/%m/%Y"),
        status == "PROTOCOLO" ~ "—",
        TRUE                  ~ "Atual"
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
    validate(need(!is.null(h), "Sem historico de fases para este processo."))
    tab <- construir_tabela_fases_export(h)

    cores_status <- c(ATIVA = "#D4EDDA", SUSPENSA = "#FFF3CD", ENCERRADA = "#F8D7DA",
                       PRE_AUTORIZACAO = "#E2E3E5", PROTOCOLO = "#D1ECF1")

    # ordering = FALSE: essa tabela e uma narrativa cronologica (PRE_
    # AUTORIZACAO sempre primeiro, ja vem ordenada do 07) — deixar o usuario
    # clicar no cabecalho e reordenar por texto quebraria essa leitura.
    DT::datatable(tab, rownames = FALSE, class = "compact", selection = "none",
      options = list(scrollX = TRUE, dom = "t", pageLength = -1, ordering = FALSE)) |>
      DT::formatStyle("Status", backgroundColor = DT::styleEqual(names(cores_status), unname(cores_status)))
  })

  # ---- Historico de penalidades e ocorrencias (Bloco I do 07) ----
  cp_penalidades_p <- reactive({
    p <- cp_proc_sel(); req(p)
    if (is.null(multas_infracoes_tabela)) return(NULL)
    h <- multas_infracoes_tabela[multas_infracoes_tabela$processo == p, , drop = FALSE]
    if (nrow(h) == 0) NULL else h
  })

  # NOVO: fatorado do renderDT abaixo pra ser reaproveitado no export do
  # relatorio docx, mesmo padrao de construir_tabela_fases_export.
  # AJUSTE (pedido usuario): Tipo vira primeira coluna, em CAPSLOCK — mesmo
  # padrao visual da coluna Fase/Status na tabela de historico de fases.
  construir_tabela_penalidades_export <- function(h) {
    data.frame(
      Tipo                    = h$tipo,
      Data                    = format(h$data, "%d/%m/%Y"),
      Evento                  = dplyr::coalesce(h$evento, "—"),
      `Descrição/Publicação`  = dplyr::coalesce(h$descricao_publicacao, "—"),
      check.names = FALSE
    )
  }

  output$cp_tabela_penalidades <- renderDT({
    h <- cp_penalidades_p()
    validate(need(!is.null(h), "Sem multas ou infracoes registradas para este processo."))
    tab <- construir_tabela_penalidades_export(h)

    cores_tipo <- c(
      "SUSPENSAO/EMBARGO"   = "#F5C6CB",
      "INFRACAO"            = "#FFCC80",
      "RETOMADA/DESEMBARGO" = "#D4EDDA"
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
      paste0("microdados_", gsub("[/]", "-", p), "_", Sys.Date(), ".xlsx")
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

    # F-09 (2026-08-25): este mapa listava 5 valores (MUDA_FASE, FECHA,
    # SUSPENDE, RETOMA, NAO_CLASSIFICADO) quando existem 15 na base real --
    # confirmado no dicionario v2 (2.920 eventos). PROTOC e as dez variantes
    # NEUTRO_* ficavam sem destaque, e NAO_CLASSIFICADO nem existe mais (foi
    # eliminado na redesenho do 07; o que nao classifica vira NEUTRO_outros).
    #
    # Os NEUTRO_* compartilham o mesmo cinza de propósito: sao ruido de fundo
    # da linha do tempo, nao decisao. O que precisa saltar aos olhos e o que
    # abre, fecha, suspende ou retoma.
    NEUTRO_BG <- "#F5F5F5"; NEUTRO_TX <- "#6C757D"
    papel_cor <- c(
      MUDA_FASE = "#D4EDDA", FECHA = "#F8D7DA", SUSPENDE = "#FFF3CD",
      RETOMA    = "#D1ECF1", PROTOC = "#E2E3F3",
      NEUTRO_outros = NEUTRO_BG, NEUTRO_barragem = NEUTRO_BG,
      NEUTRO_transferencia_direitos = NEUTRO_BG, NEUTRO_financeiro = NEUTRO_BG,
      NEUTRO_processual = NEUTRO_BG, NEUTRO_relatorio = NEUTRO_BG,
      NEUTRO_negado = NEUTRO_BG, NEUTRO_arquiv_punitivo = NEUTRO_BG,
      NEUTRO_marco_informativo = NEUTRO_BG, NEUTRO_covid_prorrogacao = NEUTRO_BG
    )
    papel_tx <- c(
      MUDA_FASE = "#155724", FECHA = "#721C24", SUSPENDE = "#856404",
      RETOMA    = "#0C5460", PROTOC = "#3F3D8F",
      NEUTRO_outros = NEUTRO_TX, NEUTRO_barragem = NEUTRO_TX,
      NEUTRO_transferencia_direitos = NEUTRO_TX, NEUTRO_financeiro = NEUTRO_TX,
      NEUTRO_processual = NEUTRO_TX, NEUTRO_relatorio = NEUTRO_TX,
      NEUTRO_negado = NEUTRO_TX, NEUTRO_arquiv_punitivo = NEUTRO_TX,
      NEUTRO_marco_informativo = NEUTRO_TX, NEUTRO_covid_prorrogacao = NEUTRO_TX
    )

    # Lista fixa volta a ficar incompleta na proxima versao do dicionario. Em
    # vez de mapa exaustivo, avisa e deixa o valor novo sem cor (fallback do
    # DT), que e visivel sem quebrar a tabela.
    papel_col <- if ("papel" %in% names(df)) df$papel else rep(NA_character_, nrow(df))
    papeis_novos <- setdiff(unique(stats::na.omit(papel_col)), names(papel_cor))
    if (length(papeis_novos) > 0) {
      warning("[app][historico] papel(is) sem cor definida: ",
              paste(papeis_novos, collapse = ", "),
              " -- acrescentar ao mapa em app.R.", call. = FALSE)
    }
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
      # BASEMAP (2026-08-25): CartoDB.Positron passou a exigir chave de API e o
      # mapa desta aba abria nele por ser o primeiro da lista -- resultado:
      # "API key required" so aqui, enquanto a aba 4 (que abre no Esri) seguia
      # normal. Trocado por Esri.WorldGrayCanvas, equivalente visual e sem
      # chave. Nada a ver com o pipeline: e mudanca de politica do provedor.
      leaflet::addProviderTiles("Esri.WorldGrayCanvas", group = "Mapa claro") |>
      leaflet::addLayersControl(baseGroups = c("Satélite", "Mapa claro"),
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
      # BASEMAP (2026-08-25): CartoDB.Positron passou a exigir chave de API e o
      # mapa desta aba abria nele por ser o primeiro da lista -- resultado:
      # "API key required" so aqui, enquanto a aba 4 (que abre no Esri) seguia
      # normal. Trocado por Esri.WorldGrayCanvas, equivalente visual e sem
      # chave. Nada a ver com o pipeline: e mudanca de politica do provedor.
      leaflet::addProviderTiles("Esri.WorldGrayCanvas", group = "Mapa claro") |>
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
        baseGroups = c("Mapa claro", "Satélite"),
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
