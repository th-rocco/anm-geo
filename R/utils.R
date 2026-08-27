################################################################################
# R/utils.R  —  funções compartilhadas do pipeline ANM/garimpo Amazônia.
#
# Este arquivo centraliza funções usadas por mais de um script do pipeline.
# Cada script deve dar source() aqui, depois de carregar seus pacotes.
#
# Seções:
#   A) Download / manifest (etapa 01) — inclui User-Agent do projeto
#   B) Utilitários genéricos de ETL (etapa 02): safe_step, safe_unzip,
#      to_upper_utf8, first_match, ler_vetor (inventário de geometrias +
#      fontes obrigatórias), clean_geometry (com contagem de descarte)
#   C) Filtro por palavra-chave com diagnóstico (etapa 02 — IBAMA/ICMBio/SEMA)
#   D) Parsing "inteligente" de CFEM com diagnóstico (delimitador/decimal)
#
# Revisão 2026-08 (auditoria Sentinela da Amazônia), 4 mudanças:
#   F-04  UA_PROJETO + options(HTTPUserAgent) — o default do R levava 403 da
#         FUNAI; atalho de skip do tis_poligonais.zip removido.
#   F-07  padroniza_doc: ordem do case_when corrigida e ramo de padding para 14
#         removido; mascarar_doc_pessoa_fisica() para uso na ingestão.
#   F-01  ler_vetor(): compara registros no arquivo vs feições lidas e falha
#         quando a fonte é obrigatória. FONTES_OBRIGATORIAS declarada.
################################################################################

suppressPackageStartupMessages({
  library(digest)
  library(here)
  library(dplyr)
  library(stringr)
  library(stringi)
  library(readr)
  library(tibble)
})

# Usados via :: (nao anexados): curl (testar_user_agent), terra (ler_vetor,
# clean_geometry, relacionar_flag_opcional), purrr, sf.

# ==============================================================================
# A) DOWNLOAD / MANIFEST (etapa 01)
# ==============================================================================

MANIFEST_DIR <- here::here("data", "_manifest")

# ------------------------------------------------------------------------------
# User-Agent (decisao 2026-08, auditoria F-04)
# ------------------------------------------------------------------------------
# O default do R ("R (versao ...)") e recusado com 403 pelo geoserver da FUNAI.
# Nao e bloqueio a robos em geral -- e rejeicao do identificador da biblioteca.
# Optamos por nos identificar honestamente (projeto + contato) em vez de forjar
# navegador. Se algum servidor recusar ESTE UA, usar UA_FALLBACK e registrar
# AQUI qual fonte exigiu e em que data.
#
# VERIFICADO 2026-08-25: UA_PROJETO aceito por TODAS as fontes, inclusive a
# FUNAI (tis_poligonais.zip baixou 22,3 MB). UA_FALLBACK nao foi necessario e
# permanece so como contingencia.
#
# Fontes que exigiram UA_FALLBACK ate hoje: (nenhuma)
UA_PROJETO  <- "anm-geo/1.0 (pesquisa academica; contato: <preencher>)"
UA_FALLBACK <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
                     "AppleWebKit/537.36 (KHTML, like Gecko)",
                     "Chrome/124.0.0.0 Safari/537.36")

options(HTTPUserAgent = UA_PROJETO)

# Diagnostico: testa um UA contra uma URL sem baixar o corpo inteiro.
#
# CORRIGIDO 2026-08-25: a primeira versao usava HEAD (nobody = TRUE) e deu 4
# falsos positivos numa rodada de 37 URLs -- ICMBio (2x), IBAMA autos e o PDF
# do MER responderam 403/NA ao HEAD e baixaram normalmente logo depois. Varios
# servidores recusam HEAD e aceitam GET. Passa a pedir so o primeiro byte via
# Range, que e igualmente barato e usa o mesmo metodo do download real.
#
# Servidor que ignora Range devolve 200 e o corpo todo; por isso o timeout
# curto e o descarte da resposta.
testar_user_agent <- function(url, ua = getOption("HTTPUserAgent"), timeout_s = 20) {
  h <- curl::new_handle()
  curl::handle_setopt(h, useragent = ua, followlocation = TRUE,
                      range = "0-0", timeout = timeout_s)
  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
  status <- if (is.null(res)) NA_integer_ else res$status_code
  message(sprintf("[UA] status %s | %s", status, substr(basename(url), 1, 45)))
  invisible(status)
}

sha256_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  digest::digest(path, algo = "sha256", file = TRUE)
}

listar_manifests_anteriores <- function(manifest_dir = MANIFEST_DIR) {
  if (!dir.exists(manifest_dir)) return(character(0))
  arquivos <- list.files(manifest_dir, pattern = "^download_log_.*\\.csv$", full.names = TRUE)
  sort(arquivos, decreasing = TRUE)
}

# Consulta (não decide) se o conteúdo de um arquivo mudou desde a última vez
# que apareceu em um manifest. Ver 01_download.R para o uso completo.
hash_anterior <- function(dest_dir, filename, manifests = listar_manifests_anteriores()) {
  if (length(manifests) == 0) return(NA_character_)
  for (m in manifests) {
    df <- tryCatch(
      readr::read_csv(m, col_types = readr::cols(.default = "c"), show_col_types = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || !all(c("dest_dir", "filename", "sha256") %in% names(df))) next
    linha <- df[df$dest_dir == dest_dir & df$filename == filename, ]
    if (nrow(linha) > 0) return(linha$sha256[nrow(linha)])
  }
  NA_character_
}

tamanho_anterior <- function(dest_dir, filename, manifests = listar_manifests_anteriores()) {
  if (length(manifests) == 0) return(NA_integer_)
  for (m in manifests) {
    df <- tryCatch(
      readr::read_csv(m, col_types = readr::cols(.default = "c"), show_col_types = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || !all(c("dest_dir", "filename", "size_bytes", "success") %in% names(df))) next
    linha <- df[df$dest_dir == dest_dir & df$filename == filename & df$success == "TRUE", ]
    if (nrow(linha) > 0) return(as.integer(linha$size_bytes[nrow(linha)]))
  }
  NA_integer_
}
 
download_file <- function(url, dest_dir, filename = basename(url), max_attempts = 5) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dst <- file.path(dest_dir, filename)
 
  # NOTA (2026-08, auditoria F-04): havia aqui um atalho que pulava o download
  # do tis_poligonais.zip quando o arquivo ja existia -- contorno para o 403 da
  # FUNAI. Com o UA corrigido o atalho se inverte: passaria a reportar sucesso
  # para um arquivo colocado a mao que nunca mais atualiza, sem passar pela
  # checagem de tamanho contra o manifest. Removido. Nenhuma fonte tem excecao.

  tam_min <- tamanho_anterior(dest_dir, filename)
  if (is.na(tam_min)) {
    message("[download] sem manifest anterior para ", filename,
            " — completude checada so por size>0 (1o download deste arquivo).")
  }
 
  resultado_falha <- list(success = FALSE, sha256 = NA_character_, size_bytes = NA_integer_,
                          attempts_used = max_attempts, note = "falhou_apos_todas_tentativas")
 
  for (attempt in seq_len(max_attempts)) {
    message("Downloading: ", filename, " [", attempt, "/", max_attempts, "]")
    ok <- tryCatch({
      download.file(url, destfile = dst, mode = "wb", method = "libcurl")

      if (!file.exists(dst) || is.na(file.info(dst)$size) || file.info(dst)$size == 0) {
        stop("Downloaded file is missing or empty.")
      }

      TRUE
    }, error = function(e) {
      warning("Attempt ", attempt, " failed: ", filename, " | ", conditionMessage(e))
      FALSE
    })
    if (ok) {
      tam_baixado <- file.info(dst)$size
      nota <- "download_ok"
      if (!is.na(tam_min) && tam_baixado < tam_min) {
        message(sprintf(
          "[download] AVISO (so log, nao bloqueia): %s veio menor que o ultimo download bem-sucedido - %s bytes agora vs %s bytes antes.",
          filename, fmt_int(tam_baixado), fmt_int(tam_min)
        ))
        nota <- "download_ok_tamanho_menor_que_anterior"
      }
      message("OK: ", filename, " | size=", fmt_int(tam_baixado))
      return(list(success = TRUE, sha256 = sha256_file(dst), size_bytes = tam_baixado,
                  attempts_used = attempt, note = nota))
    }
    Sys.sleep(runif(1, 5, 15))
  }
  resultado_falha
}
 

download_named_urls <- function(named_urls, dest_dir, target_name, max_attempts = 5) {
  purrr::imap(named_urls, ~{
    r <- download_file(url = .x, dest_dir = dest_dir, filename = .y, max_attempts = max_attempts)
    Sys.sleep(runif(1, 1, 3))
    list(target = target_name, filename = .y, url = .x, dest_dir = dest_dir,
         success = r$success, sha256 = r$sha256, size_bytes = r$size_bytes,
         attempts_used = r$attempts_used, note = r$note)
  })
}

# ==============================================================================
# B) UTILITÁRIOS GENÉRICOS DE ETL (etapa 02)
# ==============================================================================

safe_step <- function(label, expr) {
  message("\n--- ", label, " ---")
  # CORRIGIDO 2026-08-25: antes fazia force(expr), o que avalia a expressao no
  # ambiente do chamador. Qualquer return() no corpo do passo (padrao usado em
  # varios blocos do 02 para "pular" fonte ausente) estourava com
  # "no function to return from, jumping to top level" e o passo era marcado
  # como FAILED por um erro que nao tinha nada a ver com o dado.
  # Passa a montar uma funcao anonima com o corpo do passo, entao return()
  # funciona como qualquer um espera ao ler o codigo.
  corpo <- as.function(c(alist(), substitute(expr)), envir = parent.frame())
  status <- tryCatch({
    corpo()
    list(success = TRUE, error_msg = NA)
  }, error = function(e) {
    msg <- conditionMessage(e)
    warning(label, " failed: ", msg)
    list(success = FALSE, error_msg = msg)
  })
  etl_log[[label]] <<- status
  return(status$success)
}

safe_unzip <- function(zip_path, exdir) {
  # 2026-08-25: antes engolia a causa (tryCatch -> FALSE mudo), o que custou
  # tempo ao diagnosticar o zip dos microdados. Agora reporta o motivo e
  # tambem o aviso do unzip(), que sinaliza truncamento e zip64.
  if (!file.exists(zip_path)) {
    message("[safe_unzip] arquivo nao existe: ", zip_path)
    return(FALSE)
  }
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  tryCatch(
    withCallingHandlers(
      { unzip(zip_path, exdir = exdir); TRUE },
      warning = function(w) {
        message("[safe_unzip] aviso em ", basename(zip_path), ": ",
                conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      message("[safe_unzip] FALHOU em ", basename(zip_path), ": ",
              conditionMessage(e))
      FALSE
    }
  )
}

to_upper_utf8 <- function(x, from = "") {
  if (nzchar(from)) x <- stringi::stri_encode(x, from = from, to = "UTF-8")
  else x <- iconv(x, from = from, to = "UTF-8")
  toupper(x)
}

first_match <- function(path, pattern, ignore.case = TRUE) {
  x <- list.files(path, pattern = pattern, ignore.case = ignore.case, full.names = TRUE)
  if (length(x) == 0) NA_character_ else x[1]
}

clean_geometry <- function(v, label = NULL) {
  n0 <- length(v)
  v <- terra::makeValid(v)
  n1 <- length(v)
  n_makevalid <- n0 - n1          # <<< 2026-08: perda no makeValid, antes invisivel
  area_ok <- terra::expanse(v) > 0
  n_zero_area <- sum(!area_ok)
  v <- v[area_ok, ]
  n2 <- length(v)
  v <- terra::project(v, "EPSG:4326")

  if (!is.null(label)) {
    # A mensagem anterior so reportava "descartadas (area==0)", o que escondia a
    # perda do makeValid. Ex. IBAMA embargos: 90.984 -> 77.157, com area==0 = 0.
    message(sprintf(
      "[%s] clean_geometry | inicial: %d | pos-makeValid: %d (perdidas: %d) | area==0: %d | final: %d",
      label, n0, n1, n_makevalid, n_zero_area, n2
    ))
    if (n_makevalid > 0) {
      message(sprintf(
        "[%s] ATENCAO: %d feicao(oes) (%.1f%%) descartada(s) por makeValid -- geometria irreparavel.",
        label, n_makevalid, 100 * n_makevalid / n0
      ))
    }
  }
  v
}

# ------------------------------------------------------------------------------
# LEITURA VETORIAL COM INVENTARIO (decisao 2026-08, auditoria F-01)
# ------------------------------------------------------------------------------
# O pipeline foi desenhado de proposito para TOLERAR fonte ausente:
# carregar_shp_opcional() devolve NULL, relacionar_flag_opcional() grava NA (nao
# 0) e o 05 escreve um CSV de disponibilidade. Isso e bom desenho e fica.
#
# O que faltava era distinguir OPCIONAL de OBRIGATORIA, e detectar a camada que
# chega vazia sem erro. Foi assim que o shapefile de autos de infracao do ICMBio
# passou anos sem contribuir: terra::vect() escolhe UM tipo de geometria e
# descarta o resto por message (nao por erro), o passo reportou sucesso, e o
# filtro de palavra-chave zerou o que sobrou.
#
# ler_vetor() compara o numero de REGISTROS na tabela de atributos com o numero
# de FEICOES efetivamente lidas. A diferenca e exatamente o que foi descartado
# em silencio.

# Fontes sem as quais a etapa correspondente NAO tem sentido rodar.
# Chave = rotulo passado em ler_vetor(label = ...).
FONTES_OBRIGATORIAS <- c(
  "PMA",            # 03 -- poligonos dos processos minerarios (ANM)
  "AMZ_LEGAL",      # 03 -- recorte da Amazonia Legal
  "TI",             # 03 -- terras indigenas (FUNAI)
  "UC",             # 03 -- unidades de conservacao (MMA/CNUC)
  "QUILOMBOLA"      # 03 -- territorios quilombolas (INCRA)
)

# Formata inteiro com separador de milhar sem disparar o aviso do prettyNum
# ('big.mark' and 'decimal.mark' are both '.'), que poluia o log a cada chamada.
fmt_int <- function(x) formatC(x, format = "d", big.mark = ".", decimal.mark = ",")

# Le um vetor e reporta a composicao de geometrias ANTES de qualquer limpeza.
#   obrigatoria = NULL  -> decide por FONTES_OBRIGATORIAS
#   obrigatoria = TRUE  -> ausencia, leitura vazia ou descarte silencioso = erro
#   obrigatoria = FALSE -> devolve NULL/segue, mantendo o contrato atual (flag NA)
ler_vetor <- function(path, label = basename(path), obrigatoria = NULL,
                      max_perda_pct = 0) {

  if (is.null(obrigatoria)) obrigatoria <- label %in% FONTES_OBRIGATORIAS

  parar_ou_avisar <- function(msg) {
    if (obrigatoria) stop("[fonte OBRIGATORIA] ", label, ": ", msg, call. = FALSE)
    warning("[fonte opcional] ", label, ": ", msg,
            " (flag correspondente fica NA, nao 0).", call. = FALSE)
    NULL
  }

  if (is.na(path) || !file.exists(path)) {
    return(parar_ou_avisar(paste0("arquivo nao encontrado em ", path)))
  }

  # 1) Quantos REGISTROS existem na tabela de atributos (nao passa pela
  #    resolucao de tipo de geometria -- e a contagem verdadeira do arquivo).
  n_registros <- tryCatch(
    nrow(terra::vect(path, what = "attributes")),
    error = function(e) NA_integer_
  )

  # 2) Quantas FEICOES o terra devolve, capturando o aviso de descarte.
  avisos <- character(0)
  v <- withCallingHandlers(
    tryCatch(terra::vect(path), error = function(e) NULL),
    message = function(m) {
      avisos <<- c(avisos, trimws(conditionMessage(m)))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      avisos <<- c(avisos, trimws(conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )

  if (is.null(v)) return(parar_ou_avisar("terra::vect() falhou na leitura"))

  n_feicoes <- length(v)
  tipos <- tryCatch(as.character(terra::geomtype(v)), error = function(e) NA_character_)

  perdidos  <- if (is.na(n_registros)) NA_integer_ else n_registros - n_feicoes
  perda_pct <- if (is.na(perdidos) || n_registros == 0) NA_real_ else 100 * perdidos / n_registros

  message(sprintf(
    "[%s] ler_vetor | registros no arquivo: %s | feicoes lidas: %s | tipo: %s%s",
    label,
    ifelse(is.na(n_registros), "?", fmt_int(n_registros)),
    fmt_int(n_feicoes),
    tipos,
    if (!is.na(perdidos) && perdidos > 0)
      sprintf(" | DESCARTADAS EM SILENCIO: %s (%.1f%%)",
              fmt_int(perdidos), perda_pct) else ""
  ))
  for (a in avisos) message("[", label, "] aviso do terra: ", a)

  if (n_feicoes == 0) {
    return(parar_ou_avisar("leitura devolveu 0 feicoes -- camada vazia"))
  }
  if (!is.na(perda_pct) && perda_pct > max_perda_pct) {
    msg <- sprintf(paste0("%s de %s registro(s) descartado(s) na leitura (%.1f%%). ",
                          "Geometria mista? Conferir antes de seguir."),
                   fmt_int(perdidos), fmt_int(n_registros), perda_pct)
    r <- parar_ou_avisar(msg)
    if (obrigatoria) return(r)
  }

  v
}

# Mantida por compatibilidade com o 05. Delega para ler_vetor() como opcional,
# preservando o contrato de devolver NULL e deixar a flag em NA.
carregar_shp_opcional <- function(path, label = basename(path)) {
  ler_vetor(path, label = label, obrigatoria = FALSE)
}

relacionar_flag_opcional <- function(pma, camada) {
  if (is.null(camada)) return(rep(NA_integer_, nrow(pma)))
  as.integer(terra::is.related(pma, camada, "intersects"))
}

# Flags de sobreposicao/proximidade/embargo produzidas no 05.
# FONTE UNICA (2026-08-25, auditoria F-02): antes o vetor existia so no bloco de
# QA do 05, e a lista de colunas propagadas para a tabela de CFEM era escrita a
# mao -- e ficou desatualizada, levando 8 colunas e nenhuma flag. Incluir uma
# fonte nova agora e acrescentar UMA entrada aqui.
FLAGS_SOBREPOSICAO <- c(
  "TIov", "UCov", "QUIov",              # sobreposicao real (>= 5% da area)
  "TIov10km", "UCov2km", "QUIov10km",   # proximidade (donut) -- alerta, nao sobreposicao
  "inf_MT", "inf_IC",                   # autos de infracao (SEMA-MT, ICMBio)
  "emb_MTa", "emb_MTb", "emb_IB", "emb_IC"  # embargos (SEMA-MT x2, IBAMA, ICMBio)
)

# ==============================================================================
# C) FILTRO POR PALAVRA-CHAVE COM DIAGNÓSTICO (IBAMA/ICMBio/SEMA-MT)
# ==============================================================================

KEYWORDS_GARIMPO <- c(
  "GARIMP",                  # GARIMPO, GARIMPEIRO/A, GARIMPAGEM, GARIMPAR...
  "MINER",                   # MINERAL(IS), MINERARIO/A, MINERIO, MINERACAO, MINERADORA...
  "AUR[IÍ]FER[OA]",          # AURIFERO/A, AURÍFERO/A
  "CASS[IE]TERITA",          # CASSITERITA, CASSETERITA (grafia alternativa)
  "MERC[UÚ]RIO",             # MERCURIO, MERCÚRIO
  "ASSORE",                  # ASSOREAMENTO, ASSOREAR, ASSOREADO/A...
  "LEITO",
  "LAVRA",
  "BARRAGE(M|NS)",           # BARRAGEM, BARRAGENS
  "OURO",
  "DIAMANTE",
  "DIAMANT[IÍ]FER[OA]"       # DIAMANTIFERO/A, DIAMANTÍFERO/A
)
REGEX_GARIMPO <- paste(KEYWORDS_GARIMPO, collapse = "|")

get_attr_table <- function(x) as.data.frame(x)

subset_rows <- function(x, keep) {
  if (inherits(x, "SpatVector")) x[keep, ] else x[keep, , drop = FALSE]
}

aplicar_filtro_palavras_chave <- function(x, campos, regex, label,
                                          export_dir = NULL, top_n = 40,
                                          keep_extra = NULL) {
  attrs  <- get_attr_table(x)
  campos <- intersect(campos, names(attrs))
  if (length(campos) == 0) {
    warning("[", label, "] nenhum dos campos informados existe no objeto — filtro não aplicado.")
    return(x)
  }

  textos <- stats::setNames(
    lapply(campos, function(cc) as.character(attrs[[cc]])),
    campos
  )

  keep_por_campo <- lapply(textos, function(v) !is.na(v) & stringr::str_detect(v, regex))
  keep_regex <- Reduce(`|`, keep_por_campo)
  keep_regex[is.na(keep_regex)] <- FALSE
  keep <- if (is.null(keep_extra)) keep_regex else (keep_regex | keep_extra)

  n_antes  <- nrow(attrs)
  n_depois <- sum(keep)

  message(sprintf("[%s] filtro por palavra-chave | antes: %d | depois: %d (%.1f%% retido)",
                  label, n_antes, n_depois,
                  ifelse(n_antes > 0, 100 * n_depois / n_antes, NA)))
  for (cc in campos) {
    message(sprintf("    campo '%s' sozinho capturaria: %d", cc, sum(keep_por_campo[[cc]])))
  }
  if (!is.null(keep_extra)) {
    message(sprintf("    criterio extra (categoria) sozinho capturaria: %d", sum(keep_extra, na.rm = TRUE)))
  }

  if (!is.null(export_dir)) {
    dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
    for (cc in campos) {
      nao_capturado <- textos[[cc]][!keep_por_campo[[cc]] & !is.na(textos[[cc]]) & textos[[cc]] != ""]
      if (length(nao_capturado) == 0) next
      tab <- sort(table(nao_capturado), decreasing = TRUE)
      tab_top <- utils::head(tab, top_n)
      out <- tibble::tibble(valor = names(tab_top), frequencia = as.integer(tab_top))
      readr::write_csv(out, file.path(export_dir, paste0(label, "_", cc, "_nao_capturado.csv")))
    }
  }

  subset_rows(x, keep)
}

# ==============================================================================
# D) PARSING "INTELIGENTE" DE CFEM COM DIAGNÓSTICO
# ==============================================================================
# guess_delim / choose_decimal_mark / parse_numeric_cols: MESMA lógica de
# inferência do script original. O que muda é que agora cada decisão é
# registrada (score de cada opção, colunas tratadas como numéricas) em um CSV
# de log, para permitir auditoria antes de decidirmos se a heurística precisa
# mudar.

looks_numeric_char <- function(x) {
  all(is.character(x)) &&
    mean(stringr::str_detect(x, "^-?[0-9\\.,]+$") | is.na(x)) > 0.5 &&
    mean(stringr::str_detect(x, "[0-9]") | is.na(x)) > 0.5
}

parse_numeric_cols <- function(df_char, dec_mark = ",", keep_char = character()) {
  loc <- readr::locale(decimal_mark = dec_mark)
  num_cands <- names(df_char)[vapply(df_char, looks_numeric_char, logical(1))]
  num_cands <- setdiff(num_cands, keep_char)
  if (length(num_cands)) {
    df_char <- df_char |>
      dplyr::mutate(dplyr::across(dplyr::all_of(num_cands),
                                  ~ readr::parse_number(dplyr::na_if(.x, "-"), locale = loc)))
  }
  df_char
}

# Igual ao guess_delim() original, mas retorna também os números usados na
# decisão (n de colunas lidas com cada delimitador), não só o vencedor.
guess_delim_diag <- function(path, enc = "ISO-8859-1") {
  try_read <- function(delim) {
    suppressWarnings(try(
      readr::read_delim(path, delim = delim, n_max = 200, locale = readr::locale(encoding = enc),
                        col_types = readr::cols(.default = readr::col_character())),
      silent = TRUE
    ))
  }
  a <- try_read(";")
  b <- try_read(",")
  na <- if (inherits(a, "try-error")) 0 else ncol(a)
  nb <- if (inherits(b, "try-error")) 0 else ncol(b)
  if (na == 0 && nb == 0) stop("Não foi possível inferir o delimitador para: ", path)
  list(delim = if (na >= nb) ";" else ",", n_cols_pontovirgula = na, n_cols_virgula = nb)
}

# Lista padrão de colunas que NUNCA devem virar numéricas mesmo "parecendo"
# (identificadores/códigos onde perder zero à esquerda ou formatação quebraria
# a chave). Mesma lista que existia hardcoded dentro do cfem_smart_read()
# original, antes de virar parâmetro.
CFEM_KEEP_CHAR_PADRAO <- c(
  "CPF_CNPJ", "CPF", "CNPJ",
  "Processo", "AnoDoProcesso",
  "CodigoMunicipio",
  "UnidadeDeMedida", "UF", "Município", "Substância",
  "DataCriacao"
)

# Fases consideradas no universo de correção de peso/preço CFEM (usado em
# 05_integracao_final.R e em diagnósticos ad hoc sobre o mesmo universo).
FASES_CORR_PADRAO <- c(
  "LAVRA GARIMPEIRA", "REQUERIMENTO DE LAVRA GARIMPEIRA",
  "LICENCIAMENTO", "AUTORIZAÇÃO DE PESQUISA"
)

# Igual ao choose_decimal_mark() original, mas retorna os scores de ambas as
# opções e a lista de colunas candidatas, não só a marca vencedora.
#
# cols_forcar: se informado, o score é calculado SOMENTE sobre essas colunas
# (em vez de todas as colunas que "parecem numéricas" pelo regex). Uso: focar
# a decisão de separador decimal nas colunas de valor de fato (Valor,
# ValorRecolhido, QuantidadeComercializada), sem que colunas de ID (Ano, Mês,
# CPF_CNPJ etc.) dilua o score.
choose_decimal_mark_diag <- function(df_char, cols_forcar = NULL) {
  if (!is.null(cols_forcar)) {
    cols_forcar <- intersect(cols_forcar, names(df_char))
    cand <- df_char[cols_forcar]
  } else {
    cand <- df_char |> dplyr::select(dplyr::where(looks_numeric_char))
  }
  if (ncol(cand) == 0) {
    return(list(mark = ".", score_virgula = NA_real_, score_ponto = NA_real_,
               n_cols_candidatas = 0L, colunas = character(0)))
  }

  score <- function(dec) {
    loc <- readr::locale(decimal_mark = dec)
    cand |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ readr::parse_number(.x, locale = loc))) |>
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(is.na(.))), .groups = "drop") |>
      unlist() |> mean()
  }
  s_comma <- score(",")
  s_dot   <- score(".")

  list(mark = if (s_comma <= s_dot) "," else ".",
       score_virgula = s_comma, score_ponto = s_dot,
       n_cols_candidatas = ncol(cand), colunas = names(cand))
}

# Substitui cfem_smart_read() original. Mesma lógica de leitura/conversão;
# adiciona log_dir para gravar (append) uma linha de diagnóstico por arquivo
# processado em <log_dir>/cfem_parsing_log.csv.
#
# keep_char: colunas que nunca viram numéricas na conversão final (default:
# CFEM_KEEP_CHAR_PADRAO, a mesma lista do script original).
# decimal_score_cols: se informado, restringe o CÁLCULO DO SCORE de decimal
# a essas colunas (ver choose_decimal_mark_diag). Não afeta quais colunas
# são de fato convertidas — isso continua sendo controlado por keep_char.
cfem_smart_read <- function(path, enc = "ISO-8859-1",
                            keep_char = CFEM_KEEP_CHAR_PADRAO,
                            decimal_score_cols = NULL,
                            log_dir = NULL) {
  delim_info <- guess_delim_diag(path, enc = enc)
  delim <- delim_info$delim

  df_char <- readr::read_delim(
    path, delim = delim, locale = readr::locale(encoding = enc),
    col_types = readr::cols(.default = readr::col_character()), trim_ws = TRUE
  )
  names(df_char) <- trimws(names(df_char))

  dec_info <- choose_decimal_mark_diag(df_char, cols_forcar = decimal_score_cols)
  df <- parse_numeric_cols(df_char, dec_info$mark, keep_char = intersect(keep_char, names(df_char)))

  attr(df, "cfem_delim") <- delim
  attr(df, "cfem_decimal_mark") <- dec_info$mark

  message(sprintf(
    "[CFEM %s] delim='%s' (candidatos: %d ';' vs %d ',') | decimal='%s' (score_virgula=%s score_ponto=%s) | %d colunas tratadas como numericas",
    basename(path), delim, delim_info$n_cols_pontovirgula, delim_info$n_cols_virgula,
    dec_info$mark,
    ifelse(is.na(dec_info$score_virgula), "NA", sprintf("%.4f", dec_info$score_virgula)),
    ifelse(is.na(dec_info$score_ponto),   "NA", sprintf("%.4f", dec_info$score_ponto)),
    dec_info$n_cols_candidatas
  ))

  if (!is.null(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    log_row <- tibble::tibble(
      timestamp                     = format(Sys.time(), "%Y-%m-%d_%H%M%S"),
      arquivo                       = basename(path),
      delim_escolhido                = delim,
      n_cols_delim_pontovirgula      = delim_info$n_cols_pontovirgula,
      n_cols_delim_virgula           = delim_info$n_cols_virgula,
      decimal_escolhido              = dec_info$mark,
      score_na_virgula                = dec_info$score_virgula,
      score_na_ponto                  = dec_info$score_ponto,
      n_colunas_candidatas_numericas = dec_info$n_cols_candidatas,
      colunas_candidatas              = paste(dec_info$colunas, collapse = "|")
    )
    log_path <- file.path(log_dir, "cfem_parsing_log.csv")
    readr::write_csv(log_row, log_path, append = file.exists(log_path))
  }

  df
}

# ==============================================================================
# E) CHECKPOINTS / CHAVE DE PROCESSO / DOCUMENTO (reuso entre 03, 04, 05)
# ==============================================================================

CKPT_DIR_PADRAO <- here::here("data", "_checkpoints")

save_ckpt <- function(obj, nome, ckpt_dir = CKPT_DIR_PADRAO) {
  dir.create(ckpt_dir, recursive = TRUE, showWarnings = FALSE)
  caminho <- file.path(ckpt_dir, paste0(nome, ".rds"))
  if (inherits(obj, "SpatVector")) {
    saveRDS(terra::wrap(obj), caminho)
  } else {
    saveRDS(obj, caminho)
  }
  invisible(caminho)
}

load_ckpt <- function(nome, ckpt_dir = CKPT_DIR_PADRAO) {
  caminho <- file.path(ckpt_dir, paste0(nome, ".rds"))
  if (!file.exists(caminho)) stop("Checkpoint nao encontrado: ", nome, ".rds")
  obj <- readRDS(caminho)
  if (inherits(obj, "PackedSpatVector")) terra::vect(obj) else obj
}

# Remove o ponto do DSProcesso (microdados) p/ casar com 'processo' (SIGMINE/SCM).
limpar_dsprocesso <- function(x) stringr::str_replace_all(as.character(x), "\\.", "")

# Padroniza CPF/CNPJ (com ou sem mascara) para um formato unico de exibicao.
#   CNPJ (14 digitos)      -> 11.111.111/1111-11
#   CPF  (11 digitos)      -> 111.111.111-11
#   CPF mascarado ANM (6)  -> ***.111.111-**
#   vazio ("-", "", NA)    -> NA
#
# DECISOES 2026-08 (auditoria F-07):
#  (a) O ramo de 11 digitos vinha DEPOIS do ramo generico "nd > 6 & nd < 14" e
#      era codigo inalcancavel: todo CPF virava CNPJ com zeros a esquerda.
#      Ordem corrigida -- os ramos agora sao por comprimento exato.
#  (b) O ramo generico foi REMOVIDO, nao reordenado. Ele completava qualquer
#      documento de 7 a 13 digitos para 14, o que fabrica CNPJ inexistente a
#      partir de CPF que perdeu zero a esquerda em conversao numerica. Num
#      produto investigativo, inventar documento pode ligar duas entidades sem
#      relacao. Comprimento fora do esperado agora vira NA e e contado no log.
#      Volume observado nos arquivos do ICMBio: ~1.400 registros de 7 a 13
#      digitos, contra ~43.000 CPFs de 11 digitos.
#
# NAO mascara documento cru -- isso e responsabilidade da INGESTAO (etapa 02),
# antes de qualquer persistencia. Esta funcao e apenas formatadora.
padroniza_doc <- function(x, label = NULL) {
  x  <- trimws(as.character(x))
  d  <- gsub("\\D", "", x)
  nd <- nchar(d)

  out <- dplyr::case_when(
    is.na(x) | x %in% c("", "-") ~ NA_character_,
    grepl("\\*", x) & nd == 6 ~ sprintf("***.%s.%s-**", substr(d, 1, 3), substr(d, 4, 6)),
    nd == 11 ~ sprintf("%s.%s.%s-%s",
                       substr(d, 1, 3), substr(d, 4, 6), substr(d, 7, 9), substr(d, 10, 11)),
    nd == 14 ~ sprintf("%s.%s.%s/%s-%s",
                       substr(d, 1, 2), substr(d, 3, 5), substr(d, 6, 8),
                       substr(d, 9, 12), substr(d, 13, 14)),
    TRUE ~ NA_character_
  )

  descartados <- is.na(out) & !is.na(x) & !(x %in% c("", "-")) & nd > 0
  if (any(descartados)) {
    tab <- table(nd[descartados])
    message(sprintf(
      "[padroniza_doc%s] %d documento(s) com comprimento inesperado -> NA | por n de digitos: %s",
      if (is.null(label)) "" else paste0(" | ", label),
      sum(descartados),
      paste(sprintf("%s dig=%d", names(tab), as.integer(tab)), collapse = ", ")
    ))
  }
  out
}

# Mascara documento na INGESTAO (decisao 2026-08, auditoria F-07).
# Os shapefiles do ICMBio trazem CPF de 11 digitos sem mascara -- ~33k em autos
# de infracao e ~10k em embargos. A ANM ja entrega pessoa fisica mascarada; as
# fontes ambientais nao. Como a incorporacao do ICMBio (F-01) foi decidida por
# sobreposicao ESPACIAL, o documento nao e usado como chave de cruzamento, e
# portanto nao ha razao para persistir o valor cru.
#
# CPF -> mesmo formato mascarado da ANM (***.111.111-**), preservando os 6
# digitos centrais, que e o que a propria ANM publica. CNPJ (pessoa juridica)
# passa intacto: nao e dado pessoal.
mascarar_doc_pessoa_fisica <- function(x) {
  x  <- trimws(as.character(x))
  d  <- gsub("\\D", "", x)
  nd <- nchar(d)
  dplyr::if_else(
    nd == 11 & !grepl("\\*", x),
    sprintf("***.%s.%s-**", substr(d, 4, 6), substr(d, 7, 9)),
    x
  )
}

# ==============================================================================
# F) SCHEMA OFICIAL DOS MICRODADOS SCM (a partir do .ods de metadados)
# ==============================================================================
# Le a aba "Recursos" do metadados-microdados-scm.ods (formato repetido por
# tabela: Titulo / Formato / Encoding / Descricao / Identificador / Atributos
# + linhas Nome | Descricao | Tipo de dado | Formato) e devolve, por arquivo
# .txt, um readr::cols() pronto para col_types em read_delim(). Elimina a
# necessidade de descobrir tipo por regex/heuristica (looks_numeric_char) —
# o schema vem direto do dicionario de dados publicado pela ANM.
#
# Numérico -> col_double() | Alfanumérico -> col_character()
# Data / Data e Hora -> col_character() (NÃO convertido aqui: o formato de
# data — DD/MM/AAAA vs AAAA-MM-DD etc. — não vem declarado no dicionário, e
# assumir errado silenciosamente vira NA na coluna inteira. Ver
# checar_formato_datas() para decidir o formato com uma amostra real do dado
# antes de converter.)
ler_schema_microdados <- function(path_ods, sheet = "Recursos") {
  if (!requireNamespace("readODS", quietly = TRUE)) {
    stop("Pacote 'readODS' necessario para ler o schema oficial (.ods). Instale com install.packages('readODS').")
  }

  raw <- readODS::read_ods(path_ods, sheet = sheet, col_names = FALSE)
  names(raw) <- paste0("c", seq_along(names(raw)) - 1)

  schema <- list()
  arquivo_atual <- NA_character_
  em_atributos  <- FALSE

  for (i in seq_len(nrow(raw))) {
    rotulo <- raw$c1[i]
    valor  <- raw$c2[i]
    tipo   <- if ("c3" %in% names(raw)) raw$c3[i] else NA_character_

    if (!is.na(rotulo) && rotulo == "Título") {
      arquivo_atual <- valor
      schema[[arquivo_atual]] <- list(colunas = character(0), tipos = character(0))
      em_atributos <- FALSE
    } else if (!is.na(rotulo) && rotulo == "Atributos") {
      em_atributos <- TRUE
    } else if (!is.na(rotulo) && rotulo == "Nome") {
      next
    } else if (em_atributos && !is.na(rotulo) && !is.na(arquivo_atual)) {
      schema[[arquivo_atual]]$colunas <- c(schema[[arquivo_atual]]$colunas, rotulo)
      schema[[arquivo_atual]]$tipos   <- c(schema[[arquivo_atual]]$tipos, tipo)
    }
  }

  purrr::map(schema, \(tab) {
    tipos_readr <- ifelse(tab$tipos == "Numérico", "double", "character")
    spec_list <- stats::setNames(
      lapply(tipos_readr, function(tp) if (tp == "double") readr::col_double() else readr::col_character()),
      tab$colunas
    )
    do.call(readr::cols, c(spec_list, list(.default = readr::col_character())))
  })
}

# Amostra de valores brutos por coluna de data (colunas que comecam com "DT",
# convenção do dicionário oficial), para decidir o formato de parse com dado
# real em vez de assumir e arriscar NA silencioso em toda a coluna.
checar_formato_datas <- function(df, arquivo, n_amostra = 8) {
  cols_data <- names(df)[stringr::str_starts(names(df), "DT")]
  if (length(cols_data) == 0) return(tibble::tibble())

  purrr::map_dfr(cols_data, \(cc) {
    vals <- df[[cc]]
    vals <- vals[!is.na(vals) & vals != ""]
    tibble::tibble(
      arquivo = arquivo,
      coluna  = cc,
      n_nao_vazios = length(vals),
      exemplos = paste(utils::head(unique(vals), n_amostra), collapse = " | ")
    )
  })
}

# Moda de um vetor (ignora NA).
get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Classifica cada declaracao de CFEM em um "foco" de investigacao (decisao
# 2026-07-20): OURO (qualquer substancia do grupo ouro) / CASSITERITA (pura)
# / OUTROS (todo o resto -- ilmenita, columbita, cobre, prata, ferro etc.).
# Motivo: processos que misturam cassiterita/ouro com outra substancia (ex:
# um garimpo registrado como CASSITERITA no PMA que tambem declara ILMENITA)
# contaminavam os totais agregados por processo (arr_kg_T etc.) -- uma
# substancia sem nenhuma validacao/correcao podia ter erro de varias ordens
# de magnitude e inflar o total "da cassiterita" sem ninguem perceber.
# Usada em 05_integracao_final.R (arr_corr_unique) e 07_proc_shiny_dossie.R
# (resumo_por_processo) para agregar peso/valor SEPARADAMENTE por foco, sem
# descartar nenhuma declaracao.
categorizar_foco <- function(SUBSarr, SUBSarrSIM) {
  dplyr::case_when(
    SUBSarrSIM == "OURO"        ~ "ouro",
    SUBSarr    == "CASSITERITA" ~ "cassiterita",
    TRUE                        ~ "outros"
  )
}

# Minerais estrategicos monitorados (14 grupos) + classificar_grupo(): mapeia
# a substancia declarada (SUBSarr/SUBS) pro grupo. Usado no 05 (classifica
# SUBSarrSIM antes da correcao de peso) e no 06 (classifica SUBSpmaGRP do
# poligono, e SUBSarrSIM chega pronto no checkpoint 05_cfem_bruto).
target_minerals_list <- list(
  ouro         = c("OURO","MINÉRIO DE OURO","OURO NATIVO","OURO PIGMENTO","ALUVIÃO AURÍFERO"),
  diamante     = c("DIAMANTE","DIAMANTE INDUSTRIAL","CASCALHO DIAMANTÍFERO"),
  litio        = c("LÍTIO","MINÉRIO DE LÍTIO","ESPODUMÊNIO","LEPIDOLITA","PETALITA","AMBLIGONITA","POLUCITA","KUNZITA"),
  niobio       = c("NIÓBIO","MINÉRIO DE NIÓBIO","COLUMBITA","PIROCLORO"),
  tantalo      = c("TÂNTALO","MINÉRIO DE TÂNTALO","TANTALITA","TANTALITA-COLUMBITA"),
  estanho      = c("ESTANHO","MINÉRIO DE ESTANHO","CASSITERITA","ALUVIÃO ESTANÍFERO"),
  tungstenio   = c("TUNGSTÊNIO","MINÉRIO DE TUNGSTÊNIO","WOLFRAMITA","SCHEELITA"),
  titanio      = c("TITÂNIO","MINÉRIO DE TITÂNIO","ILMENITA","RUTILO","TITANITA"),
  terras_raras = c("TERRAS RARAS","MONAZITA","MINÉRIO DE CÉRIO"),
  cobalto      = c("MINÉRIO DE COBALTO"),
  grafite      = c("GRAFITA"),
  niquel       = c("NÍQUEL","MINÉRIO DE NÍQUEL","SILICATOS DE NÍQUEL"),
  vanadio      = c("VANÁDIO","MINÉRIO DE VANÁDIO"),
  molibdenio   = c("MOLIBDÊNIO","MINÉRIO DE MOLIBDÊNIO","MOLIBDENITA")
)
target_minerals <- unique(unlist(target_minerals_list))

classificar_grupo <- function(subs) {
  dplyr::case_when(
    subs %in% target_minerals_list$ouro         ~ "OURO",
    subs %in% target_minerals_list$diamante     ~ "DIAMANTE",
    subs %in% target_minerals_list$litio        ~ "LÍTIO",
    subs %in% target_minerals_list$niobio       ~ "NIÓBIO",
    subs %in% target_minerals_list$tantalo      ~ "TÂNTALO",
    subs %in% target_minerals_list$estanho      ~ "ESTANHO",
    subs %in% target_minerals_list$tungstenio   ~ "TUNGSTÊNIO",
    subs %in% target_minerals_list$titanio      ~ "TITÂNIO",
    subs %in% target_minerals_list$terras_raras ~ "TERRAS RARAS",
    subs %in% target_minerals_list$cobalto      ~ "COBALTO",
    subs %in% target_minerals_list$grafite      ~ "GRAFITE",
    subs %in% target_minerals_list$niquel       ~ "NÍQUEL",
    subs %in% target_minerals_list$vanadio      ~ "VANÁDIO",
    subs %in% target_minerals_list$molibdenio   ~ "MOLIBDÊNIO",
    TRUE                                         ~ "OUTROS"
  )
}

# unir_intervalos() -- sincronizada aqui (fonte da verdade: R/graficos_historico.R)
# pra construir_intervalos_gu() poder ser usada tanto no 06_serie_temporal.R
# (que so da source em utils.R) quanto no graficos_historico.R.
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

# GU (Guia de Utilizacao) para AUT PESQ -- decisao 2026-07-21 (achado real:
# processo com CFEM declarado em Autorizacao de Pesquisa era sempre tratado
# como "em tramitacao/pesquisa", sem considerar que a GU autoriza legalmente
# a comercializacao da substancia durante a pesquisa).
#
# Constroi janelas de validade de GU por processo a partir dos eventos
# AUTORIZADA/APROVADO (idevento com dsevento contendo esses termos, ja
# filtrado por quem chama para tipo_proc == "AUT PESQ" e categ_gu == TRUE):
#   - data-fim: extraida via regex "Validade:DD/MM/AAAA" do campo de
#     publicacao (dspublicacaodou); se nao casar, cai no fallback de duracao
#     explicita no nome do evento (idevento 2752/2753/2754 = 1/2/3 anos);
#     se nenhum dos dois for determinavel, a autorizacao e DESCARTADA
#     (decisao 2026-07-21: conservador -- nao assume "apto" sem uma janela
#     concreta).
#   - CANCELADA/SUSPENSA truncam a janela no primeiro evento desse tipo
#     posterior ao inicio da autorizacao (mesmo raciocinio do art. 211/213
#     para indeferimento de renovacao de PLG).
#
# eventos_gu_aut_pesq: tibble com colunas processo, dtevento (Date),
# idevento (chr), dsevento (chr), publicacao (chr, = dspublicacaodou) --
# ja filtrado para tipo_proc == "AUT PESQ" e categ_gu == TRUE por quem chama.
construir_intervalos_gu <- function(eventos_gu_aut_pesq) {
  vazio <- tibble::tibble(processo = character(), xmin = as.Date(character()), xmax = as.Date(character()))
  if (is.null(eventos_gu_aut_pesq) || nrow(eventos_gu_aut_pesq) == 0) return(vazio)

  autorizacoes <- eventos_gu_aut_pesq |>
    dplyr::filter(stringr::str_detect(dsevento, "AUTORIZADA|APROVADO")) |>
    dplyr::mutate(
      dt_validade_txt = stringr::str_match(publicacao, "[Vv]alidade:?\\s*(\\d{2}/\\d{2}/\\d{2,4})")[, 2],
      # as.Date(tryFormats=...) tenta "%d/%m/%Y" (4 digitos) e cai pra
      # "%d/%m/%y" (2 digitos) quando o primeiro nao casa -- cobre os dois
      # formatos de ano que aparecem no texto de publicacao.
      dt_validade_parse = suppressWarnings(as.Date(dt_validade_txt, tryFormats = c("%d/%m/%Y", "%d/%m/%y"))),
      # fallback de duracao explicita no nome do evento (1/2/3 anos) --
      # soma anos reconstruindo a string da data (evita depender de lubridate,
      # que nao e usado em nenhum outro lugar do projeto).
      dt_fim_duracao = dplyr::case_when(
        idevento == "2752" ~ as.Date(paste0(as.integer(format(dtevento, "%Y")) + 1, format(dtevento, "-%m-%d"))),
        idevento == "2753" ~ as.Date(paste0(as.integer(format(dtevento, "%Y")) + 2, format(dtevento, "-%m-%d"))),
        idevento == "2754" ~ as.Date(paste0(as.integer(format(dtevento, "%Y")) + 3, format(dtevento, "-%m-%d"))),
        TRUE                ~ as.Date(NA)
      ),
      dt_fim = dplyr::coalesce(dt_validade_parse, dt_fim_duracao)
    ) |>
    dplyr::filter(!is.na(dt_fim)) |>
    dplyr::transmute(processo, dt_inicio = dtevento, dt_fim)

  if (nrow(autorizacoes) == 0) return(vazio)

  fechamentos <- eventos_gu_aut_pesq |>
    dplyr::filter(stringr::str_detect(dsevento, "CANCELADA|SUSPENSA")) |>
    dplyr::transmute(processo, dt_fechamento = dtevento)

  autorizacoes_truncadas <- autorizacoes |>
    dplyr::left_join(fechamentos, by = "processo") |>
    dplyr::group_by(processo, dt_inicio, dt_fim) |>
    dplyr::summarise(
      dt_fim = {
        candidatos <- dt_fechamento[!is.na(dt_fechamento) & dt_fechamento > dt_inicio & dt_fechamento < dt_fim]
        if (length(candidatos) > 0) min(candidatos) else dt_fim[1]
      },
      .groups = "drop"
    )

  autorizacoes_truncadas |>
    dplyr::group_by(processo) |>
    dplyr::group_modify(~ unir_intervalos(.x$dt_inicio, .x$dt_fim)) |>
    dplyr::ungroup()
}

# ==============================================================================
# G) HISTORICO DE PROCESSO — GRAFICO GENERICO (vencimento, suspensao/retomada,
#    anulacao, com ou sem CFEM)
# ==============================================================================
# Generalizacao do que estava hardcoded em casos/coogam (checks/08_historico_...
# .R) para qualquer processo da base, nao so COOGAM/Tapajos. Principios:
#   - NENHUMA agregacao: 1 declaracao de CFEM = 1 ponto; 1 evento = 1 marcacao.
#     Datas de abertura/renovacao/vencimento/publicacao sao unicas, nunca
#     resumidas por mes ou por processo.
#   - Funciona SEM CFEM: processos sem nenhuma declaracao ainda mostram a
#     timeline de eventos administrativos (o historico e o produto; o CFEM e
#     so uma camada opcional sobreposta).
#   - Fontes (todas ja vem do 06_serie_temporal.R, sem recalcular nada):
#       situacao_documental.parquet          -> publicacao/vencimento de titulo
#       protocolos_licenca_ambiental.parquet -> protocolo de licenca ambiental
#       eventos_classificados.parquet        -> abertura/renovacao (MUDA_FASE),
#                                                encerramento/anulacao (FECHA),
#                                                suspensao (SUSPENDE), retomada
#                                                (RETOMA) — evento a evento
#       CFEM (checkpoint 05_cfem_final ou equivalente) -> declaracao a declaracao

# AS FUNCOES em si vivem em R/graficos_historico.R, NAO aqui, de proposito:
# esse arquivo e copiado (07_proc_shiny_dossie.R) direto para dentro de
# shiny_dashboard/ para o deploy no droplet, e por isso nao pode ter nenhuma
# linha de topo que dependa de here::here() (como o CKPT_DIR_PADRAO la em
# cima) ou de qualquer coisa fora de dplyr/ggplot2/tibble. Mantendo as
# definicoes so em graficos_historico.R evita ter a mesma funcao duplicada e
# desalinhada em dois arquivos.
source(here::here("R", "graficos_historico.R"))