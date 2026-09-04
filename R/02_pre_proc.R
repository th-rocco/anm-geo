###############################################################################
# 02_pre_proc.R - Processing, cleaning, and consolidation
###############################################################################

# Setup & Configuration -----------------------------------------------------
rm(list = ls(all.names = TRUE))
options(scipen = 999)

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(stringi)
  library(here)
})

Sys.unsetenv("PROJ_LIB")
Sys.unsetenv("GDAL_DATA")

source(here::here("R", "utils.R"))

# Paths and parameters
ROOT         <- here::here()
RAW_DIR      <- here::here("data", "raw_data")
PRE_PROC_DIR <- here::here("data", "pre_proc_data")
TMP_DIR      <- here::here("data", "tmp")
QA_DIR       <- here::here("data", "_qa", "02_pre_proc")

dir.create(PRE_PROC_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TMP_DIR,      recursive = TRUE, showWarnings = FALSE)
dir.create(QA_DIR,       recursive = TRUE, showWarnings = FALSE)

REGEX <- REGEX_GARIMPO  # centralizado em R/utils.R (constante KEYWORDS_GARIMPO)

etl_log <- list()

# ANM SCM -----------------------------------------------------
safe_step("ANM SCM consolidation", {
  scm_dir <- file.path(RAW_DIR, "anm_scm")
  scm_files <- list.files(scm_dir, pattern = "\\.csv$", full.names = TRUE)

  if (length(scm_files) == 0) stop("No SCM CSV files found in: ", scm_dir)

  cadastro_mineiro <- purrr::map_df(scm_files, \(f) {
    doc_type <- stringr::str_remove(basename(f), "\\.csv$")

    raw <- readr::read_file_raw(f)
    txt <- stringi::stri_encode(raw, from = "ISO-8859-1", to = "UTF-8")

    df <- readr::read_delim(
      txt,
      delim = ",",
      col_types = readr::cols(.default = "c"),
      show_col_types = FALSE
    )

    names(df) <- stringr::str_replace_all(names(df), "CPF CNPJ do titular", "CPF/CNPJ do titular")
    names(df) <- stringr::str_replace_all(names(df), "^Substancia$", "Substância(s)")

    df |> dplyr::mutate(tipo_documento = doc_type)
  })

  readr::write_excel_csv(
    cadastro_mineiro,
    file.path(PRE_PROC_DIR, "cadastro_mineiro.csv")
  )

  rm(cadastro_mineiro)
  gc()
})

safe_step("CFEM smart parsing (ANM arrecadacao)", {
  cfem_dir <- file.path(RAW_DIR, "anm_arrecadacao")
  files <- c("CFEM_Arrecadacao.csv", "CFEM_Autuacao.csv", "CFEM_Distribuicao.csv", "Tah.csv")
  paths <- file.path(cfem_dir, files)
  names(paths) <- files

  cfem_cols_valor <- list(
    "CFEM_Arrecadacao.csv"  = c("QuantidadeComercializada", "ValorRecolhido"),
    "CFEM_Autuacao.csv"     = c("Valor"),
    "CFEM_Distribuicao.csv" = c("Valor")
  )

  if (!dir.exists(cfem_dir)) stop("Missing directory: ", cfem_dir)

  purrr::iwalk(paths, \(p, nm) {
    if (!file.exists(p)) {
      message("CFEM/Tah missing: ", nm, " | Skipping.")
      return(invisible(NULL))
    }

    df <- cfem_smart_read(
      p, 
      enc = "ISO-8859-1",
      decimal_score_cols = cfem_cols_valor[[nm]],
      log_dir = QA_DIR
    )

    out <- file.path(PRE_PROC_DIR, paste0(tools::file_path_sans_ext(nm), ".csv"))
    readr::write_csv(df, out)

    message("Saved: ", basename(out))
  })
})

# SCM microdados & SIGMINE PMA ----------------------------------------------------
safe_step("ANM SCM microdados (unzip)", {
  # CORRIGIDO 2026-08-25: este passo reportava SUCCESS mesmo quando o zip nao
  # descompactava -- safe_unzip devolvia FALSE, imprimia aviso e o bloco seguia.
  # Mesma classe de falha silenciosa dos achados F-01/F-02/F-03 da auditoria, e
  # das mais caras: os microdados alimentam o 04 e o 07 (serie temporal
  # inteira). O 03 roda normalmente e a quebra so aparece duas etapas depois,
  # sem ligacao obvia com a causa. Agora falha aqui.
  micro_dir <- file.path(RAW_DIR, "anm_microdados")
  zip_path  <- file.path(micro_dir, "microdados-scm.zip")

  if (!file.exists(zip_path)) {
    stop("microdados-scm.zip nao encontrado em ", micro_dir,
         " -- rodar 01_download.R. Sem ele o 04 e o 07 nao rodam.")
  }

  ok <- safe_unzip(zip_path, micro_dir)

  # recursive = TRUE: o zip cria a subpasta microdados-scm/ e os .txt ficam
  # dentro dela. Sem isso a checagem dava falso negativo (2026-08-25).
  txt_extraidos <- list.files(micro_dir, pattern = "\\.txt$", recursive = TRUE)
  if (!ok || length(txt_extraidos) == 0) {
    stop("microdados-scm.zip nao pode ser descompactado (tamanho: ",
         fmt_int(file.info(zip_path)$size), " bytes | .txt extraidos: ",
         length(txt_extraidos), "). Conferir integridade do download. ",
         "Sem os microdados o 04 e o 07 nao rodam.")
  }
  message("Microdados SCM extracted: ", length(txt_extraidos), " arquivo(s) .txt.")
})

safe_step("SIGMINE PMA extraction", {
  # CORRIGIDO 2026-08-25: havia return() aqui dentro. safe_step avalia uma
  # EXPRESSAO, nao um corpo de funcao, entao return() gera "no function to
  # return from" em vez de pular o passo. Alem disso o PMA e a camada central do
  # projeto -- ausencia dele nao e caso de pular, e de parar.
  zip_path <- file.path(RAW_DIR, "anm_espacial", "BRASIL.zip")
  if (!file.exists(zip_path)) {
    stop("BRASIL.zip nao encontrado em ", dirname(zip_path),
         " -- rodar 01_download.R. E a camada de processos minerarios (PMA).")
  }

  exdir <- file.path(TMP_DIR, "pma_extraction")
  if (!safe_unzip(zip_path, exdir)) stop("Failed to unzip: ", zip_path)

  shp_pma <- first_match(exdir, "\\.shp$")
  if (is.na(shp_pma)) stop("No shapefile found in BRASIL.zip extraction.")

  pma <- ler_vetor(shp_pma, label = "PMA") |> clean_geometry(label = "SIGMINE PMA")

  terra::writeVector(pma, file.path(PRE_PROC_DIR, "sigmine_pma.shp"), overwrite = TRUE)
})

# FUNAI TI / MMA CNUC / INCRA ------------------------------
safe_step("FUNAI Indigenous Lands (TI)", {
  zip_path <- file.path(RAW_DIR, "geo_territorios", "tis_poligonais.zip")
  exdir    <- file.path(TMP_DIR, "funai_ti")

  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)

  shp_path <- first_match(exdir, "tis_poligonais.*\\.shp$")
  if (is.na(shp_path)) stop("No FUNAI TI shapefile found after unzip.")

  ti <- ler_vetor(shp_path, label = "TI") |> clean_geometry(label = "FUNAI TI")

  if ("terrai_nom" %in% names(ti)) {
    ti$terrai_nom <- stringi::stri_encode(ti$terrai_nom, from = "Windows-1252", to = "UTF-8")
    ti$terrai_nom <- toupper(ti$terrai_nom)
  }

  cols_keep <- c("modalidade", "fase_ti", "terrai_nom", "etnia_nome")
  cols_keep <- cols_keep[cols_keep %in% names(ti)]

  ti <- ti[, cols_keep]

  terra::writeVector(ti, file.path(PRE_PROC_DIR, "terras_indigenas.shp"), overwrite = TRUE)
})

safe_step("MMA CNUC protected areas (UC)", {
  # INSUMO MANUAL (2026-08-25): o CNUC nao e baixado pelo 01 nesta configuracao
  # (ver BAIXAR_TERRITORIOS). O zip usado e o que esta em geo_territorios/.
  #
  # CORRIGIDO 2026-08: o caminho estava fixo em "shp_cnuc.zip" e o 01 baixaria
  # "shp_cnuc_2025_08.zip" (nome com data). Passa a casar por padrao. Se houver
  # mais de uma versao na pasta, avisa qual foi usada -- pegar a "mais recente"
  # por ordem alfabetica em silencio seria outra falha muda.
  cnuc_zips <- list.files(file.path(RAW_DIR, "geo_territorios"),
                          pattern = "^shp_cnuc.*\\.zip$", full.names = TRUE)
  if (length(cnuc_zips) == 0) {
    stop("Nenhum shp_cnuc*.zip em geo_territorios -- e INSUMO MANUAL, ver leia-me.")
  }
  if (length(cnuc_zips) > 1) {
    message("[UC] ", length(cnuc_zips), " versoes do CNUC na pasta; usando: ",
            basename(sort(cnuc_zips, decreasing = TRUE)[1]))
  }
  zip_path <- sort(cnuc_zips, decreasing = TRUE)[1]
  exdir    <- file.path(TMP_DIR, "mma_cnuc")

  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)

  shp_path <- first_match(exdir, "\\.shp$")
  if (is.na(shp_path)) stop("No CNUC shapefile found after unzip.")

  uc <- ler_vetor(shp_path, label = "UC") |> clean_geometry(label = "MMA CNUC")

  # --- SNUC category mapping --------------------------------------------------
  map_snuc <- c(
    "Área de Proteção Ambiental"               = "APA",
    "Área de Relevante Interesse Ecológico"    = "ARIE",
    "Estação Ecológica"                        = "ESEC",
    "Floresta"                                 = "FLO",
    "Monumento Natural"                        = "MONA",
    "Parque"                                   = "PARQUE",
    "Refúgio de Vida Silvestre"                = "REVIS",
    "Reserva Biológica"                        = "REBIO",
    "Reserva de Desenvolvimento Sustentável"   = "RDS",
    "Reserva de Fauna"                         = "REFAU",
    "Reserva Extrativista"                     = "RESEX",
    "Reserva Particular do Patrimônio Natural" = "RPPN"
  )

  if (!("categoria" %in% names(uc))) {
    warning("Field 'categoria' not found. Skipping sigla_snuc mapping.")
    uc$sigla_snuc <- "OUTRO"
  } else {
    cat_clean <- trimws(as.character(uc$categoria))
    uc$sigla_snuc <- "OUTRO"

    idx <- match(cat_clean, names(map_snuc))
    hit <- !is.na(idx)
    uc$sigla_snuc[hit] <- unname(map_snuc[idx[hit]])
  }

  # cd_cnuc PRESERVADO (2026-08-25): e a chave de cruzamento com os autos de
  # infracao do ICMBio, que trazem o mesmo codigo no campo 'cnuc', no mesmo
  # formato (ex: "0000.00.0118"). Sem ele, a validacao cruzada so poderia ser
  # por nome -- e os nomes NAO sao comparaveis: o ICMBio abrevia
  # ("FLONA do Jamari") e o CNUC escreve por extenso ("FLORESTA NACIONAL DO
  # JAMARI"). Comparar texto dava 0 acertos em 347 processos, o que parecia
  # achado e era artefato de vocabulario.
  cols_keep <- c("cd_cnuc", "nome_uc", "pl_manejo", "grupo", "categoria",
                 "org_gestor", "sigla_snuc")
  cols_keep <- cols_keep[cols_keep %in% names(uc)]

  if (!"cd_cnuc" %in% names(uc)) {
    warning("[UC] coluna cd_cnuc ausente -- a validacao cruzada com os autos do ",
            "ICMBio (05) ficara indisponivel. Conferir a versao do CNUC.")
  }

  uc <- uc[, cols_keep]

  terra::writeVector(uc, file.path(PRE_PROC_DIR, "unidades_conservacao.shp"), overwrite = TRUE)
})

safe_step("INCRA Quilombolas", {
  territorios_dir <- file.path(RAW_DIR, "geo_territorios")
  exdir <- file.path(TMP_DIR, "incra_quilombolas")
  zip_candidates <- list.files(
    territorios_dir, pattern = "quilombola", ignore.case = TRUE, full.names = TRUE
  )

  shp_path <- NA_character_

  # 1) tenta o ZIP
  if (length(zip_candidates) > 0) {
    if (safe_unzip(zip_candidates[1], exdir)) {
      shp_path <- first_match(exdir, "\\.shp$")
    }
  }

  # 2) fallback: shapefile ja extraido manualmente em geo_territorios
  if (is.na(shp_path)) {
    shp_local <- list.files(
      territorios_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE
    )
    if (length(shp_local) > 0) shp_path <- shp_local[1]
  }

  # 3) se ainda nao achou, pula de verdade
  if (is.na(shp_path)) {
    message("INCRA: nenhum .shp encontrado (ZIP nem local) em ", territorios_dir, ". Skipping.")
  } else {
    message("Quilombolas source: ", shp_path)
    qui <- ler_vetor(shp_path, label = "QUILOMBOLA") |> clean_geometry(label = "INCRA Quilombolas")
    terra::writeVector(qui, file.path(PRE_PROC_DIR, "quilombolas.shp"), overwrite = TRUE)
    message("Quilombolas saved.")
  }
})

# IBAMA / ICMBio / SEMA-MT ----------------------
safe_step("IBAMA embargos (shp)", {
  zip_path <- file.path(RAW_DIR, "geo_ibama", "adm_embargos_ibama_a.zip")
  exdir    <- file.path(TMP_DIR, "ibama_embargos")

  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)

  shp_path <- first_match(exdir, "embargo.*\\.shp$")
  if (is.na(shp_path)) stop("No embargo shapefile found after unzip.")

  EMBib <- ler_vetor(shp_path, label = "ibama_embargos", obrigatoria = FALSE)
  if (is.null(EMBib)) stop("IBAMA embargos indisponivel -- ver aviso acima.")
  EMBib <- clean_geometry(EMBib, label = "IBAMA embargos")
  EMBib <- EMBib[, c("nome_embar", "cpf_cnpj_e", "ordem_fisc", "num_proces",
                     "des_tad", "des_infrac", "dat_embarg", "dat_ult_al")]

  EMBib$des_tad    <- to_upper_utf8(EMBib$des_tad)
  EMBib$des_infrac <- to_upper_utf8(EMBib$des_infrac)

  # LGPD (F-07): mascara na ingestao, igual as demais fontes ambientais.
  EMBib$cpf_cnpj_e <- mascarar_doc_pessoa_fisica(EMBib$cpf_cnpj_e)

  EMBib <- aplicar_filtro_palavras_chave(
    EMBib, campos = c("des_tad", "des_infrac"), regex = REGEX,
    label = "ibama_embargos", export_dir = QA_DIR
  )

  terra::writeVector(EMBib, file.path(PRE_PROC_DIR, "ibama_embargos.shp"), overwrite = TRUE)
})

safe_step("IBAMA infractions (shp)", {

  zip_path <- file.path(RAW_DIR, "geo_ibama", "ibama_autos_de_infracao_p.zip")
  if (!file.exists(zip_path)) stop("Missing zip: ", zip_path)

  # PAUSADO (decisao 2026-08): inspecao no QGIS mostrou geometria "estourando"
  # para fora do Brasil -- muitos registros com NUM_LONGIT/NUM_LATITU = 0
})

safe_step("ICMBio embargos (shp)", {
  # REVISADO 2026-08 (auditoria F-05). A auditoria reportou que o ICMBio teria
  # renomeado desc_infra/desc_sanc e derrubado o passo inteiro. NAO CONFIRMADO:
  # na versao 2026-08-04 as sete colunas originais existem todas. A checagem
  # explicita de colunas abaixo faz o descasamento aparecer como erro nomeado,
  # em vez de falha generica de subset, se a fonte mudar de fato.
  #
  # Inventario (2026-08-04): 14.620 registros = 14.364 poligonos + 256 nulos.
  # Filtro de garimpo sobre desc_infra+desc_sanc: 1.336 registros, 1.317 com
  # geometria, 1.033 em UF da Amazonia Legal.
  zip_path <- file.path(RAW_DIR, "geo_icmbio", "embargos_icmbio.zip")
  exdir    <- file.path(TMP_DIR, "icmbio_embargos")

  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)

  shp_path <- first_match(exdir, "embargos_icmbio\\.shp$")
  if (is.na(shp_path)) stop("No ICMBio embargos shapefile found after unzip.")

  EMBic <- ler_vetor(shp_path, label = "icmbio_embargos", obrigatoria = FALSE)
  if (is.null(EMBic)) stop("ICMBio embargos indisponivel -- ver aviso acima.")
  EMBic <- clean_geometry(EMBic, label = "ICMBio embargos")

  # tipo_infra acrescentado ao conjunto: o bloco de autos de infracao do MESMO
  # orgao ja filtrava por ele, e o campo existe aqui tambem. Padronizado.
  cols_EMBic <- c("cpf_cnpj", "autuado", "desc_infra", "desc_sanc", "tipo_infra",
                  "processo", "data", "ano")
  faltando <- setdiff(cols_EMBic, names(EMBic))
  if (length(faltando) > 0) {
    stop("[icmbio_embargos] colunas ausentes no shapefile: ",
         paste(faltando, collapse = ", "),
         " | presentes: ", paste(names(EMBic), collapse = ", "))
  }
  EMBic <- EMBic[, cols_EMBic]

  EMBic$desc_infra <- to_upper_utf8(iconv(EMBic$desc_infra, "UTF-8", "UTF-8", sub = ""))
  EMBic$desc_sanc  <- to_upper_utf8(iconv(EMBic$desc_sanc,  "UTF-8", "UTF-8", sub = ""))
  EMBic$tipo_infra <- to_upper_utf8(iconv(EMBic$tipo_infra, "UTF-8", "UTF-8", sub = ""))

  # LGPD (F-07): ~10 mil CPF de 11 digitos sem mascara nesta fonte.
  EMBic$cpf_cnpj <- mascarar_doc_pessoa_fisica(EMBic$cpf_cnpj)

  EMBic <- aplicar_filtro_palavras_chave(
    EMBic, campos = c("desc_infra", "desc_sanc", "tipo_infra"), regex = REGEX,
    label = "icmbio_embargos", export_dir = QA_DIR
  )

  terra::writeVector(EMBic, file.path(PRE_PROC_DIR, "icmbio_embargos.shp"), overwrite = TRUE)
})

safe_step("ICMBio infractions (shp)", {
  # REVISADO 2026-08 (auditoria F-01). Este era o unico bloco de leitura vetorial
  # sem clean_geometry e sem conferencia de contagem. O arquivo tem geometria
  # mista (pontos + registros nulos); terra::vect() resolve para UM tipo e
  # descarta o resto por message, nao por erro. O passo reportava sucesso, o
  # filtro de palavra-chave zerava o que sobrava, e a camada nunca contribuiu
  # com nada -- confirmado no cruzamento final (zero processos sinalizados).
  #
  # Inventario do arquivo (versao 2026-08-04, conferida registro a registro):
  #   41.667 registros = 41.665 pontos + 2 sem geometria
  #   nulos: Paraty/RJ e Cavalcante/GO, infracoes contra a flora, fora do recorte
  #   coordenadas: nenhuma (0,0); 2 pontos fora do bbox do Brasil
  #   filtro de garimpo: 2.074 registros, 1.478 em UF da Amazonia Legal
  #
  # DECISAO: a camada ENTRA, por sobreposicao espacial unica (ponto contra
  # poligono do processo) -- ver 05_integracao_final.R.
  zip_path <- file.path(RAW_DIR, "geo_icmbio", "autos_infracao_icmbio.zip")
  exdir    <- file.path(TMP_DIR, "icmbio_infracoes")

  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)

  shp_path <- first_match(exdir, "autos_infracao_icmbio\\.shp$")
  if (is.na(shp_path)) stop("No ICMBio infractions shapefile found after unzip.")

  # Fonte OPCIONAL (o pipeline segue com flag NA se ela cair), mas a leitura
  # agora reporta quantos registros o terra descartou em silencio.
  INFic <- ler_vetor(shp_path, label = "icmbio_infracoes", obrigatoria = FALSE)
  if (is.null(INFic)) stop("ICMBio infracoes indisponivel -- ver aviso acima.")

  # Descarte explicito de geometria vazia, com registro (antes: silencioso).
  # terra::geom() so devolve linhas para feicoes COM geometria, entao os indices
  # ausentes sao exatamente os registros nulos.
  n_antes      <- nrow(INFic)
  ids_com_geom <- sort(unique(terra::geom(INFic)[, "geom"]))
  n_nulos      <- n_antes - length(ids_com_geom)
  if (n_nulos > 0) {
    nulos_df <- as.data.frame(INFic)[setdiff(seq_len(n_antes), ids_com_geom), , drop = FALSE]
    readr::write_csv(nulos_df, file.path(QA_DIR, "icmbio_infracoes_sem_geometria.csv"))
    message("[icmbio_infracoes] ", n_nulos,
            " registro(s) sem geometria descartado(s) | detalhe em ",
            file.path(QA_DIR, "icmbio_infracoes_sem_geometria.csv"))
    INFic <- INFic[ids_com_geom, ]
  }

  # Colunas: acrescentadas nome_uc/cnuc (validacao cruzada independente da
  # geometria -- se o ponto cai no processo E a UC declarada bate com a UC que
  # o processo sobrepoe, ha confirmacao por dois caminhos) e numero_ai
  # (rastreabilidade do flag ate o documento). julgamento fica de fora: esta
  # vazio em ~96% dos autos de garimpo da Amazonia Legal, serve como flag
  # informativa e nao como criterio.
  cols_INFic <- c("numero_ai", "tipo", "valor_mult", "embargo", "apreensao",
                  "autuado", "cpf_cnpj", "desc_ai", "desc_sanc", "data", "ano",
                  "tipo_infra", "nome_uc", "cnuc", "municipio", "uf", "processo")
  faltando <- setdiff(cols_INFic, names(INFic))
  if (length(faltando) > 0) {
    stop("[icmbio_infracoes] colunas ausentes no shapefile: ",
         paste(faltando, collapse = ", "),
         " | presentes: ", paste(names(INFic), collapse = ", "))
  }
  INFic <- INFic[, cols_INFic]

  # ENCODING (auditoria F-01): o .cpg declara UTF-8 e o conteudo e UTF-8, mas
  # desc_ai vem truncado no meio de caractere multibyte em pelo menos um
  # registro. Como desc_ai e o campo do filtro, leitura estrita transforma o
  # registro em NA e ele sai do resultado sem aviso. Substitui byte invalido em
  # vez de falhar, e conta quantos foram afetados.
  n_invalidos <- sum(!validUTF8(INFic$desc_ai[!is.na(INFic$desc_ai)]))
  if (n_invalidos > 0) {
    message("[icmbio_infracoes] ", n_invalidos,
            " registro(s) com byte invalido em desc_ai -- substituido, nao descartado.")
  }
  INFic$desc_ai    <- to_upper_utf8(iconv(INFic$desc_ai,    "UTF-8", "UTF-8", sub = ""))
  INFic$desc_sanc  <- to_upper_utf8(iconv(INFic$desc_sanc,  "UTF-8", "UTF-8", sub = ""))
  INFic$tipo_infra <- to_upper_utf8(iconv(INFic$tipo_infra, "UTF-8", "UTF-8", sub = ""))

  # LGPD (auditoria F-07): este shapefile traz ~33 mil CPF de 11 digitos SEM
  # mascara (a ANM entrega pessoa fisica mascarada; as fontes ambientais nao).
  # Como a incorporacao foi decidida por sobreposicao ESPACIAL, o documento nao
  # e chave de cruzamento e nao ha razao para persistir o valor cru. Mascara na
  # INGESTAO, antes de qualquer escrita em disco.
  INFic$cpf_cnpj <- mascarar_doc_pessoa_fisica(INFic$cpf_cnpj)

  INFic <- aplicar_filtro_palavras_chave(
    INFic, campos = c("desc_ai", "tipo_infra"), regex = REGEX,
    label = "icmbio_infracoes", export_dir = QA_DIR
  )

  if (nrow(INFic) == 0) {
    stop("[icmbio_infracoes] filtro de garimpo devolveu 0 registros -- ",
         "esperado ~2.074. Conferir leitura e encoding antes de seguir.")
  }

  terra::writeVector(INFic, file.path(PRE_PROC_DIR, "icmbio_infracoes.shp"), overwrite = TRUE)
})

safe_step("SEMA-MT embargos (shp)", {
  zip_path <- file.path(RAW_DIR, "geo_sema_mt", "AREAS_EMBARGADAS_SEMA.zip")
  exdir    <- file.path(TMP_DIR, "sema_mt_embargos")

  if (!safe_unzip(zip_path, exdir)) stop("Missing or unreadable zip: ", zip_path)

  shp_path <- first_match(exdir, "AREAS_EMBARGADAS_SEMA\\.shp$")
  if (is.na(shp_path)) stop("No SEMA-MT embargos shapefile found after unzip.")

  EMBmt <- ler_vetor(shp_path, label = "sema_mt_embargos", obrigatoria = FALSE)
  if (is.null(EMBmt)) stop("SEMA-MT embargos indisponivel -- ver aviso acima.")
  EMBmt <- clean_geometry(EMBmt, label = "SEMA-MT embargos")
  EMBmt <- EMBmt[, c("NOME", "CPF_CNPJ", "DANO", "ANO_DESMAT", "DAT_LAVRAT", "N_PROCESSO")]

  EMBmt$CPF_CNPJ <- mascarar_doc_pessoa_fisica(EMBmt$CPF_CNPJ)  # LGPD (F-07)

  EMBmt$DANO <- to_upper_utf8(EMBmt$DANO, from = "Windows-1252")
  EMBmt$NOME <- to_upper_utf8(EMBmt$NOME, from = "Windows-1252")

  EMBmt <- aplicar_filtro_palavras_chave(
    EMBmt, campos = "DANO", regex = REGEX,
    label = "sema_mt_embargos", export_dir = QA_DIR
  )

  terra::writeVector(EMBmt, file.path(PRE_PROC_DIR, "sema_mt_embargos.shp"), overwrite = TRUE)
})

safe_step("SEMA-MT SIGA embargos (shp)", {
  zip_path <- file.path(RAW_DIR, "geo_sema_mt", "AREA_EMBARGADA_SIGA_POLIGONO.zip")
  exdir    <- file.path(TMP_DIR, "sema_mt_embargos_siga")

  if (!safe_unzip(zip_path, exdir)) {
    message("SIGA embargos zip missing/unreadable. Skipping.")
    return(invisible(NULL))
  }

  shp_path <- first_match(exdir, "\\.shp$")
  if (is.na(shp_path)) {
    message("SIGA embargos extracted but no .shp found. Skipping.")
    return(invisible(NULL))
  }

  x <- ler_vetor(shp_path, label = "sema_mt_siga_embargos", obrigatoria = FALSE)
  if (is.null(x)) return(invisible(NULL))
  x <- clean_geometry(x, label = "SEMA-MT SIGA embargos")

  txt_cols <- intersect(c("NOME_RAZAO", "NOME_FANTA", "TIPO", "SUBTIPO", "DISPOSITIV",
                          "DESCRICAO_", "ATIVIDADE", "ATIVIDADE_"), names(x))
  if (length(txt_cols)) {
    vals <- terra::values(x)
    vals[txt_cols] <- lapply(vals[txt_cols], to_upper_utf8, from = "Windows-1252")
    terra::values(x) <- vals
    rm(vals); gc()
  }

  # TIPO e campo categorico fechado (poucas opcoes no dominio, ex: "RECURSOS
  # MINERAIS") -- mais confiavel que regex em texto livre. Decisao 2026-08:
  # somar como criterio adicional (OR) ao filtro de palavra-chave abaixo, em
  # vez de substitui-lo, para nao perder registro capturado so pelo texto.
  keep_tipo <- if ("TIPO" %in% names(x)) {
    trimws(as.character(terra::values(x)$TIPO)) == "RECURSOS MINERAIS"
  } else {
    NULL
  }

  fcols <- intersect(c("SUBTIPO", "DISPOSITIV", "DESCRICAO_", "ATIVIDADE", "ATIVIDADE_"), names(x))
  if (length(fcols)) {
    x <- aplicar_filtro_palavras_chave(
      x, campos = fcols, regex = REGEX,
      label = "sema_mt_embargos_siga", export_dir = QA_DIR,
      keep_extra = keep_tipo
    )
  } else if (!is.null(keep_tipo)) {
    x <- subset_rows(x, keep_tipo)
  } else {
    message("SIGA embargos: no filterable text fields found; saving full layer.")
  }

  terra::writeVector(x, file.path(PRE_PROC_DIR, "sema_mt_embargos_siga.shp"), overwrite = TRUE)
})

safe_step("SEMA-MT SIGA infractions (shp)", {
  zip_path <- file.path(RAW_DIR, "geo_sema_mt", "AUTOS_DE_INFRACAO_SIGA_POLIGONO.zip")
  exdir    <- file.path(TMP_DIR, "sema_mt_infracoes_siga")

  if (!safe_unzip(zip_path, exdir)) {
    message("SIGA infractions zip missing/unreadable. Skipping.")
    return(invisible(NULL))
  }

  shp_path <- first_match(exdir, "\\.shp$")
  if (is.na(shp_path)) {
    message("SIGA infractions extracted but no .shp found. Skipping.")
    return(invisible(NULL))
  }

  x <- ler_vetor(shp_path, label = "sema_mt_siga_infracoes", obrigatoria = FALSE)
  if (is.null(x)) return(invisible(NULL))
  x <- clean_geometry(x, label = "SEMA-MT SIGA infracoes")

  txt_cols <- intersect(c("NOME_RAZAO", "NOME_FANTA", "TIPO", "SUBTIPO", "DISPOSITIV",
                          "DESCRICAO_", "ATIVIDADE", "ATIVIDADE_"), names(x))
  if (length(txt_cols)) {
    vals <- terra::values(x)
    vals[txt_cols] <- lapply(vals[txt_cols], to_upper_utf8, from = "Windows-1252")
    terra::values(x) <- vals
    rm(vals); gc()
  }

  # Mesmo raciocinio do SIGA embargos: TIPO == "RECURSOS MINERAIS" somado via
  # OR ao filtro de texto livre (ver comentario na secao de embargos acima).
  keep_tipo <- if ("TIPO" %in% names(x)) {
    trimws(as.character(terra::values(x)$TIPO)) == "RECURSOS MINERAIS"
  } else {
    NULL
  }

  fcols <- intersect(c("SUBTIPO", "DISPOSITIV", "DESCRICAO_", "ATIVIDADE", "ATIVIDADE_"), names(x))
  if (length(fcols)) {
    x <- aplicar_filtro_palavras_chave(
      x, campos = fcols, regex = REGEX,
      label = "sema_mt_infracoes_siga", export_dir = QA_DIR,
      keep_extra = keep_tipo
    )
  } else if (!is.null(keep_tipo)) {
    x <- subset_rows(x, keep_tipo)
  } else {
    message("SIGA infractions: no filterable text fields found; saving full layer.")
  }

  terra::writeVector(x, file.path(PRE_PROC_DIR, "sema_mt_infracoes_siga.shp"), overwrite = TRUE)
})

# Cleanup & Finish --------------------------------------
summary_df <- purrr::imap_dfr(etl_log, ~{
  data.frame(
    Task = .y,
    Status = if (.x$success) "SUCCESS" else "FAILED",
    Details = if (is.na(.x$error_msg)) "Completed" else .x$error_msg
  )
})

print(summary_df, row.names = FALSE)
message("\nProcess finished.")
message("Checks desta execução (logs de parsing CFEM e amostras não-capturadas pelo filtro de palavras-chave) em: ", QA_DIR)
unlink(TMP_DIR, recursive = TRUE, force = TRUE)

message("\n=== 02_pre_proc.R — CONCLUÍDO ===")