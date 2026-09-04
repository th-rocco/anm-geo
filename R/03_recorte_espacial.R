################################################################################
# 03_recorte_espacial.R
################################################################################

rm(list = ls(all.names = TRUE))
options(scipen = 999)

Sys.unsetenv("PROJ_LIB")
Sys.unsetenv("GDAL_DATA")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(tidyterra)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(tibble)
  library(stringr)
  library(here)
})

source(here::here("R", "utils.R"))

# --- Caminhos -----------------------------------------------------------------
RAW_DIR      <- here::here("data", "raw_data")
PRE_PROC_DIR <- here::here("data", "pre_proc_data")
CLEAN_DIR    <- here::here("data", "clean_data")
QA_DIR       <- here::here("data", "_qa", "03_recorte_espacial")
AMZL_DIR     <- here::here("data", "raw_data", "Limites_Amazonia_Legal_2024")

for (d in c(CLEAN_DIR, QA_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# BLOCO 0
# =============================================================================
# Falha aqui, nomeando o que falta e o que rodar para obter.
insumos_03 <- tibble::tribble(
  ~caminho,                                                    ~origem,
  file.path(PRE_PROC_DIR, "cadastro_mineiro.csv"),             "02_pre_proc.R (ANM SCM consolidation)",
  file.path(PRE_PROC_DIR, "sigmine_pma.shp"),                  "02_pre_proc.R (SIGMINE PMA extraction)",
  file.path(PRE_PROC_DIR, "terras_indigenas.shp"),             "02_pre_proc.R (FUNAI TI)",
  file.path(PRE_PROC_DIR, "unidades_conservacao.shp"),         "02_pre_proc.R (MMA CNUC)",
  file.path(PRE_PROC_DIR, "quilombolas.shp"),                  "02_pre_proc.R (INCRA Quilombolas)"
)

faltando_03 <- dplyr::filter(insumos_03, !file.exists(caminho))

# A Amazônia Legal é insumo MANUAL do IBGE: checa a pasta, não um arquivo fixo.
amzl_shp <- list.files(AMZL_DIR, pattern = "\\.shp$", full.names = TRUE)
if (!dir.exists(AMZL_DIR) || length(amzl_shp) == 0) {
  faltando_03 <- dplyr::bind_rows(
    faltando_03,
    tibble::tibble(caminho = AMZL_DIR,
                   origem  = "INSUMO MANUAL do IBGE — baixar e extrair (ver leia-me)")
  )
}

if (nrow(faltando_03) > 0) {
  message("\n[03] INSUMOS AUSENTES:")
  purrr::pwalk(faltando_03, \(caminho, origem)
               message("  - ", basename(caminho), "  <-  ", origem))
  stop("[03] ", nrow(faltando_03), " insumo(s) ausente(s). Ver lista acima.",
       call. = FALSE)
}
message("[03] insumos conferidos: ", nrow(insumos_03) + 1, " OK.")

# =============================================================================
# BLOCO 1 — CADASTRO MINEIRO (SCM)
# =============================================================================

cm <- readr::read_csv(file.path(PRE_PROC_DIR, "cadastro_mineiro.csv"), show_col_types = FALSE)

dict_rename <- c(
  "^Superintendência$"                                                      = "SUPERINTEN",
  "^Processo$"                                                              = "PROCESSO",
  "^Tipo\\.de\\.requerimento$|^Tipo de requerimento$"                       = "TIPO_REQcm",
  "^Fase\\.Atual$|^Fase Atual$"                                             = "FASEcm",
  "^CPF\\.CNPJ\\.do\\.titular$|^CPF/CNPJ do titular$|^CPF CNPJ do titular$" = "CPF_CNPJcm",
  "^Titular$"                                                               = "TITULARcm",
  "^Municipio\\.s\\.$|^Municipio\\(s\\)$"                                   = "name_muni",
  "^Substância\\.s\\.$|^Substância\\(s\\)$"                                 = "SUBScm",
  "^Tipo\\.s\\.\\.de\\.Uso$|^Tipo\\(s\\) de Uso$"                           = "TIPO_USO",
  "^Situação$"                                                              = "STATUS",
  "^Localidade$"                                                            = "LOCALIDADE",
  "^QuantidadeMinerio$"                                                     = "QTD_MINERIO",
  "^DataPublicacao$"                                                        = "DT_PUBLICACAO",
  "^Data da Cessão$"                                                        = "DT_CESSAO"
)
n_orig <- names(cm); n_new <- n_orig
for (pat in names(dict_rename)) {
  n_new <- ifelse(stringr::str_detect(n_orig, stringr::regex(pat)), dict_rename[pat], n_new)
}
names(cm) <- n_new

cm_clean <- cm |>
  dplyr::select(PROCESSO, TIPO_REQcm, FASEcm, CPF_CNPJcm, TITULARcm, SUBScm, DT_CESSAO) |>
  dplyr::filter(!is.na(PROCESSO)) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) |>
  dplyr::mutate(CPF_CNPJcm = padroniza_doc(CPF_CNPJcm))

contagem_conflitos <- cm_clean |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(
    tem_conflito = (
      dplyr::n_distinct(TIPO_REQcm, na.rm = TRUE) > 1 |
      dplyr::n_distinct(FASEcm,     na.rm = TRUE) > 1 |
      dplyr::n_distinct(SUBScm,     na.rm = TRUE) > 1 |
      dplyr::n_distinct(CPF_CNPJcm, na.rm = TRUE) > 1 |
      dplyr::n_distinct(TITULARcm,  na.rm = TRUE) > 1
    ), .groups = "drop"
  )

cm_normais <- cm_clean |>
  dplyr::inner_join(dplyr::filter(contagem_conflitos, !tem_conflito), by = "PROCESSO") |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(dplyr::across(
    c(TIPO_REQcm, FASEcm, SUBScm, CPF_CNPJcm, TITULARcm),
    ~ dplyr::last(stats::na.omit(.x))), .groups = "drop")

cm_conflitos <- cm_clean |>
  dplyr::inner_join(dplyr::filter(contagem_conflitos, tem_conflito), by = "PROCESSO")

if (nrow(cm_conflitos) > 0) {
  cm_conflitos <- cm_conflitos |>
    dplyr::mutate(DT_CESSAO_formatada = as.Date(DT_CESSAO, format = "%d/%m/%Y"))

  processos_sem_cessao <- cm_conflitos |>
    dplyr::group_by(PROCESSO) |>
    dplyr::summarise(tem_cessao = any(!is.na(DT_CESSAO_formatada)), .groups = "drop") |>
    dplyr::filter(!tem_cessao) |>
    dplyr::pull(PROCESSO)

  if (length(processos_sem_cessao) > 0) {
    conflitos_sem_cessao <- cm_conflitos |>
      dplyr::filter(PROCESSO %in% processos_sem_cessao) |>
      dplyr::select(PROCESSO, TIPO_REQcm, FASEcm, CPF_CNPJcm, TITULARcm, SUBScm, DT_CESSAO) |>
      dplyr::arrange(PROCESSO)

    readr::write_csv(conflitos_sem_cessao, file.path(QA_DIR, "cm_conflitos_sem_cessao.csv"))
    message(sprintf(
      "[cadastro_mineiro] ATENCAO: %d processo(s) em conflito SEM nenhuma DT_CESSAO (desempate por cessao nao se aplica) -- detalhe: %s",
      length(processos_sem_cessao), file.path(QA_DIR, "cm_conflitos_sem_cessao.csv")
    ))
    print(conflitos_sem_cessao, n = Inf)
  }

  cm_resolvidos <- cm_conflitos |>
    dplyr::group_by(PROCESSO) |>
    dplyr::arrange(PROCESSO, dplyr::desc(DT_CESSAO_formatada)) |>
    dplyr::summarise(dplyr::across(
      c(TIPO_REQcm, FASEcm, SUBScm, CPF_CNPJcm, TITULARcm),
      ~ dplyr::first(stats::na.omit(.x))), .groups = "drop")
} else {
  cm_resolvidos <- tibble::tibble()
}

cm_unique <- dplyr::bind_rows(cm_normais, cm_resolvidos)

save_ckpt(cm_unique, "03_cm_unique")

# =============================================================================
# BLOCO 2 — PMA (SIGMINE)
# =============================================================================

cm_unique <- load_ckpt("03_cm_unique")

pma0 <- ler_vetor(file.path(PRE_PROC_DIR, "sigmine_pma.shp"), label = "PMA")

pma1 <- pma0 |>
  dplyr::filter(AREA_HA > 0) |>
  dplyr::filter(!is.na(PROCESSO) & PROCESSO != "" & PROCESSO != "0")

invalid_geom <- !terra::is.valid(pma1)
if (any(invalid_geom)) {
  pma1 <- rbind(terra::makeValid(pma1[invalid_geom, ]), pma1[!invalid_geom, ])
}

pma2 <- pma1 |>
  dplyr::select(-dplyr::any_of(c("USO", "UF", "DSProcesso", "ID", "NUMERO", "ANO"))) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), toupper)) |>
  dplyr::rename(TITULAR = NOME) |>
  dplyr::mutate(AREA_orig = AREA_HA)

sf::sf_use_s2(FALSE)
pma3 <- pma2 |> sf::st_as_sf() |> sf::st_make_valid() |> sf::st_transform(4326)
sf::sf_use_s2(TRUE)

pma3_m <- sf::st_transform(pma3, 5880) |> sf::st_make_valid()

pma4 <- pma3_m |>
  dplyr::group_by(PROCESSO, FASE, ULT_EVENTO, TITULAR, SUBS) |>
  dplyr::summarise(AREA_orig = sum(AREA_orig, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(AREA_HA = as.numeric(sf::st_area(geometry)) / 10000) |>
  sf::st_transform(4326)

ids_conflict <- sf::st_drop_geometry(pma4) |>
  dplyr::filter(duplicated(PROCESSO) | duplicated(PROCESSO, fromLast = TRUE)) |>
  dplyr::pull(PROCESSO) |> unique()

pma5 <- pma4 |>
  dplyr::filter(!PROCESSO %in% ids_conflict) |>
  dplyr::mutate(fase_conflito_tipo = NA_character_, area_disponivel_ha = NA_real_) |>
  dplyr::select(PROCESSO, FASE, ULT_EVENTO, TITULAR, SUBS, AREA_HA, AREA_orig,
                fase_conflito_tipo, area_disponivel_ha, geometry)
pma6 <- pma4 |> dplyr::filter(PROCESSO %in% ids_conflict)

if (nrow(pma6) > 0) {

  pma6_class <- sf::st_drop_geometry(pma6) |>
    dplyr::group_by(PROCESSO) |>
    dplyr::summarise(
      n_disp     = sum(FASE == "DISPONIBILIDADE"),
      n_fase_out = dplyr::n_distinct(FASE[FASE != "DISPONIBILIDADE"]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      fase_conflito_tipo = dplyr::case_when(
        n_disp == 1 & n_fase_out == 1 ~ "disponibilidade_parcial",
        TRUE                          ~ "outro_padrao_nao_resolvido"
      )
    ) |>
    dplyr::select(PROCESSO, fase_conflito_tipo)

  pma6_detalhe <- pma6 |>
    sf::st_drop_geometry() |>
    dplyr::left_join(pma6_class, by = "PROCESSO") |>
    dplyr::mutate(
      ULT_EV_DAT = as.Date(stringr::str_extract(ULT_EVENTO, "\\d{2}/\\d{2}/\\d{4}$"), format = "%d/%m/%Y")
    ) |>
    dplyr::select(PROCESSO, fase_conflito_tipo, AREA_HA, FASE, TITULAR, SUBS, ULT_EVENTO, ULT_EV_DAT) |>
    dplyr::arrange(PROCESSO, dplyr::desc(AREA_HA))

  readr::write_csv(pma6_detalhe, file.path(QA_DIR, "poligonos_conflitantes.csv"))

  message(sprintf(
    "[recorte_espacial] poligonos conflitantes | processos: %d (disponibilidade_parcial: %d | outro_padrao_nao_resolvido: %d) | detalhe: %s",
    nrow(pma6_class),
    sum(pma6_class$fase_conflito_tipo == "disponibilidade_parcial"),
    sum(pma6_class$fase_conflito_tipo == "outro_padrao_nao_resolvido"),
    file.path(QA_DIR, "poligonos_conflitantes.csv")
  ))

  pma6 <- pma6 |> dplyr::left_join(pma6_class, by = "PROCESSO")

  # --- Padrão "disponibilidade_parcial"
  pma6_disp <- pma6 |> dplyr::filter(fase_conflito_tipo == "disponibilidade_parcial")

  if (nrow(pma6_disp) > 0) {
    area_disp <- sf::st_drop_geometry(pma6_disp) |>
      dplyr::filter(FASE == "DISPONIBILIDADE") |>
      dplyr::select(PROCESSO, area_disponivel_ha = AREA_HA)

    pma6_disp_resolved <- pma6_disp |>
      dplyr::filter(FASE != "DISPONIBILIDADE") |>
      dplyr::left_join(area_disp, by = "PROCESSO") |>
      dplyr::select(PROCESSO, FASE, ULT_EVENTO, TITULAR, SUBS, AREA_HA, AREA_orig,
                    fase_conflito_tipo, area_disponivel_ha, geometry)
  } else {
    pma6_disp_resolved <- pma6_disp
  }

  pma6_outro <- pma6 |> dplyr::filter(fase_conflito_tipo == "outro_padrao_nao_resolvido")

  if (nrow(pma6_outro) > 0) {
    pma6_outro_resolved <- pma6_outro |>
      dplyr::group_by(PROCESSO) |>
      dplyr::filter(AREA_HA == max(AREA_HA)) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::mutate(area_disponivel_ha = NA_real_) |>
      dplyr::select(PROCESSO, FASE, ULT_EVENTO, TITULAR, SUBS, AREA_HA, AREA_orig,
                    fase_conflito_tipo, area_disponivel_ha, geometry)
  } else {
    pma6_outro_resolved <- pma6_outro |>
      dplyr::mutate(area_disponivel_ha = NA_real_) |>
      dplyr::select(PROCESSO, FASE, ULT_EVENTO, TITULAR, SUBS, AREA_HA, AREA_orig,
                    fase_conflito_tipo, area_disponivel_ha, geometry)
  }

  pma6_resolved <- rbind(pma6_disp_resolved, pma6_outro_resolved)
}

pma7sf <- if (nrow(pma6) > 0) rbind(pma5, pma6_resolved) else pma5
pma7   <- terra::vect(pma7sf)

# Informação suplementar do SCM
cm_attrs <- cm_unique |> dplyr::select(PROCESSO, TIPO_REQcm, CPF_CNPJcm)
pma_cm   <- tidyterra::left_join(pma7, cm_attrs, by = "PROCESSO")

terra::writeVector(pma_cm, file.path(CLEAN_DIR, "pma_clean_cm.shp"), overwrite = TRUE)
save_ckpt(pma_cm, "03_pma_cm")

# =============================================================================
# BLOCO 3 — RECORTE AMAZÔNIA LEGAL
# =============================================================================

pma_cm <- load_ckpt("03_pma_cm") |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                              ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)))

amzl <- ler_vetor(amzl_shp[1], label = "AMZ_LEGAL") |>
  terra::project(terra::crs(pma_cm))

ti  <- ler_vetor(file.path(PRE_PROC_DIR, "terras_indigenas.shp"),     label = "TI")
uc  <- ler_vetor(file.path(PRE_PROC_DIR, "unidades_conservacao.shp"), label = "UC")
qui <- ler_vetor(file.path(PRE_PROC_DIR, "quilombolas.shp"),          label = "QUILOMBOLA")

intersect_ids <- terra::is.related(pma_cm, amzl, "intersects")
pma_amzl       <- pma_cm[intersect_ids, ]
processos_amzl <- unique(pma_amzl$PROCESSO)

ti_amzl  <- ti[terra::is.related(ti,   amzl, "intersects"), ]
uc_amzl  <- uc[terra::is.related(uc,   amzl, "intersects"), ]
qui_amzl <- qui[terra::is.related(qui, amzl, "intersects"), ]

terra::writeVector(pma_amzl, file.path(CLEAN_DIR, "pma_amzl.shp"), overwrite = TRUE)
save_ckpt(pma_amzl,       "03_pma_amzl")
save_ckpt(ti_amzl,        "03_ti_amzl")
save_ckpt(uc_amzl,        "03_uc_amzl")
save_ckpt(qui_amzl,       "03_qui_amzl")
save_ckpt(processos_amzl, "03_processos_amzl")

n_disp_parcial_amzl <- sum(pma_amzl$fase_conflito_tipo == "disponibilidade_parcial", na.rm = TRUE)
n_outro_amzl        <- sum(pma_amzl$fase_conflito_tipo == "outro_padrao_nao_resolvido", na.rm = TRUE)
message(sprintf(
  "\n=== 03_recorte_espacial.R — CONCLUÍDO === | processos AMZL: %d | disponibilidade_parcial: %d | outro_padrao_nao_resolvido: %d",
  length(processos_amzl), n_disp_parcial_amzl, n_outro_amzl
))