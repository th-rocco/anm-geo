################################################################################
# 06_correcao_cfem.R
#
# Depende do checkpoint 05_cfem_bruto (preparo, sem nenhuma correcao ainda)
# gerado pelo 05_integracao_final.R.
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
  library(stringr)
  library(here)
  library(ggplot2)
})

source(here::here("R", "utils.R"))

# --- Caminhos -----------------------------------------------------------------
RAW_DIR      <- here::here("data", "raw_data")
PRE_PROC_DIR <- here::here("data", "pre_proc_data")
QA_DIR       <- here::here("data", "_qa", "06_correcao_cfem")
QA_DIR_CORR  <- here::here("data", "_qa", "correcao_pontual_cfem")

RESULT_SHINY <- here::here("data", "result_shiny")
RESULT_GEE   <- here::here("data", "result_gee")
RESULT_DB    <- here::here("data", "result_db")

BIOMA_DIR <- here::here("data", "raw_data", "Biomas_250mil")

for (d in c(RESULT_SHINY, RESULT_GEE, RESULT_DB, QA_DIR, QA_DIR_CORR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# CORRECAO DE PESO/PRECO — CFEM (OURO e CASSITERITA)
# =============================================================================

cfem_final    <- load_ckpt("05_cfem_bruto")
pma_tp        <- load_ckpt("05_pma_tp")
cfem_aut_amzl <- load_ckpt("05_cfem_aut_amzl")

min_peso_g            <- 0.00000000000000000001
min_grp_muni          <- 5
min_grp_state         <- 10
min_grp_month         <- 15
min_grp_ano           <- 20
min_grp_global        <- 100
p_round_min           <- -20
p_round_max           <- 20
compute_median_hierarchical <- function(df, preco_col = "preco_g_orig",
                                        min_muni, min_state, min_month, min_ano, min_global,
                                        max_med_plaus, min_med_plaus) {
  df2 <- df |> dplyr::mutate(.idx = dplyr::row_number())
  med_muni  <- df2 |> dplyr::filter(!is.na(.data[[preco_col]])) |> dplyr::group_by(data, code_muni) |>
    dplyr::summarise(n_m = dplyr::n(), med_m = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_state <- df2 |> dplyr::filter(!is.na(.data[[preco_col]])) |> dplyr::group_by(data, abbrev_state) |>
    dplyr::summarise(n_s = dplyr::n(), med_s = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_month <- df2 |> dplyr::filter(!is.na(.data[[preco_col]])) |> dplyr::group_by(data) |>
    dplyr::summarise(n_mo = dplyr::n(), med_mo = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_ano   <- df2 |> dplyr::filter(!is.na(.data[[preco_col]])) |> dplyr::group_by(ANO) |>
    dplyr::summarise(n_a = dplyr::n(), med_a = median(.data[[preco_col]], na.rm = TRUE), .groups = "drop")
  med_global <- df2 |> dplyr::filter(!is.na(.data[[preco_col]])) |>
    dplyr::summarise(n_g = dplyr::n(), med_g = median(.data[[preco_col]], na.rm = TRUE))

  out <- df2 |>
    dplyr::left_join(med_muni,  by = c("data", "code_muni")) |>
    dplyr::left_join(med_state, by = c("data", "abbrev_state")) |>
    dplyr::left_join(med_month, by = c("data")) |>
    dplyr::left_join(med_ano,   by = c("ANO")) |>
    dplyr::mutate(
      med_preco_base = dplyr::case_when(
        !is.na(n_m)  & n_m  >= min_muni  & med_m  <= max_med_plaus & med_m  >= min_med_plaus ~ med_m,
        !is.na(n_s)  & n_s  >= min_state & med_s  <= max_med_plaus & med_s  >= min_med_plaus ~ med_s,
        !is.na(n_mo) & n_mo >= min_month & med_mo <= max_med_plaus & med_mo >= min_med_plaus ~ med_mo,
        !is.na(n_a)  & n_a  >= min_ano   & med_a  <= max_med_plaus & med_a  >= min_med_plaus ~ med_a,
        !is.na(med_global$med_g) & med_global$n_g >= min_global &
          med_global$med_g <= max_med_plaus & med_global$med_g >= min_med_plaus ~ med_global$med_g,
        TRUE ~ NA_real_
      ),
      med_level = dplyr::case_when(
        !is.na(n_m)  & n_m  >= min_muni  & med_m  <= max_med_plaus & med_m  >= min_med_plaus ~ "muni",
        !is.na(n_s)  & n_s  >= min_state & med_s  <= max_med_plaus & med_s  >= min_med_plaus ~ "state",
        !is.na(n_mo) & n_mo >= min_month & med_mo <= max_med_plaus & med_mo >= min_med_plaus ~ "month",
        !is.na(n_a)  & n_a  >= min_ano   & med_a  <= max_med_plaus & med_a  >= min_med_plaus ~ "ano",
        !is.na(med_global$med_g) & med_global$n_g >= min_global &
          med_global$med_g <= max_med_plaus & med_global$med_g >= min_med_plaus ~ "global",
        TRUE ~ NA_character_
      )
    ) |> dplyr::arrange(.idx)
  list(med = out$med_preco_base, level = out$med_level)
}

suggest_weight_row <- function(VALORtot, PESO_G, med_preco, p_range = p_round_min:p_round_max) {
  if (is.na(VALORtot) | is.na(med_preco) | med_preco <= 0) {
    return(list(PESO_G_sugerido = NA_real_, preco_g_sugerido = NA_real_,
                dist_rel_sug = NA_real_, corr_motivo = "no_med", candidate_name = NA_character_))
  }
  cands <- list("original" = PESO_G)
  for (p in p_range) if (p != 0) cands[[paste0("pow10_p", p)]] <- PESO_G * (10^p)
  cand_df <- tibble::tibble(name = names(cands), peso_cand = unlist(cands)) |>
    dplyr::mutate(
      preco_cand = dplyr::if_else(peso_cand > min_peso_g, VALORtot / peso_cand, NA_real_),
      dist_rel   = dplyr::if_else(!is.na(preco_cand), abs(preco_cand / med_preco - 1), NA_real_)
    )
  best_i <- which.min(replace(cand_df$dist_rel, is.na(cand_df$dist_rel), Inf))
  best   <- cand_df[best_i, ]
  list(PESO_G_sugerido = as.numeric(best$peso_cand), preco_g_sugerido = as.numeric(best$preco_cand),
       dist_rel_sug = as.numeric(best$dist_rel),
       corr_motivo = if (best$name == "original") "original" else "pow10",
       candidate_name = as.character(best$name))
}
FASES_CORR <- FASES_CORR_PADRAO
fatores_simples <- 10^(-6:6)
# Cassiterita usa alcance ampliado (+-20, igual ao alcance da mediana
# hierarquica do ouro) em vez de +-6 -- decisao 2026-08, validada em
# checks/teste_correcao_cassiterita_metodos.R: o alcance maior sozinho
# (sem mediana de vizinhos nenhuma) já resolve os mesmos casos que os
# metodos com mediana resolviam, sem herdar o risco de vizinho contaminado.
fatores_simples_amplo <- 10^(-20:20)
corrige_simples_g <- function(peso_g, valortot, pmin_g, pmax_g) {
  if (is.na(peso_g) || is.na(valortot) || peso_g <= 0 || valortot <= 0) return(c(peso = peso_g, fator = NA_real_))
  ok <- fatores_simples[ {p <- valortot / (peso_g * fatores_simples); p >= pmin_g & p <= pmax_g} ]
  if (length(ok) == 0) return(c(peso = peso_g, fator = NA_real_))
  f <- ok[which.min(abs(log10(ok)))]
  c(peso = peso_g * f, fator = f)
}

report_check <- function(df, mineral, etapa, pmin_kg, pmax_kg, qa_path = NULL) {
  d <- df |>
    dplyr::filter(!is.na(PESO_KG_final), PESO_KG_final > 0, !is.na(VALORtot), VALORtot > 0) |>
    dplyr::mutate(rs_por_kg = VALORtot / PESO_KG_final, fora = rs_por_kg < pmin_kg | rs_por_kg > pmax_kg)
  resumo <- d |> dplyr::count(FASE, fora) |> dplyr::arrange(dplyr::desc(fora), dplyr::desc(n))
  message(sprintf("[%s] %s | avaliados: %d | fora de [%g-%g] R$/kg: %d",
                  mineral, etapa, nrow(d), pmin_kg, pmax_kg, sum(d$fora, na.rm = TRUE)))
  print(resumo)
  if (!is.null(qa_path)) {
    readr::write_csv(resumo |> dplyr::mutate(mineral = mineral, etapa = etapa, .before = 1),
                     qa_path, append = file.exists(qa_path))
  }
  invisible(d)
}

corrige_mineral_3checks <- function(cfem_final, mineral_label, subs_keep, subs_col,
                                    pmin_kg, pmax_kg, min_med_plaus, max_med_plaus) {
  pmin_g <- pmin_kg / 1000; pmax_g <- pmax_kg / 1000
  qa_path_checks <- file.path(QA_DIR, "cfem_correcao_checks.csv")

  universo <- cfem_final |> dplyr::filter(.data[[subs_col]] %in% subs_keep, FASE %in% FASES_CORR)
  report_check(universo, mineral_label, "CHECK 1 (antes)", pmin_kg, pmax_kg, qa_path_checks)

  med_info <- compute_median_hierarchical(
    universo, preco_col = "preco_g_orig", min_muni = min_grp_muni, min_state = min_grp_state,
    min_month = min_grp_month, min_ano = min_grp_ano, min_global = min_grp_global,
    max_med_plaus = max_med_plaus, min_med_plaus = min_med_plaus
  )

  rob <- universo |>
    dplyr::mutate(med_preco_base = med_info$med, med_level = med_info$level) |>
    dplyr::rowwise() |>
    dplyr::mutate(sug = list(suggest_weight_row(VALORtot, PESO_G, med_preco_base, p_range = p_round_min:p_round_max))) |>
    tidyr::unnest_wider(sug) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      PESO_G_final  = dplyr::if_else(!is.na(PESO_G_sugerido), PESO_G_sugerido, PESO_G),
      PESO_KG_final = dplyr::if_else(!is.na(PESO_G_final), PESO_G_final / 1000, NA_real_),
      preco_g_final = dplyr::if_else(!is.na(PESO_G_final) & PESO_G_final > min_peso_g, VALORtot / PESO_G_final, NA_real_),
      corr = dplyr::if_else(is.na(candidate_name), "original", candidate_name)
    )

  cfem_final <- cfem_final |>
    dplyr::left_join(
      rob |> dplyr::select(row_id, PESO_G_final, PESO_KG_final, preco_g_final, corr) |>
        dplyr::rename(PESO_G_r = PESO_G_final, PESO_KG_r = PESO_KG_final, preco_g_r = preco_g_final, corr_r = corr),
      by = "row_id") |>
    dplyr::mutate(
      PESO_G_final  = dplyr::if_else(!is.na(PESO_G_r),  PESO_G_r,  PESO_G_final),
      PESO_KG_final = dplyr::if_else(!is.na(PESO_KG_r), PESO_KG_r, PESO_KG_final),
      preco_g_final = dplyr::if_else(!is.na(preco_g_r), preco_g_r, preco_g_final),
      corr          = dplyr::if_else(!is.na(corr_r),    corr_r,    corr)
    ) |>
    dplyr::select(-PESO_G_r, -PESO_KG_r, -preco_g_r, -corr_r)

  univ2 <- cfem_final |> dplyr::filter(.data[[subs_col]] %in% subs_keep, FASE %in% FASES_CORR)
  d2 <- report_check(univ2, mineral_label, "CHECK 2 (pos-robusto)", pmin_kg, pmax_kg, qa_path_checks)
  ids_fora <- d2 |> dplyr::filter(fora) |> dplyr::pull(row_id)

  if (length(ids_fora) > 0) {
    fb <- cfem_final |>
      dplyr::filter(row_id %in% ids_fora) |>
      dplyr::rowwise() |>
      dplyr::mutate(.r = list(corrige_simples_g(PESO_G_final, VALORtot, pmin_g, pmax_g)),
                    PESO_G_fb = .r[["peso"]], fator_fb = .r[["fator"]]) |>
      dplyr::ungroup() |>
      dplyr::select(row_id, PESO_G_fb, fator_fb)

    cfem_final <- cfem_final |>
      dplyr::left_join(fb, by = "row_id") |>
      dplyr::mutate(
        aplicou = !is.na(fator_fb) & fator_fb != 1,
        PESO_G_final  = dplyr::if_else(aplicou, PESO_G_fb,        PESO_G_final),
        PESO_KG_final = dplyr::if_else(aplicou, PESO_G_fb / 1000, PESO_KG_final),
        preco_g_final = dplyr::if_else(aplicou & PESO_G_final > min_peso_g, VALORtot / PESO_G_final, preco_g_final),
        corr = dplyr::if_else(aplicou, paste0("simples_1e", round(log10(fator_fb))), corr)
      ) |>
      dplyr::select(-PESO_G_fb, -fator_fb, -aplicou)
    message("[", mineral_label, "] SIMPLES aplicado a ", length(ids_fora), " remanescente(s).")
  } else {
    message("[", mineral_label, "] nenhum remanescente para o metodo simples.")
  }

  univ3 <- cfem_final |> dplyr::filter(.data[[subs_col]] %in% subs_keep, FASE %in% FASES_CORR)
  d3 <- report_check(univ3, mineral_label, "CHECK 3 (final)", pmin_kg, pmax_kg, qa_path_checks)
  resta <- d3 |> dplyr::filter(fora)
  if (nrow(resta) > 0) {
    message("[", mineral_label, "] IRRECUPERAVEIS - revisar:")
    print(resta |> dplyr::select(PROCESSO, FASE, PESO_KG, PESO_KG_final, VALORtot, rs_por_kg))
    readr::write_csv(resta |> dplyr::mutate(mineral = mineral_label, .before = 1),
                     file.path(QA_DIR, paste0("cfem_irrecuperaveis_", tolower(mineral_label), ".csv")))
  }
  dist_corr <- univ3 |> dplyr::count(corr, sort = TRUE)
  message("[", mineral_label, "] distribuicao final de 'corr':")
  print(dist_corr)
  readr::write_csv(dist_corr |> dplyr::mutate(mineral = mineral_label, .before = 1),
                   file.path(QA_DIR, paste0("cfem_distribuicao_corr_", tolower(mineral_label), ".csv")))

  cfem_final
}


# ============================================================================
# CASSITERITA -- correcao por fator de 10 contra faixa absoluta
# ("metodo white solder", validado em investigacao_white_solder.R)

#   Este metodo inverte a ordem: filtra PRIMEIRO os fatores que garantem
#   preco dentro de [pmin_kg, pmax_kg], e so entao escolhe o mais conservador
#   (menor |log10(fator)|). E impossivel produzir resultado fora da faixa --
#   ou corrige direito, ou marca dado_corrompido.
#
#   Fases (FASES_CASS): PLG + REQ PLG + AUT PESQUISA.
#     Exclui CONCESSAO DE LAVRA (mineracao industrial), que dominava a serie
#     pre-2018 e tem estrutura de preco distinta do garimpo.
#
#   Dois subsets, MESMA faixa [30,300], processados SEPARADAMENTE:
#     A) ANO >= 2018 -- faixa com respaldo empirico: 51%-94% das declaracoes
#        ja caem em [30,300] SEM correcao alguma (2018:34,5% / 2019:52,3% /
#        2020:51,1% / 2021:73,1% / 2022:84,6% / 2023:69,3% / 2024:77,7% /
#        2025:94,4% / 2026:89,1%).
#     B) ANO <= 2017 -- mesma faixa aplicada, mas aderencia observada de 0%
#        em 2006/2007/2008/2010/2012/2015 (n=145 no total, 7,5% da base).
#        Resultado esperado: alta taxa de dado_corrompido.
# ============================================================================

FASES_CASS <- c(
  "LAVRA GARIMPEIRA",
  "REQUERIMENTO DE LAVRA GARIMPEIRA",
  "AUTORIZAÇÃO DE PESQUISA"
)

ANO_CORTE_CASS <- 2018

# Nucleo do metodo: entre os fatores de 10 que jogam o preco DENTRO da faixa,
# devolve o mais conservador (menor |log10|). Se nenhum resolve, NA + motivo.
#   motivo 0 = sem_dado                 -> peso/valor ausente ou <= 0
#   motivo 1 = dado_corrompido          -> nenhum fator de 10 reconcilia
#   motivo 2 = resolvido
#   motivo 3 = sem_quantidade_declarada -> QTD_MINERIO == 0 na origem

ws_fator_10 <- function(peso_g, valortot, pmin_g, pmax_g, qtd_minerio = NA_real_, fatores = fatores_simples) {
  if (!is.na(qtd_minerio) && qtd_minerio == 0) {
    return(c(fator = NA_real_, motivo = 3))
  }
  if (is.na(peso_g) || is.na(valortot) || peso_g <= 0 || valortot <= 0) {
    return(c(fator = NA_real_, motivo = 0))
  }
  ok <- fatores[ {p <- valortot / (peso_g * fatores); p >= pmin_g & p <= pmax_g} ]
  if (length(ok) == 0) {
    return(c(fator = NA_real_, motivo = 1))
  }
  c(fator = ok[which.min(abs(log10(ok)))], motivo = 2)
}

corrige_cassiterita_ws <- function(cfem_final, pmin_kg = 30, pmax_kg = 300,
                                   subset_label, filtro_ano) {
  pmin_g <- pmin_kg / 1000; pmax_g <- pmax_kg / 1000
  qa_path_checks <- file.path(QA_DIR, "cfem_correcao_checks.csv")
  mineral_label  <- paste0("CASSITERITA_", subset_label)

  universo <- cfem_final |>
    dplyr::filter(SUBSarr == "CASSITERITA", FASE %in% FASES_CASS, filtro_ano(ANO))

  if (nrow(universo) == 0) {
    message("[", mineral_label, "] subset vazio -- nada a fazer.")
    return(cfem_final)
  }

  message("\n### ", mineral_label, " | n = ", nrow(universo), " ###")
  report_check(universo, mineral_label, "CHECK 1 (antes)", pmin_kg, pmax_kg, qa_path_checks)

  ws <- universo |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .r     = list(ws_fator_10(PESO_G, VALORtot, pmin_g, pmax_g,
                                dplyr::if_else("QTD_MINERIO" %in% names(universo),
                                               QTD_MINERIO, NA_real_),
                                fatores = fatores_simples_amplo)),
      fator  = .r[["fator"]],
      motivo = .r[["motivo"]]
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      PESO_G_ws  = dplyr::if_else(!is.na(fator), PESO_G * fator, PESO_G),
      PESO_KG_ws = PESO_G_ws / 1000,
      preco_g_ws = dplyr::if_else(!is.na(PESO_G_ws) & PESO_G_ws > 0,
                                  VALORtot / PESO_G_ws, NA_real_),
      corr_ws = dplyr::case_when(
        motivo == 3                ~ "sem_quantidade_declarada",
        motivo %in% c(0, 1)        ~ "dado_corrompido",
        !is.na(fator) & fator == 1 ~ "original",
        TRUE                       ~ paste0("pow10_p", round(log10(fator)))
      )
    ) |>
    dplyr::select(row_id, PESO_G_ws, PESO_KG_ws, preco_g_ws, corr_ws)

  cfem_final <- cfem_final |>
    dplyr::left_join(ws, by = "row_id") |>
    dplyr::mutate(
      PESO_G_final  = dplyr::if_else(!is.na(PESO_G_ws),  PESO_G_ws,  PESO_G_final),
      PESO_KG_final = dplyr::if_else(!is.na(PESO_KG_ws), PESO_KG_ws, PESO_KG_final),
      preco_g_final = dplyr::if_else(!is.na(preco_g_ws), preco_g_ws, preco_g_final),
      corr          = dplyr::if_else(!is.na(corr_ws),    corr_ws,    corr)
    ) |>
    dplyr::select(-PESO_G_ws, -PESO_KG_ws, -preco_g_ws, -corr_ws)

  univ_pos <- cfem_final |>
    dplyr::filter(SUBSarr == "CASSITERITA", FASE %in% FASES_CASS, filtro_ano(ANO))

  report_check(univ_pos, mineral_label, "CHECK 2 (final)", pmin_kg, pmax_kg, qa_path_checks)

  dist_corr <- univ_pos |> dplyr::count(corr, sort = TRUE)
  message("[", mineral_label, "] distribuicao final de 'corr':")
  print(dist_corr)
  readr::write_csv(dist_corr |> dplyr::mutate(mineral = mineral_label, .before = 1),
                   file.path(QA_DIR, paste0("cfem_distribuicao_corr_", tolower(mineral_label), ".csv")))

  nao_corrigidos <- univ_pos |>
    dplyr::filter(corr %in% c("dado_corrompido", "sem_quantidade_declarada"))
  if (nrow(nao_corrigidos) > 0) {
    message("[", mineral_label, "] nao corrigidos: ", nrow(nao_corrigidos),
            " de ", nrow(univ_pos),
            " (", round(100 * nrow(nao_corrigidos) / nrow(univ_pos), 1), "%)")
    print(dplyr::count(nao_corrigidos, corr))
    readr::write_csv(
      nao_corrigidos |>
        dplyr::select(dplyr::any_of(c("PROCESSO", "FASE", "ANO", "MES", "NOME_arr",
                                      "TITULAR", "UM", "QTD_MINERIO", "PESO_KG",
                                      "VALORarr", "VALORtot", "corr"))) |>
        dplyr::mutate(mineral = mineral_label, .before = 1),
      file.path(QA_DIR, paste0("cfem_nao_corrigidos_", tolower(mineral_label), ".csv")))
  }

  cfem_final
}

# Metodo definido (decisao 2026-08): white solder com alcance ampliado
# (fatores_simples_amplo, +-20) contra a faixa absoluta -- sem mediana
# hierarquica, sem limiar de grupo. Testado contra mediana hierarquica
# (2 perfis de limiar + banda estreita) em checks/teste_correcao_
# cassiterita_metodos.R: o alcance ampliado sozinho resolveu os mesmos
# casos, incluindo o pior caso conhecido (2021, preco caindo pra
# ~0,00000000000224 R$/g sem correcao -- 33,4 R$/kg depois), sem herdar
# o risco de "vizinho contaminado" que a mediana tem pra essa substancia
# (cassiterita e so ~3,4% do volume do ouro nas mesmas fases).
RODAR_CASSITERITA <- TRUE

if (RODAR_CASSITERITA) {
  # --- Subset A: 2018+ | faixa [30, 300] R$/kg --------------------------------
  cfem_final <- corrige_cassiterita_ws(
    cfem_final, pmin_kg = 30, pmax_kg = 300,
    subset_label = "2018MAIS",
    filtro_ano   = function(a) a >= ANO_CORTE_CASS
  )

  # --- Subset B: ate 2017 | faixa [5, 50] R$/kg -------------------------------
  cfem_final <- corrige_cassiterita_ws(
    cfem_final, pmin_kg = 5, pmax_kg = 50,
    subset_label = "ATE2017",
    filtro_ano   = function(a) a < ANO_CORTE_CASS
  )
} else {
  message("[06][cassiterita] RODAR_CASSITERITA = FALSE -- pulando correcao (cfem_final continua 'original' pra CASSITERITA).")
}

# OURO (30.000-1.000.000 R$/kg)
cfem_final <- corrige_mineral_3checks(cfem_final, "OURO", subs_keep = "OURO", subs_col = "SUBSarrSIM",
                                      pmin_kg = 30 * 1000, pmax_kg = 1000 * 1000, min_med_plaus = 30, max_med_plaus = 1000)

# ============================================================================
# COLUMBITA (grupo NIOBIO) -- correcao white solder ampliada, faixa unica
# =============================================================================
# 99,4% do grupo NIOBIO nas fases de interesse (536 de 539 declaracoes);
# MINERIO DE NIOBIO (n=3) fica de fora, amostra irrelevante -- ver
# checks/verificacao_niobio.R.
#
# Faixa [20,200] R$/kg validada em checks/teste_correcao_niobio_faixas.R:
# de 3 faixas candidatas testadas, foi a UNICA em que os 2 CNPJ do maior
# declarante (Areia Preta Metais LTDA, matriz+filial, 59,6% do volume
# 2019+) convergiram pro mesmo preco corrigido (R$85,53 vs R$84,55/kg --
# as outras 2 faixas testadas davam resultados incoerentes entre os dois
# CNPJ da MESMA empresa, alem de "empurrar" muitos valores pro limite da
# faixa, sinal de faixa mal calibrada).
#
# Decisao 2026-08: corrige SO 2019+ (505 declaracoes). Antes de 2019 sao
# so 31 declaracoes (21 delas de um unico ano, 2006) -- amostra pequena
# demais pra rodar o mesmo teste de consistencia entre declarantes, entao
# fica SEM correcao (corr = "original") ate termos evidencia melhor pra
# validar uma faixa pro periodo antigo.
# ============================================================================

FASES_NIOBIO     <- FASES_CORR_PADRAO
ANO_CORTE_NIOBIO <- 2019

corrige_columbita_ws <- function(cfem_final, pmin_kg = 20, pmax_kg = 200) {
  qa_path_checks <- file.path(QA_DIR, "cfem_correcao_checks.csv")
  pmin_g <- pmin_kg / 1000; pmax_g <- pmax_kg / 1000

  universo <- cfem_final |>
    dplyr::filter(SUBSarr == "COLUMBITA", FASE %in% FASES_NIOBIO, ANO >= ANO_CORTE_NIOBIO)

  if (nrow(universo) == 0) {
    message("[COLUMBITA] subset vazio -- nada a fazer.")
    return(cfem_final)
  }

  message("\n### COLUMBITA (", ANO_CORTE_NIOBIO, "+) | n = ", nrow(universo), " ###")
  report_check(universo, "COLUMBITA", "CHECK 1 (antes)", pmin_kg, pmax_kg, qa_path_checks)

  ws <- universo |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .r     = list(ws_fator_10(PESO_G, VALORtot, pmin_g, pmax_g,
                                dplyr::if_else("QTD_MINERIO" %in% names(universo), QTD_MINERIO, NA_real_),
                                fatores = fatores_simples_amplo)),
      fator  = .r[["fator"]], motivo = .r[["motivo"]]
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      PESO_G_ws  = dplyr::if_else(!is.na(fator), PESO_G * fator, PESO_G),
      PESO_KG_ws = PESO_G_ws / 1000,
      preco_g_ws = dplyr::if_else(!is.na(PESO_G_ws) & PESO_G_ws > 0, VALORtot / PESO_G_ws, NA_real_),
      corr_ws = dplyr::case_when(
        motivo == 3                ~ "sem_quantidade_declarada",
        motivo %in% c(0, 1)        ~ "dado_corrompido",
        !is.na(fator) & fator == 1 ~ "original",
        TRUE                       ~ paste0("pow10_p", round(log10(fator)))
      )
    ) |>
    dplyr::select(row_id, PESO_G_ws, PESO_KG_ws, preco_g_ws, corr_ws)

  cfem_final <- cfem_final |>
    dplyr::left_join(ws, by = "row_id") |>
    dplyr::mutate(
      PESO_G_final  = dplyr::if_else(!is.na(PESO_G_ws),  PESO_G_ws,  PESO_G_final),
      PESO_KG_final = dplyr::if_else(!is.na(PESO_KG_ws), PESO_KG_ws, PESO_KG_final),
      preco_g_final = dplyr::if_else(!is.na(preco_g_ws), preco_g_ws, preco_g_final),
      corr          = dplyr::if_else(!is.na(corr_ws),    corr_ws,    corr)
    ) |>
    dplyr::select(-PESO_G_ws, -PESO_KG_ws, -preco_g_ws, -corr_ws)

  univ_pos <- cfem_final |>
    dplyr::filter(SUBSarr == "COLUMBITA", FASE %in% FASES_NIOBIO, ANO >= ANO_CORTE_NIOBIO)
  report_check(univ_pos, "COLUMBITA", "CHECK 2 (final)", pmin_kg, pmax_kg, qa_path_checks)

  dist_corr <- univ_pos |> dplyr::count(corr, sort = TRUE)
  message("[COLUMBITA] distribuicao final de 'corr':")
  print(dist_corr)
  readr::write_csv(dist_corr |> dplyr::mutate(mineral = "COLUMBITA", .before = 1),
                   file.path(QA_DIR, "cfem_distribuicao_corr_columbita.csv"))

  n_pre2019 <- cfem_final |>
    dplyr::filter(SUBSarr == "COLUMBITA", FASE %in% FASES_NIOBIO, ANO < ANO_CORTE_NIOBIO) |>
    nrow()
  message(sprintf(
    "[COLUMBITA] pre-%d (%d declaracoes) NAO corrigido -- amostra insuficiente pra validar faixa (ver checks/teste_correcao_niobio_faixas.R).",
    ANO_CORTE_NIOBIO, n_pre2019
  ))

  cfem_final
}

RODAR_COLUMBITA <- TRUE
if (RODAR_COLUMBITA) {
  cfem_final <- corrige_columbita_ws(cfem_final)
} else {
  message("[06][columbita] RODAR_COLUMBITA = FALSE -- pulando correcao.")
}

                                      
# cfem_correcao_extrema -- sinaliza correcao de peso incerta/agressiva.
cfem_final <- cfem_final |>
  dplyr::mutate(
    cfem_correcao_extrema = dplyr::case_when(
      is.na(corr)                                     ~ 0L,
      stringr::str_starts(corr, "simples_")           ~ 1L,
      corr == "dado_corrompido"                       ~ 1L,
      corr == "sem_quantidade_declarada"              ~ 1L,
      stringr::str_detect(corr, "^pow10_p-?([3-9]|[1-9][0-9]+)$") ~ 1L,
      TRUE                                            ~ 0L
    )
  )

n_extrema <- sum(cfem_final$cfem_correcao_extrema, na.rm = TRUE)
message(sprintf("[CFEM] registros marcados como correcao extrema (cfem_correcao_extrema=1): %d", n_extrema))

cfem_final <- cfem_final |>
  dplyr::mutate(
    ULT_EV_ID  = stringr::str_extract(ULT_EVENTO, "^\\d+"),
    ULT_EV_DAT = as.Date(stringr::str_extract(ULT_EVENTO, "\\d{2}/\\d{2}/\\d{4}$"), format = "%d/%m/%Y"),
    ULT_EV_DES = stringr::str_trim(stringr::str_remove_all(ULT_EVENTO, paste0(ULT_EV_ID, " - |EM ", ULT_EV_DAT)))
  ) |>
  dplyr::select(-dplyr::any_of("row_id"))

pma_muni <- as.data.frame(load_ckpt("05_pma_munic")) |> dplyr::select(PROCESSO, munic_pma = munic, uf_pma = uf)

cfem_final <- cfem_final |>
  dplyr::left_join(pma_muni, by = "PROCESSO") |>
  dplyr::mutate(
    muni_falta = is.na(name_muni) | name_muni == "" | code_muni == 0 | code_muni == "0",
    preencheu_pma = muni_falta & !is.na(munic_pma),
    name_muni    = dplyr::if_else(preencheu_pma, munic_pma, name_muni),
    abbrev_state = dplyr::if_else(preencheu_pma & (is.na(abbrev_state) | abbrev_state == ""), uf_pma, abbrev_state),
    muni_fonte_cfem = dplyr::if_else(preencheu_pma, "herdado_pma", "cfem_original")
  ) |>
  dplyr::select(-munic_pma, -uf_pma, -muni_falta, -preencheu_pma)

save_ckpt(cfem_final,    "05_cfem_final")
save_ckpt(cfem_aut_amzl, "05_cfem_aut_amzl")

# ---- CHECKS ----
# Cassiterita
# Ouro
# VISUALIZAÇÃO 1 ==============================================================
df_temporal_unificado <- cfem_final |>
  dplyr::filter(SUBSarrSIM %in% c("OURO") | SUBSarr == "CASSITERITA") |>
  dplyr::filter(str_detect(toupper(FASE), "GARIMPEIRA")) |> 
  dplyr::mutate(
    substancia_plot = factor(dplyr::if_else(SUBSarr == "CASSITERITA", "CASSITERITA", "OURO"), 
                             levels = c("CASSITERITA", "OURO")),
    data = as.Date(sprintf("%04d-%02d-01", ANO, MES))
  ) |>
  dplyr::group_by(substancia_plot, data) |>
  dplyr::summarise(
    `1. Valor Arrecadado (R$)_Orig`   = sum(VALORarr, na.rm = TRUE),
    `1. Valor Arrecadado (R$)_Corr`   = sum(VALORarr, na.rm = TRUE),
    `2. Peso Declarado (Kg)_Orig`     = sum(PESO_KG, na.rm = TRUE),
    `2. Peso Declarado (Kg)_Corr`     = sum(PESO_KG_final, na.rm = TRUE),
    `3. Relação (R$/Kg)_Orig` = dplyr::if_else(sum(PESO_KG, na.rm = TRUE) > 0, 
                                               sum(VALORtot, na.rm = TRUE) / sum(PESO_KG, na.rm = TRUE), 
                                               NA_real_),
    `3. Relação (R$/Kg)_Corr` = dplyr::if_else(sum(PESO_KG_final, na.rm = TRUE) > 0, 
                                               sum(VALORtot, na.rm = TRUE) / sum(PESO_KG_final, na.rm = TRUE), 
                                               NA_real_),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    cols = -c(substancia_plot, data),
    names_to = c("metrica", "cenario"),
    names_pattern = "(.*)_(Orig|Corr)",
    values_to = "valor_metrica"
  ) |>
  dplyr::mutate(
    cenario = dplyr::if_else(cenario == "Orig", "Antes (Original)", "Depois (pow10)"),
    label_cor = dplyr::case_when(
      str_detect(metrica, "Valor") ~ "Valor Arrecadado (Inalterado)",
      str_detect(metrica, "Peso") & cenario == "Antes (Original)" ~ "Peso Original",
      str_detect(metrica, "Peso") & cenario == "Depois (pow10)" ~ "Peso Depois (pow10)",
      str_detect(metrica, "Relação") & cenario == "Antes (Original)" ~ "Relação R$/kg (original)",
      str_detect(metrica, "Relação") & cenario == "Depois (pow10)" ~ "Relação R$/kg (depois)"
    )
  ) |>
  dplyr::filter(!is.na(valor_metrica) & valor_metrica > 0)

cores_customizadas <- c(
  "Valor Arrecadado (Inalterado)" = "#1e3799",
  "Peso Original"                 = "#e74c3c",
  "Peso Depois (pow10)"           = "#27ae60",
  "Relação R$/kg (original)"      = "#e74c3c",
  "Relação R$/kg (depois)"        = "#8e44ad" 
)

p_linhas <- ggplot(df_temporal_unificado, aes(x = data, y = valor_metrica, 
                                          color = label_cor, 
                                          linetype = cenario, 
                                          alpha = cenario)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.4) +
  scale_y_log10(labels = scales::label_comma()) +
  facet_grid(metrica ~ substancia_plot, scales = "free_y") +
  scale_color_manual(values = cores_customizadas) + 
  scale_linetype_manual(values = c("Antes (Original)" = "dashed", "Depois (pow10)" = "solid")) + 
  scale_alpha_manual(values = c("Antes (Original)" = 0.4, "Depois (pow10)" = 1.0)) +            
  theme_bw() +
  labs(x = "", y = "Valores em Escala Log10", color = "") +
  guides(color = guide_legend(title = NULL, nrow = 1), linetype = "none", alpha = "none") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# [EXPORTAÇÃO 1]
ggsave(filename = file.path(QA_DIR, "01_serie_temporal_unificada.png"), 
       plot = p_linhas, width = 11, height = 8.5, dpi = 300)

# VISUALIZAÇÃO 2 ==============================================================
df_scatterplot_clean <- cfem_final |>
  dplyr::filter(SUBSarrSIM %in% c("OURO") | SUBSarr == "CASSITERITA") |>
  dplyr::filter(str_detect(toupper(FASE), "GARIMPEIRA")) |>
  dplyr::mutate(
    substancia_plot = factor(dplyr::if_else(SUBSarr == "CASSITERITA", "CASSITERITA", "OURO"), 
                             levels = c("CASSITERITA", "OURO")),
    data = as.Date(sprintf("%04d-%02d-01", ANO, MES)),
    status_ponto = dplyr::if_else(corr == "original", "Dado Original Correto", "Corrigido pelo Algoritmo (pow10)")
  ) |>
  dplyr::filter(!is.na(preco_g_orig) & !is.na(preco_g_final) & preco_g_orig > 0 & preco_g_final > 0) |>
  tidyr::pivot_longer(
    cols = c(preco_g_orig, preco_g_final),
    names_to = "cenario",
    values_to = "preco_individual"
  ) |>
  dplyr::mutate(
    cenario = factor(dplyr::if_else(cenario == "preco_g_orig", "Antes (Original)", "Depois (pow10)"),
                     levels = c("Antes (Original)", "Depois (pow10)"))
  )

cores_scatterplot <- c(
  "Dado Original Correto"           = "#34495e",
  "Corrigido pelo Algoritmo (pow10)" = "#e74c3c"
)

p_scatter <- ggplot(df_scatterplot_clean, aes(x = data, y = preco_individual, color = status_ponto)) +
  geom_point(alpha = 0.4, size = 1.0) +
  scale_y_log10(labels = scales::label_comma(suffix = " R$/g")) +
  facet_grid(substancia_plot ~ cenario, scales = "free_y") +
  scale_color_manual(values = cores_scatterplot) +
  theme_bw() +
  labs(x = "", y = "R$/kg (Escala Log10)", color = "") +
  guides(color = guide_legend(title = NULL, nrow = 1)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )
# [EXPORTAÇÃO 2]
ggsave(filename = file.path(QA_DIR, "02_scatterplot_precos.png"), 
       plot = p_scatter, width = 11, height = 7, dpi = 300)


# VISUALIZAÇÃO 3 ==============================================================
prep_violino <- function(df) {
  df |>
    filter(PESO_KG > 0, PESO_KG_final > 0, VALORtot > 0, FASE %in% FASES_CORR) |>
    mutate(
      `Antes`  = VALORtot / PESO_KG,
      `Depois` = VALORtot / PESO_KG_final
    ) |>
    pivot_longer(c(Antes, Depois), names_to = "cenario", values_to = "rs_kg") |>
    mutate(cenario = factor(cenario, levels = c("Antes", "Depois")))
}

faixa_ref <- function(pmin, pmax) {
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = pmin, ymax = pmax,
           alpha = 0.08, fill = "forestgreen")
}

p_violino_cassiterita <- prep_violino(cfem_final |> filter(SUBSarr == "CASSITERITA")) |>
  ggplot(aes(cenario, rs_kg, fill = cenario)) +
  faixa_ref(30, 300) +
  geom_violin(scale = "width", alpha = 0.8, color = NA) +
  geom_boxplot(width = 0.12, outlier.size = 0.4, alpha = 0.6) +
  facet_wrap(~ FASE, scales = "free_x", nrow = 1) + 
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = c("Antes" = "#e74c3c", "Depois" = "#2D6A4F")) +
  labs(#title = "Cassiterita — preço implícito (R$/kg) por fase, antes e depois da correção",
       x = NULL, y = "R$/kg (log)", fill = NULL) +
  theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 11))

# [EXPORTAÇÃO 3]
ggsave(filename = file.path(QA_DIR, "03_violino_cassiterita.png"), 
       plot = p_violino_cassiterita, width = 12, height = 5, dpi = 300)

# VISUALIZAÇÃO 4 ==============================================================
p_violino_ouro <- prep_violino(cfem_final |> filter(SUBSarrSIM == "OURO")) |>
  ggplot(aes(cenario, rs_kg, fill = cenario)) +
  faixa_ref(30000, 1000000) +
  geom_violin(scale = "width", alpha = 0.8, color = NA) +
  geom_boxplot(width = 0.12, outlier.size = 0.4, alpha = 0.6) +
  # O 'nrow = 1' força todas as fases a ficarem lado a lado
  facet_wrap(~ FASE, scales = "free_x", nrow = 1) + 
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = c("Antes" = "#e74c3c", "Depois" = "#1e3799")) +
  labs(#title = "Ouro — preço implícito (R$/kg) por fase, antes e depois da correção",
       x = NULL, y = "R$/kg (log)", fill = NULL) +
  theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 11))

# [EXPORTAÇÃO 4]
ggsave(filename = file.path(QA_DIR, "04_violino_ouro.png"), 
       plot = p_violino_ouro, width = 12, height = 5, dpi = 300)

# VISUALIZAÇÃO 5 ==============================================================
# Boxplot do preco por grama do ouro pos-correcao, por fase -- foco nos
# extremos (pedido 2026-08).
ouro_preco_g <- cfem_final |>
  dplyr::filter(SUBSarrSIM == "OURO", FASE %in% FASES_CORR,
                !is.na(preco_g_final), preco_g_final > 0)

p_box_ouro_preco_g <- ggplot(ouro_preco_g, aes(x = FASE, y = preco_g_final)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 30000 / 1000, ymax = 1000000 / 1000,
           alpha = 0.08, fill = "forestgreen") +
  geom_boxplot(outlier.size = 0.6, alpha = 0.7, fill = "#1e3799") +
  scale_y_log10(labels = scales::comma) +
  labs(x = NULL, y = "R$/grama (log)",
       title = "Ouro — preço por grama pós-correção, por fase") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(face = "bold", size = 11))

# [EXPORTAÇÃO 5]
ggsave(filename = file.path(QA_DIR, "05_boxplot_preco_grama_ouro.png"),
       plot = p_box_ouro_preco_g, width = 10, height = 6, dpi = 300)

# --- Resumo do preco/grama, com destaque pros extremos -----------------------
resumo_preco_g_ouro <- ouro_preco_g |>
  dplyr::summarise(
    n       = dplyr::n(),
    min     = min(preco_g_final),
    p01     = quantile(preco_g_final, 0.01),
    p25     = quantile(preco_g_final, 0.25),
    mediana = median(preco_g_final),
    p75     = quantile(preco_g_final, 0.75),
    p99     = quantile(preco_g_final, 0.99),
    max     = max(preco_g_final)
  )
message("[06][ouro] resumo do preco por grama pos-correcao (R$/g):")
print(resumo_preco_g_ouro)
readr::write_csv(resumo_preco_g_ouro, file.path(QA_DIR, "05_resumo_preco_grama_ouro.csv"))

# 10 maiores e 10 menores precos/grama -- pra inspecionar o que esta puxando
# os extremos (declaracao especifica, nao so o numero agregado).
extremos_preco_g_ouro <- dplyr::bind_rows(
  ouro_preco_g |> dplyr::slice_min(preco_g_final, n = 10) |> dplyr::mutate(extremo = "minimo"),
  ouro_preco_g |> dplyr::slice_max(preco_g_final, n = 10) |> dplyr::mutate(extremo = "maximo")
) |>
  dplyr::select(extremo, PROCESSO, ANO, MES, FASE, corr, PESO_KG, PESO_KG_final,
                VALORtot, preco_g_final)
readr::write_csv(extremos_preco_g_ouro, file.path(QA_DIR, "05_extremos_preco_grama_ouro.csv"))
message("[06][ouro] 10 maiores e 10 menores preco/grama em: ",
        file.path(QA_DIR, "05_extremos_preco_grama_ouro.csv"))

# VISUALIZAÇÃO 6 ==============================================================
# Minimo/maximo do preco final por ano -- populacao INTEIRA (nao so quem foi
# corrigido), pra acompanhar se ainda sobra declaracao feia depois da
# correcao. Ouro em R$/grama (unidade natural do ouro); cassiterita em
# R$/kg (unidade natural da cassiterita -- decisao 2026-08). "Nao resolvido"
# entra tambem: o preco final nesse caso fica igual ao original (nunca vira
# NA), entao o extremo aparece se ainda tiver algo feio sem corrigir.

extremos_ouro_por_ano <- cfem_final |>
  dplyr::filter(SUBSarrSIM == "OURO", FASE %in% FASES_CORR, !is.na(preco_g_final)) |>
  dplyr::group_by(ANO) |>
  dplyr::summarise(n = dplyr::n(), min_preco_g = min(preco_g_final), max_preco_g = max(preco_g_final), .groups = "drop") |>
  dplyr::arrange(ANO)
readr::write_csv(extremos_ouro_por_ano, file.path(QA_DIR, "06_extremos_ouro_por_ano.csv"))
message("\n[06][ouro] min/max de preco por grama, por ano:")
print(extremos_ouro_por_ano, n = Inf)

extremos_cassiterita_por_ano <- cfem_final |>
  dplyr::filter(SUBSarr == "CASSITERITA", FASE %in% FASES_CASS,
                !is.na(PESO_KG_final), PESO_KG_final > 0, !is.na(VALORtot)) |>
  dplyr::mutate(preco_kg_final = VALORtot / PESO_KG_final) |>
  dplyr::group_by(ANO) |>
  dplyr::summarise(n = dplyr::n(), min_preco_kg = min(preco_kg_final), max_preco_kg = max(preco_kg_final), .groups = "drop") |>
  dplyr::arrange(ANO)
readr::write_csv(extremos_cassiterita_por_ano, file.path(QA_DIR, "06_extremos_cassiterita_por_ano.csv"))
message("\n[06][cassiterita] min/max de preco por kg, por ano:")
print(extremos_cassiterita_por_ano, n = Inf)


# =============================================================================
# BLOCO 7 — AGREGAÇÕES DA CFEM POR PROCESSO
# =============================================================================

cfem_final <- load_ckpt("05_cfem_final")
pma_tp     <- load_ckpt("05_pma_tp")

cfem_final <- cfem_final |>
  dplyr::mutate(
    tipo_caso = dplyr::case_when(
      corr == "sem_quantidade_declarada" ~ "sem_quantidade",
      corr == "dado_corrompido"          ~ "nao_reconciliavel",
      TRUE                               ~ "correcao_aplicada"
    ),
    resolvido = tipo_caso == "correcao_aplicada",
    PESO_KG_final_limpo = dplyr::if_else(resolvido, PESO_KG_final, NA_real_),
    PESO_G_final_limpo  = dplyr::if_else(resolvido, PESO_G_final,  NA_real_),
    foco = categorizar_foco(SUBSarr, SUBSarrSIM)
  )
save_ckpt(cfem_final, "05_cfem_final")

# --- Agregacao por processo x foco -------------------------------------------

# Um processo que declara CFEM em 2-3 focos (ex: registrado
# CASSITERITA no PMA mas tambem declara ILMENITA/OURO) vira 2-3 linhas aqui,
# cada uma com o peso/valor SO daquele foco -- sem misturar substancias de
# magnitude completamente diferente no mesmo numero.

# --- Separar processos com 1 foco vs 2-3 focos (decisao 2026-07-20) --------
n_foco_por_processo <- cfem_final |>
  dplyr::distinct(PROCESSO, foco) |>
  dplyr::count(PROCESSO, name = "n_foco_distintos")

cfem_final <- cfem_final |>
  dplyr::left_join(n_foco_por_processo, by = "PROCESSO")

cfem_single <- cfem_final |> dplyr::filter(n_foco_distintos == 1)
cfem_multi  <- cfem_final |> dplyr::filter(n_foco_distintos > 1)

message(sprintf(
  "[06][foco] processos com 1 substancia (CFEM): %d | com 2-3 substancias: %d",
  dplyr::n_distinct(cfem_single$PROCESSO), dplyr::n_distinct(cfem_multi$PROCESSO)
))

# --- Funcao de agregacao por PROCESSO x foco --------------------------------
agrega_por_processo_foco <- function(df) {
  df |>
    dplyr::arrange(PROCESSO, ANO, MES) |>
    dplyr::group_by(PROCESSO, foco) |>
    dplyr::summarise(
      cfem_arr  = 1L,
      arr_kg_T  = sum(PESO_KG_final_limpo, na.rm = TRUE),
      arr_kg_L  = dplyr::if_else(all(is.na(PESO_KG_final_limpo)), NA_real_, dplyr::last(na.omit(PESO_KG_final_limpo))),
      arr_g_T   = sum(PESO_G_final_limpo,  na.rm = TRUE),
      arr_g_L   = dplyr::if_else(all(is.na(PESO_G_final_limpo)),  NA_real_, dplyr::last(na.omit(PESO_G_final_limpo))),
      arr_val_T = sum(VALORarr, na.rm = TRUE),
      arr_val_L = dplyr::if_else(all(is.na(VALORarr)), NA_real_, dplyr::last(na.omit(VALORarr))),
      arr_dt_F  = as.character(min(data, na.rm = TRUE)),
      arr_dt_L  = as.character(max(data, na.rm = TRUE)),
      arr_ndcl  = dplyr::n(),
      arr_nbuy  = dplyr::n_distinct(CPF_CNPJarr, na.rm = TRUE),
      arr_topb  = get_mode(NOME_arr),
      # --- transparencia: o que ficou de fora do total limpo acima ---------
      n_dado_corrompido = sum(tipo_caso == "nao_reconciliavel"),
      n_sem_quantidade  = sum(tipo_caso == "sem_quantidade"),
      kg_excluido       = sum(PESO_KG[!resolvido], na.rm = TRUE),
      tem_dado_problema = as.integer(n_dado_corrompido > 0 | n_sem_quantidade > 0),
      .groups = "drop"
    ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2)))
}

arr_corr_long <- dplyr::bind_rows(
  agrega_por_processo_foco(cfem_single),
  agrega_por_processo_foco(cfem_multi)
)

arr_corr_pma <- arr_corr_long |>
  dplyr::select(PROCESSO, foco, cfem_arr, arr_kg_T, arr_kg_L, arr_g_T, arr_g_L,
                arr_val_T, arr_val_L, arr_dt_F, arr_dt_L, arr_ndcl, arr_nbuy, arr_topb)

arr_corr_qa_foco <- arr_corr_long |>
  dplyr::select(PROCESSO, foco, n_dado_corrompido, n_sem_quantidade, kg_excluido, tem_dado_problema)

readr::write_csv(arr_corr_qa_foco, file.path(QA_DIR_CORR, "problemas_por_processo_foco.csv"))
message(sprintf("[06][auditoria] problemas_por_processo_foco.csv: %d linhas (processo x foco)", nrow(arr_corr_qa_foco)))

# =============================================================================
# AUDITORIA DA CORRECAO PONTUAL DE CFEM
# =============================================================================
message("[06][auditoria] gerando log de correcao pontual (antigo 05b)...")

extremos <- cfem_final |>
  dplyr::filter(cfem_correcao_extrema == 1) |>
  dplyr::mutate(
    fator_correcao = dplyr::if_else(
      !is.na(PESO_KG) & PESO_KG > 0 & resolvido,
      PESO_KG_final / PESO_KG, NA_real_),
    expoente_10     = round(log10(fator_correcao)),
    direcao         = dplyr::case_when(
      tipo_caso == "sem_quantidade"     ~ "sem_quantidade",
      tipo_caso == "nao_reconciliavel"  ~ "nao_reconciliavel",
      is.na(expoente_10)                ~ NA_character_,
      expoente_10 < 0                   ~ "dividir",
      expoente_10 > 0                   ~ "multiplicar",
      TRUE                               ~ "nenhum"
    ),
    casas_decimais  = abs(expoente_10)
  )

log_correcao_pontual <- extremos |>
  dplyr::select(PROCESSO, SUBSarr, tipo_caso, CPF_CNPJarr, NOME_arr, ANO, MES, UM,
                QTD_MINERIO, PESO_KG, PESO_KG_final, fator_correcao,
                expoente_10, direcao, casas_decimais,
                preco_g_orig, preco_g_final, corr) |>
  dplyr::arrange(SUBSarr, tipo_caso, dplyr::desc(casas_decimais))
readr::write_csv(log_correcao_pontual, file.path(QA_DIR_CORR, "log_correcao_pontual_cfem.csv"))

resumo_expoente <- extremos |> dplyr::count(SUBSarr, tipo_caso, direcao, expoente_10, sort = TRUE)
readr::write_csv(resumo_expoente, file.path(QA_DIR_CORR, "resumo_expoente.csv"))

moda_expoente <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  tb <- sort(table(x), decreasing = TRUE)
  names(tb)[1]
}

resumo_declarante <- extremos |>
  dplyr::group_by(SUBSarr, CPF_CNPJarr, NOME_arr) |>
  dplyr::summarise(
    n_registros           = dplyr::n(),
    n_corr_aplicada       = sum(tipo_caso == "correcao_aplicada", na.rm = TRUE),
    n_nao_reconciliavel   = sum(tipo_caso == "nao_reconciliavel", na.rm = TRUE),
    n_sem_quantidade      = sum(tipo_caso == "sem_quantidade", na.rm = TRUE),
    n_expoentes_distintos = dplyr::n_distinct(expoente_10, na.rm = TRUE),
    expoente_predominante = moda_expoente(expoente_10),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(n_registros))
readr::write_csv(resumo_declarante, file.path(QA_DIR_CORR, "resumo_declarante.csv"))

message(sprintf("[06][auditoria] registros no log de auditoria: %d", nrow(log_correcao_pontual)))

# --- Cassiterita nao corrigida
cassiterita_nao_corrigida_proc <- cfem_final |>
  dplyr::filter(SUBSarr == "CASSITERITA", tipo_caso %in% c("nao_reconciliavel", "sem_quantidade")) |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(
    n_declaracoes_problema = dplyr::n(),
    n_nao_reconciliavel    = sum(tipo_caso == "nao_reconciliavel"),
    n_sem_quantidade       = sum(tipo_caso == "sem_quantidade"),
    anos_afetados          = paste(sort(unique(ANO)), collapse = ";"),
    kg_original_total      = round(sum(PESO_KG, na.rm = TRUE), 2),
    valor_total            = round(sum(VALORtot, na.rm = TRUE), 2),
    .groups = "drop"
  )
readr::write_csv(cassiterita_nao_corrigida_proc, file.path(QA_DIR_CORR, "cassiterita_nao_corrigida.csv"))
message(sprintf("[06][auditoria] cassiterita_nao_corrigida.csv: %d processos afetados",
                nrow(cassiterita_nao_corrigida_proc)))

message("\n=== auditoria de correcao pontual (antigo 05b) — CONCLUIDA ===\n")

pma_tp <- pma_tp |>
  dplyr::mutate(
    SUBSpmaGRP = classificar_grupo(SUBS),
    ULT_EV_ID   = stringr::str_extract(ULT_EVENTO, "^\\d+"),
    ULT_EV_DAT_txt = stringr::str_extract(ULT_EVENTO, "\\d{2}/\\d{2}/\\d{4}$"),
    ULT_EV_DES  = stringr::str_trim(stringr::str_remove_all(ULT_EVENTO, paste0(ULT_EV_ID, " - |EM ", ULT_EV_DAT_txt))),
    ULT_EV_DAT  = as.Date(ULT_EV_DAT_txt, format = "%d/%m/%Y")
  ) |>
  dplyr::select(-ULT_EV_DAT_txt)

cfem_aut_amzl <- load_ckpt("05_cfem_aut_amzl")

aut_unique <- cfem_aut_amzl |>
  dplyr::group_by(PROCESSO) |>
  dplyr::summarise(cfem_aut = 1L, aut_val_T = round(sum(VALORaut, na.rm = TRUE), 2), aut_n = dplyr::n(), .groups = "drop")

pma_full <- pma_tp |>
  tidyterra::left_join(arr_corr_pma, by = "PROCESSO") |>
  tidyterra::left_join(aut_unique,   by = "PROCESSO") |>
  dplyr::mutate(
    cfem_arr  = tidyr::replace_na(cfem_arr, 0L),
    cfem_aut  = tidyr::replace_na(cfem_aut, 0L),
    arr_kg_T  = tidyr::replace_na(arr_kg_T, 0),
    arr_g_T   = tidyr::replace_na(arr_g_T,  0),
    arr_val_T = tidyr::replace_na(arr_val_T, 0),
    arr_ndcl  = tidyr::replace_na(arr_ndcl, 0L),
    arr_nbuy  = tidyr::replace_na(arr_nbuy, 0L),
    aut_val_T = tidyr::replace_na(aut_val_T, 0),
    aut_n     = tidyr::replace_na(aut_n, 0L)
  )
# NOTA (2026-07-20): pma_tp tem 1 linha/processo; arr_corr_pma tem 1 linha
# por PROCESSO x foco (1 a 3). O left_join acima duplica naturalmente a
# geometria/atributos do poligono para cada foco declarado -- e o motivo de
# querermos "1 linha por poligono x substancia". Processo sem CFEM nenhum
# fica com 1 linha so, foco = NA (nao ha declaracao pra classificar).

save_ckpt(pma_full, "05_pma_full")

# --- Cassiterita nao corrigida: export SHP (1 linha/processo, com geometria) -
if (nrow(cassiterita_nao_corrigida_proc) > 0) {
  pma_cassiterita_nao_corrigida <- pma_full |>
    dplyr::filter(PROCESSO %in% cassiterita_nao_corrigida_proc$PROCESSO) |>
    dplyr::distinct(PROCESSO, .keep_all = TRUE) |>
    dplyr::select(PROCESSO, TITULAR, FASE, SUBS, AREA_HA) |>
    tidyterra::left_join(cassiterita_nao_corrigida_proc, by = "PROCESSO")

  terra::writeVector(pma_cassiterita_nao_corrigida,
                      file.path(QA_DIR_CORR, "cassiterita_nao_corrigida.shp"), overwrite = TRUE)
  message(sprintf("[06][auditoria] cassiterita_nao_corrigida.shp: %d poligonos",
                  nrow(pma_cassiterita_nao_corrigida)))
} else {
  message("[06][auditoria] cassiterita_nao_corrigida: nenhum processo afetado -- shp nao gerado.")
}

# =============================================================================
# BLOCO 8 — EXPORTS (result_shiny / result_gee / result_db)
# =============================================================================

pma_full      <- load_ckpt("05_pma_full")
cfem_final    <- load_ckpt("05_cfem_final")
cfem_aut_amzl <- load_ckpt("05_cfem_aut_amzl")
ti_amzl       <- load_ckpt("03_ti_amzl")
uc_amzl       <- load_ckpt("03_uc_amzl")
qui_amzl      <- load_ckpt("03_qui_amzl")

# SHINY
terra::writeVector(pma_full, file.path(RESULT_SHINY, "pma_amzl_ALLminerals_final.geojson"), filetype = "GeoJSON", overwrite = TRUE)
terra::writeVector(ti_amzl,  file.path(RESULT_SHINY, "ti_amzl.shp"),  overwrite = TRUE)
terra::writeVector(uc_amzl,  file.path(RESULT_SHINY, "uc_amzl.shp"),  overwrite = TRUE)
terra::writeVector(qui_amzl, file.path(RESULT_SHINY, "qui_amzl.shp"), overwrite = TRUE)
readr::write_csv(cfem_final, file.path(RESULT_SHINY, "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv"))

# GEE (recorte pelo BIOMA Amazônia)
bioma_full <- terra::vect(list.files(BIOMA_DIR, pattern = "\\.shp$", full.names = TRUE)[1])
bioma <- bioma_full[bioma_full$Bioma == "Amazônia", ] |> terra::project(terra::crs(pma_full))
dentro_bioma    <- terra::is.related(pma_full, bioma, "intersects")
pma_bioma       <- pma_full[dentro_bioma, ]
processos_bioma <- unique(pma_bioma$PROCESSO)
cfem_bioma      <- cfem_final |> dplyr::filter(PROCESSO %in% processos_bioma)

terra::writeVector(pma_bioma, file.path(RESULT_GEE, "pma_AMAZONIA_ALLminerals_GEE.shp"), overwrite = TRUE)
readr::write_csv(cfem_bioma, file.path(RESULT_GEE, "cfem_AMAZONIA_ALLminerals_GEE.csv"))
cfem_bioma_mensal <- cfem_bioma |>
  dplyr::mutate(data = as.Date(sprintf("%04d-%02d-01", ANO, MES)), proc_ano = paste0(trimws(PROCESSO), "/", ANO))
readr::write_csv(cfem_bioma_mensal, file.path(RESULT_GEE, "cfem_AMAZONIA_ALLminerals_GEE_MONTHLY.csv"))

pma_db <- pma_full

rename_db <- c(name_muni = "munic_pma", abbrev_state = "uf_pma",
               name_state = "estado", name_region = "regiao", code_muni = "cod_munic")
for (old in names(rename_db)) {
  if (old %in% names(pma_db)) names(pma_db)[names(pma_db) == old] <- rename_db[[old]]
}

terra::writeVector(pma_db,      file.path(RESULT_DB, "pma_amzl_ALLminerals_final.geojson"), filetype = "GeoJSON", overwrite = TRUE)
terra::writeVector(pma_db,      file.path(RESULT_DB, "pma_amzl_ALLminerals_final.shp"), overwrite = TRUE)
terra::writeVector(ti_amzl,     file.path(RESULT_DB, "ti_amzl.geojson"),  filetype = "GeoJSON", overwrite = TRUE)
terra::writeVector(uc_amzl,     file.path(RESULT_DB, "uc_amzl.geojson"),  filetype = "GeoJSON", overwrite = TRUE)
terra::writeVector(qui_amzl,    file.path(RESULT_DB, "qui_amzl.geojson"), filetype = "GeoJSON", overwrite = TRUE)
readr::write_csv(cfem_final,    file.path(RESULT_DB, "cfem_amzl_ALLminerals_GOLD_CASScorrected.csv"))
readr::write_csv(cfem_aut_amzl, file.path(RESULT_DB, "cfem_aut_all_min_amzl.csv"))

message("\n=== 06_correcao_cfem.R — CONCLUÍDO ===")

names(pma_db)
