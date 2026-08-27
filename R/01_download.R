################################################################################
# 01_download.R - Open Data Downloader (ANM + Geo)
################################################################################

# Setup and Configuration -------------------------------------------------
rm(list = ls(all.names = TRUE))
options(scipen = 999)
options(timeout = 1800)

suppressPackageStartupMessages({
  library(purrr)
  library(here)
  library(readr)
  library(dplyr)
  library(tibble)
})
          
MAX_ATTEMPTS_DOWNLOAD <- 5  # tentativas por arquivo, por rodada (ver bloco de retry automático no fim do script)

ROOT      <- here::here()
RAW_DIR   <- here::here("data", "raw_data")

source(here::here("R", "utils.R"))

dir.create(MANIFEST_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Credenciais (decisão 2026-08, auditoria F-15) ----------------------------
# A authkey do geoserver da SEMA-MT estava embutida na URL, em texto claro, no
# código versionado. Migrada para variável de ambiente.
#
# Defina no .Renviron da raiz do projeto (que deve estar no .gitignore):
#   SEMA_MT_AUTHKEY=<chave>
#
# ATENÇÃO: mover a chave para cá NÃO a remove do histórico do versionamento.
# A chave anterior precisa ser rotatada junto com esta mudança.
#
# SEMA-MT é fonte OPCIONAL (o pipeline segue com flag NA se ela faltar), então
# a ausência da chave NÃO derruba o script: avisa e pula os três layers do MT.
SEMA_MT_AUTHKEY <- Sys.getenv("SEMA_MT_AUTHKEY", unset = NA_character_)
TEM_SEMA_MT     <- !is.na(SEMA_MT_AUTHKEY) && nzchar(SEMA_MT_AUTHKEY)
if (!TEM_SEMA_MT) {
  message("\n[AVISO] SEMA_MT_AUTHKEY não definida — os 3 layers da SEMA-MT ",
          "serão PULADOS nesta rodada.\n        As flags emb_MTa/emb_MTb/inf_MT ",
          "ficarão NA (não 0) no 05. Ver .Renviron.example.")
}

# --- Territórios: quais camadas baixar nesta rodada ---------------------------
# Decisão da rodada 2026-08-25: seguir só com TI. CNUC (UC) e quilombolas ficam
# de fora por ora. Basta acrescentar "UC" / "QUILOMBOLA" ao vetor para religar.
#
# CONSEQUÊNCIA: sem UC e sem quilombolas, os passos correspondentes do 02 falham
# (são fontes obrigatórias em FONTES_OBRIGATORIAS, R/utils.R) e o 03 não roda.
# Isso é intencional — a alternativa seria seguir com camada faltando em
# silêncio, que é exatamente a classe de falha que a auditoria encontrou.
BAIXAR_TERRITORIOS <- c("TI")

# Config: ANM ------------------------------------------------------------------
config_anm <- list(
  scm = list(
    dest = "anm_scm",
    base_url = "https://dadosabertos.anm.gov.br/SCM/",
    files = c(
      "Alvara_de_Pesquisa.csv",
      "Cessoes_de_Direitos.csv",
      "Licenciamento.csv",
      "PLG.csv",
      "Portaria_de_Lavra.csv",
      "Registro_de_Extracao_Publicado.csv",
      "Relatorio_de_Pesquisa_Aprovado.csv",
      "Requerimento_de_Lavra.csv",
      "Requerimento_de_Licenciamento.csv",
      "Requerimento_de_Pesquisa.csv",
      "Requerimento_de_PLG.csv",
      "Requerimento_de_Registro_de_Extracao_Protocolizado.csv",
      "metadados-scm.ods"
    )
  ),
  protocolo_digital = list(
    dest = "anm_protocolo_digital",
    base_url = "https://dadosabertos.anm.gov.br/PD/",
    files = c(
      "Assunto.txt",
      "AssuntoDocumento.txt",
      "AssuntoParametro.txt",
      "Indisponibilidade.txt",
      "mer_pd.png",
      "metadados-pd.ods",
      "Protocolo.txt",
      "ProtocoloAnexoSei.txt"
    )
  ),
  microdados = list(
    dest = "anm_microdados",
    base_url = "https://dadosabertos.anm.gov.br/SCM/microdados/",
    files = c(
      "microdados-scm.zip",
      "mer-microdados-scm.pdf",
      "mer-dbanm_gdb.pdf",
      "metadados-microdados-scm.ods"
    )
  ),
  sigmine = list(
    dest = "anm_espacial",
    base_url = "https://dadosabertos.anm.gov.br/SIGMINE/",
    files = c("BRASIL.zip")
  ),
  arrecadacao = list(
    dest = "anm_arrecadacao",
    base_url = "https://dadosabertos.anm.gov.br/CFEM/",
    files = c(
      "CFEM_Arrecadacao.csv",
      "CFEM_Autuacao.csv",
      "CFEM_Distribuicao.csv",
      "metadados-cfem.ods"
    )
  )
  # ,
  # Tah = list(
  #   dest = "anm_Tah",
  #   base_url = "https://dadosabertos.anm.gov.br/TAH/",
  #   files = c(
  #     "Tah.csv",
  #     "metadados-tah.ods")
  #)
)

anm_targets <- purrr::imap(config_anm, \(cfg, name) {
  urls <- setNames(paste0(cfg$base_url, cfg$files), cfg$files)
  list(name = paste0("anm_", name), dest = file.path(RAW_DIR, cfg$dest), urls = urls)
})

# Config: GEO (protected lands + enforcement) ----------------------------------
config_geo <- list(

  # --- Territórios ------------------------------------------------------------
  # REATIVADO em 2026-08 (auditoria F-04). Estava comentado como "suspenso até
  # dezembro" por diagnóstico errado de indisponibilidade da fonte: o geoserver
  # da FUNAI recusa o User-Agent padrão do R com 403 e aceita um UA nomeado.
  # Correção aplicada em R/utils.R (UA_PROJETO). Sem estas três camadas o
  # 03_recorte_espacial.R não roda.
  territorios = list(
    dest = "geo_territorios",
    urls = c(
      # FUNAI -- TI (Terras Indigenas)
      # https://metadados.inde.gov.br/geonetwork/srv/por/catalog.search#/metadata/63019b03-0937-4461-a6ed-abef1457c1a1
      "TI" = "https://geoserver.funai.gov.br/geoserver/Funai/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Funai%3Atis_poligonais&maxFeatures=10000&outputFormat=SHAPE-ZIP",
      # MMA -- UC (Unidades de Conservacao / CNUC)
      # https://dados.mma.gov.br/dataset/unidadesdeconservacao
      # ATENÇÃO: nome de arquivo com data fixa (2025_08). O CNUC publica versões
      # novas periodicamente e a URL antiga continua respondendo — ou seja, o
      # download "funciona" e traz snapshot velho, sem aviso. Conferir a versão
      # corrente no dataset antes de cada rodada de produção.
      "UC" = "https://dados.mma.gov.br/dataset/44b6dc8a-dc82-4a84-8d95-1b0da7c85dac/resource/6ba9a557-87e8-4882-acb7-b3e0f0ea192d/download/shp_cnuc_2025_08.zip",
      # INCRA -- QUI (Territorios Quilombolas)
      "QUILOMBOLA" = "https://certificacao.incra.gov.br/csv_shp/zip/%C3%81reas%20de%20Quilombolas.zip"
    ),
    # nome do arquivo em disco por camada (o 02 procura por estes nomes)
    arquivos = c(
      "TI"         = "tis_poligonais.zip",
      "UC"         = "shp_cnuc_2025_08.zip",
      "QUILOMBOLA" = "areas_quilombolas.zip"
    )
  ),

  # --- IBGE: NÃO baixado automaticamente (decisão 2026-08-25) -----------------
  # A auditoria (F-14) apontou que duas bases territoriais do IBGE são exigidas
  # pelo pipeline sem constar de nenhum download:
  #
  #   data/raw_data/Limites_Amazonia_Legal_2024/ -> 03_recorte_espacial.R (AMZL_DIR)
  #   data/raw_data/BR_Municipios_2025/          -> 05_integracao_final.R (MUNI_DIR)
  #
  # Tentamos automatizar e o geoftp devolveu 404 nas duas URLs (rodada
  # 2026-08-25). O servidor do IBGE é instável e reorganiza caminhos entre
  # edições, então automatizar aqui geraria falha recorrente sem ganho real.
  #
  # DECISÃO: ficam como INSUMO MANUAL. Baixar do site do IBGE e extrair nas duas
  # pastas acima. Está documentado no leia-me. O 03 e o 05 devem verificar a
  # presença e dizer qual pasta falta -- não descobrir por erro de leitura.
  #
  # (A auditoria menciona TRÊS bases; na varredura do código só estas duas
  # aparecem referenciadas. Se surgir uma terceira, documentar junto.)

  # --- IBAMA: fiscalizacao/embargo federal ---------------
  ibama = list(
    dest = "geo_ibama",
    urls = c(
      "adm_embargos_ibama_a.zip" =
        "https://ftp-pamgia.ibama.gov.br/dados/adm_embargos_ibama_a.zip",

      "ibama_autos_de_infracao_p.zip" =
        "https://ftp-pamgia.ibama.gov.br/dados/ibama_autos_de_infracao_p.zip",

      "auto_infracao_csv.zip" =
        "https://dadosabertos.ibama.gov.br/dados/SIFISC/auto_infracao/auto_infracao/auto_infracao_csv.zip"
    )
  ),

  # --- ICMBio: fiscalizacao/embargo federal --------------
  icmbio = list(
    dest = "geo_icmbio",
    urls = c(
      "embargos_icmbio.zip" =
        "https://www.gov.br/icmbio/pt-br/dados-icmbio/dados_geoespaciais/mapa-tematico-e-dados-geoestatisticos-das-unidades-de-conservacao-federais/embargos_icmbio_shp.zip",
      
      "autos_infracao_icmbio.zip" =
        "https://www.gov.br/icmbio/pt-br/dados-icmbio/dados_geoespaciais/mapa-tematico-e-dados-geoestatisticos-das-unidades-de-conservacao-federais/autos_infracao_icmbio_shp.zip"
    )
  ),

  sema_mt = list(
    dest = "geo_sema_mt",
    base_url = paste0(
      "https://geo.sema.mt.gov.br/geoserver/wfs?authkey=", SEMA_MT_AUTHKEY,
      "&request=getfeature&service=wfs&version=1.0.0&outputformat=SHAPE-ZIP&typename=Geoportal:"
    ),
    layers = c(
      "AREAS_EMBARGADAS_SEMA",
      "AREA_EMBARGADA_SIGA_POLIGONO",
      "AUTOS_DE_INFRACAO_SIGA_POLIGONO"
    )
  )
)

sema_urls <- setNames(
  paste0(config_geo$sema_mt$base_url, config_geo$sema_mt$layers),
  paste0(config_geo$sema_mt$layers, ".zip")
)

# Territórios: só as camadas escolhidas em BAIXAR_TERRITORIOS, já renomeadas
# para o nome de arquivo que o 02 procura.
camadas_terr <- intersect(BAIXAR_TERRITORIOS, names(config_geo$territorios$urls))
terr_urls    <- setNames(
  config_geo$territorios$urls[camadas_terr],
  config_geo$territorios$arquivos[camadas_terr]
)
message("[territorios] camadas desta rodada: ",
        if (length(camadas_terr)) paste(camadas_terr, collapse = ", ") else "(nenhuma)")

geo_targets <- list(
  if (length(terr_urls)) list(name = "geo_territorios", dest = file.path(RAW_DIR, config_geo$territorios$dest), urls = terr_urls),
  list(name = "geo_ibama",   dest = file.path(RAW_DIR, config_geo$ibama$dest),   urls = config_geo$ibama$urls),
  list(name = "geo_icmbio",  dest = file.path(RAW_DIR, config_geo$icmbio$dest),  urls = config_geo$icmbio$urls),
  if (TEM_SEMA_MT) list(name = "geo_sema_mt", dest = file.path(RAW_DIR, config_geo$sema_mt$dest), urls = sema_urls)
)
geo_targets <- purrr::compact(geo_targets)

# Run all targets --------------------------------------------------------------

targets <- c(anm_targets, geo_targets)

# Timestamp
TS_EXECUCAO <- format(Sys.time(), "%Y-%m-%d_%H%M%S")

# --- Pré-checagem de URLs (decisão 2026-08, auditoria F-04 e F-14) ------------
# Testa TODAS as URLs com HEAD antes de baixar qualquer coisa. Serve para dois
# propósitos: (a) confirmar que o UA do projeto é aceito por cada servidor,
# antes de cair para UA_FALLBACK; (b) pegar URL errada de imediato, em vez de
# descobrir 40 minutos depois. Não baixa nada e não bloqueia a execução.
#
# PRECHECAR_URLS = FALSE pula esta etapa (rodadas repetidas no mesmo dia).
PRECHECAR_URLS <- TRUE

if (PRECHECAR_URLS) {
  message("\n=== Pré-checagem de URLs (HEAD, sem download) ===")
  precheck <- purrr::map_dfr(targets, \(t) {
    purrr::imap_dfr(t$urls, \(u, nome) tibble::tibble(
      target = t$name, filename = nome,
      status = testar_user_agent(u)
    ))
  })
  ruins <- dplyr::filter(precheck, is.na(status) | status >= 400)
  if (nrow(ruins) > 0) {
    message("\n[precheck] ", nrow(ruins), " URL(s) não responderam OK:")
    purrr::pwalk(ruins, \(target, filename, status)
                 message(" - [", target, "] ", filename, " | status: ", status))
    message("[precheck] Se houver 403, testar UA_FALLBACK (ver R/utils.R) ",
            "antes de concluir que a fonte caiu.")
  } else {
    message("[precheck] Todas as ", nrow(precheck), " URLs responderam OK.")
  }
  readr::write_csv(precheck, file.path(MANIFEST_DIR,
                                       paste0("precheck_urls_", TS_EXECUCAO, ".csv")))
}

manifests_anteriores <- listar_manifests_anteriores()

results_list <- purrr::map(targets, \(t) {
  download_named_urls(t$urls, t$dest, target_name = t$name, max_attempts = MAX_ATTEMPTS_DOWNLOAD)
})

# --- Checagem dos insumos manuais do IBGE (auditoria F-14) --------------------
# Não são baixados aqui (ver nota em config_geo). Só avisa se faltarem, para
# quem monta o ambiente do zero saber o que buscar antes de rodar o 03 e o 05.
purrr::iwalk(
  c("Limites_Amazonia_Legal_2024" = "03_recorte_espacial.R",
    "BR_Municipios_2025"          = "05_integracao_final.R"),
  \(script, pasta) {
    if (!dir.exists(file.path(RAW_DIR, pasta))) {
      message("[insumo manual] data/raw_data/", pasta, "/ AUSENTE — exigida por ",
              script, ". Baixar do site do IBGE (ver leia-me).")
    }
  }
)

# Finish ---------------------------------------------------------------------
todos_arquivos <- purrr::list_flatten(results_list)
erros          <- purrr::keep(todos_arquivos, ~ .x$success == FALSE)

if (length(erros) > 0) {
  message("\n ATTENTION: ", length(erros), " download(s) failed after ",
          MAX_ATTEMPTS_DOWNLOAD, " attempts. Retrying automatically (1 extra round, no prompt)...")

  retries <- purrr::map(erros, ~ {
    r <- download_file(url = .x$url, dest_dir = .x$dest_dir, filename = .x$filename,
                        max_attempts = MAX_ATTEMPTS_DOWNLOAD)
    list(
      target = .x$target, filename = .x$filename, url = .x$url, dest_dir = .x$dest_dir,
      success = r$success, sha256 = r$sha256, size_bytes = r$size_bytes,
      attempts_used = r$attempts_used, note = paste0("retry_automatico: ", r$note)
    )
  })

  for (r in retries) {
    idx <- purrr::detect_index(
      todos_arquivos,
      ~ .x$dest_dir == r$dest_dir && .x$filename == r$filename
    )
    if (idx > 0) todos_arquivos[[idx]] <- r
  }
} else {
  message("\nProcess completed successfully.")
}

# --- Resumo final: o que continua falhando apos a rodada extra --------------
erros_finais <- purrr::keep(todos_arquivos, ~ .x$success == FALSE)
if (length(erros_finais) > 0) {
  message("\n Ocorreram: ", length(erros_finais), " arquivo(s) continuam com falha apos todas as tentativas:")
  purrr::walk(erros_finais, ~ message(" - [", .x$target, "] ", .x$filename, " | url: ", .x$url))
} else {
  message("\nTodos os arquivos foram baixados com sucesso.")
}
# --- Grava o manifest desta execucao
manifest_df <- purrr::map_dfr(todos_arquivos, ~ tibble::tibble(
  target         = .x$target,
  filename       = .x$filename,
  url            = .x$url,
  dest_dir       = .x$dest_dir,
  success        = .x$success,
  sha256         = .x$sha256,
  size_bytes     = .x$size_bytes,
  attempts_used  = .x$attempts_used,
  note           = .x$note
))
manifest_path <- file.path(MANIFEST_DIR, paste0("download_log_", TS_EXECUCAO, ".csv"))
readr::write_csv(manifest_df, manifest_path)
message("\nManifest salvo em: ", manifest_path)