################################################################################
# 09_checar_rds.R
#
# Checagem robusta de TODOS os .rds em shiny_dashboard/ antes do deploy.
# ULTIMA etapa do pipeline: le o que o 08_proc_shiny.R e o 07_serie_temporal.R
# escreveram em shiny_dashboard/, entao roda DEPOIS dos dois.
# Roda 100% local, so leitura -- nao altera nenhum arquivo.
#
# SEPARADO DO 08 em 2026-08-25 (auditoria F-13): este bloco estava colado no
# fim do 08_proc_shiny.R com o caminho "C:/GP/anm-geo/shiny_dashboard" gravado
# no codigo. Em qualquer outra maquina o script morria -- DEPOIS de ja ter
# salvo os 35 arquivos -- o que produzia um alarme falso a cada execucao.
# Agora e etapa propria do pipeline, com o diretorio resolvido a partir da raiz
# e rodando so quando se quer checar, nao a cada empacotamento.
#
# Uso:
#   Rscript R/09_checar_rds.R
#   ou, no R:  source(here::here("R", "09_checar_rds.R"))
#
# CODIGO DE SAIDA: 1 se houver problema, 0 se tudo limpo -- para poder ser
# encadeado em automacao. Antes so imprimia "nao subir pro droplet ainda" e
# terminava com sucesso, o que nenhum orquestrador consegue detectar.
#
# O que detecta:
#   1) Erro de leitura (arquivo corrompido/ausente)
#   2) Warnings emitidos durante o readRDS() -- pega qualquer warning, incluindo
#      o "cannot unserialize ALTVEC object... returning length zero vector"
#      (bug do arrow ALTREP ja identificado), sem depender do texto exato da
#      mensagem continuar igual em versoes futuras do pacote.
#   3) Checagem ESTRUTURAL independente do warning: compara o tamanho de CADA
#      coluna com nrow(df). Isso pega o bug mesmo se o R um dia parar de
#      emitir warning nesse caso -- e o teste que realmente importa, o warning
#      e so um sintoma.
#   4) Tabelas com 0 linhas (pode ser esperado ou sintoma de outro problema
#      upstream -- sinalizado, nao tratado como erro automatico).
#   5) Geometria incompativel com leaflet::addPolygons.
################################################################################

suppressPackageStartupMessages({
  library(here)
  library(sf)
})

pasta <- here::here("shiny_dashboard")

if (!dir.exists(pasta)) {
  stop("Pasta nao encontrada: ", pasta,
       " -- rodar 08_proc_shiny.R antes.", call. = FALSE)
}

arquivos_rds <- list.files(pasta, pattern = "\\.rds$", full.names = TRUE)

if (length(arquivos_rds) == 0) {
  stop("Nenhum .rds encontrado em: ", pasta,
       " -- rodar 08_proc_shiny.R antes.", call. = FALSE)
}

checar_rds <- function(caminho) {
  nome <- basename(caminho)
  warnings_capturados <- character(0)

  obj <- withCallingHandlers(
    tryCatch(readRDS(caminho), error = function(e) e),
    warning = function(w) {
      warnings_capturados <<- c(warnings_capturados, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (inherits(obj, "error")) {
    return(data.frame(arquivo = nome, status = "ERRO_LEITURA",
                       detalhe = conditionMessage(obj), stringsAsFactors = FALSE))
  }

  detalhe <- character(0)

  if (length(warnings_capturados) > 0) {
    detalhe <- c(detalhe, paste0("WARNING NA LEITURA: ", paste(unique(warnings_capturados), collapse = " || ")))
  }

  if (is.data.frame(obj)) {
    n <- nrow(obj)
    tam_colunas <- vapply(obj, length, integer(1))

    colunas_zeradas      <- names(obj)[tam_colunas == 0 & n > 0]
    colunas_incompativeis <- setdiff(names(obj)[tam_colunas != n], colunas_zeradas)

    if (length(colunas_zeradas) > 0) {
      detalhe <- c(detalhe, paste0("COLUNA(S) DE TAMANHO 0 (nrow=", n, "): ",
                                    paste(colunas_zeradas, collapse = ", ")))
    }
    if (length(colunas_incompativeis) > 0) {
      detalhe <- c(detalhe, paste0("COLUNA(S) COM TAMANHO != nrow: ",
                                    paste(colunas_incompativeis, collapse = ", ")))
    }
    if (n == 0) {
      detalhe <- c(detalhe, "AVISO: 0 linhas (confirmar se e esperado)")
    }

    if (inherits(obj, "sf")) {
      tipos <- as.character(sf::st_geometry_type(obj))
      tipos_ok <- c("POLYGON", "MULTIPOLYGON")
      tipos_ruins <- setdiff(unique(tipos), tipos_ok)
      if (length(tipos_ruins) > 0) {
        n_ruins <- sum(tipos %in% tipos_ruins)
        detalhe <- c(detalhe, paste0("GEOMETRIA INCOMPATIVEL COM leaflet::addPolygons -- ",
                                      n_ruins, " linha(s) do tipo ",
                                      paste(tipos_ruins, collapse = ", ")))
      }
    }
  } else if (is.list(obj) && length(obj) == 0) {
    detalhe <- c(detalhe, "LISTA VAZIA")
  }

  status <- if (length(detalhe) == 0) "OK" else "PROBLEMA"
  data.frame(arquivo = nome, status = status,
             detalhe = paste(detalhe, collapse = " | "), stringsAsFactors = FALSE)
}

resultado <- do.call(rbind, lapply(arquivos_rds, checar_rds))

cat("\n================ RESUMO (", nrow(resultado), "arquivos ) ================\n")
print(resultado[, c("arquivo", "status")], row.names = FALSE)

problemas <- resultado[resultado$status != "OK", ]
if (nrow(problemas) > 0) {
  cat("\n================ DETALHES DOS PROBLEMAS ================\n")
  for (i in seq_len(nrow(problemas))) {
    cat("\n->", problemas$arquivo[i], "\n   ", problemas$detalhe[i], "\n")
  }
  cat("\n", nrow(problemas), "de", nrow(resultado), "arquivo(s) com problema -- NAO subir ainda.\n")
  if (!interactive()) quit(status = 1)
} else {
  cat("\nTodos os", nrow(resultado), "arquivos passaram limpo.\n")
}

message("\n=== 09_checar_rds.R — CONCLUIDO ===")