# ============================================================================
# Definición de grupos de variables
# ENDES 2024 | Stepwise, BMA y convergencia MC3
# ============================================================================
#
# Los grupos son consistentes en TODAS las etapas del proyecto.
# Variables categóricas entran/salen como bloques completos.
# ============================================================================

#' Obtener nombres de variables categóricas
#'
#' @return vector con nombres de variables categóricas
get_categorical_vars <- function() {
  c(
    "niv_educ",
    "quintil_riq",
    "area_res",
    "etnicidad",
    "seguro",
    "situacion_lab",
    "estado_civil",
    "emb_planeado",
    "SDEPARTAMENTO"
  )
}

#' Obtener nombres de variables numéricas
#'
#' @return vector con nombres de variables numéricas
get_numeric_vars <- function() {
  c("edad", "hijos", "num_centros_medicos")
}

#' Obtener todos los nombres de variables
#'
#' @return vector con todas las variables
get_all_vars <- function() {
  c(get_categorical_vars(), get_numeric_vars())
}

#' Definir grupos para stepwise y BMA
#'
#' Los grupos permiten que las variables categóricas entren o salgan
#' como un bloque completo (ej. todos los departamentos juntos).
#'
#' @return lista con grupos
get_groups <- function() {
  
  list(
    niv_educ = c(
      "niv_educPrimaria",
      "niv_educSecundaria",
      "niv_educSuperior"
    ),
    quintil_riq = c(
      "quintil_riqPobre",
      "quintil_riqMedio",
      "quintil_riqRico",
      "quintil_riqMuy.rico"
    ),
    area_res = c("area_resUrbano"),
    etnicidad = c(
      "etnicidadEtnia.castellana",
      "etnicidadEtnia.Extranjera"
    ),
    seguro = c(
      "seguroSIS",
      "seguroESSALUD"
    ),
    situacion_lab = c(
      "situacion_labEmpleo.activo",
      "situacion_labEmpleo.en.licencia"
    ),
    estado_civil = c("estado_civilPareja.Estable"),
    emb_planeado = c("emb_planeadoPlaneado"),
    SDEPARTAMENTO = c(
      "SDEPARTAMENTOANCASH", "SDEPARTAMENTOAPURIMAC",
      "SDEPARTAMENTOAREQUIPA", "SDEPARTAMENTOAYACUCHO",
      "SDEPARTAMENTOCAJAMARCA", "SDEPARTAMENTOCALLAO",
      "SDEPARTAMENTOCUSCO", "SDEPARTAMENTOHUANCAVELICA",
      "SDEPARTAMENTOHUANUCO", "SDEPARTAMENTOICA",
      "SDEPARTAMENTOJUNIN", "SDEPARTAMENTOLA.LIBERTAD",
      "SDEPARTAMENTOLAMBAYEQUE", "SDEPARTAMENTOLIMA.PROVINCIA",
      "SDEPARTAMENTOLIMA.REGION", "SDEPARTAMENTOLORETO",
      "SDEPARTAMENTOMADRE.DE.DIOS", "SDEPARTAMENTOMOQUEGUA",
      "SDEPARTAMENTOPASCO", "SDEPARTAMENTOPIURA",
      "SDEPARTAMENTOPUNO", "SDEPARTAMENTOSAN.MARTIN",
      "SDEPARTAMENTOTACNA", "SDEPARTAMENTOTUMBES",
      "SDEPARTAMENTOUCAYALI"
    ),
    edad = "edad",
    hijos = "hijos",
    num_centros_medicos = "num_centros_medicos"
  )
}

#' Obtener nombres de los grupos
#'
#' @return vector con nombres de grupos
get_group_names <- function() {
  names(get_groups())
}

#' Obtener número de grupos
#'
#' @return entero
get_n_groups <- function() {
  length(get_groups())
}

#' Verificar que un grupo existe
#'
#' @param group_name nombre del grupo
#' @return lógico
group_exists <- function(group_name) {
  group_name %in% get_group_names()
}