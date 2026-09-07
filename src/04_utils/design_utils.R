# ============================================================================
# Utilidades para diseño muestral complejo
# ENDES 2024 | Creación de objeto svydesign
# ============================================================================

#' Crear diseño muestral complejo
#'
#' @param data data.frame con variables de diseño (conglomerado, estrato, factor_exp)
#' @return objeto svydesign para modelos con survey
create_survey_design <- function(data) {
  
  # Ajuste para PSUs solitarios (lonely PSU)
  options(survey.lonely.psu = "adjust")
  
  # Definición del diseño muestral
  design <- svydesign(
    id      = ~conglomerado,  # Unidad primaria de muestreo
    strata  = ~estrato,       # Estratificación
    weights = ~factor_exp,    # Factor de expansión
    data    = data,
    nest    = TRUE            # Anidamiento de conglomerados en estratos
  )
  
  return(design)
}