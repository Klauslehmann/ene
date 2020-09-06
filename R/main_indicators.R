
library(magrittr)

#' Calcula la tasa de ocupación
#'
#' @param data \code{dataframe} de la ENE
#' @return tasa de desempleo
#' @export
get_employment_rate <- function(data) {
  employment <- data %>%
    dplyr::select(cae_especifico, edad, fact_cal) %>%
    dplyr::mutate(ocupados = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 7, fact_cal, 0),
           pet = dplyr::if_else(edad>=15, fact_cal, 0)) %>%
    dplyr::summarise(employment_rate = sum(ocupados) / sum(pet))
  return(employment * 100)
}


#-------------------------------------------------------------------------------------
#' Calcula la tasa de desocupación
#'
#' @param data \code{dataframe} de la ENE
#' @return tasa de desocupación
#' @export
get_unemployment_rate <- function(data) {
  unemployment_rate <- data %>%
    dplyr::select(cae_especifico, fact_cal) %>%
    dplyr::mutate(desocupados = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, fact_cal, 0),
           fdt = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 9, fact_cal, 0)) %>%
    dplyr::summarise(unemployment_rate = sum(desocupados) / sum(fdt))
  return(unemployment_rate * 100)
}

#-------------------------------------------------------------------------------------
#' Calcula la tasa de participación laboral
#'
#' @param data \code{dataframe} de la ENE
#' @return tasa de participación
#' @export
get_parcipation_rate <- function(data) {
  participation_rate <- data %>%
    dplyr::select(cae_especifico, fact_cal, edad) %>%
    dplyr::mutate(pet = dplyr::if_else(edad>=15, fact_cal, 0),
           fdt = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 9, fact_cal, 0)) %>%
    dplyr::summarise(unemployment_rate = sum(fdt) / sum(pet))
  return(participation_rate * 100)
}


#-------------------------------------------------------------------------------------
#' Calcula la fuerza de trabajo
#'
#' @param data \code{dataframe} de la ENE
#' @return fuerza de trabajo
#' @export
get_labour_force <- function(data){
  labour_force <- data %>%
    dplyr::select(cae_especifico, fact_cal) %>%
    dplyr::mutate(fdt = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 9, fact_cal, 0)) %>%
    dplyr::summarise(unemployment_rate = sum(fdt) )
  return(labour_force)
}

#-------------------------------------------------------------------------------------
#' Calcula el número de ocupados
#'
#' @param data \code{dataframe} de la ENE
#' @return número de ocupados
#' @export

get_employment <- function(data) {
  employment <- data %>%
    dplyr::select(cae_especifico, edad, fact_cal) %>%
    dplyr::mutate(ocupados = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 7, fact_cal, 0)) %>%
    dplyr::summarise(employment = sum(ocupados))
  return(employment)
}

#-------------------------------------------------------------------------------------
#' Calcula el número de trabajadores informaels
#'
#' @param data \code{dataframe} de la ENE
#' @return número de trabajadores informales
#' @export
get_informal <- function(data) {
  informal <- data %>%
    dplyr::select(ocup_form, fact_cal) %>%
    dplyr::mutate(informal = dplyr::if_else(ocup_form == 2, fact_cal, 0)) %>%
    dplyr::summarise(informal = sum(informal, na.rm = T))
  return(informal)
}

#-------------------------------------------------------------------------------------
#' Calcula el número de desocupados
#'
#' @param data \code{dataframe} de la ENE
#' @return número de desocupados
#' @export
get_unemployment <- function(data) {
  unemployment <- data %>%
    dplyr::select(cae_especifico, fact_cal) %>%
    dplyr::mutate(desocupados = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, fact_cal, 0)) %>%
    dplyr::summarise(unemployment = sum(desocupados))
  return(unemployment)
}



#get_employment_rate(datos)
#get_unemployment_rate(datos)
#get_parcipation_rate(datos)
#
#get_labour_force(datos)
#get_employment(datos)
#get_informal(datos)
#get_unemployment(datos)


