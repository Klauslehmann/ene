
#' Lee archivo en formato csv
#'
#' @param f un entero.
#' @param m un entero.
#' @param f un archivo
#' @return un \code{dataframe}.
read_data_internal <- function(y, m , f) {
  if (y >= 2020 ) {
    data <- readr::read_delim(f, delim = ';',
                              escape_double = FALSE,
                              col_types = readr::cols(
                                c9_otro = readr::col_character(),
                                b18_varias = readr::col_character(),
                                e9_otro = readr::col_character(),
                                e3_10 = readr::col_character(),
                                c9_otro_covid = readr::col_character(),
                                a6_otro =  readr::col_character(),
                                e19_otro = readr::col_character(),
                                id_upm = readr::col_integer(),
                                b11_proxy = readr::col_integer(),
                                idrph_preliminar = readr::col_character(),
                                b11 = readr::col_integer(),
                                c1 = readr::col_integer(),
                                e1 = readr::col_integer(),
                                e14_mes = readr::col_integer(),
                                e14_ano = readr::col_integer(),
                                e15_anos = readr::col_integer(),
                                e17 = readr::col_integer(),
                                e19 = readr::col_integer(),
                                e8 = readr::col_integer(),
                                e15_anos = readr::col_integer(),
                                b16_otro = readr::col_character(),
                                b17_ano = readr::col_integer(),
                                a9 = readr::col_integer(),
                                a6_otro = readr::col_integer(),
                                c13 = readr::col_integer(),
                                e15_meses = readr::col_integer(),
                                b18_varias = readr::col_character()))

  } else if (y == 2019 & m == 12) {
    data <- readr::read_delim(f, delim = ';',
                              col_types = readr::cols(
                                b18_varias = readr::col_character(),
                                e3_10 = readr::col_character(),
                                a6_otro =  readr::col_character(),
                                e19_otro = readr::col_character(),
                                id_upm = readr::col_integer(),
                                b11_proxy = readr::col_integer(),
                                idrph_preliminar = readr::col_character(),
                                c13 = readr::col_integer(),
                                e15_meses = readr::col_integer(),
                                b18_varias = readr::col_character(),
                                e3_10 = readr::col_character(),
                                e10 = readr::col_character(),
                                b16_otro = readr::col_character()))

  } else {
    data <- readr::read_delim(f, delim = ',',
                              col_types = readr::cols(
                                e3_10 = readr::col_character(),
                                a6_otro =  readr::col_character(),
                                e19_otro = readr::col_character(),
                                c13 = readr::col_integer(),
                                e15_meses = readr::col_integer(),
                                b18_varias = readr::col_character(),
                                e3_10 = readr::col_character(),
                                e10 = readr::col_character(),
                                b16_otro = readr::col_character()))
  }
  return(data)
}


#' Generar mes en formato numérico a partir de un character
#'
#' @param month_str mes en formato character
#' @return mes en formato integer.
search_quarter_internal <- function(month_str) {
  quarters <- c("def", "efm", "fma", "mam", "amj", "mjj", "jja", "jas", "aso", "son", "ond", "nde")
  names(quarters) <- c(paste0("0", 1:10), "11", "12")
  return(quarters[[month_str]])
}


#------------------------------------------------------------------------------------------------------

#' Carga en memoria una base de datos de la ENE
#'
#' @param year año en formato integer
#' @param month mes en formato character
#' @param vars vector de characters
#' @return un \code{}
#' @export
get_microdata <- function(year, month, vars = "all" ) {

  #Chequear que los parámetros mínimos fueron introducidos
  if (missing(year) ) stop("debes ingresar un año")
  if (missing(month) ) stop("debes ingresar un mes")

  #Transformar parámetro mes a string para usarlo en la url
  month_str <- as.character(month)
  if (nchar(month_str) == 1) {
    month_str <- paste0("0", month_str)
  }

  #Generar variables de año y mes actual
  date <- Sys.Date()
  current_year <-  lubridate::year(date)
  current_month <-  lubridate::month(date)

  #Chequear que las fechas estén en el rango temporal
  if (year < 2010 | (year == 2010 & month == 1) | year >  current_year |
      ( month > (current_month - 2) & current_year == year) )    {
    stop("La base de datos no existe. Prueba con otros parámetros")
  }

  #Generar url diferenciada para 2020 y resto de los años
  if (year == 2020) {
    root <- "http://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/"
    quarter <- search_quarter_internal(month_str)
    file <- paste0(root, year, "/formato-csv/ene-", year, "-", month_str, "-", quarter, ".csv")
  } else {
    root <- "http://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/"
    file <- paste0(root, year, "/formato-csv/ene-", year, "-", month_str, ".csv")
  }

  #Descargar la base de datos en archivo temporal
  temp <- tempfile()
  download.file(file, temp)

  #Leer archivo, especificando tipo de datos en varias columnas
  datos <- read_data_internal(year, month, file)

  #Seleccionar variables
  if (vars[1] == "all") {
    return(datos)
  } else {
    datos <- datos %>%
      select(vars)
    return(datos)
  }

}
#------------------------------------------------------------------------------------------------------
#' Descarga al disco una base de datos de la ENE
#'
#' @param year año en formato integer
#' @param month mes en formato character
#' @param directory directorio local character
#' @param vars vector de characters
#' @return guarad en memoria un archivo RData
#' @export
download_microdata <- function(year, month, directory, vars = "all") {
  #Chequear que los parámetros mínimos fueron introducidos
  if (missing(year) ) stop("debes ingresar un año")
  if (missing(month) ) stop("debes ingresar un mes")
  if (missing(directory) ) stop("debes ingresar un directorio para guardar el archivo")

  #Transformar parámetro mes a string para usarlo en la url
  month_str <- as.character(month)
  if (nchar(month_str) == 1) {
    month_str <- paste0("0", month_str)
  }

  #Generar variables de año y mes actual
  date <- Sys.Date()
  current_year <-  lubridate::year(date)
  current_month <-  lubridate::month(date)

  #Chequear que las fechas estén en el rango temporal
  if (year < 2010 | (year == 2010 & month == 1) | year >  current_year |
      ( month > (current_month - 2) & current_year == year) )    {
    stop("La base de datos no existe. Prueba con otros parámetros")
  }

  #Generar url diferenciada para 2020 y resto de los años
  if (year == 2020) {
    root <- "http://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/"
    quarter <- search_quarter_internal(month_str)
    file <- paste0(root, year, "/formato-csv/ene-", year, "-", month_str, "-", quarter, ".csv")
  } else {
    root <- "http://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/"
    file <- paste0(root, year, "/formato-csv/ene-", year, "-", month_str, ".csv")
  }

  #Descargar la base de datos en archivo temporal
  download.file(file, directory)
}

#vars <- c("efectivas", "e11", "region")
#datos <- get_microdata(year = 2020, month = 6)

#download_microdata(year = 2020, month = 7, "data/file1.csv")
#files_2020 <- map(1:6, get_microdata, year = 2020)
#files_2018 <- map(1:12, get_microdata, year = 2018)
#files_2017 <- map(1:12, get_microdata, year = 2017)
#files_2016 <- map(1:12, get_microdata, year = 2016)



