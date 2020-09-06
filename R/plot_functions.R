
#-------------------------------------------------------------------------------------
#' Genera un gráfico de la desocupación para 2 o más meses
#'
#' @param df_list  lista de \code{dataframe}
#' @param design character con dos opciones: on u off
#' @return objeto ggplot
#' @importFrom magrittr %>%
#' @export

plot_employment_time <- function(df_list, design = "on") {

  #Revisar parámetros
  if (typeof(df_list) != "list") stop("El parámetro df_list debe se run objeto lista")
  if (design != "on" & design != "off") stop('"Las opciones para design son "on" u "off"')

  unemployment_rate_sex <- df_list %>%
    purrr::map(~select(., cae_especifico, fact_cal, mes_central, sexo) ) %>%
    purrr::reduce(bind_rows) %>%
    dplyr::group_by(mes_central, sexo) %>%
    dplyr::mutate(desocupados = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, fact_cal, 0),
           fdt = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 9, fact_cal, 0)) %>%
    dplyr::summarise(unemployment_rate = sum(desocupados) / sum(fdt) * 100)

  unemployment_rate <- df_list %>%
    purrr::map(~select(., cae_especifico, fact_cal, mes_central, sexo) ) %>%
    purrr::reduce(bind_rows) %>%
    dplyr::group_by(mes_central) %>%
    dplyr::mutate(desocupados = dplyr::if_else(cae_especifico >= 8 & cae_especifico <= 9, fact_cal, 0),
           fdt = dplyr::if_else(cae_especifico >= 1 & cae_especifico <= 9, fact_cal, 0)) %>%
    dplyr::summarise(unemployment_rate = sum(desocupados) / sum(fdt) * 100) %>%
    dplyr::mutate(sexo = 3)

  final <- unemployment_rate_sex %>%
    dplyr::bind_rows(unemployment_rate) %>%
    dplyr::mutate(sexo = dplyr::case_when(
      sexo == 1 ~ "hombre",
      sexo == 2 ~ "mujer",
      sexo == 3 ~ "nacional"
      ))

  #Generar gráfico con diseño
  if (design == "on") {
    ggplot2::ggplot(final, ggplot2::aes(x = mes_central, y = unemployment_rate, group = sexo)) +
      ggplot2::geom_line(ggplot2::aes(color = as.factor(sexo)))  +
      ggplot2::labs(title = "Tasa de desempleo",
           y = "Tasa de desempleo",
           x = "mes") +
      ggplot2::theme(legend.title = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(hjust = 0.5))


  #Generar gráfico sin diseño
  } else {
    ggplot2::ggplot(final, ggplot2::aes(x = mes_central, y = unemployment_rate, group = sexo)) +
      ggplot2::geom_line(ggplot2::aes(color = as.factor(sexo)))
  }
}

#plot_employment_time(files_2020, "on")


