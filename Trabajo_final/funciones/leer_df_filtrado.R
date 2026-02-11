# funciones/leer_df_final.R

leer_df_final <- function(path) {
  suppressMessages(
    readr::read_csv(path, show_col_types = FALSE)
  ) |>
    dplyr::mutate(
      dplyr::across(
        c(
          medicamento_psiquiatrico, suicidio_familiar,
          ruptura_reciente, crisis_pareja_actual, medidas_cautelares
        ),
        ~ factor(.x, levels = c("No", "Sí"))
      ),
      escolaridad = factor(
        escolaridad,
        levels = c(
          "Primaria incompleta",
          "Primaria completa",
          "Secundaria incompleta",
          "Secundaria completa",
          "Técnico medio / Diplomado", 
          "Universitario incompleto",
          "Universitario completo",
          "Posgrado (Maestría / Doctorado)"
        ),
        ordered = FALSE
      ),
      dplyr::across(c(estado_civil, ocupacion), as.factor)
    )
}