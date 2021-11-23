
renderNewsTable <- function(values, df_formatted) {
  dt <- DT::renderDataTable({
    if (values$is_empty) {
      DT::datatable(NULL)
    } else {
      df_formatted <- values$df_req
      df_formatted$Imagen <- paste0(
        "<a href='", df_formatted$url, "'>",
        "<img src=", df_formatted$urlToImage,
        " height=64></img></a>"
      )
      df_formatted$urlToImage <- NULL

      df_formatted %>%
        dplyr::select("Title" = title, "Image" = Imagen, "Source" = source.name, "Description" = description) %>%
        dplyr::rename_all(i18n$t) %>%
        DT::datatable(
          escape = FALSE,
          rownames = FALSE,
          class = "compact stripe",
          style = "bootstrap",
          selection = "none",
          options = list(
            dom = "tip",
            scrollY = "400px",
            language = list(
              url =
                stringr::str_interp("//cdn.datatables.net/plug-ins/1.10.11/i18n/${i18n$t('English.json')}")
            ),
            initComplete = DT::JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'font-size':'11px'});",
              "$(this.api().table().body()).css({'background-color': '#C6B3B9', 'color': '#000', 'font-size':'10px'});",
              "}"
            )
          )
        )
    }
  })

  return(dt)
}
