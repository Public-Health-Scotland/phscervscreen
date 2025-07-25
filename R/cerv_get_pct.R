
cerv_get_pct <- function(df, filter_expr,
                         numerator, denominator, ...,
                         fct_levels,
                         fy_yr_start,
                         scotland = TRUE,
                         calc_coverage = FALSE,
                         all_scr5_5_start_yr = 2025) {

  if (calc_coverage == TRUE & (fy_yr_start < all_scr5_5_start_yr)) {

    hb_data_3_5 <- df |>
      dplyr::filter({{ filter_expr }}, age <=49) |>
      dplyr::group_by(...) |>
      dplyr::summarize(denominator1 = sum({{ denominator }}),
                       numerator1 = sum(scr3_5),
                       percentage1 = (sum(scr3_5)/sum({{ denominator }})*100)) |>
      dplyr::ungroup()

    hb_data_5_5 <- df |>
      dplyr::filter({{ filter_expr }}, age >=50, age <=64) |>
      dplyr::group_by(...) |>
      dplyr::summarize(denominator1 = sum({{ denominator }}),
                       numerator1 = sum({{ numerator }}),
                       percentage1 = (sum({{ numerator }})/sum({{ denominator }})*100)) |>
      dplyr::ungroup()

    hb_data <- rbind(hb_data_3_5, hb_data_5_5) |>
      dplyr::group_by(...) |>
      dplyr::summarize(Denominator = sum(denominator1),
                       Numerator = sum(numerator1),
                       Percentage = (sum(numerator1)/sum(denominator1)*100)) |>
      dplyr::ungroup()

  } else {

    hb_data <- df |>
      dplyr::filter({{ filter_expr }}) |>
      dplyr::group_by(...) |>
      dplyr::summarize(Denominator = sum({{ denominator }}),
                       Numerator = sum({{ numerator }}),
                       Percentage = (sum({{ numerator }})/sum({{ denominator }})*100)) |>
      dplyr::ungroup()

  }

  if (scotland == TRUE) {

    col1 <- colnames(hb_data[1])

    if (calc_coverage == TRUE & (fy_yr_start < all_scr5_5_start_yr)) {

      sc_data_3_5 <- df |>
        dplyr::filter({{ filter_expr }}, age <=49) |>
        dplyr::group_by(...) |>
        dplyr::ungroup({{ col1 }}) |>
        dplyr::summarize(denominator1 = sum({{ denominator }}),
                         numerator1 = sum(scr3_5),
                         percentage1 = (sum(scr3_5)/sum({{ denominator }})*100)) |>
        dplyr::ungroup() |>
        dplyr::mutate({{ col1 }} := "S92000003", .before = 1)

      sc_data_5_5 <- df |>
        dplyr::filter({{ filter_expr }}, age >=50, age <=64) |>
        dplyr::group_by(...) |>
        dplyr::ungroup({{ col1 }}) |>
        dplyr::summarize(denominator1 = sum({{ denominator }}),
                         numerator1 = sum({{ numerator }}),
                         percentage1 = (sum({{ numerator }})/sum({{ denominator }})*100)) |>
        dplyr::ungroup() |>
        dplyr::mutate({{ col1 }} := "S92000003", .before = 1)

      scotland_data <- rbind(sc_data_3_5, sc_data_5_5) |>
        dplyr::group_by(...) |>
        dplyr::ungroup({{ col1 }}) |>
        dplyr::summarize(Denominator = sum(denominator1),
                         Numerator = sum(numerator1),
                         Percentage = (sum(numerator1)/sum(denominator1)*100)) |>
        dplyr::ungroup() |>
        dplyr::mutate({{ col1 }} := "S92000003", .before = 1)

    } else {

      scotland_data <- df |>
        dplyr::filter({{ filter_expr }}) |>
        dplyr::group_by(...) |>
        dplyr::ungroup({{ col1 }}) |>
        dplyr::summarize(Denominator = sum({{ denominator }}),
                         Numerator = sum({{ numerator }}),
                         Percentage = (sum({{ numerator }})/sum({{ denominator }})*100)) |>
        dplyr::ungroup() |>
        dplyr::mutate({{ col1 }} := "S92000003", .before = 1)

    }

    total_data <- rbind(hb_data, scotland_data)

    col1 <- colnames(total_data[1])

    total_data[[{{ col1 }}]] <- factor(total_data[[{{ col1 }}]],
                                       levels = fct_levels)

    total_data |>
      tidyr::complete(..., explicit = FALSE)

  } else {

    hb_data

  }
}
