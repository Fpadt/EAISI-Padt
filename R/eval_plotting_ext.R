#' Get a named brand color from the config.
#'
#' @param .color_name A character string, e.g. "primary" or "accent".
#' @return A string with the color hex code (e.g. "#123456").
#' @export
pa_brand_colors_get <- function(
    .color_name) {

  brand_colors <- pa_config_value_get("theme.brand_colors")

  if (!.color_name %in% names(brand_colors)) {
    stop(sprintf("Color name '%s' not found in config.", .color_name))
  }

  brand_colors[[.color_name]]
}

#' Get a palette color from the config by index.
#'
#' @param .index Integer index, e.g. 1, 2, 3...
#' @return A string with the color hex code.
#' @export
pa_palette_get <- function(
    .index) {

  palette <- pa_config_value_get("theme.color_palette")

  if (.index < 1 || .index > length(palette)) {
    stop(sprintf("Index %d out of range (1..%d).", .index, length(palette)))
  }

  palette[[.index]]
}

#' Get a color by name or index.
#'
#' @param .name_or_index Either a string (name of the color) or a numeric index.
#' @return A string with the color hex code.
#' @export
pa_brand_color_get <- function(
    .name_or_index) {

  if (is.character(.name_or_index)) {
    return(pa_brand_colors_get(.name_or_index))
  } else if (is.numeric(.name_or_index)) {
    return(pa_palette_get(.name_or_index))
  } else {
    stop("Argument 'name_or_index' must be either a color name or an integer index.")
  }
}

#' A ggplot2 theme that applies brand styling from the YAML config.
#'
#' @return A ggplot2 theme object.
#' @export
pa_brand_theme <- function() {

  # Example: a minimal theme that sets basic backgrounds, text, etc.
  ggplot2::theme_minimal() %+replace%
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      text = ggplot2::element_text(family = "sans", face = "plain"),
      # Add or adjust more elements here
    )
}


#' Custom scale_color_manual using brand colors from YAML
#'
#' This function retrieves colors from the brand color palette in the YAML configuration
#' and applies them to ggplot2's scale_color_manual() based on the number of labels.
#'
#' @param labels Character vector of labels to be used in the legend.
#' @return A ggplot2 scale_color_manual object.
#' @examples
#' \dontrun{
#' ggplot(data, aes(x, y, color = group)) +
#'   geom_line() +
#'   pa_brand_color_scale(labels = c("M-1", "M-2", "M-3"))
#'   }
#' @export
pa_brand_color_scale <- function(labels) {

  # Retrieve the color palette from YAML
  brand_palette <- pa_config_value_get("theme.color_palette")

  # Ensure YAML config is available
  if (is.null(brand_palette)) {
    stop("Brand colors are not defined in the YAML configuration.")
  }

  # Ensure the required number of colors are available
  num_labels <- length(labels)
  if (num_labels > length(brand_palette)) {
    stop("Not enough brand colors defined in YAML for the number of labels.")
  }

  # Select the required number of colors
  selected_colors <- brand_palette[1:num_labels]

  # Apply scale_color_manual() with the selected colors
  ggplot2::scale_color_manual(labels = labels, values = selected_colors)
}

#' Plot a Scaled Time Series as Columns
#'
#' Creates a column chart from time series data, scaled by a specified factor,
#' and optionally padding the y-axis range. This function is designed to display
#' historical sales data by month with branding colors.
#'
#' @param .dtTS A \code{data.table} containing at least:
#'   \describe{
#'     \item{y}{Numeric values representing sales or other metric}
#'     \item{MATERIAL}{A factor or character vector of material codes}
#'     \item{SALESORG}{A factor or character vector of sales organization codes}
#'     \item{CALMONTH}{A date or factor representing month/year}
#'   }
#' @param .scale A numeric scale factor to divide the \code{y} column by.
#'   Default is \code{1000}.
#' @param .padding A numeric value indicating how much vertical padding should
#'   be added to the plot’s y-axis range. Default is \code{0.1}.
#'
#' @return A \code{ggplot} object showing a column plot of the scaled data.
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' # Example data
#' dt_example <- data.table(
#'   CALMONTH = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
#'   y = round(runif(12, 100, 500)),
#'   MATERIAL = "MATERIAL_001",
#'   SALESORG = "ORG01"
#' )
#'
#' # Plot, scaling by 1000
#' pa_ts_col_plot(dt_example, .scale = 1000, .padding = 0.2)
#' }
pa_ts_col_plot <- function(
    .dtTS,
    .scale   = 1000,
    .padding = 0.1
) {
  # Scale
  dtTS <- copy(.dtTS)[, y := y / .scale]

  # Get the min and max values
  min_value <- min(dtTS$y)
  max_value <- max(dtTS$y)

  # Get the unique material and sales org
  matl <- unique(dtTS$MATERIAL)[1] %>% pa_matn1_output()
  sorg <- unique(dtTS$SALESORG)[1]

  # Plot the data
  ggplot(
    data    = dtTS,
    mapping = aes(x = CALMONTH, y = y)
  ) +
    geom_col(
      color = pa_brand_color_get("neutral1"),
      fill  = pa_brand_color_get("primary")
    ) +
    coord_cartesian(
      ylim = c(min_value - .padding, max_value + .padding)
    ) +
    labs(
      title    = "Historical Sales",
      subtitle = paste(matl, "for", sorg),
      x        = "Month/Year",
      y        = paste0("Sales ('x", .scale, "')")
    )
}
