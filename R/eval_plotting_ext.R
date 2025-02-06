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
#'   scale_color_brand(labels = c("M-1", "M-2", "M-3"))
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


