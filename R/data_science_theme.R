#' Function to implement a data science theme for ggplot2
#'
#' @usage theme_DS()
#' @param ... additional parameters
#' @export
theme_DS <- function(font_family = "Fira Code"){

  black = "#000000"
  # Begin construction of chart. Build from the black + white theme

  ggplot2::theme_bw(base_size=12, base_family = font_family) +

    # Title
    ggplot2::theme(plot.title = ggplot2::element_text(color = black,
                                                      size = 16,
                                                      vjust= 1.25,
                                                      hjust = 0.5)) +

    # Subtitle
    ggplot2::theme(plot.subtitle = ggplot2::element_text(color = black,
                                                         size = 12,
                                                         vjust= 1.25,
                                                         hjust = 0.5)) +

    # X-axis
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8,
                                                       color = black)) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10,
                                                        color = black,
                                                        vjust = 0)) +

    # Y-axis
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8,
                                                       color = black)
    ) +

    ggplot2::theme(axis.title.y = ggplot2::element_text(size =10,
                                                        color = black,
                                                        vjust = 1.25)) +

    # Plot panel (just the grided part)
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent",
                                                            color = "transparent")) +
    ggplot2::theme(panel.border = ggplot2::element_rect(color = "transparent")) +

    # Plot Background (entire frame)
    ggplot2::theme(legend.key = ggplot2::element_rect(fill = "transparent")) +

    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "transparent",
                                                           color = "transparent")) +

    # Format the grid
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color = DS_colors[["dark grey"]],
                                                            size = 0.25)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +

    # Format the legend
    ggplot2::theme(legend.background = ggplot2::element_rect(fill = "transparent",
                                                             color = "transparent",
                                                             size = 0.25)) +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 8,
                                                       color = black),
                   legend.title = ggplot2::element_text(size = 10,
                                                        color = black)) +

    # Facet strips
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = DS_colors[["light grey"]]),
                   strip.text = ggplot2::element_text(color = black, size = 10),
                   strip.text.y = ggplot2::element_text(angle = -90)) +

    # Plot margins
    ggplot2::theme(plot.margin = ggplot2::unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

#' List of DStheme colors
#'
DS_colors <- c(
  `red`        = "#d11141",
  `green`      = "#00b159",
  `blue`       = "#00aedb",
  `orange`     = "#f37735",
  `yellow`     = "#ffc425",
  `light grey` = "#cccccc",
  `dark grey`  = "#8c8c8c")

#' Function to extract DS colors as hex codes
#'
#' @param ... Character names of DS_colors
#'
DS_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (DS_colors)

  DS_colors[cols]
}

DS_palettes <- list(
  `main`  = DS_cols("blue", "green", "yellow"),

  `cool`  = DS_cols("blue", "green"),

  `hot`   = DS_cols("yellow", "orange", "red"),

  `mixed` = DS_cols("blue", "green", "yellow", "orange", "red"),

  `grey`  = DS_cols("light grey", "dark grey")
)

#' Return function to interpolate a DS color palette
#'
#' @param palette Character name of palette in DS_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
DS_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- DS_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}

#' Color scale constructor for DS colors
#'
#' @param palette Character name of palette in DS_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_DS <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- DS_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("DS_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for DS colors
#'
#' @param palette Character name of palette in DS_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_DS <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- DS_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("DS_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
