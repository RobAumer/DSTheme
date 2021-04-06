theme_DS <- function(font_family = "Fira Code"){
  
  black = "#000000"
  # Begin construction of chart. Build from the black + white theme
  
  theme_bw(base_size=12, base_family = font_family) + 
    
    # Title
    theme(plot.title = element_text(color = black,
                                       size = 16,
                                       vjust= 1.25,
                                       hjust = 0.5)) +
    
    # Subtitle
    theme(plot.subtitle = element_text(color = black,
                                       size = 12,
                                       vjust= 1.25,
                                       hjust = 0.5)) +
    
    # X-axis
    theme(axis.text.x=element_text(size = 8,
                                   color = black)) +
    theme(axis.title.x=element_text(size = 10,
                                    color = black,
                                    vjust = 0)) +
    
    # Y-axis
    theme(axis.text.y=element_text(size = 8,
                                   color = black)
    ) +
    
    theme(axis.title.y=element_text(size =10,
                                    color = black,
                                    vjust = 1.25)) +
    
    # Plot panel (just the grided part)
    theme(panel.background = element_rect(fill = "transparent",
                                          color = "transparent")) +
    theme(panel.border = element_rect(color = "transparent")) +
    
    # Plot Background (entire frame)
    theme(legend.key = element_rect(fill = "transparent")) +
    
    theme(plot.background = element_rect(fill = "transparent",
                                         color = "transparent")) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color = DS_colors[["dark grey"]],
                                        size = 0.25)) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.ticks = element_blank()) +
    
    # Format the legend
    theme(legend.background = element_rect(fill = "transparent",
                                           color = "transparent",
                                           size = 0.25)) +
    theme(legend.text = element_text(size = 8,
                                     color = black),
          legend.title = element_text(size = 10,
                                      color = black)) +
    
    # Facet strips
    theme(strip.background = element_rect(fill = DS_colors[["light grey"]],
                                          # color = "transparent"
    ),
    strip.text = element_text(color = black,
                              size = 10),
    strip.text.y = element_text(angle = -90)
    
    ) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

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
  
  colorRampPalette(pal, ...)
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
    discrete_scale("colour", paste0("DS_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
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
    discrete_scale("fill", paste0("DS_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
