options(scipen = 999)
utils::globalVariables(c("Year", "Total"))
#palette <- c("#14bfdd", "#80b3d2", "#040404", "#d7d9ce")
# This is ISCTE NEW palette
iscte_palette <- c("#14bfb8","#040404", "#d7d9ce", "#0c7489","#13505b","#35524A","#779CAB", "#666666", "#80B3D2", "#373F47")
#iscte_palette <- sample(base_colors, 100, replace = T)
iscte_font <- "Times New Roman"
iscte_title_size <- 16
iscte_text_size <- 10
iscte_chart_labels <- 2
iscte_plot_margins <- ggplot2::unit(c(0.3,0.3,0.3,0.3),"cm")
pdf_render <- TRUE
latex_render <- TRUE

# Import and Load fonts
#load("extrafont")
#extrafont::font_import()
#loadfonts(device="win")       #Register fonts for Windows bitmap output
#fonts()

iscte_palette_discrete <- function( pal = iscte_palette , reverse = FALSE) { if (reverse) pal <- rev(pal); return ( grDevices::colorRampPalette( pal ) ) }

iscte_palette_continuous <- function( size = 100,  pal = iscte_palette ) { return ( iscte_palette_discrete( pal )( size) )  }

iscte_palette_gradient <- function( size = 1000, pal = iscte_palette ) { return ( iscte_palette_discrete( pal )( size ) )  }


scale_color_iscte <- function( palette = iscte_palette, discrete = TRUE, reverse = FALSE ) {

  pal <- iscte_palette_discrete( palette, reverse )

  if (discrete) {
    ggplot2::discrete_scale("colour", "iscte_discrete" , palette = pal )
  } else {
    ggplot2::scale_color_gradientn(colours = pal(100) )
  }

}

scale_fill_iscte <- function( palette = iscte_palette, discrete = TRUE, reverse = FALSE ) {

  pal <- iscte_palette_discrete( palette, reverse )

  if (discrete) {
    ggplot2::discrete_scale("fill", "iscte_discrete" , palette = pal )
  } else {
    ggplot2::scale_color_gradientn(colours = pal(100) )
  }

}

scale_gradient_iscte <- function( palette = iscte_palette, reverse = FALSE ) {

  pal <- iscte_palette_discrete( palette, reverse )

  ggplot2::scale_color_gradientn(colours = pal(1000) )

}

