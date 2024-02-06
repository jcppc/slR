source("R/setup.R")

#' Title
#'
#' @param SLR_Extracted Dataframe
#' @param output Folder to put plots
#'
#' @return
#' @export
#'
#' @examples
plot_summary <- function(SLR_Extracted, output = ".")
{

#if (pdf_render) grDevices::cairo_pdf(family = iscte_font, onefile = TRUE, file = paste0(output,"/","plot-1.pdf"))

g <- ggplot2::ggplot(SLR_Extracted , ggplot2::aes(factor(Year),Total)) +
  ggplot2::labs(x = "Year") +
  ggplot2::labs(y = "Score") +
  ggplot2::ggtitle(paste0('Studies',"\n(n=",nrow(SLR_Extracted)-1,")")) +
  hrbrthemes::theme_ipsum(base_family = iscte_font) +
  ggplot2::theme(plot.margin = iscte_plot_margins, legend.position="none", legend.title = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
        #plot.title = element_text(hjust=0.5),
        plot.title = ggplot2::element_text(size = iscte_text_size, vjust = -25),
        axis.title.x = ggplot2::element_text(size = iscte_title_size ),
        axis.text.x = ggplot2::element_text(size = iscte_text_size ),
        axis.title.y = ggplot2::element_text(size = iscte_title_size ),
        axis.text.y = ggplot2::element_text(size = iscte_text_size )
        #plot.caption = element_text(hjust=0.5, size=rel(1.2)),
        #axis.title.x=element_blank(),
        #axis.title.x="X Title",
        #axis.text.x=element_blank(),
        #axis.text.x="Another Title",
        #axis.ticks.x=element_blank()
  )
#xlab("Year 162")
#ylab("")

g +
  #geom_boxplot(fill = "white") +
  ggplot2::geom_boxplot(width=0.25, color="black", alpha=0.2) +
  ggplot2::geom_violin(data=SLR_Extracted[SLR_Extracted$Year > 2012, ], fill = iscte_palette[9], width=1.2, alpha = 0.6)
  #geom_violin(fill = iscte_palette[1], width=0.5) +
  #geom_jitter(data = SLR_Extracted, size = 2, aes(colour = factor(Year)), width = 0.3, height = 0) +
  #scale_color_iscte()

#grDevices::pdf(paste0(output,"/","plot-1.pdf"), width = 12.5, height = 7)
#print(g)
#g
ggplot2::ggsave(paste0( output, "/" , "plot-1.pdf"), width = 12.5, height = 7)
#grDevices::dev.off()

}
