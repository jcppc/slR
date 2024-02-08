source("R/setup.R")

#' Boxplot chart to represent the articles' score per year
#'
#' @param slr The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#'
#' @return
#' @export
#'
#' @examples
score_per_year_boxplot <- function( slr, year.above = 1900, output = ".", save.pdf = TRUE)
{

plot.name <- "plot-1.pdf"
slr <-  slr[slr$Year > year.above,]

g <- ggplot2::ggplot( slr , ggplot2::aes(factor(Year),Total) ) +
     ggplot2::labs(x = "Year") +
     ggplot2::labs(y = "Score") +
     ggplot2::ggtitle(paste0('Studies',"\n(n=",nrow(slr),")")) +
     hrbrthemes::theme_ipsum(base_family = iscte_font) +
     ggplot2::theme( plot.margin = iscte_plot_margins, legend.position="none",
                     legend.title = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(size = iscte_text_size, vjust = -25),
                     axis.title.x = ggplot2::element_text(size = iscte_title_size ),
                     axis.text.x = ggplot2::element_text(size = iscte_text_size ),
                     axis.title.y = ggplot2::element_text(size = iscte_title_size ),
                     axis.text.y = ggplot2::element_text(size = iscte_text_size ) )

g + ggplot2::geom_boxplot(width=0.25, color="black", alpha=0.2) +
    ggplot2::geom_violin(data=slr, fill = iscte_palette[9], width=1.2, alpha = 0.6)

if ( save.pdf )  ggplot2::ggsave( paste0( output, "/" , plot.name ), width = 12.5, height = 7)

}

#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#'
#' @return
#' @export
#'
#' @examples
score_per_year_barchart <- function( slr, year.above = 1900, output = ".", save.pdf = TRUE)
{
  plot.name <- "plot-2.pdf"
  slr <-  slr[slr$Year > year.above,]

  myData <- data.frame(
    names = levels(factor(slr$Year)),
    values = summary(factor(slr$Year))
  )

  ggp <- ggplot2::ggplot(myData,ggplot2::aes(x=names,y=values)) +
    ggplot2::labs(x = "Year") +
    ggplot2::labs(y = "Frequency") +
    ggplot2::ggtitle(paste0('Studies',"\n(n=",nrow(slr),")")) +
    hrbrthemes::theme_ipsum(base_family = iscte_font) +
    ggplot2::theme(plot.margin = iscte_plot_margins, legend.position="none",
                   legend.title = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = iscte_text_size, vjust = -25),
                   axis.title.x = ggplot2::element_text(size = iscte_title_size ),
                   axis.text.x = ggplot2::element_text(size = iscte_text_size ),
                   axis.title.y = ggplot2::element_text(size = iscte_title_size ),
                   axis.text.y = ggplot2::element_text(size = iscte_text_size ) )

  # counts
  ggp + ggplot2::geom_bar(fill=iscte_palette[1], position = 'dodge', stat='identity') +
    ggplot2::geom_text(ggplot2::aes(label=values), position=ggplot2::position_dodge(width=0.9), vjust=-0.25)

  if ( save.pdf )  ggplot2::ggsave( paste0( output, "/" , plot.name ), width = 12.5, height = 7)

}


#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#'
#' @return
#' @export
#'
#' @examples
score_per_venue_barchart <- function( slr, year.above = 1900, output = ".", save.pdf = TRUE)
{
  plot.name <- "plot-3.pdf"
  slr <-  slr[slr$Year > year.above,]

  myData <- data.frame(
    names = levels(factor(slr$Venue)),
    values = summary(factor(slr$Venue))
  )

  ggp <- ggplot2::ggplot(myData,ggplot2::aes(x=names,y=values)) +
    ggplot2::labs(x = "Year") +
    ggplot2::labs(y = "Frequency") +
    ggplot2::ggtitle(paste0('Studies',"\n(n=",nrow(slr),")")) +
    hrbrthemes::theme_ipsum(base_family = iscte_font) +
    ggplot2::theme(plot.margin = iscte_plot_margins, legend.position="none",
                   legend.title = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = iscte_text_size, vjust = -25),
                   axis.title.x = ggplot2::element_text(size = iscte_title_size ),
                   axis.text.x = ggplot2::element_text(size = iscte_text_size ),
                   axis.title.y = ggplot2::element_text(size = iscte_title_size ),
                   axis.text.y = ggplot2::element_text(size = iscte_text_size ) )

  # counts
  ggp + ggplot2::geom_bar(fill=iscte_palette[2], position = 'dodge', stat='identity') +
    ggplot2::geom_text(ggplot2::aes(label=values), position=ggplot2::position_dodge(width=0.9), vjust=-0.25)

  if ( save.pdf )  ggplot2::ggsave( paste0( output, "/" , plot.name ), width = 12.5, height = 7)

}


#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#'
#' @return
#' @export
#'
#' @examples
score_per_author_barchart <- function( slr, year.above = 1900, output = ".", save.pdf = TRUE)
{
  plot.name <- "plot-4.pdf"
  slr <-  slr[slr$Year > year.above,]

  myData <- data.frame(
    names = levels(factor(slr$Author)),
    values = summary(factor(slr$Author))
  )

  ggp <- ggplot2::ggplot(myData,ggplot2::aes(x=names,y=values)) +
    ggplot2::labs(x = "Year") +
    ggplot2::labs(y = "Frequency") +
    ggplot2::ggtitle(paste0('Studies',"\n(n=",nrow(slr),")")) +
    hrbrthemes::theme_ipsum(base_family = iscte_font) +
    ggplot2::theme(plot.margin = iscte_plot_margins, legend.position="none",
                   legend.title = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = iscte_text_size, vjust = -25),
                   axis.title.x = ggplot2::element_text(size = iscte_title_size ),
                   axis.text.x = ggplot2::element_text(angle = 45, vjust = 1.2, hjust = 1, size = iscte_text_size ),
                   axis.title.y = ggplot2::element_text(size = iscte_title_size ),
                   axis.text.y = ggplot2::element_text(size = iscte_text_size ) )

  # counts
  ggp + ggplot2::geom_bar(fill=iscte_palette[2], position = 'dodge', stat='identity') +
    ggplot2::geom_text(ggplot2::aes(label=values), position=ggplot2::position_dodge(width=0.9), vjust=-0.25)

  if ( save.pdf )  ggplot2::ggsave( paste0( output, "/" , plot.name ), width = 12.5, height = 7)

}

#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#'
#' @return
#' @export
#'
#' @examples
score_per_publication_barchart <- function( slr, year.above = 1900, output = ".", save.pdf = TRUE)
{
  plot.name <- "plot-5.pdf"
  slr <-  slr[slr$Year > year.above,]

  myData <- data.frame(
    names = levels(factor(slr$Publication)),
    values = summary(factor(slr$Publication))
  )

  ggp <- ggplot2::ggplot(myData,ggplot2::aes(x=names,y=values)) +
    ggplot2::labs(x = "Year") +
    ggplot2::labs(y = "Frequency") +
    ggplot2::ggtitle(paste0('Studies',"\n(n=",nrow(slr),")")) +
    hrbrthemes::theme_ipsum(base_family = iscte_font) +
    ggplot2::theme(plot.margin = iscte_plot_margins, legend.position="none",
                   legend.title = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = iscte_text_size, vjust = -25),
                   axis.title.x = ggplot2::element_text(size = iscte_title_size ),
                   axis.text.x = ggplot2::element_text(angle = 45, vjust = 1.2, hjust = 1, size = iscte_text_size ),
                   axis.title.y = ggplot2::element_text(size = iscte_title_size ),
                   axis.text.y = ggplot2::element_text(size = iscte_text_size ) )

  # counts
  ggp + ggplot2::geom_bar(fill=iscte_palette[2], position = 'dodge', stat='identity') +
    ggplot2::geom_text(ggplot2::aes(label=values), position=ggplot2::position_dodge(width=0.9), vjust=-0.25)

  if ( save.pdf )  ggplot2::ggsave( paste0( output, "/" , plot.name ), width = 12.5, height = 7)

}
