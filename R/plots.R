source("R/setup.R")


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
author_per_year <- function( slr, year.above = 1900, output = ".", save.pdf = TRUE)
{
  slr <-  slr$articles
  slr <-  slr[slr$Year > year.above,]
  frequency_per_year_dimension( slr, year.above, output, save.pdf, plot.name = "plot-6.pdf", dimension = "Author" )
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
venue_per_year <- function( slr, year.above = 1900, output = ".", save.pdf = TRUE)
{
  slr <-  slr$articles
  slr <-  slr[slr$Year > year.above,]
  frequency_per_year_dimension( slr, year.above, output, save.pdf, plot.name = "plot-7.pdf", "Venue" )
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
publication_per_year <- function( slr, year.above = 1900, output = ".", save.pdf = TRUE)
{
  slr <-  slr$articles
  slr <-  slr[slr$Year > year.above,]
  frequency_per_year_dimension( slr, year.above, output, save.pdf, plot.name = "plot-8.pdf", "Publication", width = 20 )
}

#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#' @param plot.name Name of the file to plot data into
#' @param dimension The dimension name to report against
#'
#' @return
#' @export
#'
#' @examples
plot_dimension_per_year <- function( dimension, slr, year.above = 1900, output = ".", save.pdf = TRUE, plot.name = "new_plot.pdf"  )
{
  slr <-  slr$articles
  slr <-  slr[slr$Year > year.above,]
  slr_exploded <- slr %>% tidyr::separate_rows( dimension , sep = ",")
  frequency_per_year_dimension( slr_exploded, year.above, output, save.pdf, plot.name = paste0("plot-",dimension,".pdf"), dimension = dimension )
}

#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#' @param plot.name Name of the file to plot data into
#' @param dimension The dimension name to report against
#'
#' @return
#' @export
#'
#' @examples
plot_dimension_frequency <- function( dimension, slr, year.above = 1900, output = ".", save.pdf = TRUE, plot.name = "new_plot.pdf" )
{
  slr <-  slr$articles
  slr <-  slr[slr$Year > year.above,]
  frequency_plot_per_dimension( slr, year.above, output, save.pdf, plot.name = paste0("plot-freq-",dimension,".pdf"), dimension = dimension )
}


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
  slr <-  slr$articles
  slr <-  slr[slr$Year > year.above,]

  g <- ggplot2::ggplot( slr , ggplot2::aes(factor(Year),Total) ) +
    ggplot2::labs(x = "\nYear") +
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
  slr <-  slr$articles
  slr <-  slr[slr$Year > year.above,]

  myData <- data.frame(
    names = levels(factor(slr$Year)),
    values = summary(factor(slr$Year))
  )

  ggp <- ggplot2::ggplot(myData,ggplot2::aes(x=names,y=values)) +
    ggplot2::labs(x = "\nYear") +
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
  slr <-  slr$articles
  slr <-  slr[slr$Year > year.above,]

  myData <- data.frame(
    names = levels(factor(slr$Venue)),
    values = summary(factor(slr$Venue))
  )

  ggp <- ggplot2::ggplot(myData,ggplot2::aes(x=names,y=values)) +
    ggplot2::labs(x = "\nYear") +
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
  slr <-  slr$articles
  slr <-  slr[slr$Year > year.above,]

  myData <- data.frame(
    names = levels(factor(slr$Author)),
    values = summary(factor(slr$Author))
  )

  ggp <- ggplot2::ggplot(myData,ggplot2::aes(x=names,y=values)) +
    ggplot2::labs(x = "\nYear") +
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
  ggp + ggplot2::geom_bar(fill=iscte_palette[3], position = 'dodge', stat='identity') +
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
  slr <-  slr$articles
  slr <-  slr[slr$Year > year.above,]

  myData <- data.frame(
    names = levels(factor(slr$Publication)),
    values = summary(factor(slr$Publication))
  )

  ggp <- ggplot2::ggplot(myData,ggplot2::aes(x=names,y=values)) +
    ggplot2::labs(x = "\nYear") +
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
  ggp + ggplot2::geom_bar(fill=iscte_palette[4], position = 'dodge', stat='identity') +
    ggplot2::geom_text(ggplot2::aes(label=values), position=ggplot2::position_dodge(width=0.9), vjust=-0.25)

  if ( save.pdf )  ggplot2::ggsave( paste0( output, "/" , plot.name ), width = 12.5, height = 7)

}


frequency_per_year_dimension <- function( slr, year.above, output, save.pdf, plot.name, dimension, width = 12.5 )
{
  data <- stats::aggregate(slr, by=list(slr$Year, with(slr, get(dimension)) ), FUN=length)

  g <- ggplot2::ggplot(data, ggplot2::aes(x=as.factor(Group.1), y=Group.2, size=Study, fill=factor(Group.1))) +
    ggplot2::geom_point(alpha=0.5, shape=21, color="black") +
    ggplot2::scale_fill_manual(values= sample( iscte_palette, length(data$Group.1), replace = T) ) +
    ggplot2::scale_size_area( max_size = 20 ) +
    hrbrthemes::theme_ipsum(base_family = iscte_font) +
    ggplot2::theme(legend.position="none",
                   axis.title.x = ggplot2::element_text(size = iscte_title_size ),
                   axis.text.x = ggplot2::element_text(size = iscte_text_size ),
                   axis.title.y = ggplot2::element_text(size = iscte_title_size ),
                   axis.text.y = ggplot2::element_text(size = iscte_text_size )
    ) +
    ggplot2::ylab( dimension ) +
    ggplot2::xlab("\nYear") +
    ggplot2::geom_text(ggplot2::aes(label=Study), size=4, position=ggplot2::position_dodge(width=0), vjust=0.4)

  if ( save.pdf )  ggplot2::ggsave( paste0( output, "/" , plot.name ), width = width, height = 7)

}


frequency_plot_per_dimension <- function( slr, year.above, output, save.pdf, plot.name, dimension, width = 12.5 )
{
  Dimension <- factor(unlist(strsplit( with(slr, get(dimension)), split=",")))
  SumDimension <- data.frame( group=as.factor(dimension), names = levels(factor(Dimension)), names_latex = paste0("\\textbf{",levels(factor(Dimension)),"}"), values = summary(factor(Dimension)), percent = paste0(round(summary(factor(Dimension))/nrow(slr)*100,2),"\\%"), studies = "", stringsAsFactors = FALSE, row.names = NULL)
  SumDimension <- dplyr::arrange(SumDimension, -values)

  ggp <- ggplot2::ggplot( SumDimension, ggplot2::aes(x=names,y=values, width=0.5)) +
    ggplot2::labs(x = "") +
    ggplot2::labs(y = "") +
    hrbrthemes::theme_ipsum(base_family = iscte_font) +
    ggplot2::theme(plot.margin = iscte_plot_margins, legend.position="none",
                   legend.title = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(size = iscte_title_size,  ),
                   axis.text.x = ggplot2::element_text(size = iscte_text_size, angle = 45, hjust = 1),
                   axis.title.y = ggplot2::element_text(size = iscte_title_size ),
                   axis.text.y = ggplot2::element_text(size = iscte_text_size )  ) +
                   ggplot2::scale_x_discrete(labels=function(x){sub("\\s", "\n", x)} )

  # counts
  ggp + ggplot2::geom_bar(fill=iscte_palette[1], position = 'dodge', stat='identity') +
    ggplot2::geom_text(ggplot2::aes(label=values), position=ggplot2::position_dodge(width=0.9), vjust=-0.25)


  if ( save.pdf )  ggplot2::ggsave( paste0( output, "/" , plot.name ), width = width, height = 7)

}


#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#' @param plot.name Plot name
#' @param width Plot Width
#'
#' @return
#' @export
#'
#' @examples
frequency_per_continent <- function( slr, year.above, output, save.pdf = T, plot.name = "SumContinents.pdf", width = 12.5 )

{

  table <-  slr$authors
  n_studies <- length(unique(table$Study))
  dataset <- unique(table[c("Continent","Study")])
  studies <- data.frame( studies = stats::aggregate(Study ~ Continent, data = dataset, toString)[2])
  colnames(studies)[1] <- c("studies")
  SumContinents <- data.frame(  group=as.factor("Continents"), names = levels(factor(dataset$Continent)), names_latex = paste0("\\textbf{",levels(factor(dataset$Continent)),"}"),   values = summary(factor(dataset$Continent)), percent = paste0(round(summary(factor(dataset$Continent))/n_studies*100,2),"\\%"), studies = studies, stringsAsFactors = FALSE, row.names = NULL)
  SumContinents <- dplyr::arrange(SumContinents, -values)

  SumContinents$fraction <- SumContinents$values / n_studies
  # Compute the cumulative percentages (top of each rectangle)
  SumContinents$ymax = cumsum(SumContinents$fraction)
  # Compute the bottom of each rectangle
  SumContinents$ymin = c(0, head(SumContinents$ymax, n=-1))
  # Compute label position
  SumContinents$labelPosition <- (SumContinents$ymax + SumContinents$ymin) / 2
  # Compute a good label
  SumContinents$label <- paste0(gsub(" "," ",SumContinents$names), " : ", SumContinents$values, "\n(", round(SumContinents$fraction * 100 , 2), "%)")

  cont <- ggplot2::ggplot(SumContinents, ggplot2::aes(ymax=ymax, ymin=ymin, xmax=4.3, xmin=0.5, fill=names)) +
    ggplot2::geom_rect() +
    ggplot2::geom_text( x=6.2, ggplot2::aes(y=labelPosition, label=label, color=names), size=4 ) + # x here controls label position (inner / outer)
    #scale_fill_brewer(palette=3) +
    scale_fill_iscte() +
    scale_color_iscte() +
    #scale_color_brewer(palette=3) +
    ggplot2::coord_polar(theta="y") +
    ggplot2::xlim(c(-3, 6)) +
    hrbrthemes::theme_ipsum(base_family = iscte_font) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")


  if ( save.pdf )  ggplot2::ggsave( paste0( output, "/" , plot.name ), width = width, height = 7)

}


#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#' @param plot.name Plot name
#' @param width Plot Width
#'
#' @return
#' @export
#'
#' @examples
frequency_per_country <- function( slr, year.above, output, save.pdf = T, plot.name = "SumCountries.pdf", width = 12.5 )

{

  suppressWarnings({

  table <-  slr$authors
  table$Study <- paste0("[", table$Study , "]")
  n_studies <- length(unique(table$Study))
  dataset <- unique(table[c("Country","Study")])
  studies <- data.frame( studies = stats::aggregate(Study ~ Country, data = dataset, toString)[2])
  colnames(studies)[1] <- c("studies")
  SumCountries <- data.frame(  group=as.factor("Countries"), names = levels(factor(dataset$Country)), names_latex = paste0("\\textbf{",levels(factor(dataset$Country)),"}"),   values = summary(factor(dataset$Country)), percent = paste0(round(summary(factor(dataset$Country))/n_studies*100,2),"\\%"), studies = studies, stringsAsFactors = FALSE, row.names = NULL)
  SumCountries <- dplyr::arrange(SumCountries, -values)

  country_plot <- ggplot2::ggplot(SumCountries, ggplot2::aes(x=reorder(names,values), y=values)) +
    ggplot2::labs(x = "") +
    ggplot2::labs(y = "") +
    hrbrthemes::theme_ipsum(base_family = iscte_font) +
    ggplot2::theme(plot.margin = iscte_plot_margins, legend.position="none", legend.title = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_text(size = iscte_title_size ),
          axis.text.x = ggplot2::element_text(size = iscte_text_size ),
          axis.title.y = ggplot2::element_text(size = iscte_title_size ),
          axis.text.y = ggplot2::element_text(size = iscte_text_size )
          #axis.title.x=element_blank(),
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank()
    )
  #xlab("Year")
  #ylab("")


  # counts
  country_plot <- country_plot + ggplot2::geom_bar(fill=iscte_palette[3], position = 'dodge', stat='identity') + ggplot2::coord_flip() +
    ggplot2::geom_text( ggplot2::aes(label=values), position=ggplot2::position_dodge(width=1.9), hjust=-.35)

  if ( save.pdf )  ggplot2::ggsave( paste0( output, "/" , plot.name ), width = width, height = 7)

  })

}


#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#' @param plot.name Plot name
#' @param width Plot Width
#' @param frequency.above Plot only Institutions with more than (n) studies
#'
#' @return
#' @export
#'
#' @examples
frequency_per_institution <- function( slr, year.above, output, save.pdf = T, frequency.above = 1, plot.name = "SumInstitutions.pdf", width = 12.5 )

{
  suppressWarnings({

  table <-  slr$authors
  table$Study <- paste0("[", table$Study , "]")
  n_studies <- length(unique(table$Study))
  dataset <- unique(table[c("Institution","Study")])
  studies <- data.frame( studies = stats::aggregate(Study ~ Institution, data = dataset, toString)[2])
  colnames(studies)[1] <- c("studies")
  SumInstitutions <- data.frame(  group=as.factor("Countries"), names = levels(factor(dataset$Institution)), names_latex = paste0("\\textbf{",levels(factor(dataset$Institution)),"}"),   values = summary(factor(dataset$Institution)), percent = paste0(round(summary(factor(dataset$Institution))/n_studies*100,2),"\\%"), studies = studies, stringsAsFactors = FALSE, row.names = NULL)
  SumInstitutions <- dplyr::arrange(SumInstitutions, -values)

  country_plot <- ggplot2::ggplot(SumInstitutions[SumInstitutions$values > frequency.above, ], ggplot2::aes(x=reorder(names,values), y=values)) +
    ggplot2::labs(x = "") +
    ggplot2::labs(y = "") +
    hrbrthemes::theme_ipsum(base_family = iscte_font) +
    ggplot2::theme(plot.margin = iscte_plot_margins, legend.position="none", legend.title = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(size = iscte_title_size ),
                   axis.text.x = ggplot2::element_text(size = iscte_text_size ),
                   axis.title.y = ggplot2::element_text(size = iscte_title_size ),
                   axis.text.y = ggplot2::element_text(size = iscte_text_size )
                   #axis.title.x=element_blank(),
                   #axis.text.x=element_blank(),
                   #axis.ticks.x=element_blank()
    )
  #xlab("Year")
  #ylab("")


  # counts
  country_plot <- country_plot + ggplot2::geom_bar(fill=iscte_palette[4], position = 'dodge', stat='identity') + ggplot2::coord_flip() +
    ggplot2::geom_text( ggplot2::aes(label=values), position=ggplot2::position_dodge(width=1.9), hjust=-.35)

  if ( save.pdf )  ggplot2::ggsave( paste0( output, "/" , plot.name ), width = width, height = 7)

  })

}


