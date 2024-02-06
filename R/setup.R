options(scipen = 999)
utils::globalVariables(c("Year", "Total"))
#palette <- c("#14bfdd", "#80b3d2", "#040404", "#d7d9ce")
# This is ISCTE NEW palette
iscte_palette <- c("#14bfb8","#040404", "#d7d9ce", "#0c7489","#13505b","#35524A","#779CAB", "#666666", "#80B3D2", "#373F47")
iscte_font <- "Times New Roman"
iscte_title_size <- 16
iscte_text_size <- 10
iscte_chart_labels <- 2
iscte_plot_margins <- ggplot2::unit(c(0.3,0.3,0.3,0.3),"cm")
pdf_render <- TRUE
latex_render <- TRUE

# Import and Load fonts
#load("extrafont")
#font_import()
#loadfonts(device="win")       #Register fonts for Windows bitmap output
#fonts()

