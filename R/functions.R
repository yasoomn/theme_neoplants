# load necessary libraries and the configurations for the RhizoGene theme. This code block needs to be executed every time Rstudio is opened.
#' @export
theme_rhizogene <- function(){
  font <- "Atkinson Hyperlegible"   #assign font family up front
  text_color <- "#2A2C2A"
  background_color = "#FFFADF"

  ggplot2::theme_classic() %+replace%    #replace elements we want to change

    theme(

      #grid elements
      panel.grid.major = ggplot2::element_line(color = "#2A2C2A80",
                                               linewidth = .1),
      panel.grid.minor = ggplot2::element_blank(),    #strip minor gridlines
      axis.ticks = ggplot2::element_blank(),          #strip axis ticks

      #text elements
      plot.title = ggplot2::element_text(             #title
        family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 3,
        color = text_color),               #raise slightly

      plot.subtitle = ggplot2::element_text(          #subtitle
        family = font,            #font family
        size = 14,
        color = text_color,
        hjust = 0,
        vjust = 1.5),      #font size

      plot.caption = ggplot2::element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1,
        color = text_color),      #right align

      axis.title = ggplot2::element_text(             #axis titles
        family = font,            #font family
        size = 12,
        color = text_color),               #font size

      axis.text = ggplot2::element_text(              #axis text
        family = font,            #axis famuly
        size = 10,
        color = text_color),                #font size

      axis.text.x = ggplot2::element_text(            #margin for axis text
        margin=margin(t = 5, b = 10),
        color = text_color),
      axis.line = ggplot2::element_line(color = text_color),
      #aspect.ratio = 0.8,
      plot.margin = ggplot2::margin(t = 0.9, r = 0.5, b = 0.5, l = 0.5, unit = "line"),

      #since the legend often requires manual tweaking
      #based on plot content, don't define it here

      ## backgronunds to match wiki
      panel.background = ggplot2::element_rect(fill = background_color, color = NA, ),
      plot.background = ggplot2::element_rect(fill = background_color, color = NA),
      legend.background = ggplot2::element_rect(fill = background_color, color = NA)
    )
}

rhizogene_light = c("#668F55", "#AA9AAB", "#8497B3", "#D98896", "#D6DEB0",  "#F0BB54")
rhizogene_dark = c("#474A3B", "#33482B","#424B5A", "#554D56", "#6D444B", "#503E1C")
rhizogene_extra = c("#9072db", "#02117e", "#d815b4", "#155126", "#f23b65", "#a33e12", "#881448", "#ab7b05", "#7212ff", "#f9380a")

#theme_set(theme_rhizogene())
#options(ggplot2.discrete.fill=rhizogene_light,
        #ggplot2.discrete.color = rhizogene_dark
#)

#' @export
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}
