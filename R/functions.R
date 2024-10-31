# load necessary libraries and the configurations for the RhizoGene theme. This code block needs to be executed every time Rstudio is opened.
#' @export
theme_neoplants <- function(text_color = "#0d3331", background_color = "#FFFFFF"){
  font <- "Sohne-Kraftig"   #assign font family up front

  ggplot2::theme_classic() %+replace%    #replace elements we want to change

    theme(

      #grid elements
      panel.grid.major = ggplot2::element_line(color = "#23282880",
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

neoplants_colors = c('#38c6a3', '#174c47', '#99d9c9', '#c7ffca', '#0d3331')
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

#' @export
Summarize.plant.data <- function(csv) {

  is.dead = function(a) {
    ifelse(a == "dead", TRUE, FALSE)
  }



  # creates a column with dead/alive information, and a column with the phenotype (for plotting)
  plantData = read.csv(csv) %>%
    dplyr::mutate(amIdead = is.dead(plant.health)) %>%
    dplyr::mutate(phenotype = dplyr::case_when(
      amIdead == TRUE ~ "Dead",
      RUBY..Yes.no == "no" & new.hairy.tissue == "no" ~ "Not transformed",
      RUBY..Yes.no == "yes" & new.hairy.tissue == "no" ~ "Only RUBY",
      RUBY..Yes.no == "yes" & new.hairy.tissue == "yes" ~ "RUBY + hairy roots",
      RUBY..Yes.no == "no" & new.hairy.tissue == "yes" ~ "Hairy roots"
    ),
    isRuby = ifelse(RUBY..Yes.no == "yes", TRUE, FALSE),
    amIruby  = case_when(
      RUBY..Yes.no == "no" & new.hairy.tissue == "no" ~ FALSE,
      RUBY..Yes.no == "yes" & new.hairy.tissue == "no" ~ TRUE,
      RUBY..Yes.no == "yes" & new.hairy.tissue == "yes" ~ TRUE,
      RUBY..Yes.no == "no" & new.hairy.tissue == "yes" ~ FALSE
    )
    )

  plantData$phenotype <- as.factor(plantData$phenotype)

  # select plants that are dead on day 3
  deadAfter3days = plantData %>% dplyr::filter(Day == 3 & amIdead == T) %>% dplyr::pull(plant.no.)

  # remove plants that are dead on day 3 from the data set, they were most likely damaged durign handling
  plantDataTrimmed = plantData %>% dplyr::filter(!plant.no. %in% deadAfter3days)

  calculateRuby = function(a){
    plantDataTrimmed %>% dplyr::group_by(isRuby, Day) %>%
      dplyr::summarise(count = n() ,.groups = "keep") %>%
      dplyr::filter(Day == a & isRuby == TRUE) %>%
      dplyr::pull(count)
  }


  # create a data frame summarizing the values, containing the percentage for each phenotype
  SummaryPlants = plantDataTrimmed %>%
    dplyr::group_by(Day, phenotype, .drop = F)%>%
    dplyr::summarise(count = n() ,.groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::add_row(Day = 10, phenotype = "RUBY", count = calculateRuby(10)) %>%
    dplyr::add_row(Day = 3, phenotype = "RUBY", count = calculateRuby(3)) %>%
    dplyr::mutate(perc = (count/(nrow(plantDataTrimmed)/length(unique(plantDataTrimmed$Day)))))



  list(SummaryPlants, plantData, plantDataTrimmed)
}

#' @export
Create.plant.bar.plot <- function(plantData, title = "", subtitle = "" ) {
  as.data.frame(plantData[[1]])%>%
    dplyr::  filter(phenotype != "Only RUBY") %>%
    ggplot2::ggplot(ggplot2::aes(x = as.factor(Day), y = perc, fill = phenotype)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.8), width = 0.6, color = "#2a2c2a") +
    ggplot2::scale_fill_manual(values = c("Dead" = "#2a2c2a",
                                          "RUBY" = "#D98896",
                                          "Hairy roots" = "#8497B3",
                                          "RUBY + hairy roots" = "#AA9AAB",
                                          "Not transformed" = "#668F55",
                                          "Only RUBY" = "#D98896")) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::scale_x_discrete(labels = function(a){paste(a, "dpi")}) +
    ggplot2::annotate("text",
                      x = length(unique(plantData[[3]]$Day))+0.45,
                      y = (max(plantData[[1]]$perc) + 0.05),
                      label = paste("N =", (nrow(plantData[[3]])/length(unique(plantData[[3]]$Day))))) +
    ggplot2::labs(y = "Phenotype in %", x = "", fill = "Phenotype", title = title, subtitle = subtitle) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}
