
theme_custom <- ggplot2::theme_minimal(base_size=baseSize) +
  # ggplot2::theme(panel.border = ggplot2::element_rect(colour="gray80")) +
  ggplot2::theme(axis.text=ggplot2::element_text(colour="gray40", size=baseSize-0)) +
  ggplot2::theme(axis.title=ggplot2::element_text(colour="gray40")) +
  # ggplot2::theme(panel.border = ggplot2::element_rect(colour="gray80")) +
  # ggplot2::theme(axis.ticks.length = grid::unit(0, "cm")) +
  ggplot2::theme(text = element_text(size =baseSize))+
  # ggplot2::theme(panel.grid = element_rect(linetype = "dashed"))+
  ggplot2::theme(panel.grid.major.x = element_blank())+
  # ggplot2::theme(panel.grid.major.y = element_blank())+
  ggplot2::theme(panel.grid.minor.x = element_blank())+
  # ggplot2::theme(panel.grid.minor.y = element_blank())+
  ggplot2::theme(axis.text.x = element_text(size = baseSize+10, face = "italic"))+
  ggplot2::theme(axis.text.y = element_text(size = baseSize+10, color = "grey50")) +
  ggplot2::theme(axis.title.y = element_text(size = baseSize+10) )+
  ggplot2::theme(legend.text = element_text(size = baseSize+5) )+
  ggplot2::theme(title=ggplot2::element_text(colour="gray20",size = baseSize+2) )




# ----- define-prepare_data-function ---------------------------------------
# this function is used within make_plot_cccs_alluvia()
prepare_data_alluvia_1 <- function(
  d_input
  ,target_metric
  ,target_groupers
  ,...
){
  # browser()
  # values for testing
  # d_input <- ls0$class
  # target_metric = "n_patients"
  # # target_metric = "n_encounters"
  # target_groupers = c("service_group","service_location", "location_class_description")


  d1 <- d_input %>%
    # rename the target variable for easier syntax (part 1)
    dplyr::rename_(.dots = c("target_metric" = target_metric)) %>%
    dplyr::group_by(.dots = target_groupers ) %>%
    dplyr::summarize(
      target_metric = sum(target_metric)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange_(.dots = target_groupers)
  # rename it back (part 2)
  names(d1) <- gsub("^target_metric$",target_metric, names(d1))

  # initiate the anchor object of this production
  l_support <- list()
  l_support[["dataframe"]] <- d1
  return(l_support)
} # how to use:
# l_support <- ls0$class %>%
# l_support <- ls0$class_gender %>%
#   # dplyr::filter(event_year == 2014) %>% # optional filtering to scope application
#   # dplyr::filter(clinical_focus %in% c("MHSU","MHSU-Addictions")) %>%
#   prepare_data_cccs_alluvia(
#     target_metric = "n_patients"
#     ,target_groupers = c(
#       "service_group"
#       ,"service_location"
#       ,"location_class_code"
#     )
#   )
# l_support$dataframe %>% View()

# ---- define-make_plot-function -------------------------------------------
# enter plot description here
make_plot_alluvia_1 <- function(
  d_input
  ,target_metric = "n_patients"
  ,target_axis1  = "service_group"
  ,target_axis2  = "service_location"
  ,target_fill   = NA
  ,font_correction = -4
){
  # browser()
  # target_metric = "n_encounters"
  # target_axis1  = "service_group"
  # target_axis2 = "service_location"
  # target_fill = "gender"

  d1 <- l_support[["dataframe"]]
  # %>%
  #   dplyr::mutate(
  #     location_class_code_f = factor(location_class_code)
  #   )
  #   # script the plot
  g1 <- d1 %>%
    ggplot2::ggplot(
      aes_string(
        y     = target_metric
        ,axis1 = target_axis1
        ,axis2 = target_axis2
      )
    )
  # open optional fill
  if(!is.na(target_fill)){
    g2 <- g1 +
      geom_alluvium(
        aes_string(fill = target_fill)
        # ,width = 1/4
        # ,alpha = .5
      )
  }else{
    g2 <- g1 +
      geom_alluvium(
        fill = "firebrick",
        alpha = .5
      )
  } # close optional fill
  g3 <- g2 +
    geom_stratum(alpha = 0, color = "grey40")+
    # geom_stratum(width = 1/12, fill = NA, color = "black")+
    # https://cran.r-project.org/web/packages/ggalluvial/vignettes/labels.html
    # geom_label(stat = "stratum", label.strata = TRUE)+
    # geom_text(stat = "stratum", label.strata = TRUE)+
    ggrepel::geom_text_repel( # helps with legends to be more readable
      stat = "stratum", label.strata = TRUE, direction = "both", color = "black", size = baseSize + font_correction
    )+
    scale_x_discrete(
      limits = c(target_axis1, target_axis2)
      , expand = c(.05, .05)
      ,position = "top"
    ) +
    scale_y_continuous(label = scales::comma)+
    labs(
      y = "Number of clinicals events (initiated)"
    )+
  # add comsmetic layers
    theme_custom
# extract Legend
  # extract_legend <- function(g){
  #   tmp <- ggplot_gtable(ggplot_build(g))
  #   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  #   legend <- tmp$grobs[[leg]]
  #   return(legend)}
  # g_legend <-  extract_legend(g3)
  # g_legend <-  g3 %>% ggpubr::get_legend() %>% ggpubr::as_ggplot()

  # remove legend to print separately
  # g4 <- g3 + guides(fill = FALSE)

  # add the plot to the support object
  l_support[["plots"]][["alluvia2axes"]] <- g3 # `metric_scatter` is an example of label
  # l_support[["plots"]][["legend"]] <- g_legend # `metric_scatter` is an example of label

  l_support[["specs"]]["axis_1_name"] <- target_axis1
  l_support[["specs"]]["axis_2_name"] <- target_axis2
  l_support[["specs"]]["fill_name"] <- target_fill
  l_support[["specs"]]["metric_name"] <- target_metric
  return(l_support)
}
# how to use:
# l_support <- ls0$class_gender %>%
#   # dplyr::filter(event_year == 2014) %>% # optional filtering to scope application
#   # dplyr::filter(clinical_focus %in% c("MHSU","MHSU-Addictions")) %>%
#   prepare_data_cccs_alluvia(
#     target_metric = "n_patients"
#     ,target_groupers = c(
#       "service_group"
#       ,"service_location"
#       # ,"gender"
#       ,"location_class_code"
#     )
#   )
# l_support$dataframe <- l_support$dataframe %>%
#   dplyr::filter(service_group == "Staple") %>%
#   # dplyr::filter(!is.na(gender)) %>%
#   dplyr::mutate(
#     class_id = factor(location_class_code)
#   )
# # augment the support list object with a graph
# l_support <- l_support %>%
#   make_plot_cccs_alluvia(
#     target_metric = "n_patients"
#     ,target_axis1  = "service_group"
#     ,target_axis2  = "service_location"
#     ,target_fill   = "class_id"
#   )
#
# # subset for printing
# l_support$plots$alluvia2axes
# l_support$specs

# ---- define-print_plot-function -------------------------------------------

print_plot_alluvia_1 <- function(
  l_support
  ,path_output_folder = "./reports/cccs-alluvia/prints/cccs-alluvia-1/"
  ,plot_to_print = "alluvia2axes"
  ,prefex = NA
  ,suffix = NA
  ,...
){
  # move next line into function's argument list with the second graph design
  # plot_to_print = "alluvia2axes"
  # how a grouping (axis1) breaks into constituents (axis2)
  graph_name <- paste0(
    "(",l_support$specs["axis_1_name"],")-(",l_support$specs["axis_2_name"],")-(",l_support$specs["fill_name"],")"
  )
  l_support$plots[[plot_to_print]] <- l_support$plots[[plot_to_print]] #+
    # labs(
    #   # title = graph_name
    # )

  # add a label to distinguish a particular graph (last element in the file name)
  if(!is.na(suffix)){
    (path_save_plot <- paste0(path_output_folder,graph_name,"-",suffix))
  }else{
    (path_save_plot <- paste0(path_output_folder,graph_name))
  }
  if(!is.na(prefex)){
    (path_save_plot <- paste0(path_output_folder, prefex,"-",graph_name))
  }else{
    (path_save_plot <- paste0(path_output_folder,graph_name))
  }
  # browser()
  # print the graphical object using jpeg device
  jpeg(filename = paste0(path_save_plot, ".jpg"), ...)

  l_support$plots[[plot_to_print]] %>% print()
   # reach into the custom object we made for graphing
  dev.off() # close the device
}
# how to use
# l_support %>% print_plot_cccs_alluvia(
#   path_output_folder = "./reports/cccs-alluvia/prints/cccs-alluvia-1/"
#   ,plot_to_print = "alluvia2axes"
#   ,suffix = NA
#   ,width             = 279
#   ,height            = 216
#   ,units             = "mm"
#   ,quality           = 200 # percent
#   ,res               = 400 # dpi
# )

# Size           Width x Height (mm) Width x Height (in)  Aspect Ratio
# Half Letter      140 x 216           5.5 x  8.5          1: 1.55
# Letter           216 x 279           8.5 x 11.0          1: 1.29
# Legal            216 x 356           8.5 x 14.0          1: 1.65
# Junior Legal     127 x 203           5.0 x  8.0          1: 1.60
# Ledger/Tabloid   279 x 432          11.0 x 17.0          1: 1.55

# create a function that prints an alluvia plot with the label on top as a separate plot
print_plot_cccs_alluvia_1 <- function(
  l_support
  ,path_output_folder
  ,prefex = NA
  ,suffix = NA
  ,...
){
  # if folder does not exist yet, create it
  if(!dir.exists(path_output_folder)){
    dir.create(path_output_folder)
  }

  graph_name <- paste0(
    "(",l_support$specs["axis_1_name"],")-(",l_support$specs["axis_2_name"],")-(",l_support$specs["fill_name"],")-label"
  )

  # add a label to distinguish a particular graph (last element in the file name)
  if(!is.na(suffix)){
    (path_save_plot <- paste0(path_output_folder,graph_name,"-",suffix))
  }else{
    (path_save_plot <- paste0(path_output_folder,graph_name))
  }
  if(!is.na(prefex)){
    (path_save_plot <- paste0(path_output_folder, prefex,"-",graph_name))
  }else{
    (path_save_plot <- paste0(path_output_folder,graph_name))
  }
  # implemet plot corrects for the complex display

  # jpeg device open
  jpeg(
    filename = paste0(path_save_plot, ".jpg")
    ,...
  )
  l_support$plots$alluvia2axes %>% ggpubr::get_legend() %>% ggpubr::as_ggplot() %>% print()
  dev.off() # close the device
  # jpeg device close

}
