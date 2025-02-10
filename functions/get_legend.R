library('cowplot')

#' `coplot::get_legend()` fails (v. 1.1.3.9000)
get_legend <- function(.plot) {
  get_plot_component(.plot + theme(legend.position = 'top'),
                     pattern = 'guide-box-top', return_all = TRUE)
}