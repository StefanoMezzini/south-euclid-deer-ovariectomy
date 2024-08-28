library('ggplot2') # for fancy figures

theme_set(theme_bw() +
            theme(panel.grid = element_blank(),
                  text = element_text(face = 'bold')))

# PAL <- c(control = '#CC3311', ovariectomy = '#009988')
PAL <- c(Control = '#74A6DA', Ovariectomy = '#EF6756')
