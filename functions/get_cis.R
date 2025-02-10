library('mgcv')

get_cis <- function(m) {
  b <- summary(m, re.test = FALSE)$p.table['groupOvariectomy', 'Estimate']
  se <- summary(m, re.test = FALSE)$p.table['groupOvariectomy', 'Std. Error']
  p <- summary(m, re.test = FALSE)$p.table['groupOvariectomy', 'Pr(>|t|)']
  
  l_inv <- m$family$linkinv
  
  # beta estiamates are centered at 0.5 = 50% by default
  if(grepl('Beta', m$family$family))  {
    cat('Estimate: ', round(l_inv(b), 2) * 100 - 100 + 50,
        '%\n95% CI: (', round(l_inv(b - se * 1.96), 2) * 100 - 100 + 50,
        '%, ', round(l_inv(b + se * 1.96), 2) * 100 - 100 + 50,
        '%)\np-value: ', p, '\n', sep = '')
  } else {
    cat('Estimate: ', round(l_inv(b), 2) * 100 - 100,
        '%\n95% CI: (', round(l_inv(b - se * 1.96), 2) * 100 - 100,
        '%, ', round(l_inv(b + se * 1.96), 2) * 100 - 100,
        '%)\np-value: ', p, '\n', sep = '')
  }
}
