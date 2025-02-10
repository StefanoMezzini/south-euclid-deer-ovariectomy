get_preds <- function(parameter) {
  m <- get(paste0('m_', parameter))
  
  pr <- predict(m, newdata = newd, type = 'link', se.fit = TRUE,
                exclude = c('s(doy_cr,animal_year):groupControl',
                            's(doy_cr,animal_year):groupOvariectomy',
                            's(doy_cr,animal_year)'),
                # Smoothness uncertainty corrected covariance not available
                discrete = FALSE, unconditional = FALSE) %>%
    as.data.frame() %>%
    transmute(mu = inv_link(m)(fit),
              lwr_50 = inv_link(m)(fit - se.fit * 0.67),
              upr_50 = inv_link(m)(fit + se.fit * 0.67),
              lwr_95 = inv_link(m)(fit - se.fit * 1.96),
              upr_95 = inv_link(m)(fit + se.fit * 1.96))
  
  colnames(pr) <- paste0(parameter, '_', colnames(pr))
  
  return(pr)
}
