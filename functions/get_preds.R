get_preds <- function(parameter) {
  m <- get(paste0('m_', parameter))
  
  pr <- predict(
    m, newdata = newd, type = 'link', se.fit = TRUE,
    # drop smooths that account for individual-level trends
    exclude = smooths(m)[grepl('animal_year', smooths(m))],
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
