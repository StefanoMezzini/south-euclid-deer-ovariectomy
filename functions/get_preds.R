get_preds <- function(parameter, alpha) {
  m <- get(paste0('m_', parameter))
  
  pr <- predict(
    m, newdata = newd, type = 'link', se.fit = TRUE,
    # drop smooths that account for individual-level trends
    exclude = smooths(m)[grepl('animal_year', smooths(m))],
    # Smoothness uncertainty corrected covariance not available
    discrete = FALSE, unconditional = FALSE) %>%
    as.data.frame() %>%
    transmute(mu = inv_link(m)(fit),
              lwr = inv_link(m)(fit - se.fit * qnorm(1 - alpha / 2)),
              upr = inv_link(m)(fit + se.fit * qnorm(1 - alpha / 2)))
  
  colnames(pr) <- paste0(parameter, '_', colnames(pr))
  
  return(pr)
}
