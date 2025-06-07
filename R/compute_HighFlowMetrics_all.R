
#' High flow metric (enhanced version)
#'
#' Compute percent error and correlation between observation and model 
#' for high flow metrics, with scoring based on normalized errors 
#' and optional weighting.
#'
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year
#' @param  wy water year
#' @param  high_flow_months which to use (default = February = 2)
#' @param  wts (vector of 4 weights: annual_max_err, annual_max_cor, 
#' high_month_cor, high_month_err)
#' @param  max_err_annual_max Optional max error used to normalize 
#' annual max error (default = 50% of mean observed max)
#' @param  max_err_high_month Optional max error used to normalize 
#' high month error (default = 50% of mean observed month)
#'
#' @return A list: annual_max_err, annual_max_cor, high_month_err, 
#' high_month_cor, combined

compute_HighFlowMetrics_all <- function(m, o, month, day, year, wy,
                                        high_flow_months = 2,
                                        max_err_annual_max = NULL,
                                        max_err_high_month = NULL,
                                        wts = c(0.25, 0.25, 0.25, 0.25)) {
  
  flow <- cbind.data.frame(m, o, month, day, year, wy)
  
  # Get maximum yearly values
  tmp <- flow %>%
    dplyr::group_by(wy) %>%
    dplyr::summarize(maxo = max(o), maxm = max(m), .groups = "drop")
  
  annual_max_err <- mean(tmp$maxm - tmp$maxo)
  
  annual_max_cor <- cor(tmp$maxm, tmp$maxo)
  
  # Use default max error if not supplied
  if (is.null(max_err_annual_max)) {
    max_err_annual_max <- 0.5 * mean(tmp$maxo)
  }
  
  # Get monthly summed values
  tmp <- flow %>%
    dplyr::group_by(month, year) %>%
    dplyr::summarize(model = sum(m), obs = sum(o), .groups = "drop")
  
  # Extract high flow months
  high <- subset(tmp, month %in% high_flow_months)
  high_month_err <- mean(high$model - high$obs)
  high_month_cor <- cor(high$model, high$obs)
  
  # Use default max error if not supplied
  if (is.null(max_err_high_month)) {
    max_err_high_month <- 0.5 * mean(high$obs)
  }
  
  # Normalize errors to 0–1 scale
  annual_max_err_trans <- max(0, 1 - abs(annual_max_err / max_err_annual_max))
  high_month_err_trans <- max(0, 1 - abs(high_month_err / max_err_high_month))
  
  # Normalize weights to sum to 1
  wts <- wts / sum(wts)
  
  # Combined score (higher is better)
  combined <- wts[1] * annual_max_err_trans + 
    wts[2] * annual_max_cor + 
    wts[3] * high_month_cor + 
    wts[4] * high_month_err_trans
  
  return(list(
    annual_max_err = annual_max_err,
    annual_max_cor = annual_max_cor,
    high_month_err = high_month_err,
    high_month_cor = high_month_cor,
    combined = combined
  ))
}
