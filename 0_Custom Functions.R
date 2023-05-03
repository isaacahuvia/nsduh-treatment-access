## Make function to get summary table from svyglm
custom_summary <- function(model, round = T) {
  
  # Get summary of model
  summary <- summary(model)
  
  # Save just the coefficients
  coefficients <- as_tibble(summary$coefficients,
                            rownames = "coef")
  
  # Add in the 95% confidence interval
  coefficients_with_confint <- bind_cols(coefficients, confint(model))
  
  output <- coefficients_with_confint %>%
    # Save just the variables we want
    select(coef,
           "est" = "Estimate",
           "low" = "2.5 %",
           "high" = "97.5 %",
           "p" = "Pr(>|t|)") %>%
    # Switch log odds to odds ratios
    mutate(across(.cols = c(est, low, high), .fns = exp)) %>%
    # Adjust p-values and display significance
    mutate(padj = round(p.adjust(p, method = "BH"), 3),
           sig = case_when(
             padj < .001 ~ "***",
             padj < .01 ~ "**",
             padj < .05 ~ "*",
             padj < .1 ~ ".",
             T ~ ""
           )) %>%
    # Round values for display
    mutate(across(.cols = c(est, low, high), ~ round(., 2)),
           p = round(p, 3))
  
  # Return the output
  return(output)
  
}