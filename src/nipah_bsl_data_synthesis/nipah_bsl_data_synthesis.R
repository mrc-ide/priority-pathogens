library(BSL)
library(parallel)
library(doParallel)
library(MASS)
library(foreach)
library(ggplot2)
library(tidyverse)
library(ggsci)
library(coda)
library(bridgesampling)

# *--------------------------------- Orderly ----------------------------------*
orderly_parameters(pathogen = NULL)

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "outbreaks.csv", "models.csv", "params.csv"))

orderly_shared_resource("nipah_functions.R" = "nipah_functions.R")
source("nipah_functions.R")

orderly_artefact(description="Nipah data synthesis figures",
                 c())

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)
parameters <- dfs$parameters

# *--------------------------------- BSL Hierarchical Model ----------------------------------*

TESTING <- TRUE

# --------------------------
# Bayesian Synthetic Likelihood for Data Synthesis based on this paper
# https://www.tandfonline.com/doi/full/10.1080/10618600.2017.1302882?scroll=top&needAccess=true#d1e213
# --------------------------

orderly_strict_mode()
orderly_parameters(pathogen = NULL)
orderly_dependency()
orderly_shared_resource("nipah_functions.R" = "nipah_functions.R",
                        "bsl_data_synthesis.R" = "bsl_data_synthesis.R")
source("nipah_functions.R")
source("bsl_data_synthesis.R")

orderly_artefact()


# --------------------------
# Incubation Period datasets
# --------------------------
if(TRUE){
  datasets <- list(
    d1 = list(median = 10.0, min = 9.0, max = 12.0, n = 4),
    d2 = list(median =  4.0, min = 2.0, max =  7.0, n = 6),
    d3 = list(median =  9.0, min = 6.0, max = 14.0, n = 11),
    d4 = list(median =  9.5, min = 4.0, max = 14.0, n = 22),
    d5 = list(median =  8.0, min = 3.0, max = 20.0, n = 15),
    d6 = list(median =  9.0, min = 6.0, max = 11.0, n = 11),
    d7 = list(median = 10.0, min = 8.0, max = 15.0, n = 14),
    d8 = list(median =  9.0, min = 6.0, max = 11.0, n = 11)
  )
} else {    #testing only
  datasets <- list(
    d1 = list(mean = 5.2, sd = 2.1, median = 5.0, q25 = 4.3, q75 = 9.0, n = 100),
    d2 = list(mean = 6.0, sd = 2.8, median = 6.0, q25 = 3.0, q75 = 11.0, n = 12),
    d3 = list(mean = 7.1, sd = 1.9, median = 7.0, q25 = 6.5, q75 = 10.0, n = 40)
  )
}

if( "min" %in% names(datasets[[1]]) ) {
  summary_names_used <- c("median", "min", "max")
} else {
  summary_names_used <- c("median", "q25", "q75")
}

# --------------------------
# Flatten observed summaries
# --------------------------
S_obs <- unlist(lapply(datasets, function(d) {
  s <- numeric(length(summary_names_used))
  names(s) <- summary_names_used
  s[names(d)] <- unlist(d[names(d)])
  s
}))
stopifnot(all(is.finite(S_obs)))

# --------------------------
# Safe summary function
# --------------------------
if("min" %in% names(datasets$d1))
{
  summaries_fun <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 1) x <- 1e-6

    s_median <- median(x)
    s_min    <- min(x)
    s_max    <- max(x)

    s <- c(median = s_median, min = s_min, max = s_max)
    s[!is.finite(s)] <- 1e-6
    s <- s + runif(length(s), -1e-6, 1e-6)  # tiny noise to avoid singular covariance
    s
  }
} else {
  summaries_fun <- function(x) {
    summary_names_used <- c("median", "q25", "q75")
    #summary_names_used <- c("mean", "sd", "median", "q25", "q75")
    x <- x[is.finite(x)]
    if (length(x) < 2) x <- c(x, 1e-6)

    s_mean <- mean(x)
    s_sd   <- max(sd(x), 1e-6)
    q <- quantile(x, probs = c(0.25, 0.5, 0.75), names = FALSE, type = 1)

    s <- c(mean = s_mean, sd = s_sd, median = q[2], q25 = q[1], q75 = q[3])
    s[!is.finite(s)] <- 1e-6
    s <- s + runif(length(s), -1e-6, 1e-6)
    s[summary_names_used]
  }
}

# --------------------------
# Initial guesses
# --------------------------
theta0_lognorm <- c(log(5), log(0.5), log(0.5))
theta0_gamma   <- c(log(4), log(0.5), log(2.0))
theta0_weibull <- c(log(6), log(0.5), log(1.0))

model_list <- list(
  lognormal = bsl_create_model("lognormal", theta0_lognorm),
  gamma     = bsl_create_model("gamma", theta0_gamma),
  weibull   = bsl_create_model("weibull", theta0_weibull)
)

if( interactive() )
{
  # --------------------------
  # Parallel setup
  # --------------------------
  num_cores <- max(1, detectCores() - 1)
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  clusterExport(cl, varlist = c("bsl_make_simulator_matrix", "bsl_simulate_dataset",
                                "summaries_fun", "summary_names_used",
                                "datasets", "bsl_fnPrior_hier"))


  # --------------------------
  # Run BSL in parallel
  # --------------------------
  set.seed(2025)

  chains_per_model <- 4

  fits_multi <- foreach(
    mdl = names(model_list),
    .export = c("S_obs", "model_list", "summaries_fun",
                "bsl_fnPrior_hier", "bsl_simulate_dataset",
                "bsl_make_simulator_matrix"),
    .packages = c("BSL", "MASS"),
    .noexport = c("S_obs", "model_list")
  ) %dopar% {
    set.seed(2025 + which(names(model_list) == mdl))  # ensure different chains per model

    replicate(chains_per_model, {
      bsl(
        S_obs,
        n = if(TESTING) 1000 else 10000,
        M = 2000,
        model = model_list[[mdl]],
        covRandWalk = diag(0.125, 3),
        verbose = FALSE
      )
    }, simplify = FALSE)
  }

  # Assign model names
  names(fits_multi) <- names(model_list)
  stopCluster(cl)


  # --- Check structure
  str(fits_multi, 1)

  # --- Extract posterior samples per model (combine chains)
  posterior_samples <- lapply(fits_multi, function(chain_list) {
    do.call(rbind, lapply(chain_list, function(f) as.data.frame(f@theta)))
  })

  mcmc_lists <- bsl_to_mcmc_list_multi(fits_multi)

  # --- Gelman-Rubin (R-hat) diagnostics per model
  gelman_results <- lapply(mcmc_lists, function(ml) gelman.diag(ml, autoburnin = FALSE))
  print(gelman_results)

  # --- Effective sample size
  ess_results <- lapply(mcmc_lists, effectiveSize)
  print(ess_results)

  # --- Trace plots for visual check
  par(mfrow = c(3, 3))
  for (mdl in names(mcmc_lists)) {
    for (p in 1:ncol(as.matrix(mcmc_lists[[mdl]][[1]]))) {
      traceplot(mcmc_lists[[mdl]], varname = colnames(as.matrix(mcmc_lists[[mdl]][[1]]))[p],
                main = paste("Trace:", mdl, "-", colnames(as.matrix(mcmc_lists[[mdl]][[1]]))[p]))
    }
  }

  # --------------------------
  # Inspect results
  # --------------------------

  log_evidences <- sapply(names(fits_multi), function(mdl) {
    chain_evids <- sapply(fits_multi[[mdl]], function(f) bsl_bridge_estimate(f@loglike))
    mean(chain_evids)  # average across chains
  })

  # --- Relative Bayes factors
  delta <- log_evidences - max(log_evidences)
  bayes_factors <- exp(delta)
  model_comparison <- data.frame(
    model = names(fits_multi),
    log_evidence = log_evidences,
    rel_prob = bayes_factors / sum(bayes_factors)
  )
  print(model_comparison)

  # --- Parallel Convergence summary
  conv_summary <- lapply(names(mcmc_lists), function(m) {
    g <- gelman_results[[m]]$psrf
    e <- ess_results[[m]]
    data.frame(
      model = m,
      param = names(e),
      Rhat = g[, 1],
      ESS = e
    )
  }) %>% bind_rows() %>% remove_rownames()

  print(conv_summary)

  # Example usage (uses your posterior_samples variable from earlier)
  set.seed(2025)
  posterior_summaries <- bsl_summarise_posteriors(posterior_samples, L = 50)
  print(posterior_summaries)



  # Combine for all models
  density_summary <- do.call(rbind, lapply(names(posterior_samples), function(nm)
    bsl_make_density_summary(nm, posterior_samples[[nm]], n_draws = 200, L = 20)
  ))

  # --------------------------
  # Posterior summaries for plotting (use predictive mean & CI)
  # --------------------------
  posterior_summaries_long <- posterior_summaries %>%
    mutate(
      median_value = mean_mean,
      low_value    = mean_low,
      high_value   = mean_high
    ) %>%
    select(model, median_value, low_value, high_value)

  posterior_summaries_median_long <- posterior_summaries %>%
    mutate(
      median_value = median_mean,
      low_value    = median_low,
      high_value   = median_high
    ) %>%
    select(model, median_value, low_value, high_value)

  posterior_summaries_90th_long <- posterior_summaries %>%
    mutate(
      median_value = q90_mean,
      low_value    = q90_low,
      high_value   = q90_high
    ) %>%
    select(model, median_value, low_value, high_value)

  # --------------------------
  # Observed summaries from datasets (extract summary-list values)
  # --------------------------
  observed_summary <- do.call(rbind, lapply(names(datasets), function(nm) {
    d <- datasets[[nm]]
    data.frame(
      dataset = nm,
      median_obs = ifelse(!is.null(d$median), d$median, NA_real_),
      min_obs    = ifelse(!is.null(d$min), d$min, NA_real_),
      max_obs    = ifelse(!is.null(d$max), d$max, NA_real_)
    )
  }))
  # Note: observed_summary$dataset are d1,d2,... (not model names). We'll map them visually below.

  # --------------------------
  # Plot: density + predictive mean CI + observed medians/ranges
  # --------------------------
  obs_plot_df <- observed_summary

  posterior_summaries_long$y_jitter <- jitter(rep(0.02, nrow(posterior_summaries_long)), amount = 0.02)
  posterior_summaries_median_long$y_jitter <- jitter(rep(0.02, nrow(posterior_summaries_median_long)), amount = 0.02)
  posterior_summaries_90th_long$y_jitter <- jitter(rep(0.02, nrow(posterior_summaries_90th_long)), amount = 0.02)
  obs_plot_df$y_jitter <- jitter(rep(0, nrow(obs_plot_df)), amount = 0.015)

  ggplot(density_summary, aes(x = x, y = mean, color = model, fill = model)) +
    # Posterior densities
    geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.15, colour = NA) +
    geom_line(size = 1) +

    # Posterior medians + 95% CrIs (as points and horizontal bars above the curves)
    geom_errorbarh(
      data = posterior_summaries_median_long,
      aes(y = y_jitter + 0.15, xmin = low_value, xmax = high_value, color = model),
      height = 0.005, size = 0.8, inherit.aes = FALSE
    ) +
    geom_point(
      data = posterior_summaries_median_long,
      aes(x = median_value, y = y_jitter + 0.15, fill = model, color = model),
      shape = 8, size = 2, inherit.aes = FALSE
    ) +

    geom_point(
      data = posterior_summaries_90th_long,
      aes(x = median_value, y = y_jitter + 0.11, fill = model, color = model),
      shape = 4, size = 2, inherit.aes = FALSE
    ) +

    # Observed data medians + ranges (at bottom, in black)
    geom_errorbarh(
      data = obs_plot_df,
      aes(y = y_jitter - 0.01, xmin = min_obs, xmax = max_obs),
      height = 0.005, color = "black", size = 0.8,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = obs_plot_df,
      aes(x = median_obs, y = y_jitter - 0.01),
      shape = 18, fill = "white", color = "black", size = 2,
      inherit.aes = FALSE
    ) +

    geom_text(
      data = data.frame(x = 19, y = 0.135, label = "90th\npercentiles"),
      aes(x = x, y = y, label = label),
      color = "black",
      size = 3.5,
      inherit.aes = FALSE
    ) +

    geom_text(
      data = data.frame(x = 10, y = 0.19, label = "Medians"),
      aes(x = x, y = y, label = label),
      color = "black",
      size = 3.5,
      inherit.aes = FALSE
    ) +

    scale_color_lancet() +
    scale_fill_lancet() +
    labs(
      title = "Posterior Predictive Densities with Medians\n and 95% Credible Bands",
      x = "Days",
      y = "Density"
    ) +
    theme_bw(base_size = 14) +
    theme(legend.position = "right")


  # ===============================
  # AUTOMATED BSL DIAGNOSTIC REPORT
  # ===============================

  diagnostic_dir <- "bsl_diagnostics"
  if (!dir.exists(diagnostic_dir)) dir.create(diagnostic_dir)

  diag_results <- bsl_run_diagnostics(fits_multi)
  trace_df     <- bsl_make_trace_df(fits_multi)

  trace_long   <- trace_df %>%
    pivot_longer(cols = starts_with("theta"),
                 names_to = "parameter", values_to = "value")

  ggplot(trace_long, aes(x = iter, y = value, color = chain)) +
    geom_line(alpha = 0.7, linewidth = 0.5) +
    facet_grid(model ~ parameter, scales = "free_y") +
    labs(
      title = "Traceplots for BSL posterior samples",
      x = "Iteration",
      y = "Parameter value",
      color = "Chain"
    ) +
    scale_color_lancet() +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(fill = "grey90", color = "grey60"),
      panel.spacing = unit(0.8, "lines")
    )

  param_summaries <- summarise_parameters(posterior_samples)

  # Print to console
  print(param_summaries)

  # Save csv to diagnostics folder
  write.csv(param_summaries, file = file.path(diagnostic_dir, "parameter_summaries_per_model.csv"), row.names = FALSE)

  # Optional: pretty table print using knitr::kable if running in RMarkdown / Notebook
  if (interactive() && requireNamespace("knitr", quietly = TRUE)) {
    message("\nParameter summaries (95% CrI):\n")
    print(knitr::kable(param_summaries[, c("model", "parameter", "mean", "median", "ci_95")],
                       digits = 3, caption = "Posterior summaries for hierarchical parameters"))
  }


  # This will plot median and 95% CrI for each parameter grouped by model.
  param_plot_df <- param_summaries %>%
    mutate(parameter = factor(parameter, levels = c("mu0", "tau", "phi")))

  ggplot(param_plot_df, aes(x = parameter, y = median, ymin = low_2.5, ymax = high_97.5, color = model)) +
    geom_pointrange(position = position_dodge(width = 0.6), size = 0.7) +
    geom_errorbar(position = position_dodge(width = 0.6), width = 0.2, size = 0.5) +
    facet_wrap(~ model, nrow = 1, scales = "free_y") +
    labs(title = "Posterior parameter medians and 95% credible intervals",
         y = "Parameter value (tau & phi shown on natural scale)",
         x = "Parameter") +
    theme_bw(base_size = 13) +
    theme(legend.position = "none")

  # Save the plot
  ggsave(file.path(diagnostic_dir, "parameter_forestplot.png"), width = 10, height = 3.5)
} else {
  # NEED SOME PRE-CANNED RESULTS FOR NON-INTERACTIVE RUNS TO KEEP ORDERLY HAPPY
}

COMPARITIVE_ANALYSIS <- FALSE

if(COMPARITIVE_ANALYSIS)
{
  # Comparison to meta-analysis ---------------------------------------------
  datasets <- list(
    d1 = list(median = 10.0, min = 9.0, max = 12.0, n = 4),
    d2 = list(median =  4.0, min = 2.0, max =  7.0, n = 6),
    d3 = list(median =  9.0, min = 6.0, max = 14.0, n = 11),
    d4 = list(median =  9.5, min = 4.0, max = 14.0, n = 22),
    d5 = list(median =  8.0, min = 3.0, max = 20.0, n = 15),
    d6 = list(median =  9.0, min = 6.0, max = 11.0, n = 11),
    d7 = list(median = 10.0, min = 8.0, max = 15.0, n = 14),
    d8 = list(median =  9.0, min = 6.0, max = 11.0, n = 11)
  )

  params_in <- as_tibble(parameters) %>% filter(parameter_type == 'Human delay - incubation period' &
                                                  parameter_value_type == 'Median' &
                                                  !is.na(population_sample_size) &
                                                  population_sample_size < 23 )

  params_in %>% dplyr::select(covidence_id, parameter_value, population_sample_size, parameter_2_lower_bound, parameter_2_upper_bound)

  params_in$parameter_uncertainty_type <- 'range'
  params_in$parameter_uncertainty_lower_value <- params_in$parameter_2_lower_bound
  params_in$parameter_uncertainty_upper_value <- params_in$parameter_2_upper_bound

  d_in <- bind_rows(datasets, .id = "dataset")
  d_in$parameter_type <- 'median'
  d_in <- d_in %>% rename(parameter_value=median)

  m2 <- metamean_wrap(dataframe = params_in, estmeansd_method = "Cai",
                      plot_study = TRUE, digits = 2, lims = c(0,10), colour = "dodgerblue3", label = "Mean Onset-Admission Delay (days)",
                      width = 9500, height = 4200, resolution = 1000)


}
