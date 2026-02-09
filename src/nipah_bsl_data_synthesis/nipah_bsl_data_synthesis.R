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
library(tidybayes)
library(orderly2)
library(patchwork)

# *--------------------------------- Orderly ----------------------------------*
# Results presented in the Nipah manuscript used 10000 runs
orderly_parameters(pathogen = NULL, runs=0, actual_dataset=TRUE,
                   generate_diagnostics=FALSE)

orderly_dependency("db_cleaning", "latest(parameter:pathogen == this:pathogen)",
                   c("articles.csv", "outbreaks.csv", "models.csv", "params.csv"))

orderly_shared_resource("nipah_functions.R" = "nipah_functions.R",
                        "bsl_data_synthesis.R" = "bsl_data_synthesis.R")

artefacts <- c("nipah_incubation_period_bsl_model.pdf",
               "bsl_model_plot.RDS")

if (runs==0){
  orderly_shared_resource(
    "bsl_model_fits.RDS" = file.path("nipah","bsl_model_fits.RDS"))
}else{
  artefacts <- c(artefacts, "bsl_model_fits.RDS")
}

source("nipah_functions.R")
source("bsl_data_synthesis.R")

orderly_artefact(description="Nipah data synthesis figures",
                 artefacts)

# *------------------------------ Data curation -------------------------------*
articles   <- read_csv("articles.csv")
outbreaks  <- read_csv("outbreaks.csv")
models     <- read_csv("models.csv")
parameters <- read_csv("params.csv")

dfs <- curation(articles, outbreaks, models, parameters, plotting = TRUE)
parameters <- dfs$parameters

# *--------------------------------- BSL Hierarchical Model ----------------------------------*
# --------------------------
# Bayesian Synthetic Likelihood for Data Synthesis based on this paper
# https://www.tandfonline.com/doi/full/10.1080/10618600.2017.1302882?scroll=top&needAccess=true#d1e213

# --------------------------
# Incubation Period datasets
# --------------------------
# TODO: Get the source for this in a reproducible way
if(actual_dataset){
  datasets_with_refs <- list(
    d1 = list(median = 10.0, min = 9.0, max = 12.0, n = 4,
              access_param_id="113_003"),
    d2 = list(median =  4.0, min = 2.0, max =  7.0, n = 6,
              access_param_id="113_004"),
    d3 = list(median =  9.0, min = 6.0, max = 14.0, n = 11,
              access_param_id="138_020"),
    d4 = list(median =  9.5, min = 4.0, max = 14.0, n = 22,
              access_param_id="121_002"),
    d5 = list(median =  8.0, min = 3.0, max = 20.0, n = 15,
              access_param_id="081_001"),
    d6 = list(median =  9.0, min = 6.0, max = 11.0, n = 11,
              access_param_id="030_002"),
    d7 = list(median = 10.0, min = 8.0, max = 15.0, n = 14,
              access_param_id="151_001"),
    d8 = list(median =  9.0, min = 6.0, max = 11.0, n = 11,
              access_param_id="033_006")
  )

  datasets <- lapply(datasets_with_refs, function(x) x[!names(x) %in% "access_param_id"])
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
if("min" %in% names(datasets$d1)){
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

if(runs!=0){
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
        n = runs,
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

  saveRDS(fits_multi,'bsl_model_fits.RDS')

} else {
  fits_multi <- readRDS('bsl_model_fits.RDS')
}

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
# Commenting out for orderly run
# par(mfrow = c(3, 3))
# for (mdl in names(mcmc_lists)) {
#   for (p in 1:ncol(as.matrix(mcmc_lists[[mdl]][[1]]))) {
#     traceplot(mcmc_lists[[mdl]], varname = colnames(as.matrix(mcmc_lists[[mdl]][[1]]))[p],
#               main = paste("Trace:", mdl, "-", colnames(as.matrix(mcmc_lists[[mdl]][[1]]))[p]))
#   }
# }

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
# Makes a denisty summary by default
density_summary <- do.call(rbind, lapply(names(posterior_samples), function(nm)
  bsl_make_posterior_summary(nm, posterior_samples[[nm]],
                             x_seq=seq(0,30, length.out=400),
                             n_draws = 200, L = 20)
))

cdf_summary <- do.call(rbind, lapply(names(posterior_samples), function(nm)
  bsl_make_posterior_summary(nm, posterior_samples[[nm]],
                       x_seq=seq(0, 40, length.out=2000),
                       n_draws = 200, L = 50, posterior_cdf=TRUE)
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
  dplyr::select(model, median_value, low_value, high_value)

posterior_summaries_median_long <- posterior_summaries %>%
  mutate(
    median_value = median_mean,
    low_value    = median_low,
    high_value   = median_high
  ) %>%
  dplyr::select(model, median_value, low_value, high_value)

posterior_summaries_90th_long <- posterior_summaries %>%
  mutate(
    median_value = q90_mean,
    low_value    = q90_low,
    high_value   = q90_high
  ) %>%
  dplyr::select(model, median_value, low_value, high_value)

# --------------------------
# Observed summaries from datasets (extract summary-list values)
# --------------------------
observed_summary <- do.call(rbind, lapply(names(datasets_with_refs), function(nm) {
  d <- datasets_with_refs[[nm]]
  data.frame(
    dataset = nm,
    access_param_id = d$access_param_id,
    median_obs = ifelse(!is.null(d$median), d$median, NA_real_),
    min_obs    = ifelse(!is.null(d$min), d$min, NA_real_),
    max_obs    = ifelse(!is.null(d$max), d$max, NA_real_)
  )
}))
# Note: observed_summary$dataset are d1,d2,... (not model names). We'll map them visually below.
observed_summary <- observed_summary |>
  left_join(parameters[, c("access_param_id", "refs")])
# --------------------------
# Plot: density + predictive mean CI + observed medians/ranges
# --------------------------
# *=================================== pdf ====================================*
obs_plot_df <- observed_summary

# Observed summary row
obs_strip <- obs_plot_df %>%
  transmute(
    panel = "Summary statistics",
    model = "Summary statistics",
    y = "Summary statistics",
    x = median_obs,
    xmin = min_obs,
    xmax = max_obs
  )

# Posterior medians (with interval)
med_strip <- posterior_summaries_median_long %>%
  filter(model != "lognormal") %>%
  transmute(
    panel = "Median",
    model = model,
    y = model,
    x = median_value,
    xmin = low_value,
    xmax = high_value
  )

# Posterior 90th (point only; add xmin/xmax if you have them)
p90_strip <- posterior_summaries_90th_long %>%
  filter(model != "lognormal") %>%
  transmute(
    panel = "90th",
    model = model,
    y = model,
    x = median_value,
    xmin = NA_real_,
    xmax = NA_real_
  )

strip_df <- bind_rows(obs_strip, med_strip, p90_strip) %>%
  mutate(
    panel = factor(panel,
                   levels = c("Summary statistics", "Median", "90th"),
                   labels=c("Summary statistics", "Median",
                            "90th percentile")),
    y = row_number()
  )

# lanonc_colours <- ggsci::pal_lancet("lanonc")(9)

summary_strip_plot <-
  ggplot(strip_df, aes(x = x, y = y)) +
  # interval rows where available
  geom_errorbarh(
    aes(xmin = xmin, xmax = xmax, color = model),
    height = 0.15, linewidth = 0.5,
    na.rm = TRUE
  ) +
  # points (median / observed median / 90th point)
  geom_point(
    aes(color = model),
    size = 2
  ) +
  # facet_grid(panel ~ ., scales = "free_y", space = "free_y") +
  ggforce::facet_col(facets = vars(panel),
                     scales = "free_y",
                     space = "free") +
  labs(title = "", x = "Incubation period (days)", y = "", colour="Model") +
  guides(colour=guide_none())+
  theme_bw(base_size = 10) +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid   = element_blank(),
    plot.margin = margin(-14, 3, 0, -10),
    strip.background = element_blank(),
    strip.text = element_text(color = "black"),
  ) +
  scale_color_manual(
    values = c(
      "Summary statistics" = "black",
      gamma    = "#ED0000FF",
      weibull = "#00468BFF"
    )
  ) +
  scale_y_discrete(expand = expansion(add = 0.75))

bsl_model_plot <- ggplot(density_summary |> filter(model!="lognormal"),
                         aes(x = x, y = mean, color = model, fill = model)) +
  # Posterior densities
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1) +
  labs(
    title = "",
    colour="Model",
    fill="Model",
    x = "Incubation period (days)",
    y = "Density"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "right") +
  scale_color_lancet() +
  scale_fill_lancet()

bsl_model_plot <- bsl_model_plot + theme(text=element_text(size=28))

bsl_model_plot <- bsl_model_plot + theme(legend.position = "top") +
  inset_element(
    summary_strip_plot,
    left   = 0.65,
    bottom = 0.25,
    right  = 0.99,
    top    = 0.975
  )

saveRDS(bsl_model_plot, "bsl_pdf_model_plot.RDS")
ggsave('nipah_incubation_period_bsl_pdf_model.pdf', plot=bsl_model_plot, width = 11, height = 7)

# *=================================== cdf ====================================*
# *-------------------------------- Main text ---------------------------------*
obs_plot_df$y_jitter <- -0.3 + seq_len(nrow(obs_plot_df)) * 0.03

observation_plot <- ggplot()  +
  geom_errorbarh(
    data = obs_plot_df,
    aes(y = y_jitter - 0.01, xmin = min_obs, xmax = max_obs),
    height = 0.005, color = "black", size = 0.8,
    inherit.aes = FALSE, linetype="dashed"
  ) +
  geom_point(
    data = obs_plot_df,
    aes(x = median_obs, y = y_jitter - 0.01),
    shape = 22, fill="black", color = "black", size = 3,
    inherit.aes = FALSE
  ) +
  labs(
    title = "",
    x = "Incubation period (days)",
    y = ""
  ) +
  theme_bw(base_size = 14) +
  scale_y_continuous(
    breaks = obs_plot_df$y_jitter,
    labels = obs_plot_df$refs
  ) +
  xlim(c(0,30))

cdf_quantiles <- cdf_summary |>
  group_by(model) |>
  arrange(x, .by_group = TRUE) |>
  summarise(
    x_50      = approx(mean, x, xout = 0.5, ties = base::mean, rule = 2)$y,
    x_90      = approx(mean, x, xout = 0.9, ties = base::mean, rule = 2)$y,
    q_high_50 = approx(low,  x, xout = 0.5, ties = base::mean, rule = 2)$y,
    q_high_90 = approx(low,  x, xout = 0.9, ties = base::mean, rule = 2)$y,
    q_low_50 = approx(high, x, xout = 0.5, ties = base::mean, rule = 2)$y,
    q_low_90 = approx(high, x, xout = 0.9, ties = base::mean, rule = 2)$y,
    .groups = "drop"
  )

cdf_quantiles_long <- cdf_quantiles |>
  pivot_longer(
    cols = -model,
    names_to = "quantile",
    values_to = "x_value"
  ) |>
  mutate(
    y = as.numeric(str_extract(quantile, "(?<=_)\\d+")) / 100
  )

cdf_quantiles_wide <- cdf_quantiles_long |>
  mutate(level = sub(".*_(\\d+)$", "\\1", quantile)) |>
  mutate(type  = sub("_(\\d+)$", "", quantile)) |>
  dplyr::select(-quantile) |>
  pivot_wider(
    names_from  = type,
    values_from = x_value)

median_labels <- cdf_quantiles_wide |>
  filter(level==50) |>
  mutate(q_low=q_low-2.75)

nineth_labels <-  cdf_quantiles_wide |>
  filter(level==90) |>
  mutate(q_low=q_low-4)

bsl_cdf_model_plot <- ggplot(cdf_summary |>
                               filter(model=="weibull"),
                             aes(x = x, y = mean, color = model,
                                 fill = model)) +
  # Posterior densities
  geom_ribbon(aes(ymin = low, ymax = high),
              alpha = 0.15, colour = NA) +
  geom_line(size = 1) +
  geom_errorbarh(
    data=cdf_quantiles_wide |> filter(model=="weibull"),
    aes(xmin = q_low, xmax = q_high, y=y),
    height = 0.035) +
  geom_point(data=cdf_quantiles_wide |>
               filter(model=="weibull"),
                 aes(x = x, y=y), size = 2) +
  # Vertical lines
  # geom_segment(data = cdf_quantiles_long |> filter(model=="weibull"),
  #              aes(x = 0, xend = x_value,
  #                  y = y, yend = y),
  #              color = "black",
  #              alpha=0.6,
  #              linetype = "solid",
  #              linewidth = 0.6
  # ) +
  # geom_segment(
  #   data = cdf_quantiles_long |> filter(model=="weibull"),
  #   aes(
  #     x = x_value, xend = x_value,
  #     y = 0, yend = 0,
  #   ),
  #   color = "black",
  #   alpha=0.6,
  #   linetype = "solid",
  #   linewidth = 0.6
  # ) +
  # geom_text(
  #   data = data.frame(x = 7.5, y = 0.905),
  #   aes(x = x, y = y),
  #   label = expression(90^th~percentile),
  #   color = "black",
  #   size = 5,
  #   inherit.aes = FALSE
  # ) +
  # geom_text(
  #   data = data.frame(x = 3.5, y = 0.505, label = "Median"),
  #   aes(x = x, y = y, label = label),
  #   color = "black",
  #   size = 5,
  #   inherit.aes = FALSE
  # ) +
  geom_text(
    data = nineth_labels |> filter(model=="weibull"),
    aes(x = q_low, y = y+0.01, label = label),
    label = expression(90^th~percentile),
    color = "black",
    size = 8,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = median_labels |> filter(model=="weibull"),
    aes(x = q_low, y = y+0.01, label = label),
    label = "Median",
    color = "black",
    size = 8,
    inherit.aes = FALSE
  ) +
  scale_color_lancet() +
  scale_fill_lancet() +
  labs(
    title = "",
    x = "Incubation period (days)",
    y = "Probability",
    colour="Model",
    fill="Model"
  ) +
  xlim(c(0, 30)) +
  theme_bw(base_size = 28) +
  theme(legend.position = "right") +
  guides(color=guide_none(), fill=guide_none())

bsl_main_cdf_plot <- observation_plot / free(bsl_cdf_model_plot) +
  plot_layout(heights=c(1,2)) &
  theme(plot.margin = margin(0, 2, 0, 2))

saveRDS(bsl_main_cdf_plot, "bsl_main_cdf_plot.RDS")
ggsave('bsl_main_cdf_plot.pdf', plot=bsl_main_cdf_plot, width = 8, height = 6)

bsl_cdf_model_apx_plot <- ggplot(cdf_summary,
                             aes(x = x, y = mean, color = model,
                                 fill = model)) +
  # Posterior densities
  geom_ribbon(aes(ymin = low, ymax = high),
              alpha = 0.15, colour = NA) +
  geom_line(size = 1) +
  geom_errorbarh(
    data=cdf_quantiles_wide,
    aes(xmin = q_low, xmax = q_high, y=y),
    height = 0.035
  ) +
  geom_point(data=cdf_quantiles_wide,
             aes(x = x, y=y), size = 2) +
  geom_text(
    data = nineth_labels,
    aes(x = q_low-2, y = y+0.01, label = label, group=model),
    label = expression(90^th~percentile),
    color = "black",
    size = 5.5,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = median_labels,
    aes(x = q_low-1, y = y+0.01, label = label, group=model),
    label = "Median",
    color = "black",
    size = 5.5,
    inherit.aes = FALSE
  ) +
  scale_color_lancet() +
  scale_fill_lancet() +
  labs(
    title = "",
    x = "Incubation period (days)",
    y = "Probability",
    colour="Model",
    fill="Model"
  ) +
  facet_wrap(~model, ncol=1, labeller = labeller(model = tools::toTitleCase)) +
  xlim(c(0, 40)) +
  theme(legend.position = "right") +
  theme_minimal(base_size = 20) +
  theme(panel.border = element_rect(color = "black", linewidth = 1.25, fill = NA)) +
  guides(color=guide_none(), fill=guide_none()) +
  scale_color_manual(
    values = c(
      gamma    = "#42B540FF",
      weibull = "#00468BFF",
      lognormal="#ED0000FF")) +
  scale_fill_manual(
    values = c(
      gamma    = "#42B540FF",
      weibull = "#00468BFF",
      lognormal="#ED0000FF"))

bsl_apx_plot <- (observation_plot +
  xlim(c(0,40))) / free(bsl_cdf_model_apx_plot) +
  plot_layout(heights=c(1,3.75)) + plot_annotation(tag_levels="A") &
  theme(plot.margin = margin(0, 2, 0, 2),
        text=element_text(size=22))

saveRDS(bsl_apx_plot, "bsl_apx_cdf_plot.RDS")
ggsave('bsl_apx_cdf_plot.pdf', plot=bsl_apx_plot, width = 7, height = 12)

# ===============================
# AUTOMATED BSL DIAGNOSTIC REPORT
# ===============================
if (generate_diagnostics){
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

param_summaries <- bsl_summarise_parameters(posterior_samples)

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

  ip_same_data <- metamean_wrap(dataframe = params_in, estmeansd_method = "Cai",
                                plot_study = TRUE, digits = 2, lims = c(2,15), colour = "dodgerblue3", label = "Mean Onset-Admission Delay (days)",
                                width = 9500, height = 4200, resolution = 1000)


  params_ip_all <- as_tibble(parameters) %>% filter(parameter_type == 'Human delay - incubation period' &
                                                      !is.na(population_sample_size) & !is.na(parameter_value) ) %>%
    mutate(parameter_uncertainty_type = replace_na(parameter_uncertainty_type, 'range'),
           parameter_uncertainty_lower_value = coalesce(parameter_uncertainty_lower_value, parameter_2_lower_bound),
           parameter_uncertainty_upper_value = coalesce(parameter_uncertainty_upper_value, parameter_2_upper_bound)
    )

  ip_all_data <- metamean_wrap(dataframe = params_ip_all, estmeansd_method = "Cai",
                               plot_study = TRUE, digits = 2, lims = c(2,15), colour = "darkorange2", label = "Median Incubation Period (days)",
                               width = 9500, height = 4200, resolution = 1000)

  ggsave("nipah_ip_meta_analysis_same_data.pdf", ip_same_data$plot, width = 12, height = 6 )
  ggsave("nipah_ip_meta_analysis_all_data.pdf", ip_all_data$plot, width = 12, height = 6 )
}
