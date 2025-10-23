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

# --------------------------
# Bayesian Synthetic Likelihood for Data Synthesis based on this paper
# https://www.tandfonline.com/doi/full/10.1080/10618600.2017.1302882?scroll=top&needAccess=true#d1e213
# --------------------------


# --------------------------
# Dataset simulator
# --------------------------
bsl_simulate_dataset <- function(dist, loc, phi, n) {
  x <- switch(dist,
              lognormal = rlnorm(n, meanlog = loc, sdlog = phi),
              gamma     = { mean_d <- exp(loc); shape <- phi; rate <- shape / mean_d; rgamma(n, shape = shape, rate = rate) },
              weibull   = { scale <- exp(loc); shape <- phi; rweibull(n, shape = shape, scale = scale) },
              stop("Unknown dist"))
  x[!is.finite(x)] <- 1e-6
  x
}

# --------------------------
# Simulator returning matrix
# --------------------------
bsl_make_simulator_matrix <- function(dist, datasets, summaries_fun, summary_names) {
  function(theta, M = 1) {
    mu0 <- theta[1]; tau <- exp(theta[2]); phi <- exp(theta[3])

    sims <- matrix(NA, nrow = M, ncol = length(summary_names) * length(datasets))
    colnames(sims) <- rep(summary_names, length(datasets))

    for (m in 1:M) {
      out_list <- lapply(datasets, function(d) {
        n <- d$n
        loc_d <- rnorm(1, mu0, tau)
        x <- bsl_simulate_dataset(dist, loc_d, phi, n)
        s <- summaries_fun(x)
        s_vec <- numeric(length(summary_names))
        names(s_vec) <- summary_names
        s_vec[names(s)] <- s
        s_vec
      })
      sims[m, ] <- unlist(out_list)
    }

    sims[!is.finite(sims)] <- 1e-6
    sims
  }
}

# --------------------------
# Prior function
# --------------------------
bsl_fnPrior_hier <- function(theta) {
  mu0 <- theta[1]; log_tau <- theta[2]; log_phi <- theta[3]
  dnorm(mu0, 0, 5, log = TRUE) +
    dnorm(log_tau, log(0.5), 1.5, log = TRUE) +
    dnorm(log_phi, 0, 1.5, log = TRUE)
}


# --------------------------
# Create MODEL objects
# --------------------------
bsl_create_model <- function(dist, theta0) {
  newModel(
    fnSim = function(theta, M = 1) bsl_make_simulator_matrix(dist, datasets, summaries_fun, summary_names_used)(theta, M),
    fnSum = summaries_fun,
    fnLogPrior = bsl_fnPrior_hier,
    theta0 = theta0,
    test = FALSE
  )
}


# --------------------------
# Bridge estimator for model comparison
# --------------------------
bsl_bridge_estimate <- function(loglik) {
  m <- max(loglik)
  m + log(mean(exp(loglik - m)))
}



# Convert fits_multi into coda::mcmc.list for each model
bsl_to_mcmc_list_multi <- function(fits_multi) {
  lapply(fits_multi, function(chain_list) {
    chains <- lapply(chain_list, function(f) {
      as.mcmc(as.matrix(f@theta))
    })
    mcmc.list(chains)
  })
}

# --------------------------
# posterior samples for further analysis
# --------------------------
# Helper function to compute mean and 90th percentile for each posterior draw
# corrected get_summary_stats for hierarchical theta = (mu0, log_tau, log_phi)
bsl_get_summary_stats <- function(dist_name, post, L = 50) {
  # post: data.frame or matrix of posterior draws (columns in order theta1, theta2, theta3)
  post <- as.matrix(post)
  n_draws <- nrow(post)

  # storage: predictive mean and q90 per posterior draw
  pred_mean   <- numeric(n_draws)
  pred_median <- numeric(n_draws)
  pred_q90    <- numeric(n_draws)

  for (i in seq_len(n_draws)) {
    theta <- post[i, ]
    mu0  <- as.numeric(theta[1])
    tau  <- exp(as.numeric(theta[2]))
    phi  <- exp(as.numeric(theta[3]))   # phi is distribution-specific parameter in your simulator

    # draw L study-level locations (loc_d) to integrate over the hierarchical random effect
    locs <- rnorm(L, mean = mu0, sd = tau)

    if (dist_name == "lognormal") {
      # simulator: rlnorm(n, meanlog = loc_d, sdlog = phi)
      # mean of lognormal with (meanlog = m, sdlog = s): exp(m + s^2/2)
      means  <- exp(locs + phi^2 / 2)
      median <- qlnorm(0.5, meanlog = locs, sdlog = phi)
      q90s   <- qlnorm(0.9, meanlog = locs, sdlog = phi)

    } else if (dist_name == "gamma") {
      # simulator used: mean_d <- exp(loc); shape <- phi; rate <- shape/mean_d
      # hence mean = mean_d, where mean_d = exp(loc)
      # 90th percentile: qgamma(0.9, shape=phi, rate = phi/mean_d)
      mean_d <- exp(locs)
      means  <- mean_d
      # shape = phi, rate = phi / mean_d
      median <- qgamma(0.5, shape = phi, rate = phi / mean_d)
      q90s   <- qgamma(0.9, shape = phi, rate = phi / mean_d)

    } else if (dist_name == "weibull") {
      # simulator used: scale <- exp(loc); shape <- phi
      # mean of Weibull: scale * gamma(1 + 1/shape)
      scales <- exp(locs)
      means  <- scales * gamma(1 + 1/phi)
      median <- qweibull(0.5, shape = phi, scale = scales)
      q90s   <- qweibull(0.9, shape = phi, scale = scales)

    } else {
      stop("Unknown distribution name in get_summary_stats")
    }

    # integrate over locs by averaging
    pred_mean[i]   <- mean(means, na.rm = TRUE)
    pred_q90[i]    <- mean(q90s, na.rm = TRUE)
    pred_median[i] <- mean(median, na.rm = TRUE)
  }

  # return data.frame of predictive summaries per posterior draw
  data.frame(mean = pred_mean, q90 = pred_q90, median = pred_median)
}

# wrapper to compute posterior point estimate and 95% CrI
bsl_summarise_posteriors <- function(posterior_samples_list, L = 50) {
  out <- lapply(names(posterior_samples_list), function(model_name) {
    post <- posterior_samples_list[[model_name]]
    stats <- bsl_get_summary_stats(model_name, post, L = L)

    data.frame(
      model = model_name,
      mean_mean = mean(stats$mean),
      mean_low  = quantile(stats$mean, 0.025),
      mean_high = quantile(stats$mean, 0.975),
      median_mean  = mean(stats$median),
      median_low   = quantile(stats$median, 0.025),
      median_high  = quantile(stats$median, 0.975),
      q90_mean  = mean(stats$q90),
      q90_low   = quantile(stats$q90, 0.025),
      q90_high  = quantile(stats$q90, 0.975)
    )
  })
  do.call(rbind, out)
}


# Posterior predictive draw samples
bsl_make_density_summary <- function(dist_name, post,
                                 x_seq = seq(0, 20, length.out = 300),
                                 n_draws = 200, L = 20) {
  post <- as.matrix(post)
  draws <- sample(1:nrow(post), min(n_draws, nrow(post)))
  dens_mat <- matrix(NA, nrow = length(draws), ncol = length(x_seq))

  for (i in seq_along(draws)) {
    theta <- post[draws[i], ]
    mu0 <- theta[1]; tau <- exp(theta[2]); phi <- exp(theta[3])

    # integrate over L loc_d draws
    locs <- rnorm(L, mu0, tau)
    dens_l <- sapply(locs, function(loc_d) {
      if (dist_name == "lognormal") {
        dlnorm(x_seq, meanlog = loc_d, sdlog = phi)
      } else if (dist_name == "gamma") {
        mean_d <- exp(loc_d)
        shape <- phi
        rate <- shape / mean_d
        dgamma(x_seq, shape = shape, rate = rate)
      } else if (dist_name == "weibull") {
        scale <- exp(loc_d)
        dweibull(x_seq, shape = phi, scale = scale)
      }
    })
    dens_mat[i, ] <- rowMeans(dens_l)  # average over L loc_d draws
  }

  data.frame(
    x = x_seq,
    mean = apply(dens_mat, 2, mean, na.rm = TRUE),
    low  = apply(dens_mat, 2, quantile, 0.025, na.rm = TRUE),
    high = apply(dens_mat, 2, quantile, 0.975, na.rm = TRUE),
    model = dist_name
  )
}


# --------------------------
# Posterior predictive density summaries (for plotting)
# --------------------------
bsl_make_density_summary <- function(dist_name, post,
                                 x_seq = seq(0, 20, length.out = 400),
                                 n_draws = 200, L = 20) {
  post <- as.matrix(post)
  draws <- sample(1:nrow(post), min(n_draws, nrow(post)))
  dens_mat <- matrix(NA, nrow = length(draws), ncol = length(x_seq))
  for (i in seq_along(draws)) {
    theta <- post[draws[i], ]
    mu0 <- theta[1]; tau <- exp(theta[2]); phi <- exp(theta[3])
    locs <- rnorm(L, mu0, tau)
    dens_l <- sapply(locs, function(loc_d) {
      if (dist_name == "lognormal") {
        dlnorm(x_seq, meanlog = loc_d, sdlog = phi)
      } else if (dist_name == "gamma") {
        mean_d <- exp(loc_d); shape <- phi; rate <- shape / mean_d
        dgamma(x_seq, shape = shape, rate = rate)
      } else if (dist_name == "weibull") {
        scale <- exp(loc_d); dweibull(x_seq, shape = phi, scale = scale)
      }
    })
    dens_mat[i, ] <- rowMeans(dens_l)
  }
  data.frame(
    x = x_seq,
    mean = apply(dens_mat, 2, mean, na.rm = TRUE),
    low  = apply(dens_mat, 2, quantile, 0.025, na.rm = TRUE),
    high = apply(dens_mat, 2, quantile, 0.975, na.rm = TRUE),
    model = dist_name
  )
}


# ===============================
# AUTOMATED BSL DIAGNOSTIC REPORT
# ===============================

bsl_to_mcmc <- function(fit) {
  if (!is.null(fit@theta)) as.mcmc(as.matrix(fit@theta))
  else stop("fit@theta not found in object.")
}

bsl_combine_chains <- function(fit_list) {
  mcmc.list(lapply(fit_list, to_mcmc))
}

bsl_run_diagnostics <- function(fits_multi, diagnostic_dir = "bsl_diagnostics") {
  results <- list()

  for (model_name in names(fits_multi)) {
    message("\n🔍 Checking model: ", model_name)
    model_dir <- file.path(diagnostic_dir, model_name)
    if (!dir.exists(model_dir)) dir.create(model_dir)

    fit_list <- fits_multi[[model_name]]
    mcmc_obj <- combine_chains(fit_list)

    # === TRACEPLOT ===
    png(file.path(model_dir, paste0("traceplot_", model_name, ".png")),
        width = 1800, height = 1000, res = 150)
    traceplot(mcmc_obj, main = paste("Traceplots for", model_name))
    dev.off()

    # === AUTOCORRELATION PLOTS ===
    png(file.path(model_dir, paste0("acf_", model_name, ".png")),
        width = 1600, height = 800, res = 150)
    par(mfrow = c(1, min(3, ncol(as.matrix(mcmc_obj[[1]])))), mar = c(4,4,2,1))
    for (p in seq_len(ncol(as.matrix(mcmc_obj[[1]])))) {
      acf(as.numeric(as.matrix(mcmc_obj[[1]])[, p]),
          main = paste("ACF -", model_name, "param", p))
    }
    dev.off()

    # === DIAGNOSTICS ===
    gel <- gelman.diag(mcmc_obj)
    ess <- effectiveSize(mcmc_obj)
    gew <- geweke.diag(mcmc_obj)

    df <- data.frame(
      parameter = names(ess),
      Rhat = gel$psrf[, 1],
      ESS = as.numeric(ess),
      Geweke_Z = sapply(gew, function(x) x$z)
    )
    df$model <- model_name
    write.csv(df, file.path(model_dir, paste0("diagnostics_", model_name, ".csv")), row.names = FALSE)

    results[[model_name]] <- df
  }

  # Combine results across models
  all_results <- bind_rows(results)
  write.csv(all_results, file.path(diagnostic_dir, "all_diagnostics_summary.csv"), row.names = FALSE)

  # === 4. Plots across models ===

  # Effective sample size
  p1 <- ggplot(all_results, aes(x = parameter, y = ESS, fill = model)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_hline(yintercept = 200, linetype = "dashed", color = "red") +
    theme_bw() +
    labs(title = "Effective Sample Size (ESS)", y = "ESS per parameter")

  # R-hat
  p2 <- ggplot(all_results, aes(x = parameter, y = Rhat, color = model, group = model)) +
    geom_point(size = 3, position = position_jitter(width = 0.1, height = 0)) +
    geom_hline(yintercept = 1.05, linetype = "dashed", color = "red") +
    theme_bw() +
    labs(title = "Gelman–Rubin R̂ Diagnostic", y = "R̂ value")

  ggsave(file.path(diagnostic_dir, "effective_sample_size.png"), p1, width = 9, height = 6)
  ggsave(file.path(diagnostic_dir, "gelman_rhat.png"), p2, width = 9, height = 6)

  message("\n✅ Diagnostics complete! Files saved in: ", normalizePath(diagnostic_dir))
  return(invisible(all_results))
}


bsl_make_trace_df <- function(fits_multi) {
  trace_dfs <- lapply(names(fits_multi), function(mdl) {
    chain_list <- fits_multi[[mdl]]
    bind_rows(lapply(seq_along(chain_list), function(ch) {
      df <- as.data.frame(chain_list[[ch]]@theta)
      colnames(df) <- paste0("theta", seq_len(ncol(df)))
      df$iter <- seq_len(nrow(df))
      df$chain <- paste0("chain_", ch)
      df$model <- mdl
      df
    }))
  })
  bind_rows(trace_dfs)
}


