#' Fit or load cached CmdStanR model with separate compilation and sampling
#'
#' This function compiles a Stan model using CmdStanR (with threading support) only when the Stan file changes,
#' and re-samples only when the data or sampling arguments change. Cached compiled models and fits are stored
#' using hashes of the model, data, and sampling arguments.
#'
#' @param model_name A short name used to save the compiled model and fit files.
#' @param stan_path Path to the Stan model file (.stan).
#' @param data_list A named list of data for the Stan model.
#' @param chains Number of MCMC chains (default = 4).
#' @param cores Number of CPU cores to use (default = all available minus reserved proportion).
#' @param cache_dir Directory to store compiled model and fit files (default = "../model_fits").
#' @param force_recompile Logical, if TRUE forces recompilation even if model hash matches (default = FALSE).
#' @param force_resample Logical, if TRUE forces re-sampling even if fit hash matches (default = FALSE).
#' @param ... Additional arguments passed to \code{CmdStanModel$sample()}, including \code{threads_per_chain}, \code{reserved_core_prop}, \code{iter_sampling}, etc.
#'
#' @return A fitted \code{CmdStanMCMC} object.
#' @export
run_or_load_model <- function(model_name, stan_path, data_list,
                              chains = NULL,
                              cores = NULL,
                              cache_dir = "./model_fits",
                              force_recompile = FALSE,
                              force_resample = FALSE,
                              ...) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("The 'cmdstanr' package is required. Install with: install.packages('cmdstanr')")
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # User argument handling
  user_args <- list(...)
  threads_per_chain <- user_args$threads_per_chain
  user_args$threads_per_chain <- NULL

  # Default: reserve 10% of cores for other tasks
  reserved_core_prop <- user_args$reserved_core_prop %||% 0.1
  user_args$reserved_core_prop <- NULL

  all_cores <- parallel::detectCores()
  reserved_cores <- max(1, round(all_cores * reserved_core_prop))
  available_cores <- max(all_cores - reserved_cores, 1)
  total_cores <- if (is.null(cores)) available_cores else cores

  chains <- chains %||% 4
  threads_per_chain <- threads_per_chain %||% max(floor(total_cores / chains), 1)
  parallel_chains <- chains

  message("Using ", chains, " chains × ", threads_per_chain, " threads (",
          chains * threads_per_chain, " of ", all_cores, " cores; ",
          reserved_cores, " reserved)")

  # Paths
  model_hash_file <- file.path(cache_dir, paste0(model_name, "_modelhash.txt"))
  model_rds_file  <- file.path(cache_dir, paste0(model_name, "_model.rds"))
  fit_file        <- file.path(cache_dir, paste0(model_name, "_fit.rds"))
  fit_hash_file   <- file.path(cache_dir, paste0(model_name, "_fithash.txt"))

  # Hashing
  stan_hash <- unname(tools::md5sum(stan_path))
  data_hash <- digest::digest(data_list)
  args_hash <- digest::digest(c(
    list(chains = chains,
         parallel_chains = parallel_chains,
         threads_per_chain = threads_per_chain),
    user_args
  ))
  fit_combined_hash <- paste(stan_hash, data_hash, args_hash, sep = "_")

  # Load or compile model
  if (!file.exists(model_rds_file) || force_recompile ||
      !file.exists(model_hash_file) || readLines(model_hash_file) != stan_hash) {
    message("Compiling Stan model with threading support...")
    model_obj <- cmdstanr::cmdstan_model(
      stan_path,
      cpp_options = list(stan_threads = TRUE)
    )
    saveRDS(model_obj, model_rds_file)
    writeLines(stan_hash, model_hash_file)
  } else {
    message("Using cached compiled Stan model...")
    model_obj <- readRDS(model_rds_file)
  }

  # Load or run sampling
  if (!file.exists(fit_file) || force_resample ||
      !file.exists(fit_hash_file) || readLines(fit_hash_file) != fit_combined_hash) {
    message("Sampling ", model_name,
            " (chains = ", chains,
            ", threads_per_chain = ", threads_per_chain, ")...")
    fit <- do.call(model_obj$sample, c(
      list(
        data = data_list,
        chains = chains,
        parallel_chains = parallel_chains,
        threads_per_chain = threads_per_chain
      ),
      user_args
    ))
    saveRDS(fit, fit_file)
    writeLines(fit_combined_hash, fit_hash_file)
  } else {
    message("Loading cached fit for ", model_name, "...")
    fit <- readRDS(fit_file)
  }

  return(fit)
}

# Helper fallback operator
`%||%` <- function(a, b) if (!is.null(a)) a else b
