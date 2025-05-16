#' Fit or load cached rstan model with separate compilation
#'
#' This function separates model compilation from sampling. It compiles the Stan model only if the model file changes,
#' and fits the model only if the data or sampling arguments change.
#'
#' @param model_name A short name used to save the compiled model and fit files.
#' @param stan_path Path to the Stan model file (.stan).
#' @param data_list A named list of data for the Stan model.
#' @param chains Number of chains (default = 4).
#' @param cores Number of cores (default = 4).
#' @param cache_dir Directory to store compiled model and fit files (default = "./model_fits").
#' @param ... Additional arguments passed to \code{rstan::sampling()} (e.g., iter, warmup, seed).
#'
#' @return A fitted \code{stanfit} object.
#' @export
run_or_load_stan <- function(model_name, stan_path, data_list, chains = 4, cores = 4, cache_dir = "./model_fits", ...) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # File paths
  model_file <- file.path(cache_dir, paste0(model_name, "_compiled.rds"))
  model_hash_file <- file.path(cache_dir, paste0(model_name, "_modelhash.txt"))
  fit_file <- file.path(cache_dir, paste0(model_name, "_fit.rds"))
  fit_hash_file <- file.path(cache_dir, paste0(model_name, "_fithash.txt"))

  # Hashes
  stan_hash <- unname(tools::md5sum(stan_path))
  data_hash <- digest::digest(data_list)
  sampling_args <- list(chains = chains, cores = cores, ...)
  args_hash <- digest::digest(sampling_args)
  fit_combined_hash <- paste(stan_hash, data_hash, args_hash, sep = "_")

  # Compile or load model
  if (!file.exists(model_file) || !file.exists(model_hash_file) || readLines(model_hash_file) != stan_hash) {
    message("Compiling Stan model...")
    m <- rstan::stan_model(file = stan_path)
    saveRDS(m, model_file)
    writeLines(stan_hash, model_hash_file)
  } else {
    message("Loading compiled Stan model...")
    m <- readRDS(model_file)
  }

  # Fit or load cached result
  if (!file.exists(fit_file) || !file.exists(fit_hash_file) || readLines(fit_hash_file) != fit_combined_hash) {
    message("Sampling ", model_name, "...")
    fit <- rstan::sampling(m, data = data_list, chains = chains, cores = cores, ...)
    saveRDS(fit, fit_file)
    writeLines(fit_combined_hash, fit_hash_file)
  } else {
    message("Loading cached fit for ", model_name, "...")
    fit <- readRDS(fit_file)
  }

  return(fit)
}
