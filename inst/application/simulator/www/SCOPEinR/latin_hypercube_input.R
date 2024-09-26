#' latin_hypercube_input function
#'
#' @param tab
#' @param n_spectra
#' @param outdir
#'
#' @return
#' @export
#'
#' @author 	Christiaan van der Tol(Original version in Matlab)
#' @author Carlos Camino (Ported version into R)
#'
#' @examples
#'
latin_hypercube_input <- function(tab = read.table(file.path('input', 'dataset for_verification', 'input_borders.csv'), header = TRUE),
                                  n_spectra = 30, outdir = file.path('input', 'dataset for_verification')) {

  if (missing(tab)) {
    tab <- read.table(file.path(outdir, 'input_borders.csv'), header = TRUE)
    n_spectra <- 30
  }

  out_file <- file.path(outdir, 'lh_ts.csv')
  if (file.exists(out_file)) stop(sprintf('`%s` file already exists, delete it first', out_file))

  include <- as.logical(tab$include)
  lb <- as.numeric(tab$lower[include])
  ub <- as.numeric(tab$upper[include])
  varnames <- as.character(tab$variable[include])

  # one row - one set of parameters
  lh <- lhs::maximinLHS(n = n_spectra, k = sum(include))
  params <- t(t((ub-lb) * lh) + lb)

  if ('LIDFa' %in% varnames) {
    # abs(LIDFa + LIDFb) <= 1
    i_lidfa <- which(varnames == 'LIDFa')
    i_lidfb <- which(varnames == 'LIDFb')
    lidfa <- params[, i_lidfa]
    lidfb <- params[, i_lidfb]
    params[, i_lidfa] <- (lidfa + lidfb) / 2
    params[, i_lidfb] <- (lidfa - lidfb) / 2
  }

  t <- data.frame(params)
  colnames(t) <- varnames
  t$t <- seq_len(nrow(t))
  write.table(t, out_file, row.names = FALSE)

  varnames_in <- paste(varnames, collapse = ', ')
  cat(sprintf('Sampled %i parameters: %s\n', length(varnames), varnames_in))
  cat(sprintf('Saved lut input (parameters) in `%s`\n', out_file))
}
