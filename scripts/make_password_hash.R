args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1 || !nzchar(args[[1]])) {
  stop("Kullanım: Rscript scripts/make_password_hash.R 'cok-guclu-sifren'")
}
if (!requireNamespace("sodium", quietly = TRUE)) install.packages("sodium", repos = "https://cloud.r-project.org")
cat(sodium::password_store(args[[1]]), "\n")

