# Prints the results of the harness without their timings, which vary.
# Usage: Rscript harness_check.R results.jsonl
for (line in readLines(commandArgs(trailingOnly = TRUE)[[1]], encoding = "UTF-8")) {
  r <- jsonlite::fromJSON(line, simplifyVector = FALSE)
  r$elapsed <- NULL
  writeLines(jsonlite::toJSON(r, auto_unbox = TRUE, null = "null", digits = NA))
}
