# Evaluates the R source of each value of `rval_test.exe --cases` and prints
# its description, one per line.
# Usage: Rscript rval_check.R describe.R cases.jsonl
args <- commandArgs(trailingOnly = TRUE)
source(args[[1]])
for (line in readLines(args[[2]], encoding = "UTF-8")) {
  src <- jsonlite::fromJSON(line)$src
  writeLines(fuzz_describe_json(eval(parse(text = src, encoding = "UTF-8"))))
}
