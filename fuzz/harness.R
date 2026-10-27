# The fuzzing harness: runs the calls TypR draws for the functions of a
# package, and reports what each one does.
#
# Usage: Rscript harness.R DESCRIBE.R PKG SAMPLES.jsonl RESULTS.jsonl [TIMEOUT]
#
# PKG is loaded from the library path (R_LIBS). Each line of SAMPLES is a
# call to run:
#   {"id": 1, "kind": "r", "fun": "f", "symbol": null,
#    "args": [{"name": null, "src": "1L"}, {"name": "y", "src": "\"a\""}]}
# where `fun` is a function of the namespace of PKG (exported or not), or,
# with kind "native", the R name of a .Call entry point (an object of the
# namespace) and `symbol` its C symbol, looked up when there is no such
# object. `args` are the arguments in order, with their R source.
#
# Each call gets one line in RESULTS. A line {"id": 1, "start": true} is
# written and flushed before the call: if R dies during it, the last line is
# a start, and running the harness again with the same files resumes after
# that sample (the caller reports it as a crash). Then:
#   {"id": 1, "outcome": O, ...}
# with O one of
#   "value"     the call returned; `value` describes the result (as
#               describe.R does);
#   "error"     it signalled an error: `class`, `message`, and where it comes
#               from: `fun`, the function of the call of the condition,
#               `ns`, the namespace that function comes from, `primitive`
#               (an error raised by the C code of an entry point has the
#               entry point as `fun`, and no `ns`);
#   "timeout"   it ran past TIMEOUT seconds (default 5) of elapsed time
#               (R code only: C code is only interrupted by the caller's
#               limits);
#   "bad_args"  building the arguments failed (`message`): the sample is no
#               R value, nothing was called.
# and in every case but "bad_args": `exported`, whether the function is
# exported (R definitions only); `forced`, for each argument whether it
# was evaluated; `effects`, the side-effecting functions called (see
# fuzz_effects below), in order, without repetition; `warnings`, the
# messages of the first warnings; `elapsed`, in seconds.
#
# The harness does not contain the effects: the sandbox it runs in does.

args <- commandArgs(trailingOnly = TRUE)
source(args[[1]])
pkg <- args[[2]]
samples_file <- args[[3]]
results_file <- args[[4]]
timeout <- if (length(args) >= 5) as.numeric(args[[5]]) else 5

# ===== The function to call =====

ns <- asNamespace(loadNamespace(pkg))

fuzz_target <- function(s) {
  if (identical(s$kind, "native")) {
    obj <- get0(s$fun, envir = ns, inherits = FALSE)
    info <- if (inherits(obj, "NativeSymbolInfo")) obj
            else getNativeSymbolInfo(s$symbol, PACKAGE = pkg)
    # .Call takes the arguments in order, unnamed.
    function(...) .Call(info, ...)
  } else get(s$fun, envir = ns, inherits = FALSE)
}

# ===== Side effects =====

# Functions with effects outside R: each call made while a sample runs is
# recorded. Quitting is refused, so that the harness survives.
fuzz_effects <- c("file", "gzfile", "bzfile", "xzfile", "unlink", "file.remove",
                  "file.rename", "file.copy", "file.create", "file.append",
                  "dir.create", "saveRDS", "save", "writeLines", "system",
                  "system2", "download.file", "url", "socketConnection",
                  "Sys.setenv", "Sys.unsetenv", "setwd", "q", "quit")

fuzz <- new.env()
fuzz$active <- FALSE
fuzz$effects <- character()

fuzz_effect <- function(name) {
  if (fuzz$active) {
    fuzz$effects <- union(fuzz$effects, name)
    if (name %in% c("q", "quit")) stop("fuzz: quitting is not allowed", call. = FALSE)
  }
}

for (f in fuzz_effects) {
  suppressMessages(trace(f, where = baseenv(), print = FALSE,
                         tracer = bquote(.(fuzz_effect)(.(f)))))
}

# writeLines to the console is no effect.
fuzz_effect_writeLines <- function(con) {
  if (!identical(con, stdout())) fuzz_effect("writeLines")
}
suppressMessages(untrace("writeLines", where = baseenv()))
invisible(suppressMessages(trace("writeLines", where = baseenv(), print = FALSE,
                                 tracer = bquote(.(fuzz_effect_writeLines)(con)))))

# No interaction.
options(warn = 1, menu.graphics = FALSE)
assign("readline", function(...) "", envir = globalenv())

# ===== Errors =====

# Where an error comes from: the function of the call of the condition, the
# namespace it is defined in, and whether it is a primitive. Run while the
# stack is still there.
fuzz_origin <- function(cond, env) {
  call <- conditionCall(cond)
  if (!is.call(call)) return(list())
  head <- call[[1]]
  fun <- paste(deparse(head), collapse = "")
  calls <- sys.calls()
  for (i in rev(seq_along(calls))) {
    if (identical(calls[[i]], call)) {
      f <- sys.function(i)
      if (is.primitive(f)) break
      e <- environment(f)
      return(list(fun = fun, ns = if (is.null(e)) "" else environmentName(e),
                  primitive = FALSE))
    }
  }
  # Primitives have no frame: look the function up.
  f <- if (is.symbol(head)) get0(as.character(head), envir = env, mode = "function")
  if (is.primitive(f)) list(fun = fun, ns = "base", primitive = TRUE)
  else list(fun = fun, ns = "", primitive = FALSE)
}

# ===== Running a sample =====

fuzz_run <- function(s) {
  n <- length(s$args$src)
  srcs <- if (n) s$args$src else character()
  names <- if (n) s$args$name else character()
  vals <- tryCatch(lapply(srcs, function(src) eval(parse(text = src, encoding = "UTF-8"),
                                                   envir = globalenv())),
                   error = function(e) e)
  if (inherits(vals, "error"))
    return(list(outcome = "bad_args", message = conditionMessage(vals)))

  fn <- fuzz_target(s)
  # Each argument is a promise that records when it is forced.
  forced <- logical(n)
  env <- new.env(parent = globalenv())
  syms <- lapply(seq_len(n), function(i) {
    name <- paste0(".fuzz_arg", i)
    delayedAssign(name, { forced[[i]] <<- TRUE ; vals[[i]] }, assign.env = env)
    as.name(name)
  })
  if (n) names(syms) <- ifelse(is.na(names), "", names)
  # Called by its name, which errors then show.
  assign(s$fun, fn, envir = env)
  call <- as.call(c(list(as.name(s$fun)), syms))

  fuzz$effects <- character()
  warnings <- character()
  origin <- list()
  set.seed(s$id)
  start <- proc.time()[["elapsed"]]
  sink(nullfile())
  setTimeLimit(elapsed = timeout, transient = TRUE)
  fuzz$active <- TRUE
  res <- tryCatch(
    withCallingHandlers(list(outcome = "value", value = eval(call, env)),
      warning = function(w) {
        if (length(warnings) < 3) warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) invokeRestart("muffleMessage"),
      error = function(e)
        origin <<- tryCatch(fuzz_origin(e, if (is.primitive(fn)) baseenv() else environment(fn)),
                            error = function(e) list())),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("reached elapsed time limit", msg, fixed = TRUE)) list(outcome = "timeout")
      else c(list(outcome = "error", class = as.list(class(e)), message = msg), origin)
    })
  fuzz$active <- FALSE
  setTimeLimit(elapsed = Inf)
  sink()
  elapsed <- proc.time()[["elapsed"]] - start

  if (res$outcome == "value") res$value <- fuzz_describe(res$value)
  exported <- if (identical(s$kind, "native")) NULL else s$fun %in% getNamespaceExports(ns)
  c(res, list(exported = exported, forced = as.list(forced), effects = as.list(fuzz$effects),
              warnings = as.list(warnings), elapsed = round(elapsed, 3)))
}

# ===== Main loop =====

json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)

# Resume after the last sample started by an earlier run.
done <- -Inf
if (file.exists(results_file)) {
  for (line in readLines(results_file, encoding = "UTF-8"))
    done <- max(done, jsonlite::fromJSON(line)$id)
}

out <- file(results_file, open = "a", encoding = "UTF-8")
for (line in readLines(samples_file, encoding = "UTF-8")) {
  s <- jsonlite::fromJSON(line)
  if (s$id <= done) next
  writeLines(json(list(id = s$id, start = TRUE)), out)
  flush(out)
  r <- tryCatch(fuzz_run(s), error = function(e)
    list(outcome = "harness_error", message = conditionMessage(e)))
  writeLines(json(c(list(id = s$id), r)), out)
  flush(out)
}
close(out)
