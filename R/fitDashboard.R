## FireSense spread-fit dashboard: progress of every DEoptim fit, read from the cache.
##
## Each generation of a spread fit is one cache entry ("DEoptimForCache__<generation>"), holding that
## generation's population and objective values. A generation starts from the previous generation's
## population, and its cache entry records the digest of that starting population
## (".cacheExtra.initialpop"), so the entries chain into whole fits without any log or terminal history.
## Which ELF a fit belongs to is read from the tmux pane that runs it (pane titles end in the ELF), when
## one does; otherwise the fit is labelled by its start time.
##
## usage, from an R session on the machine that runs the fits (e.g. Positron or RStudio Server):
##   source("R/fitDashboard.R")
##   fitDashboard()                      # opens in the viewer or browser; refreshes every 2 minutes
##   fp <- fitProgress()                 # the same data, as a list of data.tables

## The DEoptim generations cached since `since`, chained into fits.
## Returns list(fits = one row per fit, gens = one row per fit and generation (objective quantiles),
## params = one row per fit, generation and parameter (quantiles and the best member's value)).
## `labelledOnly`: drop fits whose ELF is unknown (fits from earlier campaigns, whose panes are gone).
fitProgress <- function(cachePath = "/mnt/fast/cache", since = Sys.time() - 2 * 86400,
                        tmuxSession = NULL, runningWithin = 15 * 60, labelledOnly = TRUE, state = new.env()) {
  stopifnot(requireNamespace("data.table"), requireNamespace("qs2"), requireNamespace("reproducible"))
  dir <- file.path(cachePath, "cacheOutputs")
  db <- list.files(dir, pattern = "\\.dbFile\\.qs2$", full.names = TRUE)
  mt <- file.mtime(db)
  db <- db[!is.na(mt) & mt >= since]
  ## `state` keeps what was read already: a refresh reads only new entries
  if (is.null(state$entries)) state$entries <- list()
  ids <- sub("\\.dbFile\\.qs2$", "", basename(db))
  for (i in which(!ids %in% names(state$entries))) {
    tg <- tryCatch(as.data.frame(qs2::qs_read(db[i])), error = function(e) NULL)
    if (is.null(tg)) next
    fn <- tg$tagValue[tg$tagKey == "function"]
    if (!length(fn) || !grepl("^DEoptimForCache__[0-9]+$", fn[1])) {
      state$entries[[ids[i]]] <- NA          # not a generation; never read again
      next
    }
    obj <- tryCatch(qs2::qs_read(file.path(dir, paste0(ids[i], ".qs2"))), error = function(e) NULL)
    if (is.null(obj$member$pop)) next      # not written completely yet: read on the next refresh
    ip <- tg$tagValue[grepl("^\\.cacheExtra\\.initialpop:", tg$tagValue)]
    state$entries[[ids[i]]] <- list(
      id = ids[i], gen = as.integer(sub(".*__", "", fn[1])),
      time = file.mtime(db[i]),
      parent = if (length(ip)) sub(".*:", "", ip[1]) else NA_character_,
      self = reproducible::.robustDigest(obj$member$pop),
      pop = obj$member$pop, popval = obj$member$popval, names = names(obj$optim$bestmem))
  }
  ent <- Filter(is.list, state$entries[intersect(ids, names(state$entries))])
  if (!length(ent)) return(list(fits = data.table::data.table(), gens = NULL, params = NULL))
  idx <- data.table::rbindlist(lapply(ent, `[`, c("id", "gen", "time", "parent", "self")))

  ## a fit is a chain of entries: each one's `parent` is the digest of the previous one's population.
  ## The same generation can be cached more than once (a fit restarted from the cache), so keep one.
  idx <- unique(idx, by = c("self", "gen"))
  child <- idx[, .(cid = id, cgen = gen, parent)]
  link <- merge(idx[, .(id, gen, self)], child, by.x = "self", by.y = "parent")[cgen == gen + 1L]
  nxt <- stats::setNames(link$cid, link$id)
  starts <- setdiff(idx$id, link$cid)
  chains <- lapply(starts, function(s) { out <- s; while (!is.na(n <- nxt[out[length(out)]])) out <- c(out, n); out })
  chains <- chains[lengths(chains) > 1L]
  if (!length(chains)) return(list(fits = data.table::data.table(), gens = NULL, params = NULL))

  ## a fit is named by its first entry, which does not change as it grows. Its ELF, once seen in a pane,
  ## is kept in `state`: the pane's output scrolls past the fit once the pane moves on to predicting.
  names(chains) <- vapply(chains, `[`, "", 1L)
  elfOf <- .fitElfFromTmux(tmuxSession, unlist(chains))
  if (is.null(state$elf)) state$elf <- character()
  for (k in names(chains)) {
    elf <- unique(stats::na.omit(elfOf[chains[[k]]]))
    if (length(elf)) state$elf[k] <- elf[1]
  }
  if (labelledOnly) chains <- chains[names(chains) %in% names(state$elf)]
  if (!length(chains)) return(list(fits = data.table::data.table(), gens = NULL, params = NULL))
  fits <- data.table::rbindlist(lapply(names(chains), function(k) {
    e <- ent[chains[[k]]]
    last <- e[[length(e)]]
    started <- e[[1]]$time
    data.table::data.table(
      fit = k, elf = if (k %in% names(state$elf)) state$elf[[k]] else NA_character_,
      started = started, updated = last$time, firstGen = e[[1]]$gen, gen = last$gen,
      best = min(last$popval),
      status = .fitStatus(last$time, runningWithin, if (k %in% names(state$elf)) state$elf[[k]] else NA,
                          attr(elfOf, "fitting"), tmuxSeen = length(elfOf) > 0))
  }))
  fits[is.na(elf), elf := paste("fit started", format(started, "%m-%d %H:%M"))]
  gens <- data.table::rbindlist(lapply(names(chains), function(k) data.table::rbindlist(lapply(ent[chains[[k]]], function(x)
    data.table::data.table(fit = k, gen = x$gen, best = min(x$popval),
                           q10 = stats::quantile(x$popval, 0.1, names = FALSE),
                           med = stats::median(x$popval))))))
  params <- data.table::rbindlist(lapply(names(chains), function(k) data.table::rbindlist(lapply(ent[chains[[k]]], function(x) {
    p <- x$pop; nm <- colnames(p) %||% x$names %||% paste0("par", seq_len(ncol(p)))
    data.table::data.table(fit = k, gen = x$gen, param = nm,
                           best = p[which.min(x$popval), ],
                           q10 = apply(p, 2, stats::quantile, 0.1, names = FALSE),
                           med = apply(p, 2, stats::median),
                           q90 = apply(p, 2, stats::quantile, 0.9, names = FALSE))
  }))))
  list(fits = fits[order(elf)], gens = gens, params = params)
}

## RUNNING: a generation in the last `runningWithin` seconds and its pane still fitting. FINISHED: its pane
## has moved on to the simulation. STOPPED: neither (the pane is gone, or the fit ended without finishing).
## The last generation of a finished fit is recent for a while, so the pane decides.
.fitStatus <- function(lastTime, runningWithin, elf, fitting, tmuxSeen) {
  recent <- difftime(Sys.time(), lastTime, units = "secs") <= runningWithin
  if (!is.na(elf) && tmuxSeen && !elf %in% fitting) return("FINISHED")
  if (recent) "RUNNING" else "STOPPED"
}

## The ELF of each cache entry named in a tmux pane's recent output: the pane title ends in the ELF
## (experimentTmux names worker panes "<host>-<pid>-<ELF>"). NA for entries no pane names.
.fitElfFromTmux <- function(tmuxSession, ids) {
  out <- stats::setNames(rep(NA_character_, length(ids)), ids)
  attr(out, "fitting") <- character()     # ELFs whose pane is still fitting, not predicting
  if (!nzchar(Sys.which("tmux"))) return(out)
  args <- c("list-panes", if (is.null(tmuxSession)) "-a" else c("-s", "-t", tmuxSession), "-F", shQuote("#{pane_id}\t#{pane_title}"))
  panes <- suppressWarnings(system2("tmux", args, stdout = TRUE, stderr = FALSE))
  if (!length(panes) || !is.null(attr(panes, "status"))) return(out)
  for (pn in strsplit(panes, "\t")) {
    if (length(pn) < 2 || !grepl("-[0-9.]+$", pn[2])) next
    elf <- sub(".*-", "", pn[2])
    txt <- suppressWarnings(system2("tmux", c("capture-pane", "-p", "-J", "-S", "-", "-t", pn[1]), stdout = TRUE))
    ## "Saved! Cache file: <id>.qs2; fn: DEoptimForCache__<n>" or "Object to retrieve (fn: DEoptimForCache__<n>, <id>.qs2)"
    lns <- grep("DEoptimForCache__", txt, value = TRUE)
    seen <- unlist(regmatches(lns, gregexpr("[0-9a-f]{16}(?=\\.qs2)", lns, perl = TRUE)))
    hit <- intersect(seen, ids)
    out[hit] <- elf
    ## the pane's last timestamped lines are DEoptim's while it fits; after the fit they are the simulation's
    recent <- utils::tail(grep("^[A-Z][a-z]{2}[0-9]{2} ", txt, value = TRUE), 20)
    if (any(grepl("DEoptim|evaluations:|Iteration [0-9]+ done|Re-scored", recent)))
      attr(out, "fitting") <- c(attr(out, "fitting"), elf)
  }
  out
}

## The Shiny dashboard: a summary table, then a card per fit with its objective by generation and, under
## a collapsed section, its parameters by generation. `refreshMinutes`: how often it re-reads the cache.
fitDashboard <- function(cachePath = "/mnt/fast/cache", since = Sys.time() - 2 * 86400, tmuxSession = NULL,
                         refreshMinutes = 2, launch.browser = getOption("shiny.launch.browser", interactive())) {
  shiny::runApp(fitDashboardApp(cachePath, since, tmuxSession, refreshMinutes), launch.browser = launch.browser)
}

## The app fitDashboard() runs
fitDashboardApp <- function(cachePath = "/mnt/fast/cache", since = Sys.time() - 2 * 86400, tmuxSession = NULL,
                            refreshMinutes = 2, description = paste(
                              "One card per spread fit (DEoptim): the objective of the best member, the 10th percentile",
                              "and the median of the population at each generation; lower is better. The y axis stops at",
                              "the 90th percentile, so the first, very poor generations do not flatten it. Open",
                              "'parameters by generation' for each parameter's 10th-90th percentile band (blue), median",
                              "(black) and best member (orange). Running: a generation in the last 15 min. Finished:",
                              "the fit is done and its run has moved on to the simulation.")) {
  stopifnot(requireNamespace("shiny"))
  state <- new.env()
  ui <- shiny::fluidPage(
    ## Bootstrap hides the disclosure triangle of <summary>
    shiny::tags$style("details > summary { display: list-item; cursor: pointer; color: #5d655f; }"),
    shiny::titlePanel("FireSense spread fits"),
    shiny::p(description),
    shiny::textOutput("checked"),
    shiny::tableOutput("fits"),
    shiny::uiOutput("cards"))
  server <- function(input, output, session) {
    data <- shiny::reactive({
      shiny::invalidateLater(refreshMinutes * 60 * 1000)
      fitProgress(cachePath, since, tmuxSession, state = state)
    })
    output$checked <- shiny::renderText(paste("read", format(Sys.time(), "%H:%M:%S"), "- refreshes every",
                                              refreshMinutes, "min"))
    output$fits <- shiny::renderTable({
      f <- data.table::copy(data()$fits)
      if (!nrow(f)) return(data.frame(note = "No DEoptim generations of a known ELF cached in this period."))
      f[, `:=`(started = format(started, "%m-%d %H:%M"), updated = format(updated, "%m-%d %H:%M"))]
      f[, .(ELF = elf, status, generation = gen, `best objective` = round(best, 1), started, updated)]
    })
    ## one card per fit; the parameter plot is drawn when its section is first opened (Shiny does not
    ## draw hidden outputs; "shown" makes it check again)
    output$cards <- shiny::renderUI({
      f <- data()$fits
      shiny::fluidRow(lapply(seq_len(nrow(f)), function(i) shiny::column(
        4, shiny::tags$h4(paste(f$elf[i], "-", tolower(f$status[i]), "- generation", f$gen[i])),
        shiny::plotOutput(paste0("obj_", f$fit[i]), height = "260px"),
        shiny::tags$details(ontoggle = "$(this).trigger('shown')",
                            shiny::tags$summary("parameters by generation"),
                            shiny::plotOutput(paste0("par_", f$fit[i]), height = "520px")))))
    })
    shiny::observe({
      d <- data()
      for (k in d$fits$fit) local({
        k <- k
        output[[paste0("obj_", k)]] <- shiny::renderPlot(.plotObjective(data()$gens[fit == k][order(gen)]))
        output[[paste0("par_", k)]] <- shiny::renderPlot(.plotParams(data()$params[fit == k][order(gen)]))
      })
    })
  }
  shiny::shinyApp(ui, server)
}

.plotObjective <- function(g) {
  if (!nrow(g)) return(invisible())
  ## the y range stops at the 90th percentile, so the first, very poor generations do not flatten it
  yl <- c(min(g$best), stats::quantile(c(g$med, g$q10), 0.9))
  graphics::par(mar = c(3, 3.2, 0.5, 0.5), mgp = c(1.9, 0.6, 0))
  graphics::matplot(g$gen, g[, .(best, q10, med)], type = "l", lty = c(1, 2, 1), lwd = c(2, 1.2, 1.6),
                    col = c("#b5480e", "#1d6fa5", "grey20"), ylim = yl, xlab = "generation",
                    ylab = "objective (lower is better)")
  graphics::legend("topright", c("best member", "10th percentile", "median"), lty = c(1, 2, 1),
                   col = c("#b5480e", "#1d6fa5", "grey20"), bty = "n", cex = 0.8)
}

.plotParams <- function(p) {
  if (!nrow(p)) return(invisible())
  nms <- unique(p$param)
  graphics::par(mfrow = c(ceiling(length(nms) / 2), 2), mar = c(3, 3, 2, 1), mgp = c(1.8, 0.6, 0))
  for (nm in nms) {
    q <- p[param == nm]
    graphics::plot(q$gen, q$med, type = "n", ylim = range(q$q10, q$q90, q$best), main = nm,
                   xlab = "generation", ylab = "")
    graphics::polygon(c(q$gen, rev(q$gen)), c(q$q10, rev(q$q90)), col = "#1d6fa533", border = NA)
    graphics::lines(q$gen, q$med, col = "grey20", lwd = 1.4)
    graphics::lines(q$gen, q$best, col = "#b5480e", lwd = 1.4)
    if (min(q$q10) < 0 && max(q$q90) > 0) graphics::abline(h = 0, lty = 3, col = "grey50")
  }
}
