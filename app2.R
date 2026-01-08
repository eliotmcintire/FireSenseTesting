
library(shiny)

# --- Publish 'outputs' so /outputs/... URLs are served by Shiny ------------
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
outputs_root <- file.path(project_root, "outputs")
addResourcePath("outputs", outputs_root)  # publish static files under /outputs  [Shiny doc]  [2](https://anylearn.ai/guide/mastering-google-docs-api-text-insertion-techniques)

# --- Dynamic filename resolver (your intent, made robust) -------------------
dynamic_pngs <- function() {
  # All candidate PNGs under outputs that contain "iter" and "objFun"
  dd <- dir("outputs/", recursive = TRUE, pattern = ".*iter.*.png")
  elfs <- unique(sapply(strsplit(dd, "/"), function(x) x[[1]][[1]]))
  ff <- Map(elf = elfs, function(elf) {
    rr <- dir(file.path("outputs", elf), recursive = TRUE, full.names = TRUE) |>
      grep(pattern = "objFun", value = TRUE)
    fi <- file.info(rr)
    fi2 <- fi[order(fi[, "mtime"]),]
    rownames(tail(fi2, 1))
  })
  pngs <- unlist(unname(ff))
  fi <- file.info(pngs)
  fi2 <- fi[order(fi[, "mtime"], decreasing = TRUE),]
  ff <- rownames(fi2)
  unname(unlist(ff))
}

# Caption helper: second path element (e.g., "14.1" from "outputs/14.1/...")
second_segment <- function(rel_path) {
  m <- sub("^outputs/([^/]+)/.*$", "\\1", rel_path)
  if (identical(m, rel_path)) "" else m
}

# --- UI ---------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: system-ui, sans-serif; margin: 0; padding: 0 10px; }
    .grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); grid-gap: 8px; }
    .cell {
      border: 1px solid #ddd;
      padding: 10px;
      background: #fafafa;
      display: flex;
      flex-direction: column;
      align-items: center; /* center caption and image horizontally */
    }
    .cell .caption {
      font-size: 18px;     /* larger font */
      font-weight: 600;    /* bolder */
      text-align: center;  /* centered text */
      margin-bottom: 8px;  /* space between caption and image */
    }
    .cell img {
      width: 300px;        /* fixed width as requested */
      height: auto;
      display: block;
      object-fit: contain; /* keep full image visible */
    }
  "))),
  titlePanel("PNG Gallery"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      sliderInput("refresh", "Auto-refresh (sec)", min = 5, max = 120, value = 30) # or remove if you want fixed cadence
    ),
    mainPanel(width = 10, uiOutput("grid"))
  )
)

# --- Server: dynamically poll for new/updated files -------------------------
server <- function(input, output, session) {
  
  files_rx <- reactivePoll(
    intervalMillis = function() input$refresh * 1000,       # ✅ correct
    session = session,
    checkFunc = function() {
      # Cheap change signature: list + mtimes; re-render only if this string changes
      fls <- dir("outputs", recursive = TRUE, pattern = "iter.*\\.png$", full.names = TRUE)
      paste(fls, if (length(fls)) file.info(fls)$mtime else "")
    },
    valueFunc = function() {
      dynamic_pngs()  # returns "outputs/…/file.png" for latest per first-level segment
    }
  )
  
  output$grid <- renderUI({
    rel_paths <- files_rx()               # "outputs/…/file.png"
    if (!length(rel_paths)) return(tags$p("No PNGs (yet)."))
    
    urls <- paste0("/", rel_paths)        # "/outputs/…/file.png" for <img src="">
    tags$div(class = "grid", lapply(seq_along(urls), function(i) {
      u   <- urls[i]
      cap <- second_segment(rel_paths[i])
      tags$div(class = "cell", list(
        tags$div(class = "caption", cap),                    # label at top, centered
        tags$a(href = u, target = "_blank",
               tags$img(src = paste0(u, "?cb=", as.integer(Sys.time()))))
      ))
    }))
  })
}

shinyApp(ui, server, options = list(port = 8080, host = "0.0.0.0"))
