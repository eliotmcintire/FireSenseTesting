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

library(shiny)   # Shiny framework (docs: https://shiny.posit.co/)
# httpuv is pulled by shiny for serving HTTP (CRAN: https://cran.r-project.org/package=httpuv)

if (FALSE) {
# 1) Resolve absolute path to the *published root* ("outputs")
#    Adjust 'project_root' to your actual path if needed.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
outputs_root <- file.path(project_root, "outputs")

# 2) Publish that directory under URL prefix "outputs"
#    Now /outputs/... will be served by Shiny from outputs_root/.
addResourcePath("outputs", outputs_root)
# Docs: addResourcePath maps a URL prefix to a filesystem directory. [1](https://shiny.posit.co/r/reference/shiny/latest/resourcepaths.html)

# Helper: keep only files that exist and convert to URL under /outputs/
existing_png_urls <- function() {
  rels <- pngs[file.exists(file.path(project_root, pngs))]
  # convert each "outputs/12.2/..." to "/outputs/12.2/..."
  paste0("/", rels)
}

ui <- fluidPage(
  # tags$head(tags$style(HTML("
  #   body { font-family: system-ui, -apple-system, Segoe UI, Roboto, sans-serif; margin: 0; padding: 0 10px; }
  #   .grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(400px, 1fr)); grid-gap: 8px; }
  #   .cell { border: 1px solid #ddd; padding: 6px; background: #fafafa; }
  #   .cell img { width: 100%; height: auto; display:block; }
  #   .caption { font-size: 12px; color:#444; margin-top:4px; word-break: break-all; }
  # "))),
  # titlePanel("PNG Gallery"),
  # sidebarLayout(
  #   sidebarPanel(
  #     width = 2,  # ← narrower sidebar (2/12 of page width)
  #     # sliderInput("thumb",  "Thumbnail width (px)",  min = 100, max = 600, value = 160),
  #     # sliderInput("thumb_h","Thumbnail height (px)", min = 100, max = 600, value = 160),
  #     sliderInput("refresh", "Auto-refresh (seconds)", min = 10, max = 120, value = 30)
  #   ),
  #   mainPanel(
  #     width = 10,  # ← remaining width (12 - sidebar width)
  #     uiOutput("grid")
  #   )
  # )
  # --- in your UI head (replace the old CSS block or append these rules) ---
  tags$head(tags$style(HTML("
  body { font-family: system-ui, sans-serif; margin: 0; padding: 0 10px; }
  .grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); grid-gap: 8px; }
  .cell {
    border: 1px solid #ddd;
    padding: 10px;           /* a bit more padding */
    background: #fafafa;
    display: flex;           /* stack caption and image vertically */
    flex-direction: column;
    align-items: center;     /* center child elements horizontally */
  }
  .cell .caption {
    font-size: 18px;         /* larger font */
    font-weight: 600;        /* bolder */
    text-align: center;      /* centered text */
    margin-bottom: 8px;      /* spacing between caption and image */
  }
  .cell img {
    width: 300px;            /* fixed width as requested */
    height: auto;
    display: block;
    object-fit: contain;
  }
")))
)






server <- function(input, output, session) {
  
  output$grid <- renderUI({
    invalidateLater(input$refresh * 1000, session)
    
    urls <- existing_png_urls()
    if (!length(urls)) return(tags$p("No PNGs (yet)."))
    
    
    tags$div(class = "grid", lapply(seq_along(urls), function(i) {
      u <- urls[i]
      tags$div(class = "cell", list(
        # TOP caption: second path element like "14.1"
        tags$div(class = "caption", strsplit(pngs[i], "/")[[1]][2]),
        # Image
        tags$a(href = u, target = "_blank",
               tags$img(src = paste0(u, "?cb=", as.integer(Sys.time()))))
      ))
    }))
    
    # tags$div(class = "grid", lapply(seq_along(urls), function(i) {
    #   u <- urls[i]
    #   tags$div(class = "cell", list(
    #     tags$a(href = u, target = "_blank",
    #            tags$img(src = paste0(u, "?cb=", as.integer(Sys.time())),
    #                     style = sprintf("width:%dpx;", input$thumb))),
    #     # tags$div(class = "caption", pngs[i])
    #     tags$div(class = "caption", strsplit(pngs[i], "/")[[1]][2])
    #   ))
    # }))
  })
}

shinyApp(ui, server, options = list(port = 8080, host = "0.0.0.0"))

}


# --- Resolve & publish the outputs directory so Shiny can serve files ---
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
outputs_root <- file.path(project_root, "outputs")
addResourcePath("outputs", outputs_root)  # publish static files under /outputs/  (Ref: Shiny addResourcePath)  [1][2]

# --- Helper: existing PNG URLs (as /outputs/...) ---
existing_png_urls <- function() {
  rels <- pngs[file.exists(file.path(project_root, pngs))]
  paste0("/", rels)  # convert to URL (e.g., /outputs/12.2/...)
}

# --- Safe caption: "second element of the path" after 'outputs/'
# Returns "14.1" for "outputs/14.1/figures/.../file.png"; "" if not present.
second_segment <- function(rel_path) {
  # Expect rel_path like "outputs/<seg2>/..."
  m <- sub("^outputs/([^/]+)/.*$", "\\1", rel_path)
  if (identical(m, rel_path)) "" else m
}

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
      object-fit: contain; /* keep full image visible; no cropping */
    }
  "))),
  titlePanel("PNG Gallery"),
  mainPanel(uiOutput("grid"))
)

server <- function(input, output, session) {
  
  output$grid <- renderUI({
    invalidateLater(30000, session)  # refresh every 30s
    
    urls <- existing_png_urls()
    if (!length(urls)) {
      cat("[Gallery] No existing PNGs found under", outputs_root, "\n")
      return(tags$p("No PNGs (yet)."))
    }
    
    # Build cards
    tags$div(class = "grid", lapply(seq_along(urls), function(i) {
      u <- urls[i]
      rel <- pngs[i]                   # original relative path (e.g., "outputs/14.1/.../file.png")
      cap <- second_segment(rel)       # "14.1" etc.
      # Logging for troubleshooting
      cat(sprintf("[Gallery] i=%d url=%s caption=%s exists=%s\n",
                  i, u, cap, file.exists(file.path(project_root, rel))))
      tags$div(class = "cell", list(
        # caption ON TOP
        tags$div(class = "caption", cap),
        # image
        tags$a(href = u, target = "_blank",
               tags$img(src = paste0(u, "?cb=", as.integer(Sys.time()))))
      ))
    }))
  })
}

shinyApp(ui, server, options = list(port = 8080, host = "0.0.0.0"))
