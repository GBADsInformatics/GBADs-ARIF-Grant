fixedPage(
  
  tags$script(HTML("
  Shiny.addCustomMessageHandler('download-plotly', function(plotOutputId) {
    var el = document.getElementById(plotOutputId);
    if (el) {
      var plot = el.children[0];
      if (plot && typeof Plotly !== 'undefined') {
        Plotly.downloadImage(plot, {
          format: 'png',
          filename: 'plotly_export',
          height: 600,
          width: 800,
          scale: 2
        });
      }
    }
  });
")),
  
  # Theming -----------------------------------------------------------------
  includeCSS("styles.css"),
  useShinyjs(),
  use_googlefont("raleway"),
  use_theme(create_theme(
    theme = "default",
    bs_vars_font(
      family_sans_serif = "raleway"
    )
  )),
  
  # header ------------------------------------------------------------------
  fluidRow(
    column(
      width = 4,
      style = "padding-top: 40px;",
      div(
        style = "display: flex; flex-direction: column; align-items: flex-start;",
        img(src = "UpdatedLogoWide.png", height = "100px"),
        div(
          style = "margin-top: -10px; margin-left: 20px; display: flex; align-items: center; gap: 8px;",
          h3(textOutput("dashboardTitle"), style = "margin: 0; font-weight: bold;"),
          actionButton("editTitle", label = NULL, icon = icon("pencil-alt"), class = "edit-icon")
        ),
        div(
          style = "margin-left: 20px;",
          p(
            tags$span(style = "font-weight: bold;", "Created by: "),
            textOutput("user_name", inline = TRUE)
          )
        )
      )
    ),
    column(
      width = 8,
      style = "padding-top: 20px;",
      div(
        style = "display: flex; flex-direction: column; align-items: flex-end;",
        h4("Dashboard Details"),
        p(tags$span(style = "font-weight: bold;", "Date Created: "), "2025-05-06"),
        p(tags$span(style = "font-weight: bold;", "Model Version: "), "v1.2"),
        p(tags$span(style = "font-weight: bold;", "Model Name: "), "Ethiopia_Cattle_Run1"),
        p(tags$span(style = "font-weight: bold;", "Species: "), "Cattle"),
        div(
        actionButton("configure_db", "Configure", style = "font-family: Raleway; font-weight: bold; background-color: #FFF; color: #F7931D; border: 1px solid #F7931D;"),
        actionButton("save_db", "Save Dashboard", style = "font-family: Raleway; font-weight: bold; background-color: #F7931D; color: #fff")
        )
      )
    )
  ),
  hr(),
  
  # main content ------------------------------------------------------------
  
  br(style = "height: 30px;"),
  
  withSpinner(
    uiOutput("dynamicUI"),
    type = 6,  # You can change spinner style 1–8
    color = "#F7931D"
  ),
  
  # footer ------------------------------------------------------------------
  tags$footer(
    tags$hr(),
    div(class = "footer-content",
        tags$div(
          tags$img(src = "logoFull.png", height = "70px"),
          br(),
          tags$a(href = "https://animalhealthmetrics.org", "GBADs Home", target = "_blank"),
          " | ",
          tags$a(href = "https://gbadske.org", "GBADs Informatics", target = "_blank"),
          " | ",
          tags$a(href = "https://animalhealthmetrics.org/about-us-2", "About", target = "_blank"),
          " | ",
          tags$a(href = "https://animalhealthmetrics.org/contact", "Contact", target = "_blank"),
          br(),
          HTML("&copy; 2025 GBADs Informatics"),
          br(),
          tags$a(href = "https://opensource.org/licenses/MIT", 
                 "MIT Licensed", target = "_blank", class = "footer-link"),
          " | ",
          tags$a(
            href = "https://github.com/your-repo",
            target = "_blank",
            class = "footer-link",
            icon("github"),
            "Source Code"
          )
        )
    )
  )
)
