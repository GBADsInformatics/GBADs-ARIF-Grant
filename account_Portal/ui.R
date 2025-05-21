fluidPage(
  includeCSS("styles.css"),
  use_googlefont("raleway"),
  useShinyjs(),
  use_theme(create_theme(
    theme = "default",
    bs_vars_font(
      family_sans_serif = "raleway"
    )
  )),
  
  setBackgroundColor(color = "#FFF"),
  
  div(class = "page-wrapper",
      
      # Main content
      div(class = "content",
          br(),
          uiOutput("dynamicContent")
      ),
      
      # Footer
      tags$footer(
        tags$hr(),
        div(class = "footer-content",
            # Left: Logo
            tags$div(class = "footer-left",
                     tags$img(src = "logo.png", height = "70px")
            ),
            
            # Center: Links
            tags$div(class = "footer-center",
                     tags$a(href = "https://animalhealthmetrics.org", "GBADs Home", target = "_blank"),
                     " | ",
                     tags$a(href = "https://gbadske.org", "GBADs Informatics", target = "_blank"),
                     " | ",
                     tags$a(href = "https://animalhealthmetrics.org/about-us-2", "About", target = "_blank"),
                     " | ",
                     tags$a(href = "https://animalhealthmetrics.org/contact", "Contact", target = "_blank")
            ),
            
            # Right: Copyright
            tags$div(class = "footer-right",
                     HTML("&copy; 2025 GBADs Informatics"),
                     tags$br(),
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
)
