radioImages <- function(inputId, images, values, texts = NULL) {
  # If texts is NULL or shorter, fill with empty strings
  if (is.null(texts)) texts <- rep("", length(images))
  if (length(texts) < length(images)) {
    texts <- c(texts, rep("", length(images) - length(texts)))
  }
  
  radios <- lapply(
    seq_along(images),
    function(i) {
      id <- paste0(inputId, i)
      tagList(
        tags$input(
          type = "radio",
          name = inputId,
          id = id,
          class = "input-hidden",
          value = as.character(values[i])
        ),
        tags$label(
          `for` = id,
          style = "display: inline-block; text-align: center; cursor: pointer; margin: 10px;",
          tags$img(
            src = images[i],
            style = "
              height: 90px;
              background-color: white;
              border: 2px solid transparent;
              border-radius: 10px;
              transition: border-color 0.3s ease;
              display: block;
              margin: 0 auto 5px auto;
            ",
            class = "radio-icon"
          ),
          tags$div(
            style = "font-size: 14px; color: #333;",
            texts[i]
          )
        )
      )
    }
  )
  
  div(
    class = "shiny-input-radiogroup", 
    id = inputId, 
    style = "text-align:center;",
    radios,
    
    tags$style(HTML("
      .input-hidden {
        position: absolute !important;
        left: -9999px !important;
      }
    ")),
    
    tags$script(HTML(sprintf("
      function updateBorders%s() {
        var container = document.getElementById('%s');
        var inputs = container.querySelectorAll('input[type=radio]');
        var images = container.querySelectorAll('img.radio-icon');
        inputs.forEach(function(input, i) {
          if(input.checked) {
            images[i].style.borderColor = '#F7931D';
          } else {
            images[i].style.borderColor = 'transparent';
          }
        });
      }
      document.addEventListener('DOMContentLoaded', updateBorders%s);
      document.getElementById('%s').addEventListener('change', updateBorders%s);
    ", inputId, inputId, inputId, inputId, inputId)))
  )
}