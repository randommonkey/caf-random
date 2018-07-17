
shinyUI(
  fluidPage(
    useShinyjs(),
   # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
    #                 tags$img(src = 'Cargando.gif', class="loadmessage")),
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="style.css"),
      includeScript("js/iframeSizer.contentWindow.min.js"),
      includeScript("js/caf.js")
    ),
   div(class = 'contCaf',
       div(class = 'latStyle',
       uiOutput('botonesT'),
       uiOutput('descripcion')),
       div(class = 'mapaStyle',
       highchartOutput('Mapa', width = 750, height = 575),
       uiOutput('siglasMapa')
       )#,
       #verbatimTextOutput('tabala')
       )
  )
)


