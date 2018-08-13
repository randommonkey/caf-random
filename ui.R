
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
   HTML('<h5 style="margin-left:3%;font-weight: 600;">Este portal integra múltiples bases de datos que permiten entender 
        las problemáticas de movilidad en América Latina y aportar al 
        desarrollo de políticas sostenibles.</h2>'),
   div(class = 'col-xs-12 contCaf',
       div(class = 'latStyle',
       uiOutput('botonesT'),
       uiOutput('descripcion')),
       div(class = 'mapaStyle',
       highchartOutput('Mapa', width = 700, height = 611),
       uiOutput('siglasMapa')#,
       #verbatimTextOutput('bla')
       )#,
       #verbatimTextOutput('tabala')
       )
  )
)


