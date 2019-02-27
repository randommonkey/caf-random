

dataCaf <- read_csv('data/clean/movilidad_latam_data.csv')
dataCaf <- dataCaf %>% select(-`fleet_buses.3`)
dicCaf <- read_csv('data/clean/movilidad_latam_dic_.csv') %>% drop_na(grupo)
dicCaf <- dicCaf %>% filter(id != 'fleet_buses.3')
dicCaf$Unidad[dicCaf$Unidad == 'Porcentaje'] <- '%'
dicCaf$Unidad[dicCaf$Unidad == 'Número'] <- ''
dicCaf$Unidad[dicCaf$Unidad == '-'] <- ''
dicCaf$Unidad[is.na(dicCaf$Unidad)] <- ''
dicCaf <- dicCaf %>% filter(ctypes == 'Num')
dataCaf <- dataCaf[, c('pais', 'ciudad',dicCaf$id)]
mapLam <- jsonlite::fromJSON("data/latin-america.json", simplifyVector = FALSE)
codigos <- read_csv('data/clean/codigos.csv')

dicFicha <- dicCaf %>% filter(ficha == 'SI')

options(scipen=999)

count_pl <- function(x) {
  if(is.na(x)){return(0)}
  
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

paleta <- c('#5c9ebc', '#73ca96', '#a0e13a', '#d3a6a2', '#59242d', '#733571', '#f9b233', '#bb8526')

caf_theme <- hc_theme(
  colors = paleta,
  chart = list(
    backgroundColor = "transparent",
    style = list(
      fontFamily= 'Open Sans',
      textOutline= FALSE,
      textShadow = FALSE,
      textDecoration = 'none',
      color='#fff',
      whiteSpace = "nowrap"
    )
  ),
  xAxis= list(
    title= list(
      style= list(
        color= '#fff'
      )
    ),
    labels= list(
      style = (
        color = '#fff'
      )
    )),
  yAxis= list(
    title= list(
      style= list(
        color= '#fff'
      )
    ),
    # minorTickInterval= 'auto',
     lineColor= '#fff',
     lineWidth= 1,
     tickWidth= 1,
    # tickColor= 'red',
    labels= list(
      style = (
        color = '#fff'
)
    )),
  legend = list(
    itemStyle = list(
      color = '#fff'
    )
  )
)


hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- "."
hcoptslang$decimalPoint <- ","
options(highcharter.lang = hcoptslang)

shinyServer(function(input, output, session) {

   output$botonesT <- renderUI({
    temas <- c('Movilidad', 'Seguridad vial', 'Infra - estructura',
               'Gestión del tránsito', 'Socio - económico', 'Patrimonio', 
               'Flota transporte colectivo', 'Flota transporte individual', 
               'Costos de infra - estructura', 'Costos de viaje', 'Tarifas, recaudo y subsidios - TC',
               'Recursos transporte colectivo', 'Operación transporte colectivo', 'Consumo energía')#sort(unique(dicCaf$grupo_mod))
    
   l <- purrr::map(temas, function(z){
        HTML( paste0('<div class = "contMenu">',
        tags$button(id = z, class = 'butTemas', type = "button", z
            ),
        ' <div class = "dropdownMenuFicha">
	        <div class = "dropdownMenuContent">',
	    	paste('<a id = "',dicCaf$id[dicCaf$grupo_mod == z], '" href = "#" class = "itemID">', dicCaf$label[dicCaf$grupo_mod == z] ,'</a>', collapse = ''), '</div></div>
           </div>'
            ))
        })
   
   HTML(paste0('<div class = "contBotones">', paste(l, collapse = ''), '</div>'))
   })
   

  
   
   dataBubble <- reactive({
     varS <- trimws(input$last_btn)
     
     datM <- dataCaf %>% left_join(codigos)
     
     
     if(is.null(varS) | identical(varS, character(0))) {
       df1 <- datM %>% select(name = ciudad, lat, lon) %>% drop_na()
       df1$z <- 1
       df1$w <- 'Elegir una variable o da click para ver ficha'
       df1$label <- 'Puedes'
     } else {
       df1 <- datM %>% select(name = ciudad, lat, lon, z = varS) %>% drop_na()
       df1$label <- dicCaf$label[dicCaf$id == varS]
       if (varS == 'tarifa_minimo') {
       df1$w <- map_chr(df1$z, function(x) format(round(x,4), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=".", decimal.mark = ",")) 
       } else {
       df1$w <- map_chr(df1$z, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=".", decimal.mark = ","))
       }
     }
     
     df1
   })
   
   output$tabala <- renderPrint({
     dataBubble()
   })
   
   dataSerie <- reactive({
     df0 <- dataCaf %>% left_join(codigos) %>% select(name = country_name) %>% distinct()
     df0$z <- 1
     df0
   })
   
   output$Mapa <- renderHighchart({
     
     myClickFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.name, timestamp: new Date().getTime()});}")

     
     highchart(type = "map") %>%
       hc_chart(backgroundColor = "transparent", 
                style = list(
                  fontFamily= 'Open Sans'
                )) %>%
       hc_add_series_map(map = mapLam, showInLegend = FALSE, nullColor = "#70a6ca",
                         borderWidth = 1, borderColor = '#fff',#'#466575',
                         df = dataSerie(),  value = "z", joinBy = "name",
                         allowPointSelect = TRUE,
                         tooltip= list(
                           headerFormat= '',
                           pointFormat='<b>{point.name}</b>'
                         )) %>%
       hc_colorAxis(maxColor = "#70a6ca", minColor = "#70a6ca") %>% 
       hc_legend(enabled = FALSE) %>% 
       hc_add_series(data = dataBubble(), type = "mapbubble",
                     dataLabels= list(
                       enabled= TRUE,
                       color= '#000',
                       format = '{point.name}',
                       style= list(
                         fontWeight = 'bold',
                         textShadow = FALSE,
                         fontSize = '13px',
                         fontFamily = 'Open Sans',
                         textOutline = FALSE#,
                         #color = '#4CB8FF'
                       )
                       ),
                     allowPointSelect = TRUE,
                     cursor = 'pointer', minSize = ifelse(mean(dataBubble()$z) == 1, '0.5%', '1%'),
                     maxSize = ifelse(mean(dataBubble()$z) == 1, 0.5, 50),
                     marker= list(
                       fillOpacity=0.8),
                     color = "#f7af2d", events = list(click = myClickFunc),
                     tooltip= list(
                       headerFormat= '',
                       pointFormat='<b>{point.name}</b><br>
                                    <b>{point.label}: </b>{point.w}</>'
                     )) %>% 
       hc_mapNavigation(enabled = TRUE,
                        buttonOptions= list(
                          align = 'right',
                          fill = 'aqua'
                        ))
     
   })
   
   
   output$descripcion <- renderUI({
     
     varS <- trimws(input$last_btn)
     if(is.null(varS) | identical(varS, character(0))) return()
  
      HTML(paste0(
        '<span style="color:#509f27;font-size: 18px;font-weight: 600;">',dicCaf$grupo[dicCaf$id == varS],'</span>', 
        '<p style="color:#000;margin-top: 10px;"><b>', dicCaf$label[dicCaf$id == varS], ': </b>',
        dicCaf$Descripción[dicCaf$id == varS] ,'</p><p><b>'
      ))
   })
   
   
   ciuID <- reactive({
     input$hcClicked$id
   })
   
   

   lala <- reactiveValues(paisID = NULL)

   observe({
     ciudEl <- ciuID()
     codigos <- codigos %>% fill(FIPS_CNTRY)
     codigos$FIPS_CNTRY <- tolower(codigos$FIPS_CNTRY)
     code <- codigos$FIPS_CNTRY[codigos$ciudad == ciudEl]

     lala$paisID <- paste0("countries/", code, "/", code, "-all")
   })

    
   output$contenidoF <- renderUI({
     HTML(paste0('<ul><li>', unique(dicFicha$grupo)  ,'</li></ul>'))
   })
   
   
   datGen <- reactive({
     datC <- dataCaf %>% filter(ciudad == input$hcClicked$id)
     datC <- Filter(function(x) !all(is.na(x)), datC)
     varInf <- data.frame( id = names(datC))
     dicG <- varInf %>% inner_join(dicCaf)
     dicG
   })

   datSocEc <- reactive({
     
     ind <- datGen()[datGen()$grupo == 'Socioeconómico',]
     ciudEl <- ciuID()
     datF <- ind %>% filter(ficha == 'SI')
 
     if (nrow(datF) == 0) {
       df <- ind
       d <- df$id
       d <- dataCaf[, c('pais', 'ciudad', d)]
       d <- d %>% filter( ciudad == ciudEl)
       d <- d %>%
              gather( variable, valor, -pais, -ciudad) %>%
                  drop_na()
       dataC <- d %>% select(id = variable, valor)
       a <- dataC %>% left_join(df)
     } else {
       df <- dicFicha[dicFicha$grupo == 'Socioeconómico',]
       d <- df$id
       DataSe <- dataCaf[, c('pais', 'ciudad', d)]
       DataSe <- DataSe %>%
         gather( variable, valor, -pais, -ciudad) %>%
         drop_na()
       dataC <- DataSe %>% filter(ciudad == ciudEl) %>% select(id = variable, valor)
       a <- dataC %>% left_join(df)
     }
     a
   })
   
   
   output$socEcon <- renderUI({

     dataC <- datSocEc()
     dataC$valor <- map_chr(dataC$valor, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=".", decimal.mark = ","))

     if (nrow(dataC) == 0) return()
     
     HTML(paste0('<table>
                     <tr><th class = "colTabDesc" scope="row">',dataC$label, '</th> <td class = "colTabValr">',
                 dataC$valor, ' <b>', dataC$Unidad, '</b> (', dataC$Descripción,')','</td></tr></table></br>'))
   })
   
 
   
   datMov <- reactive({
     ind <- grepl('modal_', datGen()$id)
     sumIn <- sum(ind)
     ciudEl <- ciuID()
     dicG <- datGen()
     rem <- grep('Total|total', dicG$label)
     dicG <- dicG[-rem,]
     
     if (sumIn == 0) {
       dicM <- dicG %>% filter(grupo == 'Movilidad')
       unMov <- sample(unique(dicM$Unidad),1)
       dicM <- dicM[dicM$Unidad == unMov,]
       d <- dataCaf[, c('pais', 'ciudad', dicM$id)]
       d <- d %>%
              gather(id, valor, -ciudad, -pais) %>%
                drop_na(valor) %>% filter(ciudad == ciudEl) %>% select(id, valor)
       d <- d %>% inner_join(dicM) %>% select(variable = label, valor)
       d$valor <- round(d$valor, 2)
     } else {
       d <- dataCaf[, c('pais', 'ciudad', dicFicha$id[dicFicha$grupo == 'Movilidad'])]
       d <- d %>%
              gather(id, valor, modal_motorized:modal_transport.1) %>%
                drop_na(valor) %>% filter(ciudad == ciudEl) %>% select(id, valor)
       d <- d %>% inner_join(dicFicha) %>% select(variable = label, valor)
     }

     d

   })
   
  
   output$DespMov <- renderUI({
     varE <- datMov()$variable[1]
     texto <- dicCaf$des_Alt[dicCaf$label == varE]
     HTML(paste0('<p style = "font-size:13px;">', texto, '</p>'))
   })
   
   output$movElg <- renderHighchart({
     ind <- grepl('modal_', datGen()$id)
     sumIn <- sum(ind)
     dataC <- datMov()
     
     if (sumIn == 0){
       viz <- hgchmagic::hgch_bar_CatNum(dataC, theme = caf_theme, horLabel = ' ', verLabel = ' ', tooltip = list(headerFormat = NULL, pointFormat = '<b>{point.a}:</b> {point.b}'), orientation = 'hor')
     } else {
       viz <- highchart() %>% 
         hc_add_series_labels_values(dataC$variable, dataC$valor, type = "pie",
                                     name = "Bar", colorByPoint = TRUE,
                                     showInLegend= TRUE,
                                     size = 150, dataLabels = list(enabled = FALSE)) %>% 
         hc_legend(
           align= 'right',
           layout= 'vertical',
           verticalAlign= 'top',
           x= -100,
           y= 70
         ) %>% 
         hc_tooltip(headerFormat = "", 
                    pointFormat = "<b>{point.name}</b>: {point.y}%", 
                    followPointer = TRUE, shared = TRUE) %>% 
          hc_add_theme(custom_theme(custom = caf_theme))
     }
     viz
   })
   
   
   datSeg <- reactive({
     ind <- datGen()[datGen()$grupo == 'Seguridad vial',]
     ciudEl <- ciuID()
     datF <- ind %>% filter(ficha == 'SI')
     if (nrow(datF) == 0) {
       df <- ind
       d <- sample(df$id, 1)
       d <- dataCaf[, c('pais', 'ciudad', d)]
       paisEl <- unique(d$pais[d$ciudad == ciudEl])
       a <- d %>% filter(pais == paisEl) %>% select(-pais) %>% drop_na()
       a[,2] <- round(a[,2], 2)
     } else {
       DataS <- dataCaf[, c('pais', 'ciudad', dicFicha$id[dicFicha$grupo == 'Seguridad vial'])]
       DataS <- DataS %>% drop_na()
       paisEl <- unique(DataS$pais[DataS$ciudad == ciudEl])
       a <- DataS %>% filter(pais == paisEl) %>% select(-pais) 
       a[,2] <- round(a[,2], 2)
     }
     a
   })

   output$DesVial <- renderUI({
     dataP <- datSeg()
     labVar <- dicCaf$Descripción[dicCaf$id == names(dataP)[2]]
     HTML(paste0('<p style = "font-size:13px;">', labVar, '</p>'))
   })
   
   output$segElg <- renderHighchart({
     dataP <- datSeg()
     
     labVar <- dicCaf$label[dicCaf$id == names(dataP)[2]]
     
     if (is.null(labVar)) return()
     
     hgch_bar_CatNum(dataP, horLabel = labVar, orientation = 'hor', sort = 'desc', theme = caf_theme,  highlightValue =  ciuID() , tooltip = list(pointFormat = paste0("<b>{point.a}</b><br/><b>", labVar, ": </b>{point.b}")))
   })
   
  
   datTrans <- reactive({
     ind <- datGen()[datGen()$grupo == 'Tarifas, recaudo y subsidios - TC',]
     ciudEl <- ciuID()
     datF <- ind %>% filter(ficha == 'SI')
     if (nrow(datF) == 0) {
       df <- ind
       df <- dataCaf[, c('pais', 'ciudad', df$id[1])]
       df <- df %>% filter(ciudad == ciudEl) 
       df <- df %>%
               gather(id, valor, -ciudad, -pais) %>%
                 drop_na()
       df <- df %>% left_join(dicCaf)
       df <- df %>% select(label, valor)
     } else {
       df <- dataCaf[, c('pais', 'ciudad', dicFicha$id[dicFicha$grupo == 'Tarifas, recaudo y subsidios - TC'])]
       df <- df %>% filter(ciudad == ciudEl)
       df <- df %>%
                   gather(id, valor, -ciudad, -pais) %>%
                     drop_na()
       df <- df %>% left_join(dicFicha)
       df <- df %>% select(label, valor)
     }
     df
   })
   
   output$DesTrans <- renderUI({
     dataP <- datTrans()
     labVar <- dicCaf$des_Alt[dicCaf$label == dataP$label[1]]
     HTML(paste0('<p style = "font-size:13px;">', labVar, '</p>'))
   })
   
 
   output$transporte <- renderHighchart({
     Dg <- datTrans()
     hgch_bar_CatNum(Dg, theme = caf_theme, horLabel = ' ', tooltip = list(pointFormat = paste0("<b>{point.a}</b><br/><b>: </b>{point.b}")))
   })

   
   
   paisSel <- reactive({
     idCiu <- input$hcClicked$id
     idCiu
     #codigos$ggch[codigos$ciudad == idCiu]
   })
   
   output$mapaFicha <- renderUI({
     
 HTML(paste0('<img src = "imgMp/', paisSel() ,'.png" style = "width: 100%;">'))

   })
   
  output$ciudSel <- renderUI({
    div(style = 'font-size:19px;margin-top: 25px;',
    paste0('Perfil de ', input$hcClicked$id)
    )
  })
   
   output$fichaCiudad <- renderUI({

     htmlTemplate("templates/template.html",
                  grafico = uiOutput('mapaFicha'),
                  contenidoFicha = uiOutput('contenidoF'),
                  ciudadP = uiOutput('ciudSel'),
                  TextMovil = uiOutput('DespMov'),
                  Movilidad = highchartOutput('movElg', height = 200),
                  TextSegur = uiOutput('DesVial'),
                  Seguridad = highchartOutput('segElg', height = 300),
                  Socioeconomico = uiOutput('socEcon'),
                  TextTransp = uiOutput('DesTrans'),
                  Transporte = highchartOutput('transporte', height = 300)
     )
   })
   # 
   # 
   # 
   

   observeEvent(input$hcClicked, {
     id = input$hcClicked$id
     showModal(modalDialog(
       title = '',
       easyClose = TRUE,
       footer = modalButton("Cerrar"), 
       uiOutput('fichaCiudad'), 
       br()
     )
     )
   })
   
   
   output$siglasMapa <- renderUI({
     
     if (mean(dataBubble()$z) == 1) return()
     
     varS <- trimws(input$last_btn)
     if(is.null(varS)) varS <- "population_city"
     if(identical(varS, character(0))) varS <- "population_city"
     a <- HTML(dicCaf$Unidad[dicCaf$id == varS])
     if (is.na(a)) a <- ''
     
     maxValue <- unique(dataBubble() %>%  filter(z == max(dataBubble()$z)) %>%  .$w)
     minValue <- unique(dataBubble() %>%  filter(z == min(dataBubble()$z)) %>%  .$w)
     
     if (maxValue == minValue) {
      div(class = 'ContSigla',
                div(class = 'circulo', ''),
                HTML(paste0('<span>', maxValue, ' <span>', a, '</span></span>'))
       )
     } else {
     div(class = 'ContSigla',
     div(class = 'circulo', ''),
     HTML(paste0('<span>', maxValue, ' <span>', a, '</span></span>')),
     div(class = 'circuloPequ', ''),
     HTML(paste0('<span>', minValue, ' <span>', a, '</span></span>'))
     )
     }
     
     
   })
   
   output$bla <- renderPrint({
     datTrans()
   })
   
   
  })