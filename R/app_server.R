#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny rlang dplyr
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  data_operations <- reactiveVal()
  data_csi <- reactiveVal()
  data_cb <- reactiveVal()
  data_dette <- reactiveVal()
  data_pib <- reactiveVal()

  data_operations(as.data.table(read_parquet("data/operations.parquet")))
  data_csi(as.data.table(read_parquet("data/csi.parquet")))
  data_cb(as.data.table(read_parquet("data/cb.parquet")))
  data_dette(as.data.table(read_parquet("data/dette.parquet")))
  data_pib(as.data.table(read_parquet("data/pib.parquet")))


observe({
  req(data_operations())
  req(data_pib())
  if(is.null(data_csi()) | is.null(data_operations()) | is.null(data_cb()) | is.null(data_dette()) | is.null(data_pib())){
    return(NULL)
  } else{
    updatePickerInput(session = session,inputId = "operations_secteur",choices = unique(data_operations()[,sect_inst]),
                      selected = "SO")
    updatePickerInput(session = session,inputId = "operations_operation",choices = unique(data_operations()[,operation]),
                      selected = "P3M")
    updatePickerInput(session = session,inputId = "operations_produit",choices = unique(data_operations()[,cna_produit]),
                      selected = c("A17-C4","A17-DE"))
    updatePickerInput(session = session,inputId = "operations_valorisation",choices = unique(data_operations()[,valorisation]),
                      selected = "L")


    updatePickerInput(session = session,inputId = "csi_secteur",choices = unique(data_csi()[,sect_inst]))
    updatePickerInput(session = session,inputId = "csi_compte",choices = unique(data_csi()[,compte]))
    updatePickerInput(session = session,inputId = "csi_operation",choices = unique(data_csi()[,operation]))
    updatePickerInput(session = session,inputId = "csi_correction",choices = unique(data_csi()[,correction]))

    updatePickerInput(session = session,inputId = "cb_operation",choices = unique(data_cb()[,operation]))
    updatePickerInput(session = session,inputId = "cb_produit",choices = unique(data_cb()[,cna_produit]))
    updatePickerInput(session = session,inputId = "cb_valorisation",choices = unique(data_cb()[,valorisation]))
    updatePickerInput(session = session,inputId = "cb_unite",choices = unique(data_cb()[,unit_measure]))
    updatePickerInput(session = session,inputId = "cb_correction",choices = unique(data_cb()[,correction]))

    updatePickerInput(session = session,inputId = "dette_indicateur",choices = unique(data_dette()[,indicateur]))
    updatePickerInput(session = session,inputId = "dette_secteur",choices = unique(data_dette()[,sect_inst]))
    updatePickerInput(session = session,inputId = "dette_instruments",choices = unique(data_dette()[,dette_maastricht_intruments]))

    updatePickerInput(session = session,inputId = "pib_secteur",choices = unique(data_pib()[,sect_inst]))
    updatePickerInput(session = session,inputId = "pib_operation",choices = unique(data_pib()[,operation]))
    updatePickerInput(session = session,inputId = "pib_nature",choices = unique(data_pib()[,nature]))
    updatePickerInput(session = session,inputId = "pib_valorisation",choices = unique(data_pib()[,valorisation]))
    updatePickerInput(session = session,inputId = "pib_unit",choices = unique(data_pib()[,unit]))
    updatePickerInput(session = session,inputId = "pib_correction",choices = unique(data_pib()[,correction]))

  }
})

# OPERATIONS

# Function to update dependent inputs
update_dependent_inputs <- function() {
  if(!is.null(input$operations_secteur) | !is.null(input$operations_operation) |
     !is.null(input$operations_produit) | !is.null(input$operations_valorisation)){
  filtered_data <- data_operations()

  # 1 nul
  if (!is.null(input$operations_secteur) & is.null(input$operations_operation) &
      is.null(input$operations_produit) & is.null(input$operations_valorisation)) {
    filtered_data <- filtered_data[sect_inst %in% input$operations_secteur]
    updatePickerInput(session, "operations_operation", choices = unique(filtered_data$operation))
    updatePickerInput(session, "operations_produit", choices = unique(filtered_data$cna_produit))
    updatePickerInput(session, "operations_valorisation", choices = unique(filtered_data$valorisation))
  }
  if (is.null(input$operations_secteur) & !is.null(input$operations_operation) &
      is.null(input$operations_produit) & is.null(input$operations_valorisation)) {
    filtered_data <- filtered_data[operation %in% input$operations_operation]
    updatePickerInput(session, "operations_secteur", choices = unique(filtered_data$sect_inst))
    updatePickerInput(session, "operations_produit", choices = unique(filtered_data$cna_produit))
    updatePickerInput(session, "operations_valorisation", choices = unique(filtered_data$valorisation))
  }
  if (is.null(input$operations_secteur) & is.null(input$operations_operation) &
      !is.null(input$operations_produit) & is.null(input$operations_valorisation)) {
    filtered_data <- filtered_data[cna_produit %in% input$operations_produit]
    updatePickerInput(session, "operations_secteur", choices = unique(filtered_data$sect_inst))
    updatePickerInput(session, "operations_operation", choices = unique(filtered_data$operation))
    updatePickerInput(session, "operations_valorisation", choices = unique(filtered_data$valorisation))
  }
  if (is.null(input$operations_secteur) & is.null(input$operations_operation) &
      is.null(input$operations_produit) & !is.null(input$operations_valorisation)) {
    filtered_data <- filtered_data[valorisation %in% input$operations_valorisation]
    updatePickerInput(session, "operations_secteur", choices = unique(filtered_data$sect_inst))
    updatePickerInput(session, "operations_operation", choices = unique(filtered_data$operation))
    updatePickerInput(session, "operations_produit", choices = unique(filtered_data$cna_produit))
  }

  # 2 nuls
  if (!is.null(input$operations_secteur) & !is.null(input$operations_operation) &
      is.null(input$operations_produit) & is.null(input$operations_valorisation)) {
    filtered_data <- filtered_data[sect_inst %in% input$operations_secteur & operation %in% input$operations_operation]
    updatePickerInput(session, "operations_produit", choices = unique(filtered_data$cna_produit))
    updatePickerInput(session, "operations_valorisation", choices = unique(filtered_data$valorisation))
  }
  if (!is.null(input$operations_secteur) & is.null(input$operations_operation) &
      !is.null(input$operations_produit) & is.null(input$operations_valorisation)) {
    filtered_data <- filtered_data[sect_inst %in% input$operations_secteur & cna_produit %in% input$operations_produit]
    updatePickerInput(session, "operations_operation", choices = unique(filtered_data$operation))
    updatePickerInput(session, "operations_valorisation", choices = unique(filtered_data$valorisation))
  }
  if (!is.null(input$operations_secteur) & is.null(input$operations_operation) &
      is.null(input$operations_produit) & !is.null(input$operations_valorisation)) {
    filtered_data <- filtered_data[sect_inst %in% input$operations_secteur & valorisation %in% input$operations_valorisation]
    updatePickerInput(session, "operations_operation", choices = unique(filtered_data$operation))
    updatePickerInput(session, "operations_produit", choices = unique(filtered_data$cna_produit))
  }

  if (is.null(input$operations_secteur) & !is.null(input$operations_operation) &
      !is.null(input$operations_produit) & is.null(input$operations_valorisation)) {
    filtered_data <- filtered_data[operation %in% input$operations_operation & cna_produit %in% input$operations_produit]
    updatePickerInput(session, "operations_secteur", choices = unique(filtered_data$sect_inst))
    updatePickerInput(session, "operations_valorisation", choices = unique(filtered_data$valorisation))
  }
  if (is.null(input$operations_secteur) & !is.null(input$operations_operation) &
      is.null(input$operations_produit) & !is.null(input$operations_valorisation)) {
    filtered_data <- filtered_data[operation %in% input$operations_operation & valorisation %in% input$operations_valorisation]
    updatePickerInput(session, "operations_secteur", choices = unique(filtered_data$sect_inst))
    updatePickerInput(session, "operations_produit", choices = unique(filtered_data$cna_produit))
  }
  if (is.null(input$operations_secteur) & is.null(input$operations_operation) &
      !is.null(input$operations_produit) & !is.null(input$operations_valorisation)) {
    filtered_data <- filtered_data[cna_produit %in% input$operations_produit & valorisation %in% input$operations_valorisation]
    updatePickerInput(session, "operations_secteur", choices = unique(filtered_data$sect_inst))
    updatePickerInput(session, "operations_operation", choices = unique(filtered_data$operation))
  }

  # 3 nuls
  if (!is.null(input$operations_secteur) & !is.null(input$operations_operation) &
      !is.null(input$operations_produit) & is.null(input$operations_valorisation)) {
    filtered_data <- filtered_data[sect_inst %in% input$operations_secteur & operation %in% input$operations_operation &
                                     cna_produit %in% input$operations_produit]
    updatePickerInput(session, "operations_valorisation", choices = unique(filtered_data$valorisation))
  }
  if (!is.null(input$operations_secteur) & !is.null(input$operations_operation) &
      is.null(input$operations_produit) & !is.null(input$operations_valorisation)) {
    filtered_data <- filtered_data[sect_inst %in% input$operations_secteur & operation %in% input$operations_operation &
                                     valorisation %in% input$operations_valorisation]
    updatePickerInput(session, "operations_produit", choices = unique(filtered_data$cna_produit))
  }
  if (!is.null(input$operations_secteur) & is.null(input$operations_operation) &
      !is.null(input$operations_produit) & !is.null(input$operations_valorisation)) {
    filtered_data <- filtered_data[sect_inst %in% input$operations_secteur & valorisation %in% input$operations_valorisation &
                                     cna_produit %in% input$operations_produit]
    updatePickerInput(session, "operations_operation", choices = unique(filtered_data$operation))
  }
  if (is.null(input$operations_secteur) & !is.null(input$operations_operation) &
      !is.null(input$operations_produit) & !is.null(input$operations_valorisation)) {
    filtered_data <- filtered_data[valorisation %in% input$operations_valorisation & operation %in% input$operations_operation &
                                     cna_produit %in% input$operations_produit]
    updatePickerInput(session, "operations_secteur", choices = unique(filtered_data$sect_inst))
  }
  }
}

# Observe events for each input
observeEvent(input$operations_secteur, {
  update_dependent_inputs()
})
observeEvent(input$operations_operation, {
  update_dependent_inputs()
})
observeEvent(input$operations_produit, {
  update_dependent_inputs()
})
observeEvent(input$operations_valorisation, {
  update_dependent_inputs()
})


observeEvent(input$operations_reset,{
  req(data_operations())
  if(is.null(data_operations())){
    return(NULL)
  } else{
    updatePickerInput(session = session,inputId = "operations_secteur",choices = unique(data_operations()[,sect_inst]))
    updatePickerInput(session = session,inputId = "operations_operation",choices = unique(data_operations()[,operation]))
    updatePickerInput(session = session,inputId = "operations_produit",choices = unique(data_operations()[,cna_produit]))
    updatePickerInput(session = session,inputId = "operations_valorisation",choices = unique(data_operations()[,valorisation]))
  }
})

## Onglet OPERATIONS #############################
observe({
  req(input$operations_secteur)
  req(input$operations_operation)
  req(input$operations_produit)
  req(input$operations_valorisation)

  tab <- data_operations()[sect_inst %in% input$operations_secteur &
                             operation %in% input$operations_operation &
                             cna_produit %in% input$operations_produit &
                             valorisation %in% input$operations_valorisation &
                             substr(period,1,4) %in% input$date_operation[1]:input$date_operation[2]]

  tab <- tab[order(period)]

  selectize_length <- c(length(input$operations_secteur),
                        length(input$operations_operation),
                        length(input$operations_produit))

  longueur_superieure_a_un <- function(...) {
    longueur_plus_grande_1 <- which(lengths(list(...)) > 1)
    if (length(longueur_plus_grande_1) == 1) {
      return(names(list(...))[longueur_plus_grande_1])
    }else if (length(longueur_plus_grande_1) > 1) {
      return(names(list(...))[longueur_plus_grande_1])
    }else {
      return(NULL)
    }
  }

  input_long <- longueur_superieure_a_un("sect_inst" = input$operations_secteur,
                                         "operation" = input$operations_operation,
                                         "cna_produit" = input$operations_produit)


  if(is.null(input_long)){
    if(input$evol_operations){
      tab <- tab  %>%
        arrange(period) %>%
        mutate(value = (value - lag(value))/lag(value)*100)
    }
    output$operations_hc_synth <- renderHighchart({
      chart <- hchart(
        tab,
        "line",
        hcaes(x = period, y = value)
      ) %>%
        hc_xAxis(title = list(text = "Trimestre")) %>%
        hc_yAxis(title = list(text = input$operations_valorisation,minorTickInterval = 'auto'))
      chart
    })
  }else if(length(input_long) == 1){
    if(input$evol_operations){
      tab <- tab %>%
        group_by(!!sym(input_long)) %>%
        arrange(period) %>%
        mutate(value = (value - lag(value))/lag(value)*100)
    }
    output$operations_hc_synth <- renderHighchart({
      chart <- hchart(
        tab,
        "line",
        hcaes(x = period, y = value, group = !!sym(input_long))
      ) %>%
        hc_xAxis(title = list(text = "Trimestre")) %>%
        hc_yAxis(title = list(text = input$operations_valorisation,minorTickInterval = 'auto'))
      chart
    })
  }
})




## Onglet CSI #############################
observe({
  req(input$csi_secteur)
  req(input$csi_compte)
  req(input$csi_operation)
  req(input$csi_correction)

  tab <- data_csi()[sect_inst %in% input$csi_secteur &
                             compte %in% input$csi_compte &
                             operation %in% input$csi_operation &
                             correction %in% input$csi_correction &
                             substr(period,1,4) %in% input$date_csi[1]:input$date_csi[2]]

  tab <- tab[order(period)]

  selectize_length <- c(length(input$csi_secteur),
                        length(input$csi_compte),
                        length(input$csi_operation))

  longueur_superieure_a_un <- function(...) {
    longueur_plus_grande_1 <- which(lengths(list(...)) > 1)
    if (length(longueur_plus_grande_1) == 1) {
      return(names(list(...))[longueur_plus_grande_1])
    }else if (length(longueur_plus_grande_1) > 1) {
      return(names(list(...))[longueur_plus_grande_1])
    }else {
      return(NULL)
    }
  }

  input_long <- longueur_superieure_a_un("sect_inst" = input$csi_secteur,
                                         "compte" = input$csi_compte,
                                         "operation" = input$csi_operation)


  if(is.null(input_long)){
    if(input$evol_csi){
      tab <- tab  %>%
        arrange(period) %>%
        mutate(value = (value - lag(value))/lag(value)*100)
    }
    output$csi_hc_synth <- renderHighchart({
      chart <- hchart(
        tab,
        "line",
        hcaes(x = period, y = value)
      ) %>%
        hc_xAxis(title = list(text = "Trimestre")) %>%
        hc_yAxis(title = list(text = input$csi_correction,minorTickInterval = 'auto'))
      chart
    })
  }else if(length(input_long) == 1){
    if(input$evol_csi){
      tab <- tab %>%
        group_by(!!sym(input_long)) %>%
        arrange(period) %>%
        mutate(value = (value - lag(value))/lag(value)*100)
    }
    output$csi_hc_synth <- renderHighchart({
      chart <- hchart(
        tab,
        "line",
        hcaes(x = period, y = value, group = !!sym(input_long))
      ) %>%
        hc_xAxis(title = list(text = "Trimestre")) %>%
        hc_yAxis(title = list(text = input$csi_correction,minorTickInterval = 'auto'))
      chart
    })
  }
})



## Onglet CB #############################
observe({
  req(input$cb_operation)
  req(input$cb_produit)
  req(input$cb_valorisation)
  req(input$cb_unite)
  req(input$cb_correction)

  tab <- data_cb()[operation %in% input$cb_operation &
                      cna_produit %in% input$cb_produit &
                      valorisation %in% input$cb_valorisation &
                      unit_measure %in% input$cb_unite &
                      correction %in% input$cb_correction &
                      substr(period,1,4) %in% input$date_cb[1]:input$date_cb[2]]

  tab <- tab[order(period)]

  selectize_length <- c(length(input$cb_operation),
                        length(input$cb_produit),
                        length(input$cb_valorisation),
                        length(input$cb_unite),
                        length(input$cb_correction))

  longueur_superieure_a_un <- function(...) {
    longueur_plus_grande_1 <- which(lengths(list(...)) > 1)
    if (length(longueur_plus_grande_1) == 1) {
      return(names(list(...))[longueur_plus_grande_1])
    }else if (length(longueur_plus_grande_1) > 1) {
      return(names(list(...))[longueur_plus_grande_1])
    }else {
      return(NULL)
    }
  }

  input_long <- longueur_superieure_a_un("operation" = input$cb_operation,
                                         "cna_produit" = input$cb_produit,
                                         "valorisation" = input$cb_valorisation,
                                         "unit_measure" = input$cb_unite,
                                         "correction" = input$cb_correction)


  if(is.null(input_long)){
    if(input$evol_cb){
      tab <- tab  %>%
        arrange(period) %>%
        mutate(value = (value - lag(value))/lag(value)*100)
    }
    output$cb_hc_synth <- renderHighchart({
      chart <- hchart(
        tab,
        "line",
        hcaes(x = period, y = value)
      ) %>%
        hc_xAxis(title = list(text = "Trimestre")) %>%
        hc_yAxis(title = list(text = input$cb_correction,minorTickInterval = 'auto'))
      chart
    })
  }else if(length(input_long) == 1){
    if(input$evol_cb){
      tab <- tab %>%
        group_by(!!sym(input_long)) %>%
        arrange(period) %>%
        mutate(value = (value - lag(value))/lag(value)*100)
    }
    output$cb_hc_synth <- renderHighchart({
      chart <- hchart(
        tab,
        "line",
        hcaes(x = period, y = value, group = !!sym(input_long))
      ) %>%
        hc_xAxis(title = list(text = "Trimestre")) %>%
        hc_yAxis(title = list(text = input$cb_correction,minorTickInterval = 'auto'))
      chart
    })
  }
})




## Onglet dette #############################
observe({
  req(input$dette_indicateur)
  req(input$dette_secteur)
  req(input$dette_instruments)

  tab <- data_dette()[indicateur %in% input$dette_indicateur &
                        sect_inst %in% input$dette_secteur &
                        dette_maastricht_intruments %in% input$dette_instruments &
                     substr(period,1,4) %in% input$date_dette[1]:input$date_dette[2]]

  tab <- tab[order(period)]

  selectize_length <- c(length(input$dette_indicateur),
                        length(input$dette_secteur),
                        length(input$dette_instruments))

  longueur_superieure_a_un <- function(...) {
    longueur_plus_grande_1 <- which(lengths(list(...)) > 1)
    if (length(longueur_plus_grande_1) == 1) {
      return(names(list(...))[longueur_plus_grande_1])
    }else if (length(longueur_plus_grande_1) > 1) {
      return(names(list(...))[longueur_plus_grande_1])
    }else {
      return(NULL)
    }
  }

  input_long <- longueur_superieure_a_un("indicateur" = input$indicateur,
                                         "sect_inst" = input$dette_secteur,
                                         "dette_maastricht_intruments" = input$dette_instruments)


  if(is.null(input_long)){
    if(input$evol_dette){
      tab <- tab  %>%
        arrange(period) %>%
        mutate(value = (value - lag(value))/lag(value)*100)
    }
    output$dette_hc_synth <- renderHighchart({
      chart <- hchart(
        tab,
        "line",
        hcaes(x = period, y = value)
      ) %>%
        hc_xAxis(title = list(text = "Trimestre"))
        # hc_yAxis(title = list(text = input$cb_correction,minorTickInterval = 'auto'))
      chart
    })
  }else if(length(input_long) == 1){
    if(input$evol_dette){
      tab <- tab %>%
        group_by(!!sym(input_long)) %>%
        arrange(period) %>%
        mutate(value = (value - lag(value))/lag(value)*100)
    }
    output$dette_hc_synth <- renderHighchart({
      chart <- hchart(
        tab,
        "line",
        hcaes(x = period, y = value, group = !!sym(input_long))
      ) %>%
        hc_xAxis(title = list(text = "Trimestre"))
        # hc_yAxis(title = list(text = input$cb_correction,minorTickInterval = 'auto'))
      chart
    })
  }
})


## Onglet PIB #############################
observe({
  req(input$pib_secteur)
  req(input$pib_operation)
  req(input$pib_nature)
  req(input$pib_valorisation)
  req(input$pib_unit)
  req(input$pib_correction)

  tab <- data_pib()[sect_inst %in% input$pib_secteur &
                     operation %in% input$pib_operation &
                     nature %in% input$pib_nature &
                     valorisation %in% input$pib_valorisation &
                     unit %in% input$pib_unit &
                     correction %in% input$pib_correction &
                     substr(period,1,4) %in% input$date_pib[1]:input$date_pib[2]]

  tab <- tab[order(period)]

  selectize_length <- c(length(input$pib_secteur),
                        length(input$pib_operation),
                        length(input$pib_nature),
                        length(input$pib_valorisation),
                        length(input$pib_unit),
                        length(input$pib_correction))

  longueur_superieure_a_un <- function(...) {
    longueur_plus_grande_1 <- which(lengths(list(...)) > 1)
    if (length(longueur_plus_grande_1) == 1) {
      return(names(list(...))[longueur_plus_grande_1])
    }else if (length(longueur_plus_grande_1) > 1) {
      return(names(list(...))[longueur_plus_grande_1])
    }else {
      return(NULL)
    }
  }

  input_long <- longueur_superieure_a_un("sect_inst" = input$pib_secteur,
                                         "operation" = input$pib_operation,
                                         "nature" = input$pib_nature,
                                         "valorisation" = input$pib_valorisation,
                                         "unit" = input$pib_unit,
                                         "correction" = input$pib_correction)


  if(is.null(input_long)){
    if(input$evol_pib){
      tab <- tab  %>%
        arrange(period) %>%
        mutate(value = (value - lag(value))/lag(value)*100)
    }
    output$pib_hc_synth <- renderHighchart({
      chart <- hchart(
        tab,
        "line",
        hcaes(x = period, y = value)
      ) %>%
        hc_xAxis(title = list(text = "Trimestre")) %>%
        hc_yAxis(title = list(text = input$pib_correction,minorTickInterval = 'auto'))
      chart
    })
  }else if(length(input_long) == 1){
    if(input$evol_pib){
      tab <- tab %>%
        group_by(!!sym(input_long)) %>%
        arrange(period) %>%
        mutate(value = (value - lag(value))/lag(value)*100)
    }
    output$pib_hc_synth <- renderHighchart({
      chart <- hchart(
        tab,
        "line",
        hcaes(x = period, y = value, group = !!sym(input_long))
      ) %>%
        hc_xAxis(title = list(text = "Trimestre")) %>%
        hc_yAxis(title = list(text = input$pib_correction,minorTickInterval = 'auto'))
      chart
    })
  }
})



  output$donnees_operations <- create_dt(data_operations(),
                                          cols_names = c("Secteur","Opération","Produit","Valorisation","Valeur","Date"))
  output$donnees_csi <- create_dt(data_csi(),
                                         cols_names = c("Secteur","Compte","Opération","Correction","Valeur","Date"))
  output$donnees_cb <- create_dt(data_cb(),
                                  cols_names = c("Opération","Produit","Valo","Unité","Correction","Valeur","Date"))
  output$donnees_dette <- create_dt(data_dette(),
                                 cols_names = c("Indicateur","Secteur","Instrument","Valeur","Date"))
  output$donnees_pib <- create_dt(data_pib(),
                                 cols_names = c("Secteur","Opération","Nature","Valo",
                                                "Unité","Correction","Valeur","Date"))


}

