# =============================================================================
# ONGLET : Allele Frequencies Missing Data
# =============================================================================

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui_allele_frequencies <- function() {
  fluidPage(
    titlePanel("Fréquences Alléliques et Données Manquantes"),
    
    fluidRow(
      column(12,
        wellPanel(
          h4("Informations Générales"),
          fluidRow(
            column(3, 
              valueBoxOutput("total_individuals", width = NULL)
            ),
            column(3, 
              valueBoxOutput("total_populations", width = NULL)
            ),
            column(3, 
              valueBoxOutput("total_markers", width = NULL)
            ),
            column(3, 
              valueBoxOutput("total_missing", width = NULL)
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(6,
        box(
          title = "Paramètres d'Analyse", status = "primary", solidHeader = TRUE,
          width = NULL, height = "200px",
          selectInput("selected_population", 
                     "Sélectionner Population:",
                     choices = c("Toutes les populations" = "all"),
                     multiple = FALSE),
          selectInput("selected_marker",
                     "Sélectionner Marqueur:",
                     choices = NULL,
                     multiple = FALSE),
          actionButton("update_analysis", "Mettre à jour l'analyse", 
                      class = "btn-primary")
        )
      ),
      column(6,
        box(
          title = "Données Manquantes par Population", status = "warning", 
          solidHeader = TRUE, width = NULL, height = "200px",
          tableOutput("missing_data_summary")
        )
      )
    ),
    
    fluidRow(
      column(12,
        box(
          title = "Fréquences Alléliques", status = "success", 
          solidHeader = TRUE, width = NULL,
          tabsetPanel(
            tabPanel("Par Population",
              br(),
              fluidRow(
                column(9,
                  DT::dataTableOutput("allele_freq_by_pop")
                ),
                column(3,
                  h5("Actions d'Export"),
                  downloadButton("download_freq_pop", "Export CSV", 
                               class = "btn-success btn-sm", style = "width: 100%; margin-bottom: 10px;"),
                  downloadButton("download_freq_pop_excel", "Export Excel", 
                               class = "btn-info btn-sm", style = "width: 100%; margin-bottom: 10px;"),
                  br(),
                  h5("Statistiques Rapides"),
                  verbatimTextOutput("quick_stats_pop", placeholder = TRUE)
                )
              )
            ),
            tabPanel("Globales",
              br(),
              fluidRow(
                column(9,
                  DT::dataTableOutput("allele_freq_global")
                ),
                column(3,
                  h5("Actions d'Export"),
                  downloadButton("download_freq_global", "Export CSV", 
                               class = "btn-success btn-sm", style = "width: 100%; margin-bottom: 10px;"),
                  downloadButton("download_freq_global_excel", "Export Excel", 
                               class = "btn-info btn-sm", style = "width: 100%; margin-bottom: 10px;"),
                  br(),
                  h5("Statistiques Globales"),
                  verbatimTextOutput("quick_stats_global", placeholder = TRUE)
                )
              )
            ),
            tabPanel("Graphiques",
              br(),
              fluidRow(
                column(6,
                  plotOutput("allele_freq_plot", height = "400px")
                ),
                column(6,
                  plotOutput("missing_data_plot", height = "400px")
                )
              ),
              br(),
              fluidRow(
                column(6,
                  plotOutput("diversity_plot", height = "400px")
                ),
                column(6,
                  plotOutput("allele_richness_plot", height = "400px")
                )
              )
            ),
            tabPanel("Diversité Génétique",
              br(),
              fluidRow(
                column(8,
                  DT::dataTableOutput("genetic_diversity_table")
                ),
                column(4,
                  h5("Indices de Diversité"),
                  p("He : Hétérozygotie attendue"),
                  p("Ho : Hétérozygotie observée"),
                  p("Na : Nombre d'allèles"),
                  p("Ne : Nombre effectif d'allèles"),
                  p("Fis : Coefficient de consanguinité"),
                  br(),
                  downloadButton("download_diversity", "Export Diversité", 
                               class = "btn-warning btn-sm", style = "width: 100%;")
                )
              )
            ),
            tabPanel("Matrice Allélique",
              br(),
              fluidRow(
                column(12,
                  h4("Matrice Présence/Absence des Allèles par Population"),
                  DT::dataTableOutput("allele_matrix")
                )
              ),
              br(),
              fluidRow(
                column(6,
                  downloadButton("download_matrix", "Export Matrice", 
                               class = "btn-primary btn-sm")
                ),
                column(6,
                  plotOutput("allele_sharing_heatmap", height = "300px")
                )
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(6,
        box(
          title = "Détails par Marqueur", status = "info", 
          solidHeader = TRUE, width = NULL,
          DT::dataTableOutput("marker_details")
        )
      ),
      column(6,
        box(
          title = "Analyse de Qualité des Données", status = "warning", 
          solidHeader = TRUE, width = NULL,
          tabsetPanel(
            tabPanel("Résumé Qualité",
              br(),
              DT::dataTableOutput("data_quality_summary")
            ),
            tabPanel("Allèles Rares",
              br(),
              numericInput("rare_threshold", "Seuil fréquence rare (%):", 
                         value = 5, min = 1, max = 20, step = 1),
              DT::dataTableOutput("rare_alleles_table")
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(12,
        box(
          title = "Outils de Filtrage et Nettoyage", status = "danger", 
          solidHeader = TRUE, width = NULL, collapsible = TRUE, collapsed = TRUE,
          fluidRow(
            column(3,
              h5("Filtres de Qualité"),
              numericInput("min_sample_size", "Taille min. échantillon:", 
                         value = 5, min = 1, max = 50),
              numericInput("max_missing_percent", "Max % données manquantes:", 
                         value = 50, min = 0, max = 100)
            ),
            column(3,
              h5("Filtres Alléliques"),
              numericInput("min_allele_freq", "Fréq. allélique min. (%):", 
                         value = 1, min = 0, max = 50, step = 0.5),
              checkboxInput("remove_monomorphic", "Exclure marqueurs monomorphes", value = FALSE)
            ),
            column(3,
              h5("Sélection Populations"),
              checkboxGroupInput("populations_to_include", "Populations à inclure:",
                               choices = NULL, selected = NULL)
            ),
            column(3,
              h5("Actions"),
              br(),
              actionButton("apply_filters", "Appliquer Filtres", 
                         class = "btn-warning", style = "width: 100%; margin-bottom: 10px;"),
              downloadButton("download_filtered_data", "Export Données Filtrées", 
                           class = "btn-danger", style = "width: 100%; margin-bottom: 10px;"),
              actionButton("reset_filters", "Reset Filtres", 
                         class = "btn-secondary", style = "width: 100%;")
            )
          )
        )
      )
    )
  )
}