library(shiny)
library(readxl)
library(htmltools)
library(bslib)
library(shinyWidgets)

# Variables globales ----

## Couleurs ----

brun = "#884c42"
brun2 = "#9e7b71"
beige = "#f4eee5"
beige2 = "#e6d7c0"
rose = "#ffa593"


## Vocabulaire ----

study_type = read_xlsx("../spec/init_database_vocabulaire.xlsx", sheet = "study_type")
base = read_xlsx("../spec/init_database_vocabulaire.xlsx", sheet = "base")
status = read_xlsx("../spec/init_database_vocabulaire.xlsx", sheet = "status")
script_type = read_xlsx("../spec/init_database_vocabulaire.xlsx", sheet = "script_type")



# Shiny ---- 
## CSS ----


fluidPage(

tags$head(tags$style(HTML(
'/* CSS for the checked checkboxes */
  .pretty.p-default input:checked~.state label:after {
    background-color: #9e7b71 !important;
  }

.pretty.p-default label:before {
    background-color: #ffa593 !important;
    
  }

.form-check-input:checked, .shiny-input-container .checkbox input:checked, .shiny-input-container .checkbox-inline input:checked, .shiny-input-container .radio input:checked, .shiny-input-container .radio-inline input:checked {
    background-color: #9e7b71;
    border-color: #9e7b71;
}

.form-check-input:focus, .shiny-input-container .checkbox input:focus, .shiny-input-container .checkbox-inline input:focus, .shiny-input-container .radio input:focus, .shiny-input-container .radio-inline input:focus {
    border-color: #9e7b71;
    outline: 0;
    box-shadow: 0 0 0 0rem rgba(0,123,194,0.25);
}

.form-check-input, .shiny-input-container .checkbox input , .shiny-input-container .checkbox-inline input, .shiny-input-container .radio input:focus, .shiny-input-container .radio-inline input:focus {
    background-color: #f4eee5;
    border-color: #9e7b71;
    border-width: 2px;
    outline: 0;
}


.form-control, .form-control:focus {
    color: #884c42;
    background-color: #f4eee5;
    background-clip: padding-box;
    border-color: #9e7b71;
    border-width: 3px;
    border-radius: 40px;
    transition: border-color 0.15s cubic-bezier(0.3, -0.12, 0.58, 1),box-shadow 0.15s ease-in-out;
    t: ;
    box-shadow: 0 0 0 0rem rgba(0,123,194,0.25);

}


.btn-outline-secondary {
    color: #f4eee5;
    background-color: #9e7b71;
    background-clip: padding-box;
    border-color: #9e7b71;
    border-width: 3px;
    border-radius: 40px;
    transition: border-color 0.15s cubic-bezier(0.3, -0.12, 0.58, 1),box-shadow 0.15s ease-in-out;
    t: ;
    box-shadow: 0 0 0 0rem rgba(0,123,194,0.25);
    --bs-btn-active-bg: #9e7b71;
    --bs-btn-active-border-color: #9e7b71

}

.btn-outline-secondary:hover {
    color: #9e7b71;
    background-color: #f4eee5;
    background-clip: padding-box;
    border-color: #9e7b71;
    border-width: 3px;
    border-radius: 40px;
    transition: border-color 0.15s cubic-bezier(0.3, -0.12, 0.58, 1),box-shadow 0.15s ease-in-out;
    t: ;
    box-shadow: 0 0 0 0rem rgba(0,123,194,0.25);

}



.dropdown-toggle .filter-option-inner-inner {
    overflow: hidden;
    color: #9e7b71;
    font-style : italic;
}

.dropdown-menu {
  font-size: 12px;
}

.dropdown-item:hover, .dropdown-menu>li>a:hover, .dropdown-item:focus, .dropdown-menu>li>a:focus {
    color: #9e7b71 !important;
}

.dropdown-item:active, .dropdown-menu>li>a:active, .dropdown-item:focus, .dropdown-menu>li>a:focus {
    color: #9e7b71 !important;
    background-color : transparent !important;
}

.btn-outline-default, .btn-default:not(.btn-primary,.btn-secondary,.btn-info,.btn-success,.btn-danger,.btn-warning,.btn-light,.btn-dark,.btn-link,[class*="btn-outline-"]) {
    color: #f4eee5;
    background-color: #9e7b71;
    background-clip: padding-box;
    border-color: #9e7b71;
    border-width: 3px;
    border-radius: 10px;
    transition: border-color 0.15s cubic-bezier(0.3, -0.12, 0.58, 1),box-shadow 0.15s ease-in-out;
    t: ;
    box-shadow: 0 0 0 0rem rgba(0,123,194,0.25);
}


.btn-outline-default:hover, .btn-default:not(.btn-primary,.btn-secondary,.btn-info,.btn-success,.btn-danger,.btn-warning,.btn-light,.btn-dark,.btn-link,[class*="btn-outline-"]):hover {
    color: #9e7b71;
    background-color: #f4eee5;
    background-clip: padding-box;
    border-color: #9e7b71;
    border-width: 3px;
    border-radius: 10px;
    transition: border-color 0.15s cubic-bezier(0.3, -0.12, 0.58, 1),box-shadow 0.15s ease-in-out;
    t: ;
    box-shadow: 0 0 0 0rem rgba(0,123,194,0.25);

}

.btn-outline-default:active, .btn-default:not(.btn-primary,.btn-secondary,.btn-info,.btn-success,.btn-danger,.btn-warning,.btn-light,.btn-dark,.btn-link,[class*="btn-outline-"]):active {
    color: #f4eee5;
    background-color: #9e7b71;
    background-clip: padding-box;
    border-color: #9e7b71;
    border-width: 3px;
    border-radius: 10px;
    transition: border-color 0.15s cubic-bezier(0.3, -0.12, 0.58, 1),box-shadow 0.15s ease-in-out;
    t: ;
    box-shadow: 0 0 0 0rem rgba(0,123,194,0.25);
}

table.dataTable {
    color: #666;
    border-color: transparent
    }
                      
   

table.dataTable tr.selected td, table.dataTable tr.selected {
    box-shadow: inset 0 0 0 9999px #ffa593 !important; 
    color: white !important;
    border-color: transparent;
    }
                      
                      :root {
    --dt-row-selected: transparent !important;
}


table.dataTable tbody tr:hover, table.dataTable tbody tr:hover td {
    color: #ffa593;
    background-color: transparent !important;
    border-color: transparent;
}

table.dataTable.row-border>tbody>tr.selected+tr.selected>td, table.dataTable.display>tbody>tr.selected+tr.selected>td {
    border-top-color: transparent !important;
}

.dataTables_wrapper .dataTables_info {
  color: #9e7b71 !important;
  font-weight: bold;
  font-style: italic
}

.navbar-brand {
  font-family: Forte !important;
}

'
))),

tags$style(".fa-cart-shopping {color:#9e7b71}"),


  
  
  page_navbar(
    title = "SNDS QueryHub",
  
    bg = brun,
    underline = TRUE,
    
## Message ----

  
  
## Accueil ----

    nav_spacer(),
    nav_panel(
      title = "Accueil",
      modalDialog(tags$h2("Bienvenue !",
                          style = 'color:#ffa593; font-family: Forte; font-size:50px; font-weigth:bold'),
                  tags$br(),
                  tags$div(tags$b("SNDS QueryHub"), "vous permet de découvrir et télécharger des scripts R réutilisables dédiés à l'exploitation du SNDS.",
                  tags$br(),
                  tags$br(),
                  "Deux façons de faire votre shopping :",
                  tags$br(),
                  tags$li("Parcourer les scripts et ajouter à votre panier ceux que vous souhaiteriez utiliser"),
                  tags$li("Sélectionner une étude dont vous souhaitez répliquer la méthode pour ajouter à votre panier touts les scripts associés"),
                  tags$br(),
                  "Rendez-vous dans votre panier pour télécharger votre sélection !", style = "color:#884c42"),
                  
                  easyClose = T,
                  footer = modalButton("Compris !")),
      fluidRow(
        tags$b((tags$h1("Nos derniers scripts",
                style = 'color:#ffa593; font-family: Forte; font-size:50px; font-weigth:bold')))
        ),
      
      fluidRow(
          
        column(
          width = 4,
          value_box(
            title = uiOutput("title_script_1"),
            value = uiOutput("description_script_1"),
            p(uiOutput("type_base_script1")),
            style = 'background-color: #e6d7c0!important;',
            height = 250,
            checkboxInput(
              inputId = "script1_cart", 
              label = icon("cart-shopping"))
            ),
        ),
        
        column(
          width = 4,
          value_box(
            title = uiOutput("title_script_2"),
            value = uiOutput("description_script_2"),
            style = 'background-color: #e6d7c0!important;',
            height = 250,
            checkboxInput(
              inputId = "script2_cart", 
              label = icon("cart-shopping"))
          ),
        ),
        
        column(
          width = 4,
          value_box(
            title = uiOutput("title_script_3"),
            value = uiOutput("description_script_3"),
            style = 'background-color: #e6d7c0!important;',
            height = 250,
            checkboxInput(
              inputId = "script3_cart", 
              label = icon("cart-shopping"))
            )
          )

        ),
      
    fluidRow(
        tags$b((tags$h1("Nos dernières études",
                              style = 'color:#ffa593; font-family: Forte; font-size:50px; font-weigth:bold')))
    ),
      
      fluidRow(
        
        column(
          width = 4,
          value_box(
            title = uiOutput("title_study_1"),
            value = uiOutput("description_study_1"),
            p(uiOutput("type_author_study1")),
            style = 'background-color: #e6d7c0!important;',
            height = 550,
            checkboxInput(
              inputId = "study1_cart", 
              label = tags$i(icon("cart-shopping"),"Ajouter tous ses scripts", style = "color:#9e7b71"))
          )
        ),
        
        column(
          width = 4,
          value_box(
            title = uiOutput("title_study_2"),
            value = uiOutput("description_study_2"),
            style = 'background-color: #e6d7c0!important;',
            height = 550,
            checkboxInput(
              inputId = "study2_cart", 
              label = tags$i(icon("cart-shopping"),"Ajouter tous ses scripts", style = "color:#9e7b71"))
          )
        ),
        
        column(
          width = 4,
          value_box(
            title = uiOutput("title_study_3"),
            value = uiOutput("description_study_3"),
            style = 'background-color: #e6d7c0!important;',
            height = 550,
            checkboxInput(
              inputId = "study3_cart", 
              label = tags$i(icon("cart-shopping"),"Ajouter tous ses scripts", style = "color:#9e7b71"))
          )
        )
        
      )
      


      ),
    
## Page Scripts ----
    
    nav_spacer(),
    nav_panel(title = "Scripts",
              fluidRow(
                tags$b((tags$h1("Tous nos scripts",
                                      style = 'color:#ffa593; font-family: Forte; font-size:50px; font-weigth:bold')))
              ),
              searchInput(inputId = "search_script",
                          label = tags$b("Rechercher un scipt par mots-clés", style = "color:#884c42"),
                          placeholder = "ex : Délivrances, Séquences ... ",
                          btnSearch = icon("magnifying-glass"),
                          btnReset = icon("xmark"),
                          width = "100%"
              ),
              fluidRow(
                column(
                  pickerInput("script_type", 
                              tags$b("Type de script", style = "color:#884c42"), 
                              choices = script_type$script_type_label,
                              inline = T,
                              width = '100%', 
                              options = pickerOptions(
                                noneSelectedText = "Choisir au moins un type de script",
                                selectAllText = "Sélectionner tout",
                                deselectAllText = "Effacer tout",
                                actionsBox = TRUE, 
                                size = 10,
                                selectedTextFormat = "count > 10",
                                style = ""
                              ), 
                              multiple = TRUE,
                              selected = script_type$script_type_label),
                  width = 6),
                column(
                  pickerInput("base", 
                              tags$b("Base concernée", style = "color:#884c42"), 
                              choices = base$base_label,
                              inline = T,
                              width = "100%", 
                              options = pickerOptions(
                                noneSelectedText = "Choisir au moins une base",
                                selectAllText = "Sélectionner tout",
                                deselectAllText = "Effacer tout",
                                actionsBox = TRUE, 
                                size = 10,
                                selectedTextFormat = "count > 10",
                                style = ""
                              ), 
                              multiple = TRUE,
                              selected = base$base_label),
                  width = 6)
              ),
              fluidRow(
                column(
                  tags$b("Sélectionner les scripts de votre choix et ajouter les au panier ici : ", style = "color:#884c42"),
                  width = 7),
                column(
                  tags$a(icon("cart-shopping"), style = "font-size:30px"),
                  
                  HTML('&ensp;'),
                  actionButton("add_cart_script", tags$div("Ajouter au panier"),
                               width = "50%"),,
                  width = 5)
                ),
              dataTableOutput("script_table")
                      
            )
              
              ,

## Page Etudes
    
    nav_spacer(),
    nav_panel(title = "Etude",
              fluidRow(
                tags$b((tags$h1("Toutes nos études",
                                      style = 'color:#ffa593; font-family: Forte; font-size:50px; font-weigth:bold')))
              ),
              searchInput(inputId = "search_study",
                          label = tags$b("Rechercher une étude par mots-clés ou premir auteur", style = "color:#884c42"),
                          placeholder = "ex : Anxiolytiques, Guidelines ... ",
                          btnSearch = icon("magnifying-glass"),
                          btnReset = icon("xmark"),
                          width = "100%"
              ),
              fluidRow(
                column(
                  pickerInput("study_type", 
                              tags$b("Type d'étude", style = "color:#884c42"), 
                              choices = study_type$study_type_label,
                              inline = T,
                              width = '100%', 
                              options = pickerOptions(
                                noneSelectedText = "Choisir au moins un type d'étude",
                                selectAllText = "Sélectionner tout",
                                deselectAllText = "Effacer tout",
                                actionsBox = TRUE, 
                                size = 10,
                                selectedTextFormat = "count > 10",
                                style = ""
                              ), 
                              multiple = TRUE,
                              selected = study_type$study_type_label),
                  width = 6),
                column(
                  pickerInput("status", 
                              tags$b("status", style = "color:#884c42"), 
                              choices = status$status_label,
                              inline = T,
                              width = "100%", 
                              options = pickerOptions(
                                noneSelectedText = "Choisir au moins un status",
                                selectAllText = "Sélectionner tout",
                                deselectAllText = "Effacer tout",
                                actionsBox = TRUE, 
                                size = 10,
                                selectedTextFormat = "count > 10",
                                style = ""
                              ), 
                              multiple = TRUE,
                              selected = status$status_label),
                  width = 6)
              ),
              fluidRow(
                column(
                  tags$b("Sélectionner les études de votre choix et ajouter au panier les scripts associés ici : ", style = "color:#884c42"),
                  width = 7
                  ),
                column(
                  tags$a(icon("cart-shopping"), style = "font-size:30px"),
                  HTML("&ensp;"),
                  actionButton("add_cart_study", "Ajouter au panier", width = "80%"),
                  width = 5
                  )
              ),
              dataTableOutput("study_table")),
  
## Panier ----
  
    nav_spacer(),
    nav_panel(title = icon("cart-arrow-down"), 
              fluidRow(
                tags$b((tags$h1("Votre panier",
                                      style = 'color:#ffa593; font-family: Forte; font-size:50px; font-weigth:bold')))
              ),
              
              fluidRow(tags$div("Certains des scripts que vous avez sélectionnés peuvent faire appel à d'autres de nos scripts pour",
                                "leur fonctionnement. Toutes les dépendances sont listés en bas de page, vous pouvre choisir de télécharger votre",
                                "sélection avec ou sans les dépendances. Nous recommandons de toujours télécharger les dépendances.",
                                style = "color:#884c42"),
                       tags$br()),
              # fluidRow(
              #          tags$div("Si vous souhaitez retirer des élèments de votre panier, sélectionnez-les puis cliquez sur",
              #                   tags$i(tags$b("Supprimer")),".",
              #                            style = "color:#884c42"),
              #          tags$br(),
              # ),
              fluidRow(
                column(downloadButton("download_with_dep", "Télécharger tout", width = "45%"), width = 3),
                # HTML("&emsp;"),
                column(downloadButton("download_without_dep", "Télécharger sans les dépendences", width = "45%"), width = 5)
                ),
              # fluidRow(column(width = 10, ""),
              # column(actionButton("remove_select", "Supprimer"), width = 2 )),
                dataTableOutput("selection"),
              
              fluidRow(
                tags$b((tags$h1("Dépendances",
                                      style = 'color:#ffa593; font-family: Forte; font-size:50px; font-weigth:bold')))
              ),
              fluidRow(
                tags$div("Certains des scripts ci-dessous sont peut-être déjà dans votre panier. Dans ce cas, ils ne seront téléchargés qu'une seule fois.",
                       style = "color:#884c42"),
                tags$br()
              ),
                dataTableOutput("dependences")
              
              ),
  
## Plus d'infos
  
    nav_menu(
      title = icon("circle-question"),
      align = "right",
      nav_item(
        tags$a(icon("circle-info"), "A propos", 
               href = "https://gitlab.com/Chloe_StDizier/projets-csd/-/tree/main/250422_appli_scripts?ref_type=heads"
        ),
        tags$a(icon("gitlab"), "Accéder aux scripts via GitLab", 
               href = "https://git.f2rsmpsy.org/f2rsm/tools_f2rsm/src/branch/main/snds/scripts/R"
        )
      )
    )
  )
)
