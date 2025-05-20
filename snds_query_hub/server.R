
library(shiny)
library(data.table)
library(stringr)
library(stringi)
library(DT)
library(magrittr)
library(dplyr)


## Couleurs ----

brun = "#884c42"


# Load data base ----

## Tables principales ----

script = read_xlsx("../spec/init_database_table_ppl.xlsx", sheet = "script")
study = read_xlsx("../spec/init_database_table_ppl.xlsx", sheet = "study")
id_management = read_xlsx("../spec/init_database_table_ppl.xlsx", sheet = "id_management")
requirements = read_xlsx("../spec/init_database_table_ppl.xlsx", sheet = "requirements")


## Vocabulaire ----

study_type = read_xlsx("../spec/init_database_vocabulaire.xlsx", sheet = "study_type")
base = read_xlsx("../spec/init_database_vocabulaire.xlsx", sheet = "base")
status = read_xlsx("../spec/init_database_vocabulaire.xlsx", sheet = "status")
script_type = read_xlsx("../spec/init_database_vocabulaire.xlsx", sheet = "script_type")


function(input, output, session) {

  
  # Accueil ----
  
  output$title_script_1 = renderUI(
    
    tags$div(as.character(script %>% 
      arrange(desc(publication_date)) %>% 
      head(1) %>% 
      select(script_label)),
      tags$br(),
      style = "font-size:25px; color:#884c42"
      )
    
  )
  
  
  output$description_script_1 = renderUI(
    
    tags$div(
      tags$div(as.character(script %>% 
                              arrange(desc(publication_date)) %>% 
                              head(1) %>% 
                              select(script_description)),
               style = "font-size:15px; color:#9e7b71"
      ),
      tags$div(
        
        paste0(
          as.character(script %>% 
                         arrange(desc(publication_date)) %>% 
                         head(1) %>% 
                         select(script_type_id) %>% 
                         left_join(script_type) %>% 
                         select(script_type_label)),
          
          " - ",
          
          as.character(script %>% 
                         arrange(desc(publication_date)) %>% 
                         head(1) %>% 
                         select(base_id) %>% 
                         left_join(base) %>% 
                         select(base_label))
          
        ),
        
        style = "font-size:15px; text-align:right; color:#884c42"
        
      )
    )
  )
  
  
  output$title_script_2 = renderUI(
    
    tags$div(as.character((script %>% 
                            arrange(desc(publication_date)) %>% 
                            head(2) %>% 
                            select(script_label))[2,]),
             tags$br(),
             style = "font-size:25px; color:#884c42"
    )
    
  )
  
  
  output$description_script_2 = renderUI(
    
    tags$div(
      tags$div(as.character((script %>% 
                              arrange(desc(publication_date)) %>% 
                              head(2) %>% 
                              select(script_description))[2,]),
               style = "font-size:15px; color:#9e7b71"
      ),
      tags$div(
        
        paste0(
          as.character((script %>% 
                         arrange(desc(publication_date)) %>% 
                         head(2) %>% 
                         select(script_type_id) %>% 
                         left_join(script_type) %>% 
                         select(script_type_label))[2,]),
          
          " - ",
          
          as.character((script %>% 
                         arrange(desc(publication_date)) %>% 
                         head(2) %>% 
                         select(base_id) %>% 
                         left_join(base) %>% 
                         select(base_label))[2,])
          
        ),
        
        style = "font-size:15px; text-align:right; color:#884c42"
        
      )
    )
  )

  
  
  
  output$title_script_3 = renderUI(
    
    tags$div(as.character((script %>% 
                             arrange(desc(publication_date)) %>% 
                             head(3) %>% 
                             select(script_label))[3,]),
             tags$br(),
             style = "font-size:25px; color:#884c42"
    )
    
  )
  
  
  output$description_script_3 = renderUI(
    
    tags$div(
      tags$div(as.character((script %>% 
                               arrange(desc(publication_date)) %>% 
                               head(3) %>% 
                               select(script_description))[3,]),
               style = "font-size:15px; color:#9e7b71"
      ),
      tags$div(
        
        paste0(
          as.character((script %>% 
                          arrange(desc(publication_date)) %>% 
                          head(3) %>% 
                          select(script_type_id) %>% 
                          left_join(script_type) %>% 
                          select(script_type_label))[3,]),
          
          " - ",
          
          as.character((script %>% 
                          arrange(desc(publication_date)) %>% 
                          head(3) %>% 
                          select(base_id) %>% 
                          left_join(base) %>% 
                          select(base_label))[3,])
          
        ),
        
        style = "font-size:15px; text-align:right; color:#884c42"
        
      )
    )
  )
  
  
  
  output$title_study_1 = renderUI(
    
    tags$div(as.character(study %>% 
                            arrange(desc(date)) %>% 
                            head(1) %>% 
                            select(title)),
             tags$br(),
             style = "font-size:25px; color:#884c42"
    )
    
  )
  
  
  output$description_study_1 = renderUI(
    
    tags$div(
      tags$div(as.character(study %>% 
                              arrange(desc(date)) %>% 
                              head(1) %>% 
                              select(study_description)),
               style = "font-size:15px; color:#9e7b71"
      ),
      tags$br(),
      tags$div(
        
        paste0(
          as.character(study %>% 
                         arrange(desc(date)) %>% 
                         head(1) %>% 
                         select(study_type_id) %>% 
                         left_join(study_type) %>% 
                         select(study_type_label)),
          
          " - ",
          
          as.character(study %>% 
                         arrange(desc(date)) %>% 
                         head(1) %>% 
                         select(first_author))
          
        ),
        
        style = "font-size:15px; text-align:right; color:#884c42"
        
      )
    )
  )
  
  
  output$title_study_2 = renderUI(
    
    tags$div(as.character((study %>% 
                             arrange(desc(date)) %>% 
                             head(2) %>% 
                             select(title))[2,]),
             tags$br(),
             style = "font-size:25px; color:#884c42"
    )
    
  )
  
  
  output$description_study_2 = renderUI(
    
    tags$div(
      tags$div(as.character((study %>% 
                               arrange(desc(date)) %>% 
                               head(2) %>% 
                               select(study_description))[2,]),
               style = "font-size:15px; color:#9e7b71"
      ),
      tags$br(),
      tags$div(
        
        paste0(
          as.character((study %>% 
                          arrange(desc(date)) %>% 
                          head(2) %>% 
                          select(study_type_id) %>% 
                          left_join(study_type) %>% 
                          select(study_type_label))[2,]),
          
          " - ",
          
          as.character((study %>% 
                          arrange(desc(date)) %>% 
                          head(2) %>% 
                          select(first_author))[2,])
          
        ),
        
        style = "font-size:15px; text-align:right; color:#884c42"
        
      )
    )
  )
  
  
  
  
  output$title_study_3 = renderUI(
    
    tags$div(as.character((study %>% 
                             arrange(desc(date)) %>% 
                             head(3) %>% 
                             select(title))[3,]),
             tags$br(),
             style = "font-size:25px; color:#884c42"
    )
    
  )
  
  
  output$description_study_3 = renderUI(
    
    tags$div(
      tags$div(as.character((study %>% 
                               arrange(desc(date)) %>% 
                               head(3) %>% 
                               select(study_description))[3,]),
               style = "font-size:15px; color:#9e7b71"
      ),
      tags$br(),
      tags$div(
        
        paste0(
          as.character((study %>% 
                          arrange(desc(date)) %>% 
                          head(3) %>% 
                          select(study_type_id) %>% 
                          left_join(study_type) %>% 
                          select(study_type_label))[3,]),
          
          " - ",
          
          as.character((study %>% 
                          arrange(desc(date)) %>% 
                          head(3) %>% 
                          select(first_author))[3,])
          
        ),
        
        style = "font-size:15px; text-align:right; color:#884c42"
        
      )
    )
  )
  
  # Cart ----
  
  cart_accueil = reactiveValues(
    script_1 = F,
    script_2 = F,
    script_3 = F,
    
    study_1 = F,
    study_2 = F,
    study_3 = F
  )
  

  

  # Scripts ----

  




  observe(

    if (is.null(input$search_script) | input$search_script == "") {
      
      tbl = data.table(
        script %>%
          left_join(base) %>%
          filter(base_label %in% input$base) %>%
          left_join(script_type) %>%
          filter(script_type_label %in% input$script_type) %>%
          mutate(desc_clean =  stri_trans_general(str = tolower(script_description), id = "Latin-ASCII"),
                 label_clean = stri_trans_general(str = tolower(script_label), id = "Latin-ASCII")) %>%
          select(script_label, script_description, script_type_label, base_label))
      rownames(tbl) =  tbl$script_label
      
      output$script_table = renderDT(
        tbl,
        rownames = F,
        colnames = c("Nom", "Description", "Type", "Base"),
        options = list(
          dom = "itp",
          language = list(paginate = 
                            list('next' = "Précédent", 
                                 previous="Suivant"),
                          sEmptyTable = "Aucun script ne correspond à votre recherche",
                          sInfo = "_TOTAL_ résultat(s)")
        )
      )

      } else {
        output$script_table = renderDT(
          data.table(
            script %>%
              left_join(base) %>%
              filter(base_label %in% input$base) %>%
              left_join(script_type) %>%
              filter(script_type_label %in% input$script_type) %>%
              mutate(desc_clean =  stri_trans_general(str = tolower(script_description), id = "Latin-ASCII"),
                     label_clean = stri_trans_general(str = tolower(script_label), id = "Latin-ASCII")) %>%
              filter(str_detect(desc_clean,
                                pattern = stri_trans_general(str = tolower(input$search_script), id = "Latin-ASCII")) |
                       str_detect(label_clean,
                                  pattern = stri_trans_general(str = tolower(input$search_script), id = "Latin-ASCII")) ) %>%
              select(script_label, script_description, script_type_label, base_label)),
          rownames = F,
          colnames = c("Nom", "Description", "Type", "Base"),
          options = list(
            dom = "itp",
            language = list(paginate = 
                                       list('next' = "Précédent", 
                                            previous="Suivant"),
                            sEmptyTable = "Aucun script ne correspond à votre recherche",
                            sInfo = "_TOTAL_ résultat(s)")
          )
        )

      }

   
   
  )


  cart_script = reactiveValues("selected" = data.frame())
  tableProxy_script <- dataTableProxy('script_table')
  
  
  # Action button to add all rows in current view to previous selection
  observeEvent(input$add_cart_script, {
    
    
    if (is.null(input$search_script) | input$search_script == "") {
      
      tbl = data.table(
        script %>%
          left_join(base) %>%
          filter(base_label %in% input$base) %>%
          left_join(script_type) %>%
          filter(script_type_label %in% input$script_type) %>%
          mutate(desc_clean =  stri_trans_general(str = tolower(script_description), id = "Latin-ASCII"),
                 label_clean = stri_trans_general(str = tolower(script_label), id = "Latin-ASCII")) %>%
          select(script_id,script_label, script_description, script_type_label, base_label))
    } else {
      tbl = data.table(
          script %>%
            left_join(base) %>%
            filter(base_label %in% input$base) %>%
            left_join(script_type) %>%
            filter(script_type_label %in% input$script_type) %>%
            mutate(desc_clean =  stri_trans_general(str = tolower(script_description), id = "Latin-ASCII"),
                   label_clean = stri_trans_general(str = tolower(script_label), id = "Latin-ASCII")) %>%
            filter(str_detect(desc_clean,
                              pattern = stri_trans_general(str = tolower(input$search_script), id = "Latin-ASCII")) |
                     str_detect(label_clean,
                                pattern = stri_trans_general(str = tolower(input$search_script), id = "Latin-ASCII")) ) %>%
            select(script_id,script_label, script_description, script_type_label, base_label))
      
    
    }
    
    
    if (!is.null(input$script_table_rows_selected)) {
      if (is.null(cart_script$selected)) {
        cart_script$selected = tbl[input$script_table_rows_selected,] %>% 
          select(script_id) %>% 
          mutate(Depuis = "Script",
                 etude = 0)
      }else {
  
        cart_script$selected = plyr::rbind.fill(cart_script$selected, tbl[input$script_table_rows_selected,]) %>% 
          select(script_id) %>% 
          mutate(Depuis = "Script",
                 etude = 0)
      }
      
      
      selectRows(proxy = tableProxy_script,
                 selected = c())
    }
    
  })
  
  
  
  
  
  # Etudes ----
 
  observe(
    
    if (is.null(input$search_study) | input$search_study == "") {
      
      tbl = data.table(
        study %>%
          left_join(status) %>%
          filter(status_label %in% input$status) %>%
          left_join(study_type) %>%
          filter(study_type_label %in% input$study_type) %>%
          mutate(desc_clean =  stri_trans_general(str = tolower(study_description), id = "Latin-ASCII"),
                 title_clean = stri_trans_general(str = tolower(title), id = "Latin-ASCII")) %>%
          select(title, study_description, study_type_label, status_label, first_author))
  
      
      output$study_table = renderDT(
        tbl,
        rownames = F,
        colnames = c("Titre", "Description", "Type","Statut", "Premier auteur"),
        options = list(
          dom = "itp",
          language = list(paginate = 
                            list('next' = "Précédent", 
                                 previous="Suivant"),
                          sEmptyTable = "Aucune étude ne correspond à votre recherche",
                          sInfo = "_TOTAL_ résultat(s)")
        )
      )
      
    } else {
      output$study_table = renderDT(
        data.table(
          study %>%
            left_join(status) %>%
            filter(status_label %in% input$status) %>%
            left_join(study_type) %>%
            filter(study_type_label %in% input$study_type) %>%
            mutate(desc_clean =  stri_trans_general(str = tolower(study_description), id = "Latin-ASCII"),
                   title_clean = stri_trans_general(str = tolower(title), id = "Latin-ASCII"),
                   author_clean = stri_trans_general(str = tolower(first_author), id = "Latin-ASCII")) %>%
            filter(str_detect(desc_clean,
                              pattern = stri_trans_general(str = tolower(input$search_study), id = "Latin-ASCII")) |
                     str_detect(title_clean,
                                pattern = stri_trans_general(str = tolower(input$search_study), id = "Latin-ASCII")) |
                     str_detect(author_clean,
                                pattern = stri_trans_general(str = tolower(input$search_study), id = "Latin-ASCII"))) %>%
            select(title, study_description, study_type_label, status_label, first_author)),
        rownames = F,
        colnames = c("Titre", "Description", "Type","Statut", "Premier auteur"),
        options = list(
          dom = "itp",
          language = list(paginate = 
                            list('next' = "Précédent", 
                                 previous="Suivant"),
                          sEmptyTable = "Aucune étude ne correspond à votre recherche",
                          sInfo = "_TOTAL_ résultat(s)")
        )
      )
      
    }
    
    
    
  )
  
  
  cart_study = reactiveValues("selected" = c())
  tableProxy_study <- dataTableProxy('study_table')
  
  
  observeEvent(input$add_cart_study, {
    
    if (is.null(input$search_study) | input$search_study == "") {
      
      tbl = data.table(
        study %>%
          left_join(status) %>%
          filter(status_label %in% input$status) %>%
          left_join(study_type) %>%
          filter(study_type_label %in% input$study_type) %>%
          mutate(desc_clean =  stri_trans_general(str = tolower(study_description), id = "Latin-ASCII"),
                 title_clean = stri_trans_general(str = tolower(title), id = "Latin-ASCII")) %>%
          select(study_id, title, study_description, study_type_label, status_label, first_author))
    } else {
      tbl = data.table(
        study %>%
          left_join(status) %>%
          filter(status_label %in% input$status) %>%
          left_join(study_type) %>%
          filter(study_type_label %in% input$study_type) %>%
          mutate(desc_clean =  stri_trans_general(str = tolower(study_description), id = "Latin-ASCII"),
                 title_clean = stri_trans_general(str = tolower(title), id = "Latin-ASCII"),
                 author_clean = stri_trans_general(str = tolower(first_author), id = "Latin-ASCII")) %>%
          filter(str_detect(desc_clean,
                            pattern = stri_trans_general(str = tolower(input$search_study), id = "Latin-ASCII")) |
                   str_detect(title_clean,
                              pattern = stri_trans_general(str = tolower(input$search_study), id = "Latin-ASCII")) |
                   str_detect(author_clean,
                              pattern = stri_trans_general(str = tolower(input$search_study), id = "Latin-ASCII"))) %>%
          select(study_id, title, study_description, study_type_label, status_label, first_author))
      
      
    }
    
    if (!is.null(input$study_table_rows_selected)) {
      
      if (is.null(cart_study$selected)) {
        cart_study$selected = tbl[input$study_table_rows_selected,] %>% 
          left_join(id_management) %>% 
          left_join(script) %>% 
          mutate(Depuis = title,
                 etude = 1) %>% 
          select(script_id, Depuis, etude)
        
        
      }else {
        
        cart_study$selected = plyr::rbind.fill(cart_study$selected, 
                                               tbl[input$study_table_rows_selected,] %>% 
                                                 left_join(id_management) %>% 
                                                 left_join(script) %>% 
                                                 mutate(Depuis = title,
                                                        etude = 1) %>% 
                                                 select(script_id, Depuis, etude))
        
      }
      
  
      selectRows(proxy = tableProxy_study,
                 selected = c())
    }
    
  })
  

  
  # Cart accueil ----
  
  cart_from_accueil = reactiveValues(
    "study1" = data.frame(),
    "study2" = data.frame(),
    "study3" = data.frame(),
    "script1" = data.frame(),
    "script2" = data.frame(),
    "script3" = data.frame(),
    "all_selected" = data.frame())
    
  

  
  observe({
    
    if (input$script1_cart == T){
      
      cart_from_accueil$script1 = script %>% 
          arrange(desc(publication_date)) %>% 
          head(1) %>% 
          select(script_id) %>% 
          mutate(Depuis = "Accueil") %>% 
          mutate(etude = 0)
      
      cart_from_accueil$all_selected = data.frame(plyr::rbind.fill(cart_from_accueil$script1,
                                                                   cart_from_accueil$script2,
                                                                   cart_from_accueil$script3,
                                                                   cart_from_accueil$study1,
                                                                   cart_from_accueil$study2,
                                                                   cart_from_accueil$study3)) %>% 
        filter(!is.na(script_id))
      
      
    }
    else {
      
      cart_from_accueil$script1 = data.frame("script_id" = NA, "Depuis" = NA, "etude" = NA)
      
      cart_from_accueil$all_selected = data.frame(plyr::rbind.fill(cart_from_accueil$script1,
                                                                   cart_from_accueil$script2,
                                                                   cart_from_accueil$script3,
                                                                   cart_from_accueil$study1,
                                                                   cart_from_accueil$study2,
                                                                   cart_from_accueil$study3)) %>% 
        filter(!is.na(script_id))
      
      
      
           
    }
  })
  
  
  
  observe(
    
    if (input$script2_cart == T){
      
      cart_from_accueil$script2 = 
        (script %>% 
           arrange(desc(publication_date)) %>% 
           head(2) %>% 
           select(script_id))[2,] %>% 
        mutate(Depuis = "Accueil") %>% 
        mutate(etude = 0)
      
      cart_from_accueil$all_selected = data.frame(plyr::rbind.fill(cart_from_accueil$script1,
                                                                   cart_from_accueil$script2,
                                                                   cart_from_accueil$script3,
                                                                   cart_from_accueil$study1,
                                                                   cart_from_accueil$study2,
                                                                   cart_from_accueil$study3)) %>% 
        filter(!is.na(script_id))
      
      
      
    }
    else {
      
      cart_from_accueil$script2 = data.frame("script_id" = NA, "Depuis" = NA, "etude" = NA)
      
      cart_from_accueil$all_selected = data.frame(plyr::rbind.fill(cart_from_accueil$script1,
                                                                   cart_from_accueil$script2,
                                                                   cart_from_accueil$script3,
                                                                   cart_from_accueil$study1,
                                                                   cart_from_accueil$study2,
                                                                   cart_from_accueil$study3)) %>% 
        filter(!is.na(script_id))
      
      
    }
  )
  
  observe(
    
    if (input$script3_cart == T){
      
      cart_from_accueil$script3 = 
        (script %>% 
          arrange(desc(publication_date)) %>% 
          head(3) %>% 
          select(script_id))[3,] %>% 
          mutate(Depuis = "Accueil") %>% 
          mutate(etude = 0)
      
      cart_from_accueil$all_selected = data.frame(plyr::rbind.fill(cart_from_accueil$script1,
                                                                   cart_from_accueil$script2,
                                                                   cart_from_accueil$script3,
                                                                   cart_from_accueil$study1,
                                                                   cart_from_accueil$study2,
                                                                   cart_from_accueil$study3)) %>% 
        filter(!is.na(script_id))
      
      
      
    }
    else {
      
      cart_from_accueil$script3 = data.frame("script_id" = NA, "Depuis" = NA, "etude" = NA)
 
      cart_from_accueil$all_selected = data.frame(plyr::rbind.fill(cart_from_accueil$script1,
                                                                   cart_from_accueil$script2,
                                                                   cart_from_accueil$script3,
                                                                   cart_from_accueil$study1,
                                                                   cart_from_accueil$study2,
                                                                   cart_from_accueil$study3)) %>% 
        filter(!is.na(script_id))
      
      
    }
    )
  
  
    observe(
    
    if (input$study1_cart == T){
      
      cart_from_accueil$study1 = 
        study %>% 
          arrange(desc(date)) %>% 
          head(1) %>% 
          select(study_id, Depuis = title) %>% 
          left_join(id_management) %>% 
          select(-study_id) %>% 
          mutate(etude = 1)
      
      cart_from_accueil$all_selected = data.frame(plyr::rbind.fill(cart_from_accueil$script1,
                                                                   cart_from_accueil$script2,
                                                                   cart_from_accueil$script3,
                                                                   cart_from_accueil$study1,
                                                                   cart_from_accueil$study2,
                                                                   cart_from_accueil$study3)) %>% 
        filter(!is.na(script_id))
      
      
    }
    else {
      
      cart_from_accueil$study1 = data.frame("script_id" = NA, "Depuis" = NA, "etude" = NA)
      
      cart_from_accueil$all_selected = data.frame(plyr::rbind.fill(cart_from_accueil$script1,
                                                                   cart_from_accueil$script2,
                                                                   cart_from_accueil$script3,
                                                                   cart_from_accueil$study1,
                                                                   cart_from_accueil$study2,
                                                                   cart_from_accueil$study3)) %>% 
        filter(!is.na(script_id))
      
      
      
      
    }
)
    
    
    
    
    observe(
    
    if (input$study2_cart == T){
      
      cart_from_accueil$study2 = (study %>% 
          arrange(desc(date)) %>% 
          head(2) %>% 
          select(study_id, Depuis = title))[2,] %>% 
          left_join(id_management) %>% 
          select(-study_id) %>% 
          mutate(etude = 1)
      
      cart_from_accueil$all_selected = data.frame(plyr::rbind.fill(cart_from_accueil$script1,
                                                                   cart_from_accueil$script2,
                                                                   cart_from_accueil$script3,
                                                                   cart_from_accueil$study1,
                                                                   cart_from_accueil$study2,
                                                                   cart_from_accueil$study3)) %>% 
        filter(!is.na(script_id))
      
      
   } else {
      
      cart_from_accueil$study2 = data.frame("script_id" = NA, "Depuis" = NA, "etude" = NA)
      
      cart_from_accueil$all_selected = data.frame(plyr::rbind.fill(cart_from_accueil$script1,
                                                                   cart_from_accueil$script2,
                                                                   cart_from_accueil$script3,
                                                                   cart_from_accueil$study1,
                                                                   cart_from_accueil$study2,
                                                                   cart_from_accueil$study3)) %>% 
        filter(!is.na(script_id))
    }
      
      
  )
    
    observe(
    
    if (input$study3_cart == T){
      
      cart_from_accueil$study3 =   (study %>% 
           arrange(desc(date)) %>% 
           head(3) %>% 
           select(study_id, Depuis = title))[3,] %>% 
          left_join(id_management) %>% 
          select(-study_id) %>% 
          mutate(etude = 1)
      
      cart_from_accueil$all_selected = data.frame(plyr::rbind.fill(cart_from_accueil$script1,
                                                                   cart_from_accueil$script2,
                                                                   cart_from_accueil$script3,
                                                                   cart_from_accueil$study1,
                                                                   cart_from_accueil$study2,
                                                                   cart_from_accueil$study3)) %>% 
        filter(!is.na(script_id))
      
      
      
    }

    else {
      
      cart_from_accueil$study3 = data.frame("script_id" = NA, "Depuis" = NA, "etude" = NA)
      
      cart_from_accueil$all_selected = data.frame(plyr::rbind.fill(cart_from_accueil$script1,
                                                                   cart_from_accueil$script2,
                                                                   cart_from_accueil$script3,
                                                                   cart_from_accueil$study1,
                                                                   cart_from_accueil$study2,
                                                                   cart_from_accueil$study3)) %>% 
        filter(!is.na(script_id))
      
      
      
      
    }
        
    )
    
 
# Panier final----

  output$selection = renderDT(cart_from_accueil$all_selected %>%
                                plyr::rbind.fill(cart_script$selected) %>%
                                plyr::rbind.fill(cart_study$selected) %>% 
                                left_join(script) %>% 
                                mutate(Depuis = ifelse(etude == 0 | is.na(etude), "EMPTY", Depuis)) %>% 
                                group_by(script_label, script_name) %>%
                                summarise(associated = paste(paste0(str_sub(Depuis,1, 20),"..."), 
                                                              collapse = ", ",
                                                              recycle0 = T)) %>% 
                                mutate(associated = str_replace_all(associated, "EMPTY..., ", ""),
                                       associated = str_replace_all(associated, "EMPTY...", ""),
                                       associated = str_replace_all(associated, "NA...", "")),
                              colnames = c("Script", "Nom du fichier", "Associé à "),
                              rownames = F,
                              options = list(
                                dom = "tp",
                                language = list(paginate = 
                                                  list('next' = "Précédent", 
                                                       previous="Suivant"),
                                                sEmptyTable = "Votre panier est encore vide !",
                                                sInfo = "_TOTAL_ résultat(s)")
                              )) 
  
  # Dépendences----
  
  
  output$dependences = renderDT(cart_from_accueil$all_selected %>%
                                plyr::rbind.fill(cart_script$selected) %>%
                                plyr::rbind.fill(cart_study$selected) %>% 
                                left_join(script) %>% 
                                select(script_id_initial = script_id, script_label_initial = script_label) %>% 
                                inner_join(requirements, by = c("script_id_initial" = "script_id")) %>% 
                                left_join(script, by = c("script_id_required" = "script_id")) %>% 
                                group_by(script_label, script_name) %>%
                                summarise(for_script = paste(paste0(str_sub(script_label_initial,1,40), "..."), collapse = (", "))),
                              colnames = c("Script requis", "Nom du fichier", "Nécessaire pour "),
                              rownames = F,
                              options = list(
                                dom = "tp",
                                language = list(paginate = 
                                                  list('next' = "Précédent", 
                                                       previous="Suivant"),
                                                sEmptyTable = "Les scripts de votre panier sont prêt à l'emploi ! Aucune dépendance n'a été trouvé.",
                                                sInfo = "_TOTAL_ résultat(s)")
                              )) 
  
  

  
  
    
    output$download_with_dep = downloadHandler(
      
      filename = paste0("SNDS_QueryHub_cart_with_dependencies_,",Sys.Date(),".tar"),
      
      content = function(file){
        tmpdir = tempdir()
        select = (cart_from_accueil$all_selected %>%
          plyr::rbind.fill(cart_script$selected) %>%
          plyr::rbind.fill(cart_study$selected) %>%
          left_join(script))$script_path %>%
          unique()


        dep = (cart_from_accueil$all_selected %>%
          plyr::rbind.fill(cart_script$selected) %>%
          plyr::rbind.fill(cart_study$selected) %>%
          left_join(script) %>%
          select(script_id_initial = script_id, script_label_initial = script_label) %>%
          inner_join(requirements, by = c("script_id_initial" = "script_id")) %>%
          left_join(script, by = c("script_id_required" = "script_id")))$script_path %>%
          unique()



        to_download = c(select, dep) %>% unique()

        file.copy(from = to_download, to = tmpdir)
        setwd(tempdir())
        
        
        tar(file,  files = (basename(to_download)))

      }
        
    )

    
    
    output$download_without_dep = downloadHandler(
      
      filename = paste0("SNDS_QueryHub_cart_no_dependencies_,",Sys.Date(),".tar"),
      
      content = function(file){
        tmpdir = tempdir()
        select = (cart_from_accueil$all_selected %>%
                    plyr::rbind.fill(cart_script$selected) %>%
                    plyr::rbind.fill(cart_study$selected) %>%
                    left_join(script))$script_path %>%
          unique()
        
        
        
        to_download = select 
        file.copy(from = to_download, to = tmpdir)
        setwd(tempdir())
        
        
        tar(file,  files = (basename(to_download)))
        
      }
      
    )
  
# Ne pas toucher ----  
}
  
