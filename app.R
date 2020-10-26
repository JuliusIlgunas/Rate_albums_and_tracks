# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(shiny)
library(shinyjs)
library(ggplot2)
library(spotifyr)
library(tidyverse)
library(knitr)
library(lubridate)
library(rhandsontable)
library(plotly)
library(ggimage)
#devtools::install_github('womeimingzi11/spotifyr')

myclientid <- 'e84d99dc52424232882d7d8ad7425190'
myclientsecret <- '19c49108a68944b59555f12cac8851c6'


Sys.setenv(SPOTIFY_CLIENT_ID = myclientid)
Sys.setenv(SPOTIFY_CLIENT_SECRET = myclientsecret)

access_token <- get_spotify_access_token()

ui <-  navbarPage("Rate albums and tracks",
                  
                  
       tabPanel("Artist and album search",
                column(3, 
                       textInput("text", h3("Enter artist name:"))
                       ), 
    
                column(4, 
                       actionButton('testb','Confirm artist', icon = icon("ok", lib = "glyphicon")),
                       actionButton('add','Add album', icon = icon("plus", lib = "glyphicon")),
                       checkboxInput("checkEP", label = "Singles/EPs", value = FALSE),
                       actionButton('clear','Clear text', icon = icon("remove", lib = "glyphicon"))
                       ),
                
                # Create a new Row in the UI for selectInputs
   
                selectInput("alb",
                           "Studio albums:",
                           "All"),
        
                # Create a new row for the table.
                h3("Artist tracks", noWS = 5),
                DT::dataTableOutput("table"),
                h3("Artist discography"),
                DT::dataTableOutput("table2")),
    
        tabPanel("My albums",
                 fileInput("file1", "Import your existing playlist",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                h3("Newly added albums"),
                DT::dataTableOutput("table3"),
                h3("My album playlist summary"),
                DT::dataTableOutput("table4")
                #tableOutput("table35"),
                 ),
    
  
        tabPanel("My playlist",  
                 downloadButton("downloadData", "Download Playlist"),
                 
                 actionButton("save", "Save playlist", icon = icon("floppy-disk", lib = "glyphicon")),
                 
                 selectInput("sort",
                             "Sort by:",
                             c("", "Track No.", "Track", "Rating",
                               "Artist","Album","Length", "Release year")),
                 
                 checkboxInput("checkA", label = "Ascending", value = TRUE),
                 
                 uiOutput("year"),
                 #("checkD", label = "Descending", value = TRUE),
                 div(style = 'overflow-x: scroll', rHandsontableOutput("example"))),
       
       tabPanel("Stats",
                h3("Total number of albums/releases each year"),
                plotOutput("albumsbars"),
                h3("Total number of songs each year"),
                plotOutput("songsbars"),
                plotOutput("coverplot")),
       tabPanel("Test",
                h3("newedit"),
                tableOutput("testtable1"),
                h3("fulltable"),
                tableOutput("testtable2"),
                h3("values"),
                tableOutput("testtable3"),
                h3("newvalues"),
                tableOutput("testtable4"))
       )

server <- function(input, output, session) {
 
    ####################### PRIMARY SEARCH DATA ##################################################
   
  
    #observeEvent(input$ep,{
  
  
    myData <- reactive({
              
    if(input$checkEP == TRUE) {
      
      data <- get_artist_audio_features(input$text,
                                        include_groups = "single",
                                        dedupe_albums = F,
                                        return_closest_artist = T)
            
    } else {
      
      data <- get_artist_audio_features(input$text,
                                        #include_groups = c("album", "single"),
                                        dedupe_albums = F,
                                        return_closest_artist = T)
    }
      
      data
      
    })
    
    ###################### TABLE AFTER CONFIRMING THE ARTIST #####################################
    
    pdata <- reactive({
        
        #req(input$text1 != "")
      
        data1 <- myData()
        
        u <- unlist(data1$album_images)
        u <- unname(u[names(u) == "url2"])
        
        data1 <- data1 %>% mutate(alburl = u)
        
        if (input$alb != "All") {
            data1 <- data1[data1$album_name == input$alb,] %>%
              distinct(tolower(track_name), tolower(album_name), .keep_all = TRUE) %>%
              mutate(length = format( as.POSIXct(Sys.Date())+duration_ms/1000, "%M:%S")) %>% 
              select(disc_number, track_number, track_name, artist_name, album_name, length,
                     album_release_year, artist_id, album_id, alburl) 
            
            
        }
        else{
            data1 <- data1  %>%
              distinct(tolower(track_name),tolower(album_name), .keep_all = TRUE)%>% 
              mutate(length = format( as.POSIXct(Sys.Date())+duration_ms/1000, "%M:%S")) %>% 
              select(disc_number, track_number, track_name, artist_name, album_name, length,
                     album_release_year, artist_id, album_id, alburl)
            
            #print("If this is the correct artist click 'Confirm Artist' ")
        }
        colnames(data1) <- c("Disc No.","Track No.", "Track", "Artist", "Album", "Length", "Release year",
                             "Artist id", "Album id", "Album image url")
        data1$Track <- as.character(data1$Track)
        data1$Artist <- as.character(data1$Artist)
        data1$Album <- as.character(data1$Album)
        
        data1$Rating <- NA
        data1$Rating <- as.numeric(round(data1$Rating),2)
        data1
        
        })
    
       values <- reactiveVal(data.frame())
       myvalues <- reactiveVal(data.frame())
    
   
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
      
      req(input$text != "")
      
        pdata() %>% select(`Track No.`, Track, Artist, Album, Length, `Release year`)
        
       }))
    
    ##################### TABLE BELOW OF ALL THE ALBUMS AND RELEASE YEAR ###############################   
    
    output$table2 <- DT::renderDataTable(DT::datatable({
      
      req(input$text != "")
        
        data2 <- myData()
        albums <- data2 %>% select(album_name, album_release_year) %>%
            group_by(album_name) %>% summarise_all(funs(max)) %>% arrange(desc(album_release_year))
        colnames(albums) <- c("Album", "Release year")
        albums
        
    }))
     
    ################### ALBUM SELECTION WHILE SEARCHING ####################################################
    
    observeEvent(input$testb,{
        
        data2 <- myData()
        ch <- unique(data2$album_name)
       
        updateSelectizeInput(session, "alb",
                         label = "Studio albums:",
                         choices = c("All",ch),
                         selected = c("All",ch)[1])
        
     })
    
    
    ################## ADD ALBUM BUTTON ####################################################################
    
    observeEvent(input$add,{
        
       currentThings = values()
       newThings = rbind(currentThings, pdata())
        
       values(newThings)
        
    })
    
    ################## CLEAR BUTTON #############################################################
    
    observeEvent(input$clear,{
      
      updateTextInput(session, "text", value = "")
      updateSelectizeInput(session, "alb", selected = "All")
      
    })
    
    ################## UPLOAD EXISTING PLAYLIST ############################################################
    
    myfile <- reactive({
      
      df <- read.csv(input$file1$datapath, header = T, check.names=FALSE)
      df$Track <- as.character(df$Track)
      df$Artist <- as.character(df$Artist)
      df$Album <- as.character(df$Album)
      df$`Release year` <- as.numeric(as.character(df$`Release year`))
      df$Rating <- as.numeric(df$Rating)
      df
        
      })
    
    observeEvent(input$file1,{
      
      myvalues(myfile())
      
    })
    
   # observeEvent(input$file1,{
   #   
    #  myvalues()
    #  
    #})
    
    
   ################## ALBUM SUMMARY TABLE AFTER ADDING ALBUMS FROM SEARCH ##################################
    
    output$table3 <- DT::renderDataTable(DT::datatable({
      
      #datae <- data.frame()
      
      req(input$text != "")
      req(input$add)
      
      choices <- values() %>% select(Artist, Album, `Release year`) %>%
        group_by(Album) %>% mutate(`Release year` = max(`Release year`)) %>%
        ungroup() %>% 
        distinct(Album, Artist, .keep_all = TRUE) %>%
        arrange(desc(Artist))
      
      colnames(choices) <- c("Artist", "Album", "Release year")
      choices
      
     
      #if(!exists("myfile()")){
        
      #  choices3 <- rbind(datae,choices)
        
     # } else if (!exists("values()")){
        
      #  choices3 <- rbind(datae,choices2)
      
     # } else{
        
      #  choices3 <- rbind(choices,choices2)
        
     # }
      #return(choices3)
      
      
      
    }))
    
    ####################### COMBINE EXISTING AND NEW PLAYLIST ######################################
    
    fulltable <- reactive({
      
      full <- rbind(values(),myvalues())
      full$`Release year` <- as.character(round(full$`Release year`, digits = 0)) 
      full <- full %>% arrange(tolower(Album), `Disc No.`, `Track No.`)
      
       if(input$sort != "") {
        
        if(input$checkA == TRUE){
        
          full <- full[do.call('order', c(full[paste(input$sort)], list(decreasing=FALSE))),]
          
        } else {
        
         full <- full[do.call('order', c(full[paste(input$sort)], list(decreasing=TRUE))),]
          
        }
        
        #full <-  full %>% arrange(paste(input$sort))
          
       }
      
      if(input$year != "All"){
        
        full <- full %>% filter(`Release year`  == input$year)
      }
      
      
      #full$Rating <- NA
      #full$Rating <- as.numeric(full$Rating)
          full
      
    })
    
    ######################  EDITED PLAYLIST ########################################################
    
    newvalues <- reactiveVal(data.frame())
    
    newedit <- reactive({
      
      edit <- rbind(values(),newvalues())
      
      edit <- edit %>% distinct(Rating, tolower(Track), tolower(Album), .keep_all = TRUE) %>%
                       arrange(tolower(Album), is.na(Rating)) %>%
                       distinct(tolower(Album), tolower(Track), .keep_all = TRUE) %>%
                       arrange(tolower(Album), `Disc No.`, `Track No.`)
      
      
      if(input$sort != "") {
        
        if(input$checkA == TRUE){
          
          edit <- edit[do.call('order', c(edit[paste(input$sort)], list(decreasing=FALSE))),]
          
        } else {
          
          edit <- edit[do.call('order', c(edit[paste(input$sort)], list(decreasing=TRUE))),]
          
        }
        
        #full <-  full %>% arrange(paste(input$sort))
        
      }
      
      if(input$year != "All"){
        
        edit <- edit %>% filter(`Release year`  == input$year)
      }
      #edit[] <- lapply(edit, function(x) if(is.factor(x)) as.character(x) else x)
      #edit$`Release year` <- as.numeric(edit$`Release year`)
      #edit$Rating <- as.numeric(edit$Rating)
      edit
      
    })
    
    #########################
    
    output$year <- renderUI({
      
      #full <-  fulltable() %>%  select(`Release year`)
      #edit <- newedit() %>%  select(`Release year`)
      
      selectInput("year", "Select year:", c("All",
                                            #unique(c(full, edit))),
                                            paste(rev(seq(1920, 2020)), sep = ",")),
                                            selected = 1)
      })
    
    ###################### NONEXISTENT TABLE? ###############################################
    
    # output$table35 <- renderTable({
    #   
    #   choices_full <- fulltable() %>% select(Artist, Album, `Release year`, Rating) %>%
    #     group_by(Album) %>% mutate(`Release year` = max(`Release year`), Rating = round(mean(Rating),3)) %>%
    #     ungroup() %>% 
    #     distinct(Album, Artist, .keep_all = TRUE) %>%
    #     arrange(desc(Rating))
    #   
    #   colnames(choices_full) <- c("Artist", "Album", "Release year", "Rating")
    #   choices_full
    #   
    # })
    
    ##################### TABLE OF NEW AND IMPORTED ALBUMS #########################################
    
    output$table4 <- DT::renderDataTable(DT::datatable({
     
      if(input$save == 0){
        
        req(input$file1)
        
        choices_full <- fulltable() %>% dplyr::select(Artist, Album, `Release year`, Rating) %>%
          group_by(Album) %>% mutate(`Release year` = max(`Release year`), Rating = round(mean(Rating),2)) %>%
          ungroup() %>% 
          distinct(Album, Artist, .keep_all = TRUE) %>%
          arrange(desc(Rating))
        
        colnames(choices_full) <- c("Artist", "Album", "Release year", "Rating")
        choices_full
        
      } else {
       
      #req(input$text != "")
      
      choices_full <- newedit() %>% dplyr::select(Artist, Album, `Release year`, Rating) %>%
        group_by(Album) %>% mutate(`Release year` = max(`Release year`), Rating = round(mean(Rating),2)) %>%
        ungroup() %>% 
        distinct(Album, Artist, .keep_all = TRUE) %>%
        arrange(desc(Rating))
      
      colnames(choices_full) <- c("Artist", "Album", "Release year", "Rating")
      choices_full
      
      }
      
    }))
   
    #output$example <- renderTable({
      
    #    fulltable()
      
    #})
    
    ##################### EDITABLE TABLE ############################################################
  
    output$example <- renderRHandsontable({
      
        req(fulltable())
      
      if(input$save == 0){
      
        fullhandson <- fulltable() %>% select(`Track No.`, Track, Rating,
                                              Artist, Album, Length, `Release year`)
       
      }  else {
        
        fullhandson <- newedit() %>% select(`Track No.`, Track, Rating,
                                            Artist, Album, Length, `Release year`)
        
      }
    rhandsontable(fullhandson, stretchH = "all")
        
        #fupdate2 <- isolate(fulltable())
        #print(fupdate2)
      
        #newvalues(fupdate2)
    })
    
    ##################### TEST TABLES ######################################################
    
    output$testtable1 <- renderTable({
      
      newedit()
      
    })
    
    output$testtable2 <- renderTable({
      
      fulltable()
      
    })
    
    output$testtable3 <- renderTable({
      
      values()
      
    })
    
    output$testtable4 <- renderTable({
      
      newvalues()
      
    })
    
    ##################### SAVE PLAYLIST BBUTTON ######################################################
    
    observeEvent(input$save,
                 {
                   
                   if (!is.null(isolate(input$example)))
                   {
                     #Convert to R object
                     x <- hot_to_r(isolate(input$example))
                     
                     newcols <- c("Disc No.","Artist id", "Album id", "Album image url")
                     
                     x[,newcols] <- fulltable()[,newcols]
                     #write.csv(x, file = 'MyPlaylist.csv', row.names=FALSE)
                     
                     newvalues(x)
                     
                     
                   }
                 }
    )
    
    ##################### DOWNLOAD PLAYLIST BBUTTON ##################################################
    
    output$downloadData <- downloadHandler(
      filename = "MyPlaylist.csv",
      content = function(file) {
        write.csv(newvalues(), file, row.names = FALSE)
      }
    )
    
    ##################### TOTAL ALMUMS EACH YEAR PLOT #################################################
    
    output$albumsbars <- renderPlot({
      
      if(input$save == 0){
        
        choices_full <- fulltable() %>% dplyr::select(Artist, Album, `Release year`) %>%
          group_by(Album) %>% mutate(`Release year` = max(`Release year`)) %>% ungroup() %>% 
          distinct(Album, Artist, .keep_all = TRUE)
        
        p <- ggplot(choices_full,aes(x=factor(`Release year`))) +
          geom_bar(stat = "count", width=0.7, fill="steelblue", position = "dodge") + 
          xlab("Release Year") + ylab("Total number of albums") +
          geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)
        
        #ggplotly(p)
        p
        
      }  else {
        
        choices_full <- newedit() %>% dplyr::select(Artist, Album, `Release year`) %>%
          group_by(Album) %>% mutate(`Release year` = max(`Release year`)) %>% ungroup() %>% 
          distinct(Album, Artist, .keep_all = TRUE)
        
        p <- ggplot(choices_full,aes(x=factor(`Release year`))) + 
          geom_bar(stat = "count", width=0.7, fill="steelblue", position = "dodge") + 
          xlab("Release Year") + ylab("Total number of albums") +
          geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)
        
        #ggplotly(p)
        p
      }
      
    })
    
    ##################### TOTAL SONGS EACH YEAR PLOT #################################################
    
    output$songsbars <- renderPlot({
      
      if(input$save == 0){
        
        p <- ggplot(fulltable(),aes(x=factor(`Release year`))) +
             geom_bar(stat = "count", width=0.7, fill="steelblue", position = "dodge") + 
             xlab("Release Year") + ylab("Total number of songs") +
             geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)
        
        #ggplotly(p)
        p
        
      }  else {
        
        p <- ggplot(newedit(),aes(x=factor(`Release year`))) + 
             geom_bar(stat = "count", width=0.7, fill="steelblue", position = "dodge") + 
             xlab("Release Year") + ylab("Total number of songs") +
             geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)
        
        #ggplotly(p)
        p
      }
      
    })
    
    ###############################################################################
    
    output$coverplot <- renderPlot({
    
    if(input$save == 0){  
      
    choices_full <- fulltable() %>% dplyr::select(Artist, Album,
                                                `Release year`, Rating,
                                                `Album image url`) %>%
      group_by(Album) %>% mutate(`Release year` = max(`Release year`), 
                                 Rating = round(mean(Rating),2)) %>%
      ungroup() %>% 
      distinct(Album, Artist, .keep_all = TRUE) %>%
      arrange(desc(`Release year`))
    
    colnames(choices_full) <- c("Artist", "Album", "Release year", "Rating", "Album image url")
    #choices_full
    choices_full$sequence <- sequence(rle(choices_full$`Release year`)$lengths)
    
    choices_full$y <- rnorm(nrow(choices_full))
    
    p <- ggplot(choices_full, aes(`Release year`,sequence)) + 
      geom_image(aes(image=`Album image url`), size=.1, by = "height", asp = 1.75) +
      scale_size_identity() +
      ylim(1,10) +
      #scale_y_continuous(breaks = c(seq(from = 1, to = 5, by = 1))) +
      #coord_fixed(ratio = 0.5) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
    #ggplotly(p)
    
    p
    
    } else {
      
    choices_full <- newedit() %>% dplyr::select(Artist, Album,
                                                  `Release year`, Rating,
                                                  `Album image url`) %>%
        group_by(Album) %>% mutate(`Release year` = max(`Release year`), 
                                   Rating = round(mean(Rating),2)) %>%
        ungroup() %>% 
        distinct(Album, Artist, .keep_all = TRUE) %>%
        arrange(desc(`Release year`))
      
    colnames(choices_full) <- c("Artist", "Album", "Release year", "Rating", "Album image url")
    #choices_full
    choices_full$sequence <- sequence(rle(choices_full$`Release year`)$lengths)
    
    choices_full$y <- rnorm(nrow(choices_full))
    
    p <- ggplot(choices_full, aes(`Release year`,sequence)) + 
      geom_image(aes(image=`Album image url`), size=.1, by = "height", asp = 1.75) +
      scale_size_identity() +
      ylim(1,10) +
      #scale_y_continuous(breaks = c(seq(from = 1, to = 5, by = 1))) +
      #coord_fixed(ratio = 0.5) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
    #ggplotly(p)
    
    p
      
    }
    
    })
    
    ###############################################################################
}

options(shiny.sanitize.errors = TRUE)
# Run the application 
shinyApp(ui = ui, server = server)
