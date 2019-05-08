shinyServer(function(input, output) {
    # Queue management ----
    remainingQueue <- eventReactive(
        eventExpr = input$submit_btn,
        label = "remainingQueue_eventRxtive",
        valueExpr = {
            # Scape user input to see what images user has already reviewed
            img_ids_complete <- list.files(path = kDIR_RES_OUT) %>% fp_stem()

            # Get remaining portion of queue
            img_ids_todo <- sample_df[sample_df$img %ni% img_ids_complete, "img"]
            img_ids_todo
        }
    )

    output$imgIdUi <- renderUI({
        todo <- remainingQueue()
        selectInput("imgIdIn", "Img Id:", choices = todo, selected = todo[1])
    })

    # Original Note ----
    output$noteTxt <- renderPrint({
        sample_df[sample_df$img == input$imgIdIn, "Report Text"] %>% as.character() %>% cat()
    })

    # Annotation recording ----
    usrImpressionDf <- shiny::reactive({
        impression_df <- data.frame(
            img_id = input$imgIdIn %||% NA,
            fx = input$isFx,
            prior_imaging = input$priorImaging,
            indication = input$cc,
            other = input$otherInd
        )

        return(impression_df)
    })

    observeEvent(
        eventExpr = input$submit_btn,
        label = "Save User Data",
        handlerExpr = {
            submit_data_df <- usrImpressionDf()
            Write(submit_data_df, fp = file.path(kDIR_RES_OUT, str_c(input$imgIdIn, ".csv")))
            cat("data saved")
        },
        priority = 10  # Execute before remainingQueue and all others so they act on the next img
    )
})
