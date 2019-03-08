library(shiny)
library(plotly)

server <- function(input, output, session) {

  data_list <- reactive({
    load('data_list.RData', data_list <- new.env()) 
    data_list
    })

  best_model <- reactive({
    best_cols = c("n", "nac")
    df = data_list()$best_model
    df = missing_cols_handler(best_cols, df)
    df$prepost = as.factor(1)
    df
    })

  temp_df <- reactive({
    util_cols = c("using_fuel_oil", "using_sqft")
    df = data_list()$utility
    colnames(df)[colnames(df) == 'oat'] <- 'OAT'
    df = missing_cols_handler(util_cols, df)
    df = fixed_time(df)
    })


  binfo_df <- reactive({
    data_list()$building_info
    })

  post_df <- reactive({
    data_list()$post_model
  })

  lean_df <- reactive({
    data_list()$lean_rank
  })

  co2eui_df <- reactive({
    data_list()$co2eui_rank
  })

  breakdown_df <- reactive({
    data_list()$co2eui_breakdown
  })

  energy_df <- reactive({
    data_list()$energy_breakdown
    })

  b_df <- reactive({
    if(!is.null(binfo_df()))
    { 
      temp_index = unique(match(temp_df()$bdbid, binfo_df()$bdbid))
      b_name = binfo_df()$building_name[temp_index]
      b_id = binfo_df()$bdbid[temp_index]
      comb = paste(b_id, b_name, sep = ' - ')
      b_df = data.frame(bdbid = b_id, name = comb)
    }else
    { 
      bdbid = unique(temp_df()$bdbid)
      b_df = data.frame(bdbid = bdbid, name = bdbid)
    }
    b_df
  })

  output$first <- renderUI({
    tagList(
      selectInput('bdbid', 'Choose Facility', b_df()$name, selected = b_df()$name[1], multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
    )
  })

  binfo_output_list <- reactive({
    if (is.null(binfo_df()))
    {
      list(binfo_df1 = data.frame(), binfo_df1 = data.frame())
    }else
    { 
      binfo_table(binfo_df(), bdbid_n())
    }
  })

  bdbid_n <- reactive({
    b_df()$bdbid[b_df()$name == input$bdbid]
  })

  observeEvent(input$prevBin, {
        current <- which(b_df()$bdbid == bdbid_n())
        if(current > 1){
            updateSelectInput(session, "bdbid",
                              choices = b_df()$name,
                              selected = b_df()$name[current - 1])
        }
        #click_n() = click_n() + 1
  })
    
  observeEvent(input$nextBin, {
        current <- which(b_df()$bdbid  == bdbid_n())
        if(current < length(b_df()$name)){
            updateSelectInput(session, "bdbid",
                              choices = b_df()$name,
                              selected = b_df()$name[current + 1])
        }
        #click_n() = click_n() + 1
  })


  #############################################
  ################# Calculator ################
  #############################################

  ################# Energy Dependent ###############
  per_num_list_elec <- reactive({
    per_num_func(lean_df(), bdbid_n(), 'Elec')
    })

  per_num_list_fuel <- reactive({
    per_num_func(lean_df(), bdbid_n(), 'Fuel')
    })

  ################# Energy Independent ###############

  energy_break_plot <- reactive({
    if (bdbid_n() %in% unique(energy_df()$bdbid))
    {
      df = subset(energy_df(), energy_df()$bdbid == bdbid_n())
      p = energy_break_pie_chart(df)
      p$break_table = make_break_table(df)
      return(p)
    }else
    {
      p = plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No usage points')
      return(list(site_p = p, source_p = p, break_table = data.frame()))
    }
    })

  area_info <- reactive({
    flag_area = check_sqft_na(binfo_output_list()$binfo_df2)
    if (flag_area)
    {
        return(list(flag_area = flag_area, area = binfo_output_list()$binfo_df2['Gross Square Feet',]))
    }else
    {
        return(list(flag_area = flag_area))
    }
  })

  b_name  <- reactive({get_building_name(binfo_df(), bdbid_n())})

  co2_lean_plot <- reactive({
    if(is.null(co2eui_df()))
    {
      return(plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No data points'))
    }else
    {
        co2_rank = co2_rank_get(co2eui_df(), bdbid_n())
        return(percent_figure(co2_rank, co2_flag = TRUE))
    }
  })

  co2_break_df <- reactive({
    co2_value_get(breakdown_df(), bdbid_n())
  })

  binfo_output_df2 <- reactive({
    binfo_df2 = binfo_output_list()$binfo_df2
    binfo_df2[3,] = round(as.numeric(binfo_df2[3,]), 2)
    binfo_df2[3,] = prettyNum(binfo_df2[3,], big.mark = ",", format = 'f')
    binfo_df2
  })

  #############################################
  ################# ELEC OUTPUT ###############
  #############################################

  output$elec_binfo_df1 <- renderTable({
    binfo_output_list()$binfo_df1
  }, align = 'l', colnames = FALSE, rownames = TRUE, width = "auto")

  output$elec_binfo_df2 <- renderTable({
    binfo_output_df2()
  }, align = 'l', colnames = FALSE, rownames = TRUE, width = "auto")

  #timeseries
  output$elec_timeseries <- renderPlotly({

    if (flag_func(temp_df(), bdbid_n(), 'Elec'))
    { 
      util = subset(temp_df(), bdbid == bdbid_n() & energy_type == 'Elec')
      plot_timeseries(util, 'Elec')
    }else
    {
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No usage points for Elec')
    }
  })

  #prameter plot
  output$elec_param_plot <- renderPlotly({main_plot(temp_df(), best_model(), bdbid_n(), 'Elec', b_name())})


  output$elec_sqft <- renderText({
    if (flag_func(temp_df(), bdbid_n(), 'Elec'))
      { 
        sqft_col = subset(temp_df()$using_sqft, temp_df()$bdbid == bdbid_n() & temp_df()$energy_type == 'Elec')
        sqft_message(sqft_col)
      }else
      {
        "No usage points for Elec."
      }
    })

  output$elec_params_df <- renderTable({
      params_table(best_model(), bdbid_n(), 'Elec')
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$elec_stat_df <- renderTable({
      stat_table(best_model(), bdbid_n(), 'Elec')
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$elec_post_df <- renderTable({
    if (!flag_func(post_df(), bdbid_n(), 'Elec'))
    {
      return(NULL)
    }
    n = ifelse(flag_func(best_model(), bdbid_n(), 'Elec'), subset(best_model()$n, best_model()$bdbid == bdbid_n() & best_model()$energy_type == 'Elec'), 0)
    post_output_df_server(post_df(), bdbid_n(), 'Elec', area_info(), n)
  }, align = 'l', rownames = TRUE, colnames = TRUE, width = "auto", digits = 7)

  #lean rank plot
  output$elec_lean_plot <- renderPlotly({per_num_list_elec()$percent_fig})

  #lean rank table
  output$elec_numeric_lean_df <- renderTable({
      per_num_list_elec()$numeric_df
  }, align = 'l', rownames = TRUE, colnames = TRUE, width = "auto", digits = NULL)

  #co2_eui_lean plot
  output$elec_co2eui_lean_plot <- renderPlotly({
    co2_lean_plot()
  })

  #co2e breakdown table
  output$elec_co2e_break_df <- renderTable({
    co2_break_df()
    }, align = 'l', rownames = TRUE, colnames = TRUE, width = "auto", digits = NULL)

  output$elec_site_break <- renderPlotly({
    energy_break_plot()$site_p
    })

  output$elec_source_break <- renderPlotly({
    energy_break_plot()$source_p
    })

  output$elec_break_table <- renderTable({
    energy_break_plot()$break_table
    }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = NULL)

  #############################################
  ################# FUEL OUTPUT ###############
  #############################################

  output$fuel_binfo_df1 <- renderTable({
    binfo_output_list()$binfo_df1
  }, align = 'l', colnames = FALSE, rownames = TRUE, width = "auto")

  output$fuel_binfo_df2 <- renderTable({
    binfo_output_df2()
  }, align = 'l', colnames = FALSE, rownames = TRUE, width = "auto")

  #timeseries
  output$fuel_timeseries <- renderPlotly({

    if (flag_func(temp_df(), bdbid_n(), 'Fuel'))
    { 
      util = subset(temp_df(), bdbid == bdbid_n() & energy_type == 'Fuel')
      plot_timeseries(util, 'Fuel')
    }else
    {
      plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = 'No usage points for Fuel')
    }
  })

  #prameter plot
  output$fuel_param_plot <- renderPlotly({main_plot(temp_df(), best_model(), bdbid_n(), 'Fuel', b_name())})


  output$fuel_sqft <- renderText({
    if (flag_func(temp_df(), bdbid_n(), 'Fuel'))
      { 
        sqft_col = subset(temp_df()$using_sqft, temp_df()$bdbid == bdbid_n() & temp_df()$energy_type == 'Fuel')
        sqft_message(sqft_col)
      }else
      {
        "No usage points for Fuel."
      }
    })

  output$fuel_params_df <- renderTable({
      params_table(best_model(), bdbid_n(), 'Fuel')
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$fuel_stat_df <- renderTable({
      stat_table(best_model(), bdbid_n(), 'Fuel')
  }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)

  output$fuel_post_df <- renderTable({
    #figure()$'Elec'$post_output_df
    if (!flag_func(post_df(), bdbid_n(), 'Fuel'))
    {
      return(NULL)
    }
    n = ifelse(flag_func(best_model(), bdbid_n(), 'Fuel'), subset(best_model()$n, best_model()$bdbid == bdbid_n() & best_model()$energy_type == 'Fuel'), 0)
    post_output_df_server(post_df(), bdbid_n(), 'Fuel', area_info(), n)
  }, align = 'l', rownames = TRUE, colnames = TRUE, width = "auto", digits = 7)

  #lean rank plot
  output$fuel_lean_plot <- renderPlotly({per_num_list_fuel()$percent_fig})

  #lean rank table
  output$fuel_numeric_lean_df <- renderTable({
      per_num_list_elec()$numeric_df
  }, align = 'l', rownames = TRUE, colnames = TRUE, width = "auto", digits = NULL)

  #co2_eui_lean plot
  output$fuel_co2eui_lean_plot <- renderPlotly({
    co2_lean_plot()
  })

  #co2e breakdown table
  output$fuel_co2e_break_df <- renderTable({
    co2_break_df()
    }, align = 'l', rownames = TRUE, colnames = TRUE, width = "auto", digits = NULL)

  output$fuel_site_break <- renderPlotly({
    energy_break_plot()$site_p
    })
  output$fuel_source_break <- renderPlotly({
    energy_break_plot()$source_p
    })

  output$fuel_break_table <- renderTable({
    energy_break_plot()$break_table
    }, align = 'l', rownames = FALSE, colnames = TRUE, width = "auto", digits = NULL)

  ############### Elec Vs Fuel ################

  output$elec_fuel_plot <- renderPlotly({
    if (length(best_model()$energy_type[best_model()$bdbid == bdbid_n()]) == 2)
    {
      return(elec_fuel_graph_func(temp_df(), best_model(), bdbid_n(), height = input$height_ef, width = input$width_ef))
    }else
    {
      return(plotly_empty(type = 'scatter', mode = 'markers') %>% layout(title = paste('Only one energy type for', bdbid_n())))
    }
  })

  #############################################
  ################ MULTI PLOTS ################
  #############################################

  output$elec_multi_plot <- renderUI({
    plot_output_list <- lapply(1:length(unique(temp_df()$bdbid)), function(i) {
      lapply(1:3, function(j){
      bdbid_n <- unique(temp_df()$bdbid)[i]

      if(j == 1)
      {
        plotname <- paste("plot", bdbid_n, sep="")
        output[[plotname]] <- renderPlotly({
          b_name = get_building_name(binfo_df(), bdbid_n)
          main_plot(temp_df(), best_model(), bdbid_n, 'Elec', b_name)
        })
        plotlyOutput(plotname)
      }else if(j == 2)
      { tablename1 <- paste("table", bdbid_n, sep="")
        output[[tablename1]] <- renderTable({
          stat_table(best_model(), bdbid_n, 'Elec')
          }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)
        tableOutput(tablename1)
      }else
      { 
        linebreak <- paste("break", bdbid_n, sep="") 
        output[[linebreak]] <- renderUI({HTML("<br/>")})
        uiOutput(linebreak)
      }
    })
    }
    )

    do.call(tagList, plot_output_list)
  })

  output$fuel_multi_plot <- renderUI({
    plot_output_list <- lapply(1:length(unique(temp_df()$bdbid)), function(i) {
      lapply(1:3, function(j){
      bdbid_n <- unique(temp_df()$bdbid)[i]

      if(j == 1)
      {
        plotname2 <- paste("plot2", bdbid_n, sep="")
        output[[plotname2]] <- renderPlotly({
          b_name = get_building_name(binfo_df(), bdbid_n)
          main_plot(temp_df(), best_model(), bdbid_n, 'Fuel', b_name)
        })
        plotlyOutput(plotname2)
      }else if(j == 2)
      { tablename2 <- paste("table2", bdbid_n, sep="")
        output[[tablename2]] <- renderTable({
          stat_param_table(best_model(), bdbid_n, 'Fuel')
          }, align = 'c', rownames = FALSE, colnames = TRUE, width = "auto", digits = 7)
        tableOutput(tablename2)
      }else
      { 
        linebreak2  <- paste("break2", bdbid_n, sep="") 
        output[[linebreak2]] <- renderUI({HTML("<br/>")})
        uiOutput(linebreak2)
      }
    })
    }
    )

    do.call(tagList, plot_output_list)
  })


}



