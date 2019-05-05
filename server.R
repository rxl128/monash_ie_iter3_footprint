# TODO:
# Split charts by type of product or recyclable/non-recyclable.
# Add other recycling items (ie. large appliances, small appliances)
# Comparison to "Average"

# Create functions.
input_val = function(x){
  return(is.na(x) | x<0 | !is.numeric(x))
}

server <- function(input, output, session)
{
  ##################################
  ############# Setup ##############
  ##################################
  theme_set(theme_classic()) #ggplot theme
  
  # plot image loading
  img_trash = readJPEG("waste_img_crop.jpg")
  img_trash = rasterGrob(img_trash, width = unit(1,"npc"),
                         height = unit(1,"npc"), interpolate = TRUE)

  img_recycle = readJPEG("recycle_img.jpg")
  img_recycle = rasterGrob(img_recycle, width = unit(1,"npc"),
                         height = unit(1,"npc"), interpolate = TRUE)
  
  # Hide top bar.
  observe(
    {
      hide(selector = "#navBar")
    })

  # Set up user input storage. Stores raw user data.
  userdata_raw <- reactiveValues(
    mobile = 0,
    tv = 0,
    desktop = 0,
    laptop = 0)
    
  # Set up user input storage. Stores user waste items (num products) in 5 years.
  userdata <- reactiveValues(
    mobile = 0,
    tv = 0,
    desktop = 0,
    laptop = 0)
  
  # Set up storage of information about user/user's household
  userinfo <- reactiveValues(
    num_persons = 0,
    recycle_well = 0)
  
  # Set up flag storage - flag is true if recycled
  flag <- reactiveValues(
    mobile = FALSE,
    tv = FALSE,
    desktop = FALSE,
    laptop = FALSE)
  
  # Set up plot parameters.
  plot_param = reactiveValues(
    min_y = 0,
    max_y = 0
  )
  
  
  #########################################
  #### Set up item component variables.####
  #########################################
  # Recyclable vs non-recyclable proportions based on research by Pranay (avail in drive)
  # RAW values in kg per object (so 0.2 kilograms)
  mobile = list(
    recyclable = .14994,
    non_recyclable = .01666
  )
  # Average smartphone weight in 2018 166.6g: https://www.gsmarena.com/2018_half_year_report-news-31939.php
  
  tv = list(
    recyclable = 11.304,
    non_recyclable = 0.595
  )
  # TV size as est avg LCD screen size 2019 (47.1 inch): https://www.statista.com/statistics/760288/average-tv-screen-size-worldwide/
  # average weight of those tv's, by scaling down based on 50 inch tv. Data from sampling JB 50 inch tv's. (11.9kg)
  # Note LCD dominates market share, and are generally lighter. OLED growing, and is heavier. Plasma shrinking, and is heavier: https://www.flatpanelshd.com/focus.php?subaction=showfull&id=1398762499
  
  desktop = list(
    recyclable = 9.31 + 5.282,
    non_recyclable = 0.19 + 0.278
  )
  # Desktops' heavies components are the tower and monitor (obviously).
  # Tower weight varies. From browsing Officeworks and Harvey Norman Using 9.5kg as base, between standard OTS and gaming OTS towers.
  # Standard monitors size is 23". Based on data here, was 22.1" in 2016 and rising: https://techtalk.pcpitstop.com/2017/01/16/pc-monitor-display-size/.
  # Browsing Amazon, HP, etc - weight ranges from 12-12.5lb. Using 12.25lb as base (5.56kg). Using same split of recyclable/non-recyclable as televisions.
  

  laptop = list(
    recyclable = 1.666,
    non_recyclable = 0.034
  )
  # Using standard 14" display size as base (sizes can range from 10" to 20+", this should be close to median).
  # A quick look at GoodGuys and Officeworks shows these range from 1.6-1.8kg
  

  # Store average per person e-waste statistics
  avginfo <- list(
    recycle_pc_aus = 0.064,
    recycle_pc_global = 0.2,
    recycle_vol = 1, # Volume of recycling per person per 5 years.
    waste_vol = 9 # Volume of waste generated per person per 5 years.
  )
  
  
  ###################################
  ############# Page 2 ##############
  ###################################
  # Not in use for now. Testing.
  output$p2_test_out <- renderText({
    paste0("Mobiles: ", userdata$mobile, ", TVs: ", userdata$tv)
  })

  # output$p2_test_out <- renderText({
  #   paste0(no_recycle()$recyclable, no_recycle()$non_recyclable, recycle()$recyclable)
  # })

  
  
  ###################################
  ############# Page 3 ##############
  ###################################
  ### Heading text output ###
  output$p3_txt_heading <- renderUI({HTML(paste0("<h2>Your household will generate ",
                                                 "<br/><b><font color='red'>", round(plot_param$max_y,1), "kg </font></b>",
                                                 "<br/>of e-waste in the next five years</h2>",
                                                 "<h3><font color='green'>Now, let's do some recycling!</h3>"))})
  
  # Show correct ui objects based on if being recycled or not.
  show("p3_div_mobile_no_recycle")
  show("p3_div_tv_no_recycle")
  show("p3_div_desktop_no_recycle")
  show("p3_div_laptop_no_recycle")
    
  hide("p3_div_mobile_recycle")
  hide("p3_div_tv_recycle")
  hide("p3_div_desktop_recycle")
  hide("p3_div_laptop_recycle")
  
  ### Sidebar Text outputs ###
  # Mobile phone text outputs
  txt_mobile = reactive({
    HTML(paste0("Recyclable: ", round(userdata$mobile * mobile$recyclable,2), " kg",
                "<br/> Non-recyclable: ",  round(userdata$mobile * mobile$non_recyclable,2), " kg",
                "<br/><p style='font-size:13px'> <a href='http://recyclewell.tk/how-to-recycle-it-new/' target='_blank'>How do I recycle this?</a></p>"))
  })
  
  output$p3_txt_mobile_no_recycle <- renderUI({txt_mobile()})
  output$p3_txt_mobile_recycle <- renderUI({txt_mobile()})

  # TV text outputs
  txt_tv = reactive({
    HTML(paste0("Recyclable: ", round(userdata$tv * tv$recyclable,2), " kg",
                "<br/> Non-recyclable: ", round(userdata$tv * tv$non_recyclable,2), " kg",
                "<br/><p style='font-size:13px'><a href='http://recyclewell.tk/how-to-recycle-it-new/' target='_blank'>How do I recycle this?</a></p>"))
  })
  
  output$p3_txt_tv_no_recycle <- renderUI({txt_tv()})
  output$p3_txt_tv_recycle <- renderUI({txt_tv()})

  # desktop text outputs
  txt_desktop = reactive({
    HTML(paste0("Recyclable: ", round(userdata$desktop * desktop$recyclable,2), " kg",
                "<br/> Non-recyclable: ", round(userdata$desktop * desktop$non_recyclable,2), " kg",
                "<br/><p style='font-size:13px'><a href='http://recyclewell.tk/how-to-recycle-it-new/' target='_blank'>How do I recycle this?</a></p>"))
  })
  
  output$p3_txt_desktop_no_recycle <- renderUI({txt_desktop()})
  output$p3_txt_desktop_recycle <- renderUI({txt_desktop()})

  # laptop text outputs
  txt_laptop = reactive({
    HTML(paste0("Recyclable: ", round(userdata$laptop * laptop$recyclable,2), " kg",
                "<br/> Non-recyclable: ", round(userdata$laptop * laptop$non_recyclable,2), " kg",
                "<br/><p style='font-size:13px'><a href='http://recyclewell.tk/how-to-recycle-it-new/' target='_blank'>How do I recycle this?</a></p>"))
  })
  
  output$p3_txt_laptop_no_recycle <- renderUI({txt_laptop()})
  output$p3_txt_laptop_recycle <- renderUI({txt_laptop()})
  
  
  
  ### Image click behaviour ###
  # move to recycle
  shinyjs::onclick("p3_img_mobile_no_recycle", {
    flag$mobile <- TRUE
    hide("p3_div_mobile_no_recycle")
    show("p3_div_mobile_recycle")
    hide("p3_div_recycle_instructions")
    })

  shinyjs::onclick("p3_img_tv_no_recycle", {
    flag$tv <- TRUE
    hide("p3_div_tv_no_recycle")
    show("p3_div_tv_recycle")
    hide("p3_div_recycle_instructions")
  })

  shinyjs::onclick("p3_img_desktop_no_recycle", {
    flag$desktop <- TRUE
    hide("p3_div_desktop_no_recycle")
    show("p3_div_desktop_recycle")
    hide("p3_div_recycle_instructions")
  })
  
  shinyjs::onclick("p3_img_laptop_no_recycle", {
    flag$laptop <- TRUE
    hide("p3_div_laptop_no_recycle")
    show("p3_div_laptop_recycle")
    hide("p3_div_recycle_instructions")
  })
  
    
  # move to trashed.
  shinyjs::onclick("p3_img_mobile_recycle", {
    flag$mobile <- FALSE
    show("p3_div_mobile_no_recycle")
    hide("p3_div_mobile_recycle")
  })
  
  shinyjs::onclick("p3_img_tv_recycle", {
    flag$tv <- FALSE
    show("p3_div_tv_no_recycle")
    hide("p3_div_tv_recycle")
  })
  
  shinyjs::onclick("p3_img_desktop_recycle", {
    flag$desktop <- FALSE
    show("p3_div_desktop_no_recycle")
    hide("p3_div_desktop_recycle")
  })
  
  shinyjs::onclick("p3_img_laptop_recycle", {
    flag$laptop <- FALSE
    show("p3_div_laptop_no_recycle")
    hide("p3_div_laptop_recycle")
  })
  
  
  ### Draw plots ###
  # Trash plot
  output$plot_trash = renderPlot({
    ggplot(data_no_recycle(), aes(type, volume)) +
      annotation_custom(img_trash, xmin=0, xmax=0.99, ymin=0, ymax = plot_param$max_y) + 
      geom_ribbon(aes(ymin=volume, ymax=plot_param$max_y), fill="white") +
      scale_y_continuous(limits=c(0,plot_param$max_y), expand=c(0,0))+
      scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
      labs(x="", y="Kilograms of e-waste") + 
      theme(text = element_text(size=17),
            axis.title.y = element_text(margin = margin(r = 10)),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    })
  
  # Recycled plot
  output$plot_recycle = renderPlot({
    ggplot(data_recycle(), aes(type, volume)) +
      annotation_custom(img_recycle, xmin=0, xmax=0.99, ymin=0, ymax = plot_param$max_y) + 
      geom_ribbon(aes(ymin=volume, ymax=plot_param$max_y), fill="white") +
      scale_y_continuous(limits=c(0,plot_param$max_y), expand=c(0,0), position="right") +
      scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
      labs(x="", y="Kilograms recycled") + 
      theme(text = element_text(size=17),
            axis.title.y = element_text(margin = margin(r = 10)),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  })
  
  
  ### Draw text above plots ###
  output$p3_txt_trash_counter = renderText({
    paste0(round(Reduce("+",no_recycle()),2), " kg")
  })

  output$p3_txt_recycle_counter = renderText({
    paste0(round(Reduce("+",recycle()),2), " kg")
  })
  
  
  ###################################
  ############# Page 4 ##############
  ###################################
  # E-waste report card.
  
  
  ### Overall status box
  # Background colour changer
  observe({
    ### Calculate recycled %.
    # Grab recycled flags, create a list from it, order it by ascending key, unlist it, and conver to integer
    recycled_flag = c(as.integer(unlist(reactiveValuesToList(flag)[order(names(reactiveValuesToList(flag)))])))
    num_items = c(unlist(reactiveValuesToList(userdata)[order(names(reactiveValuesToList(userdata)))], use.names = FALSE))
    vol_items = c(Reduce("+", desktop), # Ensure this list is sorted
                  Reduce("+", laptop),
                  Reduce("+", mobile),
                  Reduce("+", tv))

    total_vol = sum(num_items*vol_items)
    recycled_vol = sum(recycled_flag*num_items*vol_items)

    recycled_pc = ifelse(total_vol==0, 0, recycled_vol/total_vol)

    
    # Colour background based on how good they are recycling.
    if (recycled_pc > avginfo$recycle_pc_global){
      runjs(sprintf("
            document.getElementById('%s').style.backgroundColor = '%s';
        ", "p4_box_status", "#ccffcc"))
      userinfo$recycle_well = 2
    }else{
      runjs(sprintf("
            document.getElementById('%s').style.backgroundColor = '%s';
        ", "p4_box_status", "#ffcccc"))
      userinfo$recycle_well = 0
    }
  })
  
  # Render status box contents.
  # Image
  output$p4_img_status <- renderImage({
    if(userinfo$recycle_well == 0){
      return(list(
        src = "www/mobile_small.png",
        contentType = "image/png"
      ))
    } else if(userinfo$recycle_well == 1){
      return(list(
        src = "www/laptop_small.png",
        contentType = "image/png"
      ))
    } else if(userinfo$recycle_well == 2){
      return(list(
        src = "www/tv_small.png",
        contentType = "image/png"
      ))
    }
  }, deleteFile = FALSE)
  
  # text
  
  
  ###################################
  ######### GRAPH DATA ##############
  ###################################
  # Generate graph data (volume of metals being recycled/not recycled). Based on whether an item is being recycled.
  
  # not-recycled plot value storage
  no_recycle <- reactive({
    list(recyclable = as.integer(!flag$mobile) * userdata$mobile * mobile$recyclable +
           as.integer(!flag$tv) * userdata$tv * tv$recyclable +
           as.integer(!flag$desktop) * userdata$desktop * desktop$recyclable +
           as.integer(!flag$laptop) * userdata$laptop * laptop$recyclable,
         non_recyclable = userdata$mobile * mobile$non_recyclable + 
           userdata$tv * tv$non_recyclable +
           userdata$desktop * desktop$non_recyclable +
           userdata$laptop * laptop$non_recyclable)
  })
  
  # recycled plot value storage
  recycle <- reactive({
    list(recyclable = as.integer(flag$mobile) * userdata$mobile * mobile$recyclable + 
           as.integer(flag$tv) * userdata$tv * tv$recyclable +
           as.integer(flag$desktop) * userdata$desktop * desktop$recyclable +
           as.integer(flag$laptop) * userdata$laptop * laptop$recyclable)
  })

  
  ### Plot data ###
  data_no_recycle = reactive({
    data <- data.table(
      type = c("total"),  # Can alter later to plot by recyclable/non-reyclable, or by product.
      volume = c(Reduce("+",no_recycle())))
    data2 = rbind(data, data)
    data2[,type := c(0,1)]
  })

  data_recycle = reactive({
    data <- data.table(
      type = c("total"),
      volume = c(Reduce("+",recycle())))
    data2 = rbind(data, data)
    data2[,type := c(0,1)]
  })
  
  
  
  
  ################################################################
  ########### Page selection controls and activities #############
  ################################################################
  observeEvent(input$btn_next1,
               {
                 updateNavbarPage(session, "navBar", selected="page3")
                 
                 # Store waste items per 5 years.
                 userdata$mobile = ifelse(input_val(input$in_mobile), 0, input$in_mobile * as.numeric(input$in_mobile_ror) * 5)
                 userdata$tv = ifelse(input_val(input$in_tv), 0, input$in_tv * 0.725) # rate based on 6.9 year life cycle, as per 2018 NPD group market research: https://www.npd.com/wps/portal/npd/us/news/press-releases/2018/seven-percent-compound-annual-growth-expected-for-internet-connected-tv-devices-in-the-u-s--through-2021--forecasts-npd/
                 userdata$desktop = ifelse(input_val(input$in_desktop), 0,input$in_desktop * 1) # Anectodal evidence (many sources) is 3-5 years. Assumption being made that monitor is replaced when box is replaced (ie, someone buying a full system). As tv's/monitors should be 6.9y as above, using 5y base.
                 userdata$laptop = ifelse(input_val(input$in_laptop), 0,input$in_laptop * 1.42) # Anectodal evidence (many sources) is 3-4 years. 
                 
                 # Store raw user input.
                 userdata_raw$mobile = ifelse(input_val(input$in_mobile), 0,input$in_mobile)
                 userdata_raw$tv = ifelse(input_val(input$in_tv), 0,input$in_tv)
                 userdata_raw$desktop = ifelse(input_val(input$in_desktop), 0,input$in_desktop)
                 userdata_raw$laptop = ifelse(input_val(input$in_laptop), 0,input$in_laptop)
                 
                 # Store user info.
                 userinfo$num_persons = ifelse(input_val(input$in_num_persons) | input$in_num_persons < 1, 1, as.integer(input$in_num_persons))
                 
                 plot_param$max_y = Reduce("+",no_recycle()) + Reduce("+",recycle())
               })

  observeEvent(input$btn_next2,
               {
                 updateNavbarPage(session, "navBar", selected="page3")
               })
  
  observeEvent(input$btn_prev2,
               {
                 updateNavbarPage(session, "navBar", selected="page1")
               })
  
  observeEvent(input$btn_next3,
               {
                 updateNavbarPage(session, "navBar", selected="page4")
               })
  
  observeEvent(input$btn_prev3,
               {
                 updateNavbarPage(session, "navBar", selected="page1")
               })
  
  observeEvent(input$btn_prev4,
               {
                 updateNavbarPage(session, "navBar", selected="page3")
               })
  

}
