# TODO:
# Split charts by type of product or recyclable/non-recyclable.
# Add other recycling items (ie. large appliances, small appliances)
# Comparison to "Average"

# Load data/models about average household
load("avg_household.RData")

# Create functions.
input_inval = function(x){
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
    recycle_prop = 0,
    waste_vol = 0,
    recycle_vol = 0,
    
    f_recycle_well = 0,  # Three variables for tracking scorecard metrics. 0,1,2 =  bad/ok/good.
    f_recycle_prop = 0,
    f_waste_vol = 0,
    f_recycle_vol = 0)
  
  # Store average household E-waste statistics, based on uers' household size.
  avginfo <- reactiveValues(
    recycle_pc_aus = 0.064,
    recycle_pc_global = 0.2,
    recycle_vol = 0, # Will be calculated below.
    waste_vol = 0 # Will be calculated below.
  )
  
  
  
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
  output$p3_txt_heading <- renderUI({HTML(paste0("<h1>Your household will generate ",
                                                 "<br/><b><font color='red'>", round(plot_param$max_y,1), "kg </font></b>",
                                                 "<br/>of E-waste in the next five years</h1>",
                                                 "<h3><font color='green'>Commit to recycling and see its impact below!</h3>"))})
  
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
                "<br/><p style='font-size:13px; border-style: dashed; border-width: 1px; border-color: #0645AD;'> <a href='http://recyclewell.tk/how-to-recycle-it-new/' target='_blank'>How do I recycle this?</a></p>"))
  })
  
  output$p3_txt_mobile_no_recycle <- renderUI({txt_mobile()})
  output$p3_txt_mobile_recycle <- renderUI({txt_mobile()})

  # TV text outputs
  txt_tv = reactive({
    HTML(paste0("Recyclable: ", round(userdata$tv * tv$recyclable,2), " kg",
                "<br/> Non-recyclable: ", round(userdata$tv * tv$non_recyclable,2), " kg",
                "<br/><p style='font-size:13px; border-style: dashed; border-width: 1px; border-color: #0645AD;'><a href='http://recyclewell.tk/how-to-recycle-it-new/' target='_blank'>How do I recycle this?</a></p>"))
  })
  
  output$p3_txt_tv_no_recycle <- renderUI({txt_tv()})
  output$p3_txt_tv_recycle <- renderUI({txt_tv()})

  # desktop text outputs
  txt_desktop = reactive({
    HTML(paste0("Recyclable: ", round(userdata$desktop * desktop$recyclable,2), " kg",
                "<br/> Non-recyclable: ", round(userdata$desktop * desktop$non_recyclable,2), " kg",
                "<br/><p style='font-size:13px; border-style: dashed; border-width: 1px; border-color: #0645AD;'><a href='http://recyclewell.tk/how-to-recycle-it-new/' target='_blank'>How do I recycle this?</a></p>"))
  })
  
  output$p3_txt_desktop_no_recycle <- renderUI({txt_desktop()})
  output$p3_txt_desktop_recycle <- renderUI({txt_desktop()})

  # laptop text outputs
  txt_laptop = reactive({
    HTML(paste0("Recyclable: ", round(userdata$laptop * laptop$recyclable,2), " kg",
                "<br/> Non-recyclable: ", round(userdata$laptop * laptop$non_recyclable,2), " kg",
                "<br/><p style='font-size:13px; border-style: dashed; border-width: 1px; border-color: #0645AD;'><a href='http://recyclewell.tk/how-to-recycle-it-new/' target='_blank'>How do I recycle this?</a></p>"))
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
      labs(x="", y="Kilograms of E-waste") + 
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
  
    ################ Overall status box ####################
    # Background colour changer
    observe({

      # Generate users' average rating across three recycling metrics (recycle %, waste volume, recycled materials volume)
     userinfo$f_recycle_well = mean(c(userinfo$f_recycle_prop, userinfo$f_waste_vol, userinfo$f_recycle_vol))

      # Colour background and generate text based on overall e-waste management status.
      if (userinfo$f_recycle_well >= 1.5){
        # Good
        runjs(sprintf("
              document.getElementById('%s').style.backgroundColor = '%s';
          ", "p4_box_status", "#ccffcc"))
        runjs(sprintf("
              document.getElementById('%s').style.backgroundColor = '%s';
          ", "p4_box_mid", "#ccffcc"))

        output$p4_txt_status = renderUI({HTML(paste0("<h2>You're doing fantastic!</br></h2>",
                                                     "<div style='font-size:20px;line-height:24px;'>","You are doing much better than the vast majority of households,</br>both in Australia and globally!",
                                                     "</br><br>Thank you for helping in the fight against E-waste!</div>"
                                                     ))})
        
      }else if (userinfo$f_recycle_well >= 0.5) {
        # Average
        runjs(sprintf("
              document.getElementById('%s').style.backgroundColor = '%s';
          ", "p4_box_status", "#ffff99"))
        runjs(sprintf("
              document.getElementById('%s').style.backgroundColor = '%s';
          ", "p4_box_mid", "#ffff99"))
        
        output$p4_txt_status = renderUI({HTML(paste0("<h2>You're doing alright.</br></h2>",
                                                     "<div style='font-size:20px;line-height:24px;'>","You've taken some steps to manage your E-waste properly,</br> but there is still much more you can do!"
                                                     ))})

      }else{
        runjs(sprintf("
              document.getElementById('%s').style.backgroundColor = '%s';
          ", "p4_box_status", "#ffcccc"))
        runjs(sprintf("
              document.getElementById('%s').style.backgroundColor = '%s';
          ", "p4_box_mid", "#ffcccc"))
        
        output$p4_txt_status = renderUI({HTML(paste0("<h2>You're not doing so great.</br></h2>",
                                                     "<div style='font-size:20px;line-height:24px;'>","Your habits are contributing to the e-Waste problem,<br>and you're doing poorly compared to other households in Australia",
                                                     "</br><br>Please join us in the global fight against E-waste!</div>"
                                                     ))})
        

      }
    })

    # Render status box contents.
    # Image
    output$p4_img_status <- renderImage({
      if(userinfo$f_recycle_well < 0.5){
        return(list(
          src = "www/bad.png",
          contentType = "image/png",
          width = 150,
          height = 150
        ))
      } else if(userinfo$f_recycle_well < 1.5){
        return(list(
          src = "www/ok.png",
          contentType = "image/png",
          width = 150,
          height = 150
        ))
      } else {
        return(list(
          src = "www/good.png",
          contentType = "image/png",
          width = 150,
          height = 150
        ))
      }
    }, deleteFile = FALSE)

    
    ### Render status plot
    output$plot_status = renderPlot({
      
      ### Generate data
      df_status = data.table(metric = factor(levels=c("Recycling %", "E-waste to Landfill (kg)", "Recovered Resources (kg)", "gold", "blue")),
                             group = factor(levels=c("You", "Australia", "Target")),
                             value = numeric(),
                             norm_value = numeric(),
                             border = factor(levels=c("red", "orange", "green4", "gold", "blue")))

      border_lookup = c("red","orange","green4")
      
      # Recycle % data
      tmp_tar_val = 2*avginfo$recycle_pc_global
      tmp_max = max(userinfo$recycle_prop, avginfo$recycle_pc_aus, tmp_tar_val)
      
      tmp_you = list("Recycling %", "You", round(userinfo$recycle_prop*100,0), userinfo$recycle_prop/tmp_max, border_lookup[userinfo$f_recycle_prop+1])
      tmp_aus = list("Recycling %", "Australia", round(avginfo$recycle_pc_aus*100,0), avginfo$recycle_pc_aus/tmp_max, "gold")
      tmp_tar = list("Recycling %", "Target", round(tmp_tar_val*100,0), tmp_tar_val/tmp_max, "blue")
      
      df_status = rbind(df_status, tmp_you, tmp_aus, tmp_tar)
      
      # Waste volume data
      tmp_tar_val = avginfo$waste_vol/(1-avginfo$recycle_pc_aus)*(1-avginfo$recycle_pc_global*2)
      tmp_max = max(userinfo$waste_vol, avginfo$waste_vol, tmp_tar_val)
      
      tmp_you = list("E-waste to Landfill (kg)", "You", round(userinfo$waste_vol,1), userinfo$waste_vol/tmp_max, border_lookup[userinfo$f_waste_vol+1])
      tmp_aus = list("E-waste to Landfill (kg)", "Australia", round(avginfo$waste_vol,1), avginfo$waste_vol/tmp_max, "gold")
      tmp_tar = list("E-waste to Landfill (kg)", "Target", round(tmp_tar_val,1), tmp_tar_val/tmp_max, "blue")
      
      df_status = rbind(df_status, tmp_you, tmp_aus, tmp_tar)
      
      # Recycle volume data
      tmp_tar_val = 2*(avginfo$recycle_vol/avginfo$recycle_pc_aus*avginfo$recycle_pc_global)
      tmp_max = max(userinfo$recycle_vol, avginfo$recycle_vol, tmp_tar_val)
      
      tmp_you = list("Recovered Resources (kg)", "You", round(userinfo$recycle_vol,1), userinfo$recycle_vol/tmp_max, border_lookup[userinfo$f_recycle_vol+1])
      tmp_aus = list("Recovered Resources (kg)", "Australia", round(avginfo$recycle_vol,1), avginfo$recycle_vol/tmp_max, "gold")
      tmp_tar = list("Recovered Resources (kg)", "Target", round(tmp_tar_val,1), tmp_tar_val/tmp_max, "blue")
      
      df_status = rbind(df_status, tmp_you, tmp_aus, tmp_tar)
      
      # Grouped
      ggplot(df_status, aes(x=metric, y=norm_value, fill=group, colour=border)) + 
        geom_bar(position = position_dodge(0.5),width=0.5, stat="identity", size=1.2) +
        scale_fill_manual(values=c("You" = "gray90", "Australia" = "gold", "Target" = "blue")) + 
        scale_colour_identity() +
        geom_text(aes(label=value), position=position_dodge(width=0.5), vjust=-0.25, color="black", size=5)+
        scale_y_continuous(limits=c(0,1.1))+
        theme(axis.ticks = element_blank(),
              axis.title = element_blank(),
              axis.text.y = element_blank(),
              axis.line.y = element_blank(),
              legend.title = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_blank(),
              
              legend.position = "bottom",
              legend.key.size = unit(1.3, "line"),
              legend.text = element_text(size = 17, margin=margin(r = 30, unit = "pt")),
              axis.text.x =  element_text(size = 17)
              )

    }, bg="transparent")
    

  ################## Recycling % box ######################
  observe({
    ### Calculate recycled %.
    # Grab recycled flags, create a list from it, order it by ascending key, unlist it, and convert to integer
    recycled_flag = c(as.integer(unlist(reactiveValuesToList(flag)[order(names(reactiveValuesToList(flag)))])))
    num_items = c(unlist(reactiveValuesToList(userdata)[order(names(reactiveValuesToList(userdata)))], use.names = FALSE))
    vol_items = c(Reduce("+", desktop), # Ensure this list is sorted
                  Reduce("+", laptop),
                  Reduce("+", mobile),
                  Reduce("+", tv))
    
    total_vol = sum(num_items*vol_items) # Total volume
    recycled_vol = sum(recycled_flag*num_items*vol_items) # recycle total
    
    recycled_pc = ifelse(total_vol==0, 0, recycled_vol/total_vol) # User's recycled %
    
    userinfo$recycle_prop = recycled_pc
    
    # Colour background and generate text based on comparison to reference targets.
    if (recycled_pc > 2*avginfo$recycle_pc_global){
      # Good
      runjs(sprintf("
                    document.getElementById('%s').style.backgroundColor = '%s';
                    ", "p4_box_recycle_pc", "#ccffcc"))
      
      # Text outputs
      output$p4_txt_user_recycle_pc = renderUI({HTML(paste0("<h3 style='color:DarkGreen; margin-top:10px; margin-bottom:0px'><b>", round(recycled_pc*100,0), "%</b></h3>"))})

      
      output$p4_txt_user_recycle_pc_2 = renderUI({HTML(paste0("<div style='font-size:16px;'>",
                                                              "</br>You are recycling more than double the global average of <b>", round(avginfo$recycle_pc_global*100,0), "%</b>, ",
                                                              "and far above Australia's extremely poor recycling rate of <b>", round(avginfo$recycle_pc_aus*100,0), "%</b>.</br>",
                                                              "<span style='font-size:20px; line-height:40px;'><b>", "Well done!", "</b></span></div>"))})
      
      userinfo$f_recycle_prop = 2
    }else if (recycled_pc > avginfo$recycle_pc_global){
      # Average
      runjs(sprintf("
                    document.getElementById('%s').style.backgroundColor = '%s';
                    ", "p4_box_recycle_pc", "#ffff99"))
      
      
      output$p4_txt_user_recycle_pc = renderUI({HTML(paste0("<h3 style='color:DarkOrange; margin-top:10px; margin-bottom:0px'><b>", round(recycled_pc*100,0), "%</b></h3>"))})

      output$p4_txt_user_recycle_pc_2 = renderUI({HTML(paste0("<div style='font-size:16px;'>",
                                                              "</br>This is higher than Australia's extremely poor recycling rate of <b>", round(avginfo$recycle_pc_aus*100,0), "%</b>, ",
                                                              "as well as the global average of <b>", round(avginfo$recycle_pc_global*100,0), "%</b> - but not by much! ",
                                                              "</br></br>There is still room for improvement!", "</div>"))})
      
      userinfo$f_recycle_prop = 1
    } else {
      # Bad
      runjs(sprintf("
                    document.getElementById('%s').style.backgroundColor = '%s';
                    ", "p4_box_recycle_pc", "#ffcccc"))
      
      output$p4_txt_user_recycle_pc = renderUI({HTML(paste0("<h3 style='color:Red; margin-top:10px; margin-bottom:0px'><b>", round(recycled_pc*100,0), "%</b></h3>"))})
      
      output$p4_txt_user_recycle_pc_2 = renderUI({HTML(paste0("<div style='font-size:16px;'>",
                                                              "</br>You are recycling less than the global average of <b>", round(avginfo$recycle_pc_global*100,0), "%</b>. ",
                                                              "</br></br><b>Please help us improve Australia's poor E-waste recycling rate of ", round(avginfo$recycle_pc_aus*100,0), "%!</b></div>"))})
      
      userinfo$f_recycle_prop = 0
    }

  })
  
  ################## Waste volume box ######################
  observe({
    
    # Get e-waste to landfill volume.
    waste_vol = Reduce("+",no_recycle())
    
    userinfo$waste_vol = waste_vol

    # Colour background and generate text based on comparison to reference targets.
    if (waste_vol < avginfo$waste_vol/(1-avginfo$recycle_pc_aus)*(1-avginfo$recycle_pc_global*2)){  # Aus recycle rate is low. Want to beat (approx) theoretical waste generated by aus household recycling @ double global rate before saying "good enough".
      # Good
      runjs(sprintf("
                    document.getElementById('%s').style.backgroundColor = '%s';
                    ", "p4_box_waste", "#ccffcc"))
      
      # Text outputs, good
      output$p4_txt_user_waste = renderUI({HTML(paste0("<h3 style='color:DarkGreen; margin-top:10px; margin-bottom:0px'><b>", round(waste_vol,1), "kg</b></h3>"))})
      
      output$p4_txt_user_waste_2 = renderUI({HTML(paste0("<div style='font-size:16px;margin-top:20px'>",
                                                         "An average Australian household your size would dump an incredible <b>", round(avginfo$waste_vol,1), "kg</b> of E-waste. ",
                                                         "E-waste contains many toxic substances that can leech into soil and water systems.",
                                                         "</br></br><b>Thank you for keeping your E-waste out of landfill!</b>"))})

      userinfo$f_waste_vol = 2
    }else if (waste_vol < avginfo$waste_vol){ # Beat aus household recycling at aus rate.
      # Average
      runjs(sprintf("
                    document.getElementById('%s').style.backgroundColor = '%s';
                    ", "p4_box_waste", "#ffff99"))

      output$p4_txt_user_waste = renderUI({HTML(paste0("<h3 style='color:DarkOrange; margin-top:10px; margin-bottom:0px'><b>", round(waste_vol,1), "kg</b></h3>"))})
      
      output$p4_txt_user_waste_2 = renderUI({HTML(paste0("<div style='font-size:16px;margin-top:20px'>",
                                                         "E-waste contains many toxic substances that can leech into soil and water systems. ",
                                                         "An average Australian household your size would dump an incredible <b>", round(avginfo$waste_vol,1), "kg</b> of E-waste. ",
                                                         "</br></br>You're doing better than this, but still sending a lot to landfill!</div>"))})
      
      userinfo$f_waste_vol = 1
    } else {
      # Bad
      runjs(sprintf("
                    document.getElementById('%s').style.backgroundColor = '%s';
                    ", "p4_box_waste", "#ffcccc"))

      output$p4_txt_user_waste = renderUI({HTML(paste0("<h3 style='color:Red; margin-top:10px; margin-bottom:0px'><b>", round(waste_vol,1), "kg</b></h3>"))})
      
      output$p4_txt_user_waste_2 = renderUI({HTML(paste0("<div style='font-size:16px;margin-top:20px'>",
                                                         "This is more than an average Australian household your size, which would dump <b>", round(avginfo$waste_vol,1), "kg</b>. ",
                                                         "E-waste contains many toxic substances that can leech into soil and water systems.",
                                                         "</br></br><b>Please help us keep these harmful substances out of our ecosystems!</b>"))})

      userinfo$f_waste_vol = 0
    }
    
  })

  
  ################## Recycled volume box ######################
  observe({
    
    # Get recycled materials volume
    recycle_vol = Reduce("+",recycle())
    
    userinfo$recycle_vol = recycle_vol
    
    # Colour background and generate text based on comparison to reference targets.
    if (recycle_vol > 2*(avginfo$recycle_vol/avginfo$recycle_pc_aus*avginfo$recycle_pc_global)){  # Aus recycle rate is low. Want to beat (approx) theoretical recycled materials generated by aus household recycling @ double global rate before saying "good enough".
      # Good, normal method.
      runjs(sprintf("
                    document.getElementById('%s').style.backgroundColor = '%s';
                    ", "p4_box_recycle_vol", "#ccffcc"))
      
      # Text outputs, good
      output$p4_txt_user_recycle_vol = renderUI({HTML(paste0("<h3 style='color:DarkGreen; margin-top:10px; margin-bottom:0px'><b>", round(recycle_vol,1), "kg</b></h3>"))})
      
      output$p4_txt_user_recycle_vol_2 = renderUI({HTML(paste0("<div style='font-size:16px;margin-top:10px'>",
                                                         "Your recycling allows a high volume of valuable materials to be recovered - much more than the average of <b>", round(avginfo$recycle_vol,1), "kg</b> for a household of your size! ",
                                                         "</br>These materials go straight into the production of new products.",
                                                         "</br></br><b>Thank you for helping Australia sustainably source materials!</b>"))})
      
      userinfo$f_recycle_vol = 2
      
    }else if (userinfo$f_recycle_prop == 2) { # A secondary way to trigger "good" rating - due to low household electronics usage but high recycling rate.
      # Good, low electronics usage method.
      
      runjs(sprintf("
                    document.getElementById('%s').style.backgroundColor = '%s';
                    ", "p4_box_recycle_vol", "#ccffcc"))
      
      # Text outputs, good
      output$p4_txt_user_recycle_vol = renderUI({HTML(paste0("<h3 style='color:DarkGreen; margin-top:10px; margin-bottom:0px'><b>", round(recycle_vol,1), "kg</b></h3>"))})
      
      output$p4_txt_user_recycle_vol_2 = renderUI({HTML(paste0("<div style='font-size:16px;margin-top:10px'>",
                                                               "Almost all of the materials in electronics can be recovered through recycling.",
                                                               "</br>These materials go straight into the production of new products.",
                                                               "</br></br><b>Thank you for helping Australia sustainably source materials!</b>"))})
      
      userinfo$f_recycle_vol = 2

    }else if (recycle_vol > avginfo$recycle_vol){  # Beat aus household recycling at aus rate.
      # Average
      runjs(sprintf("
                    document.getElementById('%s').style.backgroundColor = '%s';
                    ", "p4_box_recycle_vol", "#ffff99"))

      output$p4_txt_user_recycle_vol = renderUI({HTML(paste0("<h3 style='color:DarkOrange; margin-top:10px; margin-bottom:0px'><b>", round(recycle_vol,1), "kg</b></h3>"))})

      output$p4_txt_user_recycle_vol_2 = renderUI({HTML(paste0("<div style='font-size:16px;margin-top:10px'>",
                                                         "Almost all of the materials in electronics can be recovered through recycling. These materials go straight into the production of new products.",
                                                         " An average household of your size recovers <b>", round(avginfo$recycle_vol,1), "kg</b> of materials through recycling.",
                                                         "</br></br>You're doing well, but can still improve!</div>"))})

      userinfo$f_recycle_vol = 1
    } else {
      # Bad
      runjs(sprintf("
                    document.getElementById('%s').style.backgroundColor = '%s';
                    ", "p4_box_recycle_vol", "#ffcccc"))

      output$p4_txt_user_recycle_vol = renderUI({HTML(paste0("<h3 style='color:Red; margin-top:10px; margin-bottom:0px'><b>", round(recycle_vol,1), "kg</b></h3>"))})

      output$p4_txt_user_recycle_vol_2 = renderUI({HTML(paste0("<div style='font-size:16px;margin-top:10px'>",
                                                               "This is less than an average household of your size, which recovers <b>", round(avginfo$recycle_vol,1), "kg</b> of materials through recycling.",
                                                               "</br>Almost all of the materials in electronics can be recovered through recycling.",
                                                               "</br></br><b>Please consider recycling more to help Australia sustainably source materials!</b></div>"))})

      userinfo$f_recycle_vol = 0
    }
    
  })
  
  
  
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
                 # Validation check 1
                 if (input_inval(input$in_mobile) | input_inval(input$in_tv) | input_inval(input$in_desktop) | input_inval(input$in_laptop) | input_inval(input$in_num_persons)){
                   output$p1_txt_input_valid = renderText({"Please enter only numbers above."})         
                 } else {
                   output$p1_txt_input_valid = renderText({""})
                   
                   updateNavbarPage(session, "navBar", selected="page3")
                   
                   # Store waste items per 5 years.
                   userdata$mobile = ifelse(input_inval(input$in_mobile), 0, input$in_mobile * as.numeric(input$in_mobile_ror) * 5)
                   userdata$tv = ifelse(input_inval(input$in_tv), 0, input$in_tv * 0.725) # rate based on 6.9 year life cycle, as per 2018 NPD group market research: https://www.npd.com/wps/portal/npd/us/news/press-releases/2018/seven-percent-compound-annual-growth-expected-for-internet-connected-tv-devices-in-the-u-s--through-2021--forecasts-npd/
                   userdata$desktop = ifelse(input_inval(input$in_desktop), 0,input$in_desktop * 1) # Anectodal evidence (many sources) is 3-5 years. Assumption being made that monitor is replaced when box is replaced (ie, someone buying a full system). As tv's/monitors should be 6.9y as above, using 5y base.
                   userdata$laptop = ifelse(input_inval(input$in_laptop), 0,input$in_laptop * 1.42) # Anectodal evidence (many sources) is 3-4 years. 
                   
                   # Store raw user input.
                   userdata_raw$mobile = ifelse(input_inval(input$in_mobile), 0,input$in_mobile)
                   userdata_raw$tv = ifelse(input_inval(input$in_tv), 0,input$in_tv)
                   userdata_raw$desktop = ifelse(input_inval(input$in_desktop), 0,input$in_desktop)
                   userdata_raw$laptop = ifelse(input_inval(input$in_laptop), 0,input$in_laptop)
                   
                   # Store user info.
                   userinfo$num_persons = ifelse(input_inval(input$in_num_persons) | input$in_num_persons < 1, 1, as.integer(input$in_num_persons))
                   
                   plot_param$max_y = Reduce("+",no_recycle()) + Reduce("+",recycle())
                   
                 }
               })

  observeEvent(input$btn_next3,
               {
                 updateNavbarPage(session, "navBar", selected="page3")
                 
                 ### Calculate average per-household metrics.
                 # Calculate total items owned by household of size of user's.
                 avg_mobile_vol = userinfo$num_persons * 1.1654 # Based on our iter2 model prediction of 2019 number of phones per person.
                 avg_tv_vol = as.numeric(predict(lm_tv_persons, data.table(persons=userinfo$num_persons))) * tvph_scaleup # TV volume, based on model. See investigation.R code on github.
                 avg_desktop_vol = desktop_pp*userinfo$num_persons # Based on expected per person values. See investigation.R
                 avg_laptop_vol = laptop_pp*userinfo$num_persons  # Based on expected per person values. See investigation.R
                 
                 # Calculate volume of waste items generated in 5 years based on turnover rates explained for user data above.
                 avg_mobile_vol = avg_mobile_vol * 0.556 * 5
                 avg_tv_vol = avg_tv_vol* 0.725
                 avg_desktop_vol = avg_desktop_vol * 1
                 avg_laptop_vol = avg_laptop_vol * 1.42
                 
                 # Calculate average recycled volume
                 avg_recycle <- avginfo$recycle_pc_aus *
                   (avg_mobile_vol * mobile$recyclable +
                   avg_tv_vol * tv$recyclable +
                   avg_desktop_vol * desktop$recyclable +
                   avg_laptop_vol * laptop$recyclable)
                 
                 # Calculate non-recycled volume. Total volume minus recycled volume.
                 avg_waste = Reduce("+", desktop) * avg_desktop_vol +
                   Reduce("+", laptop) * avg_laptop_vol + 
                   Reduce("+", mobile) * avg_mobile_vol + 
                   Reduce("+", tv) * avg_tv_vol -
                   avg_recycle
                 
                 avginfo$recycle_vol = avg_recycle
                 avginfo$waste_vol = avg_waste
                   
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
