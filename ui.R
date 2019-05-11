library(shiny)
library(shinyjs)
library(data.table)
library(ggplot2)
library(grid)
library(jpeg)


ui <- fluidPage(
  # Styling to hide navbar.
  useShinyjs(),
  tags$style(type='text/css', "nav.navbar.navbar-default.navbar-static-top{border-color: #f5f5f5;background-color: #f5f5f5;}"),
  tags$style(type='text/css', ".navbar{min-height: 0px; margin-bottom: 0px;}"),
  tags$style(type='text/css', ".navbar-brand{height: 0px; padding: 0px 0px;}"),

  #Styling of elements.
  tags$style(HTML("
    input:invalid 
    {
      background-color: #FFCCCC;
    }
    
    #p4_box_status
    {
      border-top: 2px solid black;
      border-left: 2px solid black;
      border-right: 2px solid black;
    }
    
    #p4_box_mid
    {
      border-left: 2px solid black;
      border-right: 2px solid black;
      border-bottom: 1px solid black;
    }

    #p4_box_recycle_pc
    {
      border-left: 2px solid black;
      border-bottom: 2px solid black;
      border-right: 1px solid black;
    }

    #p4_box_waste
    {
      border-bottom: 2px solid black;
      border-right: 1px solid black;
    }
    
    #p4_box_recycle_vol
    {
      border-bottom: 2px solid black;
      border-right: 2px solid black;
    }
                  ")),
  
  # # CSS styling. Not in use, couldn't eliminate gaps in ui.
  # tags$style(HTML("
  #                   div.p3_trash {
  #                       background-color: pink;
  #                   }
  #                   div.p3_recycle {
  #                       border: 2px dashed blue;
  #                   }"
  #                 )
  #            ),
  navbarPage
  (
    title = NULL, id = "navBar",

    ###############################################
    ###### PAGE 1: ENTER ELECTRONICS DETAILS ######
    ###############################################
    tabPanel
    (
      title = NULL, value = "page1",
      h1("What is my household's E-waste footprint?", align = "center"),
      
      # Household size row
      fluidRow
      (
        column
        (
          style = "padding-top: 40px;",
          width = 6,
          align = "right",
          numericInput(inputId = "in_num_persons", label = "How many people live in your household?", value = 1, min = 1, width="40%")
        ),
        column
        (
          width = 6,
          align = "left",
          img(src='home_icon.png')
        )
      ),
      
      h3("Take a look around your household. How many of these electronics are used?", align = "center"),
      
      # Mobile phone and Desktop row
      fluidRow
      (
        style = "padding-bottom: 40px;",
        
        # Mobile phone
        column
        (
          style = "padding-top: 40px;",
          width=3,
          numericInput(inputId = "in_mobile", label = "Mobile Phones", value = 0, min = 0),
          selectInput(inputId = "in_mobile_ror", label = "How often do you replace mobile phones?", # Response stored as replacements per year.
                      c("I don't know!" = 0.556, # 21.6 months, based on average lifecycle in USA, table 3.2 ewaste monitor (courtesy kantar world panel https://www.kantarworldpanel.com/global/News/Double-Digit-Smartphone-Market-Growth-is-over)
                        "Every 6 months" = 2,
                        "Every year" = 1,
                        "Every 2 years" = 0.5,
                        "Every 3 years" = 0.333,
                        "More than 3 years" = 0.213  # 4.7 years, based on average mobile life expectancy. https://www.cta.tech/News/Blog/Articles/2014/September/The-Life-Expectancy-of-Electronics.aspx
                        )     
          )
        ),
        column
        (
          style = "padding-top: 50px;",          
          width=3,
          img(src='mobile.png')
        ),
        
        # Desktop
        column
        (
          style = "padding-top: 40px;",          
          width=3,
          numericInput(inputId = "in_desktop", label = "Desktop PCs", value = 0, min = 0)
        ),
        column
        (
          style = "padding-top: 40px;",
          width=3,
          img(src='desktop.png')
        )
      ),

      # Television and Laptop row
      fluidRow
      (
        # Television
        column
        (
          width=3,
          style = "padding-top: 40px;",
          numericInput(inputId = "in_tv", label = "Televisions", value = 0, min = 0)
        ),
        column
        (
          width=3,
          style = "padding-top: 20px;",          
          img(src='tv.png')
        ),
        
        
        # Laptop
        column
        (
          width=3,
          style = "padding-top: 40px;",          
          numericInput(inputId = "in_laptop", label = "Laptops", value = 0, min = 0)
        ),
        column
        (
          width=3,
          style = "padding-top: 30px;",          
          img(src='laptop.png')
        )
      ),
      
      # Page controls
      fluidRow
      (
        column
        (
          width = 12,
          style = "padding-top: 40px;",
          align="center",
          actionButton("btn_next1", "Next", width = "25%", style = "background-color:green; color:white; font-weight:bold"),
          div(textOutput("p1_txt_input_valid"), style="color:red;", align = "left")
        )
      )
    ),

    
    ####################################################
    ###### PAGE 2: DISPLAY RESOURCES USED TO MAKE ######    
    ####################################################
    tabPanel
    (
      title = NULL, value = "page2",
      h1("How did it get here?", align = "center"),
      textOutput("p2_test_out"),
      
      actionButton("btn_prev2", "Back", width = "10%", style = "background-color:green; color:white; font-weight:bold"),
      actionButton("btn_next2", "Next", width = "10%", style = "background-color:green; color:white; font-weight:bold")
    ),
    
    
    ########################################################
    ###### PAGE 3: DISPLAY WASTE AND RECYCLING IMPACT ######
    ########################################################
    tabPanel
    (
      title = NULL, value = "page3",
      htmlOutput("p3_txt_heading", align='center'),
      # tags$h1("", align = "center"),
      # tags$h3("Sooner or later, all the electonics you own will need to be disposed of.", tags$br(), "Below is the amount of waste you will generate over the next five years.", align = "center"),
      # tags$h4(align = "center", style="font-weight:bold;", "But it's not all bad news!"),
      
      sidebarLayout
      (
        #Sidebar contains waste images and counts, split into "not recycled" and "recycled" categories.
        sidebarPanel
        (
          width = 6,
          align = "center",
          h4("Click an image in trash to recycle it!", align = "center", style="color:red"),
          
          # Row for displaying heading data.
          fluidRow
          (
            column
            (
              width = 5,
              div(
                h2("Trashed", align="center")
              )
            ),
            column
            (
              # Filler column
              width=2
            ),
            column
            (
              width = 5,
              h2("Recycled", align="center")
            )

          ),


          # Row for displaying mobile phone data.
          fluidRow
          (
            style = "padding-bottom: 15px;",
            
            column
            (
              width=5,
              div
              (
                id = "p3_div_mobile_no_recycle",
                img(id="p3_img_mobile_no_recycle", src='mobile_small.png'),
                htmlOutput("p3_txt_mobile_no_recycle")
              )
            ),
            
            column
            (
              # Filler column
              width=2,
              style = "padding-top: 25px;",
              img(src='double_arrow.png', width="50px", align = "right")
            ),

            column
            (
              width=5,
              div
              (
                id = "p3_div_mobile_recycle",
                img(id="p3_img_mobile_recycle", src='mobile_small.png'),
                htmlOutput("p3_txt_mobile_recycle")
              ),
              
              # Recycle instructions.
              div
              (
                id="p3_div_recycle_instructions",
                style="color:red; font-size: 18px;",
                align = "center",
                "Click on an image to the left to recycle it!"
              )
            )          
          ),
          
          # Row for displaying television data.
          fluidRow
          (
            style = "padding-bottom: 15px;",
            
            column
            (
              width=5,
              div
              (
                id = "p3_div_tv_no_recycle",
                img(id="p3_img_tv_no_recycle", src='tv_small.png'),
                htmlOutput("p3_txt_tv_no_recycle")
              )
            ),
            
            column
            (
              # Filler column
              width=2,
              style = "padding-top: 25px;",
              img(src='double_arrow.png', width="50px", align = "right")
            ),
            
            
            column
            (
              width=5,
              div
              (
                id = "p3_div_tv_recycle",
                img(id="p3_img_tv_recycle", src='tv_small.png'),
                htmlOutput("p3_txt_tv_recycle")
              )
            )          
          ),
          
          # Row for displaying desktop data.
          fluidRow
          (
            style = "padding-bottom: 15px;",
            
            column
            (
              width=5,
              div
              (
                id = "p3_div_desktop_no_recycle",
                img(id="p3_img_desktop_no_recycle", src='desktop_small.png'),
                htmlOutput("p3_txt_desktop_no_recycle")
              )
            ),
            
            column
            (
              # Filler column
              width=2,
              style = "padding-top: 25px;",
              img(src='double_arrow.png', width="50px", align = "right")
            ),
            
            column
            (
              width=5,
              div
              (
                id = "p3_div_desktop_recycle",
                img(id="p3_img_desktop_recycle", src='desktop_small.png'),
                htmlOutput("p3_txt_desktop_recycle")
              )
            )          
          ),
          
          
          # Row for displaying laptop data.
          fluidRow
          (
            style = "padding-bottom: 15px;",
            
            column
            (
              width=5,
              div
              (
                id = "p3_div_laptop_no_recycle",
                img(id="p3_img_laptop_no_recycle", src='laptop_small.png'),
                htmlOutput("p3_txt_laptop_no_recycle")
              )
            ),
            
            column
            (
              # Filler column
              width=2,
              style = "padding-top: 25px;",
              img(src='double_arrow.png', width="50px", align = "right")
            ),
            
            column
            (
              width=5,
              div
              (
                id = "p3_div_laptop_recycle",
                img(id="p3_img_laptop_recycle", src='laptop_small.png'),
                htmlOutput("p3_txt_laptop_recycle")
              )
            )          
          )
          
        ),
        
        # Main panel used for plotting.
        mainPanel
        (
          width=6,
          fluidRow(
            column
            (
              width = 6,
              align = "center",
              h2("Trashed", align = "center"),
              div(textOutput("p3_txt_trash_counter"), style="color:red; font-size: 20px;", align = "center"),
              plotOutput("plot_trash", height="650px", width = "200px"),
              div(style="font-size: 8px", "Photograph by Stefan Czapski, Geograph. CC-BY-SA-2.0")
            ),
            
            column
            (
              width = 6,
              h2("Recycled", align = "center"),
              div(textOutput("p3_txt_recycle_counter"), style="color:green; font-size: 20px;", align = "center"),
              plotOutput("plot_recycle", height="650px", width = "200px")
            )
          )
        )
      ),
      
      # Page controls
      fluidRow
      (
        column
        (
          width = 2,
          style = "padding-top: 0px; padding-bottom: 20px;",
          align="left",
          actionButton("btn_prev3", "Back", width = "80%", style = "background-color:grey; color:white; font-weight:bold")
        ),
        column
        (
          width = 8,
          style = "padding-top: 0px; padding-bottom: 20px;",
          align = "center",
          actionButton("btn_next3", "Your Scorecard", width = "39%", style = "background-color:green; color:white; font-weight:bold")
        )
      )
    ),
    ##########################################
    ###### PAGE 4: PERSONAL REPORT CARD ######
    ##########################################
    tabPanel
    (
      title = NULL, value = "page4",
      # h1("Your household's E-waste report card", align = "center"),

      # Overall status box.
      fluidRow
      (
        # Overall status box.
        id = "p4_box_status",
        style = "padding-left: 10px; padding-right: 10px;",
        align="center",
        
        HTML("<h1><b>Your Household's E-waste Report Card</b></hl>"),
        HTML("<div style='height: 170px;padding-top:20px;'>"),
        imageOutput("p4_img_status"),
        HTML("</div>"),
        htmlOutput("p4_txt_status"),
        plotOutput("plot_status", height="300px", width = "100%")
      ),
      
      fluidRow
      (
        id = "p4_box_mid",
        align="center",
        HTML("<h2><b>Over the next five years...</b></h3>")
      ),
      
      fluidRow
      (
          
        # Recycling % box
        column
        (
          id = "p4_box_recycle_pc",
          width = 4,
          align="center",
          style = "padding-left: 10px; padding-right: 10px; height: 330px;",
          
          HTML("<h3>You will recycle </h3>"),
          htmlOutput("p4_txt_user_recycle_pc"),
          HTML("<h3 style='margin-top:10px'>of your E-waste</h3>"),
          htmlOutput("p4_txt_user_recycle_pc_2")
          ),
        
        # Waste volume box.
        column
        (
          id = "p4_box_waste",
          width = 4,
          align="center",
          style = "padding-left: 2px; padding-right: 2px;  height: 330px;",
          
          HTML("<h3>Your household will send</h3>"),
          htmlOutput("p4_txt_user_waste"),
          HTML("<h3 style='margin-top:10px'>of E-waste to landfill</h3>"),
          htmlOutput("p4_txt_user_waste_2")
          ),
        
        # Recycling volume box
        column
        (
          id = "p4_box_recycle_vol",
          width = 4,
          align="center",
          style = "padding-left: 2px; padding-right: 2px;  height: 330px;",
          
          HTML("<h3>Your household will create</h3>"),
          htmlOutput("p4_txt_user_recycle_vol"),
          HTML("<h3 style='margin-top:10px'>of recovered resources</h3>"),
          htmlOutput("p4_txt_user_recycle_vol_2")
          # htmlOutput("p4_user_create") # divide volume by weight of some electronic goods/etc and show output.
        )
      ),
    
      # Page controls
      fluidRow
      (
        column
        (
          width = 2,
          style = "padding-top: 10px; padding-bottom: 20px;",
          align="left",
          actionButton("btn_prev4", "Back", width = "80%", style = "background-color:grey; color:white; font-weight:bold")
        )
      )
    )
  )
)