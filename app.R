source("myFunc.R")

ui <- fluidPage(
  headerPanel("Multivariate Plots for Facebook Data", br()),
  conditionalPanel(condition = "input.conditionedPanels==1", br(),
                   sidebarPanel(width = 4,
                                selectInput("x_axis", "X variable", 
                                            colnames(fb), selected = "Lifetime.Post.Consumptions"),
                                selectInput("y_axis", "Y variable", 
                                            colnames(fb), selected = "Post.hour"),
                                selectInput("size", "Sized by ordinal variables", 
                                            ordinal_variables, selected = "Post.Weekday"),
                                selectInput("color", "Colored by nominal variables", 
                                            nominal_names, selected = "Type"))
                   ),
  
  conditionalPanel(condition = "input.conditionedPanels==2",
                   sidebarPanel(width = 4,
                                selectInput("scatter_vars", "Scatter Plot Variables", 
                                            multiple = T,
                                            choices = numeric_names,
                                            selected = c("Page.total.likes", 
                                                         "Lifetime.Post.Total.Reach", 
                                                         "Lifetime.Post.Total.Impression")),
                                radioButtons("scatter_color",
                                             "Color by",
                                             choices = c("None", "Select variables"),
                                             selected = c("Select variables")),
                                conditionalPanel(condition = "input.scatter_color != 'None'",
                                                              selectInput('color_by',
                                                                          'Select categorical variable to plot:',
                                                                           choices = factor_names,
                                                                           selected = c("Paid"))))
                   ),
  
  conditionalPanel("input.conditionedPanels==3",
                   sidebarPanel(width = 3,
                                selectizeInput("parallel_vars", "Select parallel variables(>1)",
                                               colnames(fb), multiple = T,
                                               selected = c("share", 
                                                            "Post.Hour",
                                                            "comment",
                                                            "Lifetime.Post.Total.Reach",
                                                            "like")),
                                selectInput("parallel_color", "Select color variable", 
                                            choices = factor_names,
                                            selected = "Post.Weekday"))
                   ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Bubble plot", 
               br(),
               tags$h4("Bubble Plot"),
               uiOutput("ggvis_ui"),ggvisOutput("ggvis"), value = 1),
      tabPanel("Scatter plot", 
               br(),
               plotOutput("scatter_plot"),value = 2),
               # plotlyOutput("scatter_plot", width = 800, height = 600),
      tabPanel("Parallel coordinates plot",
               br(),
               plotlyOutput("parallel_plot",width = 700, height = 500),
               value = 3),
      id = "conditionedPanels"
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #########################################################################################
  #                                < BUBBLE PLOT > --ggivs                                #
  #########################################################################################
  
  # create plot
  bub <- reactive({
    
    names <- c(input$x_axis, input$y_axis,input$color, input$size)
    
    # create id
    df1 <- fb
    df1$id <- 1:nrow(df1)

    # hover function
    hover_values <- function(x) {
      if(is.null(x)) return(NULL)
      row <- df1[df1$id==x$id, names]
      paste0("<b>",names(row), ": ","</b>", format(row), collapse = "<br />")
    }
    
    # reactive dataframe to plot
    df1 %>%
      ggvis(x = ~df1[[names[1]]], y = ~df1[[names[2]]], key :=~id,
            fill=~df1[[names[3]]], fillOpacity := 0.6, fillOpacity.hover := 1,
            # stroke := NA, stroke.hover = ~fb[[names[3]]], 
            strokeWidth := 4, strokeOpacity := 0.5) %>%
      add_tooltip(hover_values, "hover") %>%

      layer_points(size = ~ df1[[input$size]]) %>%   # auto scaled rather than ":"
      add_legend("fill", title = input$color,
                 properties = legend_props(legend = list(y = 20),
                                           title = list(fontSize=12),
                                           label=list(fontSize=12) )) %>%
      add_legend("size", title = input$size,
                 properties = legend_props(legend = list(y = 100),
                                           title = list(fontSize=12),
                                           label=list(fontSize=9))) %>%
      # hide_legend("stroke") %>% 
      add_axis("y", title = input$y_axis,
              properties = axis_props(title = list(fontSize=10), label=list(fontSize=8)))  %>%
      add_axis("x", title = input$x_axis,
             properties = axis_props(title = list(fontSize=10), label=list(fontSize=8))) %>% 
      set_options(height = 500, width = 700, duration=0)
  })
  
  # pipe to ggvis
  bub %>% bind_shiny("ggvis", "ggvis_ui")

  #########################################################################################
  #                               < SCATTER PLOT >  --GGally                              #
  #########################################################################################
  
  output$scatter_plot <- renderPlot({
    
    ## color by variable
    if (input$scatter_color!='None'){
      color.var <- input$color_by
      color.by <- color.var
      color.by.inTitle <- color.var
    }else{
      color.var <- factor_names
      color.by <- factor(1)
      color.by.inTitle <- "no variables"
    }
    
    ## define data frame to ploy based on selected color variables
    df <- reactive({
      df <- fb
      df = fb[,c(input$scatter_vars, color.var)]
      df
    })
    
    ## plot variables and color variables 
    num_vals <- names(sapply(df(), is.numeric))[sapply(df(), is.numeric)]
    fac_vals <- names(sapply(df(), is.factor))[sapply(df(), is.factor)]
    
    ## specify lower matrix format: simple liear regression with fitted line
    lm_fn <- function(data, mapping, method="loess", ...){
      p <- ggplot(data = data, mapping = mapping) + 
        geom_point() + 
        geom_smooth(method=method, ...)
      p
    }
    
    ## wrap to plot
    ggpairs(data=df(),
            columns= which(names(df()) %in% num_vals), 
            # upper = list(continuous = "density"),
            # lower = list(combo = "facetdensity"),
            lower = list(continuous = wrap(lm_fn, method="lm")),
            # lower=list(combo=wrap("facethist",binwidth=1)),
            title= paste("Scatteer Plot Matrix Colored by '", color.by.inTitle, "'." ),
            aes_string(colour=color.by, alpha=0.5),
            legends=T)+
      
    ## add theme outside of ggpairs 
    theme(legend.position="right",
          plot.title = element_text(lineheight=.8, face="bold", size=20))
  })

  
  #########################################################################################
  #                         < PARALLEL PLOT >  --plotly + GGally                          #
  #########################################################################################
  output$parallel_plot <- renderPlotly({
    validate(
      need(length(input$parallel_vars) >= 2, label = "Select at least 2 variables to start plotting")
    )
    
    ggparcoord(fb, columns = which(colnames(fb) %in% input$parallel_vars),
               groupColumn = which(colnames(fb) == input$parallel_color),
               scale = "uniminmax", 
               scaleSummary = "center",
               showPoints = T,
               alphaLines = 0.5,
               title= "Parallel Plot") +
      theme_bw() +
      theme(axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(lineheight=.8, face="bold", size=20))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
