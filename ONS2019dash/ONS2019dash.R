library(magrittr)

ggplot2::theme_set(ggplot2::theme_bw())

ONS_2019= readRDS('geo_ons.rds')
#exclude year, Region, areaName and the first 12
colchoice = colnames(ONS_2019@data)[c(13:42, 44:58, 60:ncol(ONS_2019@data))]
downloadchoice = colnames(ONS_2019@data)[c(12, 43, 13:42, 44:58, 60:ncol(ONS_2019@data))]

# provide pretty names
names(colchoice) = c("Female pop.",
                     "Male pop.",
                     "Mean female age",
                     "Mean male age",
                     "Median age female",
                     "Median age male",
                     "Mode age female",
                     "Mode age male",
                     "St.dev age female",
                     "St.dev age male",
                     "Female teens pop.",
                     "Male teens pop.",
                     "Female young adults pop.",
                     "Male young adults pop.",
                     "Female adults pop.",
                     "Male adults pop.",
                     "Female elderly pop.",
                     "Male elderly pop.",
                     "Female very elder pop.",
                     "Male very elder pop.",
                     "Total pop.",
                     "Mean age",
                     "Median age",
                     "Mode age",
                     "St.dev age",
                     "Teen pop.",
                     "Young adults pop.",
                     "Adults pop.",
                     "Elderly pop.",
                     "Very elderly pop.",
                     "GDP (£ million)",
                     "Female LE at 65",
                     "Female LE at birth",
                     "Male LE at 65",
                     "Male LE at birth",
                     "Lower CI Female LE at 65",
                     "Lower CI Female LE at birth",
                     "Lower CI Male LE at 65",
                     "Lower CI Male LE at birth",
                     "Upper CI Female LE at 65",
                     "Upper CI Female LE at birth",
                     "Upper CI Male LE at 65",
                     "Upper CI Male LE at birth",
                     "Number of suicides",
                     "Unemployment rate(%)",
                     "Anxiety avg. score(/10)",
                     "Happiness avg. score(/10)",
                     "Life satisfaction avg. score(/10)",
                     "Worthwhile avg. score(/10)",
                     "Anxiety avg. range",
                     "Happiness avg. range",
                     "Life satisfaction avg. range",
                     "Worthwhile avg. range",
                     "Mean hourly pay",
                     "Mean hourly pay (ft)",
                     "Mean hourly pay (pt)",
                     "Mean hourly pay (fem)",
                     "Mean hourly pay (fem - ft)",
                     "Mean hourly pay (fem - pt)",
                     "Mean hourly pay (male)",
                     "Mean hourly pay (male - ft)",
                     "Mean hourly pay (male - pt)",
                     "Median hourly pay",
                     "Median hourly pay (ft)",
                     "Median hourly pay (pt)",
                     "Median hourly pay (fem)",
                     "Median hourly pay (fem - ft)",
                     "Median hourly pay (fem - pt)",
                     "Median hourly pay (male)",
                     "Median hourly pay (male - ft)",
                     "Median hourly pay (male - pt)",
                     "CoV - Mean hourly pay",
                     "CoV - Mean hourly pay (ft)",
                     "CoV - Mean hourly pay (pt)",
                     "CoV - Mean hourly pay (fem)",
                     "CoV - Mean hourly pay (fem - ft)",
                     "CoV - Mean hourly pay (fem - pt)",
                     "CoV - Mean hourly pay (male)",
                     "CoV - Mean hourly pay (male - ft)",
                     "CoV - Mean hourly pay (male - pt)",
                     "CoV - Median hourly pay",
                     "CoV - Median hourly pay (ft)",
                     "CoV - Median hourly pay (pt)",
                     "CoV - Median hourly pay (fem)",
                     "CoV - Median hourly pay (fem - ft)",
                     "CoV - Median hourly pay (fem - pt)",
                     "CoV - Median hourly pay (male)",
                     "CoV - Median hourly pay (male - ft)",
                     "CoV - Median hourly pay (male - pt)" )

principal = function(){
    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
        shinyBS::bsCollapse(
            id = 'Search', open = 'Set up your search (click to collapse/expand)',
            shinyBS::bsCollapsePanel(
                'Set up your search (click to collapse/expand)',
                shiny::em('Note 1:'), shiny::p(' In search bar, LE means life
           expectancy, pop. stands for population, ft for full time, pt. for part time,
           fem. for female, CoV for Coefficient of Variation, avg. for average, St.dev for
           standard deviation, CI for confidence interval '),
                shiny::em('Note 2:'), shiny::p(' for population statistics, teens means younger than 17, young adults
           means between 17 and 30, adults between 30 and 64, elderly between 64 and 81 and
           very elderly older than 81.'),
                shiny::selectInput(
                    "datasource",
                    "Data source",
                    choices = c('All', 'Population', 'Age',
                                'Work/Economy', 'Wellbeing',
                                'Life expectancy'),
                    selected = 'All'),
                shiny::checkboxInput(
                    'per', 'Second column', value = FALSE),
                shiny::column(
                    2,
                    shiny::selectInput("column",
                                       "Main column",
                                       choices = colchoice,
                                       selected = 'TotalPeople_All')
                ),
                shiny::column(
                    1,
                    shiny::conditionalPanel(condition = "input.per == true",
                                            shiny::selectInput("operation",
                                                               "Operation",
                                                               choices = c('/','*', '+', '-'),
                                                               selected = '/')
                    )
                ),
                shiny::column(
                    2,
                    shiny::conditionalPanel(condition = "input.per == true",
                                            shiny::selectInput("seccolumn",
                                                               "Per column",
                                                               choices = colchoice,
                                                               selected = 'sum_spent')
                    )
                ),
                shiny::checkboxInput('log10', 'log10 scale', value = FALSE),
                shiny::actionButton('submit', 'Submit', icon = shiny::icon('refresh'))
            )),
        shiny::mainPanel(
            width = 12,
            shiny::h1('Main panel'),
            shiny::fluidRow(
                shiny::column(
                    6,
                    shiny::h2('Map'),
                    shinycssloaders::withSpinner(
                        leaflet::leafletOutput("map", width = '100%', height = 600)
                    )
                ),
                shiny::column(
                    6,
                    shiny::h2('Histogram'),
                    shinycssloaders::withSpinner(
                        plotly::plotlyOutput('hist', width = '100%', height = 600)
                    )
                )
            )
        )
    )
}

# Define UI for application that draws a histogram
ui <- function(){
    shiny::navbarPage(
        "ONS data visualisation in 2019 by Local Authority District",
        theme = shinythemes::shinytheme('darkly'),
        shiny::tabPanel(
            title = 'Interactive map',
            principal()
        ),
        shiny::tabPanel(
            title = 'Data exploration',
            shiny::mainPanel(
                width = 12,
                shiny::downloadButton('downloadData', 'Download complete dataset'),
                shiny::h1(),
                DT::dataTableOutput('table'))
        )
    )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    ## column selection
    shiny::observe({
        
        x = input$datasource
        if ( x == 'Population'){
            shiny::updateSelectInput(session, "column",
                                     choice = colchoice[c(1:2, 11:21, 26:30)],
                                     selected = 'TotalPeople_All'
            )
            shiny::updateSelectInput(session, "seccolumn",
                                     choice = colchoice[c(1:2, 11:21, 26:30)],
                                     selected = 'TotalPeople_All'
            )
            
        } else if (x == 'Age') {
            shiny::updateSelectInput(session, "column",
                                     choice = colchoice[c(4:10, 22:25)],
                                     selected = 'MeanAge_All'
            )
            shiny::updateSelectInput(session, "seccolumn",
                                     choice = colchoice[c(4:10, 22:25)],
                                     selected = 'MeanAge_All'
            )
        } else if (x == 'Work/Economy') {
            shiny::updateSelectInput(session, "column",
                                     choice = colchoice[c(31, 45, 54:89)],
                                     selected = 'Pay_Mean_All_All'
            )
            shiny::updateSelectInput(session, "seccolumn",
                                     choice = colchoice[c(31, 45, 54:89)],
                                     selected = 'GDP.millionGBP.'
            )
        } else if (x == 'Wellbeing') {
            shiny::updateSelectInput(session, "column",
                                     choice = colchoice[c(44,46:53)],
                                     selected = 'Value_Happiness'
            )
            shiny::updateSelectInput(session, "seccolumn",
                                     choice = colchoice[c(44,46:53)],
                                     selected = 'Value_Anxiety'
            )
        } else if (x == 'Life expectancy') {
            shiny::updateSelectInput(session, "column",
                                     choice = colchoice[c(32:43)],
                                     selected = 'Value_LE_FemAtBirth'
            )
            shiny::updateSelectInput(session, "seccolumn",
                                     choice = colchoice[c(32:43)],
                                     selected = 'Value_LE_MaleAtBirth'
            )
        } else {
            shiny::updateSelectInput(session, "column",
                                     choice = colchoice,
                                     selected = 'TotalPeople_All'
            )
            shiny::updateSelectInput(session, "seccolumn",
                                     choice = colchoice,
                                     selected = 'TotalPeople_All'
            )
            
        }
        
    })
    
    # PRGn pallete is not bad
    # Spectral is good too, these 2 are both divergent
    
    #Sequential palette are not bad either
    
    pal <- leaflet::colorNumeric('YlOrRd', NULL)
    # map
    output$map <- leaflet::renderLeaflet({
        input$submit ## depends on submit button
        col = shiny::isolate(input$column) ## shiny::isolate makes the output no dependent on the state of the
        if (shiny::isolate(input$per)) col = paste0(col, shiny::isolate(input$operation), shiny::isolate(input$seccolumn))
        # column
        logscale = shiny::isolate(input$log10)
        
        m = leaflet::leaflet(ONS_2019, height = 200) %>%
            leaflet::addProviderTiles('Stamen.TonerLite') %>%
            leaflet::setView(lng = -1.4, lat = 52.7, zoom = 6)
        
        if (logscale){ #ifelse in the fillcolor column does not work
            m %>%
                leaflet::addPolygons(stroke = FALSE,
                                     smoothFactor = 0.3,
                                     fillOpacity = 1,
                                     fillColor = ~pal(log10(eval(parse(text = col)))), # this function likes it parse
                                     ## alternatively you could pass log10(ONS_2019@data[,col]
                                     label = ~paste0(lad19nm, ": ",
                                                     ezplot::ez_labels(log10(eval(parse(text = col))),
                                                                       signif = 3))) %>%
                leaflet::addLegend(pal = pal,
                                   values = ~log10(eval(parse(text = col))),
                                   title = paste0('log10 of ',col),
                                   opacity = 1.0)
        } else {
            m %>%
                leaflet::addPolygons(stroke = FALSE,
                                     smoothFactor = 0.3,
                                     fillOpacity = 1,
                                     fillColor = ~pal(eval(parse(text = col))),
                                     label = ~paste0(lad19nm, ": ",
                                                     ezplot::ez_labels(eval(parse(text = col)),
                                                                       signif = 3))) %>%
                leaflet::addLegend(pal = pal,
                                   values = ~(eval(parse(text = col))),
                                   title = col,
                                   opacity = 1.0)
        }
        
    })
    
    #histogram
    output$hist = plotly::renderPlotly({
        input$submit
        col = shiny::isolate(input$column)
        if (shiny::isolate(input$per)) col = paste0(col, '/', shiny::isolate(input$seccolumn))
        logscaled = shiny::isolate(input$log10)
        
        if (logscaled){
            gp = ggplot2::ggplot(ONS_2019@data, ggplot2::aes_string(x = col, label = 'areaName')) +
                ggplot2::scale_x_log10() +ggplot2::xlab(paste('log10 of', col))
        } else {
            gp =ggplot2::ggplot(ONS_2019@data, ggplot2::aes_string(x = col, label = 'areaName'))
        }
        plotly::ggplotly(
            gp +
                ggplot2::geom_histogram(fill = 'orange', color = 'black') +
                ggplot2::geom_point(ggplot2::aes(y = -10),
                                    position = ggplot2::position_jitter(height = 5), size=1,
                                    fill = 'orange', color = 'black', shape = 21)+
                ggplot2::ylab('Count')
        )
    })
    
    ## Data exploration tab
    output$table = DT::renderDataTable({
        DT::datatable(ONS_2019@data[,downloadchoice],
                      style = 'bootstrap',
                      filter = 'top',
                      options = list(lengthMenu = c(10,30,50,100,200)))
    })
    
    # download button
    output$downloadData <- shiny::downloadHandler(
        filename = 'ons_data_2019.csv',
        content = function(file) {
            utils::write.csv(ONS_2019@data[,downloadchoice], file, row.names = FALSE)
        }
    )
    
    
}

# Run the application
shiny::shinyApp(ui = ui, server = server)

