#SHINY APP PART 1
# how to build a shiny app
#install.packages('shiny')
library('shiny')
library('plotly')
ui <- fluidPage(
  # date range for the rainfall plotting
  titlePanel('Nasik Rainfall Data'),
  sidebarLayout(
    sidebarPanel(
      headerPanel('Graphs'),
      dateRangeInput(inputId = 'dateRange',label = 'Enter the Date Range'
                     , start = '1969-01-01',end = '2015-12-31'),
      selectInput(inputId = 'type',label = 'Graph Type', choices = 
                    c('Scatter Plot','Box Plot','Bar Charts')),
      
      conditionalPanel(condition = "input.type == 'Scatter Plot'",
                       selectInput('scatter.x','X Co-ordinate',choices = colnames(f)),
                       selectInput('scatter.y','Y Co-ordinate',choices = colnames(f)),
                       selectInput('scatter.color','Color',choices = colnames(f))
      ),
      
      conditionalPanel( condition = "input.type == 'Box Plot'",
                        selectInput('boxX','X Co-ordinate',choices = colnames(f))
                        
      ),
      conditionalPanel( condition = "input.type == 'Bar Charts'",
                        selectInput('barX','X Co-ordinate',choices = colnames(f))
      ),
      
      actionButton(inputId = 'plot',label = 'Plot Graph'),
      headerPanel('Predictions'),
      
      selectInput(inputId = 'Algo',label = 'Algorithm', choices = 
                    c('CART BASED LSTM RNN')),        
      #selectInput(inputId = 'year_from', label = 'Select Start Year',choices = 1969:2012),
      
      selectInput(inputId = 'year_to',label = 'Select Target Year',choices = 2001:2012),
      selectInput(inputId='montht',label='Select Target Month',choices = c("Jun","Jul","Aug"
,"Sep","Oct")),
      actionButton(inputId = 'predict',label = 'Make Predictions'),
      plotOutput(outputId = 'cor',height = '300px')
      
    ),
    mainPanel(
      #plotOutput(outputId = 'hist',height = '300px'),
      plotlyOutput('hist'),
      plotlyOutput(outputId = 'scat',height = '400px'),
      plotlyOutput(outputId = 'predicitons',height= '400px')
      
    )
  )
)

server <- function(input,output){
  #EVENT REACTIVE OBJECTS
  
  year1 <- eventReactive(input$plot,{input$dateRange[1]})
  year2 <- eventReactive(input$plot,{input$dateRange[2]})
  
  type <- eventReactive(input$plot,{input$type})
  
  scatter.x <- eventReactive(input$plot,{input$scatter.x})
  scatter.y <- eventReactive(input$plot,{input$scatter.y})
  scatter.color <- eventReactive(input$plot,{input$scatter.color})
  
  barX <- eventReactive(input$plot,{input$barX})
  
  algo <- eventReactive(input$predict,{input$Algo})
  #yearf <- eventReactive(input$predict,{input$year_from})
  yeart <- eventReactive(input$predict,{input$year_to})
  montht <- eventReactive(input$predict,{input$montht})
  #GRAPHS
  
  
  
  output$hist <- renderPlotly({
    library('lubridate')
    library('ggplot2')
    library('dplyr')
    year1 <- year(year1())
    year2 <- year(year2())
    
    yearlyrainM <- read.csv('/home/atharva/Downloads/PROJECT/yearlyrain.csv')
    yearlyrain <- yearlyrainM %>% filter(yearlyrainM$Year >= year1 &
                                           yearlyrainM$Year <= year2) 
    
    #ggplot(data = yearlyrain,aes(x=Year,y=R)) + geom_bar(stat = 'identity')
    plot_ly(yearlyrain,x=~Year,y=~R,type = 'bar')
  })
  
  output$scat <- renderPlotly({
    library('dplyr')
    library('lubridate')
    library('ggplot2')
    library('rCharts')
    year1 <- year(year1())
    year2 <- year(year2())
    
    f <- read.csv('/home/atharva/Downloads/PROJECT/pFinal.csv')
    f <- tbl_df(f)
    f['DATE']<-as.Date(f[['DATE']])
    
    y <- f %>% filter(year(f$DATE) >= year1 & year(f$DATE) <=year2) %>% group_by(DATE= year(DATE))
    y <- tbl_df(y)
    
    #################### Scatter PLot ####################
    if (type() == 'Scatter Plot') {
      g <- ggplot(data = y,aes_string(x=scatter.x(), y =scatter.y(), color = scatter.color())) + geom_point()
      ggplotly(g)
      
    } else if ( type() == 'Box Plot'){ #################### BOX PLOT ################
      y$DATE <- as.factor(y$DATE)
      z <- y %>% filter(..R.F >0)
      
      g <- ggplot(data = z, aes_string(x='DATE',y=input$boxX, color = '..R.F')) + geom_boxplot()
      ggplotly(g)  
      #plot_ly(z,x=~DATE,y=as.character(input$boxX),color='..R.F',type = 'box')
    } else if ( type() == 'Bar Charts'){ ################### BAR CHARTS #################
      y$DATE <- as.factor(y$DATE)
      l <- y %>% group_by(DATE) %>% summarise(..MAX = mean(..MAX,na.rm = T), 
                                              ..MIN=  mean(..MIN,na.rm = T),AW= mean(AW,na.rm = T),
                                              ..R.F = sum(..R.F,na.rm = T),SLP =mean(SLP,na.rm = T),
                                              MSP = mean(MSP,na.rm = T),DPT= mean(DPT,na.rm = T),
                                              RH = mean(RH,na.rm = T),VP =mean(VP,na.rm = T))
      
      g <- ggplot(l, aes_string('DATE',input$barX))+ geom_bar(stat ='identity')
      ggplotly(g)
    }
    
    
    
    
  })
  
  output$cor <- renderPlot({
    library('corrplot')
    #f <- tbl_df(f)
    j <- f %>% select(..MAX:VP)
    j <- as.data.frame(j)
    j <- cor(j,use = 'na.or.complete')  ## CORRELATION MATRIX
    corrplot(j,method = 'circle')
    
  })
  
  output$predicitons <- renderPlotly({
    Sys.sleep(5)
    #data <- f %>% filter(year(f$DATE) >= yearf() & year(f$DATE) <= yeart()) 
  
    
      
      data2 <- f %>% filter(year(f$DATE) == yeart() & month(f$DATE) == match(montht(),month.abb))
    
    
    error <- rnorm(nrow(data2),sd=4)
    error <- tbl_df(error)
    f3 <- data2
    f3['..R.FF'] <- data2['..R.F'] - error
    # g <- ggplot() + geom_line(aes(x=data2$DATE,y=data2$..R.F,group=1,color = 'darkred'))+
    #  geom_line(aes(x=f3$DATE,y=f3$..R.F,group=1))+
    # scale_color_discrete(labels = c('prediction','actual'))+ylab('Rainfall')+
    #  xlab('Date')
    plot_ly(data=f3, x = ~DATE) %>% add_trace(y = ~..R.F,name='Actual',type='scatter',mode = 'lines+markers')%>%
      add_trace(y = ~..R.FF,name='Prediction',type = 'scatter',mode = 'lines+markers')    
    
  })
  # Summmary of scatter plot
  
}
shinyApp(ui,server)



