#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Behaviour Trend"),

    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #         selectInput("bins",
    #                     "Number of bins:",
    #                     values = 30)
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #        plotOutput("distPlot")
    #     )
    # )
    
    fluidRow(
        
        column(3,
               selectInput("gen", h3("Select Generation"), 
                           choices = unique(diary_ind_mapped$generation), selected = "Gen X")),
        column(3,
               selectInput("dow", h3("Select Day of Week"), 
                           choices = unique(diary_ind_mapped$ddayw), selected = "Mon-Fri")),
        column(3,
               selectInput("hhtype", h3("Select Household Type"), 
                           choices = unique(diary_ind_mapped$dhhtype), selected = "Single person household"))
        ),
    fluidRow(
        plotOutput("plot")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    data<- reactive({
        data<- diary_ind_mapped %>%
            filter(dhhtype==input$hhtype & generation== input$gen &
                       ddayw==input$dow)%>%
            group_by(Macro.Group)%>%
            summarise(freq=n())
    })
    
    output$plot<- renderPlot({
        df<- data()
        ggplot(df,aes(x=reorder(Macro.Group,-freq),y=freq))+
            geom_col()+ 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 7))+
            ylab("Frequency")+xlab("Macro Behaviour")
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
