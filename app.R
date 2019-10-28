library(shiny)
library(dplyr)
library(plotly)

total_seats <- 338
left <- data.frame(Party=c('NDP-New Democratic Party',
                      'Green Party',
                      'Liberal',
                      'Communist',
                      'Marxist-Leninist',
                      'Bloc Québécois',
                      'Animal Alliance/Environment Voters'),
                    Leaning=c('Left'))

right <- data.frame(Party=c('Conservative',
                       'Christian Heritage Party',
                       'Libertarian',
                       'PC Party'),
                    Leaning=c('Right'))

left_right <- rbind(left, right)
party_colors <- data.frame(Party=c("Conservative",
                                   "Liberal",
                                   "NDP-New Democratic Party",
                                   "Green Party"),
                           Color=c("blue",
                                   "red",
                                   "orange",
                                   "green"))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Canadian Federal Election Results"),

    # Sidebar with a slider input for number of bins
    tabsetPanel(
        tabPanel(
            "Results Over Time",
            fluidPage(
                fluidRow(
                    column(
                        12,
                        wellPanel(
                            radioButtons("over_time_count_type",
                                         "Count Type for Figure:",
                                         c("By Riding",
                                           "By National Popular Vote",
                                           "Left vs Right Leaning - By Riding",
                                           "Left vs Right Leaning - By National Popular Vote"),
                                         inline=TRUE)
                        )
                    )
                ),
                fluidRow(
                    column(
                        12,
                        h4("Plot Goes Here")
                    )
                )
            )
        ),
        tabPanel(
            "Per Election Results",
            sidebarLayout(
                sidebarPanel(
                    selectInput("election_year",
                                "Select an Election Year:",
                                c("---",
                                  "2015",
                                  "2011",
                                  "2008",
                                  "2006"),
                                selected = "---"),
                    radioButtons("count_type",
                                 "Count Type for Figure:",
                                 c("By Riding",
                                   "By National Popular Vote",
                                   "Left vs Right Leaning - By Riding",
                                   "Left vs Right Leaning - By National Popular Vote"))
                ),
        
                # Show a plot of the generated distribution
                mainPanel(
                   plotOutput("distPlot"),
                )
            ),
            fluidRow(
                column(3,
                       h3("Results by Party - First Past the Post (by Riding)"),
                       tableOutput("summaryTableRiding")),
                column(3,
                       h3("Results by Party - National Popular Vote"),
                       tableOutput("summaryTablePop")),
                column(3,
                       h3("Results by Political Leaning - First Past the Post (by Riding)"),
                       tableOutput("summaryTableLrRiding")),
                column(3,
                       h3("Results by Political Leaning - National Popular Vote"),
                       tableOutput("summaryTableLrPop"))
                )
            )
        )
)
    
    # Define server logic required to draw a histogram
    server <- function(input, output) {
        output$distPlot <- renderPlot({
            election_year <- input$election_year
            fname <- paste(election_year, '_election_combined.csv',
                           sep="")
            dat <<- read.csv(fname)
            typ <- input$count_type
            
            res <- count_type(dat, typ)
            res$Party <- factor(res$Party, levels = unique(res$Party)[order(res$Seats, decreasing = TRUE)])
            
            barplot(
                res$Seats,
                names.arg=res$Party
            )
        })
        
        output$summaryTableRiding <- renderTable({
            election_year <- input$election_year
            count_type(dat, 'By Riding')
        })
        
        output$summaryTablePop <- renderTable({
            election_year <- input$election_year
            count_type(dat, 'By National Popular Vote')
        })
        
        output$summaryTableLrRiding <- renderTable({
            election_year <- input$election_year
            count_type(dat, 'Left vs Right Leaning - By Riding')
        })
        
        output$summaryTableLrPop <- renderTable({
            election_year <- input$election_year
            count_type(dat, 'Left vs Right Leaning - By National Popular Vote')
        })
}


count_type <- function(x, type) {
    if (type == 'By Riding'){
        x <- aggregate(x$Candidate.Poll.Votes.Count,
                       by=list(District=x$Electoral.District.Number,
                       Party=x$Political.Affiliation.Name_English),
                       FUN=sum)
        res <- x %>% group_by(District) %>% filter(x == max(x))
        res$Party <- as.factor(res$Party)
        res$Party <- droplevels(res$Party)
        
        res <- data.frame(Party=levels(res$Party),
                          Seats=tabulate(res$Party))
    }
    
    if (type == 'By National Popular Vote'){
        res <- aggregate(x$Candidate.Poll.Votes.Count,
                        by=list(Party=x$Political.Affiliation.Name_English),
                        FUN=sum)
        res$Seats <- round((res$x/sum(res$x))*total_seats)
        res <- res %>% filter(Seats >= 1)
    }
    
    if (type == 'Left vs Right Leaning - By Riding'){
        x <- aggregate(x$Candidate.Poll.Votes.Count,
                       by=list(District=x$Electoral.District.Number,
                               Party=x$Political.Affiliation.Name_English),
                       FUN=sum)
        x <- left_join(x, left_right, by="Party")
        x <- aggregate(x$x,
                       by=list(District=x$District,
                               Party=x$Leaning),
                       FUN=sum)
        res <- x %>% group_by(District) %>% filter(x == max(x))
        res$Party <- as.factor(res$Party)
        res$Party <- droplevels(res$Party)
        
        res <- data.frame(Party=levels(res$Party),
                          Seats=tabulate(res$Party))
    }
    
    if (type == 'Left vs Right Leaning - By National Popular Vote'){
        x <- aggregate(x$Candidate.Poll.Votes.Count,
                       by=list(Party=x$Political.Affiliation.Name_English),
                       FUN=sum)
        x <- left_join(x, left_right, by="Party")
        res <- aggregate(x$x,
                        by=list(Party=x$Leaning),
                        FUN=sum)
        res$Seats <- round((res$x/sum(res$x))*total_seats)
        res <- res %>% filter(Seats >= 1)
    }
    
    res$Seats <- round(res$Seats, 0)
    return(res[, c("Party", "Seats")][order(res$Seats, decreasing=TRUE),])
}


# Run the application 
shinyApp(ui = ui, server = server)
