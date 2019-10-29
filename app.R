library(shiny)
library(dplyr)
library(plyr)
library(plotly)
library(stringr)
library(ggplot2)

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
                                   "NDP",
                                   "Green Party",
                                   "Bloc Québécois",
                                   "Left",
                                   "Right"),
                           Color=c("blue",
                                   "red",
                                   "orange",
                                   "green",
                                   "rgb(51, 153, 255)",
                                   "rgb(255, 102, 102)",
                                   "rgb(51, 153, 255)"))
full_dat <- data.frame()
for(year in c(2006, 2008, 2011, 2015)){
    full_dat <- rbind(full_dat, read.csv(paste(year, '_election_combined.csv',
                                               sep=""),))
}

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
                plotlyOutput("overTime", height='100%')
            )
        ),
        tabPanel(
            "Per Election Results",
            sidebarLayout(
                sidebarPanel(
                    selectInput("election_year",
                                "Select an Election Year:",
                                c("2015",
                                  "2011",
                                  "2008",
                                  "2006"),
                                selected = "2015"),
                    radioButtons("count_type",
                                 "Count Type for Figure:",
                                 c("By Riding",
                                   "By National Popular Vote",
                                   "Left vs Right Leaning - By Riding",
                                   "Left vs Right Leaning - By National Popular Vote"))
                ),
        
                # Show a plot of the generated distribution
                mainPanel(
                   plotlyOutput("distPlot"),
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
            ),
        
        tabPanel(
            "Results by Riding(s)",
            fluidPage(
                fluidRow(
                    column(
                        12,
                        wellPanel(
                            selectizeInput("ridings",
                                           "Select riding(s):",
                                           unique(full_dat$Electoral.District.Name_English),
                                           selected = c("Barrie"), multiple = TRUE)
                        )
                    )
                ),
                plotlyOutput("byRidingVotes", height='100%')
            )
        )
    )
)


    server <- function(input, output) {
        output$byRidingVotes <- renderPlotly({
            ridings <- input$ridings
            dat <- filter(full_dat, Electoral.District.Name_English %in% ridings)
            
            time_dat <- data.frame()
            for(year in unique(dat$Year)){
                print(year)
                temp_dat <- filter(dat, Year == year)
                if(length(temp_dat)>0){ # in case a new riding at a given year
                    temp <- count_type(temp_dat, 'Votes')
                    row.names(temp) <- temp$Party
                    temp <- data.frame(temp["Votes"])
                    temp <- cbind(t(temp), Year=year)
                    
                    time_dat <- rbind.fill(time_dat, data.frame(temp))
                    time_dat[is.na(time_dat)] <- 0
                }
            }
            new_col_names <- c()
            for(col in colnames(time_dat)){
                x <- str_replace_all(col, '[.]', ' ')
                new_col_names <- c(new_col_names, x)
            }
            colnames(time_dat) <- new_col_names
            
            p <- plot_ly(time_dat, type='scatter', mode = 'markers+lines')
            for(party in colnames(time_dat)){
                if(party != 'Year'){
                    party_color <- get_party_color(party)
                    p <- p %>% add_trace(x=time_dat$Year,
                                         y = time_dat[, party],
                                         name = party,
                                         marker = list(color = party_color),
                                         line =list(color = party_color)
                    )
                }
            }
            p <- p %>% layout(title = paste('Election Results ',
                                            min(dat$Year),
                                            '-',
                                            max(dat$Year),
                                            ' by ',
                                            typ,
                                            ' for: ',
                                            paste(ridings, sep=", "),
                                            sep=""),
                              xaxis=list(title='Election Year'),
                              yaxis=list(title='Votes'))
            p
        })
        
        output$overTime <- renderPlotly({
            typ <- input$over_time_count_type
            
            time_dat <- data.frame()
            for(year in unique(full_dat$Year)){
                dat <<- filter(full_dat, Year==year)
                temp <- count_type(dat, typ)
                row.names(temp) <- temp$Party
                temp <- data.frame(temp["Seats"])
                temp <- cbind(t(temp), Year=year)
                
                time_dat <- rbind.fill(time_dat, data.frame(temp))
                time_dat[is.na(time_dat)] <- 0
            }
            
            new_col_names <- c()
            for(col in colnames(time_dat)){
                x <- str_replace_all(col, '[.]', ' ')
                new_col_names <- c(new_col_names, x)
            }
            colnames(time_dat) <- new_col_names
            
            p <- plot_ly(time_dat, type='scatter', mode = 'markers+lines')
            for(party in colnames(time_dat)){
                if(party != 'Year'){
                    party_color <- get_party_color(party)
                    p <- p %>% add_trace(x=time_dat$Year,
                                         y = time_dat[, party],
                                         name = party,
                                         marker = list(color = party_color),
                                         line =list(color = party_color)
                                         )
                }
            }
            p <- p %>% layout(title = paste('Election Results ',
                                            min(full_dat$Year),
                                            '-',
                                            max(full_dat$Year),
                                            ' by ',
                                            typ,
                                            sep=""),
                              xaxis=list(title='Election Year'),
                              yaxis=list(title='Seats'))
            p
        })

        output$distPlot <- renderPlotly({
            election_year <- input$election_year
            dat <<- filter(full_dat, Year==election_year)
            typ <- input$count_type
            
            res <- count_type(dat, typ)
            res <- merge(res, party_colors)
            res$Color[is.na(res$Color)] <- 'gray'
            res$Party <- factor(res$Party, levels = unique(res$Party)[order(res$Seats, decreasing = TRUE)])
            res <- res[order(res$Seats, decreasing = TRUE), ]
            
            p <- plot_ly(type='bar')
            for(party in unique(res$Party)){
                party_color <- get_party_color(party)
                p <- p %>% add_trace(x=party,
                                     y=res$Seats[res$Party==party],
                                     marker = list(color = party_color),
                                     name=party)
            }
            p
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


get_party_color <- function(x){
    if(x %in% party_colors$Party){
        res <- party_colors$Color[party_colors$Party==x]
        return(res)
    } else {return('gray')}
    
}
    
count_type <- function(x, type) {
    if (type %in% c('By Riding',
                    'Left vs Right Leaning - By Riding')){
        x <- aggregate(x$Candidate.Poll.Votes.Count,
                       by=list(District=x$Electoral.District.Number,
                               Party=x$Political.Affiliation.Name_English),
                       FUN=sum)
    }
    if (type %in% c('By National Popular Vote',
                   'Left vs Right Leaning - By National Popular Vote')
        ){
        total_seats <- length(unique(x$Electoral.District.Number))
        x <- aggregate(x$Candidate.Poll.Votes.Count,
                         by=list(Party=x$Political.Affiliation.Name_English),
                         FUN=sum)
    }
    
    if (type == 'Votes'){
        res <- aggregate(x$Candidate.Poll.Votes.Count,
                        by=list(Party=x$Political.Affiliation.Name_English),
                        FUN=sum)
        print(res)
        colnames(res) <- c('Party', 'Votes')
        return(res)
    }
    
    if (type == 'By Riding'){
        res <- x %>% group_by(District) %>% filter(x == max(x))
        res$Party <- as.factor(res$Party)
        res$Party <- droplevels(res$Party)
        
        res <- data.frame(Party=levels(res$Party),
                          Seats=tabulate(res$Party))
    }
    
    if (type == 'By National Popular Vote'){
        x$Seats <- round((x$x/sum(x$x))*total_seats)
        res <- x %>% filter(Seats >= 1)
    }
    
    if (type == 'Left vs Right Leaning - By Riding'){
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
