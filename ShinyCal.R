pacman::p_load(tidyverse, ggplot2, DT, shiny,lubridate)

library(shiny)
ui <- fluidPage(
  titlePanel("SBS Lab Gantt Chart"),
  
 # sidebarPanel()
  mainPanel(
    HTML(paste(
      ("To track number of lab members, upload a csv file with the following 5 columns:"),'<br/>',
      ("Name (string), Start (d.m.y), End (d.m.y), Role (string), FTE (string) "),'<br/>','<br/>',
      ("Contact Ryssa Moffat for template (ryssa.moffat (at) gess.ethz.ch). "),'<br/>','<br/>')),
 
    radioButtons(inputId = 'sep', label = 'Separator',
                 choices = c(Comma=',',Semicolon=';'), selected = ','),
    fileInput('datafile', 'Choose CSV file',
              accept=c('csv', 'comma-separated-values','.csv')),
  plotOutput('plot1'),
  plotOutput('plot2')
))

server <- function(input, output,session) {
 

#### DF1
dataframe<-reactive({
  if (is.null(input$datafile))
      return(NULL)
  data<-read.csv(input$datafile$datapath, sep=input$sep)
  start_month <- min(dmy(data$Start))
  end_month <- max(dmy(data$End))
  months <- seq(from = start_month, 
                to = end_month, 
                by = "days")
  
  data_b <- merge(data, months) %>%
    mutate(active = case_when((dmy(Start) <= ymd(y) & ymd(y) <= dmy(End)) ~ "active",
                              TRUE ~"inactive")) %>%
    pivot_longer(cols = c(Start, End),
                 names_to = "Start_End",
                 values_to = "Dates") %>%
    mutate(Dates = dmy(Dates),
           Year = case_when(str_sub(as.character(Dates),6,11) == "01-01" ~ Dates,
                            TRUE ~ NA),
           FTE = factor(FTE),
           Role = factor(Role, levels = c("Professor", "Office Manager",
                                          "Postdoc", "PhD", "Masters", "RA",
                                          "Intern", "Guest")),
           role_int = as.integer(case_when(Role == "Professor" ~ 1, 
                                           Role == "Office Manager" ~ 2,
                                           Role == "Postdoc" ~ 3,
                                           Role == "PhD" ~ 4, 
                                           Role == "Masters" ~ 5,
                                           Role == "RA" ~ 6,
                                           Role == "Intern" ~ 7,
                                           Role == "Guest" ~ 8, TRUE ~ 9))) %>%
    filter(active == "active")
      
})
  
#### PLOT1
output$plot1 <- renderPlot({
  
  theme_set(theme_light()+
              theme(
                plot.title = element_text(size=rel(2),face="bold"),
                axis.title = element_text(size=rel(1.2),face="bold"),
                axis.text = element_text(size=rel(1.2),colour = 'black'),
                strip.text = element_text(size=rel(1.2),colour = 'black', 
                                          face = "bold"),
                legend.text = element_text(size=rel(1.2)),
                legend.title = element_text(size=rel(1.2),face="bold"),
                panel.grid.minor = element_blank()))
  
  ggplot(dataframe(), aes(x=y, y= reorder(Name, -role_int),
                     group=Name, color = Role, linetype = FTE)) +
    geom_vline(xintercept = dataframe()$Year)+
    geom_line(linewidth = 2)+
    labs(title = "Personnel Calendar (18 months forecast)", x = "Dates", y = "Names")+
    scale_x_date(date_breaks = "1 months",
                 limits = c(Sys.Date()-months(3), Sys.Date()+months(18)))+
    scale_color_brewer(palette = "Set2") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
})



#### DF2
dataframe2<-reactive({
  if (is.null(input$datafile))
    return(NULL)
  data<-read.csv(input$datafile$datapath, sep=input$sep)
  start_month <- min(dmy(data$Start))
  end_month <- max(dmy(data$End))
  months <- seq(from = start_month,
                to = end_month,
                by = "month")

  count_active_employees <- function(month) {
    sum(dmy(data$Start) <= month & (is.na(dmy(data$End)) | dmy(data$End) >= month))
  }

  monthlySum <- sapply(ymd(months), count_active_employees)

  totalPeople <- data.frame(
    month = months,
    n = monthlySum) %>%
    mutate(Year = factor(year(month)))
  
  totalPeople
})

#### PLOT2
output$plot2 <- renderPlot({
  
  theme_set(theme_light()+
              theme(
                plot.title = element_text(size=rel(2),face="bold"),
                axis.title = element_text(size=rel(1.2),face="bold"),
                axis.text = element_text(size=rel(1.2),colour = 'black'),
                strip.text = element_text(size=rel(1.2),colour = 'black', 
                                          face = "bold"),
                legend.text = element_text(size=rel(1.2)),
                legend.title = element_text(size=rel(1.2),face="bold"),
                panel.grid.minor = element_blank()))
  
  ggplot(dataframe2(), aes(x = month, y = n, fill = Year))+
    geom_bar(stat = "identity", alpha = 0.8)+
    geom_text(aes(label=n), vjust=-.4, size = 3) +
    labs(title = "Total people (full time span)", x = "Date", y = "Number of people")+
    scale_x_date(date_breaks = "2 months")+
    scale_fill_brewer(palette = "Spectral") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

}

shinyApp(ui, server)