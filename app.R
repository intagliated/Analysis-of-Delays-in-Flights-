
# Load Shiny
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(viridis)
library(bslib)
library(bs4Dash)

data <- read.csv("Airlines.csv")

required_columns <- c("id","Airline","Flight","AirportFrom","AirportTo","DayOfWeek","Time","Length","Delay")

data <- subset(data, select = required_columns)

#code for pie chart
df <- as.data.frame(table((data%>%filter(Delay>0))$DayOfWeek))
colnames(df) <- c("Day_Of_the_Week", "freq")
pie <- ggplot(df, aes(x = "", y= " ", fill = factor(Day_Of_the_Week))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Day_Of_the_Week", 
       x=NULL, 
       y=NULL, 
       title=" Variation in Delay in Different Days of the Week", 
       caption="")

# for plot3 
delays <- aggregate(Delay ~ DayOfWeek, data=data,FUN = sum)


#for plot4 
frequency_table <- table(data$Airline)
frequency_df <- as.data.frame(frequency_table)
frequency_df <- frequency_df[order(frequency_df$Freq, decreasing = TRUE), ]
#custom_colors <- rainbow(18)

#for plot 5 
df_summary1 <- data %>%
  #dplyr::filter(!is.na(Delay) & Delay > 0) %>%  # Filter for rows where there is a delay
  group_by(Airline, Delay) %>%
  summarise(count = n(),.groups='drop')  # Count number of delays

#for plot6
df_summary2 <- data %>%
  dplyr::filter(!is.na(Delay) & Delay > 0) %>%  # Filter for rows where there is a delay
  group_by(Airline, DayOfWeek) %>%
  summarise(count = n())  # Count number of delays


# for plot 9 
colors <- rainbow(10)
popular_routes <- data %>%
  group_by(AirportFrom, AirportTo) %>%
  summarise(route_count = n(), .groups = 'drop') %>%
  ungroup()

top_routes <- popular_routes %>%
  top_n(10, wt = route_count) %>%
  mutate(route_label = paste(AirportFrom, AirportTo))

top_routes <- top_routes %>%
  arrange(desc(route_count))




# Define UI
ui <- dashboardPage(
  skin= NULL,
  dashboardHeader(title = "Flight Delays"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Distribution of Delay", tabName = "dist_delay", icon = icon("chart-bar")),
      menuItem("Delay by Day of the Week", tabName = "delay_day", icon = icon("calendar")),
      menuItem("Flights by Airline", tabName = "flights_airline", icon = icon("plane")),
      menuItem("Delay by Airline", tabName = "delay_airline", icon = icon("clock")),
      menuItem("Delay by Airline and Days", tabName = "delay_airline_days", icon = icon("calendar-alt")),
      menuItem("Departure Times", tabName = "departure_times", icon = icon("clock")),
      menuItem("Flight Length", tabName = "flight_length", icon = icon("ruler")),
      menuItem("Popular Travel Routes", tabName = "popular_routes", icon = icon("route"))
      # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    #bs_theme(version = 4, bootswatch = "minty"),
    tabItems(
      tabItem(
        tabName = "data",
        h3("Analysis of Flight Delays"),
        h6("--------------------------------------------------"),
        h4("Data Description"),
        p("The dataset contains information on whether flights operated by different airlines from various airports are delayed. It contains 539383
      rows and 9 columns."),
        tableOutput("dataTable")
      ),
      tabItem(
        tabName = "dist_delay",
        h4("Frequency Distribution of Delay"),
        p("The plot shows that the data is roughly balanced with around 45% delays. The features can be evaluated to further understand their dependence on the Occurence of Delays."),
        plotOutput("plot1", width = "800px", height = "300px")
      ),
      tabItem(
        tabName = "delay_day",
        h4("Distribution of Delay by Day of the Week"),
        plotOutput("plot3", width = "700px", height = "300px"),
        p("Delays are more scattered in the middle of the week, with higher proportions on Day 3 and Day 4. The minimum delay is observed on Day 6(11%).")
      ),
      tabItem(
        tabName = "flights_airline",
        h4("Frequency of Flights by Airline"),
        plotOutput("plot4"),
        p("Choose the required Flights"),
        
        
        checkboxGroupInput("airlines", "Select Airlines",
                                    choices = unique(data$Airline),
                                    selected = unique(data$Airline)[1:4]))
                 #checkboxInput("select_all", "Select All", FALSE) )
        ,
        #p("in the Dataset The largest number of flights is by WN(13-14%), and the least is by HA.")
      tabItem(
        tabName = "delay_airline",
        h4("Variation of Delay by Airlines"),
        plotOutput("plot5"),
        p("The flights operated by ‘WN’ Airlines are delayed the most, while HA has the least delays.")
      ),
      tabItem(
        tabName = "delay_airline_days",
        h4("Delay by Airline and Days"),
        plotOutput("plot6", width = "1000px", height = "425px"),
        p("The variation of frequency of delays by airlines across different days of the week.")
      ),
      tabItem(
        tabName = "departure_times",
        h4("Scheduled Departure Times"),
        plotOutput("plot7"),
        p("Flights pick up around 4 AM, peak at 8 AM, and decline after 7 PM.")
      ),
      tabItem(
        tabName = "flight_length",
        h4("Length of Flights"),
        plotOutput("plot8"),
        p("Flights shorter than 2 hours are more common in the dataset.")
      ),
      tabItem(
        tabName = "popular_routes",
        h4("Popular Travel Routes"),
        plotOutput("plot9", width = "900px"),
        p("LAX to SFO and its reverse route are the most frequently traveled routes.")
      )
    )
  )
)


  

    #"Conclusions",
    #fluidPage(
      #h4("Results and Conclusion"),
      #p("Worldwide airline delays are one of the many issues that cause the aviation industry to suffer enormous losses. Airports, airlines, and passengers alike often bear the brunt of aircraft delays. These factors make it important to create the appropriate plans in order to maintain the aviation sector's smooth operation. From the data we can gain some insights on different factors that are contributing to the Flight Delay") , # Placeholder for analysis summary,
    
       #"- Roughly 45% of the flights are delayed."),
        #li(""),
        #li(""),
      #)
      #)
  #),
  
  # Tab 5: Settings
  #tabPanel(
    #"Settings",
    #fluidPage(
      #h2("Settings"),
      #checkboxGroupInput(
        #"settingsOptions",
        #"Choose options:",
        #choices = c("Option 1", "Option 2", "Option 3")
      #)
    #)
  #)

# Define the server logic
server <- function(input,output) {
  
  # Handle "Select All" for airlines
  #observeEvent(input$select_all, {
    #updateCheckboxGroupInput(
      #session, "airlines",
      #selected = if (input$select_all) unique(data$Airline) else NULL
    #)
  #})
  
  # Data Overview 
  output$dataTable <- renderTable({
    head(data)  # Display first few rows of the dataset
  })
  
  # Example Plot1
  output$plot1 <- renderPlot({
    
    ggplot(data, aes(y = factor(Delay))) +
      geom_bar(fill="lightblue",color="black",alpha=0.8)+
      labs(x = "Frequency",
           y = "Delay",
           title = "Frequency Distribution of Delay")
    
   
  })
  
  output$plot2 <- renderPlot({
   pie + coord_polar(theta = "y", start=0)+
      theme_minimal() +
      scale_fill_brewer()
  
  })
  
  
  output$plot3 <- renderPlot({
    ggplot(data = delays, aes(x = factor(DayOfWeek), y = Delay)) +
      geom_bar(stat = "identity",color='black', fill = "yellow", alpha = 0.5) +
      labs(title = "Frequency of Flight Delays by Days of the Week", x = "Day of the Week", y = "Frequency of Delay") +
      scale_x_discrete(labels = c("1", "2", "3", "4", "5",
                                  "6", "7")) +
      theme_minimal()
    
  })
  
  output$plot4 <- renderPlot({
    # Filter data based on selected airlines
    validate(
      need(input$airlines != "", "Please select a Flight from the Checkbox"))
    selected_airlines_data <- subset(data, Airline %in% input$airlines)
    
    # Create frequency table for selected airlines
    frequency_table <- table(selected_airlines_data$Airline)
    frequency_df <- as.data.frame(frequency_table)
    frequency_df <- frequency_df[order(frequency_df$Freq, decreasing = TRUE), ]
    #custom_colors <- rainbow(length(input$airlines))
    
    # Create ggplot for frequency of flights by airline
    ggplot(frequency_df, aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) +
      geom_bar(stat = "identity") +
      labs(title = "Frequency of Flights by Airline",
           x = "Airline",
           y = "Frequency") +
      theme_minimal() +
      coord_flip() +
      scale_fill_viridis(discrete=T)
  })
    #ggplot(frequency_df, aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) +
      #geom_bar(stat = "identity") +
      ##    x = "Airline",
      #     y = "Frequency") +
      #theme_minimal() +
      #coord_flip() +
      #scale_fill_viridis(discrete=T)
  #})
  
  output$plot5 <- renderPlot({
    ggplot(df_summary1, aes(x = Airline, y = count, fill = factor(Delay))) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = " Distribution of Delay among different Airlines",
           x = "Airline",
           y = "Count(Delayed/Not Delayed)") +
      
      theme_minimal() +
      scale_fill_brewer(palette='Greens')+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
  })
  output$plot6 <- renderPlot({
    ggplot(df_summary2, aes(x = DayOfWeek, y = count, fill = Airline)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Count of Delays by Airline and Day of the Week",
           x = "Day of the Week",
           y = "Count of Delays") +
      scale_fill_viridis(discrete = TRUE)+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
    
  })
  output$plot7 <- renderPlot({
    ggplot(data, aes(x = Time)) +
      geom_histogram(binwidth = 100, fill = "lightgreen", color = "black", alpha = 0.6) +
      geom_vline(xintercept = seq(0, 1440, by = 100), color = "black", linetype = "dotted") +
      labs(x = "Scheduled Departure Time(mins)",
           y = "Frequency",
           title = "Distribution of Scheduled Departure Times") +
      scale_x_continuous(breaks = seq(0, 2400, by = 200)) +
      theme_minimal()
    
  })
  output$plot8 <- renderPlot({
    ggplot(data, aes(x = Length)) +
      geom_histogram(binwidth = 30, fill = "gold", color = "black", alpha = 0.8) +
      geom_vline(xintercept = seq(0, 655, by = 60), color = "black", linetype = "dotted") +
      labs(x = "Length of the Flight(in mins)",
           y = "Frequency",
           title = "Distribution of Length of the Flights") +
      theme_minimal()
    
  })
  output$plot9 <- renderPlot({
    ggplot(top_routes, aes(x = reorder(route_label, route_count), y = route_count, fill = route_label)) +
      geom_bar(stat = "identity") +
      labs(title = " 10 Most Popular Travel Routes",
           x = "Route (Origin - Destination)",
           y = "Frequency") +
      theme_classic() +
      coord_flip() +
      scale_fill_viridis(discrete = TRUE)
    
  })
  

}

# Run the application
shinyApp(ui = ui, server = server)
