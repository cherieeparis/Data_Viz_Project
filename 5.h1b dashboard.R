# Group Project 

# Preparation
## Load Necessary Libraries
library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
library(shinydashboard)
library(janitor)
library(readr)
library(lubridate)
library(ggplot2)
library(DT)

## Data Preparation
h1b <- read_csv("combined_h1b_rate.csv", show_col_types = FALSE) %>% 
  clean_names()
lca <- read_csv("LCA_final_title_cleaned.csv") 


# Data Preprocessing
## Part 1: Interactive Map on Approval Rate and Number of Companies
### Data Cleaning
h1b <- h1b %>% 
  filter(state != "XX")
h1b <- h1b %>% 
  mutate(sector = ifelse(sector == "Public Administration (not covered in economic census)", 
                         "Public Administration", sector))

### Select necessary data

h1b_data_all <- h1b %>%
  select(employer, city, state, sector, fiscal_year, initial_approval, initial_denial,  continuing_approval, continuing_denial, approval_rate, total_application)

h1b_data_filtered <- h1b_data_all %>%
  filter(initial_approval + continuing_approval >= 1)


### Calculate the approval rate and number of companies
h1b_counts <- h1b_data_filtered %>%
  group_by(state, sector, fiscal_year) %>%
  summarise(employer_count = n_distinct(employer), .groups = "drop")
h1b_summary <- h1b_data_all %>%
  group_by(state, sector, fiscal_year) %>%
  summarise(mean_approval_rate = mean(approval_rate, na.rm = TRUE), .groups = "drop")

### Merge the results
final_result <- full_join(h1b_counts, h1b_summary, by = c("state", "sector", "fiscal_year")) %>%
  mutate(employer_count = ifelse(is.na(employer_count), 0, employer_count))

### Filters for years and sectors
years <- 2017:2023
sectors <- unique(final_result$sector)

### Calculate the top 10 states with the highest average approval rates
top_approval_rates <- h1b_data_all %>%
  group_by(state) %>%
  summarise(avg_approval_rate = mean(approval_rate, na.rm = TRUE)) %>%
  arrange(desc(avg_approval_rate)) %>%
  slice_head(n = 10)

### Calculate the top 10 states with the highest number of distinct companies
top_employer_counts <- final_result %>%
  group_by(state) %>%
  summarise(total_employers = sum(employer_count, na.rm = TRUE)) %>%
  arrange(desc(total_employers)) %>%
  slice_head(n = 10)

### Calculate the top 10 sectors with the highest average approval rates
top_approval_rates_sector <- h1b_data_all %>%
  group_by(sector) %>%
  summarise(avg_approval_rate = mean(approval_rate, na.rm = TRUE)) %>%
  arrange(desc(avg_approval_rate)) %>%
  slice_head(n = 10)

### Calculate the top 10 states with the highest number of distinct companies
top_employer_counts_sector <- final_result %>%
  group_by(sector) %>%
  summarise(total_employers = sum(employer_count, na.rm = TRUE)) %>%
  arrange(desc(total_employers)) %>%
  slice_head(n = 10)


## Part 4: Comparative study
# Filter data for New York, California, and Washington
ny_data <- final_result %>% filter(state == "NY")
ca_data <- final_result %>% filter(state == "CA")
wa_data <- final_result %>% filter(state == "WA")

# Function to generate plot
consolidated_approval_plot <- function(ny_data, ca_data, wa_data) {
  plot <- plot_ly() %>%
    add_lines(data = ny_data, x = ~fiscal_year, y = ~mean_approval_rate, name = "New York", line = list(color = 'blue')) %>%
    add_lines(data = ca_data, x = ~fiscal_year, y = ~mean_approval_rate, name = "California", line = list(color = 'green')) %>%
    add_lines(data = wa_data, x = ~fiscal_year, y = ~mean_approval_rate, name = "Washington", line = list(color = 'red')) %>%
    layout(title = "Average H1B Visa Approval Rate Over Time",
           xaxis = list(title = "Fiscal Year"),
           yaxis = list(title = "Approval Rate"),
           showlegend = TRUE)
  return(plot)
}


## Part 2: Time Series 
### Data preparation for time series
# approval rate
h1b_rate <- h1b_data_all %>%
  group_by(state, sector, fiscal_year) %>%
  summarise(
    initial_approval = sum(initial_approval, na.rm = TRUE), 
    initial_denial = sum(initial_denial, na.rm = TRUE),  
    continuing_approval = sum(continuing_approval, na.rm = TRUE), 
    continuing_denial = sum(continuing_denial, na.rm = TRUE),
    total_application = sum(total_application, na.rm = TRUE),
    total_approval = sum(initial_approval + continuing_approval, na.rm = TRUE),
    average_approval_rate = sum(total_approval, na.rm = TRUE) / sum(total_application, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(average_approval_rate = ifelse(total_application > 0, average_approval_rate, NA))

# Aggregate rows for initial approvals
h1b_initial <- h1b_data_all %>%
  group_by(state, sector, fiscal_year) %>%
  summarise(
    total_initial_decisions = sum(initial_approval + initial_denial, na.rm = TRUE),
    initial_approval_rate = sum(initial_approval, na.rm = TRUE) / total_initial_decisions,
    .groups = "drop"
  ) %>%
  mutate(initial_approval_rate = ifelse(total_initial_decisions > 0, initial_approval_rate, NA))


# Aggregate rows for continuing approvals
h1b_continuing <- h1b_data_all %>%
  group_by(state, sector, fiscal_year) %>%
  summarise(
    total_continuing_decisions = sum(continuing_approval + continuing_denial, na.rm = TRUE),
    continuing_approval_rate = sum(continuing_approval, na.rm = TRUE) / total_continuing_decisions,
    .groups = "drop"
  ) %>%
  mutate(continuing_approval_rate = ifelse(total_continuing_decisions > 0, continuing_approval_rate, NA))

# Merge h1b_counts and h1b_summary
merged_data <- full_join(h1b_counts, h1b_rate, by = c("state", "sector", "fiscal_year"))

# Merge merged_data with h1b_initial
merged_data <- full_join(merged_data, h1b_initial, by = c("state", "sector", "fiscal_year"))

# Merge merged_data with h1b_continuing
final_result2 <- full_join(merged_data, h1b_continuing, by = c("state", "sector", "fiscal_year"))

# Replace NA values in 'employer_count' column with 0
final_result2 <- final_result2 %>%
  mutate(employer_count = ifelse(is.na(employer_count), 0, employer_count))

# Filter out the NA and "XX" states
states2 <- unique(final_result2$state[!is.na(final_result2$state) & final_result2$state != "XX"])
sectors2 <- unique(final_result2$sector[!is.na(final_result2$sector)])


### Part 3: LCA Data
#### lca data processing section

# data process 
# convert receive data and decision date to date format a
lca <- lca %>%
  mutate(
    RECEIVED_DATE = ymd(RECEIVED_DATE, quiet = TRUE),  # Parse date, assuming no time component
    DECISION_DATE = ymd(DECISION_DATE, quiet = TRUE)  # Parse date, assuming no time component
  ) %>%
  filter(!is.na(RECEIVED_DATE) & !is.na(DECISION_DATE)) %>%
  mutate(
    DECISION_DAYS = as.numeric(DECISION_DATE - RECEIVED_DATE)  # Calculate decision days
  )

# get year from receive date
lca <- lca %>%
  mutate(YEAR = format(RECEIVED_DATE, "%Y"))

# use received period to represent the month for second trends' figure (for those whose case was received by that month)
lca$PERIOD <- floor_date(lca$RECEIVED_DATE, unit = "month")

# select data table column
sd_1 <- lca %>% 
  select(YEAR, JOB_TITLE, EMPLOYER_NAME, WORKSITE_CITY, WORKSITE_POSTAL_CODE, HOURLY_WAGE, HOURLY_PREVAILING_WAGE) %>%
  mutate(HOURLY_WAGE = as.integer(HOURLY_WAGE),
         HOURLY_PREVAILING_WAGE = as.integer(HOURLY_PREVAILING_WAGE))

# only keep distinct value in data table 
lca0 <- sd_1 %>%
  distinct(YEAR, JOB_TITLE, EMPLOYER_NAME, WORKSITE_CITY, WORKSITE_POSTAL_CODE, HOURLY_WAGE, HOURLY_PREVAILING_WAGE, .keep_all = TRUE)

lca1 <- lca %>%
  filter(YEAR %in% c("2021","2022", "2023"))

### select top 1% wage for top 10 soc titles
top_soc_titles <- lca %>%
  arrange(desc(HOURLY_WAGE)) %>%
  slice(1:(n()*0.01)) %>%
  count(SOC_TITLE) %>%
  top_n(10, n)


# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "H1B Sponsorship"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("H1B Metrics by State and Sector", tabName = "h1b"),
      menuItem("Wage by State and Sector", tabName = "lca"),
      menuItem("Comparisons", tabName = "compare")
    )
  ),
  dashboardBody(
    
    ## Section 1: About ~~~~~~~
    tabItems(
      tabItem(tabName = "about", class = "active", 
              fluidPage(
                tags$div(
                  tags$h1("Project Purpose"),
                  column(12,p("Our project focuses on providing comprehensive insights into the H-1B visa program for stakeholders. 
                              Leveraging historical data from the USCIS H-1B Employer Data Hub and employers' Labor Condition Applications data, 
                              we aim to uncover variations in lottery outcomes across states and industries, 
                              as well as disparities in prevailing salaries.")),
                  
                  column(12,p("By analyzing historical data on H-1B visa outcomes,  
                              we aim to offer valuable insights into trends across states, time periods, and sectors. 
                              This analysis will aid international talents considering H-1B visa applications, providing them with 
                              essential information for decision-making.")),
                  
                  column(12,p("Additionally, we will conduct a comparative analysis focusing on New York and other key states such as 
                              Washington and California. By comparing top sponsoring industries and prevailing salary trends, 
                              we aim to provide a nuanced understanding of regional dynamics within the H-1B visa landscape.")),                  
                  
                  tags$h1("Data Sources"),
                  column(12, p("We utilize two primary datasets for our analysis:")),
                  
                  column(12,p("1. USCIS H-1B Employer Data Hub: 
                              This dataset
                              contains information on H-1B visa petition decisions, providing valuable insights into approval rates, 
                              trends, and outcomes over a significant period. The dataset includes the first decisions USCIS made 
                              on petitions for initial and continuing employment from fiscal year 2009 through fiscal year 2023
                              The dataset can be accessed through https://www.uscis.gov/tools/reports-and-studies/h-1b-employer-data-hub.")), 
                  
                  column(12,p("2. LCA Data (Labor Condition Applications):
                              This dataset is derived from employers' Labor Condition Applications (Form ETA-9035), includes final determinations 
                              issued by the Department's Office of Foreign Labor Certification (OFLC). It provides crucial details on 
                              prevailing wages, employer attestations, and other pertinent information related to H-1B, H-1B1, and 
                              E-3 visa programs.
                              The dataset can be accessed through https://www.dol.gov/agencies/eta/foreign-labor/performance.")), 
                  
                  tags$h1("Team Members"),
                  p("Fiona Fang, Yuyang Zhang, Cherie Xu, Zixuan Pan")
                )
              )
      ),
      
      ## Section 2: H1B datahub ~~~~~~~
      tabItem(tabName = "h1b",
              fluidPage(
                titlePanel("H1B Metrics by State and Sector"),
                
                # Add description text here
                tags$hr(),
                h3("Interactive Maps Visualizations"),
                p("This map provides insights into the H1B visa approval rates and the number of companies that sponsor 
                  H1B visas across different states. 
                  Select different years and sectors to see how trends have changed over time."), 
                
                # First plot here
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    selectInput("year", "Select Year:",
                                choices = years,
                                selected = max(years)),
                    selectInput("sector", "Select Sector:",
                                choices = sectors,
                                selected = sectors[1]),
                    radioButtons("metric", "Select Metric to Display:",
                                 choices = c("H1B Approval Rate" = "approval_rate",
                                             "Number of Companies that Have H1B Sponsor" = "employer_count"),
                                 selected = "approval_rate")
                  ),
                  mainPanel(
                    plotlyOutput("map")
                  )
                ),
                
                # Add description text here
                tags$hr(),
                h3("Top 10 States and Sectors:"),
                p("This plot shows the top 10 states and sectors with the highest approval rate or highest number of companies that sponsor H1B."), 
                p("Select the metrics, state or sector to display the results. "), 
                
                # Second plot here
                tabsetPanel(
                  tabPanel("States",
                           sidebarLayout(
                             sidebarPanel(
                               width = 3,
                               selectInput("dataChoiceStates", "Choose Data Type:", choices = c("Approval Rates" = "rate", "Number of Companies" = "companies"))
                             ),
                             mainPanel(
                               plotlyOutput("plotStates")
                             )
                           )
                  ),
                  tabPanel("Sectors",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("dataChoiceSectors", "Choose Data Type:", choices = c("Approval Rates" = "rate", "Number of Companies" = "companies"))
                             ),
                             mainPanel(
                               plotlyOutput("plotSectors")
                             )
                           )
                  )
                ),
                
                # Add description text here
                tags$hr(),
                h3("Trends of H1B Visa Application Metrics"),
                p("This line plot shows the trends of H1B Visa application metrics over the years."),
                p("Select state and sector to check the approval rate changes during FY 2017 - 2023."),
                p("Initial approvals refer to H-1B petitions with “New employment” or “New concurrent employment” selected on Part 2, Question 2 of the Form I-129."),
                p("Continuing approvals refer to H-1B petitions with anything other than “New employment” or “New concurrent employment”, including continuing employment, change of employer, and amended petitions."),
                
                # Third plot here
                sidebarLayout(
                  sidebarPanel(
                    width = 3, 
                    selectInput("state", "State:", choices = unique(states2)),
                    selectInput("sector", "Sector:", choices = unique(sectors2))
                  ),
                  mainPanel(
                    plotOutput("line_plot")
                  )
                ),
              ),
              ################NOT SURE
      ),
      
      ## Section 3: LCA ~~~~~~~
      tabItem(tabName = "lca", 
              fluidPage(
                titlePanel("H1B Wage and Case Processing Information"),
                
                # section for hourly wage insights
                h3("Hourly Wage"),
                p("Explore wage distributions by job title and employer over time. Year indicates the case reception date, 
                while Prevailing Wage represents the average wage for similar roles in the specified area by the U.S. Department of Labor. 
                Our analysis focuses on hourly wages ranging from $0 to $125, covering approximately 99% of the dataset. 
                All wages at or above $125 are included.
                Please allow for a brief loading time due to the size of the dataset."), 
                fluidRow(
                  column(width = 3,
                         sliderInput("MinSalaryRange", "Select the minimum hourly wage:",
                                     min = min(sd_1$HOURLY_WAGE, na.rm = TRUE), 
                                     max = 125,  # adjust exclusive top 1% wage 
                                     value = 26, step = 1)
                  ),
                  column(width = 9,
                         dataTableOutput("table")
                  )
                ),
                
                # Section for Decision Days Trends
                tags$h3("Decision Days by Job Title"),
                p("Explore how decision days vary across different SOC titles. 
                  In the interactive tab, three trend lines display the variations in processing times. 
                  These include the maximum and minimum waiting days for a decision, as well as the average decision dates. 
                  The data is presented on a monthly basis, offering employers valuable insights into the processing times for each position."),
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    selectizeInput("selected_soc_title", "Select Job Title:",
                                   choices = NULL,  # Initially, no choices.
                                   options = list(maxOptions = 1000))
                  ),
                  mainPanel(
                    plotlyOutput("trendPlot")
                  )
                ),
                
                # third part of top 1% 
                tags$h3("Top 1% Group's Wage with Top 10 SOC Titles"),
                p("This section visualizes the top SOC titles that fall within the top 1% of wages. 
                  It highlights which job categories typically have the highest wages. 
                  The plot shows the distribution of wages for the top 1% of earners within each SOC title, providing insights into wage disparities across different job categories."),
                plotOutput("topSOCPlot")  
              )
      ),
      
      
      ## Section 4: Comparisons ~~~~~~~
      tabItem(tabName = "compare", 
        fluidPage(
          titlePanel("Comparisons between New York and California/Washington"),
          
          # Description and headers
          tags$hr(),
          h3("Interactive Visualizations"),
          p("This plot provides insights into the H1B visa approval rates among states. 
                  Select different sectors to see how trends have changed over time."), 
          
          fluidRow(
            column(width = 12,
                   plotlyOutput("approvalPlot")))
        )
)
)
)
)


# Server Logic
server <- function(input, output) {
  
  # Generating the map output
  output$map <- renderPlotly({
    data <- final_result %>% filter(fiscal_year == input$year & sector == input$sector)
    
    metric_value <- if (input$metric == "approval_rate") {
      list(z = ~mean_approval_rate, colorbar_title = "Approval Rate")
    } else {
      list(z = ~employer_count, colorbar_title = "Number of Companies that Have H1B Sponsor")
    }
    
    plot_ly(data,
            z = metric_value$z, 
            text = ~paste("State: ", state,
                          "<br>",
                          if (input$metric == "approval_rate") {
                            paste("Approval Rate: ", round(mean_approval_rate, 3))
                          } else {
                            paste("Number of Companies: ", employer_count)
                          }),
            locations = ~state,
            type = 'choropleth',
            locationmode = 'USA-states',
            hoverinfo = 'text',
            colors = colorRampPalette(c("#cce5ff", "#99ccff", "#3366ff"))(10)  # Shades of blue from light to dark
    ) %>% 
      layout(title = paste("H1B Data in", input$year, "- Sector:", input$sector),
             geo = list(scope = 'usa'),
             colorbar = list(title = metric_value$colorbar_title))
  })
  
  output$plotStates <- renderPlotly({
    data <- if (input$dataChoiceStates == "rate") {
      top_approval_rates
    } else {
      top_employer_counts
    }
    
    plot_ly(data, x = ~data[[2]], y = ~data[[1]], type = 'bar',
            hoverinfo = 'text', text = ~paste(data[[1]], ': ', round(data[[2]], 2)),
            orientation = 'h',
            marker = list(color = "#cce5ff")) %>%  # Set custom color here
      layout(title = paste("Top 10 States by", if (input$dataChoiceStates == "rate") "Approval Rates" else "Number of Companies"),
             xaxis = list(title = if (input$dataChoiceStates == "rate") "Average Approval Rate" else "Total Employers"),
             yaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE))
  })
  
  output$plotSectors <- renderPlotly({
    data <- if (input$dataChoiceSectors == "rate") {
      top_approval_rates_sector
    } else {
      top_employer_counts_sector
    }
    
    plot_ly(data, x = ~data[[2]], y = ~data[[1]], type = 'bar',
            hoverinfo = 'text', text = ~paste(data[[1]], ': ', round(data[[2]], 2)),
            orientation = 'h',
            marker = list(color = "#cce5ff")) %>%  # Apply the same custom color
      layout(title = paste("Top 10 Sectors by", if (input$dataChoiceSectors == "rate") "Approval Rates" else "Number of Companies"),
             xaxis = list(title = if (input$dataChoiceSectors == "rate") "Average Approval Rate" else "Total Employers"),
             yaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE))
  })
  
  # Output for the line plot within the "h1b" tab
  output$line_plot <- renderPlot({
    # Filter data based on user input
    filtered_data <- final_result2 %>%
      filter(state == input$state & sector == input$sector)
    
    # Calculate the maximum value for employer count for dynamic scaling
    max_count <- max(filtered_data$employer_count, na.rm = TRUE)
    
    # Plot trend lines
    ggplot(filtered_data, aes(x = fiscal_year)) +
      geom_line(aes(y = employer_count, color = "Employer Count"), size = 1) +
      geom_line(aes(y = average_approval_rate * max_count, color = "Approval Rate"), size = 1) +
      geom_line(aes(y = initial_approval_rate * max_count, color = "Initial Approval Rate"), size = 1) +
      geom_line(aes(y = continuing_approval_rate * max_count, color = "Continuing Approval Rate"), size = 1) +
      scale_y_continuous(
        name = "Employer Count",
        limits = c(0, max_count),
        sec.axis = sec_axis(~./max_count, name="Approval Rate", breaks = seq(0, 1, by = 0.2))
      ) +
      labs(title = paste("Trends for", input$state, "-", input$sector),
           x = "Fiscal Year") +
      scale_color_manual(values = c("Employer Count" = "blue", 
                                    "Approval Rate" = "red", 
                                    "Initial Approval Rate" = "green", 
                                    "Continuing Approval Rate" = "orange")) +
      theme_minimal()
  })
  
  # Prepare reactive data based on sector selection
  filtered_data <- reactive({
    if (input$sector != "All") {
      sector_filtered <- final_result %>% filter(sector == input$sector)
    } else {
      sector_filtered <- final_result
    }
    sector_filtered
  })
  
  # Generate plot based on the state-specific data and reactive sector filtering
  output$approvalPlot <- renderPlotly({
    # Filter for each state, ensuring the sector filtering is respected
    ny_data <- filtered_data() %>% filter(state == "NY")
    ca_data <- filtered_data() %>% filter(state == "CA")
    wa_data <- filtered_data() %>% filter(state == "WA")
    
    # Generate the plot using the specific state data
    consolidated_approval_plot(ny_data, ca_data, wa_data)
  })
  
  
  # for lca -  first data table 
  output$table <- renderDataTable({
    f_b <- lca0 %>%
      filter(HOURLY_WAGE >= input$MinSalaryRange) %>%
      arrange(HOURLY_WAGE) 
    
    colnames(f_b) <- c("Year", "Job Title", "Employer", "Worksite", "Postal Code", "Wage", "Prevailing Wage")
    datatable(f_b, options = list(pageLength = 20))
  })
  
  # for new plot - top 1% wages
  output$topSOCPlot <- renderPlot({
    top_soc_titles <- lca %>%
      arrange(desc(HOURLY_WAGE)) %>%
      slice(1:(n()*0.01)) %>%
      count(SOC_TITLE, sort = TRUE) %>%
      top_n(10, n)  # Include the top categories
    
    ggplot(top_soc_titles, aes(x = reorder(SOC_TITLE, n), y = n)) +
      geom_bar(stat = "identity", fill = "#cce5ff") +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 14),
            plot.title = element_text(size = 15)) +
      labs(x = "SOC Title", y = "Count", title = "Top 1% Wage Group's SOC Titles")
  })
  
  
  
  # for days trend
  reactive_soc_titles <- reactive({
    unique(lca1$SOC_TITLE)
  })
  
  observe({
    updateSelectizeInput(inputId = "selected_soc_title", choices = reactive_soc_titles())
  })
  
  # Reactive data filtered by selected SOC_TITLE
  filtered_lca <- reactive({
    lca1 %>%
      filter(SOC_TITLE == input$selected_soc_title, DECISION_DAYS != 0, !is.na(DECISION_DAYS)) %>%
      group_by(PERIOD) %>%
      summarize(
        MIN_DECISION_DAYS = min(DECISION_DAYS, na.rm = TRUE),
        MAX_DECISION_DAYS = max(DECISION_DAYS, na.rm = TRUE),
        AVG_DECISION_DAYS = mean(DECISION_DAYS, na.rm = TRUE),
        .groups = 'drop'  # Ensures the result is no longer grouped
      )
  })
  
  # Generate plot
  output$trendPlot <- renderPlotly({
    trend_lca <- filtered_lca()
    if (nrow(trend_lca) > 0) {
      p <- ggplot(trend_lca, aes(x = PERIOD)) +
        geom_line(aes(y = MIN_DECISION_DAYS, colour = "Minimum"), linewidth = 1) +
        geom_line(aes(y = MAX_DECISION_DAYS, colour = "Maximum"), linewidth = 1) +
        geom_line(aes(y = AVG_DECISION_DAYS, colour = "Average"), linewidth = 1) +
        scale_color_manual(values = c("Minimum" = "#1E90FF", "Maximum" = "#FF7F50", "Average" = "#3CB371")) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
        labs(x = "Month", y = "Decision Days",
             title = paste("Trends of Decision Days for", input$selected_soc_title),
             colour = "Metric") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 7), 
              legend.position = "bottom")
      ggplotly(p)  # Convert ggplot to plotly for interactivity
    } else {
      p <- ggplot() + 
        labs(title = "No data available for selected SOC title",
             subtitle = "Please select a different SOC title")
      ggplotly(p)
    }
  })

}


# Running the Shiny App
shinyApp(ui = ui, server = server)

