library(tidyr)
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(shinyWidgets)
library(purrr)
data <- read_excel("DS_student_request.xlsx")
data <- data %>% filter(!is.na(FirstYear_CGPA))

ui <- fluidPage(
  titlePanel("AUA Students Interactive Dashborad"),
  
  # Placeholder text below the title
  HTML("<br><p>Welcome to Dashboard, meticulously crafted for the vibrant community of the American University of Armenia (AUA).
       This interactive tool is designed to cater to the diverse needs of both students and staff, offering insightful 
       and detailed visual representations of various academic and administrative datasets. 
       Whether you are a student exploring academic trends, a faculty member conducting research,
       or an administrative staff member seeking data-driven insights for decision-making, 
       this dashboard serves as a powerful resource. Navigate through the tabs to uncover valuable information about enrollment trends, 
       academic performances, major selections, and more, all tailored to enhance the academic and operational excellence at AUA.</p><br>"),
  
  tabsetPanel(
    tabPanel("Bar Chart",
             sidebarLayout(
               sidebarPanel(
                 h4("Bar Chart Filters"),
                 selectInput("selectedYear", "Select Year:", choices = c("All", unique(data$FirstEnrolled_Year))),
                 selectInput("selectedGender", "Select Gender:", choices = c("All", unique(data$Gender))),
                 checkboxInput("toggleFinancialAid", "Toggle Financial Aid", FALSE),
                 selectInput("selectedMajor", "Select Major:", choices = c("All", unique(data$FirstEnrolled_MajorCode)))
               ),
               mainPanel(
                 HTML("<br><p>This Bar Chart provides a dynamic view of the Mean First-Year CGPA across different genders, financial aid statuses, years, and majors. </p><br>"),
                 plotOutput("barChart")
                 # Placeholder text for Bar Chart
               )
             )
    ),
    
    tabPanel("Density Plot",
             sidebarLayout(
               sidebarPanel(
                 h4("Density Plot Filters"),
                 selectInput("Major1", "Select First Major for Density Plot:", choices = unique(data$FirstEnrolled_MajorCode)),
                 selectInput("Major2", "Select Second Major for Density Plot:", choices = unique(data$FirstEnrolled_MajorCode))
               ),
               mainPanel(
                 HTML("<br><p>The Density Plot offers an insightful comparison between two selected majors in terms of student 
                      enrollment density over a range of GPAs. Adjust the plot using the filters to choose different majors and 
                      compare their GPA distributions.</p><br>"),
                 plotOutput("densityPlot")
               )
             )
    ),
    
    tabPanel("School GPA Plot VS First Year GPA",
             sidebarLayout(
               sidebarPanel(
                 h4("Filters for Both Plots"),
                 selectInput("linePlotMajor", "Select Major:", choices = c("All", unique(data$FirstEnrolled_MajorCode))),
                 sliderInput("linePlotYearRange", "Select Year Range:", 
                             min = min(data$FirstEnrolled_Year, na.rm = TRUE), 
                             max = max(data$FirstEnrolled_Year, na.rm = TRUE),
                             value = c(min(data$FirstEnrolled_Year, na.rm = TRUE), max(data$FirstEnrolled_Year, na.rm = TRUE)),
                             step = 1)
               ),
               mainPanel(
                 HTML("<br><p>This section showcases two line plots comparing the average School GPA and the average First-Year CGPA over 
                      selected years for a chosen major. The Year Range slider allows for a specific time frame focus, enabling a
                      longitudinal view of academic performance trends within the selected major at the university.
                      This comparison can highlight how students' GPAs evolve from their school performance to their first year in college.</p><br>"),
                 plotOutput("schoolGpaPlot"),
                 plotOutput("firstYearGpaPlot")
               )
             )
    ),
    
    tabPanel("Gender Distribution",
             sidebarLayout(
               sidebarPanel(
                 h4("Gender Distribution Filters"),
                 selectInput("majorInput6", "Select Major:", choices = c("Total", unique(data$FirstEnrolled_MajorCode))),
                 selectInput("selectedYear3", "Select Year:", choices = c("All", unique(data$FirstEnrolled_Year))),
                 checkboxInput("toggleFinancialAid1", "Toggle Financial Aid", FALSE),
               ),
               mainPanel(
                 HTML("<br><p>The Gender Distribution Pie Chart visually represents the proportion of genders
                      within a selected major or across the entire university (when 'Total' is selected).</p><br>"),
                 plotOutput("genderDistributionPlot")
               )
             )
    ),
    
    
    tabPanel("Comparison Plot",
             sidebarLayout(
               sidebarPanel(
                 h4("Comparison Plot Filters"),
                 selectInput("comparisonPlotMajor", "Select Major:", choices =  c("BAB","BSCS","BAEC"))
               ),
               mainPanel(
                 HTML("<br><br><p>This Comparison Plot enables an in-depth analysis of a selected major CGPA at first year VS
                      at the final year.<br>Please note that BCDS and BCES are not selectable due to the fact that the data is not available.</p>"),
                 plotOutput("comparisonPlot")
               )
             )
    )
  )
)


# Server
server <- function(input, output) {
  # Helper function to convert academic years to Date
  convert_to_date <- function(year) {
    if (!is.na(year) && year != "" && nchar(as.character(year)) == 4) {
      as.Date(paste0(year, "-01-01"))
    } else {
      return(NA)
    }
  }
  
  
  filterData <- function(data, inputMajor) {
    if (inputMajor == "Total") {
      return(data)
    } else {
      return(data %>% filter(FirstEnrolled_MajorCode == inputMajor))
    }
  }
  
  output$barChart <- renderPlot({
    filtered_data <- data %>%
      filter((FirstEnrolled_Year == input$selectedYear | input$selectedYear == "All"),
             (Gender == input$selectedGender | input$selectedGender == "All"),
             (FincialAid_Received_AtLeast_Once == input$toggleFinancialAid | input$toggleFinancialAid == FALSE),
             (FirstEnrolled_MajorCode == input$selectedMajor | input$selectedMajor == "All")) %>%
      group_by(Gender, FincialAid_Received_AtLeast_Once) %>%
      summarise(Mean_GPA = mean(FirstYear_CGPA, na.rm = TRUE))
    
    dodge_width <- position_dodge(width = 0.9) # Set dodge width to match geom_bar
    
    ggplot(filtered_data, aes(x = Gender, y = Mean_GPA, fill = factor(FincialAid_Received_AtLeast_Once))) +
      geom_bar(stat = "identity", position = dodge_width) +
      geom_text(aes(label = round(Mean_GPA, 2), y = Mean_GPA + 0.05), # Adjust y position for labels
                position = dodge_width, vjust = 0, color = "black", size = 5) +
      labs(title = "Mean First Year CGPA by Gender, Financial Aid", y = NULL, x = NULL) +
      scale_fill_manual(values = c("0" = "#4342D9", "1" = "#FFDE00"), labels = c("No Aid", "Received Aid"), name ='Aid Status') +
      scale_y_continuous(limits = c(0, 4)) +
      theme_minimal()+
      theme(axis.text.x= element_text(size = 15),
            plot.title = element_text(face = "bold", size = 15, hjust = 0.5))
  })

  
  output$densityPlot <- renderPlot({
    major1_data <- data %>% filter(FirstEnrolled_MajorCode == input$Major1)
    major2_data <- data %>% filter(FirstEnrolled_MajorCode == input$Major2)
    
    ggplot() +
      geom_density(data = major1_data, aes(x = FirstYear_CGPA, fill = input$Major1), alpha = 0.5) +
      geom_density(data = major2_data, aes(x = FirstYear_CGPA, fill = input$Major2), alpha = 0.5) +
      labs(title = "Density Plot of First Year CGPA by Major", x = "First Year CGPA", y = "Density") +
      scale_fill_manual(values = c("Major1" = "#4342D9", "Major2" = "#FFDE00")) +
      theme(legend.position = "none")+
      scale_y_continuous(limits = c(0, 1)) +
      theme_minimal()+
      theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
            axis.title.x= element_text(size = 15),
            axis.title.y = element_blank())+
      scale_fill_manual(values = c("#4342D9", "#FFDE00"), labels = c(input$Major1, input$Major2),name = "Major")
            })

    
  output$schoolGpaPlot <- renderPlot({

    filtered_data <- data %>%
      filter((input$linePlotMajor == "All" | FirstEnrolled_MajorCode == input$linePlotMajor),
             between(FirstEnrolled_Year, input$linePlotYearRange[1], input$linePlotYearRange[2])) %>%
      arrange(FirstEnrolled_Year) %>%
      group_by(FirstEnrolled_Year) %>%
      summarize(Avg_School_GPA = mean(School_GPA, na.rm = TRUE))
    
    ggplot(filtered_data, aes(x = FirstEnrolled_Year, y = Avg_School_GPA, group = 1)) +
      geom_line(color = "#4342D9") +
      geom_point(color = "#4342D9", size = 5) + 
      geom_text(aes(label = round(Avg_School_GPA, 2)), vjust = -1, color = "black", size = 5) + 
      labs(title = "Average School GPA Over Years", x = "Year", y = "Average School GPA") +
      scale_y_continuous(limits = c(0, 5)) +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))+
      coord_cartesian(ylim = c(3,5))+
      theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
            axis.title = element_blank())+
      scale_x_continuous(breaks = unique(filtered_data$FirstEnrolled_Year),
                         labels = as.character(unique(filtered_data$FirstEnrolled_Year)))
      
  })
  
  
  output$firstYearGpaPlot <- renderPlot({
    
    filtered_data <- data %>%
      filter((input$linePlotMajor == "All" | FirstEnrolled_MajorCode == input$linePlotMajor),
             between(FirstEnrolled_Year, input$linePlotYearRange[1], input$linePlotYearRange[2])) %>%
      arrange(FirstEnrolled_Year) %>%
      group_by(FirstEnrolled_Year) %>%
      summarize(Avg_FirstYear_CGPA = mean(FirstYear_CGPA, na.rm = TRUE))
    
    ggplot(filtered_data, aes(x = FirstEnrolled_Year, y = Avg_FirstYear_CGPA, group = 1)) +
      geom_line(color = "#FFDE00") +
      geom_point(color = "#FFDE00", size = 5) +
      geom_text(aes(label = round(Avg_FirstYear_CGPA, 2)), vjust = -1, color = "black", size = 5) + 
      labs(title = "Average First Year CGPA Over Years", x = "Year", y = "Average First Year CGPA") +
      scale_y_continuous(limits = c(0, 4)) +
      theme_minimal()+
      theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
            axis.title = element_blank())+
      coord_cartesian(ylim = c(2,4))+
      scale_x_continuous(breaks = unique(filtered_data$FirstEnrolled_Year),
                         labels = as.character(unique(filtered_data$FirstEnrolled_Year))) 
      
  })
  
  
  
  output$genderDistributionPlot <- renderPlot({
    filtered_data <- data %>%
      filter(input$selectedYear3 == "All" | FirstEnrolled_Year == input$selectedYear3) %>%
      filter(input$toggleFinancialAid1 == FALSE | FincialAid_Received_AtLeast_Once == 1) %>%
      filter(input$majorInput6 == "Total" | FirstEnrolled_MajorCode == input$majorInput6) %>%
      count(Gender) %>%
      mutate(freq = n / sum(n)) %>%
      arrange(desc(Gender)) %>%
      mutate(label_position = cumsum(freq) - 0.5 * freq)
    print(filtered_data)
    observe({
      print(paste("Selected Year:", input$selectedYear))
      print(paste("Financial Aid Toggle:", input$toggleFinancialAid))
    })
    
    if (nrow(filtered_data) == 0) {
      return(ggplot() + labs(title = "No data available"))
    }
    
    # Plotting the pie chart
    ggplot(filtered_data, aes(x = "", y = freq, fill = Gender)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(fill = "Gender", title = "Gender Distribution") +
      scale_fill_manual(values = c("Female" = "#FFDE00", "Male" = "#4342D9")) +
      geom_text(aes(y = label_position, label = scales::percent(freq)), color = "black", size = 8) +
      theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
            legend.title = element_text(size = 15))
  })
  
  
  

  output$comparisonPlot <- renderPlot({
    filtered_data <- data %>%
      filter(FirstEnrolled_Year >= 2013, FirstEnrolled_Year <= 2016,
             FirstEnrolled_MajorCode == input$comparisonPlotMajor) %>%
      gather(key = "GPA_Type", value = "GPA", FirstYear_CGPA, All_CGPA)
    
    if (nrow(filtered_data) > 0) {
      ggplot(filtered_data, aes(x = GPA_Type, y = GPA, fill = GPA_Type)) +
        geom_boxplot() +
        labs(title = "Comparison of First Year and Final Year GPA", x = "", y = "GPA") +
        scale_fill_manual(values = c("FirstYear_CGPA" = "#4342D9", "All_CGPA" = "#FFDE00"),labels = c('All CGPA',"First Year CGPA"), name = "GPA Type")+
        theme_minimal( )+
        theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
              axis.title = element_blank(),
              axis.text.x = element_blank(),
              legend.title = element_text(size = 15))
    } else {
      ggplot() + labs(title = "No data available for selected Major")
    }
  })
}
  # Run the application
shinyApp(ui = ui, server = server)