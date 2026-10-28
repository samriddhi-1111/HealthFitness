library(ggplot2)

fitness <- read.csv("health_fitness.csv")

repeat {
  cat("\n========================================\n")
  cat("   HEALTH AND FITNESS DATA ANALYZER\n")
  cat("========================================\n")
  cat("1. View Fitness Data\n")
  cat("2. Average Fitness Statistics\n")
  cat("3. Check Fitness Goals\n")
  cat("4. Analyze Sleep\n")
  cat("5. Daily Steps Graph\n")
  cat("6. Exercise Duration Graph\n")
  cat("7. Sleep Distribution Graph\n")
  cat("8. Steps vs Calories Graph\n")
  cat("9. Highest and Lowest Values\n")
  cat("10. Fitness Goal Comparison Graph\n")
  cat("11. Save Results\n")
  cat("12. Exit\n")
  
  choice <- suppressWarnings(
    as.integer(readline("Enter your choice: "))
  )
  
  if (is.na(choice)) {
    cat("Please enter a valid number.\n")
    next
  }
  
  if (choice == 1) {
    
    print(fitness)
    
  } else if (choice == 2) {
    
    cat("\nAverage Daily Steps:",
        round(mean(fitness$Steps)), "\n")
    cat("Average Exercise Duration:",
        round(mean(fitness$Exercise_Minutes), 2), "minutes\n")
    cat("Average Sleep:",
        round(mean(fitness$Sleep_Hours), 2), "hours\n")
    cat("Average Calories Burned:",
        round(mean(fitness$Calories_Burned), 2), "\n")
    
  } else if (choice == 3) {
    
    cat("Days achieving 8000 steps:",
        sum(fitness$Steps >= 8000), "\n")
    cat("Days achieving 30 minutes of exercise:",
        sum(fitness$Exercise_Minutes >= 30), "\n")
    
  } else if (choice == 4) {
    
    cat("Average Sleep:",
        round(mean(fitness$Sleep_Hours), 2), "hours\n")
    cat("Days with less than 7 hours of sleep:",
        sum(fitness$Sleep_Hours < 7), "\n")
    
    print(table(ifelse(
      fitness$Sleep_Hours < 7,
      "Below 7 Hours",
      "7 Hours or More"
    )))
    
  } else if (choice == 5) {
    
    print(
      ggplot(fitness, aes(x = Day, y = Steps)) +
        geom_line(color = "#2563EB", linewidth = 1.2) +
        geom_point(color = "#F97316", size = 3) +
        labs(
          title = "Daily Steps Analysis",
          x = "Day",
          y = "Number of Steps"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(
            color = "#7C3AED", face = "bold", size = 16
          ),
          axis.title = element_text(
            color = "#2563EB", face = "bold"
          )
        )
    )
    
  } else if (choice == 6) {
    
    print(
      ggplot(fitness, aes(
        x = factor(Day),
        y = Exercise_Minutes,
        fill = Exercise_Minutes
      )) +
        geom_col(color = "white") +
        scale_fill_gradient(
          low = "#67E8F9",
          high = "#7C3AED"
        ) +
        labs(
          title = "Daily Exercise Duration",
          x = "Day",
          y = "Exercise (Minutes)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(
            color = "#7C3AED", face = "bold", size = 16
          ),
          axis.title = element_text(
            color = "#2563EB", face = "bold"
          ),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )
    )
    
  } else if (choice == 7) {
    
    print(
      ggplot(fitness, aes(x = Sleep_Hours)) +
        geom_histogram(
          bins = 8,
          fill = "#14B8A6",
          color = "white",
          linewidth = 1
        ) +
        labs(
          title = "Sleep Hours Distribution",
          x = "Sleep Hours",
          y = "Number of Days"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(
            color = "#7C3AED", face = "bold", size = 16
          ),
          axis.title = element_text(
            color = "#2563EB", face = "bold"
          )
        )
    )
    
  } else if (choice == 8) {
    
    print(
      ggplot(fitness, aes(
        x = Steps,
        y = Calories_Burned,
        color = Calories_Burned
      )) +
        geom_point(size = 3, alpha = 0.9) +
        geom_smooth(
          aes(group = 1),
          method = "lm",
          se = FALSE,
          color = "#EF4444",
          linewidth = 1.2
        ) +
        scale_color_gradient(
          low = "#06B6D4",
          high = "#7C3AED"
        ) +
        labs(
          title = "Steps vs Calories Burned",
          x = "Daily Steps",
          y = "Calories Burned",
          color = "Calories"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(
            color = "#7C3AED", face = "bold", size = 16
          ),
          axis.title = element_text(
            color = "#2563EB", face = "bold"
          )
        )
    )
    
  } else if (choice == 9) {
    
    cat("Highest Steps:", max(fitness$Steps), "\n")
    cat("Lowest Steps:", min(fitness$Steps), "\n")
    cat("Maximum Exercise Duration:",
        max(fitness$Exercise_Minutes), "minutes\n")
    cat("Minimum Exercise Duration:",
        min(fitness$Exercise_Minutes), "minutes\n")
    cat("Maximum Sleep:",
        max(fitness$Sleep_Hours), "hours\n")
    cat("Minimum Sleep:",
        min(fitness$Sleep_Hours), "hours\n")
    cat("Maximum Calories Burned:",
        max(fitness$Calories_Burned), "\n")
    cat("Minimum Calories Burned:",
        min(fitness$Calories_Burned), "\n")
    
  } else if (choice == 10) {
    
    fitness$Goal_Status <- ifelse(
      fitness$Steps >= 8000,
      "Goal Achieved",
      "Goal Not Achieved"
    )
    
    print(
      ggplot(fitness, aes(
        x = Goal_Status,
        fill = Goal_Status
      )) +
        geom_bar(color = "white", linewidth = 1) +
        scale_fill_manual(
          values = c(
            "Goal Achieved" = "#22C55E",
            "Goal Not Achieved" = "#F87171"
          )
        ) +
        labs(
          title = "Daily Step Goal Achievement",
          x = "Goal Status",
          y = "Number of Days",
          fill = "Goal Status"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(
            color = "#7C3AED", face = "bold", size = 16
          ),
          axis.title = element_text(
            color = "#2563EB", face = "bold"
          ),
          legend.position = "bottom"
        )
    )
    
  } else if (choice == 11) {
    
    write.csv(
      fitness,
      "D:/Samriddhi/CU/Sem 1/Statistical Techniques using R/Mini Project/health_fitness_results.csv",
      row.names = FALSE
    )
    
    cat("Results saved as health_fitness_results.csv\n")
    
  } else if (choice == 12) {
    
    cat("Thank you for using the analyzer!\n")
    break
    
  } else {
    
    cat("Invalid choice. Select a number from 1 to 12.\n")
    
  }
}