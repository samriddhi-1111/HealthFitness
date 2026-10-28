set.seed(123)

days <- 1:30

fitness <- data.frame(
  Day = days,
  Steps = sample(3000:15000, 30, replace = TRUE),
  Exercise_Minutes = sample(10:90, 30, replace = TRUE),
  Sleep_Hours = round(runif(30, 4.5, 9), 1),
  Calories_Burned = sample(150:700, 30, replace = TRUE)
)

write.csv(fitness, "health_fitness.csv", row.names = FALSE)

print(fitness)