
install.packages(c(
 # "dplyr",
 # "ggplot2",
 # "car",
 # "rpart",
 # "rpart.plot",
 # "randomForest",
 # "caret"
))



              

bike <- read.csv(file.choose())
head(bike[, c("season", "yr", "holiday",
              "workingday", "weathersit")])

str(bike)

## Convert categorical variables into factors
bike$season <- factor(bike$season)

bike$yr <- factor(bike$yr)

bike$holiday <- factor(bike$holiday)

bike$workingday <- factor(bike$workingday)

bike$weathersit <- factor(bike$weathersit)

# Check data structure and missing values
str(bike)
colSums(is.na(bike))
str(bike)
colSums(is.na(bike))

write.csv(bike, "bike_cleaned.csv", row.names = FALSE)

# Select numerical variables
cor_data <- bike[, c("cnt", "temp", "atemp",
                     "hum", "windspeed")]

# Calculate Pearson correlation
cor_matrix <- cor(cor_data, method = "pearson")

# Display correlation results
round(cor_matrix, 3)

# Temperature and bike rentals
cor.test(bike$temp, bike$cnt, method = "pearson")

# Humidity and bike rentals
cor.test(bike$hum, bike$cnt, method = "pearson")

# Windspeed and bike rentals
cor.test(bike$windspeed, bike$cnt, method = "pearson")

library(ggplot2)

ggplot(bike, aes(x = temp, y = cnt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Temperature and Bike Rentals",
    x = "Temperature",
    y = "Total Daily Bike Rentals"
  ) +
  theme_minimal()

# Calculate average daily rentals by season
season_summary <- aggregate(
  cnt ~ season,
  data = bike,
  FUN = mean
)

print(season_summary)

# Perform one-way ANOVA
season_anova <- aov(cnt ~ season, data = bike)

# Display ANOVA results
summary(season_anova)

# Check equality of variances
library(car)
leveneTest(cnt ~ season, data = bike)

# Check normality of ANOVA residuals
shapiro.test(residuals(season_anova))

# Run Kruskal-Wallis alternative
kruskal.test(cnt ~ season, data = bike)

library(ggplot2)

ggplot(season_summary,
       aes(x = season, y = cnt)) +
  geom_col() +
  labs(
    title = "Average Daily Bike Rentals by Season",
    x = "Season",
    y = "Average Daily Rentals"
  ) +
  theme_minimal()
ggsave(
  "seasonal_bike_rentals.png",
  width = 8,
  height = 5,
  dpi = 300
)

pairwise.wilcox.test(
  bike$cnt,
  bike$season,
  p.adjust.method = "bonferroni"
)
