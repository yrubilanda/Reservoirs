library(readr)
library(ggplot2)

reservoir <- c('Palace', 'Temple', 'Tikal', 'Corriental', 'Inscription', 'Causeway', 'Bejucal', 'Madeira', 'Perdido', 'Hidden', 'Subin', 'Las Chamacas', 'Pital Aguada')
CH_Vol <- c(38680, 27140, 21060, 17380, 15310, 14270, 7380, 6340, 3070, 2620, 400, 350, 310)
Geo_Vol <- c(28437, 12455, 811, 988, 217, 9416, 221, 1605, 638, 3793, 45, 69, 104)
HS_Vol <- c(45633, 24613, 2391, 33217, 26232, 23424, 5857, 5377, 2340, 23822, 51, 621, 127)
Depth_Vol <- c(38235, 34813, 12013, 12910, 20169, 11698, 3541, 6172, 59, 3417, 0, 0, 0)


reservoirs <- data.frame(reservoir, CH_Vol, Geo_Vol, HS_Vol, Depth_Vol)
print(reservoirs)

#Carr and Hazard versus Georeferenced
# Fit linear model
lm_model1 <- lm(CH_Vol ~ Geo_Vol, data = reservoirs)

# Extract R² value
r_squared1 <- summary(lm_model1)$r.squared

# Print R² value to check if it is calculating correctly
print(paste("R² =", round(r_squared1, 3)))  


p1 <- ggplot(data = reservoirs, aes(x = Geo_Vol, y = CH_Vol)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(title = "Linear Regression: Carr and Hazard Volumes vs Georeferenced Volumes",
       x = "Georeferenced (meters cubed)",
       y = "Carr and Hazard (meters cubed)") +
  annotate("text", x = 5000, y = 40000, # Adjust these values as needed
           label = paste("R² =", round(r_squared1, 2)), 
           size = 5, color = "red")

# Print the plot
print(p1)


#Carr and Hazard versus Hillshade
# Fit linear model
lm_model2 <- lm(CH_Vol ~ HS_Vol, data = reservoirs)

# Extract R² value
r_squared2 <- summary(lm_model2)$r.squared

# Print R² value to check if it is calculating correctly
print(paste("R² =", round(r_squared2, 3)))  


p2 <- ggplot(data = reservoirs, aes(x = HS_Vol, y = CH_Vol)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(title = "Linear Regression: Carr and Hazard Volumes vs Hillshade Volumes",
       x = "Hillshade (meters cubed)",
       y = "Carr and Hazard (meters cubed)") +
  annotate("text", x = 5000, y = 40000, # Adjust these values as needed
           label = paste("R² =", round(r_squared2, 2)), 
           size = 5, color = "red")

# Print the plot
print(p2)


#Carr and Hazard versus DepthinSink
# Fit linear model
lm_model3 <- lm(CH_Vol ~ Depth_Vol, data = reservoirs)

# Extract R² value
r_squared3 <- summary(lm_model3)$r.squared

# Print R² value to check if it is calculating correctly
print(paste("R² =", round(r_squared3, 3)))  


p3 <- ggplot(data = reservoirs, aes(x = Depth_Vol, y = CH_Vol)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(title = "Linear Regression: Carr and Hazard Volumes vs DepthinSink Volumes",
       x = "Depth in Sink (meters cubed)",
       y = "Carr and Hazard (meters cubed)") +
  annotate("text", x = 5000, y = 40000, # Adjust these values as needed
           label = paste("R² =", round(r_squared3, 2)), 
           size = 5, color = "red")

# Print the plot
print(p3)