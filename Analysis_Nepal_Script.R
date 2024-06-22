install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)

setwd("Documents/Project Assistant (Rubee Dev)/Analysis Project (SEA STEPS data WHO)")
data <- read.csv("npl2019.csv")

blue <- data |>
  select(h1,h2a,h2b,h6,h7a,h7b,m8,mhx1,mhx2,mhx3,mhx4,t1,t2,a1,a2,m11,m12,b8,v11,sex,age,p13,p14,c6,d8a,d8b,d1,d2,d3,d4) |>
  mutate(m11 = m11/100) |> #convert the height from cm to m
  mutate(bmi = m12/(m11^2)) |> #create a bmi(kg/ m^2)
  filter(sex == 0) #filter only women in the dataset

#proportion of women in Nepal that is pregnant = 0.0175 (1.75%) and use tobacco = 0.1154 (11.54%), ignoring the NA values
prop <- blue %>%
  summarize(prop_pregnant = mean(m8 == 1, na.rm = TRUE),
            prop_tobacco = mean(t1 == 1, na.rm = TRUE),
            prop_diabet = mean(h7a == 1, na.rm = TRUE),
            prop_hypertension = mean(h2a == 1, na.rm = TRUE)) 

#diabetes and hypertension among women in Nepal (group by pregnant (m8)? diet?)

summary(blue)
blue

# during model selection, make sure to check the model function using summary of the model (summary(lm(formula= y ~ x))), see p values, R squared,
#Model selection (log likelihood (forgot, need to check on the Syllabus of STAT 306), AIC OR CP Mallow's statistics (good CP value is around the same as the number of parameters and low))
# using stepwise process in the dailylog
#afterwards, choosing the right models, and INTERPRET the RESULT OF THE LOGISTIC REGRESSION (i.e. for every 1 kg change in weight, contributes to 0.5 percent heart attack/ stroke)


#Visualizations

# Create a hexbin plot for Height vs Weight for women in Nepal
ggplot(blue, aes(x = m12, y = m11)) +
  stat_binhex(bins = 20) +  # Adjust the number of bins as needed
  scale_fill_viridis_c() +  # Use a color scale for better visualization
  labs(x = "Weight (kg)", y = "Height (m)", fill = "Count", title = "Distribution of Height vs Weight")  # Add axis labels and legend

# Distribution of women heights in Nepal
ggplot(blue,aes(x=m11)) +
  geom_histogram(binwidth = 0.05, fill = "yellow", color = "black") +
  theme_minimal() +  # Apply a minimal theme for a clean look
  labs(title = "Distribution of women's height in Nepal (m11)", x = "Height (m)", y = "Count")  # Add title and axis labels

# Distribution of women weights in Nepal
ggplot(blue,aes(x=m12)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  theme_minimal() +  # Apply a minimal theme for a clean look
  labs(title = "Distribution of women's weight in Nepal (m12)", x = "Weight (kg)", y = "Count")  # Add title and axis labels

