# Load required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

# Create dataset from csv file
dl <- tempfile()
download.file("https://dl.dropboxusercontent.com/s/isa5k53bp0tsgii/Crime_Data_from_2020_to_Present.csv?dl=1", dl)
la_crime_stats <- as.data.table(read_csv(dl, col_names = TRUE,
                                         col_types = cols('Crm Cd 3' = col_double(),
                                                          'Crm Cd 4' = col_double())))

la_crime_stats <- la_crime_stats %>%
  select(DR_NO, `DATE OCC`, `TIME OCC`, AREA, `AREA NAME`, `Crm Cd`, `Crm Cd Desc`,
         `Vict Age`, `Vict Sex`, `Vict Descent`, `Premis Cd`, `Premis Desc`)

# Separate crimes into more concise categories
violent_crimes <- c(110,113,121,122,210,220,230,231,235,236,250,251,434,435,436,
                    437,622,623,624,625,626,627,647,753,755,756,761,860,910,920,
                    928,930,943)

# Convert DATE OCC to date type and pull month/day fields from it
la_crime_stats <- la_crime_stats %>%
  mutate(`DATE OCC` = as.Date(`DATE OCC`, "%m/%d/%y"),
         month = month(`DATE OCC`),
         day = day(`DATE OCC`),
         dow = weekdays(`DATE OCC`))

dow_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
               "Saturday")
la_crime_stats$dow <- factor(la_crime_stats$dow, level = rev(dow_order))

la_crime_stats <- la_crime_stats %>%
  mutate(`TIME OCC` = substr(as.POSIXct(sprintf("%04.0f", `TIME OCC`),
                                        format="%H%M"), 12, 16))
# Add violent field to dataset
la_crime_stats <- la_crime_stats %>%
  mutate(violent = case_when(
    `Crm Cd` %in% violent_crimes ~ 1,
    TRUE ~ 0
  ))

la_crime_stats$violent <- as.factor(la_crime_stats$violent)

# Generate train and test sets from la_crime_stats dataset
set.seed(1, sample.kind="Rounding")
index <- createDataPartition(la_crime_stats$violent, times = 1, p = 0.2, list = FALSE)
train_set <- la_crime_stats[-index]
test_set <- la_crime_stats[index]

# Frequency of each crime type per day of the month for the entire year
train_set %>%
  group_by(violent, month, day) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = day, y = month, fill = n)) +
  geom_tile() +
  ggtitle("Frequency of Crimes Per Day of Month for 2020, Faceted by Crime Type") +
  scale_fill_gradient(low = "white", high = "blue") + 
  facet_wrap(~ violent)
# Noticeable drop in crimes around June, coinciding with COVID lockdowns

# Frequency of each crime type per hour by day of the week
train_set %>%
  mutate(hour = as.numeric(substr(`TIME OCC`, 1,2))) %>%
  group_by(violent, dow, hour) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = hour, y = dow, fill = n)) +
  geom_tile() +
  ggtitle("Frequency of Crimes Per Hour by Day of Week, Faceted by Crime Type") +
  scale_fill_gradient(low = "white", high = "blue") +
  facet_wrap(~ violent)
# All crimes are significantly less common in the early morning,
# most common in the early evening

# Number of crimes committed in each area
train_set %>%
  filter(violent == 0) %>%
  group_by(DR_NO) %>%
  mutate(n = n()) %>%
  ggplot(aes(x = `AREA NAME`, y = n)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Non-Violent Crime Occurrence by Area") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = scales::comma)
# Top three areas: 77th Street, Southeast, Central
# Bottom three areas: West LA, Devonshire, Northeast

train_set %>%
  filter(violent == 1) %>%
  group_by(DR_NO) %>%
  mutate(n = n()) %>%
  ggplot(aes(x = `AREA NAME`, y = n)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Violent Crime Occurrence by Area") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = scales::comma)
# Top three areas: Pacific, West LA, N Hollywood
# Bottom three areas: Foothill, Hollenbeck, Rampart

# Average age of victims per crime type
train_set %>%
  group_by(DR_NO) %>%
  filter(`Vict Age` %in% c(seq(0:100))) %>%
  ggplot(aes(x = violent, y = `Vict Age`)) +
  geom_jitter() +
  ggtitle("Average Age of Victims by Crime Type") +
  scale_y_continuous(labels = scales::comma)
# Retirement-aged individuals are less likely to be the victims of violent crimes

train_set %>%
  group_by(DR_NO) %>%
  filter(`Vict Sex` %in% c("M","F"), `Vict Age` %in% c(seq(0:100))) %>%
  ggplot(aes(x = violent, y = `Vict Age`)) +
  geom_jitter() +
  ggtitle("Average Age of Victims by Crime, Faceted by Victim Sex") +
  facet_grid(. ~ `Vict Sex`)
# Very little difference between men and women, men are targeted for violent
# crimes a bit longer than women on average, but don't get targeted for
# non-violent crimes until a few years later than women

# Crimes by ethnicity of victims
# Ethnicity Code: A - Other Asian B - Black C - Chinese D - Cambodian F - Filipino
# G - Guamanian H - Hispanic/Latin/Mexican I - American Indian/Alaskan Native J - Japanese
# K - Korean L - Laotian O - Other P - Pacific Islander S - Samoan U - Hawaiian
# V - Vietnamese W - White X - Unknown Z - Asian Indian
train_set %>%
  group_by(`DR_NO`) %>%
  mutate(n = n()) %>%
  ggplot(aes(x = violent, y = n, color = `Vict Descent`)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of Crimes Committed per Crime Type by Victim Ethnicity")
# Black and Hispanic/Latino individuals are equally affected by both violent and
# and non-violent crimes, but other ethnicities are more likely to be targeted
# by non-violent crimes

# Types of locations where crimes occur per area
train_set %>%
  group_by(`DR_NO`) %>%
  mutate(n = n()) %>%
  ggplot(aes(x = `Premis Cd`, y = n, color = `violent`)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))
# A vast majority of crimes occur on streets, in single-family dwellings, and
# in multi-unit dwellings (apartments, duplexes, etc.)

train_set %>%
  group_by(`Premis Desc`) %>%
  summarize(n = n()) %>%
  top_n(5) %>%
  pull(`Premis Desc`)

# Train GLM model
set.seed(1, sample.kind="Rounding")
fit_glm <- train(violent ~ AREA, train_set, method = "glm")
pred_glm <- predict(fit_glm, test_set)
acc <- mean(pred_glm == test_set$violent)