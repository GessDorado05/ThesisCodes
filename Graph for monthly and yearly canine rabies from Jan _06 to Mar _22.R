install.

# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# If your data is in a CSV file, read it like this:
# df <- read.csv("your_data_file.csv")

# Simulate reading your data (assuming it's already loaded in R as a dataframe)
# Create the data frame manually (for demonstration purposes)
# Replace this with your actual data import
data <- read.table(header=TRUE, text='
Month	Positive
Jan-06	3
Feb-06	2
Mar-06	2
Apr-06	2
May-06	2
Jun-06	2
Jul-06	5
Aug-06	2
Sep-06	3
Oct-06	2
Nov-06	4
Dec-06	3
Jan-07	3
Feb-07	2
Mar-07	3
Apr-07	2
May-07	1
Jun-07	1
Jul-07	2
Aug-07	1
Sep-07	1
Oct-07	3
Nov-07	6
Dec-07	0
Jan-08	5
Feb-08	2
Mar-08	2
Apr-08	5
May-08	3
Jun-08	0
Jul-08	4
Aug-08	3
Sep-08	0
Oct-08	0
Nov-08	0
Dec-08	3
Jan-09	0
Feb-09	2
Mar-09	5
Apr-09	0
May-09	0
Jun-09	1
Jul-09	0
Aug-09	2
Sep-09	2
Oct-09	3
Nov-09	0
Dec-09	0
Jan-10	3
Feb-10	0
Mar-10	0
Apr-10	1
May-10	0
Jun-10	0
Jul-10	0
Aug-10	3
Sep-10	0
Oct-10	1
Nov-10	1
Dec-10	0
Jan-11	4
Feb-11	2
Mar-11	0
Apr-11	0
May-11	0
Jun-11	2
Jul-11	2
Aug-11	1
Sep-11	1
Oct-11	1
Nov-11	1
Dec-11	2
Jan-12	1
Feb-12	0
Mar-12	5
Apr-12	0
May-12	0
Jun-12	0
Jul-12	0
Aug-12	0
Sep-12	3
Oct-12	3
Nov-12	1
Dec-12	0
Jan-13	1
Feb-13	1
Mar-13	2
Apr-13	2
May-13	1
Jun-13	2
Jul-13	0
Aug-13	2
Sep-13	0
Oct-13	1
Nov-13	0
Dec-13	2
Jan-14	1
Feb-14	0
Mar-14	1
Apr-14	0
May-14	2
Jun-14	0
Jul-14	1
Aug-14	1
Sep-14	2
Oct-14	2
Nov-14	1
Dec-14	0
Jan-15	1
Feb-15	1
Mar-15	0
Apr-15	1
May-15	1
Jun-15	1
Jul-15	1
Aug-15	2
Sep-15	1
Oct-15	0
Nov-15	0
Dec-15	3
Jan-16	1
Feb-16	1
Mar-16	3
Apr-16	1
May-16	3
Jun-16	1
Jul-16	1
Aug-16	2
Sep-16	0
Oct-16	2
Nov-16	0
Dec-16	0
Jan-17	1
Feb-17	1
Mar-17	1
Apr-17	1
May-17	5
Jun-17	0
Jul-17	0
Aug-17	3
Sep-17	3
Oct-17	4
Nov-17	2
Dec-17	1
Jan-18	3
Feb-18	1
Mar-18	3
Apr-18	8
May-18	8
Jun-18	13
Jul-18	8
Aug-18	7
Sep-18	7
Oct-18	7
Nov-18	6
Dec-18	3
Jan-19	2
Feb-19	2
Mar-19	0
Apr-19	2
May-19	2
Jun-19	1
Jul-19	6
Aug-19	3
Sep-19	6
Oct-19	5
Nov-19	5
Dec-19	4
Jan-20	4
Feb-20	4
Mar-20	6
Apr-20	0
May-20	1
Jun-20	2
Jul-20	3
Aug-20	5
Sep-20	3
Oct-20	4
Nov-20	0
Dec-20	3
Jan-21	3
Feb-21	5
Mar-21	0
Apr-21	4
May-21	3
Jun-21	1
Jul-21	0
Aug-21	0
Sep-21	2
Oct-21	3
Nov-21	1
Dec-21	1
Jan-22	5
Feb-22	6
Mar-22	2
') # Add the rest of the dataset similarly

# Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Convert Month to proper Date format (e.g., Jan-06 -> 01-Jan-2006)
data$Date <- dmy(paste0("01-", data$Month))
data$Year <- year(data$Date)

# Compute yearly totals
yearly_totals <- data %>%
  group_by(Year) %>%
  summarize(Total = sum(Positive), .groups = 'drop') %>%
  mutate(Date = as.Date(paste0(Year, "-01-01")))  # Align yearly totals to Jan 1

# Add label for plotting
data$Type <- "Monthly Reported Cases"
yearly_totals$Type <- "Yearly Reported Cases"

# Combine both datasets for unified plotting
combined_data <- bind_rows(
  data %>% select(Date, Value = Positive, Type),
  yearly_totals %>% select(Date, Value = Total, Type)
)

# Plot
ggplot(combined_data, aes(x = Date, y = Value, color = Type, linetype = Type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Monthly Reported Cases" = "black", "Yearly Reported Cases" = "red")) +
  scale_linetype_manual(values = c("Monthly Reported Cases" = "solid", "Yearly Reported Cases" = "solid")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 5)) + 
  labs(
    x = "Year",
    y = "Positive Rabies Cases",
    title = "Rabies Cases in Davao City (Jan 2006–Mar 2022)",
    color = "Legend",
    linetype = "Legend"
  ) +
  theme_minimal(base_size = 17) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.line = element_line(color = "black"),  # Axis visibility
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10)),
    legend.position = "top"
  )

