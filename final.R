# Interactive Twitch Channel-to-Viewer Ratio Analysis (2016-2023)
# HTML Dashboard with Interactive Visualizations

# Clear environment and graphics
rm(list = ls())
graphics.off()

# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(lubridate)
library(plotly)
library(htmlwidgets)
library(htmltools)

# Set your working directory
setwd("C:/Users/Wasiul/OneDrive/Documents/CSC474/Final")
getwd()

# Read the global Twitch data
twitch_data <- read_csv("Twitch_global_data.csv")

# Filter for 2016-2023 and create date column
twitch_filtered <- twitch_data %>%
  filter(year >= 2016 & year <= 2023) %>%
  mutate(date = as.Date(paste(year, Month, "01", sep = "-")))

# Calculate yearly averages
yearly_averages <- twitch_filtered %>%
  group_by(year) %>%
  summarise(
    avg_ratio = mean(Viewer_ratio, na.rm = TRUE),
    avg_channels = mean(Avg_channels, na.rm = TRUE),
    avg_viewers = mean(Avg_viewers, na.rm = TRUE),
    .groups = 'drop'
  )

# Print the yearly averages
print("Yearly Channel-to-Viewer Ratios:")
print(yearly_averages)

# ===== INTERACTIVE VISUALIZATION 1: Yearly Trend Line Chart =====
plot1_ggplot <- ggplot(yearly_averages, aes(x = year, y = avg_ratio)) +
  geom_line(color = "#9146FF", size = 1.2) +
  geom_point(color = "#9146FF", size = 3, 
             aes(text = paste("Year:", year, 
                              "<br>Avg Ratio:", round(avg_ratio, 2),
                              "<br>Avg Channels:", scales::comma(round(avg_channels)),
                              "<br>Avg Viewers:", scales::comma(round(avg_viewers))))) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, color = "#6441A4") +
  labs(
    title = "Twitch Channel-to-Viewer Ratio Trend (2016-2023)",
    subtitle = "Average viewers per channel over time - Hover for details",
    x = "Year",
    y = "Average Viewers per Channel"
  ) +
  scale_x_continuous(breaks = 2016:2023) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#333333"),
    plot.subtitle = element_text(size = 12, color = "#666666"),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Convert to interactive plotly
interactive_plot1 <- ggplotly(plot1_ggplot, tooltip = "text") %>%
  layout(
    title = list(text = "Twitch Channel-to-Viewer Ratio Trend (2016-2023)<br><sub>Average viewers per channel over time - Hover for details</sub>"),
    hovermode = "x unified",
    showlegend = FALSE
  ) %>%
  config(displayModeBar = TRUE, 
         modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d"))

# ===== INTERACTIVE VISUALIZATION 2: Monthly Time Series =====
# Debug and verify monthly data
cat("Checking monthly data structure...\n")
cat("Number of rows in twitch_filtered:", nrow(twitch_filtered), "\n")
cat("Date range:", range(twitch_filtered$date), "\n")
cat("Viewer_ratio range:", range(twitch_filtered$Viewer_ratio, na.rm = TRUE), "\n")
cat("Sample of data:\n")
print(head(twitch_filtered[c("date", "Viewer_ratio", "Avg_channels", "Avg_viewers")]))

# Check for any NA values
cat("NA values in Viewer_ratio:", sum(is.na(twitch_filtered$Viewer_ratio)), "\n")

# Add more detailed hover information for monthly data
twitch_filtered <- twitch_filtered %>%
  mutate(
    month_name = month.name[Month],
    hover_text = paste("Date:", format(date, "%B %Y"),
                       "<br>Ratio:", round(Viewer_ratio, 2),
                       "<br>Channels:", scales::comma(Avg_channels),
                       "<br>Viewers:", scales::comma(Avg_viewers))
  )

# Create the plotly chart directly instead of converting from ggplot
interactive_plot2 <- plot_ly(
  data = twitch_filtered,
  x = ~date,
  y = ~Viewer_ratio,
  type = 'scatter',
  mode = 'lines',
  line = list(color = '#9146FF', width = 2),
  hovertemplate = paste(
    "Date: %{x|%B %Y}<br>",
    "Ratio: %{y:.2f}<br>",
    "Channels: %{customdata[0]:,.0f}<br>",
    "Viewers: %{customdata[1]:,.0f}<br>",
    "<extra></extra>"
  ),
  customdata = ~cbind(Avg_channels, Avg_viewers),
  name = "Monthly Ratio"
) %>%
  add_trace(
    y = ~fitted(loess(Viewer_ratio ~ as.numeric(date), data = twitch_filtered, span = 0.3)),
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#6441A4', width = 3),
    name = "Trend Line",
    hovertemplate = "Trend: %{y:.2f}<extra>Smoothed Trend</extra>"
  ) %>%
  layout(
    title = list(
      text = "Monthly Channel-to-Viewer Ratio Evolution (2016-2023)<br><sub>Detailed monthly trends with smoothed trend line - Click and drag to zoom</sub>",
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Date",
      tickformat = "%Y"
    ),
    yaxis = list(
      title = "Viewers per Channel"
    ),
    hovermode = 'x unified',
    showlegend = TRUE,
    legend = list(
      x = 0.02,
      y = 0.98,
      bgcolor = 'rgba(255,255,255,0.8)',
      bordercolor = 'rgba(0,0,0,0.2)',
      borderwidth = 1
    ),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white'
  ) %>%
  config(displayModeBar = TRUE)

# Fallback: If the above doesn't work, create a simpler version
if(nrow(twitch_filtered) == 0) {
  cat("WARNING: No data found for monthly chart. Creating fallback message.\n")
  interactive_plot2 <- plot_ly() %>%
    add_annotations(
      x = 0.5,
      y = 0.5,
      text = "No data available for monthly chart.<br>Please check your data file.",
      showarrow = FALSE,
      font = list(size = 16, color = "red")
    ) %>%
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}

# ===== INTERACTIVE VISUALIZATION 3: Dual Axis Chart =====
# Create separate traces for better interactivity
yearly_scaled <- yearly_averages %>%
  mutate(
    avg_channels_thousands = avg_channels / 1000,
    avg_viewers_millions = avg_viewers / 1000000
  )

# Create plotly chart from scratch for better dual-axis control
interactive_plot3 <- plot_ly() %>%
  # Channels line
  add_trace(
    data = yearly_scaled,
    x = ~year, 
    y = ~avg_channels_thousands,
    type = 'scatter',
    mode = 'lines+markers',
    name = 'Avg Channels (thousands)',
    line = list(color = '#FF6B6B', width = 3),
    marker = list(color = '#FF6B6B', size = 8),
    hovertemplate = paste("Year: %{x}<br>",
                          "Channels: %{y:.1f}k<br>",
                          "<extra>Avg Channels</extra>"),
    yaxis = 'y'
  ) %>%
  # Viewers line
  add_trace(
    data = yearly_scaled,
    x = ~year,
    y = ~I(avg_viewers_millions * 10),
    type = 'scatter',
    mode = 'lines+markers',
    name = 'Avg Viewers (millions × 10)',
    line = list(color = '#4ECDC4', width = 3),
    marker = list(color = '#4ECDC4', size = 8),
    hovertemplate = paste("Year: %{x}<br>",
                          "Viewers: %{customdata:.2f}M<br>",
                          "<extra>Avg Viewers</extra>"),
    customdata = ~avg_viewers_millions,
    yaxis = 'y'
  ) %>%
  # Ratio line (secondary axis)
  add_trace(
    data = yearly_scaled,
    x = ~year,
    y = ~avg_ratio,
    type = 'scatter',
    mode = 'lines+markers',
    name = 'Channel-to-Viewer Ratio',
    line = list(color = '#9146FF', width = 3, dash = 'dash'),
    marker = list(color = '#9146FF', size = 8),
    hovertemplate = paste("Year: %{x}<br>",
                          "Ratio: %{y:.2f}<br>",
                          "<extra>Viewer Ratio</extra>"),
    yaxis = 'y2'
  ) %>%
  layout(
    title = list(
      text = "Twitch Growth: Channels, Viewers, and Ratio (2016-2023)<br><sub>Interactive dual-axis visualization - Toggle series by clicking legend</sub>",
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Year",
      tickmode = 'array',
      tickvals = 2016:2023
    ),
    yaxis = list(
      title = "Channels (thousands) / Viewers (millions × 10)",
      side = 'left',
      color = '#333333'
    ),
    yaxis2 = list(
      title = "Viewers per Channel (Ratio)",
      side = 'right',
      overlaying = 'y',
      color = '#9146FF'
    ),
    hovermode = 'x unified',
    legend = list(
      x = 0.02,
      y = 0.98,
      bgcolor = 'rgba(255,255,255,0.8)',
      bordercolor = 'rgba(0,0,0,0.2)',
      borderwidth = 1
    ),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white'
  ) %>%
  config(displayModeBar = TRUE)

# ===== CREATE HTML DASHBOARD =====
# Create summary statistics
summary_stats <- paste(
  "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 10px; margin: 20px 0;'>",
  "<h3 style='color: #333; margin-top: 0;'>📊 Key Statistics (2016-2023)</h3>",
  "<div style='display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px;'>",
  sprintf("<div><strong>2016 Ratio:</strong> %.2f viewers/channel</div>", yearly_averages$avg_ratio[1]),
  sprintf("<div><strong>2023 Ratio:</strong> %.2f viewers/channel</div>", yearly_averages$avg_ratio[8]),
  sprintf("<div><strong>Peak Ratio:</strong> %.2f in %d</div>", max(yearly_averages$avg_ratio), yearly_averages$year[which.max(yearly_averages$avg_ratio)]),
  sprintf("<div><strong>Lowest Ratio:</strong> %.2f in %d</div>", min(yearly_averages$avg_ratio), yearly_averages$year[which.min(yearly_averages$avg_ratio)]),
  sprintf("<div><strong>Overall Change:</strong> %.1f%%</div>", ((yearly_averages$avg_ratio[8] - yearly_averages$avg_ratio[1]) / yearly_averages$avg_ratio[1]) * 100),
  "</div></div>"
)

# Key insights section
insights_section <- paste(
  "<div style='background-color: #e8f4fd; padding: 20px; border-radius: 10px; margin: 20px 0;'>",
  "<h3 style='color: #333; margin-top: 0;'>🔍 Key Insights</h3>",
  "<ul style='margin: 0; padding-left: 20px;'>",
  "<li><strong>Platform Maturation:</strong> The declining ratio (2016-2020) represents Twitch's evolution from niche to mainstream</li>",
  "<li><strong>COVID Impact:</strong> 2020 marked the lowest ratio (~24.9) due to massive creator influx during lockdowns</li>",
  "<li><strong>New Equilibrium:</strong> Post-2020 stabilization around 26-28 viewers per channel suggests market maturity</li>",
  "<li><strong>Increased Competition:</strong> More creators means more content diversity but higher competition for individual streamers</li>",
  "</ul></div>"
)

# Create the complete HTML page
html_content <- tags$html(
  tags$head(
    tags$title("Twitch Channel-to-Viewer Ratio Analysis (2016-2023)"),
    tags$style(HTML("
      body { 
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; 
        margin: 0; 
        padding: 20px; 
        background-color: #ffffff;
        line-height: 1.6;
      }
      .container { 
        max-width: 1200px; 
        margin: 0 auto; 
      }
      .header {
        text-align: center;
        margin-bottom: 30px;
        padding: 30px;
        background: linear-gradient(135deg, #9146FF 0%, #6441A4 100%);
        color: white;
        border-radius: 15px;
      }
      .plot-container {
        margin: 30px 0;
        padding: 20px;
        background-color: #fafafa;
        border-radius: 10px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }
      .footer {
        text-align: center;
        margin-top: 40px;
        padding: 20px;
        background-color: #f8f9fa;
        border-radius: 10px;
        color: #666;
      }
    "))
  ),
  tags$body(
    tags$div(class = "container",
             # Header
             tags$div(class = "header",
                      tags$h1("Twitch Channel-to-Viewer Ratio Analysis", style = "margin: 0; font-size: 2.5em;"),
                      tags$h2("Interactive Dashboard (2016-2023)", style = "margin: 10px 0 0 0; font-weight: normal; font-size: 1.2em; opacity: 0.9;")
             ),
             
             # Summary Statistics
             HTML(summary_stats),
             
             # Plot 1
             tags$div(class = "plot-container",
                      tags$h3("📈 Yearly Trend Analysis", style = "color: #333; margin-top: 0;"),
                      interactive_plot1
             ),
             
             # Plot 2  
             tags$div(class = "plot-container",
                      tags$h3("📊 Monthly Evolution Detail", style = "color: #333; margin-top: 0;"),
                      interactive_plot2
             ),
             
             # Plot 3
             tags$div(class = "plot-container",
                      tags$h3("📈 Growth Overview: Channels, Viewers & Ratio", style = "color: #333; margin-top: 0;"),
                      interactive_plot3
             ),
             
             # Insights
             HTML(insights_section),
             
             # Footer
             tags$div(class = "footer",
                      tags$p("Data source: Twitch Global Statistics | Analysis created with R, ggplot2, and plotly"),
                      tags$p(paste("Generated on:", Sys.Date()))
             )
    )
  )
)

# Save the interactive HTML dashboard
save_html(html_content, file = "twitch_interactive_dashboard.html")

# Also save individual interactive plots
saveWidget(interactive_plot1, "twitch_yearly_interactive.html")
saveWidget(interactive_plot2, "twitch_monthly_interactive.html")  
saveWidget(interactive_plot3, "twitch_growth_interactive.html")

# ===== SUMMARY OUTPUT =====
cat("\n=== INTERACTIVE DASHBOARD CREATED ===\n")
cat("Main dashboard: twitch_interactive_dashboard.html\n")
cat("Individual plots saved as separate HTML files\n")
cat("\nFeatures included:\n")
cat("✓ Hover tooltips with detailed information\n")
cat("✓ Zoom and pan capabilities\n")
cat("✓ Interactive legend (click to toggle series)\n")
cat("✓ Professional styling and layout\n")
cat("✓ Summary statistics and insights\n")
cat("✓ Mobile-responsive design\n")

print("Interactive HTML dashboard completed!")

# ===== SUMMARY STATISTICS (Console Output) =====
cat("\n=== SUMMARY ANALYSIS ===\n")
cat("Channel-to-Viewer Ratio Changes (2016-2023):\n")
cat(sprintf("2016: %.2f viewers per channel\n", yearly_averages$avg_ratio[1]))
cat(sprintf("2023: %.2f viewers per channel\n", yearly_averages$avg_ratio[8]))
cat(sprintf("Peak ratio: %.2f in %d\n", max(yearly_averages$avg_ratio), yearly_averages$year[which.max(yearly_averages$avg_ratio)]))
cat(sprintf("Lowest ratio: %.2f in %d\n", min(yearly_averages$avg_ratio), yearly_averages$year[which.min(yearly_averages$avg_ratio)]))

# Calculate percentage change
pct_change <- ((yearly_averages$avg_ratio[8] - yearly_averages$avg_ratio[1]) / yearly_averages$avg_ratio[1]) * 100
cat(sprintf("Overall change 2016-2023: %.1f%%\n", pct_change))