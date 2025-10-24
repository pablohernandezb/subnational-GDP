# Load necessary libraries
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Set the path to your data file
data_file <- "bfi_data_VE.csv"

# Load the data
df <- read.csv(data_file)

# 1. Prepare the data for mapping
# Filter for the target country (Venezuela, ISO: VEN), remove NAs, and apply the scaling
venezuela_data <- df %>%
  filter(iso == "VEN") %>%
  # Remove rows with NA coordinates
  drop_na(longitude, latitude) %>%
  # Create the new scaled column as requested: value * 1,000,000,000
  mutate(cell_GDPC_Scaled = cell_GDPC_current_USD * 1000000000)

# Define the bounding box for Venezuela to limit the background map
# Longitude range: approx -73.5 to -59.5
# Latitude range: approx 0.5 to 12.5
xlims <- c(min(venezuela_data$longitude, na.rm = TRUE) - 1, max(venezuela_data$longitude, na.rm = TRUE) + 1)
ylims <- c(min(venezuela_data$latitude, na.rm = TRUE) - 1, max(venezuela_data$latitude, na.rm = TRUE) + 1)

# 2. Convert the cell data into an 'sf' (simple features) object
# We need to create polygons for the cells, which are 0.25x0.25 degrees,
# but the data only provides the center (longitude, latitude).
# A common way to visualize this data is by plotting the points, or by 
# attempting to reconstruct the grid structure. Since the grid is regular, 
# we can use geom_tile() which is easier than full polygon creation.

# The 'cell_size' column suggests the original grid is 0.25-deg by 0.25-deg.
# We will use geom_tile() to represent the grid cells. The width/height 
# of the tiles will be set to 0.25 degrees.

# 3. Get the country boundary map for context
world <- ne_countries(scale = "medium", returnclass = "sf")
venezuela_border <- world %>% filter(iso_a3 == "VEN")

# 3. Create the plot
venezuela_plot <- ggplot() +
  # Add the country boundary as a background layer
  geom_sf(data = venezuela_border, fill = "grey90", color = "grey30") +
  
  # Add the grid cells using geom_tile, coloring by the new scaled variable
  geom_tile(data = venezuela_data, 
            aes(x = longitude, y = latitude, fill = cell_GDPC_Scaled), # Use the scaled column
            width = 0.25, # Width of the tile (0.25 degree cell size)
            height = 0.25, # Height of the tile (0.25 degree cell size)
            color = NA) + # Remove tile borders
  
  # Use a viridis color scale (without log transformation)
  scale_fill_viridis_c(
    name = "Scaled GDP per Capita\n(Current USD × $10^6$)", 
    option = "magma", # Choose a color palette
    # The 'trans' argument is removed as requested
    labels = scales::label_number(accuracy = 0.1, big.mark = ",") # Format labels with higher precision
  ) +
  
  # Set map limits and aspect ratio
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
  
  # Add labels and title
  labs(
    title = "Venezuela Cell-Level Scaled GDP per Capita (2021)",
    subtitle = "Scaled Current USD (× 1,000,000)",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  # Apply a clean theme
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey80", linetype = "dashed", linewidth = 0.2),
    panel.background = element_rect(fill = "aliceblue"), # Light blue for ocean/background
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Print the plot
print(venezuela_plot)