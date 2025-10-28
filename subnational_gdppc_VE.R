#### 1. Loading libraries ####

libs <- c(
  "tidyverse", "sf", "geodata",
  "terra", "classInt", "rayshader",
  "exactextractr"
)

installed_libs <- libs %in% rownames(
  installed.packages()
)

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

invisible(lapply(
  libs,
  library,
  character.only = T
))

#### 2. Getting county boundaries ####

get_country_borders <- function() {
  main_path <- getwd()
  borders <- geodata::gadm(
    country = "VEN",
    level = 0,
    path = main_path
  ) |>
    sf::st_as_sf()
  
  return(borders)
  
}

get_state_borders <- function() {
  main_path <- getwd()
  borders <- geodata::gadm(
    country = "VEN",
    level = 1,
    path = main_path
  ) |>
    sf::st_as_sf()
  
  return(borders)
}

get_municipality_borders <- function() {
  main_path <- getwd()
  borders <- geodata::gadm(
    country = "VEN",
    level = 2,
    path = main_path
  ) |>
    sf::st_as_sf()
  
  return(borders)
}

country_sf <- get_country_borders()
state_sf <- get_state_borders()
municipality_sf <- get_municipality_borders()

esequibo_sf <- st_read("Estados_Venezuela.shp") |>
  sf::st_as_sf() |>
  filter(ID == 24)

target_crs <- st_crs(country_sf)

# 2. Load Guyana's grid data
bfi_geom_guyana <- sf::st_read(shp_file) |>
  filter(iso == "GUY")
bfi_data_guyana <- read_csv(csv_file) |>
  filter(iso == "GUY" & year == 2021)

# Make sure 'cell_id' exists and types match for Guyana
bfi_geom_guyana$cell_id <- as.numeric(bfi_geom_guyana$cell_id) 
bfi_data_guyana$cell_id <- as.numeric(bfi_data_guyana$cell_id)

# 3. Merge Guyana grid data
bfi_esequibo_merged <- bfi_geom_guyana |>
  left_join(bfi_data_guyana, by = join_by_cols_final) |>
  filter(!is.na(cell_GDPC_current_USD)) |>
  st_transform(crs = target_crs) # Transform CRS

# 4. Create the scaled variable for Guyana too
bfi_esequibo_merged$scaled_gdp <- bfi_esequibo_merged$cell_GDPC_current_USD * 1e9

# 5. Spatially clip Guyana's data to the Esequibo boundary
# This keeps only the grid cells (or parts of them) that fall within the Esequibo claim.
bfi_esequibo_sf <- st_intersection(bfi_esequibo_merged, esequibo_sf)

# 6. Combine the two datasets *before* calculating breaks
# Only select the geometry and the variable of interest for consistency
gdp_all_sf <- bind_rows(
  bfi_merged_sf |> select(geometry, scaled_gdp),
  bfi_esequibo_sf |> select(geometry, scaled_gdp)
)

#### 3. Load BFI dataset ####

# Paths to your files
csv_file <- "0_25deg/final_GDP_0_25deg_postadjust_pop_dens_0_adjust.csv"
shp_file <- "0_25deg/shapefile/geom_0_25deg.shp"

# Load shapefile and CSV
bfi_geom <- sf::st_read(shp_file)
bfi_data <- read_csv(csv_file)

bfi_geom <- bfi_geom |>
  filter(iso == "VEN")

bfi_data <- bfi_data |>
  filter(iso == "VEN" & year == 2021)

# Make sure 'cell_id' exists and types match
bfi_geom$cell_id <- as.numeric(bfi_geom$cell_id)  # or as.character() below, depending which works
bfi_data$cell_id <- as.numeric(bfi_data$cell_id)

# bfi_data          bfi_geom
# cell_id           cell_id
# subcell_id        sbcell_d
# subcell_id_0_25   s__0_25

#### 4. Merge datasets ####

# The join is: bfi_geom |> left_join(bfi_data, by = join_by_cols)
# Format: "column_in_bfi_geom" = "column_in_bfi_data"
join_by_cols_final <- c(
  "cell_id" = "cell_id",             
  "sbcll_d" = "subcell_id",           # Corrected column name from the diagnostic check
  "s__0_25" = "subcell_id_0_25"      
)

# Perform the left join
bfi_merged_sf <- bfi_geom |>
  left_join(bfi_data, by = join_by_cols_final) |>
  # Filter based on the variable we are about to use (cell_GDPC_current_USD)
  filter(!is.na(cell_GDPC_current_USD))

#### 5. Data Processing for Plotting ####

bfi_merged_sf$scaled_gdp <- bfi_merged_sf$cell_GDPC_current_USD * 1e9 

# source_var is "scaled_gdp"
source_var <- "scaled_gdp" 
n_classes <- 10
epsilon <- 1e-9 

# Use the combined data for break calculation
gdp_values <- gdp_all_sf[[source_var]]

# 1. Calculate breaks using QUANTILE with jitter (to handle tied values)
set.seed(42)
gdp_values_jittered <- gdp_values + runif(length(gdp_values), 0, epsilon)

breaks_gdp <- classInt::classIntervals(
  var = gdp_values_jittered,
  n = n_classes,
  style = "quantile"
)$brks

breaks_gdp[1] <- min(gdp_values) 
breaks_gdp[n_classes + 1] <- max(gdp_values) 
n_unique_classes <- n_classes

# 2. Format the labels using the breaks
labels_gdp <- sapply(
  1:n_unique_classes,
  function(i) {
    # No 'digits = 0' here
    start <- floor(breaks_gdp[i])
    end_raw <- floor(breaks_gdp[i + 1])
    
    if (i == n_unique_classes) {
      end <- floor(end_raw)
      # For the last interval, use [a, b]
      return(paste0(start, " – ", end))
    } else {
      # For all other intervals, use [a, b) (subtract epsilon for display)
      end <- floor(end_raw - epsilon)
      return(paste0(start, " – ", end))
    }
  }
)

# 3. Apply the classification to BOTH datasets

# Apply to Venezuela data
bfi_merged_sf <- bfi_merged_sf |>
  mutate(
    gdp_decile = cut(!!sym(source_var), breaks = breaks_gdp, labels = labels_gdp, include.lowest = T, right = TRUE)
  )

# Apply to Esequibo data
bfi_esequibo_sf <- bfi_esequibo_sf |>
  mutate(
    gdp_decile = cut(!!sym(source_var), breaks = breaks_gdp, labels = labels_gdp, include.lowest = T, right = TRUE)
  )

# Reverse the order of the factor levels/labels for the desired legend display (same as before)
bfi_merged_sf$gdp_decile <- factor(bfi_merged_sf$gdp_decile, levels = rev(labels_gdp))
bfi_esequibo_sf$gdp_decile <- factor(bfi_esequibo_sf$gdp_decile, levels = rev(labels_gdp))

# 4. Define the color palette (order remains lightest to darkest for internal use)
gdp_colors <- c(
  "#FFFF66", "#FFD700", "#FFA500", "#FF4500", "#CD0000", # Lighter colors
  "#8B008B", "#483D8B", "#191970", "#000080", "#000033"  # Darker colors
)

# Ensure 10 colors are used if 10 classes were calculated
if (length(gdp_colors) != n_unique_classes) {
  gdp_colors <- colorRampPalette(gdp_colors)(n_unique_classes)
}

labels_gdp_raw <- sapply(
  1:n_unique_classes,
  function(i) {
    
    start_value <- floor(breaks_gdp[i])
    start <- format(start_value, big.mark = ",", nsmall = 0)
    
    end_raw <- breaks_gdp[i + 1]
    
    if (i == n_unique_classes) {
      end_value <- floor(end_raw)
      end <- format(end_value, big.mark = ",", nsmall = 0)
      return(paste0(start, " – ", end))
    } else {
      # For all other intervals, subtract epsilon and then floor for display
      end_value <- floor(end_raw - epsilon)
      end <- format(end_value, big.mark = ",", nsmall = 0)
      return(paste0(start, " – ", end))
    }
  }
)

# New number of classes will be 9 (n_unique_classes - 1)
new_n_classes <- n_unique_classes - 1

# The new set of labels starts from the second element of the raw labels
labels_gdp <- labels_gdp_raw[2:(new_n_classes + 1)] 

# Since the categories are merged, the number of colors must be reduced to 9.
gdp_colors_merged <- tail(gdp_colors, new_n_classes) 

# 3. Apply the classification to BOTH datasets with the adjusted number of classes
# NOTE: The breaks_gdp array still has 11 elements (10 intervals), but 
# the first interval is implicitly merged with the second by dropping its label.
# Since the first two breaks were both close to 0, using the breaks as-is 
# and just adjusting the labels is the simplest solution.

# Apply to Venezuela data
bfi_merged_sf <- bfi_merged_sf |>
  mutate(
    # Use the breaks as originally calculated (which had 10 intervals)
    gdp_decile = cut(!!sym(source_var), breaks = breaks_gdp, labels = labels_gdp_raw, include.lowest = T, right = TRUE)
  )

# Apply to Esequibo data
bfi_esequibo_sf <- bfi_esequibo_sf |>
  mutate(
    gdp_decile = cut(!!sym(source_var), breaks = breaks_gdp, labels = labels_gdp_raw, include.lowest = T, right = TRUE)
  )

# --------------------------------------------------------------------------
# FIX: Relabel and reverse the factor levels based on the new, merged labels.
# --------------------------------------------------------------------------

# Create a relabeling vector: replace the first two levels with the new merged label
# This requires using the breaks_gdp_raw labels as factors first:
levels_to_merge <- labels_gdp_raw[1:2]
new_merged_label <- labels_gdp_raw[2] # "0 – 1459"

# Helper function to correct the factor levels
correct_factors <- function(df, old_labels, new_labels, new_merged_label) {
  df <- df |>
    mutate(
      gdp_decile = as.character(gdp_decile), # Convert to character to relabel
      gdp_decile = ifelse(gdp_decile %in% old_labels, new_merged_label, gdp_decile),
      gdp_decile = factor(gdp_decile, levels = rev(c(new_merged_label, new_labels[3:10]))) # Define new levels and reverse
    )
  return(df)
}

bfi_merged_sf <- correct_factors(bfi_merged_sf, levels_to_merge, labels_gdp_raw, new_merged_label)
bfi_esequibo_sf <- correct_factors(bfi_esequibo_sf, levels_to_merge, labels_gdp_raw, new_merged_label)

# Update the gdp_colors to use the 9-color palette (reversing the palette will be in Section 6)
gdp_colors <- gdp_colors[2:10]

#### 6. Plot the map ####

# Plot title and subtitle
plot_title <- "PIB per cápita de Venezuela (2021) cada 0.25°x 0.25°"
plot_subtitle <- "Precios corrientes (USD)"
plot_caption <- "©2025 Pablo Hernández Borges | pablohernandezb.dev\nDatos: Rossi-Hansberg & Zhang (2025)"

plot_title_EN <- "Venezuela GDP per Capita (2021) by 0.25°x 0.25° cells"
plot_subtitle_EN <- "Current Prices (USD)"
plot_caption_EN <- "©2025 Pablo Hernández Borges | pablohernandezb.dev\nData: Rossi-Hansberg & Zhang (2025)"

# Define the border color for the squares
grid_border_color <- "grey70" 

# Create the map
venezuela_map <- ggplot() +
  
  # 1. Add the main grid data layer (Venezuela)
  geom_sf(
    data = bfi_merged_sf,
    aes(fill = gdp_decile),
    # FIX: Change color from NA to a light gray
    color = grid_border_color, 
    size = 0.075 # This controls the thickness of the border line
  ) +
  
  # 2. Add the Esequibo grid data layer (Guyana data for Esequibo)
  # geom_sf(
  #   data = bfi_esequibo_sf,
  #   aes(fill = gdp_decile),
  #   # FIX: Change color from NA to a light gray
  #   color = grid_border_color, 
  #   size = 0.075
  # ) +
  
  # 3. Add state borders (Venezuela only)
  geom_sf(
    data = state_sf,
    fill = NA,
    color = "grey30",
    linewidth = 0.5
  ) +
  
  # 4. Add the Esequibo claimed border (distinct visual style)
  # geom_sf(
  #   data = esequibo_sf,
  #   fill = NA, 
  #   color = "red", 
  #   linetype = "dashed", 
  #   linewidth = 1
  # ) +
  
  # 5. Add country border for the outline
  geom_sf(
    data = country_sf,
    fill = NA,
    color = "black",
    linewidth = 1
  ) +
  
  # 6. Apply the custom color palette
  scale_fill_manual(
    values = rev(gdp_colors), 
    name = "GDP per capita (USD)",
    drop = FALSE
  ) +
  
  # 7. Add labs and theme
  labs(
    title = plot_title_EN,
    subtitle = plot_subtitle_EN,
    caption = plot_caption_EN
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# Display the map
print(venezuela_map)

# Save the map (optional)
ggsave("venezuela_gdp_map_2.png", plot = venezuela_map, width = 8, height = 10, dpi = 300)


#### State and Municipal Levels ####

# Ensure the combined grid data has a unique ID for each cell
bfi_all_grid_sf <- bind_rows(
  bfi_merged_sf |> mutate(grid_id = 1:n()), 
  bfi_esequibo_sf |> mutate(grid_id = (n() + 1):(n() + nrow(bfi_esequibo_sf)))
) |>
  select(grid_id, scaled_gdp, geometry)

# Create an empty raster from one of the grid cells
# Assuming 0.25 degree resolution, you might need to adjust based on exact data.
# Get resolution from your shapefile's bbox or a sample cell
resolution_x <- diff(st_bbox(bfi_all_grid_sf)$xlim) / (max(bfi_all_grid_sf$grid_id) / 100) # Rough estimate
resolution_y <- diff(st_bbox(bfi_all_grid_sf)$ylim) / (max(bfi_all_grid_sf$grid_id) / 100) # Rough estimate

# A better way: create a raster template from the geometry directly
# Assuming your grid cells are regular squares, you can get the extent
# and resolution directly from one cell's bounding box.
first_cell_bbox <- st_bbox(bfi_all_grid_sf[1,])
cell_res_x <- first_cell_bbox$xmax - first_cell_bbox$xmin
cell_res_y <- first_cell_bbox$ymax - first_cell_bbox$ymin

# Create a raster from the `scaled_gdp` values
# First, create an empty raster that covers the extent of all your grid data
r_template <- terra::rast(
  ext(bfi_all_grid_sf), 
  resolution = c(cell_res_x, cell_res_y), 
  crs = st_crs(bfi_all_grid_sf)$wkt
)

# Now, rasterize the sf object to get the scaled_gdp values into the raster
# Ensure your sf object has a column named 'scaled_gdp'
gdp_raster <- terra::rasterize(
  x = bfi_all_grid_sf, 
  y = r_template, 
  field = "scaled_gdp", 
  fun = "mean" # If multiple polygons fall into one cell, take the mean
)

# --------------------------------------------------------------------------
# NEW: Perform spatial aggregation for states and municipalities
# --------------------------------------------------------------------------

# Ensure the combined grid data has a unique ID for each cell
# This assumes bfi_merged_sf and bfi_esequibo_sf are already prepared from previous steps
bfi_all_grid_sf <- bind_rows(
  bfi_merged_sf |> mutate(grid_id = 1:n()), 
  bfi_esequibo_sf |> mutate(grid_id = (n() + 1):(n() + nrow(bfi_esequibo_sf)))
) |>
  select(grid_id, scaled_gdp, geometry)

# Check 1: Ensure the combined data is still valid geometry
if (any(!st_is_valid(bfi_all_grid_sf)) | any(st_is_empty(bfi_all_grid_sf))) {
  bfi_all_grid_sf <- st_make_valid(bfi_all_grid_sf)
}

# Check 2: Get resolution and extent for the raster template
# Assuming your grid cells are regular squares, get resolution from one cell's bounding box.
first_cell_bbox <- st_bbox(bfi_all_grid_sf[1,])
cell_res_x <- first_cell_bbox$xmax - first_cell_bbox$xmin
cell_res_y <- first_cell_bbox$ymax - first_cell_bbox$ymin

# Create an empty raster that covers the extent of all your grid data
r_template <- terra::rast(
  ext(bfi_all_grid_sf), 
  resolution = c(cell_res_x, cell_res_y), 
  crs = st_crs(bfi_all_grid_sf)$wkt
)

# Check 3 (CRITICAL): Rasterize the sf object to get the scaled_gdp values into the raster
# Ensure the geometry column is named 'geom' if the rasterize fails
gdp_raster <- terra::rasterize(
  x = bfi_all_grid_sf, 
  y = r_template, 
  field = "scaled_gdp", 
  fun = "mean"
)

# Check 4: Verify the raster was created
if (inherits(gdp_raster, "SpatRaster")) {
  message("✅ gdp_raster created successfully.")
} else {
  stop("❌ ERROR: Failed to create gdp_raster object.")
}

# a) Aggregate Esequibo mean GDP (using the simple Esequibo boundary)
esequibo_mean_value <- exact_extract(
  x = gdp_raster,
  y = esequibo_sf,
  fun = "mean"
)

# FIX: Create a single-row data frame (tibble) for the result.
esequibo_gdp_avg <- tibble(aggregated_gdp = esequibo_mean_value)

# a) Aggregate Esequibo mean GDP (Result is a tibble: esequibo_gdp_avg)
# (Assuming esequibo_gdp_avg is calculated as: tibble(aggregated_gdp = esequibo_mean_value))

# b) Aggregate Venezuelan states (excluding the Esequibo entry)
venezuela_states <- state_sf |> filter(NAME_1 != "Esequibo") # Filter out the placeholder if it exists

state_gdp_avg <- exact_extract(
  x = gdp_raster,
  y = venezuela_states,
  fun = "mean",
  append_cols = c("NAME_1", "GID_1")
) |>
  rename(aggregated_gdp = mean)

# c) Join Venezuelan results to the base SF object
state_sf_venezuela_agg <- venezuela_states |>
  left_join(state_gdp_avg, by = c("NAME_1", "GID_1")) |>
  select(starts_with("NAME_"), starts_with("GID_"), aggregated_gdp, geometry) # Use 'geometry'

# d) Create the Esequibo state row
esequibo_state_row <- esequibo_sf |> 
  # Assign the calculated GDP mean and necessary ID/Name columns
  mutate(
    aggregated_gdp = esequibo_gdp_avg$aggregated_gdp[1],
    NAME_1 = "Esequibo",
    GID_1 = "VEN.99_1" # Use a dummy GID
  ) |>
  # Ensure the column structure matches state_sf_venezuela_agg
  select(starts_with("NAME_"), starts_with("GID_"), aggregated_gdp, geometry) 

# e) Combine all final state data
state_sf_aggregated <- bind_rows(
  state_sf_venezuela_agg, 
  esequibo_state_row
) |>
  filter(!is.na(aggregated_gdp))

# a) Aggregate Venezuelan municipalities (excluding the Esequibo entry)
venezuela_municipalities <- municipality_sf |> filter(NAME_1 != "Esequibo") 

municipality_gdp_avg <- exact_extract(
  x = gdp_raster,
  y = venezuela_municipalities,
  fun = "mean",
  append_cols = c("NAME_2", "GID_2", "NAME_1")
) |>
  rename(aggregated_gdp = mean)

# b) Join Venezuelan results to the base SF object
municipality_sf_venezuela_agg <- venezuela_municipalities |>
  left_join(municipality_gdp_avg, by = c("NAME_2", "GID_2", "NAME_1")) |>
  select(starts_with("NAME_"), starts_with("GID_"), aggregated_gdp, geometry) # Use 'geometry'


# c) Create the Esequibo municipality row
# The Esequibo is treated as one municipality, so we use the single Esequibo boundary (esequibo_sf)
esequibo_municipality_row <- esequibo_sf |> 
  # Assign the calculated GDP mean and necessary ID/Name columns
  mutate(
    aggregated_gdp = esequibo_gdp_avg$aggregated_gdp[1],
    NAME_1 = "Esequibo",
    NAME_2 = "Guayana Esequiba",
    GID_2 = "VEN.99.1_1" # Use a dummy GID
  ) |>
  # Ensure the column structure matches municipality_sf_venezuela_agg
  select(starts_with("NAME_"), starts_with("GID_"), aggregated_gdp, geometry) 

# d) Combine all final municipality data
municipality_sf_aggregated <- bind_rows(
  municipality_sf_venezuela_agg, 
  esequibo_municipality_row
) |>
  filter(!is.na(aggregated_gdp))

#### 6. Plot the maps (States and Municipalities - Continuous Scale) ####

# --- Common Aesthetics ---
grid_border_color <- "white" 
color_name <- "PIB per cápita (USD)"

# --- Plot 1: State Level Map ---
plot_title_state <- "PIB per cápita de Venezuela (2021) ponderado por estado"
plot_subtitle_state <- "Precios corrientes (USD)"
plot_caption_state <- "©2025 Pablo Hernández Borges | pablohernandezb.dev\nDatos: Rossi-Hansberg & Zhang (2025)"

plot_title_state_EN <- "Venezuela GDP per capita (2021) by state"
plot_subtitle_state_EN <- "Current Prices (USD)"
plot_caption_state_EN <- "©2025 Pablo Hernández Borges | pablohernandezb.dev\nData: Rossi-Hansberg & Zhang (2025)"

venezuela_state_map <- ggplot() +
  geom_sf(
    data = state_sf_aggregated,
    # FIX: Map the continuous variable 'aggregated_gdp' to fill
    aes(fill = aggregated_gdp),
    color = grid_border_color, 
    linewidth = 0.5
  ) +
  scale_fill_viridis_c(
    option = "A", # Option "A" is Magma. Option "D" is Viridis.
    direction = -1, # Use 1 to map low values to light colors, high values to dark colors (as is standard for GDP/income)
    name = color_name
    #labels = scales::label_number(big.mark = ",", decimal.mark = ".", scale = 1e-9, suffix = "B")
  ) +
  labs(
    title = plot_title_state_EN,
    subtitle = plot_subtitle_state_EN,
    caption = plot_caption_state_EN
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.key.width = unit(1.5, "cm"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(venezuela_state_map)

# --- Plot 2: Municipality Level Map ---
plot_title_municipality <- "PIB per cápita de Venezuela (2021) ponderado por municipio"
plot_subtitle_municipality <- "Precios corrientes (USD)"
plot_caption_municipality <- "©2025 Pablo Hernández Borges | pablohernandezb.dev\nDatos: Rossi-Hansberg & Zhang (2025)"

plot_title_municipality_EN <- "Venezuela GDP per capita (2021) by municipality"
plot_subtitle_municipality_EN <- "Current Prices (USD)"
plot_caption_municipality_EN <- "©2025 Pablo Hernández Borges | pablohernandezb.dev\nData: Rossi-Hansberg & Zhang (2025)"

venezuela_municipality_map <- ggplot() +
  geom_sf(
    data = municipality_sf_aggregated,
    # FIX: Map the continuous variable 'aggregated_gdp' to fill
    aes(fill = aggregated_gdp),
    color = grid_border_color, 
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    option = "A", # Option "A" is Magma. Option "D" is Viridis.
    direction = -1, # Use 1 to map low values to light colors, high values to dark colors (as is standard for GDP/income)
    name = color_name
    #labels = scales::label_number(big.mark = ",", decimal.mark = ".", scale = 1e-9, suffix = "B")
  ) +
  labs(
    title = plot_title_municipality_EN,
    subtitle = plot_subtitle_municipality_EN,
    caption = plot_caption_municipality_EN
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.key.width = unit(1.5, "cm"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(venezuela_municipality_map)
# ggsave("venezuela_municipality_gdp_continuous_map.png", plot = venezuela_municipality_map, width = 8, height = 10, dpi = 300)

