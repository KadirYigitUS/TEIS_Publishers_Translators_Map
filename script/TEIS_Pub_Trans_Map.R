# Load necessary libraries
library(leaflet)
library(readr)
library(dplyr)
library(leafem)  # For addMouseCoordinates
library(leaflet.extras)
library(sf)  # For working with shapefiles
library(stringr)

# Load CSV data
data_pub <- read_csv("data/csv/publisher_info_card_df.csv")
data_trans <- read_csv("data/csv/translator_info_card_df.csv")

# Split translators into those with and without copyrights
data_trans_telifli <- data_trans %>% filter(Copyright == TRUE)
data_trans_telifsiz <- data_trans %>% filter(Copyright == FALSE)

# Create a function to check if any translator from a list is in the Trans_Worked
check_translators <- function(trans_worked, translators) {
  # Split by comma, trim whitespace and convert to lowercase for matching
  worked_list <- str_split(trans_worked, ",")[[1]] %>% 
    trimws() %>%
    tolower()
  # Check if any translator exists in the list
  any(tolower(translators) %in% worked_list)
}

# Create data_pub_telifli by filtering publishers that worked with telifli translators
data_pub_telifli <- data_pub %>%
  filter(sapply(Trans_Worked, check_translators, translators = data_trans_telifli$Trans))

# Create data_pub_telifsiz by filtering publishers that worked with telifsiz translators
data_pub_telifsiz <- data_pub %>%
  filter(sapply(Trans_Worked, check_translators, translators = data_trans_telifsiz$Trans))

Telifli_Pub_icon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/KadirYigitUS/TEIS_Publishers_Translators_Map/refs/heads/main/images/printing-press-svgrepo-com.svg",
  iconWidth = 25, iconHeight = 25
)

Telifsiz_Pub_icon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/KadirYigitUS/TEIS_Publishers_Translators_Map/refs/heads/main/images/printing-press_2-svgrepo-com.svg",
  iconWidth = 25, iconHeight = 25
)

Telifli_Trans_icon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/KadirYigitUS/TEIS_Publishers_Translators_Map/refs/heads/main/images/translator-language-svgrepo-com.svg",
  iconWidth = 25, iconHeight = 25
)

Telifsiz_Trans_icon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/KadirYigitUS/TEIS_Publishers_Translators_Map/refs/heads/main/images/translator-language_2-svgrepo-com.svg",
  iconWidth = 25, iconHeight = 25
)



# Offset translators' longitude slightly to avoid perfect overlap with publishers
data_trans$Trans_loc_lng <- data_trans$Trans_loc_lng + 0.01 # Offset by 0.01 degrees

# Generate a color palette for the relations based on unique translators
unique_translators <- unique(data_trans$Trans)
colors <- colorFactor(topo.colors(length(unique_translators)), unique_translators)

# Function to draw relations from translators to publishers
# will take in data_trans_telifli (trans_data@Copyright == TRUE)
# or data_trans_telifli (trans_data@Copyright == FALSE)
# relation_type is either "Telifli Eserler İlişkileri" for data_trans_telifli 
# or "Telifsiz Eserler İlişkileri" for data_trans_telifsiz
draw_relations_trans_to_pub <- function(map, trans_data, pub_data, color_palette, relation_type) {
  # Clear any existing polylines in the same group before adding new ones
  map <- clearGroup(map, relation_type)
  
  # Loop through each translator and create lines
  for (i in 1:nrow(trans_data)) {
    # Get the translator's information
    trans_lng <- trans_data$Trans_loc_lng[i]
    trans_lat <- trans_data$Trans_loc_lat[i]
    trans_name <- trans_data$Trans[i]
    pub_worked <- trans_data$Pub_Worked[i]
    # Split and clean the publisher names
    if (!is.na(pub_worked) && pub_worked != "") {
      pub_list <- strsplit(pub_worked, ",")[[1]] # Split by comma
      pub_list <- trimws(pub_list)  # Strip leading/trailing whitespace
      pub_list <- pub_list[pub_list != ""]  # Remove empty values if any
      # Loop through each publisher for the current translator
      for (pub_name in pub_list) {
        pub_matches <- pub_data %>% filter(Pub == pub_name)
        # Find all matching publishers in data_pub
        for (j in 1:nrow(pub_matches)) {
          pub_lng <- pub_matches$Pub_loc_lng[j]
          pub_lat <- pub_matches$Pub_loc_lat[j]
          # Add the line from translator to publisher with unique color based on translator
          map <- addPolylines(
            map,
            lng = c(trans_lng, pub_lng),
            lat = c(trans_lat, pub_lat),
            color = color_palette(trans_name), # Use the color palette based on translator
            weight = 2,
            group = relation_type, # Group the relations
            popup = paste("<b>Yazar-Çevirmen:</b>", trans_name, "<br>",
                          "<b>Yayınevi/Basımevi:</b>", pub_name)
          )
        }
      }
    }
  }
  return(map)
}

# Load the shapefile
places <- st_read("data/shp/countries/ne_110m_populated_places.shp")

# Create the leaflet map
map <- leaflet() %>%
  
  addProviderTiles(providers$CartoDB.Positron) %>% # Use CartoDB Positron basemap
  # Add a blank tile layer to serve as "Boş Katman"
  addTiles(group = "Boş Katman") %>%  # This is the custom empty layer
  # Add a Reset View button
  addResetMapButton() %>%
  # Top-right legend with title
  addLegend(position = "topright",
            colors = character(0),  # Pass empty list for colors
            labels = character(0),  # Pass empty list for labels
            title = htmltools::HTML(
              "<div style='font-size:12px; text-align:center; line-height:1; margin:0;'>",
              "<b>Türk Edebiyatı İsimler Sözlüğü</b> (https://teis.yesevi.edu.tr) Verilerine Dayalı<br/>
              Yayınevleri/Basımevleri ve Yazar-Çevirmenler</div>"
            ),
            opacity = 0  # Set opacity to 0 to hide color boxes
  ) %>%
  # Bottom-left legend with credits
  addLegend(position = "bottomleft",
            colors = character(0),  # Pass empty list for colors
            labels = character(0),  # Pass empty list for labels
            title = htmltools::HTML(
              "<div style='font-size:10px; text-align:left; line-height:1; margin:0;'>",
              "Hazırlayan: Kadir Yiğit US - kyigitus@gmail.com<br/>
              TEIS'in izniyle hazırlanmıştır<br/>
              Kod Kaynağı: Pizza Map (Learn R to make maps)<br/>
              Giovanni Pietro Vitali - giovannipietrovitali@gmail.com</div>"
            ),
            opacity = 0  # Set opacity to 0 to hide color boxes
  ) %>%
  
  # Add publishers that has copyrighted material (data_pub_telifli) with markers with custom HTML popup content
  addMarkers(
    data = data_pub_telifli,
    lng = ~Pub_loc_lng,
    lat = ~Pub_loc_lat,
    label = ~Pub,
    icon = Telifli_Pub_icon,
    popup = paste(sep = "<br/>",
                  paste0("<div class='leaflet-popup-scrolled' style='max-width:200px;max-height:200px'>"),
                  paste0("Yayınevi/Basımevi Adı: ", "<b>",  data_pub_telifli$Pub, "</b>"),
                  paste0("Konum: ", "<b>", data_pub_telifli$Pub_loc, "</b>"),
                  paste0("Çalıştığı Çevirmenler: ", "<b>", data_pub_telifli$Trans_Worked, "</b>"),
                  paste0("Çalıştığı Toplam Çevirmen Sayısı: ", "<b>", data_pub_telifli$Total_Trans, "</b>"),
                  paste0("Bastığı/Yayınladığı Toplam Çeviri Eser: ", "<b>", data_pub_telifli$Total_Trans_Ttl, "</b>"),
                  paste0("</div>")),
    clusterOptions = markerClusterOptions(),
    group = "Telifli Eser İçeren Yayınevleri/Basımevleri"
  ) %>%
  # Add publishers that has not-copyrighted material (data_pub_telifsiz) with markers with custom HTML popup content
  addMarkers(
    data = data_pub_telifsiz,
    lng = ~Pub_loc_lng,
    lat = ~Pub_loc_lat,
    label = ~Pub,
    icon = Telifsiz_Pub_icon,
    popup = paste(sep = "<br/>",
                  paste0("<div class='leaflet-popup-scrolled' style='max-width:200px;max-height:200px'>"),
                  paste0("Yayınevi/Basımevi Adı: ", "<b>",  data_pub_telifsiz$Pub, "</b>"),
                  paste0("Konum: ", "<b>", data_pub_telifsiz$Pub_loc, "</b>"),
                  paste0("Çalıştığı Çevirmenler: ", "<b>", data_pub_telifsiz$Trans_Worked, "</b>"),
                  paste0("Çalıştığı Toplam Çevirmen Sayısı: ", "<b>", data_pub_telifsiz$Total_Trans, "</b>"),
                  paste0("Bastığı/Yayınladığı Toplam Çeviri Eser: ", "<b>", data_pub_telifsiz$Total_Trans_Ttl, "</b>"),
                  paste0("</div>")),
    clusterOptions = markerClusterOptions(),
    group = "Telifsiz Eser İçeren Yayınevleri/Basımevleri"
  ) %>%
  # Add markers for telifli translators
  addMarkers(
    data = data_trans_telifli,
    lng = ~Trans_loc_lng,
    lat = ~Trans_loc_lat,
    label = ~Trans,
    icon = Telifli_Trans_icon,
    popup = paste(sep = "<br/>",
                  paste0("<div class='leaflet-popup-scrolled' style='max-width:200px;max-height:200px'>"),
                  paste0("Yazar/Çevirmen Adı: ", "<b>",  data_trans_telifli$Trans, "</b>"),
                  paste0("Doğum Yeri: ", "<b>",  data_trans_telifli$Trans_loc, "</b>"),
                  paste0("URL: ", "<b><a href='", data_trans_telifli$URL, "' target='_blank'>", data_trans_telifli$URL, "</a></b>"),
                  paste0("Toplam Çeviri: ", "<b>", data_trans_telifli$Total_Ttl, "</b>"),
                  paste0("Çalışma Alanları: ", "<b>", data_trans_telifli$Jbs_Fld, "</b>"),
                  paste0("Kimlikler: ", "<b>", data_trans_telifli$Trans_ID, "</b>"),
                  paste0("Eserleri Telifli mi?: ", "<b>", "Evet", "</b>"),
                  paste0("Çalıştığı Yayınevleri/Basımevleri: ", "<b>", 
                         ifelse(is.na(data_trans_telifli$Pub_Worked), "", data_trans_telifli$Pub_Worked), "</b>"),  # Use empty string for NA
                  paste0("Çalıştığı Toplam Yayınevi/Basımevi Sayısı: ", "<b>", data_trans_telifli$Total_Pub, "</b>"),
                  paste0("Doğum Yılı: ", "<b>", 
                         ifelse(is.na(data_trans_telifli$Trans_BD), "", data_trans_telifli$Trans_BD), "</b>"),  # Use empty string for NA
                  paste0("Ölüm Yılı: ", "<b>", 
                         ifelse(is.na(data_trans_telifli$Trans_DD), "", data_trans_telifli$Trans_DD), "</b>"),  # Use empty string for NA
                  paste0("Yaş: ", "<b>",
                         ifelse(is.na(data_trans_telifli$Age), "", data_trans_telifli$Age), "</b>"),  # Use empty string for NA
                  paste0("</div>")),
    clusterOptions = markerClusterOptions(),
    group = "Telifli Yazar-Çevirmenler"
  ) %>%
  # Add translators markers with custom HTML popup content
  addMarkers(
    data = data_trans_telifsiz,
    lng = ~Trans_loc_lng,
    lat = ~Trans_loc_lat,
    label = ~Trans,
    icon = Telifsiz_Trans_icon,
    popup = paste(sep = "<br/>",
                  paste0("<div class='leaflet-popup-scrolled' style='max-width:200px;max-height:200px'>"),
                  paste0("Yazar/Çevirmen Adı: ", "<b>",  data_trans_telifsiz$Trans, "</b>"),
                  paste0("Doğum Yeri: ", "<b>",  data_trans_telifsiz$Trans_loc, "</b>"),
                  paste0("URL: ", "<b><a href='", data_trans_telifsiz$URL, "' target='_blank'>", data_trans_telifsiz$URL, "</a></b>"),
                  paste0("Toplam Çeviri: ", "<b>", data_trans_telifsiz$Total_Ttl, "</b>"),
                  paste0("Çalışma Alanları: ", "<b>", data_trans_telifsiz$Jbs_Fld, "</b>"),
                  paste0("Kimlikler: ", "<b>", data_trans_telifsiz$Trans_ID, "</b>"),
                  paste0("Eserleri Telifli mi?: ", "<b>", "Hayır", "</b>"),
                  paste0("Çalıştığı Yayınevleri/Basımevleri: ", "<b>", 
                         ifelse(is.na(data_trans_telifsiz$Pub_Worked), "", data_trans_telifsiz$Pub_Worked), "</b>"),  # Use empty string for NA
                  paste0("Çalıştığı Toplam Yayınevi/Basımevi Sayısı: ", "<b>", data_trans_telifsiz$Total_Pub, "</b>"),
                  paste0("Doğum Yılı: ", "<b>", 
                         ifelse(is.na(data_trans_telifsiz$Trans_BD), "", data_trans_telifsiz$Trans_BD), "</b>"),  # Use empty string for NA
                  paste0("Ölüm Yılı: ", "<b>", 
                         ifelse(is.na(data_trans_telifsiz$Trans_DD), "", data_trans_telifsiz$Trans_DD), "</b>"),  # Use empty string for NA
                  paste0("Yaş: ", "<b>",
                         ifelse(is.na(data_trans_telifsiz$Age), "", data_trans_telifsiz$Age), "</b>"),  # Use empty string for NA
                  paste0("</div>")),
    clusterOptions = markerClusterOptions(),
    group = "Telifsiz Yazar-Çevirmenler"
  ) %>%
  # Draw relations (lines) from translators to publishers and place them in overlay groups
  draw_relations_trans_to_pub(data_trans_telifli, data_pub_telifli, colors, "Telifli Eserler İlişkileri") %>%
  draw_relations_trans_to_pub(data_trans_telifsiz, data_pub_telifsiz, colors, "Telifsiz Eserler İlişkileri") %>%
  
  # Add a layers control for switching between publishers, translators, and relations
  addLayersControl(
    baseGroups = c("Boş Katman"),
    overlayGroups = c(
      "Telifli Eser İçeren Yayınevleri/Basımevleri", 
      "Telifsiz Eser İçeren Yayınevleri/Basımevleri", 
      "Telifli Yazar-Çevirmenler", 
      "Telifsiz Yazar-Çevirmenler",
      "Telifli Eserler İlişkileri",   # Added for relations
      "Telifsiz Eserler İlişkileri"   # Added for relations
    ),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  
  # Add search functionality for publishers (both telifli and telifsiz)
  addSearchFeatures(
    targetGroups = c("Telifli Eser İçeren Yayınevleri/Basımevleri", "Telifsiz Eser İçeren Yayınevleri/Basımevleri"),
    options = searchFeaturesOptions(
      propertyName = "label",  # Search based on the label (Pub names)
      zoom = 15,               # Zoom level when a result is found
      openPopup = TRUE,        # Open popup on the search result
      autoCollapse = TRUE,     # Auto collapse the search box after a search
      hideMarkerOnCollapse = TRUE
    )
  ) %>%
  
  # Add search functionality for translators (both telifli and telifsiz)
  addSearchFeatures(
    targetGroups = c("Telifli Yazar-Çevirmenler", "Telifsiz Yazar-Çevirmenler"),
    options = searchFeaturesOptions(
      propertyName = "label",  # Search based on the label (Translator names)
      zoom = 15,
      openPopup = TRUE,
      autoCollapse = TRUE,
      hideMarkerOnCollapse = TRUE
    )
  ) %>%
  # Add some extra map features
  addMouseCoordinates() %>%
  addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2))

legend_html <- "
  <div style='padding: 0px; background-color: white; border-radius: 0px;'>
    <div style='font-weight: bold; margin-bottom: 0px; font-size: 10px;'>Açıklama</div>
    <div style='display: flex; align-items: center; margin-bottom: 0px;'>
      <img src='https://raw.githubusercontent.com/KadirYigitUS/TEIS_Publishers_Translators_Map/refs/heads/main/images/printing-press-svgrepo-com.svg' style='width: 10px; height: 10px; margin-right: 1px;'>
      <span style='font-size: 8px;'>Telifli Eser - Yayıncı</span>
    </div>
    <div style='display: flex; align-items: center; margin-bottom: 0px;'>
      <img src='https://raw.githubusercontent.com/KadirYigitUS/TEIS_Publishers_Translators_Map/refs/heads/main/images/printing-press_2-svgrepo-com.svg' style='width: 10px; height: 10px; margin-right: 1px;'>
      <span style='font-size: 8px;'>Telifsiz Eser - Yayıncı</span>
    </div>
    <div style='display: flex; align-items: center; margin-bottom: 0px;'>
      <img src='https://raw.githubusercontent.com/KadirYigitUS/TEIS_Publishers_Translators_Map/refs/heads/main/images/translator-language-svgrepo-com.svg' style='width: 10px; height: 10px; margin-right: 1px;'>
      <span style='font-size: 8px;'>Telifli Yazar-Çevirmen</span>
    </div>
    <div style='display: flex; align-items: center;'>
      <img src='https://raw.githubusercontent.com/KadirYigitUS/TEIS_Publishers_Translators_Map/refs/heads/main/images/translator-language_2-svgrepo-com.svg' style='width: 10px; height: 10px; margin-right: 1px;'>
      <span style='font-size: 8px;'>Telifsiz Yazar-Çevirmen</span>
     </div>
"

# add legend
map <- map %>%
  addControl(html = legend_html, position = "bottomright", className = "custom-legend")

map
