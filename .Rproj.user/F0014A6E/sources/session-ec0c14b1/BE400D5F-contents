# Load necessary libraries
library(readr)
library(dplyr)

# Load CSV data
data_pub <- read_csv("data/csv/publisher_info_card_df.csv")
data_trans <- read_csv("data/csv/translator_info_card_df.csv")
colnames(data_pub)
colnames(data_trans)
# Initialize data frames for nodes and edges
nodes <- data.frame(id = character(), label = character(), stringsAsFactors = FALSE)
edges <- data.frame(source = character(), target = character(), stringsAsFactors = FALSE)

# Function to create nodes and edges
create_gephi_nodes_edges <- function(trans_data, pub_data) {
  
  # Loop through each translator
  for (i in 1:nrow(trans_data)) {
    
    # Get the translator's information
    trans_name <- trans_data$Trans[i]
    pub_worked <- trans_data$Pub_Worked[i]  # Corrected variable name
    
    # Add translator to nodes if not already present
    if (!(trans_name %in% nodes$id)) {
      nodes <<- rbind(nodes, data.frame(id = trans_name, label = trans_name, stringsAsFactors = FALSE))
    }
    
    # Split and clean the publisher names
    if (!is.na(pub_worked) && pub_worked != "") {
      pub_list <- strsplit(pub_worked, ",")[[1]]  # Split by comma
      pub_list <- trimws(pub_list)  # Strip leading/trailing whitespace
      pub_list <- pub_list[pub_list != ""]  # Remove empty values if any
      
      # Loop through each publisher for the current translator
      for (pub_name in pub_list) {
        
        # Find all matching publishers in data_pub
        pub_matches <- pub_data %>% filter(Pub == pub_name)
        
        # Add publisher to nodes and create edges
        for (j in 1:nrow(pub_matches)) {
          
          # Add publisher to nodes if not already present
          if (!(pub_name %in% nodes$id)) {
            nodes <<- rbind(nodes, data.frame(id = pub_name, label = pub_name, stringsAsFactors = FALSE))
          }
          
          # Add an edge from translator to publisher
          edges <<- rbind(edges, data.frame(source = trans_name, target = pub_name, stringsAsFactors = FALSE))
        }
      }
    }
  }
}

# Create nodes and edges
create_gephi_nodes_edges(data_trans, data_pub)

# Print nodes and edges data frames
print("Nodes:")
print(nodes)
print("Edges:")
print(edges)

# Save to CSV for Gephi
write_csv(nodes, "data/csv/Gephi/TEIS_Trans_Pub_nodes.csv")
write_csv(edges, "data/csv/Gephi/TEIS_Trans_Pub_edges.csv")

