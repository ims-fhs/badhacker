# nodes <- data.frame(id = functions$id,
#                     label = functions$names,
#                     value = rep(0.3, nrow(functions)),
#                     # tooltip (html or character), when the mouse is above
#                     title = paste0("<p><b>", functions$names,"</b><br>Node !</p>"))
#
# functional_relations <- get_edges_from_structure(my_strcuture)
# edges <- data.frame(from = functional_relations$from_id, to = functional_relations$to_id,
#                     arrows = rep("to", nrow(functional_relations)))
#
# visNetwork(nodes, edges, height = "100%", width = "100%") %>%
#   visOptions(highlightNearest = TRUE) %>%
#   visLayout(hierarchical = TRUE) %>%
#   visInteraction(navigationButtons = TRUE)

