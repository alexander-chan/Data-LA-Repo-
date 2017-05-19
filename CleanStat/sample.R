#Sample Sankey
URL <- paste0('https://cdn.rawgit.com/christophergandrud/networkD3/',
              'master/JSONdata/energy.json')
energy <- jsonlite::fromJSON(URL)

# Plot
sankeyNetwork(Links = energy$links, Nodes = energy$nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name',
              units = 'TWh', fontSize = 12, nodeWidth = 30)

# Colour links
energy$links$energy_type <- sub(' .*', '',
                                energy$nodes[energy$links$source + 1, 'name'])

sankeyNetwork(Links = energy$links, Nodes = energy$nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name',
              LinkGroup = 'energy_type', NodeGroup = NULL)

energy$nodes
head(energy$links)
str(energy$nodes)
dim(energy$links)

str(energy$links)
energy$links

str(energy)
str(cleanstat)
