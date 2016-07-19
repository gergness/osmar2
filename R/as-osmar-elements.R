

### Attributes: ######################################################

extract_attr <- function(xroot) {
  nodeattr <- extract_attr_type(xroot, "node")
  wayattr <- extract_attr_type(xroot, "way")
  relationattr <- extract_attr_type(xroot, "relation")

  list(nodeattr = nodeattr, wayattr = wayattr, relationattr = relationattr)
}

extract_attr_type <- function(xroot, type){
  xnodes <- xml_find_all(xroot, sprintf("/osm/%s", type))

  # To add on the missing columns
  empty <- data_frame(id = numeric(),
                      visible = character(),
                      timestamp = character(),  # How to create a POSIXlt(0)?
                      version = numeric(),
                      changeset = numeric(),
                      user = factor(),
                      uid = factor(),
                      lat = numeric(),
                      lon = numeric())
  
  ret <- do.call(rbind, xml_attrs(xnodes))
  ret <- bind_rows(as_data_frame(ret), empty)

  ret$timestamp <- as.POSIXct(strptime(ret$timestamp, format="%Y-%m-%dT%H:%M:%S"))
  ret$lat<- as.numeric(as.character(ret$lat))
  ret$lon<- as.numeric(as.character(ret$lon))
  ret$id<- as.numeric(as.character(ret$id))
  ret$version<- as.numeric(as.character(ret$version))
  ret$uid<- as.factor(as.character(ret$uid))
  ret$user<- as.factor(as.character(ret$user))
  ret$changeset<- as.numeric(as.character(ret$changeset))

  # lat and lon are only in nodes
  if (type != "node") {
    ret$lat <- NULL
    ret$lon <- NULL
  }
  
  ret
}


### Data: ############################################################

extract_data <- function(xroot){
  nodedata <- extract_data_type(xroot, "node")
  waydata <- extract_data_type(xroot, "way")
  relationdata <- extract_data_type(xroot, "relation")
  list(nodedata=nodedata, waydata=waydata, relationdata=relationdata)
}

extract_data_type <- function(xroot, type){
  values <- xml_find_all(xroot, sprintf("/osm/%s[./tag]", type))
  
  ##auswahl der nodes MIT daten
  if(length(values)==0)
    return(data.frame(id=numeric(),
                      k=factor(),
                      v=factor()))

  ret <- xml2long(values, "data")

  ret$id <- as.numeric(as.character(ret$id))
  ret$k <- as.factor(as.character(ret$k))
  ret$v <- as.factor(as.character(ret$v))

  ret
}

xml2long <- function(x, dfType) {
  if (dfType != "data") stop(paste0("xml2long expected dfType = 'data', got '", dfType, "'"))
  out <- data_frame(
    id = xml_attr(x, "id"),
    
    dfs = lapply(x, function(parent_node) {
      tag_nodes <- xml_find_all(parent_node, "./tag")
      
      data_frame(k = xml_attr(tag_nodes, "k"), 
                       v = xml_attr(tag_nodes, "v"))
    })
  )
  
  unnest(out)
}


### Relation refernces: ##############################################

extract_ref <- function(xroot){
  wayref <- extract_ref_type(xroot, "way")
  relationref <- extract_ref_type(xroot, "relation")
  list(wayref=wayref, relationref=relationref)
}

extract_ref_type <- function(xroot, type) {
  if (type == "way") ref_node <- "nd"
  if (type == "relation") ref_node <- "member"
  values <- xml_find_all(xroot, sprintf("/osm/%s[./%s]", type, ref_node))
  
  ##auswahl der nodes MIT daten
  if(length(values)==0)
    return(data.frame(id=numeric(),
                      k=factor(),
                      v=factor()))
  
  ret <- xml2long(values, "data")
  
  ret$id <- as.numeric(as.character(ret$id))
  ret$k <- as.factor(as.character(ret$k))
  ret$v <- as.factor(as.character(ret$v))
  
  ret
}
