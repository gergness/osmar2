

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
  ret <- osm_to_df(xroot, type, "tag", c(k = "k", v = "v"))
  
  if(nrow(ret)==0)
    return(data.frame(id=numeric(),
                      k=factor(),
                      v=factor()))

  ret$id <- as.numeric(as.character(ret$id))
  ret$k <- as.factor(as.character(ret$k))
  ret$v <- as.factor(as.character(ret$v))

  ret
}


### Relation refernces: ##############################################

extract_ref <- function(xroot){
  wayref <- extract_ref_type(xroot, "way")
  relationref <- extract_ref_type(xroot, "relation")
  list(wayref=wayref, relationref=relationref)
}

extract_ref_type <- function(xroot, type) {
  if (type == "way") {
    ref_node <- "nd"
    vars <- c(ref = "ref") 
  }
  if (type == "relation") {
    ref_node <- "member"
    vars <- c(type = "type", ref = "ref", role = "role")
  }
 
  ret <- osm_to_df(xroot, type, ref_node, vars)
  
  if(nrow(ret)==0)
    return(data.frame(id=numeric(),
                      k=factor(),
                      v=factor()))
  
  ret$id <- as.numeric(as.character(ret$id))
  ret$ref <- as.numeric(as.character(ret$ref))
  
  ret
}


### Helpers: #########################################################

osm_to_df <- function(xroot, elem, ref, vars) {
  # It is wayyyy faster to go from the root every time, so this code
  # seems a little indirect.
  
  # Get id from the parent node, repeat it once per node that the other vars
  # are going to come from.
  nodes <- xml_find_all(xroot, sprintf("/osm/%s[./%s]", elem, ref))
  id <- xml_attr(nodes, "id")
  lens <- xml_find_num(nodes, sprintf("count(./%s)", ref))
  id <- rep(id, lens)
  
  # Other vars are from children
  ex_vars <- lapply(vars, function(x) {
    xml_text(xml_find_all(xroot, sprintf("/osm/%s/%s/@%s", elem, ref, x)))
  })
  
  # Put them together
  dplyr::bind_cols(
    data_frame(id = id), 
    ex_vars
  )
}
