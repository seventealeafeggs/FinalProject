mapboxOptions <- function(accessToken = getOption("mapbox.pk.eyJ1Ijoid2lsbDY3NjciLCJhIjoiY2p2MHZiZDMxMDV0cjN5cWp1ZWh6azh2cCJ9.Fst54QIIFhDrEMctpjkI8Q"),
                          style = NULL, ...) {
  
  opts <- list(
    accessToken = accessToken,
    style = style
  )
  
  # Filter out NULL
  opts[!vapply(opts, is.null, logical(1))]
}

#' @export
addMapboxGL <- function(map, options) {
  if (is.null(options[["accessToken"]])) {
    accessToken <- getOption("mapbox.pk.eyJ1Ijoid2lsbDY3NjciLCJhIjoiY2p2MHZiZDMxMDV0cjN5cWp1ZWh6azh2cCJ9.Fst54QIIFhDrEMctpjkI8Q")
    if (is.null(accessToken)) {
      stop("Please supply addMapboxGL() with a Mapbox access token, either via `options(mapbox.accessToken = pk.eyJ1Ijoid2lsbDY3NjciLCJhIjoiY2p2MHZiZDMxMDV0cjN5cWp1ZWh6azh2cCJ9.Fst54QIIFhDrEMctpjkI8Q)` or directly on the options argument.")
    }
    options$accessToken <- accessToken
  }
  map$dependencies <- c(
    map$dependencies,
    mapboxgl_deps
  )
  
  map <- htmlwidgets::onRender(map, "function(el, x, options) {
                               debugger;
                               L.mapboxGL(options).addTo(this);
}", options)
  
  map
  }

# Example
leaflet(quakes) %>%
  addMapboxGL(options = mapboxOptions(
    style = "mapbox://styles/mapbox/light-v9"
  )) %>%
  addMarkers()
