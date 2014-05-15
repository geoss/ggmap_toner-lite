get_map_2 <- function (location = c(lon = -95.3632715, lat = 29.7632836), 
    zoom = "auto", scale = "auto", maptype = c("terrain", "satellite", 
        "roadmap", "hybrid", "toner", "toner-lite", "watercolor"), messaging = FALSE, 
    urlonly = FALSE, filename = "ggmapTemp", crop = TRUE, color = c("color", 
        "bw"), source = c("google", "osm", "stamen", "cloudmade"), 
    api_key) 
{
    args <- as.list(match.call(expand.dots = TRUE)[-1])
    if ("verbose" %in% names(args)) {
        .Deprecated(msg = "verbose argument deprecated, use messaging.")
        messaging <- eval(args$verbose)
    }
    if ("center" %in% names(args)) {
        .Deprecated(msg = "center argument deprecated, use location.")
        location <- eval(args$center)
    }
    source <- match.arg(source)
    color <- match.arg(color)
    if (missing(maptype)) {
        if (source != "cloudmade") {
            maptype <- "terrain"
        }
        else {
            maptype <- 1
        }
    }
    if (source == "stamen") {
        if (!(maptype %in% c("terrain", "watercolor", "toner", "toner-lite"))) {
            stop("when using stamen maps, only terrain, watercolor, and toner maptypes available", 
                call. = FALSE)
        }
    }
    if (source == "google" & (maptype == "toner" || maptype == 
        "watercolor")) {
        message(paste0("maptype = \"", maptype, "\" is only available with source = \"stamen\"."))
        message(paste0("resetting to source = \"stamen\"..."))
        source <- "stamen"
    }
    location_stop <- TRUE
    if (is.character(location) && length(location) == 1) {
        location_type <- "address"
        location_stop <- FALSE
    }
    if (is.numeric(location) && length(location) == 2) {
        location_type <- "lonlat"
        location_stop <- FALSE
        if (!is.null(names(location))) {
            loc_names <- names(location)
            if (all(loc_names == c("long", "lat"))) {
                names(location) <- c("lon", "lat")
            }
            else if (all(loc_names == c("lat", "lon"))) {
                message("note : locations should be specified in the lon/lat format, not lat/lon.")
                location <- location[c("lon", "lat")]
            }
            else if (all(loc_names == c("lat", "long"))) {
                message("note : locations should be specified in the lon/lat format, not lat/lon.")
                location <- location[c("long", "lat")]
                names(location) <- c("lon", "lat")
            }
        }
        else {
            names(location) <- c("lon", "lat")
        }
    }
    if (is.numeric(location) && length(location) == 4) {
        location_type <- "bbox"
        location_stop <- FALSE
        if (length(names(location)) > 0) {
            if (!all(names(location) %in% c("left", "bottom", 
                "right", "top"))) {
                stop("bounding boxes should have name left, bottom, right, top)", 
                  call. = FALSE)
            }
            location <- location[c("left", "bottom", "right", 
                "top")]
        }
        else {
            names(location) <- c("left", "bottom", "right", "top")
        }
    }
    if (location_stop) {
        stop("improper location specification, see ?get_map.", 
            call. = F)
    }
    if (zoom == "auto" && location_type == "bbox") {
        if (zoom == "auto") {
            lon_range <- location[c("left", "right")]
            lat_range <- location[c("bottom", "top")]
            if (missing(zoom)) {
                lonlength <- diff(lon_range)
                latlength <- diff(lat_range)
                zoomlon <- ceiling(log2(360 * 2/lonlength))
                zoomlat <- ceiling(log2(180 * 2/latlength))
                zoom <- max(zoomlon, zoomlat)
            }
        }
    }
    else if (zoom == "auto" && location_type != "bbox") {
        zoom = 10
    }
    if (scale == "auto") {
        if (source == "google") 
            scale <- 2
        if (source == "osm") 
            scale <- OSM_scale_lookup(zoom)
    }
    if (source == "google") {
        if (location_type == "bbox") {
            warning("bounding box given to google - spatial extent only approximate.", 
                call. = FALSE, immediate. = TRUE)
            message("converting bounding box to center/zoom specification. (experimental)")
            user_bbox <- location
            location <- c(lon = mean(location[c("left", "right")]), 
                lat = mean(location[c("bottom", "top")]))
        }
        map <- get_googlemap(center = location, zoom = zoom, 
            maptype = maptype, scale = scale, messaging = messaging, 
            urlonly = urlonly, filename = filename, color = color)
        if (FALSE) {
            bb <- attr(map, "bb")
            mbbox <- c(left = bb$ll.lon, bottom = bb$ll.lat, 
                right = bb$ur.lon, top = bb$ur.lat)
            size <- dim(map)
            if (location_type == "bbox") {
                slon <- seq(mbbox["left"], mbbox["right"], length.out = size[1])
                slat <- seq(mbbox["top"], mbbox["bottom"], length.out = size[2])
                keep_x_ndcs <- which(user_bbox["left"] <= slon & 
                  slon <= user_bbox["right"])
                keep_y_ndcs <- which(user_bbox["bottom"] <= slat & 
                  slat <= user_bbox["top"])
                map <- map[keep_y_ndcs, keep_x_ndcs]
                class(map) <- c("ggmap", "raster")
                attr(map, "bb") <- data.frame(ll.lat = user_bbox["bottom"], 
                  ll.lon = user_bbox["left"], ur.lat = user_bbox["top"], 
                  ur.lon = user_bbox["right"])
            }
        }
        return(map)
    }
    if (source == "osm") {
        if (location_type != "bbox") {
            gm <- get_googlemap(center = location, zoom = zoom, 
                filename = filename)
            location <- as.numeric(attr(gm, "bb"))[c(2, 1, 4, 
                3)]
        }
        return(get_openstreetmap(bbox = location, scale = scale, 
            messaging = messaging, urlonly = urlonly, filename = filename, 
            color = color))
    }
    if (source == "stamen") {
        if (location_type != "bbox") {
            gm <- get_googlemap(center = location, zoom = zoom, 
                filename = filename)
            location <- as.numeric(attr(gm, "bb"))[c(2, 1, 4, 
                3)]
        }
        return(get_stamenmap_2(bbox = location, zoom = zoom, maptype = maptype, 
            crop = crop, messaging = messaging, urlonly = urlonly, 
            filename = filename, color = color))
    }
    if (source == "cloudmade") {
        if (missing(api_key)) 
            stop("an api key must be provided for cloudmade maps, see ?get_cloudmademap.", 
                call. = FALSE)
        if (location_type != "bbox") {
            gm <- get_googlemap(center = location, zoom = zoom, 
                filename = filename)
            location <- as.numeric(attr(gm, "bb"))[c(2, 1, 4, 
                3)]
        }
        return(get_cloudmademap(bbox = location, zoom = zoom, 
            maptype = maptype, crop = crop, messaging = messaging, 
            urlonly = urlonly, filename = filename, highres = TRUE, 
            color = color, api_key = api_key))
    }
}

get_stamenmap_2 <- function (bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, 
    top = 30.14344), zoom = 10, maptype = c("terrain", "watercolor", 
    "toner", "toner-lite"), crop = TRUE, messaging = FALSE, urlonly = FALSE, 
    filename = "ggmapTemp", color = c("color", "bw"), ...) 
{
    args <- as.list(match.call(expand.dots = TRUE)[-1])
    argsgiven <- names(args)
    if ("bbox" %in% argsgiven) {
        if (!(is.numeric(bbox) && length(bbox) == 4)) {
            stop("bounding box improperly specified.  see ?get_openstreetmap", 
                call. = F)
        }
    }
    if ("zoom" %in% argsgiven) {
        if (!(is.numeric(zoom) && length(zoom) == 1 && zoom == 
            round(zoom) && zoom >= 0 && zoom <= 18)) {
            stop("scale must be a postive integer 0-18, see ?get_stamenmap.", 
                call. = F)
        }
    }
    if ("messaging" %in% argsgiven) 
        stopifnot(is.logical(messaging))
    if ("urlonly" %in% argsgiven) 
        stopifnot(is.logical(urlonly))
    if ("filename" %in% argsgiven) {
        filename_stop <- TRUE
        if (is.character(filename) && length(filename) == 1) 
            filename_stop <- FALSE
        if (filename_stop) 
            stop("improper filename specification, see ?get_stamenmap.", 
                call. = F)
    }
    if ("checkargs" %in% argsgiven) {
        .Deprecated(msg = "checkargs argument deprecated, args are always checked after v2.1.")
    }
    maptype <- match.arg(maptype)
    color <- match.arg(color)
    if (is.null(names(bbox))) 
        names(bbox) <- c("left", "bottom", "right", "top")
    fourCorners <- expand.grid(lon = c(bbox["left"], bbox["right"]), 
        lat = c(bbox["bottom"], bbox["top"]))
    fourCorners$zoom <- zoom
    row.names(fourCorners) <- c("lowerleft", "lowerright", "upperleft", 
        "upperright")
    fourCornersTiles <- apply(fourCorners, 1, function(v) LonLat2XY(v[1], 
        v[2], v[3]))
    xsNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, 
        function(df) df$X)))))
    numXTiles <- length(xsNeeded)
    ysNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, 
        function(df) df$Y)))))
    numYTiles <- length(ysNeeded)
    tilesNeeded <- expand.grid(x = xsNeeded, y = ysNeeded)
    if (nrow(tilesNeeded) > 40) {
        message(paste0(nrow(tilesNeeded), " tiles needed, this may take a while ", 
            "(try a smaller zoom)."))
    }
    xTileProgression <- rep(1:numXTiles, numYTiles)
    yTileProgression <- rep(1:numYTiles, each = numXTiles)
    base_url <- "http://tile.stamen.com/"
    base_url <- paste(base_url, maptype, "/", zoom, sep = "")
    urls <- paste(base_url, apply(tilesNeeded, 1, paste, collapse = "/"), 
        sep = "/")
    urls <- paste(urls, ".png", sep = "")
    if (messaging) 
        message(length(urls), " tiles required.")
    if (urlonly) 
        return(urls)
    size <- 256 * c(length(xsNeeded), length(ysNeeded))
    map <- matrix("NA", nrow = size[2], ncol = size[1])
    destfile <- paste(filename, "png", sep = ".")
    for (k in seq_along(urls)) {
        download.file(urls[[k]], destfile = destfile, quiet = !messaging, 
            mode = "wb")
        tile <- readPNG(destfile)
        if (color == "color") {
            tile <- apply(tile, 2, rgb)
        }
        else if (color == "bw") {
            tile_dim <- dim(tile)
            tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
                2] + 0.11 * tile[, , 3])
            dim(tile) <- tile_dim[1:2]
        }
        map[(1 + 256 * (yTileProgression[k] - 1)):(256 * yTileProgression[k]), 
            (1 + 256 * (xTileProgression[k] - 1)):(256 * xTileProgression[k])] <- tile
    }
    bboxOfTile <- function(vXY) {
        lonlat_upperleft <- XY2LonLat(vXY[1], vXY[2], zoom)
        lonlat_lowerright <- XY2LonLat(vXY[1] + 1, vXY[2] + 1, 
            zoom)
        data.frame(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
            right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
    }
    tileBboxes <- ldply(split(tilesNeeded, 1:nrow(tilesNeeded)), 
        function(df) bboxOfTile(as.numeric(df)))
    mbbox <- c(left = min(tileBboxes$left), bottom = min(tileBboxes$bottom), 
        right = max(tileBboxes$right), top = max(tileBboxes$top))
    if (!crop) {
        map <- as.raster(map)
        class(map) <- c("ggmap", "raster")
        attr(map, "bb") <- data.frame(ll.lat = mbbox["bottom"], 
            ll.lon = mbbox["left"], ur.lat = mbbox["top"], ur.lon = mbbox["right"])
        return(map)
    }
    if (crop) {
        slon <- seq(mbbox["left"], mbbox["right"], length.out = size[1])
        slat <- seq(mbbox["top"], mbbox["bottom"], length.out = size[2])
        keep_x_ndcs <- which(bbox["left"] <= slon & slon <= bbox["right"])
        keep_y_ndcs <- which(bbox["bottom"] <= slat & slat <= 
            bbox["top"])
        croppedmap <- map[keep_y_ndcs, keep_x_ndcs]
    }
    croppedmap <- as.raster(croppedmap)
    class(croppedmap) <- c("ggmap", "raster")
    attr(croppedmap, "bb") <- data.frame(ll.lat = bbox["bottom"], 
        ll.lon = bbox["left"], ur.lat = bbox["top"], ur.lon = bbox["right"])
    croppedmap
}
environment(get_map_2) <- asNamespace('ggmap')
environment(get_stamenmap_2) <- asNamespace('ggmap')