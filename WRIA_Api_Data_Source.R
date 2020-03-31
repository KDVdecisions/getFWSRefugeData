setClass(
  Class="SiteData",
  representation(baseData="sf",
                 nwisSites="sf",
                 wqpSites="sf",
                 availableNwisData="data.frame",
                 availableWqpData="data.frame")
)

#'creates SiteData object which contains information about a requested site
#'
#'@param baseData: sf object containing base geometry for requested site
#'@param NWIS: Logical indicating whether NWIS data should be included
#'@param WQP: Logical indicating whether WQP data should be included
#'@param buffer: Logical indicating whether the bounding box should be constructed from a buffered base geometry
#'
createSiteObject <- function(baseData, NWIS, WQP, buffer=FALSE){
  if(buffer){
    siteBuffer  <- st_as_sf(st_buffer(st_union(baseData), dist=0.01))
    boundingBox <- getBoundingBox(siteBuffer, epsg=4269)  
  }
  else{
    siteBuffer <- NULL
    boundingBox <- getBoundingBox(baseData, epsg=4269)
  }
  
  site <- new("SiteData")
  site@baseData <- baseData
  
  if(NWIS){
    nwisSites <- getSites(source="NWIS", focalSf=baseData, bufferSf=siteBuffer,
                         bbox=boundingBox, epsg=4269)
    availableNwisData <- whatNWISdata(siteNumber=nwisSites$site_no)
    site@nwisSites <- nwisSites
    site@availableNwisData <- availableNwisData
  }
 
  if(WQP){
    wqpSites <- getSites(source="WQP", focalSf=baseData, bufferSf=siteBuffer,
                        bbox=boundingBox, epsg=4269)
    
    print(NROW(wqpSites))
    
    availableWqpData <- whatWQPdata(siteid=wqpSites$MonitoringLocationIdentifier)
    site@wqpSites <- wqpSites
    site@availableWqpData <- availableWqpData
  }
  return(site)
}

#'returns a list of SiteData objects containing information requested in userInput
#'@param userInput: data.frame containing user input
#'TODO: Clean up and make more readable
getSiteData <- function(userInput){
  fwsInterest=NULL
  fwsApproved=NULL
  huc8=NULL
  huc10=NULL
  
  if(userInput$Approved){
    fwsApproved <- createSiteObject(baseData=getFWSCadastral(userInput$Refuge,approved=TRUE),
                                    NWIS=userInput$NWIS,
                                    WQP=userInput$WQP,
                                    buffer=TRUE)
    if(userInput$HUC8){
      huc8 <- createSiteObject(baseData=getHucs(hucLayer="WBDHU8",
                                                focalSf=fwsApproved@baseData),
                               NWIS=userInput$NWIS,
                               WQP=userInput$WQP)
    }
    if(userInput$HUC10){
      huc10 <- createSiteObject(baseData=getHucs(hucLayer="WBDHU10",
                                                 focalSf=fwsApproved@baseData),
                                NWIS=userInput$NWIS,
                                WQP=userInput$WQP)
    }
  }#if userInput$approved
  
  if(userInput$Interest){
    fwsInterest <- createSiteObject(baseData=getFWSCadastral(userInput$Refuge), 
                                    NWIS=userInput$NWIS,
                                    WQP=userInput$WQP,
                                    buffer=TRUE)
    if(!userInput$Approved){
      if(userInput$HUC8){
        huc8 <- createSiteObject(baseData=getHucs(hucLayer="WBDHU8",
                                                  focalSf=fwsInterest@baseData),
                                 NWIS=userInput$NWIS,
                                 WQP=userInput$WQP)
      }
      if(userInput$HUC10){
        huc10 <- createSiteObject(baseData=getHucs(hucLayer="WBDHU10",
                                                   focalSf=fwsInterest@baseData),
                                  NWIS=userInput$NWIS,
                                  WQP=userInput$WQP)
      }
    }#if !approved
  }#if userInput$interest
  return(list("interestData"=fwsInterest,"approvedData"=fwsApproved,"huc8Data"=huc8,"huc10Data"=huc10))
}
#'Generates plot based on requested data
#'@param siteData: list of SiteData objects containing requested data
generatePlot <- function(siteData){
  regionLeaf <- leaflet() %>%
    addProviderTiles("Esri.WorldStreetMap", group="Map") %>%
    addProviderTiles("Esri.WorldImagery", group="Image")
  overlayGroups <- c()
  if(!is.null(siteData$approvedData)){
    regionLeaf <- addPolygons(regionLeaf,data=as_Spatial(siteData$approvedData@baseData,4269), stroke=F,  
                              fillColor="red", fillOpacity=0.7, group="Approved Boundaries")
    overlayGroups <- c(overlayGroups,"Approved Boundaries")
  }
  if(!is.null(siteData$interestData)){
    regionLeaf <- addPolygons(regionLeaf,data=as_Spatial(siteData$interestData@baseData,4269), stroke=F,  
                              fillColor="cyan", fillOpacity=0.7, group="Interest Boundaries")
    overlayGroups <- c(overlayGroups,"Interest Boundaries")
  }
  if(!is.null(siteData$huc8Data)){
    regionLeaf <- addPolygons(regionLeaf,data=as_Spatial(siteData$huc8Data@baseData,4269), fill=F, color="goldenrod",
                opacity=0.8, group="HUC8")
    overlayGroups <- c(overlayGroups,"HUC8")
  }
  if(!is.null(siteData$huc10Data)){
    regionLeaf <- addPolygons(regionLeaf,data=as_Spatial(siteData$huc10Data@baseData,4269), fillColor="white", fillOpacity=0.1,
                color="blue",weight=2, opacity=1, group="HUC10")
    overlayGroups <- c(overlayGroups,"HUC10")
  }
  
  regionLeaf <- addLayersControl(regionLeaf,baseGroups=c("Map", "Image"),
                   overlayGroups = overlayGroups,
                   options = layersControlOptions(collapsed=FALSE))
 return(regionLeaf)
}



#' Abbreviates refuge name
#' 
#' @param refugeName: char, refuge nam
createNameAbbrev <- function(refugeName){
  name <- str_replace_all(str_to_title(refugeName), pattern=" ", repl="")
  return(sprintf("%s_%s",name,"shapeFile"))
}


#' Check to see if warning indicates result directory couldn't be created
#' 
#' @param warning: warnings generated by call to st_write()
#' TODO: Needs wider use cases to cover other potential warnings/errors
isWriteWarning <- function(warning){
  warningText <- conditionMessage(warning)
  dirWarning <- "cannot create dir"
  return(grepl(dirWarning, warningText, fixed=TRUE))
}

#' Write refuge data to a given location
#' 
#' @param dat: sf, refuge data obtained from FWS Rest API
#' @param refugeName: char, Name of given refuge
#' @param resultsFolder: char, Path to a location in which to store refuge data
#' TODO: Add code to catch errors when trying to create result folder, hide warnings generated by st_write(),
##' if .shp already exists should it automatically be overwritten?
writeFWSCadastral <- function(dat, refugeName, resultsFolder){
  refugeAbbrev <- createNameAbbrev(refugeName)  
  resultsPath <- sprintf("%s/%s",resultsFolder,refugeAbbrev)
  if(is.null(resultsFolder)){
    print("Please provide a path to folder in which to store results")
    return(dat)
  }
  else if(dir.exists(resultsFolder)) {         #if dir exists
    if(file.access(resultsFolder,2) == 0){    #if R has write permissions
      dir.create(file.path(resultsPath))
      st_write(dat, sprintf("%s/%s_Boundary.shp", 
                            resultsPath, refugeAbbrev), delete_dsn=TRUE, quiet = TRUE)   
    }
    else{
      print(sprintf("This program does not have permission to write to %s",resultsFolder))
    }
  }
  else{       #if dir doesn't exist
    tryCatch({
      dir.create(file.path(resultsFolder))
      dir.create(file.path(resultsPath))
    },
    warning = function(w){
      if(isWriteWarning(w)){ 
        print(sprintf("Cannot create/access '%s', please check provided path", resultsFolder))
        do.call(return, list(NULL),envir = sys.frame(-4)) #return to getFWSCadastral
      }
    })
    st_write(dat, sprintf("%s/%s_ApprovedBoundary.shp", 
                          resultsPath, refugeAbbrev), delete_dsn=TRUE, quiet = TRUE)  
  }
  
}#Write FWSCadastral



#' Obtain intersecting huc8 or huc10 waterbody from USGS Rest API
#' 
#' @param hucLayer: String dictating whether to get huc8 or huc10
#' @param focalSf: Shape file containing site which to obtain interecting waterbodies
#' TODO: Lot's of testing, attempt to find way to query API with polygon rather than
#'       bounding box to remove need to filter out excess HUCs
getHucs <- function(hucLayer, focalSf){
  bbox <- toString(st_bbox(focalSf))
  epsg <- st_crs(focalSf)$epsg
  
  if(hucLayer == "WBDHU8"){
    baseUrl <- readLines("Data/Base_URL.txt")[2]
  }
  else if (hucLayer == "WBDHU10"){
    baseUrl <- readLines("Data/Base_URL.txt")[3]
  }
  
  url <- param_set(baseUrl, key ="geometry", value = URLencode(bbox)) %>%
    param_set(key = "inSR", value = epsg) %>%
    param_set(key = "outSR", value = epsg)
  
  huc <- read_sf(url)
  huc <- dplyr::filter(huc, lengths(st_intersects(huc,focalSf))>0)
  
  return(huc)
}


#' Obtain refuge data from FWS Rest API
#' 
#' @param refugeName: char, Name of desired refuge
#' @param writeResult: boolean, describes whether or not to store obtained data
#' @param resultsFolder: char, path to folder in which to store obtained data
#' TODO: Include check for time out(ie FSW servers are down)
getFWSCadastral <- function(refugeName, approved=FALSE){
  if(nchar(refugeName) < 1){ #generate url to query DB
    print("Please provide a valid refuge name")
    return(NULL)
  }
  if(approved){
    baseUrl <- readLines("Data/base_URL.txt")[4]
  }else{
    baseUrl <- readLines("Data/base_URL.txt")[1]
  }
  
  url <- param_set(baseUrl, key = "where", value = sprintf("ORGNAME+LIKE+'%s%%25'",gsub(" ","+",refugeName))) %>%
    param_set(key = "outSR", value = 4269)
  dat <- read_sf(url)    #query db and read into sf obj
  if(nrow(dat)==0| is.null(dat)){
    print("No data returned for this request, please check provided refuge name")
  }
  else{
    return(dat)
  }
  
}#getFWSCadastral


#'returns bounding box of sf features as vector
#' @author: Dr. Ashton Drew
getBoundingBox <- function(sfData, epsg, addBuffer=FALSE, distBuffer=NA){
  # could be polygon, line, point, or mixed geometry
  dat <- sfData 
  if (addBuffer==TRUE){
    dat <- st_as_sf(st_buffer(st_union(dat), dist=distBuffer))
  }
  if (st_crs(dat) != st_crs(epsg)) {
    # Check CRS and transorm if necessary
    dat <- st_transform(dat, crs=epsg)
  }
  # Get bounding box as vector (xmin, ymin, xmax, ymax)
  box <- as.vector(st_bbox(dat, crs=epsg))
}

#' TODO: fix
getSites <- function(source, focalSf, bufferSf=NULL, bbox, epsg){
  # Identify NWIS sites within boundary
  latColName <- "LatitudeMeasure"
  lngColName <- "LongitudeMeasure"
  idColName <- "MonitoringLocationIdentifier"
  if(source=="NWIS"){
    sitesDf <- whatNWISsites(bBox=round(bbox, 1))
    latColName <- "dec_lat_va"
    lngColName <- "dec_long_va"
    idColName <- "site_no"
  } else {
    sitesDf <- whatWQPsites(bBox=round(bbox, 1))
  }
  # convert located sites to spatial simple features
  sitesSf <- st_as_sf(sitesDf, coords=c(lngColName, latColName), crs=epsg)
  sitesColumns <- names(sitesSf)
  # Identify all sites within focal area and label
  inSites <- st_intersection(sitesSf, focalSf) %>%
    dplyr::select(one_of(sitesColumns)) %>%
    dplyr::mutate(InNear = "In")
  # Identify sites near but outside focal area
  if (!is.null(bufferSf)){
    
    nearSites <- st_intersection(sitesSf, bufferSf) %>%
      dplyr::select(one_of(sitesColumns)) %>%
      dplyr::mutate(InNear = "Near")
    if (nrow(inSites)>0){
      dat <- st_intersects(nearSites,focalSf)
      nearSites <- nearSites[!(lengths(dat) > 0), ]
      allSites <- rbind(inSites, nearSites)
      return(allSites)
    } else {
      allSites <- nearSites
    }
  } else {
    allSites <- inSites
    return(allSites)
  }
}

#'reads in user input from ReportRequest.csv and ensures logical input is valid
readReportRequest <- function(){
  input <- read.csv("data/ReportRequest.csv",stringsAsFactors = FALSE)
  for(i in 2:7){
    input[i] <- tolower(input[i])
    if(input[i] == "yes"){
      input[i] = TRUE
    }
    else if(input[i] == "no"){
      input[i] = FALSE
    }
    else{
      stop(sprintf("invalid input in '%s' field, please check ReportRequest.csv in Data folder",names(input)[i]))
      return(1)
    }
  }
  if(input$Approved == FALSE & input$Interest == FALSE){
    stop("Invalid input, either interest field or approved field must be 'yes', please check ReportRequest.csv in Data folder")
    return(2)
  }
  return(input)
}

#st_intersection(nearSites,focalSf)

#dat <- st_intersects(nearSites,focalSf)

#lengths(dat) > 0

#dat[[4]]

#dat <- st_intersection(sitesSf,focalSf)%>%
#  dplyr::select(one_of(sitesColumns)) %>%
#  dplyr::mutate(InNear = "In")

#dat$InNear
#leaflet()%>%
#  addProviderTiles("Esri.WorldStreetMap") %>%
#  addPolygons(data=focalSf) %>%
#  addPolygons(data=bufferSf,color="cyan",fill=FALSE) %>%
#  addCircles(data=allSites,radius=5,color="red")
  #addCircles(data=inSites,radius=5,color="red") %>%
  #addCircles(data=nearSites,radius=3,color="green")

#inSites <- st_intersection(sitesSf, focalSf) %>%
#  dplyr::select(one_of(sitesColumns)) %>%
#  dplyr::mutate(InNear = "In")


