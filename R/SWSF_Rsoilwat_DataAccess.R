#' Rsoilwat data access functions
#'
#' @param x An object of class \code{\linkS4class{swOutput}}.
#' @param st An object as returned from the function \code{simTiming}.
#'
#' @name swOutput_access
NULL

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_Response_aggL <- compiler::cmpfun(function(response,
                              tscale = c("dy", "dyAll", "mo", "moAll", "yr", "yrAll"),
                              scaler = 10, FUN, weights = NULL,
                              x, st, st2, topL, bottomL) {
  FUN <- match.fun(FUN)
  tscale <- match.arg(tscale)
  responseRepeats <- if (response %in% c("TRANSP", "HYDRED")) {
      # divide by 5, because: each soil layer (cm): total, trees, shrubs, forbs, grasses
      5L
    } else {
      # c(sw_vwc, sw_evsoil, sw_soiltemp, sw_swc, sw_swa)
      1L
    }

  temp1 <- scaler * slot(slot(x, response),
    switch(tscale,
      dy = "Day", dyAll = "Day",
      mo = "Month", moAll = "Month",
      yr = "Year", yrAll = "Year"))

  if (inherits(temp1, "try-error"))
    stop("Necessary SoilWat output files are not present for aggregation of results")

  if (tscale == "dy") {
    index.col <- 2
    index.usetimestep <- st$index.usedy
    timestep_ForEachEntry <- st2$doy_ForEachUsedDay
  } else if (tscale == "dyAll") {
    index.col <- 2
    index.usetimestep <- seq_len(nrow(temp1))
    timestep_ForEachEntry <- NULL
  } else if (tscale == "mo") {
    index.col <- 2
    index.usetimestep <- st$index.usemo
    timestep_ForEachEntry <- st2$month_ForEachUsedMonth
  } else if (tscale == "moAll") {
    index.col <- 2
    index.usetimestep <- seq_len(nrow(temp1))
    timestep_ForEachEntry <- NULL
  } else if (tscale == "yr") {
    index.col <- 1
    index.usetimestep <- st$index.useyr
    timestep_ForEachEntry <- NULL
  } else if (tscale == "yrAll") {
    index.col <- 1
    index.usetimestep <- seq_len(nrow(temp1))
    timestep_ForEachEntry <- NULL
  }

  layers <- seq_len((ncol(temp1) - index.col) / responseRepeats)

  #adjust topL and bottomL locally in case temp1 doesn't contain information for every layer, e.g., soil evaporation
  if (max(layers) <= max(topL)) {
    topL <- layers
    bottomL <- 0
  } else if (max(layers) < max(bottomL)) {
    bottomL <- min(bottomL):max(layers)
  }

  res <- list()
  res[["top"]] <- if (length(topL) > 1) {
      if (is.null(weights)) {
        apply(temp1[index.usetimestep, index.col + topL, drop = FALSE], 1, FUN)
      } else {
        apply(temp1[index.usetimestep, index.col + topL, drop = FALSE], 1, FUN, weights[topL])
      }
    } else {
      temp1[index.usetimestep, index.col + topL]
    }
  res[["bottom"]] <- if (length(bottomL) > 1) {
      if(is.null(weights)){
        apply(temp1[index.usetimestep, index.col + bottomL, drop = FALSE], 1, FUN)
      } else {
        apply(temp1[index.usetimestep, index.col + bottomL, drop = FALSE], 1, FUN, weights[bottomL])
      }
    } else if (is.null(bottomL) || identical(bottomL, 0)) {
      matrix(data = 0, nrow = length(index.usetimestep), ncol = 1)
    } else {
      temp1[index.usetimestep, index.col + bottomL]
    }

  if (!is.null(timestep_ForEachEntry)) {
    res[["aggMean.top"]] <- tapply(res[["top"]], timestep_ForEachEntry, mean)
    res[["aggMean.bottom"]] <- tapply(res[["bottom"]], timestep_ForEachEntry, mean)
  }

  if (tscale == "dyAll" || tscale == "moAll" || tscale == "yrAll") {
     res[["val"]] <- temp1
  }

  res
})

get_SWPmatric_aggL <- compiler::cmpfun(function(vwcmatric, texture, sand, clay) {
  res <- list()

  res[["top"]] <- VWCtoSWP(vwcmatric$top, texture$sand.top, texture$clay.top)
  res[["bottom"]] <- VWCtoSWP(vwcmatric$bottom, texture$sand.bottom, texture$clay.bottom)

  if (!is.null(vwcmatric$aggMean.top)) {
    res[["aggMean.top"]] <- VWCtoSWP(vwcmatric$aggMean.top, texture$sand.top, texture$clay.top)
    res[["aggMean.bottom"]] <- VWCtoSWP(vwcmatric$aggMean.bottom, texture$sand.bottom, texture$clay.bottom)
  }

  if (!is.null(vwcmatric$val)) {
    if (all(as.integer(vwcmatric$val[, 2]) == vwcmatric$val[, 2])) {
      index.header <- 1:2
    } else {
      index.header <- 1
    }
    res[["val"]] <- cbind(vwcmatric$val[, index.header], VWCtoSWP(vwcmatric$val[, -index.header], sand, clay))
  }

  res
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_Temp_yr <- compiler::cmpfun(function(x, st) {
  list(mean = slot(slot(x, "TEMP"), "Year")[st$index.useyr, 4])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_Temp_mo <- compiler::cmpfun(function(x, st) {
  x <- slot(slot(x, "TEMP"), "Month")[st$index.usemo, ]
  list(min =  x[, 4],
       mean = x[, 5],
       max =  x[, 3])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_Temp_dy <- compiler::cmpfun(function(x, st) {
  x <- slot(slot(x, "TEMP"), "Day")[st$index.usedy, ]
  list(min =  x[, 4],
       mean = x[, 5],
       max =  x[, 3])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_VPD_mo <- compiler::cmpfun(function(sc, temp.mo, xin, st2) {
  rH <- rSOILWAT2::swCloud_SkyCover(xin[[sc]])
  rH <- as.vector(rH[st2$month_ForEachUsedMonth])

  list(mean = vpd(temp.mo$min, temp.mo$max, rH))
})

get_VPD_dy <- compiler::cmpfun(function(sc, temp.dy, xin, st2) {
  rH <- rSOILWAT2::swCloud_SkyCover(xin[[sc]])
  rH <- as.vector(rH[st2$month_ForEachUsedDay])

  list(mean = vpd(temp.dy$min, temp.dy$max, rH))
})

get_PPT_yr <- compiler::cmpfun(function(x, st) {
  x <- 10 * slot(slot(x, "PRECIP"), "Year")[st$index.useyr, ]
  list(ppt = x[, 2], rain = x[, 3],
       snowfall = x[, 4], snowmelt = x[, 5], snowloss = x[, 6])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_PPT_mo <- compiler::cmpfun(function(x, st) {
  x <- 10 * slot(slot(x, "PRECIP"), "Month")[st$index.usemo, ]
  list(ppt = x[, 3], rain = x[, 4],
       snowmelt = x[, 6])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_PPT_dy <- compiler::cmpfun(function(x, st) {
  x <- 10 * slot(slot(x, "PRECIP"), "Day")[st$index.usedy, ]
  list(ppt = x[, 3], rain = x[, 4],
       snowfall = x[, 5], snowmelt = x[, 6], snowloss = x[, 7])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_PET_yr <- compiler::cmpfun(function(x, st) {
  list(val = 10 * slot(slot(x, "PET"), "Year")[st$index.useyr, 2])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_PET_mo <- compiler::cmpfun(function(x, st) {
  list(val = 10 * slot(slot(x, "PET"), "Month")[st$index.usemo, 3])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_AET_yr <- compiler::cmpfun(function(x, st) {
  list(val = 10 * slot(slot(x, "AET"), "Year")[st$index.useyr, 2])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_AET_mo <- compiler::cmpfun(function(x, st) {
  list(val = 10 * slot(slot(x, "AET"), "Month")[st$index.usemo, 3])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_AET_dy <- compiler::cmpfun(function(x, st) {
  list(val = 10 * slot(slot(x, "AET"), "Day")[st$index.usedy, 3])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_SWE_mo <- compiler::cmpfun(function(x, st) {
  list(val = 10 * slot(slot(x, "SNOWPACK"), "Month")[st$index.usemo, 3])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_SWE_dy <- compiler::cmpfun(function(x, st) {
  list(val = 10 * slot(slot(x, "SNOWPACK"), "Day")[st$index.usedy, 3])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_Inf_yr <- compiler::cmpfun(function(x, st) {
  list(inf = 10 * slot(slot(x, "SOILINFILT"), "Year")[st$index.useyr, 2])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_Inf_mo <- compiler::cmpfun(function(x, st) {
  list(inf = 10 * slot(slot(x, "SOILINFILT"), "Month")[st$index.usemo, 3])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_Inf_dy <- compiler::cmpfun(function(x, st) {
  list(inf = 10 * slot(slot(x, "SOILINFILT"), "Day")[st$index.usedy, 3])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_Esurface_yr <- compiler::cmpfun(function(x, st) {
  x <- 10 * slot(slot(x, "EVAPSURFACE"), "Year")[st$index.useyr, ]
  list(sum = x[, 2],
       veg = rowSums(x[, 3:6]),
       litter = x[, 7],
       surfacewater = x[, 8])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_Esurface_dy <- compiler::cmpfun(function(x, st) {
  x <- 10 * slot(slot(x, "EVAPSURFACE"), "Day")[st$index.usedy, ]
  list(sum = x[, 3],
       veg = rowSums(x[, 4:7]),
       litter = x[, 8],
       surfacewater = x[, 9])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_Interception_yr <- compiler::cmpfun(function(x, st) {
  x <- 10 * slot(slot(x, "INTERCEPTION"), "Year")[st$index.useyr, ]
  list(sum = x[, 2],
       veg = rowSums(x[, 3:6]),
       litter = x[, 7])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_DeepDrain_yr <- compiler::cmpfun(function(x, st) {
  list(val = 10 * slot(slot(x, "DEEPSWC"), "Year")[st$index.useyr, 2])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_DeepDrain_mo <- compiler::cmpfun(function(x, st) {
  list(val = 10 * slot(slot(x, "DEEPSWC"), "Month")[st$index.usemo, 3])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_DeepDrain_dy <- compiler::cmpfun(function(x, st) {
  list(val = 10 * slot(slot(x, "DEEPSWC"), "Day")[st$index.usedy, 3])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_Runoff_mo <- compiler::cmpfun(function(x, st) {
  x <- 10 * slot(slot(x, "RUNOFF"), "Month")[st$index.usemo, ]
  list(val = x[, 3],
       ponded = x[, 4],
       snowmelt = x[, 5])
})

#' @paramInherits swOutput_access
#' @rdname swOutput_access
get_Runoff_yr <- compiler::cmpfun(function(x, st) {
  x <- 10 * slot(slot(x, "RUNOFF"), "Year")[st$index.useyr, ]
  list(val = x[, 2],
       ponded = x[, 3],
       snowmelt = x[, 4])
})
