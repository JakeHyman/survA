
####################################
### splits at dashes and slashes ###
####################################

#' Extract Piece of Date - yyyy, mm, dd
#'
#' @param date A POSIXct, Date, or string representing a date.
#' @param return_num The piece (separated by - or /) to return.
#'
#' @return A character string representing the selected piece of the date.
#'
#' @examples
#' return_split(date = "2021-11-30", return_num = 2)
#' @export
return_split <- function(date, return_num) {
  if(typeof(date) == "character"){
    piece <- strsplit(date, "-|/")[[1]][return_num]
    return(piece)
  } else{
    date = paste0(date)
    piece <- strsplit(date, "-|/")[[1]][return_num]
    return(piece)
  }
}


#######################################################
### breaks apart date and reformats into yyyy-mm-dd ###
#######################################################
#' Reformat date into format = 'yyyy-mm-dd HH:MM:SS'
#'
#' @param data A data frame.
#' @param startdate Start date variable name.
#' @param endate End date variable name.
#' @param date_format date format, options = \code{c("yyyy-mm-dd", "dd-mm-yyyy", "mm-dd-yyyy")}.
#' @param splits list of data variables to return along with startdate and endate.
#' @param event binary variable indicating if an individual did not survive (was not censured)
#'
#' @return a dataframe containing \code{startdate, endate, event, tenure, splits}.
#' \code{tenure} is the difference in time between startdate and endate.
#'
#'
#' @export
date_reformat <- function(data, startdate, endate, date_format, splits, event){

  data['time_startdate'] <- gsub(".+ ", "", data[[startdate]])
  data['time_startdate'] <- ifelse(data['time_startdate'] == data[[startdate]], "", data['time_startdate'])
  data['time_enddate'] <- gsub(".+ ", "", data[[endate]])
  data['time_enddate'] <- ifelse(data['time_enddate'] == data[[endate]], "", data['time_enddate'])
  data[[startdate]] <- gsub(" .+", "", as.character(data[[startdate]]))
  data[[endate]] <- gsub(" .+", "", as.character(data[[endate]]))

  if(date_format == "yyyy-mm-dd"){
    s_year <- lapply(data[[startdate]], return_split, return_num = 1)
    s_month <- lapply(data[[startdate]], return_split, return_num = 2)
    s_day <- lapply(data[[startdate]], return_split, return_num = 3)
    e_year <- lapply(data[[endate]], return_split, return_num = 1)
    e_month <- lapply(data[[endate]], return_split, return_num = 2)
    e_day <- lapply(data[[endate]], return_split, return_num = 3)

    data[[startdate]] <- paste(s_year, s_month, s_day, sep = "-")
    data[[startdate]] <- paste(data[[startdate]], data[['time_startdate']], sep = " ")

    data[[endate]] <- paste(e_year, e_month, e_day, sep = "-")
    data[[endate]] <- paste(data[[endate]], data[['time_enddate']], sep = " ")

  } else if (date_format == "mm-dd-yyyy"){
    s_year <- lapply(data[[startdate]], return_split, return_num = 3)
    s_month <- lapply(data[[startdate]], return_split, return_num = 1)
    s_day <- lapply(data[[startdate]], return_split, return_num = 2)
    e_year <- lapply(data[[endate]], return_split, return_num = 3)
    e_month <- lapply(data[[endate]], return_split, return_num = 1)
    e_day <- lapply(data[[endate]], return_split, return_num = 2)

    data[[startdate]] <- paste(s_year, s_month, s_day, sep = "-")
    data[[startdate]] <- paste(data[[startdate]], data[['time_startdate']], sep = " ")

    data[[endate]] <- paste(e_year, e_month, e_day, sep = "-")
    data[[endate]] <- paste(data[[endate]], data[['time_enddate']], sep = " ")
  } else if (date_format == "dd-mm-yyyy"){
    s_year <- lapply(data[[startdate]], return_split, return_num = 3)
    s_month <- lapply(data[[startdate]], return_split, return_num = 2)
    s_day <- lapply(data[[startdate]], return_split, return_num = 1)
    e_year <- lapply(data[[endate]], return_split, return_num = 3)
    e_month <- lapply(data[[endate]], return_split, return_num = 2)
    e_day <- lapply(data[[endate]], return_split, return_num = 1)

    data[[startdate]] <- paste(s_year, s_month, s_day, sep = "-")
    data[[startdate]] <- paste(data[[startdate]], data[['time_startdate']], sep = " ")

    data[[endate]] <- paste(e_year, e_month, e_day, sep = "-")
  }

  data[[startdate]] <- trimws(data[[startdate]])
  data[[endate]] <- trimws(data[[endate]])

  data <- dplyr::select(data, -c(time_startdate, time_enddate))

  data <- clean_date(data = data, startdate = startdate, enddate = endate)

  data[[startdate]] <- as.POSIXct(data[[startdate]], format = "%Y-%m-%d %H:%M:%S")
  data[[endate]] <- as.POSIXct(data[[endate]], format = "%Y-%m-%d %H:%M:%S")
  data['tenure'] <- as.numeric(difftime(data[[endate]], data[[startdate]], units = 'days'))
  data[[event]] <- as.numeric(data[[event]])


  if("none" %in% splits | is.null(splits)){
    data <- dplyr::select(data, c(startdate, endate, event, tenure))
  } else {
    data <- select(data, c(startdate, endate, event, tenure, splits))
  }
  return(data)
}
###############################################
#### adds time to dates that don't have time ###
###############################################

#' Add time to date variable
#'
#' @param data A data frame.
#' @param startdate Start date variable.
#' @param enddate End date variable.
#'
#' @return data.frame
#'
#'
#' @export
clean_date <- function(data, startdate, enddate) {

  if(max(nchar(data[[startdate]])) <= 10) {
    data[[startdate]] <- paste(data[[startdate]], '00:00:00', sep = " ")
  }

  if(max(nchar(data[[enddate]])) <= 10){
    data[[enddate]] <- paste(data[[enddate]], '00:00:00', sep = " ")
  }
  return(data)
}



####################################################
### uses Survival package to render plot of data ###
####################################################

#' Create KM curves: wrapper for \code{survival::survfit} and \code{survminer::ggsurvplot}
#'
#' @param data A data frame.
#' @param splits variables used to create km plot strata
#'
#' @return prints a km plot plot based on user entered strata
#'
#'
#' @export
create_SurvPlot <- function(data, splits){

  if("none" %in% splits | is.null(splits)){
    surv_fit <- eval(parse(text = paste0("survival::survfit(survival::Surv(time = data[, 4],event =  data[, 3]) ~ 1, data = data)")))
    gg <- survminer::ggsurvplot(eval(parse(text = paste0("survival::survfit(survival::Surv(time = data[, 4],event =  data[, 3]) ~ 1, data = data)"))),
                     data = data)$plot+
      scale_color_lt()
    plotly::ggplotly(gg)
  }
  else{
    surv_fit <- eval(parse(text = paste0("survival::survfit(survival::Surv(time = data[, 4],event =  data[, 3]) ~ ", paste(splits, collapse = "+"),", data = data)")))
    gg <- survminer::ggsurvplot(eval(parse(text = paste0("survival::survfit(survival::Surv(time = data[, 4],event =  data[, 3]) ~ ", paste(splits, collapse = "+"),", data = data)"))),
                     data = data)$plot+
      scale_color_lt()
    plotly::ggplotly(gg)
  }
}


#' Fit Cox Regression - wrapper for \code{survival::coxph()}
#'
#' @param data A data frame.
#' @param splits Variables to use in Cox regression
#'
#'
#'
#' @export
cox_fit <- function(data, splits){
  if("none" %in% splits | is.null(splits)){
    cx <- NULL
    return(cx)
  } else{
    model <- stats::as.formula(paste0("survival::Surv(data[, 4], data[, 3]) ~",
                               paste0(splits, collapse = "+")))
    cx <- survival::coxph(formula = model,
                          data = data)
    return(cx)
  }
}


#' Create Weibull Model and Plot of Data
#'
#' @details wrapper for \code{survminer::ggsurvplot()} and \code{survival::survreg()}
#' @param data A data frame
#'
#' @return Interactive plotly plot including KM curve of data and Weibull model of data
#'
#'
#' @export
create_model <- function(data){
  surv_fit <- eval(parse(text = paste0("survival::survfit(survival::Surv(time = data[, 4],event =  data[, 3]) ~ 1, data = data)")))
  wb_ <- eval(parse(text = paste0("survival::survreg(survival::Surv(data[, 4], data[, 3]) ~ 1, data = data)")))
  surv <- seq(0.99, 0.01, by = -0.01)
  t <- stats::predict(wb_, type = "quantile", surv = surv, p = 1 - surv, newdata = data.frame(1))
  surv_wb <- data.frame(time = t, surv = surv)

  gg <- survminer::ggsurvplot(eval(parse(text = paste0("survival::survfit(survival::Surv(time = data[, 4],event =  data[, 3]) ~ 1, data = data)"))),
                   data = data)$plot +
    ggplot2::geom_line(data = surv_wb, ggplot2::aes(x = as.numeric(time), y = as.numeric(surv)))+
    scale_color_lt()
  plotly::ggplotly(gg)
}







#####################
### color palette ###
#####################
lt_colors <- c(
  `Lending Tree Green` = rgb(8/320, 193/320, 119/320),
  `Deep Verde` = rgb(7/265, 129/265, 129/265),
  `Electric Salmon` = rgb(255/443, 112/443, 76/443),
  `OG Link Blue` = rgb(0/413, 174/413, 239/413),
  `Ocean Blue` = rgb(33/361, 135/361, 193/361),
  `Deep Azul` = rgb(16/95, 31/95, 48/95),
  `Fashion Fuschia` = rgb(217/350, 52/350, 81/350),
  `Twinkle Twinkle` = rgb(255/459, 197/459, 7/459),
  `Hopper Green (icons)` = rgb(0/337, 178/337, 159/337),
  `White` = "#FFFFFF",
  `Dark Gray` = "#333333",
  `Medium Gray` = "#999999",
  `Light Gray` = "#CCCCCC"
)


# returns the rgb for any searched color in lt_cols
#' Return Searched colors
#'
#' @param ... A list specifying the colors from lt_colors to return
#'
#' @return A list of colors to use with \code{ggplot}
#'
#'
#' @examples lt_cols("Twinkle Twinkle", "Electric Salmon", "Fashion Fuschia")
#' @export
lt_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return(lt_colors)

  lt_colors[cols]
}

# create LT color palettes
lt_palettes <- list(
  `cool`  = lt_cols("Lending Tree Green", "Hopper Green (icons)", "Deep Verde", "Ocean Blue", "OG Link Blue", "Deep Azul"),

  `warm`  = lt_cols("Twinkle Twinkle", "Electric Salmon", "Fashion Fuschia"),

  `main`   = lt_cols("Lending Tree Green", "Twinkle Twinkle", "Deep Verde", "Ocean Blue", "Deep Azul", "Fashion Fuschia", "Electric Salmon" ),

  `greens` = lt_cols("Lending Tree Green", "Hopper Green (icons)", "Deep Verde"),

  `blues`  = lt_cols("OG Link Blue", "Ocean Blue", "Deep Azul"),

  `grays`  = lt_cols("Dark Gray", "Medium Gray", "Light Gray"),

  `mix`    = lt_cols("Lending Tree Green", "Hopper Green (icons)", "Deep Verde",
                     "Ocean Blue", "OG Link Blue", "Deep Azul",
                     "Twinkle Twinkle", "Electric Salmon", "Fashion Fuschia",
                     "Dark Gray", "Medium Gray", "Light Gray"),

  `btg`    = lt_cols("Lending Tree Green", "Ocean Blue"),

  `gtb`    = lt_cols("Ocean Blue", "Lending Tree Green"),

  `gtr`    = lt_cols("Lending Tree Green", "Fashion Fuschia")
)


# creates a color ramp, defaults to using the palette "main"

#' Create a color ramp
#'
#' @param palette A pallet selected from lt_palettes which itself is composed of lt_colors
#' @param reverse Binary argument that reverses the palette order
#' @param ... selected color list
#'
#' @return \code{colorRampPalette()} of selected colors
#'
#'
#' @export
lt_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- lt_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}

#' Create a scale for \code{color}
#'
#' @param palette A pallete selected from lt_palettes
#' @param discrete A binary argument specifying if colored values are discrete
#' @param reverse A binary argyment that reverses the color palette
#' @param ... color list used to create gradient
#'
#' @return a color palette to be used in ggplot
#'
#'
#' @export
scale_color_lt <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- lt_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("lt_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Create a scale for \code{fill}
#'
#' @param palette A pallete selected from lt_palettes
#' @param discrete A binary argument specifying if colored values are discrete
#' @param reverse A binary argyment that reverses the color palette
#' @param ... color list used to create gradient
#'
#' @return a color palette to be used in ggplot
#'
#' @export
scale_fill_lt <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- lt_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("lt_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



