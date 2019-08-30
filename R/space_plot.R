#'
#' the Space plot
#'
#' Create variable specific spatial heatmaps to show data availability and data summary over the geographical study area.
#'
#' @param entity_df (data.frame) A data.frame containing entity-level data. The "data" child element of an entity-level list object in metajam output format.
#' @param varname Name of variable to make data availability and summary. Remember to enclose in quotes.
#' @param space_cols A character vector whose two elements are named "y_col" and "x_col". Outpur from \code{\link{space_detective}}
#'
#' @return For all variables, function returns a data availability plot over the study area. For categorical variables, function returns an extra plot showing most prevalent level of that variable in each spatial grid cell over the study area. For numeric variables, function returns an extra plot showing mean of that variable in each spatial grid cell over the study area. y_dim and lon are binned; there are 50 bins normalized to attempt to enforce equal scales along each dimension.
#'
#' @export

space_plot <-
  function (entity_df, varname, space_cols = space_cols) {
    if (is.vector(space_cols) & length(space_cols) == 2) {
      var <- entity_df[[varname]]
      x_col <- entity_df[[space_cols[["x_col"]]]]
      y_col <- entity_df[[space_cols[["y_col"]]]]
      
      xrange <-
        max(x_col, na.rm = T) - min(x_col, na.rm = T)
      yrange <-
        max(y_col, na.rm = T) - min(y_col, na.rm = T)
      
      # only proceed if lon y_dim values aren't all identical
      if (xrange > 0 & yrange > 0) {
        # divide 50 bins among
        xbreak <- round(50 / (xrange + yrange) * xrange)
        ybreak <- round(50 / (xrange + yrange) * yrange)
        
        # account for really narrow, transect-like study extents
        
        if (xbreak == 1) {
          ybreak <- 20
          agg_by <- list(y_dim = cut(y_col, breaks = ybreak)) 
          
        } else if (ybreak == 1) {
          xbreak <- 20
          agg_by <- list(x_dim = cut(x_col, breaks = xbreak))

        } else {
          agg_by <- list(
            y_dim = cut(y_col, breaks = ybreak),
            x_dim = cut(x_col, breaks = xbreak)
          )
        }
        
        non_na <-
          aggregate(
            var,
            by = agg_by,
            FUN = function(x) {
              sum(!is.na(x))
            }
          )
        
        if (xbreak == 1){
          non_na$x_dim <- as.factor(paste0("(", min(x_col, na.rm = T), ",", max(x_col, na.rm = T), "]"))
        } 
        if (ybreak == 1){
          non_na$y_dim <- as.factor(paste0("(", min(y_col, na.rm = T), ",", max(y_col, na.rm = T), "]"))
        }

        non_na_plot <-
          ggplot(non_na, aes(x = as.factor(x_dim),  y = as.factor(y_dim), fill = x)) +
          geom_tile(colour = "white", size = 0.25) +
          scale_fill_gradient(low = "blue",
                              high = "red",
                              na.value = "transparent") +
          coord_fixed(ratio = 1) +
          labs(
            title = paste0(
              "How much data are there in variable \"",
              varname,
              "\" over the study area?"
            ),
            x = "x dimension (binned)",
            y = "y dimension (binned)"
          ) +
          guides(fill = guide_legend(paste0(
            "Count of non-NAs in \n variable \"", varname, "\""
          )))

        if (is.numeric(var)) {
          avg <-
            aggregate(
              var,
              by = agg_by,
              FUN = function(x) {
                mean(x, na.rm = T)
              }
            )
          
          if (xbreak == 1){
            avg$x_dim <- as.factor(paste0("(", min(x_col, na.rm = T), ",", max(x_col, na.rm = T), "]"))
          } 
          if (ybreak == 1){
            avg$y_dim <- as.factor(paste0("(", min(y_col, na.rm = T), ",", max(y_col, na.rm = T), "]"))
          }
          avg_plot <-
            ggplot(avg, aes(
              x = as.factor(x_dim),
              y = as.factor(y_dim),
              fill = x
            )) +
            geom_tile(colour = "white", size = 0.25) +
            scale_fill_gradient(low = "blue",
                                high = "red",
                                na.value = "transparent") +
            coord_fixed(ratio = 1) +
            labs(
              title = paste0(
                "What is the average of numeric variable \"",
                varname,
                "\" over the study area?"
              ),
              x = "x dimension (binned)",
              y = "y dimension (binned)"
            ) +
            guides(fill = guide_legend(paste0(
              "Mean of \n variable \"", varname, "\""
            )))
          
          return(list(non_na_plot, avg_plot))
          
        } else if (is.factor(var) | is.character(var)) {
          prev <-
            aggregate(
              var,
              by = agg_by,
              FUN = function(x) {
                names(which.max(table(x)))
              }
            )
          if (xbreak == 1){
            prev$x_dim <- as.factor(paste0("(", min(x_col, na.rm = T), ",", max(x_col, na.rm = T), "]"))
          } 
          if (ybreak == 1){
            prev$y_dim <- as.factor(paste0("(", min(y_col, na.rm = T), ",", max(y_col, na.rm = T), "]"))
          }
          
          
          prev_plot <-
            ggplot(prev, aes(
              x = as.factor(x_dim),
              y = as.factor(y_dim),
              fill = x
            )) +
            geom_tile(colour = "white", size = 0.25) +
            coord_fixed(ratio = 1) +
            labs(
              title = paste0(
                "What is the most common level in categorical variable \"",
                varname,
                "\" over the study area?"
              ),
              x = "x dimension (binned)",
              y = "y dimension (binned)"
            ) +
            guides(fill = guide_legend(paste0(
              "Level in \n variable \"", varname, "\""
            )))
          return(list(non_na_plot, prev_plot))
          
        } else if (is.Date(var) |
                   is.POSIXct(var)) {
          return(list(non_na_plot, NULL))
        }
      } else if (xrange == 0 & yrange == 0) {
        message("Spatial range in both dimensions is zero. Spatial plots will not be generated.")
        return(NULL)
      }
    }
  }