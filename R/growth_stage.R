#' Determine Corn Growth Stage Based on Cumulative GDUs
#'
#' This function assigns the corn growth stage from a numeric vector of cumulative growing degree
#' units (GDU), using default thresholds based on widely used agronomic practices (e.g., McMaster & Wilhelm, 1997)
#' and the TidyAgronomy framework.
#'
#' The default stages are:
#' \describe{
#'   \item{Pre-VE}{cum_gdu < 100.}
#'   \item{VE}{100 ≤ cum_gdu < 300.}
#'   \item{V6}{300 ≤ cum_gdu < 900.}
#'   \item{VT}{900 ≤ cum_gdu < 1000.}
#'   \item{R1}{1000 ≤ cum_gdu < 2000.}
#'   \item{R6}{cum_gdu ≥ 2000.}
#' }
#'
#' @param cum_gdu A numeric vector of cumulative growing degree units.
#' @param thresholds A named numeric vector of thresholds. The defaults are
#'   \code{c(VE = 100, V6 = 300, VT = 900, R1 = 1000, R6 = 2000)}.
#'
#' @return A character vector indicating the corn growth stage.
#'
#' @references
#' McMaster, G. S., & Wilhelm, W. W. (1997). Growing degree-days: one equation, two interpretations.
#' \emph{Agricultural and Forest Meteorology, 87}(4), 291–300.
#'
#' @export
corn_growth_stage <- function(cum_gdu,
                              thresholds = c(VE = 100, V6 = 300, VT = 900, R1 = 1000, R6 = 2000)) {
  dplyr::case_when(
    cum_gdu < thresholds["VE"] ~ "Pre-VE",
    cum_gdu < thresholds["V6"] ~ "VE",
    cum_gdu < thresholds["VT"] ~ "V6",
    cum_gdu < thresholds["R1"] ~ "VT",
    cum_gdu < thresholds["R6"] ~ "R1",
    cum_gdu >= thresholds["R6"] ~ "R6",
    TRUE ~ NA_character_
  )
}

#' Determine Soybean Growth Stage Based on Cumulative GDUs
#'
#' This function returns soybean growth stages based on cumulative growing degree units (GDU). The default
#' thresholds provided here are illustrative; users should calibrate these based on local agronomic recommendations.
#'
#' The default stages are:
#' \describe{
#'   \item{Pre-Emergence}{cum_gdu < 50.}
#'   \item{VE}{50 ≤ cum_gdu < 200.}
#'   \item{V3}{200 ≤ cum_gdu < 500.}
#'   \item{R1}{500 ≤ cum_gdu < 700.}
#'   \item{R5}{700 ≤ cum_gdu < 900.}
#'   \item{R7}{cum_gdu ≥ 900.}
#' }
#'
#' @param cum_gdu A numeric vector of cumulative growing degree units.
#' @param thresholds A named numeric vector of thresholds. Defaults are
#'   \code{c(VE = 50, V3 = 200, R1 = 500, R5 = 700, R7 = 900)}.
#'
#' @return A character vector indicating the soybean growth stage.
#'
#' @export
soybean_growth_stage <- function(cum_gdu,
                                 thresholds = c(VE = 50, V3 = 200, R1 = 500, R5 = 700, R7 = 900)) {
  dplyr::case_when(
    cum_gdu < thresholds["VE"] ~ "Pre-Emergence",
    cum_gdu < thresholds["V3"] ~ "VE",
    cum_gdu < thresholds["R1"] ~ "V3",
    cum_gdu < thresholds["R5"] ~ "R1",
    cum_gdu < thresholds["R7"] ~ "R5",
    cum_gdu >= thresholds["R7"] ~ "R7",
    TRUE ~ NA_character_
  )
}

#' Estimate Wheat Growth Stage on the Haun Scale Based on Cumulative GDD (°F)
#'
#' This function estimates the wheat growth stage (as a Haun scale stage) from a numeric vector of cumulative
#' growing degree days (GDD in °F), following the NDAWN guidelines :contentReference[oaicite:4]{index=4}&#8203;:contentReference[oaicite:5]{index=5}. Temperatures below 32°F are
#' set to 32°F, and daily maximum temperatures are capped (70°F prior to Haun stage 2.0, 95°F after).
#'
#' The default cumulative GDD thresholds are:
#'
#' \describe{
#'   \item{Pre-Emergence}{cum_gdd < 180.}
#'   \item{0.5 (Emergence)}{180 ≤ cum_gdu < 252.}
#'   \item{1.0}{252 ≤ cum_gdu < 395.}
#'   \item{2.0}{395 ≤ cum_gdu < 538.}
#'   \item{3.0}{538 ≤ cum_gdu < 681.}
#'   \item{4.0}{681 ≤ cum_gdu < 824.}
#'   \item{5.0}{824 ≤ cum_gdu < 967.}
#'   \item{6.0}{967 ≤ cum_gdu < 1110.}
#'   \item{7.0}{1110 ≤ cum_gdu < 1181.}
#'   \item{7.5}{1181 ≤ cum_gdu < 1255.}
#'   \item{8.0}{1255 ≤ cum_gdu < 1396.}
#'   \item{9.0}{1396 ≤ cum_gdu < 1539.}
#'   \item{10.0}{1539 ≤ cum_gdu < 1567.}
#'   \item{10.2}{1567 ≤ cum_gdu < 1682.}
#'   \item{11.0}{1682 ≤ cum_gdu < 1739.}
#'   \item{11.4}{1739 ≤ cum_gdu < 1768.}
#'   \item{11.6}{1768 ≤ cum_gdu < 1825.}
#'   \item{12.0}{cum_gdu ≥ 1825.}
#' }
#'
#' @param cum_gdd A numeric vector of cumulative growing degree days (in °F).
#' @param thresholds A named numeric vector specifying the thresholds. The defaults are:
#'   \code{c(Emergence = 180, Stage1 = 252, Stage2 = 395, Stage3 = 538, Stage4 = 681,
#'           Stage5 = 824, Stage6 = 967, Stage7 = 1110, `Stage7.5` = 1181, Stage8 = 1255,
#'           Stage9 = 1396, Stage10 = 1539, `Stage10.2` = 1567, Stage11 = 1682,
#'           `Stage11.4` = 1739, `Stage11.6` = 1768, Stage12 = 1825)}.
#'
#' @return A character vector indicating the estimated wheat Haun stage.
#'
#' @examples
#' cum_gdd_wheat <- c(150, 230, 300, 450, 600, 800, 1000, 1200, 1400, 1600, 1700, 1800)
#' wheat_growth_stage(cum_gdd_wheat)
#'
#' @references
#' Bauer, A., Fanning, C., Enz, J. W., & Eberlein, C. V. (1984). Use of growing-degree days
#' to determine spring wheat growth stages. North Dakota Coop. Ext. Ser. EB-37.
#'
#' NDAWN Wheat Growing Degree Days Help. \url{https://ndawn.ndsu.nodak.edu/help-wheat-growing-degree-days.html} :contentReference[oaicite:6]{index=6}&#8203;:contentReference[oaicite:7]{index=7}
#'
#' @export
wheat_growth_stage <- function(cum_gdd, thresholds = c(
  Emergence   = 180,
  Stage1      = 252,
  Stage2      = 395,
  Stage3      = 538,
  Stage4      = 681,
  Stage5      = 824,
  Stage6      = 967,
  Stage7      = 1110,
  `Stage7.5`  = 1181,
  Stage8      = 1255,
  Stage9      = 1396,
  Stage10     = 1539,
  `Stage10.2` = 1567,
  Stage11     = 1682,
  `Stage11.4` = 1739,
  `Stage11.6` = 1768,
  Stage12     = 1825
)) {
  dplyr::case_when(
    cum_gdd < thresholds["Emergence"] ~ "Pre-Emergence",
    cum_gdd < thresholds["Stage1"] ~ "0.5",       # Emergence: 0.5 Haun stage
    cum_gdd < thresholds["Stage2"] ~ "1.0",
    cum_gdd < thresholds["Stage3"] ~ "2.0",
    cum_gdd < thresholds["Stage4"] ~ "3.0",
    cum_gdd < thresholds["Stage5"] ~ "4.0",
    cum_gdd < thresholds["Stage6"] ~ "5.0",
    cum_gdd < thresholds["Stage7"] ~ "6.0",
    cum_gdd < thresholds["Stage7.5"] ~ "7.0",
    cum_gdd < thresholds["Stage8"] ~ "7.5",
    cum_gdd < thresholds["Stage9"] ~ "8.0",
    cum_gdd < thresholds["Stage10"] ~ "9.0",
    cum_gdd < thresholds["Stage10.2"] ~ "10.0",
    cum_gdd < thresholds["Stage11"] ~ "10.2",
    cum_gdd < thresholds["Stage11.4"] ~ "11.0",
    cum_gdd < thresholds["Stage11.6"] ~ "11.4",
    cum_gdd < thresholds["Stage12"] ~ "11.6",
    cum_gdd >= thresholds["Stage12"] ~ "12.0",
    TRUE ~ NA_character_
  )
}

#' Estimate Canola Growth Stage Based on Cumulative GDD (°F)
#'
#' This function estimates the canola growth stage from a numeric vector of cumulative growing degree days (GDD in °F),
#' using the guidelines provided by NDAWN Canola Growing Degree Days Help :contentReference[oaicite:8]{index=8}&#8203;:contentReference[oaicite:9]{index=9}. For canola, the base temperature
#' is 41°F. If the daily maximum and/or minimum temperature is below 41°F it is set to 41°F, and no upper limit is applied.
#'
#' The default cumulative GDD ranges (in °F) and corresponding stages are:
#'
#' \describe{
#'   \item{Planting}{cum_gdd = 0.}
#'   \item{Seedling}{0 < cum_gdd < 142.}
#'   \item{Rosette - 3rd Leaf}{142 ≤ cum_gdd < 220.}
#'   \item{Rosette - 4th Leaf}{220 ≤ cum_gdd < 404.}
#'   \item{Early Bud}{404 ≤ cum_gdd < 460.}
#'   \item{Late Bud}{460 ≤ cum_gdd < 518.}
#'   \item{Early Flower}{518 ≤ cum_gdd < 647.}
#'   \item{Late Flower}{647 ≤ cum_gdd < 776.}
#'   \item{Early Ripening}{776 ≤ cum_gdd < 908.}
#'   \item{Late Ripening}{908 ≤ cum_gdd < 1041.}
#'   \item{Ripe}{cum_gdd ≥ 1041.}
#' }
#'
#' @param cum_gdd A numeric vector of cumulative growing degree days (in °F).
#' @param thresholds A named numeric vector specifying the cumulative GDD upper bounds for each stage.
#'   The defaults are:
#'   \code{c(Seedling = 142, Rosette3 = 220, Rosette4 = 404, EarlyBud = 460,
#'           LateBud = 518, EarlyFlower = 647, LateFlower = 776, EarlyRipening = 908,
#'           LateRipening = 1041)}.
#'
#' @return A character vector indicating the estimated canola growth stage.
#'
#' @examples
#' cum_gdd_canola <- c(50, 150, 300, 450, 500, 600, 750, 900, 1100)
#' canola_growth_stage(cum_gdd_canola)
#'
#' @references
#' NDAWN Canola Growing Degree Days Help. \url{https://ndawn.ndsu.nodak.edu/help-canola-growing-degree-days.html} :contentReference[oaicite:10]{index=10}&#8203;:contentReference[oaicite:11]{index=11}
#'
#' @export
canola_growth_stage <- function(cum_gdd, thresholds = c(
  Seedling      = 142,
  Rosette3      = 220,
  Rosette4      = 404,
  EarlyBud      = 460,
  LateBud       = 518,
  EarlyFlower   = 647,
  LateFlower    = 776,
  EarlyRipening = 908,
  LateRipening  = 1041
)) {
  dplyr::case_when(
    cum_gdd <= 0 ~ "Planting",
    cum_gdd < thresholds["Seedling"] ~ "Seedling",
    cum_gdd < thresholds["Rosette3"] ~ "Rosette - 3rd Leaf",
    cum_gdd < thresholds["Rosette4"] ~ "Rosette - 4th Leaf",
    cum_gdd < thresholds["EarlyBud"] ~ "Early Bud",
    cum_gdd < thresholds["LateBud"] ~ "Late Bud",
    cum_gdd < thresholds["EarlyFlower"] ~ "Early Flower",
    cum_gdd < thresholds["LateFlower"] ~ "Late Flower",
    cum_gdd < thresholds["EarlyRipening"] ~ "Early Ripening",
    cum_gdd < thresholds["LateRipening"] ~ "Late Ripening",
    cum_gdd >= thresholds["LateRipening"] ~ "Ripe",
    TRUE ~ NA_character_
  )
}

#' Determine Cotton Growth Stage Based on Cumulative GDUs
#'
#' This function returns cotton growth stages based on cumulative growing degree units (GDU). The default
#' thresholds provided here are illustrative and should be adjusted based on local growing conditions.
#'
#' The default stages are:
#' \describe{
#'   \item{Germination}{cum_gdu < 100.}
#'   \item{Seedling}{100 ≤ cum_gdu < 250.}
#'   \item{Squaring}{250 ≤ cum_gdu < 500.}
#'   \item{Flowering}{500 ≤ cum_gdu < 800.}
#'   \item{Boll Formation}{800 ≤ cum_gdu < 1200.}
#'   \item{Maturity}{cum_gdu ≥ 1200.}
#' }
#'
#' @param cum_gdu A numeric vector of cumulative growing degree units.
#' @param thresholds A named numeric vector of thresholds. Defaults are
#'   \code{c(Seedling = 100, Squaring = 250, Flowering = 500, Boll = 800, Maturity = 1200)}.
#'
#' @return A character vector indicating the cotton growth stage.
#'
#' @export
cotton_growth_stage <- function(cum_gdu,
                                thresholds = c(Seedling = 100, Squaring = 250, Flowering = 500, Boll = 800, Maturity = 1200)) {
  dplyr::case_when(
    cum_gdu < thresholds["Seedling"] ~ "Germination",
    cum_gdu < thresholds["Squaring"] ~ "Seedling",
    cum_gdu < thresholds["Flowering"] ~ "Squaring",
    cum_gdu < thresholds["Boll"] ~ "Flowering",
    cum_gdu < thresholds["Maturity"] ~ "Boll Formation",
    cum_gdu >= thresholds["Maturity"] ~ "Maturity",
    TRUE ~ NA_character_
  )
}
