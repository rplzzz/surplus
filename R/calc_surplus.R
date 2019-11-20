#' Calculate the difference in total economic surplus
#'
#' Calculate the difference in total economic suplus for a control scenario and for an
#' experimental scenario.  Data is assumed to be for a single time step.
#'
#' The tricky thing in this calculation is that the control and experimental
#' conditions may not have data up to the same price level.  We compute the
#' difference over the range of overlapping prices between the two scenarios.
#'
#' @param cprice Control prices
#' @param csup Control supply quantity
#' @param cdmnd Control demand quantity
#' @param eprice Experimental prices
#' @param esup Experimental supply quantity
#' @param edmnd Experimental demand quantity
#' @return Difference in total economic surplus:  Experimental - Control
#' @export
calc_net_surplus <- function(cprice, csup, cdmnd, eprice, esup, edmnd)
{

    assertthat::assert_that(length(cprice)==length(cdmnd))
    assertthat::assert_that(length(eprice)==length(edmnd))
    assertthat::assert_that(length(cprice)==length(csup))
    assertthat::assert_that(length(eprice)==length(esup))

    ## Envelope is the min of supply and demand quantities
    cqty <- pmin(csup, cdmnd)
    eqty <- pmin(esup, edmnd)

    ## Establish the price bounds
    p0 <- pmax(min(cprice), min(eprice))  # higher of the two lower bounds
    p1 <- pmin(max(cprice), max(eprice))  # lower of the two upper bounds
    message('Valid price range: ', p0, ' -- ', p1)

    ## Constuct a piecewise linear interpolating function
    fc <- stats::approxfun(cprice, cqty, rule=2)
    fe <- stats::approxfun(eprice, eqty, rule=2)

    csurplus <- integrate(fc, p0, p1)
    esurplus <- integrate(fe, p0, p1)

    esurplus$value - csurplus$value
}

