# — POSITIVE MATCHING INDEX - #


PMI <- function(a, b, c, dropReq8 = TRUE) {
        # Negative values are invalid. 
        stopifnot(all(c(a, b, c) >= 0)) 
        # Stop if both lists show no positive entry 
        if (!dropReq8) stopifnot((a + b >0) && (a + c > 0)) 
        # No positive match occurs. 
        if (a == 0) return(0) 
        # Trivial case can be evaluated 
        # if Requirement 8 would be dropped out.
        # Equation (1). 
        if (b == c) return(a/(a + b)) 
        # Equation (2). 
        return(a/abs(b - c)* log((a + max(b, c))/(a + min(b, c))))
}


