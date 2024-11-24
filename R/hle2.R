

outer_hle2 <- function(x, y) {
    outer_vec <- outer(y, x, "-")
    return(median(outer_vec))
}
