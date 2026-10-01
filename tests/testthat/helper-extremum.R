na_wins <- function(out, x, y) {
  is_na <- \(x) is.na(x) & !is.nan(x)
  out[is.na(x) & is.na(y) & (is_na(x) | is_na(y))] <- NA
  out
}
