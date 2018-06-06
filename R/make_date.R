# create a date function to calculate date from Excel values
make.date <- function(x = 0) {
  # Excel z??hlt die Tage vom 0.0.1900 an, daher nehmen wir den 31.12.1899
  # au??erdem z??hlt Excel den 29.2.1900, den es nicht gibt.
  # Siehe https://support.microsoft.com/de-de/kb/214326
  as.Date("1899-12-31") + x
}
