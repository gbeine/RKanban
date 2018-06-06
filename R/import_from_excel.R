# Import proper data from excel, even if the sheet uses different data types
import.from.excel <- function(dir, file, sheet) {
  wd <- getwd()
  setwd(dir)
  import <- readxl::read_excel(file, sheet = sheet)
  features <- ncol(import)
  import <- readxl::read_excel(file, sheet = sheet, col_types = rep("text", features))
  import[is.na(import)] <- ""
  setwd(wd)
  return(import)
}
