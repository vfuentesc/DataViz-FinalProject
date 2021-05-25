get_enaho <- function(year, module_code) {
  
  yr      <- 2004:2020
  code    <- c(280,281,282,283,284,285,279,291,324,404,440,498,546,603,634,687,737)
  yr_code <- which(yr == year)
  
  mdl     <- c("01", "02", "34")
  prefixx <- c("enaho01", "enaho01", "sumaria")
  suffixx <- c("100", "200", "")
  mdl_code <- which(mdl == module_code)
  
  survey_code <- code[yr_code]
  url_base    <- "http://iinei.inei.gob.pe/iinei/srienaho/descarga/SPSS/"
  url_full    <- paste0(url_base, survey_code, "-modulo", module_code, ".zip")
  # folder      <- paste0(code[yr_code], "-Modulo", module_code)
  file_name   <- ifelse(module_code == "34",
                        paste0(prefixx[mdl_code], "-", year, ".sav"),
                        paste0(prefixx[mdl_code], "-", year, "-", suffixx[mdl_code], ".sav"))
                
  if (file.exists(file.path(getwd(), file_name))){
    cat("File ", file_name, " founded on current directory\n")
  } else{
    temp <- tempfile()
    download.file(url_full, temp, quite = T)
    list_file <- unzip(temp, list = TRUE)$Name
    file_searched<- sapply(1:length(list_file), function(x) grepl(tolower(file_name), tolower(list_file[x])))
    file_located = list_file[which(file_searched == T)]
    utils::unzip(temp, file_located, junkpaths = T)
    unlink(temp)
  }

}