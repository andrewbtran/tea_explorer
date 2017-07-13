library(tm)
library(pdftools)
library(tesseract)
library(stringr)
library(readr)
library(tidyverse)

sct <- read_csv("states_counties_tracts.csv")
sct$t_only <- gsub("Census Tract ", "", sct$tract_name)
fl <- filter(sct, abb=="FL")

county_names <- data.frame(unique(fl$county_name))
colnames(county_names) <- "names"

fl_list <- list.files("pdf/FL")
rm(state_tea_all)

for (x in 1:length(fl_list)) {
  txt <- paste0("pdf/FL/", fl_list[x])
  
  pages <- pdf_info(txt)
  pages <- pages$pages
  
  for (i in 1:pages) {
  bitmap <- pdf_render_page(txt, page=i, dpi = 300, numeric = TRUE)
  tiff::writeTIFF(bitmap, "page.tiff")
  out <- ocr("page.tiff")
  
    if (grepl("Sincerely", out)) {
      state_abbr <- "FL"
      state_fips <- "12"
      county_solo <- gsub("County, Florida.*", "", out)
      county_solo <- paste0(county_solo, "County")
      #county_solo <- gsub(".*Florida, in ", "", out)
      #county_solo <- gsub(", Florida.*", "", county_solo)
      
      county_solo <- str_extract(county_solo, "[^ ]+ [^ ]+$")
      
      county_solo <- gsub(", Florida.*", "", county_solo)
      
      county_solo <- filter(county_names, str_detect(names, county_solo))
      county_solo <- as.character(county_solo$names)
      
      sct_county <- filter(sct, county_name==county_solo & abb==state_abbr)
      county_fips <- unique(sct_county$county)
      
      if (grepl(" has veriﬁed that ", out)) {
        project_address <- gsub(".*has veriﬁed that ", "", out)
        project_address <- gsub(" in .*", "", project_address)
        project_address <- gsub("\n", " ", project_address)
      } else if (grepl(" has verified that ", out)) {
        project_address <- gsub(".*has verified that ", "", out)
        project_address <- gsub(" in .*", "", project_address)
        project_address <- gsub("\n", " ", project_address)
      } else {
        project_address <- "NONE"
      }
      
      ifelse()
      
      body_text <- gsub(".*in Census tract ", "", out)
      body_text <- gsub("qualifies as .*", "", body_text)
      
      tracts <- gsub("[a-zA-Z]", "", body_text)
      tracts <- gsub("\n", " ", tracts)
      tracts <- gsub(", ,", ",", tracts)
      tracts <- gsub("\\. ", ", ", tracts)
      tracts <- unlist(strsplit(tracts, ","))
      tracts <- str_trim(tracts)
      tracts <-unique(tracts)
      tracts <- tracts[tracts!=""]
      #tracts <- gsub("\\.", "", tracts)
      
      tracts_df <- filter(sct, abb==state_abbr & county_name==county_solo & t_only %in% tracts)
      diff <- length(tracts) - nrow(tracts_df)
      #tracts <- ifelse(nchar(tracts)==5, paste0("0", tracts), tracts)
      #tracts <- paste0(state_fips, county_fips, tracts)
    
      #UH OH
      #}
  
    tracts <- paste(tracts_df$tract_full, collapse=", ")
    
    if (grepl("2014", out)) {
      date <- "2014"
    } else if (grepl("2015", out)) {
      date <- "2015"
    } else if (grepl("2016", out)) {
      date <- "2016"
    } else if (grepl("2017", out)) {
      date <- "2017"
    } else {
      date <- "NA"
    }
    
    state_tea <- data.frame(state_abbr, project_address, date, tracts, diff)
    
    if (!exists("state_tea_all")) {
      state_tea_all <- state_tea
    } else {
      state_tea_all <- rbind(state_tea_all, state_tea)
      write.csv(state_tea_all, "all.csv")
    }
    }
    print(paste(x, i, "out of", length(fl_list)))
  }
}
