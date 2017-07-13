library(stringrs)
library(tm)
library(pdftools)
library(tesseract)

txt <- "pdf/FL/2017 Batch 5.pdf"

pages <- pdf_info(txt)
pages <- pages$pages

for (i in 1:pages) {
bitmap <- pdf_render_page(txt, page=1, dpi = 300, numeric = TRUE)
tiff::writeTIFF(bitmap, "page.tiff")
out <- ocr("page.tiff")
}