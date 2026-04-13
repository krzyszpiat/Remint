printReport <- function(report, reportFormat = "html", showReport = 1){
  
  if (report == 1) {
    
    if (reportFormat == "pdf"){
      
      rmarkdown::render("raport.Rmd", "pdf_document")
      if (showReport == 1) {
        file.show("raport.pdf")
      }
      
    }else{
      
      rmarkdown::render("raport.Rmd", "html_document")
      if (showReport == 1) {
        file.show("raport.html")
      }
      
    }
    
    
    
  }
  
}