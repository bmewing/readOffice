#' Read data from a Modern Word File
#'
#' @param docx The .docx file to read
#' @return Vector of document text
#' @details
#' Only accepts one file at a time and only .docx files.  Modifying file extensions will not work.
#'
#' Text is broken out into the XML defined paragraphs in the vector.
#'
#' @examples
#' read_docx(docx = system.file('extdata','example.docx',package='readOffice'))
#'
#' @export
read_docx = function(docx){
  ext = rev(strsplit(docx,"\\.")[[1]])[1]
  if(ext == "doc") warning("The tm package has support for reading in .doc files.")
  if(ext != "docx") stop("Only .docx file formats are supported.")

  td = tempfile(pattern = "readOffice")
  np = tempfile(fileext=".zip")
  file.copy(docx,np,T)
  utils::unzip(np,exdir=td)
  fc = xml2::read_xml(file.path(td,"word","document.xml"))
  fc = xml2::read_xml(gsub("<w:br/>","&#160;",as.character(fc)))
  on = rvest::xml_nodes(fc,"w\\:p")
  ot = xml2::xml_text(on)
  ot = gsub("[\n ]+"," ",ot)
  ot = gsub("[\n ]+$|^[\n ]+","",ot)
  unlink(td, recursive=TRUE)
  return(ot)
}
