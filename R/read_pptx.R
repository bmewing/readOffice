#' Read data from a Modern PowerPoint File
#'
#' @param pptx The .pptx file to read
#' @return Vector of text, broken by paragraph
#' @details
#' Only accepts one file at a time and only .pptx files.  Modifying file extensions will not work.
#'
#' @export
read_pptx = function(pptx){
  ext = rev(strsplit(pptx,"\\.")[[1]])[1]
  if(ext != "pptx") stop("Only pptx file formats are supported.")

  td = tempfile(pattern = "readOffice")
  dir.create(td)
  np = tempfile(tmpdir = td,fileext=".zip")
  file.copy(pptx,np,T)
  utils::unzip(np,exdir=td)
  slides = list.files(file.path(td,"ppt","slides"),pattern = ".xml",full.names = T)
  nnum = nchar(gsub("[\\.a-zA-Z]","",slides))
  extran = max(nnum)-nnum
  for(i in seq_along(slides)){
    if(extran[i] > 0) file.rename(slides[i],gsub("(.*?slide)([0-9]+\\..*)",paste0("\\1",rep("0",extran[i]),"\\2"),slides[i]))
  }
  slides = list.files(file.path(td,"ppt","slides"),pattern = ".xml",full.names = T)

  output = purrr::map(slides,function(x){
    fc = xml2::read_xml(x)
    on = rvest::xml_nodes(fc,"a\\:t")
    ot = xml2::xml_text(on)
    return(ot)
  })

  unlink(td, recursive=TRUE)
  return(output)
}
