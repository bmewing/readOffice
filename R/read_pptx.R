#' Read data from a Modern PowerPoint File
#'
#' @param pptx The .pptx file to read
#' @param tables Should tables be processed from the document?
#' @param drawings Should drawings be processed from the document?
#' @param diagrams Should diagrams be processed from the document?
#' @return List containing slide elements.
#'
#' @details
#' Only accepts one file at a time and only .pptx files.  Modifying file extensions will not work.
#'
#' The returned list contains named lists of the elements on the slide, each element of which is either a data.frame or a matrix containing the text and minor details about the structure on the page.
#'
#' Data frames will contain the text in addition to the following columns:
#' "Bulleted" indicates if the text is part of a bulleted or numbered list on the slide.
#' "Hierarchy" indicates the tabbed depth of the element in a bulleted or numbered list (NA if not bulleted).
#'
#' Alternatively, returns a matrix for tables on the slide.
#'
#' @examples
#' read_pptx(system.file('extdata','example.pptx',package='readOffice'))
#'
#' @export
read_pptx = function(pptx,tables = T,drawings = T,diagrams = T){
  ext = rev(strsplit(pptx,"\\.")[[1]])[1]
  if(ext != "pptx") stop("Only pptx file formats are supported.")

  td = tempfile(pattern = "readOffice")
  dir.create(td)
  np = tempfile(tmpdir = td,fileext=".zip")
  file.copy(pptx,np,T)
  utils::unzip(np,exdir=td)
  slides = list.files(file.path(td,"ppt","slides"),pattern = "\\.xml",full.names = T)
  if(length(slides) > 9){
    nnum = nchar(gsub(".*?slide([0-9]+)\\.xml","",slides))
    extran = max(nnum)-nnum
    for(i in seq_along(slides)){
      if(extran[i] > 0) file.rename(slides[i],gsub("(.*?slide)([0-9]+\\..*)",paste0("\\1",rep("0",extran[i]),"\\2"),slides[i]))
    }
    slides = list.files(file.path(td,"ppt","slides"),pattern = ".xml",full.names = T)
  }

  d = c()
  if(diagrams){
    reset = processDiagram_PPTX(d=NULL,r=TRUE)
    d = list.files(file.path(td,"ppt","diagrams"),pattern = "data[0-9]+\\.xml",full.names = T)
    if(length(d) > 9){
      nnum = nchar(gsub(".*?data([0-9]+)\\.xml","\\1",d))
      extran = max(nnum)-nnum
      for(i in seq_along(slides)){
        if(extran[i] > 0) file.rename(slides[i],gsub("(.*?data)([0-9]+\\..*)",paste0("\\1",rep("0",extran[i]),"\\2"),slides[i]))
      }
      d = list.files(file.path(td,"ppt","diagrams"),pattern = ".xml",full.names = T)
    }
  }

  output = purrr::map(slides,processSlide,tbl=tables,dgrm=diagrams,dlist=d)

  unlink(td, recursive=TRUE)
  return(output)
}
