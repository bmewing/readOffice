#' Read data from a Modern Word File
#'
#' @param docx The .docx file to read
#' @param tables Should tables be processed from the document?
#' @param drawings Should drawings be processed from the document?
#' @param diagrams Should diagrams be processed from the document?
#' @return Named list with document contents
#' @details
#' Only accepts one file at a time and only .docx files.  Modifying file extensions will not work.
#'
#' List is comprised of named elements, one per 'section' (sections are recognized after a page break).  If tables exist in the document and are processed, then the named list elements will be lists containing the text of paragraphs, drawings (if present and processed) and matrices holding the table structure.  Otherwise, the list elements will contain vectors of the text processed.
#'
#' Diagrams are typically what Microsoft calls 'SmartArt'
#'
#' @examples
#' read_docx(docx = system.file('extdata','example.docx',package='readOffice'))
#' read_docx(docx = system.file('extdata','example.docx',package='readOffice'),diagrams=FALSE)
#'
#' @export
read_docx = function(docx,tables = T,drawings = T,diagrams = T){
  ext = rev(strsplit(docx,"\\.")[[1]])[1]
  if(ext == "doc") warning("The tm package has support for reading in .doc files.")
  if(ext != "docx") stop("Only .docx file formats are supported.")

  output = list()

  td = tempfile(pattern = "readOffice")
  np = tempfile(fileext=".zip")
  file.copy(docx,np,T)
  utils::unzip(np,exdir=td)
  fc = xml2::read_xml(file.path(td,"word","document.xml"))
  fc = xml2::read_xml(gsub("<w:br/>","&#160;",as.character(fc)))

  nodes = rvest::xml_nodes(rvest::xml_node(fc,"w\\:body"),xpath='child::*')
  types = unlist(purrr::map(nodes,docxNodeType))
  drop = c("emp","drw"[!drawings],"tbl"[!tables])
  nodes[types %in% drop] = NULL
  types = types[!(types %in% drop)]

  page = 1
  item = 1
  output$`Section 1` = list()
  for(i in seq_along(nodes)){
    if(types[i] == "pbr"){
      page = page+1
      `[[`(output,paste0("Section ",page)) = list()
      item = 1
      next
    }
    f = switch(types[i],
        txt = processParagraph,
        tbl = purrr::partial(processTable,type='docx'),
        drw = processDrawing
        )
    `[[`(output,paste0("Section ",page))[[item]] = f(nodes[i])
    item = item+1
  }

  if(!("tbl" %in% types)) output = purrr::map(output,unlist)

  if(diagrams){
    d = list.files(file.path(td,"word","diagrams"),pattern = "data[0-9]+\\.xml",full.names = T)
    if(length(d) > 9){
      nnum = nchar(gsub(".*?data([0-9]+)\\.xml","\\1",d))
      extran = max(nnum)-nnum
      for(i in seq_along(d)){
        if(extran[i] > 0) file.rename(d[i],gsub("(.*?data)([0-9]+\\..*)",paste0("\\1",rep("0",extran[i]),"\\2"),d[i]))
      }
      d = list.files(file.path(td,"word","diagrams"),pattern = ".xml",full.names = T)
    }
    for(i in seq_along(d)){
      `[[`(output,paste0("Diagram ",i)) = processDiagram_DOCX(d[i])
    }
  }

  unlink(td, recursive=TRUE)
  return(output)
}
