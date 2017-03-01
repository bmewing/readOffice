#' @importFrom magrittr "%>%"

makeNumeric = function(x){
  o = if(length(x) != 0) as.numeric(x) else NA
  return(o)
}

processSlide = function(xml){
  fc = xml2::read_xml(xml)
  blocks = rvest::xml_nodes(fc,"p\\:sp")
  blockNames = blocks %>%
    rvest::xml_node("p\\:cNvPr") %>%
    rvest::html_attr("name")
  blockContent = purrr::map(blocks,rvest::xml_nodes,css="a\\:p")
  bulleted = purrr::map(seq_along(blockContent),function(x){
    output = if(grepl("^Title",blockNames[x])) {
      purrr::map(blockContent[[x]],function(y){
        bullet = y %>%
          rvest::xml_nodes("a\\:buChar")
        return(length(bullet) != 0)
      }) %>% unlist()
    } else {
      purrr::map(blockContent[[x]],function(y){
        bullet = y %>%
          rvest::xml_nodes("a\\:buNone")
        return(length(bullet) == 0)
      }) %>% unlist()
    }
    return(output)
  })

  lvl = purrr::map(seq_along(blockContent),function(x){
    output = if(grepl("^Title|^Subtitle",blockNames[x])) {
      if(bulleted[[x]]) return(0) else return(NA)
    } else {
      purrr::map(blockContent[[x]],function(y){
        bullet = y %>%
          rvest::xml_nodes("a\\:pPr") %>%
          rvest::html_attr("lvl") %>%
          readOffice:::makeNumeric()
        return(bullet)
      }) %>% unlist()
    }
    return(output)
  })

  text = purrr::map(blockContent,function(x){
    purrr::map(x,function(y){
      y %>%
        rvest::xml_nodes("a\\:r") %>%
        xml2::xml_text() %>%
        paste(collapse="")
    }) %>% unlist()
  })

  output = purrr::map(seq_along(blockContent),function(x){
    nlvl = ifelse(bulleted[[x]] & is.na(lvl[[x]]),0,lvl[[x]])
    data.frame(Text = text[[x]],Bulleted = bulleted[[x]],Hierarchy = (bulleted[[x]]+nlvl),stringsAsFactors = F)
  })
  names(output) = blockNames

  return(output)
}
