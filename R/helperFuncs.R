makeNumeric = function(x){
  o = if(length(x) != 0) as.numeric(x) else NA
  return(o)
}

processTable = function(tbl){
  rows = rvest::xml_nodes(tbl,"a\\:tr")
  table = purrr::map(rows,function(r){
    rvest::xml_nodes(r,"a\\:tc") %>%
      xml2::xml_text()
  }) %>% do.call(rbind,.)
  return(table)
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
          makeNumeric()
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
    if(is.null(text[[x]]) | is.null(lvl[[x]]) | is.null(bulleted)) return(NULL)
    nlvl = ifelse(bulleted[[x]] & is.na(lvl[[x]]),0,lvl[[x]])
    tmp = data.frame(Text = text[[x]],Bulleted = bulleted[[x]],Hierarchy = (bulleted[[x]]+nlvl),stringsAsFactors = F)
    tmp = tmp[tmp$Text != "",]
    tmp$Hierarchy[tmp$Bulleted == FALSE] = NA
    if(nrow(tmp) == 0) return(NULL) else return(tmp)
  })
  names(output) = blockNames

  tables = rvest::xml_nodes(fc,"a\\:tbl")
  if(length(tables) > 0){
    for(i in seq_along(tables)){
      `[[`(output,paste0("Table ",i)) = processTable(tables[i])
    }
  }

  return(output[!sapply(output,is.null)])
}
