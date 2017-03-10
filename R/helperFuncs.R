makeNumeric = function(x){
  o = if(length(x) != 0) as.numeric(x) else NA
  return(o)
}

docxNodeType = function(node){
  name = rvest::html_name(node)
  if(name == "tbl") return("tbl")
  type = rvest::html_attr(rvest::xml_node(node,"w\\:br"),"type")
  if(!is.na(type) && type == "page") return("pbr")
  drawing = rvest::xml_node(node,"w\\:drawing")
  if(!is.na(drawing)){
    if(is.na(rvest::xml_node(node,"wps\\:txbx"))) return("emp") else return("drw")
  }
  if(xml2::xml_text(node) == "") return("emp")
  return("txt")
}

processDiagram = function(d){
  text = xml2::read_xml(d) %>%
    rvest::xml_nodes("a\\:p") %>%
    xml2::xml_text()
  return(text[text != ""])
}

processParagraph = function(p){
  out = p %>%
    xml2::xml_text() %>%
    gsub("\n[[:space:]\n]+","\n",.) %>%
    gsub("\n$","",.)
  return(out)
}

processDrawing = function(d){
  out = d %>%
    rvest::xml_nodes("wps\\:txbx") %>%
    xml2::xml_text() %>%
    gsub("\n[[:space:]\n]+","\n",.) %>%
    gsub("\n$","",.)
  return(out)
}

processTable = function(tbl,type){
  row_node = ifelse(type=="docx",'w\\:tr','a\\:tr')
  col_node = ifelse(type=="docx",'w\\:tc','a\\:tc')
  rows = rvest::xml_nodes(tbl,row_node)
  table = purrr::map(rows,function(r){
    cols = rvest::xml_nodes(r,col_node)
    purrr::map(cols,function(l){
      l %>%
        as.character() %>%
        gsub("<.*?>","",.)
    }) %>% unlist() %>%
      gsub("\n[[:space:]\n]+","\n",.) %>%
      gsub("\n$|^\n","",.)
  }) %>% do.call(what = rbind)
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
      `[[`(output,paste0("Table ",i)) = processTable(tables[i],"pptx")
    }
  }

  return(output[!sapply(output,is.null)])
}
