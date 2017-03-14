processCharts = function(xml){
  fc = xml2::read_xml(xml)
  chart = fc %>% rvest::xml_node("c\\:chart")
  type = chart %>% rvest::xml_node("c\\:plotArea") %>% rvest::xml_nodes(xpath='child::*') %>% rvest::html_name() %>% grep(pattern = "Chart", value = T)

  title = chart %>% rvest::xml_node("c\\:title") %>% xml2::xml_text()
  title_x = chart %>% rvest::xml_node("c\\:catAx") %>% rvest::xml_nodes("a\\:p") %>% xml2::xml_text() %>% paste(collapse="\n") %>% gsub(pattern = "\n+$",replacement = "")
  title_y = chart %>% rvest::xml_node("c\\:valAx") %>% rvest::xml_nodes("a\\:p") %>% xml2::xml_text() %>% paste(collapse="\n") %>% gsub(pattern = "\n+$",replacement = "")
  series = chart %>% rvest::xml_nodes("c\\:ser") #color elements
  data = purrr::map(series,function(x){
    series_name = rvest::xml_node(x,"c\\:v") %>% xml2::xml_text()
    categories = rvest::xml_node(x,"c\\:cat") %>% rvest::xml_nodes("c\\:pt") %>% xml2::xml_text()
    values = rvest::xml_node(x,"c\\:val") %>% rvest::xml_nodes("c\\:pt") %>% xml2::xml_text() %>% as.numeric()
    data.frame(s = series_name,c = categories,v = values,stringsAsFactors = FALSE)
  }) %>% do.call(what = rbind)

  if(type == "barChart"){
    plot = ggplot2::ggplot(data = data,ggplot2::aes(x = c, y = v, fill = s)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::labs(title = title,x = title_x,y = title_y) +
      ggplot2::theme(legend.title=ggplot2::element_blank())
  } else if(type == "pieChart"){
    plot = ggplot2::ggplot(data = data,ggplot2::aes(x = "", y = v, fill = c)) +
      ggplot2::geom_col(width=1) +
      ggplot2::coord_polar("y") +
      ggplot2::labs(title = title,x = title_x,y = title_y) +
      ggplot2::theme(legend.title=ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(labels = NULL)
  }
  return(list(Data = data, Chart = plot))
}

