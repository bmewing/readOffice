processCharts = function(xml){
  fc = xml2::read_xml(xml)
  chart = fc %>% rvest::xml_node("c\\:chart")
  type = chart %>% rvest::xml_node("c\\:plotArea") %>% rvest::xml_nodes(xpath='child::*') %>% rvest::html_name() %>% grep(pattern = "Chart", value = T)

  title = chart %>% rvest::xml_node("c\\:title") %>% xml2::xml_text()
  title_x = chart %>% rvest::xml_node("c\\:catAx") %>% rvest::xml_nodes("a\\:p") %>% xml2::xml_text() %>% paste(collapse="\n") %>% gsub(pattern = "\n+$",replacement = "")
  title_y = chart %>% rvest::xml_node("c\\:valAx") %>% rvest::xml_nodes("a\\:p") %>% xml2::xml_text() %>% paste(collapse="\n") %>% gsub(pattern = "\n+$",replacement = "")
  series = chart %>% rvest::xml_nodes("c\\:ser") #color elements
  if(grepl("^bar|^pie|^line",type)){
    data = purrr::map(series,function(x){
      series_name = rvest::xml_node(x,"c\\:v") %>% xml2::xml_text()
      categories = rvest::xml_node(x,"c\\:cat") %>% rvest::xml_nodes("c\\:pt") %>% xml2::xml_text()
      values = rvest::xml_node(x,"c\\:val") %>% rvest::xml_nodes("c\\:pt") %>% xml2::xml_text() %>% as.numeric()
      data.frame(s = series_name,c = categories,v = values,nc = seq_along(categories),stringsAsFactors = FALSE)
    }) %>% do.call(what = rbind)
  } else if(grepl("^scatter|^bubble",type)){
    data = purrr::map(series,function(x){
      series_name = rvest::xml_node(x,"c\\:v") %>% xml2::xml_text()
      xval = rvest::xml_node(x,"c\\:xVal") %>% rvest::xml_nodes("c\\:pt") %>% xml2::xml_text() %>% as.numeric()
      xvalID = rvest::xml_node(x,"c\\:xVal") %>% rvest::xml_nodes("c\\:pt") %>% xml2::xml_attr("idx")
      yval = rvest::xml_node(x,"c\\:yVal") %>% rvest::xml_nodes("c\\:pt") %>% xml2::xml_text() %>% as.numeric()
      yvalID = rvest::xml_node(x,"c\\:yVal") %>% rvest::xml_nodes("c\\:pt") %>% xml2::xml_attr("idx")
      if(grepl("^bubble",type)){
        size = rvest::xml_node(x,"c\\:bubbleSize") %>% rvest::xml_nodes("c\\:pt") %>% xml2::xml_text() %>% as.numeric()
        sizeID = rvest::xml_node(x,"c\\:bubbleSize") %>% rvest::xml_nodes("c\\:pt") %>% xml2::xml_attr("idx")
      } else {
        size = rep(1,length(union(xvalID,yvalID)))
        sizeID = union(xvalID,yvalID)
      }
      mk = intersect(sizeID,intersect(xvalID,yvalID))
      data.frame(s = series_name,c = xval[xvalID %in% mk],v = yval[yvalID %in% mk],d = size[sizeID %in% mk],stringsAsFactors = FALSE)
    }) %>% do.call(what = rbind)
  }


  if(grepl("^bar",type)){
    orientation = chart %>% rvest::xml_node("c\\:plotArea") %>% rvest::xml_node("c\\:barDir") %>% xml2::xml_attr("val")
    stacked = chart %>% rvest::xml_node("c\\:plotArea") %>% rvest::xml_node("c\\:grouping") %>% xml2::xml_attr("val")
    plot = ggplot2::ggplot(data = data,ggplot2::aes(x = c, y = v, fill = s)) +
      ggplot2::geom_col(position = ifelse(stacked == "clustered", "dodge","stack")) +
      ggplot2::labs(title = title,x = title_x,y = title_y) +
      ggplot2::theme(legend.title=ggplot2::element_blank())
    if(orientation == "bar") plot = plot + ggplot2::coord_flip()
  } else if(grepl("^pie",type)){
    plot = ggplot2::ggplot(data = data,ggplot2::aes(x = "", y = v, fill = c)) +
      ggplot2::geom_col(width=1) +
      ggplot2::coord_polar("y") +
      ggplot2::labs(title = title,x = title_x,y = title_y) +
      ggplot2::theme(legend.title = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(labels = NULL)
  } else if(grepl("^line",type)){
    plot = ggplot2::ggplot(data = data,ggplot2::aes(x = nc, y = v, color = s)) +
      ggplot2::geom_line() +
      ggplot2::labs(title = title,x = title_x,y = title_y) +
      ggplot2::theme(legend.title = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank()) +
      ggplot2::scale_x_continuous(breaks = data$nc[1:length(unique(data$nc))],labels = data$c[1:length(unique(data$nc))])
  } else if(grepl("^scatter",type)){
    style = chart %>% rvest::xml_node("c\\:plotArea") %>% rvest::xml_node("c\\:scatterStyle") %>% xml2::xml_attr("val")
    #linemarker is basic scatterplot
    plot = ggplot2::ggplot(data = data,ggplot2::aes(x = c, y = v, color = s)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = title,x = title_x,y = title_y) +
      ggplot2::theme(legend.title = ggplot2::element_blank())
  } else if(grepl("^bubble",type)){
    plot = ggplot2::ggplot(data = data,ggplot2::aes(x = c, y = v, color = s, size = d)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = title,x = title_x,y = title_y) +
      ggplot2::theme(legend.title = ggplot2::element_blank())
  }
  return(list(Data = data, Chart = plot))
}

