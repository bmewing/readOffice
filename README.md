# readOffice

***A package for reading in text from 'unstructured' modern Microsoft Office file types***

[![Build Status](https://travis-ci.org/bmewing/readOffice.svg?branch=master)](https://travis-ci.org/bmewing/readOffice) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/readOffice)](https://CRAN.R-project.org/package=readOffice) ![](http://cranlogs.r-pkg.org/badges/readOffice)

## Why do I want this?
If you do any kind of text analysis work, you probably have text arrive in inconvenient formats like Word or PowerPoint.  While copy and paste can be an effective way of getting the text into an easily readable format, this package aims to make loading in those files even easier.

## Install it!
    
```r
#install from CRAN
install.packages("readOffice")

#install from this github repo
devtools::install_github("bmewing/readOffice")
```

## Supported files
.docx and .pptx files are supported

## Usage
```r
read_docx("file/path/to/word.docx")
```

Returns a vector of characters, one element for each paragraph in the file.

```r
read_pptx("file/path/to/powerpoint.pptx")
```

Returns a list, one for each slide, containing the elements of that slide.  Slides are kept in order.  Each item in the list contains a data.frame or matrix containing the textual content of an element on the slide.  Example elements are title boxes, text boxes, shapes, tables.  Tables are returned as a matrix, everything else as a data.frame.  Data frames have one row per 'paragraph' of text with indicators as to the 'bulleted'-ness of the text and if so, the hierarcy of the text. 
