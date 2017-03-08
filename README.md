# readOffice

***A package for reading in text from 'unstructured' modern Microsoft Office file types***

[![Build Status](https://travis-ci.org/bmewing/readOffice.svg?branch=master)](https://travis-ci.org/bmewing/readOffice) ![](http://cranlogs.r-pkg.org/badges/readOffice)

## Why do I want this?
If you do any kind of text analysis work, you probably have text arrive in inconvenient formats like Word or PowerPoint.  While copy and paste can be an effective way of getting the text into an easily readable format, this package aims to make loading in those files even easier.

## Supported files
.docx and .pptx files are supported

## Usage
`read_docx("file/path/to/word.docx")`

Returns a vector of characters, one element for each paragraph in the file.

`read_pptx("file/path/to/powerpoint.pptx")`

Returns a list, one for each slide, each element in the list containing a vector of characters, one element for each paragraph of text on the slide.  Slides are kept in order.
