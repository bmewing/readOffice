# readOffice 0.2.2

Updated release with minor improvements to functions to read in Microsoft Word and PowerPoint files.

## Improvements
Components on PowerPoint slides are stored in a named list to preserve structure.
Tables on PowerPoint slides are now detected and extracted as character matrices.

## .docx support
File is read in, broken by XML defined paragraph and returned as a vector.

## .pptx support
File is read in, each slide is processed and returned as an element of a list.
Each slide has most components identified (titles, subtitles, text blocks, shapes, tables) and extracts the text.
This text is returned as either a data.frame or a matrix (for tables) with minor formating details provided.
This text is stored in a named list (names are the slide component names).
