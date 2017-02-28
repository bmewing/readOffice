# readOffice 0.1.0

Initial release with functions to read in Microsoft Word and PowerPoint files.

## .docx support
File is read in, broken by paragraph and returned as a vector.

## .pptx support
File is read in, text is extracted from each slide (by paragraph) and returned as a list of slides each containing a vector of text from that slide.
