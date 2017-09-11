There is an extra step for herring, which must be done prior to the usual steps to compile the PDF. Copy the two R files in `csas-latex/doc/` ("Functions.R", and "ResearchDocumentV2.R") to `csas-latex/models/`.
Source the file ResearchDocumentV2.R; this will go through the each stock and each model to create tables and figures used in the document.
You have to specify which stocks to include via the variables `allRegions` (Line 110) and `regions` (Line 133).
Note that these names must correspond to the folder names used for stock, and will show up in the document in figure and table captions.
You also have to specify which models to include via the variable `mNames` (Line 81).
Again, these names have to correspond to the subfolder names used for model output, and will show up in figure and table captions.
Currently, the script requires that each stock have the same model names (and number of models).