# Allows prediction of numerical and categorical targets 
# utilizing a model expressed in PMML and the OpenScoring Server
#
# see:
#   http://code.google.com/p/openscoring/
#   http://scottmutchler.blogspot.de/2012/11/openscoring-open-source-scoring-of-pmml.html
#############################################################################################

predictPMMLModel <- function(dataset, 	# dataset for prediction 
		transformTargetAttribute, 	  	# type of target attribute
		modelURL, 						# place where the scoring engine could find the pmml model
		applServerURL					# servlet url
){
	require(XML)
	require(RCurl)
	
	header <- paste(colnames(dataset), collapse=",") # extract header
	# transformation to characters is necessary to avoid some “bad surprise” from R's handling of factor attributes
	datasetTransf <- data.frame(lapply(dataset, as.character), stringsAsFactors=FALSE)
	dataString <- paste(header,"|", paste(do.call("rbind",
							by(datasetTransf, 1:nrow(datasetTransf), function(row) {
										paste(row, collapse=",")
									}, simplify = FALSE)), collapse ="|"), "|", sep = "")
	
	# create xml document
	xmlHeader <- xmlPINode(sys = 'xml', value = 'version="1.0" encoding="UTF-8"')
	xmlRequest <- xmlNode("scoring_request", 
			xmlNode("pmml_url", modelURL), 
			xmlNode("model_name"),
			xmlNode("csv_input_rows",xmlCDataNode(dataString)))
	
	# xml request as string
	fullXMLRequest <- paste(toString(xmlHeader),"\n", gsub(" ", "", toString(xmlRequest, sep=""), fixed = TRUE))
	
	# http post request
	r = dynCurlReader()
	curlPerform(postfields = fullXMLRequest, url = applServerURL, 
			verbose = TRUE, post = 1L, writefunction = r$update)
	r$value()
	
	# parse results - !!caution: currently no error checking!!
	tmp <- xmlTreeParse(r$value())
	predictionString <- xmlValue(tmp[[1]][[1]][[4]])
	# extract predictions line by line
	predictionLines <- strsplit(predictionString, split ="|", fixed = TRUE)[[1]][-1]
	predictions <- transformTargetAttribute(sapply(predictionLines, function(s){
						gsub('\"','',tail(strsplit(s, ',', fixed = TRUE)[[1]], n=1))
					}))
	names(predictions) <- NULL
	return(predictions)
}