#' Convert month name to number
#'
#' Convert a month name to a number.  Kinds of names supported include, for example, "October", "Oct", and "Oct.".  Unambiguous matching is used. For example, "J", "Ja", and "Janua" would all be matched to January so would return a 1 (first month).  "M" and "Ma" would not be matched since they could both stand for "March" or "May".
#' @param mo Character vector of month names.
#' @param names If \code{TRUE} (default), name the output vector by input names.
#' @return Integer vector.
#' @examples
#' 
#' mo <- c('jan', 'Jan', 'Jan.', 'Janu', 'january')
#' monthAsNum(mo)
#' 
#' mo <- c('j', 'f', 'm', 'a', 'm', 'j', 'j', 'a', 's', 'o', 'n', 'd')
#' monthAsNum(mo)
#' monthAsNum(mo, FALSE)
#' 
#' @export
monthAsNum <- compiler::cmpfun(function(mo, names=TRUE) {

	moOrig <- mo
	mo <- tolower(mo)
	mo <- gsub(mo, pattern='\\.', replacement='')
	
	choices <- c('january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'ocrober', 'november', 'december')
	
	out <- rep(NA, length(mo))
	for (i in seq_along(mo)) {
		out[i] <- pmatch(mo[i], choices)
	}
	if (names) names(out) <- moOrig
	out

})
