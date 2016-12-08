sl.match.comp <-
function(x,table,comp.op="gt",offset=0) {
	if (comp.op == "gt") {
		for (i in (1+offset):length(table)) {
			if (table[i] > x) {return(i)}
		}
	} else if (comp.op == "lt") {
		for (i in (1+offset):length(table)) {
			if (table[i] < x) {return(i)}
		}
	} else if (comp.op == "get") {
		for (i in (1+offset):length(table)) {
			if (table[i] >= x) {return(i)}
		}
	} else if (comp.op == "let") {
		for (i in (1+offset):length(table)) {
			if (table[i] <= x) {return(i)}
		}
	} else if (comp.op == "eq") {   # this is equivalent to 'match'
		for (i in (1+offset):length(table)) {
			if (table[i] == x) {return(i)}
		}
	}
	warning("no match found")
	return(NA)
}
