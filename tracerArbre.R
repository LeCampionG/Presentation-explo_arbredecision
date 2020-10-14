tracerArbre <- function(arbre, type, uniform, use.n, all, fancy) {

	if (type=="Graphique classique") {
		plot(arbre, uniform=uniform, margin=0.1)
		text(arbre, use.n=use.n, all=all, fancy=fancy, pretty=0)
	} else {
		fancyRpartPlot(arbre)
	}
	
}
