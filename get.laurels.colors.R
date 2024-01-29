get.laurels.colors <- function(n) {
# n is a sequence of numbers corresponding to the list of colors
	
many.colors.name <- matrix(c(
"FLAME","#E4572E",
"Dark Tangerine","#F3A712",
"Laurel Green","#A8C686",
"Blue Sapphire","#035E7B",
"Cerulean Frost","#669BBC"
),ncol=2,byrow=TRUE)

#if (length(n)>1) pastel.out <- pastel.name[1:n,]
#if (length(n)==1) pastel.out <- pastel.name[n,]

many.colors.out <- many.colors.name[n,]
many.colors.out
}

