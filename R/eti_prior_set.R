#' Set the etiology prior
#'
#' Assign etiology prior to the pathogens entering the model.
#'
#'
#' @param model_options See \code{nplcm_fit} function argument list.
#'
#'
#' @return The vector of etiology prior parameters
#'
#'
#' @export

eti_prior_set <- function(model_options){
        Jcause <- length(model_options$cause_list)
        cat("==Etiology priors: ==" ,"\n",model_options$Eti_prior,"\n")
        if (model_options$Eti_prior=="overall_uniform"){
                  alpha    <-  rep(1,Jcause)
        }

        if (model_options$Eti_prior=="0_1"){
            alpha    <-  rep(.1,Jcause)
        }
		
        if (model_options$Eti_prior=="combo50"){
            alpha    <-  c(rep(.1,8),rep(.03,15),0.1)
        }
		for (i in 1: Jcause){
		nametwo = paste0("EPTwo",i)
		namefour = paste0("EPFour",i)
        if (model_options$Eti_prior==nametwo){
                  alpha    <-  rep(.1,Jcause)
				  alpha[i] = .2
        }
		if (model_options$Eti_prior==namefour){
		  alpha    <-  rep(.1,Jcause)
		  alpha[i] = .4
		}
		}

        alpha
}

