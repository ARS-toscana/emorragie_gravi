#' 'ApplyComponentStrategy'
#' 
#'ApplyComponentStrategy takes as input 
#'. a dataset where component algorithms have been assigned, and the instructions to build composite algorithms based on them. It produces: a dataset where the pairs_to_be_compared have been calculated; a dataset where the overlap of selected pairs of algorithms is computed; as an option, a graph that allows exploring the impact of the overlap. The input and output datasets may be either at individual level, or datasets of counts.   
#'. alternatively, a dataset where the overlap of selected pairs of algorithms is computed. It produces a graph that allows exploring the impact of the overlap.
#'
#'ApplyComponentStrategy calls CreateFigureComponentStrategy to produce the graph.
#'
#' @param dataset name of dataset
#' @param individual (boolean, default=FALSE). If TRUE, the dataset is at individual level.
#' @param intermediate_output (boolean, default=FALSE). If TRUE, an intermediate dataset is saved in ".RData".
#' @param intermediate_output_name (str, default="intermediate_output_dataset"). If intermediate_output=TRUE this is the name assigned to the intermediate dataset. (path is comprised in the name,if any).
#' @param components (list of str, default=NULL). List of the names of the binary variables associated to the components.
#' @param composites_to_be_created (list of integers, default=NULL). Each list is associated to a composite algorithm, the integers refer to the components, therefore the integers must be <= the number of components
#' @param labels_of_composites_to_be_created(list of str, optional, default=components). This list must have the same length as -composites_to_be_created-; each string is the label of the corresponding composite.
#' @param composites_to_be_created(list of integers, default=NULL). Each list is associated to a composite algorithm, the integers refer to the components, therefore the integers must be <= the number of components
#' @param pairs_to_be_compared (list of pairs of integers, default=NULL). Each pair is associated to a pair of algorithms to be compared; it contains the numbering of the two algorithms that form the component; the numbering refers to the order in the list -c(components, composites_to_be_created)
#' @param labels_of_components (list of str, optional, default=components). This list must have the same length as -components-; each string is the label of the corresponding component.
#' @param labels_of_composites_to_be_created(list of str, optional, default=components). This list must have the same length as -composites_to_be_created-; each string is the label of the corresponding composite.
#' @param labels_of_pairs_to_be_compared (list of str, optional, default=pairs_to_be_compared). This list must have the same length as -pairs_to_be_compared-; each string is the label of the corresponding component.
#' @param expected_number (str, optional, default=NULL). Variable containing the number of persons expected to be observed with the study variable of interest (in the corresponding stratum, if any).
#' @param count_var (str, only if individual=FALSE). Name of the variable containting the counts.
#' @param strata (list of str, optional, default=NULL). List of the names of the variables containing covariates or strata.
#' @param	nameN (str, default="N"). Prefix of the variables in the output dataset counting occurrences of algorithms. 
#' @param namePROP (str, default="PROP"). Prefix of the variables in the output dataset counting proportion of individuals detected by the algorithm. 
#' @param K (int, default=100). Scale of the proportions.
#' @param figure (boolean, default=TRUE). If TRUE, a figure is generated.
#' @param output_name (str, default="output"). Namefile assigned to the output dataset. Path is comprised in the name,if any. The file is saved in ".RData".
#' @param numcomponents (default=NULL) number of components
#' @param namevar_10 (str, default=NULL) proportion of individuals in the algorithm A but not in B, scaled by K.
#' @param namevar_11 (str, default=NULL) proportion of individuals both in the algorithm A and B, scaled by K.
#' @param namevar_01 (str, default=NULL) proportion of individuals in the algorithm B but not in A, scaled by K.
#' @param namevar_ (str, default=NULL) proportion of individuals in the population, scaled by K.
#' @param namevar_TRUE (str, optional, default=NULL) Variable containing the proportion of persons (scaled by K) expected to be observed with the study variable of interest
#' @param namevar_strata (vector of str, optional, default=NULL). Vector of the names of the variables containing covariates or strata.
#' @param namevar_labels (str, default=NULL) variable containing labels.
#' @param K (int, default=100). Scale of the proportions.
#' @param dirfigure (str). Directory where figure is saved.
#' @param figure_name (str, default="figure"). Namefile assigned to the figure. Path is NOT comprised in the name. The figure is saved in ".pdf".


ApplyComponentStrategy <- function(dataset,
                                   aggregate=T,
                                   individual=F,
                                   intermediate_output=F,
                                   intermediate_output_name="intermediate_output_dataset",
                                   components=NULL,  #
                                   composites_to_be_created=NULL,
                                   pairs_to_be_compared=NULL,
                                   labels_of_composites_to_be_created=NULL, 
                                   labels_of_components=components,
                                   labels_of_pairs_to_be_compared=pairs_to_be_compared,
                                   count_var=NULL,
                                   expected_number=NULL,
                                   nameN="N",
                                   K=100,
                                   namePROP="PROP",
                                   strata=NULL,
                                   figure=T,
                                   output_name="output",
                                   numcomponents=NULL,
                                   namevar_10=NULL ,
                                   namevar_11 =NULL,
                                   namevar_01 =NULL ,
                                   namevar_ =NULL ,
                                   namevar_TRUE = NULL,    
                                   namevar_strata = NULL,
                                   namevar_labels=NULL,
                                   dirfigure,
                                   figure_name="figure"){
  
  
  if (aggregate==F) { 
    #install packages
    if (!require("data.table")) install.packages("data.table")
    library(data.table)
    if (!require("survival")) install.packages("survival")
    library(survival)                           #used for strata() 
    if (!require("purrr")) install.packages("purrr")
    library(purrr)                               #used for flatten_chr()
    if (!require("tidyr")) install.packages("tidyr")
    library(tidyr)                               #used for pivot_longer()
    if (!require("ggplot2")) install.packages("ggplot2")
    library(ggplot2)
    
    ################ parameter pairs_to_be_compared #############################
    ## check that it is a list (do NOT use is.list())
    if (!inherits(pairs_to_be_compared, "list")){  
      stop("parameter pairs_to_be_compared must be a list of one dimensional vectors, each of them containing integers")
    }
    
    if (!inherits(composites_to_be_created, "list")){  
      stop("parameter composites_to_be_created must be a list of one dimensional vectors, each of them containing integers")
    }
    
    ## check that it is a list of lists
    check_and_transform <- function(tmp_elem) {
      error_message = "parameter pairs_to_be_compared must be a list of one dimensional vectors, each of them containing integers"
      if (inherits(tmp_elem, "numeric")){  ## check if atomic -> transform to list
        tmp_elem <- as.list(tmp_elem)
      }
      if (!inherits(tmp_elem, "list") | length(tmp_elem) < 2) { ## check if list and has at least two elements
        stop(message)
      } else if (length(tmp_elem) > length(components)) { ## number in pairs_to_be_compared <= number of algorithm
        stop("the numbers of parameter pairs_to_be_compared must be less or equal than the number of algorithms")
      }
      # Apply the function is_integer to all pairs_to_be_compared and in case of errors use the predefined message
      tmp_elem <- tryCatch(lapply(tmp_elem, is_integer, error_message),
                           error = function(e) {stop(error_message)})
      return(tmp_elem)
    }
    
    # check if element is a integer (even if type double)
    is_integer <- function(tmp_elem, error_message = F) {
      #If at least one of the element of the vector is =! 0 then raise error (works also on single integers)
      if (sum(tmp_elem %% 1) != 0){ 
        stop(error_message)
      }
      return(tmp_elem)
    }
    
    # Apply the chec on pairs_to_be_compared
    pairs_to_be_compared = lapply(pairs_to_be_compared, check_and_transform)
    composites_to_be_created = lapply(composites_to_be_created, check_and_transform)
    
    ################## parameter labels_of_components ##################
    ## check length of parameter labels_of_components
    if (length(labels_of_components)!=length(components)){
      stop("parameter labels_of_components must have the same length as components")
    }
    
    if (length(labels_of_composites_to_be_created)!=length(composites_to_be_created)){
      stop("parameter labels_of_composites_to_be_created must have the same length as composites_to_be_created")
    }
    
    ####################################################################
    #delete row with missing components
    dataset<-dataset[complete.cases(dataset[,..components]),]
    
    ######################################################################
    
    numcomposites<-length(pairs_to_be_compared)  #number of pairs_to_be_compared
    numcomponents=length(components)   #number of components
    numcomposites2becomp <- length(composites_to_be_created) 
    tot<-numcomposites+numcomponents + numcomposites2becomp

    #####################################################################
    ##############   composites_to_be_created       ####################    
    #####################################################################
    varname<-copy(components)
    iteration = 0
    for (c in composites_to_be_created){
      iteration <-  iteration + 1
      varname <- append(varname, labels_of_composites_to_be_created[[iteration]]) # Create alg5, alg6, alg7,...
      elem_or <- FALSE # binary vectors containing the column for the alg5 (and then alg6...)
      for (elem in c) {
        for (single_alg in elem) {# cicle for every list inside each element of each composite
          # sum the vectors by element using a or condition
          elem_or <- elem_or|dataset[[varname[[single_alg]]]]
        }
      }
      # Apply the resulting vector to the column
      dataset[[varname[[ length(varname) ]]]] <- as.integer(elem_or)
    }
    
    components <- c(components, labels_of_composites_to_be_created)
    numcomponents <- numcomponents + numcomposites2becomp
    #####################################################################   
    
 
    
    # N_pop PROP_TRUE
    # N_TRUE = mean(get(expected_number))
    
    A<-list()      #numeric vector:  first component of the pairs_to_be_compared
    B<-list()      #numeric vector:  second component of the pairs_to_be_compared
    varname<-copy(components)
    for (comp in pairs_to_be_compared){
      A<-append(A, list(comp[[1]]))
      B<-append(B, list(comp[[2]]))
      tmp_lenght <- length(varname) + 1
      varname <- append(varname, paste("alg", tmp_lenght, sep="")) # Create alg5, alg6, alg7,...
      elem_or <- FALSE # binary vectors containing the column for the alg5 (and then alg6...)
      for (elem in comp) {
        for (single_alg in elem) {# cicle for every list inside each element of each composite
          # sum the vectors by element using a or condition
          elem_or <- elem_or|dataset[[varname[[single_alg]]]]
        }
      }
      # Apply the resulting vector to the column
      dataset[[varname[[tmp_lenght]]]] <- as.integer(elem_or)
    }
    
    #save the first dataset 
    if (intermediate_output==T){
      assign(sapply(strsplit(intermediate_output_name, "/"), tail, 1),dataset)
      save(list=sapply(strsplit(intermediate_output_name, "/"), tail, 1), file=paste0(intermediate_output_name,".RData")) 
    }
    
    ##############################################
    
    algA<-copy(components)  #character vector: each string is the label of the corresponding first component of the pairs_to_be_compared
    algB<-integer(numcomponents) #character vector: each string is the label of the corresponding second component of the pairs_to_be_compared
    
    for (i in 1:numcomposites) {
      algA<-append(algA, varname[[A[[i]]]])
      algB<-append(algB, varname[[B[[i]]]])
    }
    
    ##############################################
    
    if (is.null(strata)){
      dim=1
    }else{
      n<-list()          #list of character vector: levels of the variables containing strata   
      for(i in 0+1:length(strata)){ 
        n[i]<-list(levels(strata(dataset[[strata[[i]]]])))
      }
      dim=prod(prod(lengths(n)))       #product of the dimensions 
    }
    
    
    ord_algA<-vector()  #numbers corresponding to the first component of the pairs_to_be_compared
    ord_algB<-vector()  #numbers corresponding to the second component of the pairs_to_be_compared
    ord_alg<-vector()   #labels
    
    ord_algA<-c(ord_algA,rep("-",dim*numcomponents))
    ord_algB<-c(ord_algB,rep("-",dim*numcomponents))
    
    labels_of_components <- c(labels_of_components, labels_of_composites_to_be_created)
    
    for (i in 1:numcomponents) {
      ord_alg<-c(ord_alg,rep(paste0(i,": ",labels_of_components[[i]]),dim))
    }
    
    for (i in 1:numcomposites){
      ord_algA<-c(ord_algA,rep(A[[i]],dim))
      ord_algB<-c(ord_algB,rep(B[[i]],dim))
      ord_alg<-c(ord_alg,rep(paste0(i+numcomponents,": ",labels_of_pairs_to_be_compared[[i]]),dim))
    }
    
    ##############################################
    
    if (individual){
      dataset[["n"]]<-1   #creates a variable containing 1 (if the dataset is at individual level)
      count_var<-'n'      
    }
    
    N_<-rbind()          #number of the individuals in the first algorithm or in the second argorithm
    N_00<-rbind()        #number of individuals who are not in the two algorithms
    N_01<-rbind()        #number of individuals in the second algorithm
    N_10<-rbind()        #number of individuals in the first algorithm
    N_11<-rbind()        #number of the individuals in the first algorithm and in the second argorithm
    for (i in 0+1:tot){
      if(i<=numcomponents){
        N_<- rbind(N_,
                   dataset[get(algA[[i]])==1, sum(get(count_var)),
                           by=eval(if (!is.null(strata) == T) {by = eval(flatten_chr(strata))})])   
        N_00<- rbind(N_00,
                     dataset[get(algA[[i]])!=1, sum(get(count_var)),
                             by=eval(if (!is.null(strata) == T) {by = eval(flatten_chr(strata))})])
        N_01<- rbind(N_01,
                     dataset[get(algA[[i]])==1, 0,
                             by=eval(if (!is.null(strata) == T) {by = eval(flatten_chr(strata))})])
        N_10<- rbind(N_10,
                     dataset[get(algA[[i]])==1, 0,
                             by=eval(if (!is.null(strata) == T) {by = eval(flatten_chr(strata))})])
        N_11<- rbind(N_11,
                     dataset[get(algA[[i]])==1, 0,
                             by=eval(if (!is.null(strata) == T) {by = eval(flatten_chr(strata))})])
      }else{
        N_<- rbind(N_,
                   dataset[get(algA[[i]])==1 | get(algB[[i]])==1, sum(get(count_var)),
                           by=eval(if (!is.null(strata) == T) {by = eval(flatten_chr(strata))})])
        N_00<- rbind(N_00,
                     dataset[get(algA[[i]])!=1 & get(algB[[i]])!=1, sum(get(count_var)),
                             by=eval(if (!is.null(strata) == T) {by = eval(flatten_chr(strata))})])
        N_01<- rbind(N_01,
                     dataset[get(algA[[i]])==0 & get(algB[[i]])==1, sum(get(count_var)),
                             by=eval(if (!is.null(strata) == T) {by = eval(flatten_chr(strata))})])
        N_10<- rbind(N_10,
                     dataset[get(algA[[i]])==1 & get(algB[[i]])==0, sum(get(count_var)),
                             by=eval(if (!is.null(strata) == T) {by = eval(flatten_chr(strata))})])
        N_11<- rbind(N_11,
                     dataset[get(algA[[i]])==1 & get(algB[[i]])==1, sum(get(count_var)),
                             by=eval(if (!is.null(strata) == T) {by = eval(flatten_chr(strata))})])
      }  
    }
    
    ####################
    
    if (is.null(strata)==F){
      col_strata<-N_[,-"V1"]  #variables containing strata
    }
    
    #################### 
    
    N_pop<-cbind(N_$V1+N_00$V1,deparse.level = 2)    #total number of individuals
    colnames(N_pop)<-"N_pop"
    
    ####################
    
    #only if expeced_number is a string (present in datataset)
    if (!is.null(expected_number)==TRUE){     
      
      N_TRUE<-rbind()
      for (i in 1:tot){
        if(i<=numcomponents){
          N_TRUE<-rbind(N_TRUE,dataset[get(algA[[i]])==1,
                                       mean(get(expected_number)),
                                       by=eval(if (!is.null(strata) == T) {by = eval(flatten_chr(strata))})])
        }else{
          N_TRUE<-rbind(N_TRUE,dataset[get(algA[[i]])==1| get(algB[[i]])==1 ,
                                       mean(get(expected_number)),
                                       by=eval(if (!is.null(strata) == T) {by = eval(flatten_chr(strata))})])
        }
      }
      
      
      PROP_TRUE<-K*(N_TRUE$V1/N_pop)
      colnames(PROP_TRUE)<-"PROP_TRUE"
    }
    
    #### proportions
    PROP_<-K*(N_$V1/N_pop)       
    colnames(PROP_)<-"PROP_"
    
    PROP_10<-K*(N_10$V1/N_pop)      #proportion of the individuals in the first algorithm 
    PROP_01<-K*(N_01$V1/N_pop)      #proportion of the individuals in the second argorithm
    PROP_11<-K*(N_11$V1/N_pop)      #proportion of the individuals in the first algorithm and in the second argorithm
    colnames(PROP_10)<-"PROP_10"
    colnames(PROP_01)<-"PROP_01"
    colnames(PROP_11)<-"PROP_11"
    
    
    ###################################################################################
    
    #########  output
    if (!is.null(expected_number)==TRUE){
      x<-data.frame(ord_alg,N_TRUE$V1,N_00$V1,N_$V1,N_pop,N_01$V1,N_10$V1,N_11$V1,ord_algA,ord_algB,PROP_TRUE,PROP_,PROP_10,PROP_11,PROP_01)
      setnames(x,"N_TRUE.V1",paste0(nameN,"_TRUE"))
      setnames(x,"PROP_TRUE",paste0(namePROP,"_TRUE"))
    }else{
      x<-data.frame(ord_alg,N_00$V1,N_$V1,N_pop,N_01$V1,N_10$V1,N_11$V1,ord_algA,ord_algB,PROP_,PROP_10,PROP_11,PROP_01) 
    }
    
    if (is.null(strata)==F){
      x<-cbind(col_strata,x)
    }
    
    #change prefix N_
    setnames(x,"N_00.V1",paste0(nameN,"_00"))
    setnames(x,"N_.V1",paste0(nameN,"_"))
    setnames(x,"N_01.V1",paste0(nameN,"_01"))
    setnames(x,"N_10.V1",paste0(nameN,"_10"))
    setnames(x,"N_11.V1",paste0(nameN,"_11"))
    setnames(x,"N_pop",paste0(nameN,"_pop"))
    
    #change prefix PROP_
    setnames(x,"PROP_",paste0(namePROP,"_"))
    setnames(x,"PROP_01",paste0(namePROP,"_01"))
    setnames(x,"PROP_10",paste0(namePROP,"_10"))
    setnames(x,"PROP_11",paste0(namePROP,"_11"))
    
    if (is.null(strata)==T){ x[, "ord_alg"] <- as.character(x[, "ord_alg"])}
    x<-as.data.table(x)
    
    #save the second output 
    assign(sapply(strsplit(output_name, "/"), tail, 1),x)
    save(list=sapply(strsplit(output_name, "/"), tail, 1), file=paste0(output_name,".RData")) 
    
    if (figure==T){
      
      CreateFigureComponentStrategy(dataset=x,
                                    numcomponents,
                                    namevar_10 = paste0(namePROP,"_10"),
                                    namevar_11 = paste0(namePROP,"_11") ,
                                    namevar_01 = paste0(namePROP,"_01") ,
                                    namevar_ = paste0(namePROP,"_"),
                                    if (is.null(expected_number)){namevar_TRUE = NULL}else{namevar_TRUE=paste0(namePROP,"_TRUE")} ,
                                    if (is.null(strata)){namevar_strata = NULL}else{namevar_strata=eval(flatten_chr(strata))} ,
                                    namevar_labels="ord_alg",
                                    K=K,
                                    dirfigure,
                                    figure_name)
    }  
    
    return(x)
    
  }else{  
    
    CreateFigureComponentStrategy(dataset=dataset,
                                  numcomponents,
                                  namevar_10 ,
                                  namevar_11 ,
                                  namevar_01 ,
                                  namevar_ ,
                                  namevar_TRUE ,    
                                  namevar_strata ,
                                  namevar_labels,
                                  K,
                                  dirfigure,
                                  figure_name)
    
  }
}

