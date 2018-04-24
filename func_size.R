#'Functional Size
#'
#'This function computes functional size, which is just N/D, where N = number of 
#'mock witnesses and D = number of mock witnesses choosing the suspect
#'
#'@param lineup_vec A numeric vector of lineup choices
#'@param susp_pos Suspect/lineup member position. Must be declared by user
#'@returns Functional size of lineup
#'@references Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'            Law and Human Behavior, 22(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures of
#'            lineup size and lineup bias. Applied Cognitive Psychology, 13, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. Law and Human Behavior,
#'            3(4), 285-293.
func_size <- function(lineup_vec, susp_pos){
    fsize <- length(lineup_vec) / sum(lineup_vec == susp_pos)
}