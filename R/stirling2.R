#' Prints the value of Stirling numbers of second kind
#' @export
#' @param n the first parameter representing the number of elements in the set total.
#' @param k the second parameter representing the number of groups to be formed.
#' @author Anik Paul
#' @description Determining the Stirling number of second kind.
#' @details Stirling numbers of second kind is a very useful term used in combinatorics denoting the number of all possible groups of size k from a set of size n.
#' @references Bóna,Miklós(2017,ISBN 9789813148840).
#' @return Stirling2: the determined value of Stirling numbers of second kind.
#' @name stirling2
#' @examples Stirling2(3,2)


Stirling2=function(n,k){
  if((k==0 & n>0)|(n==0 & k>0)|(n<k)){
    return(0)
  }
  else if(n==1 & k==1){
    return(1)
  }
  else{
    return(Stirling2(n-1,k-1)+k*Stirling2(n-1,k))
  }
}
