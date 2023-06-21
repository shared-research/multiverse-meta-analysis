.get_all_Tspace <- function(mods){
  # dplyr::bind_cols(lapply(mods, function(md) md$Tspace))
  tab=mods[[1]]$Tspace
  for (i in 2:length(mods)){
    tab=cbind(tab,mods[[i]]$Tspace)
  }
  tab
}

.set_tail <- function(Tspace,tail=0){
  if((tail==0)|(tail=="two.sided"))
    Tspace=abs(Tspace) else
      if((tail<0)|(tail=="less"))
        Tspace=-Tspace

      Tspace
}

#' p_adjust_fwer
#'
#' @param mods mods
#' @param method method
#' @param tail tail
#'
#' @export
p_adjust_fwer <- function(mods,method = "maxT",tail=0){
  Tspace=as.matrix(.get_all_Tspace(mods))
  #  smr=summary_ei(mods)
  #  nrm=(smr$Score/smr$eff_size)^-1
  #  Tspace=Tspace%*%diag(nrm)
  # sumY2s=.get_all_sumScore2(mods)
  # n=nrow(mods[[1]]$scores)
  colnames(Tspace)=paste0("v",1:ncol(Tspace))

  p.adj=flip::flip.adjust(.set_tail(Tspace,tail = tail),
                          method = method)
  tmp=lapply(1:length(mods),function(id){
    tt=summary.joinmeta(mods[id])
    tt$p.adj=p.adj[id]
    mods[[id]]$summary_table=tt
    mods[[id]]
  })

  class(tmp) <- c("jointest", class(tmp))
  tmp
}
