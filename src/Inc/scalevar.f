      logical doscalevar
      logical, save:: foundpow=.false.
      integer alphaspow,maxscalevar
      real(dp):: scalereweight(8)
      common/doscalevarcommon/doscalevar,maxscalevar
      common/scalevarint/alphaspow
      common/scalevardp/scalereweight
!$omp threadprivate(foundpow,/scalevarint/,/scalevardp/)
