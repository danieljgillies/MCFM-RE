      subroutine bookplot(n,tag,titlex,var,wt,wt2,xmin,xmax,dx,llplot) 
      implicit none
      include 'types.f'
      include 'nplot.f'
      include 'kpart.f'
      include 'outputflags.f'
      include 'vegas_common.f'
      include 'scalevar.f'
      integer:: n,iscale
      character*(*) titlex
      character*40 trimtitle,origtitle
      character*3 llplot
      integer tag
      real(dp):: var,wt,wt2,origwt,origwt2,xmin,xmax,dx
      integer, parameter:: tagbook=1, tagplot=2
      logical, save :: threadfirst(500)=.true.
!$omp threadprivate(threadfirst)

      call checkmaxhisto(n)

      trimtitle=trim(titlex)
      if (doscalevar) then
        iscale=0
        origtitle=trimtitle
        origwt=wt
        origwt2=wt2
      endif

   66 continue

      if     (tag == tagbook) then
        if (dswhisto .eqv. .false.) then
c--- Traditional MCFM histograms
          call mbook(n,trimtitle,dx,xmin,xmax)
c--- also book the errors now (in maxhisto+n,2*maxhisto+n)
          call mbook(maxhisto+n,trimtitle,dx,xmin,xmax)
          call mbook(2*maxhisto+n,trimtitle,dx,xmin,xmax)
          if ( (kpart==kreal) .or. (kpart==ktota)
     &     .or.(kpart==ktodk) .or. (kpart==ksnlo)
     &     .or.(kpart==knnlo)) then
c            call tmpmbook(n,trimtitle,dx,xmin,xmax)
            call smartbook(n,trimtitle,dx,xmin,xmax)
        endif
        else
c--- DSW histograms - call hbook booking routine
          call dswhbook(n,trimtitle,dx,xmin,xmax)
        endif
      elseif (tag == tagplot) then
        if (dswhisto .eqv. .false.) then
c--- Traditional MCFM histograms
c--- also book the errors now (in maxhisto+n); fill temp histos for real
          if ((kpart==klord) 
     &   .or. (kpart==kvirt) 
     &   .or. (kpart==kfrag)
     &   .or. (kpart==ksnlo)
     &   .or. (kpart==knnlo)
     &   .or. (kpart==kll)
     &   .or. (kpart==knll)
     &   .or. (kpart==knllexpd)
     &   .or. (kpart==knnll)
     &   .or. (kpart==knnllexpd)
     &   .or. (kpart==klumi)
     &   .or. (kpart==klumi0)
     &   .or. (kpart==klumi1)) then
            call mfill(n,var,wt)
            call mfill(maxhisto+n,var,wt2)
          else
            if (threadfirst(n)) then
c              call tmpmbook(n,trimtitle,dx,xmin,xmax)
              call smartbook(n,trimtitle,dx,xmin,xmax)
              threadfirst(n)=.false.
            endif
c            call tmpmfill(n,var,wt)
            call smartfill(n,var,wt)
        endif
        else
c--- DSW histograms - call hbook filling routine
c--- note that we divide by # of iterations here since it is only
c--- handled at the end in the default MCFM histograms
          call dswhfill(n,var,wt/real(itmx,dp))
        endif
        linlog(n)=llplot
        titlearray(n)=titlex
      endif

      if (doscalevar) then
        iscale=iscale+1
        if (iscale <= maxscalevar) then
          if     (iscale == 1) then
            trimtitle=trim(origtitle)//': scale uu'
          elseif (iscale == 2) then
            trimtitle=trim(origtitle)//': scale dd'
          elseif (iscale == 3) then
            trimtitle=trim(origtitle)//': scale u-'
          elseif (iscale == 4) then
            trimtitle=trim(origtitle)//': scale d-'
          elseif (iscale == 5) then
            trimtitle=trim(origtitle)//': scale -u'
          elseif (iscale == 6) then
            trimtitle=trim(origtitle)//': scale -d'
          endif  
          wt=origwt*scalereweight(iscale)
          wt2=origwt2*scalereweight(iscale)**2
          n=n+1
          goto 66
        endif
        wt=origwt
        wt2=origwt2
      endif  

      return
      end

      subroutine ebookplot(n,tag,var,wt) 
      implicit none
      include 'types.f'
      include 'PDFerrors.f'
      include 'outputflags.f'
      integer:: n
      real(dp):: var,wt
      integer tag
      integer, parameter:: tagbook=1, tagplot=2

      if (PDFerrors .eqv. .false.) return

      if (tag==tagbook) then
        if (dswhisto .eqv. .false.) then
c--- Traditional MCFM histograms
          call ebook(n)
        endif
      elseif (tag == tagplot) then
        if (dswhisto .eqv. .false.) then
c--- Traditional MCFM histograms
          call efill(n,var,wt)
        endif
      endif

      return
      end

