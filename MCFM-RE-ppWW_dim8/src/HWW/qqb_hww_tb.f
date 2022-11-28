      subroutine qqb_hww_tb(p,msq)
      use gg_ww_dim8
      implicit none
      include 'types.f'
      
c--- Author: J. M. Campbell, June 2011
c--- Implementation of gg -> H -> WW -> leptons,
c--- including both top and bottom quark loops
      include 'constants.f'
      include 'nf.f'
      include 'mxpart.f'
      include 'cplx.h'
      include 'ewcouple.f'
      include 'qcdcouple.f'
      include 'masses.f'
      include 'sprods_com.f'
      include 'zprods_decl.f'
      include 'anom_higgs.f' 
      include 'kappa.f'
      integer:: h1,h2,j,k
      real(dp):: p(mxpart,4),msq(fn:nf,fn:nf),msqgg,fac
      real(dp):: mfsq,tau,tauinv,rt,rescale
      complex(dp):: Ahiggs_t(2,2), Ahiggs_b(2,2), Ahiggs_g(2,2)
      complex(dp):: Ahiggs(2,2),fachiggs,amphiggs,f,e3De4
      complex(dp):: Adim8(6,2,2)
      complex(dp):: rdim8
!      complex(dp):: num_c

      do j=-nf,nf
      do k=-nf,nf
      msq(j,k)=0._dp
      enddo
      enddo

      call spinoru(6,p,za,zb)
      
c--- fill amplitudes with contributions of Higgs: top loop
      mfsq=mt**2
      tau=s(1,2)/(4._dp*mfsq)
      tauinv=1._dp/tau
      fachiggs=cone/cplx2(s(1,2)-hmass**2,hmass*hwidth)

!====== SEYMOUR ISA APPROX 
!      num_c=cplx1(hmass**2/s(1,2)) 
!      fachiggs=num_c/cplx2(s(1,2)-hmass**2,s(1,2)*hwidth/hmass)

      if (tau <= 1._dp) then
         f=cplx1(asin(sqrt(tau))**2)
      elseif (tau > 1._dp) then
         rt=sqrt(1._dp-tauinv)
         f=-0.25_dp*(cplx1(log((1._dp+rt)/(1._dp-rt)))-im*pi)**2
      else
         f=czip
      endif
      e3De4=2._dp*za(3,5)*zb(6,4)/(s(3,4)*s(5,6))
      amphiggs=k_t*mfsq*(cone+(cone-cplx1(tauinv))*f)*im*e3De4
      Ahiggs_t(1,1)=fachiggs*amphiggs*za(1,2)/zb(2,1)
      Ahiggs_t(1,2)=czip
      Ahiggs_t(2,1)=czip
      Ahiggs_t(2,2)=fachiggs*amphiggs*zb(1,2)/za(2,1)

c--- fill amplitudes with contributions of Higgs: bottom loop
      mfsq=mb**2
      tau=s(1,2)/(4._dp*mfsq)
      tauinv=1._dp/tau

      if (tau <= 1._dp) then
         f=cplx1(asin(sqrt(tau))**2)
      elseif (tau > 1._dp) then
         rt=sqrt(1._dp-tauinv)
         f=-0.25_dp*(cplx1(log((1._dp+rt)/(1._dp-rt)))-im*pi)**2
      else
         f=czip
      endif
      amphiggs=k_b*mfsq*(cone+(cone-cplx1(tauinv))*f)*im*e3De4

      Ahiggs_b(1,1)=fachiggs*amphiggs*za(1,2)/zb(2,1)
      Ahiggs_b(1,2)=czip
      Ahiggs_b(2,1)=czip
      Ahiggs_b(2,2)=fachiggs*amphiggs*zb(1,2)/za(2,1)

c---  fill amplitudes with contributions of Higgs: contact interaction
      amphiggs=k_g*(s(1,2)/6._dp)*im*e3De4

      Ahiggs_g(1,1)=fachiggs*amphiggs*za(1,2)/zb(2,1)
      Ahiggs_g(1,2)=czip
      Ahiggs_g(2,1)=czip
      Ahiggs_g(2,2)=fachiggs*amphiggs*zb(1,2)/za(2,1)

c--- Rescale for width study
      if((keep_smhiggs_norm).and.(anom_higgs)) then 
         rescale=chi_higgs**2 
         Ahiggs_t(:,:)=Ahiggs_t(:,:)*rescale
         Ahiggs_b(:,:)=Ahiggs_b(:,:)*rescale
         Ahiggs_g(:,:)=Ahiggs_g(:,:)*rescale
      endif

c---  dimension 8 operators
! hijacked
      Adim8(1,2,2)= A1pp(za,zb)*im*(four,zero)
      Adim8(1,2,1)= A1pm(za,zb)*im*ctwo
      Adim8(1,1,2)= conjg(Adim8(1,2,1))
      Adim8(1,1,1)= conjg(Adim8(1,2,2))

      Adim8(2,2,2)= A2pp(za,zb)*im*(8._dp,zero)
      Adim8(2,2,1)= czip
      Adim8(2,1,2)= czip
      Adim8(2,1,1)= conjg(Adim8(2,2,2))

      Adim8(3,2,2)= A3pp(za,zb)*im*(8._dp,zero)
      Adim8(3,2,1)= A3pm(za,zb)*im*(8._dp,zero)
      Adim8(3,1,2)= conjg(Adim8(3,2,1))
      Adim8(3,1,1)= conjg(Adim8(3,2,2))

      Adim8(4,2,2)= A4pp(za,zb)*im*(four,zero)
      Adim8(4,2,1)= A4pm(za,zb)*im*(four,zero)
      Adim8(4,1,2)= conjg(Adim8(4,2,1))
      Adim8(4,1,1)= conjg(Adim8(4,2,2))

      Adim8(5,2,2)= A5pp(za,zb)*im*wmass**2
      Adim8(5,2,1)= A5pm(za,zb)*im*wmass**2
      Adim8(5,1,2)= conjg(Adim8(5,2,1))
      Adim8(5,1,1)= conjg(Adim8(5,2,2))

      Adim8(6,2,2)= A6pp(za,zb)*im*(four,zero)*wmass**2
      Adim8(6,2,1)= czip
      Adim8(6,1,2)= czip
      Adim8(6,1,1)= conjg(Adim8(6,2,2))

c---  MCFM propagator convention
      Adim8 = Adim8/(s(3,4)*s(5,6))
      
c---  Ratio convention
c---  strong coupling constant omitted due to cancelling with kappa/v^4
      rdim8 = ctwo*pi/gwsq
!      check with 1602.05141 - omit unless checking
!      rdim8 = rdim8*four*pi/gsq
      Adim8 = Adim8*rdim8
!     our normalisation, comment out when checking with 1602.05141
      Adim8=Adim8*gwsq**2/((16d0,zero)*wmass**4)

c---  fill amplitude with full contributions of Higgs
      Ahiggs(1,1)=Ahiggs_t(1,1)+Ahiggs_b(1,1)+Ahiggs_g(1,1)
      Ahiggs(1,2)=Ahiggs_t(1,2)+Ahiggs_b(1,2)+Ahiggs_g(1,2)
      Ahiggs(2,1)=Ahiggs_t(2,1)+Ahiggs_b(2,1)+Ahiggs_g(2,1)
      Ahiggs(2,2)=Ahiggs_t(2,2)+Ahiggs_b(2,2)+Ahiggs_g(2,2)

      msqgg=0._dp
      do h1=1,2
      do h2=1,2
         msqgg=msqgg+abs(Ahiggs(h1,h2)
     &        +dot_product(kdim8(:),Adim8(:,h1,h2)))**2

      if (intonly) then
c---  accumulate just the interference terms
        msqgg=msqgg-abs(Ahiggs_t(h1,h2))**2
     &             -abs(Ahiggs_b(h1,h2))**2
     &             -abs(Ahiggs_g(h1,h2))**2
      endif

      enddo
      enddo

c--- overall factor from diagrams
      fac=avegg*V*(2._dp*gwsq*gsq/(16._dp*pisq)*gwsq/2._dp)**2
     & *s(3,4)**2/((s(3,4)-wmass**2)**2+(wwidth*wmass)**2)
     & *s(5,6)**2/((s(5,6)-wmass**2)**2+(wwidth*wmass)**2)
      
      msq(0,0)=msqgg*fac

      return
      end
      
      
