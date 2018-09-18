      function tloop(s23,mtsq)
        use mod_qcdloop_c
      implicit none
      include 'types.f'
      include 'constants.f'
      include 'scale.f'
      complex(dp):: tloop
      real(dp):: s23,mtsq
      complex(dp):: Bdiff

      tloop=-1._dp-2._dp*mtsq/s23*(
     & 3._dp*s23*qlI3(zip,zip,s23,mtsq,mtsq,mtsq,musq,0)
     & +6._dp*Bdiff(s23,zip,mtsq)+12._dp)
      return
      end
