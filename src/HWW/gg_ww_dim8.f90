module gg_ww_dim8
  implicit none
  include 'types.f'
  
  private

  public :: A1pp,A1mm,A2pp,A3pp,A5pp
  public :: A1pm,A1mp,A5pm,A5mp

contains

  function A1pp(za,zb) result(res)
    complex(dp), intent(in) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = za(3,4)*za(5,6)*((zb(1,4)*zb(2,6))**2+&
         &(zb(1,6)*zb(2,4))**2)

  end function A1pp

  function A1mm(za,zb) result(res)
    complex(dp), intent(in) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = zb(3,4)*zb(5,6)*((za(1,3)*za(2,5))**2+&
         &(za(1,5)*za(2,3))**2)

  end function A1mm

  function A2pp(za,zb) result(res)
    complex(dp), intent(in) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = (zb(1,2)**2)*(za(3,4)*za(5,6)*zb(4,6)**2&
    &+zb(3,4)*zb(5,6)*za(3,5)**2)

  end function A2pp

  function A3pp(za,zb) result(res)
    complex(dp), intent(in) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = -(zb(1,2)**2)*(za(3,4)*za(5,6)*zb(4,6)**2&
    &-zb(3,4)*zb(5,6)*za(3,5)**2)

  end function A3pp

  function A5pp(za,zb) result(res)
    complex(dp), intent(in) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = -(zb(1,2)**2)*za(3,5)*zb(4,6)

  end function A5pp


  !---------------- Other Sign terms -----------------------

  function A1pm(za,zb) result(res)
    complex(dp), intent(in) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = -(za(3,4)*zb(5,6)*((za(2,5)*zb(1,4))**2)+&
         &zb(3,4)*za(5,6)*((za(2,3)*zb(1,6))**2))
    
  end function A1pm

  function A1mp(za,zb) result(res)
    complex(dp), intent(in) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = -(za(3,4)*zb(5,6)*((za(1,5)*zb(2,4))**2)+&
         &zb(3,4)*za(5,6)*((za(1,3)*zb(2,6))**2))

  end function A1mp

  function A5pm(za,zb) result(res)
    complex(dp), intent(in) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = 2*za(2,3)*za(2,5)*zb(1,4)*zb(1,6)
    
  end function A5pm

  function A5mp(za,zb) result(res)
    complex(dp), intent(in) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = 2*za(1,3)*za(1,5)*zb(2,4)*zb(2,6)

  end function A5mp

  
end module gg_ww_dim8

