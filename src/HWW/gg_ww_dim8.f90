module gg_ww_dim8
  implicit none
  include 'types.f'
  
  private

  public :: A1pp,A2pp,A3pp,A4pp,A5pp,A6pp
  public :: A1pm,A3pm,A4pm,A5pm

contains

  function A1pp(za,zb) result(res)
    complex(dp) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = ((za(1,4)*za(2,3)-za(1,3)*za(2,4))*&
         &(za(1,6)*za(2,5)-za(1,5)*za(2,6))*&
         &(zb(4,2)**2*zb(6,1)**2+zb(4,1)**2*zb(6,2)**2))/za(1,2)**2
    
  end function A1pp

  function A2pp(za,zb) result(res)
    complex(dp) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = -(zb(2,1)**2*((za(3,5)*zb(5,4)+za(3,6)*zb(6,4))*&
         &(za(3,5)*zb(6,3)+za(4,5)*zb(6,4))+zb(5,3)*zb(6,4)*&
         &(za(3,5)*zb(5,3)+za(4,5)*zb(5,4)+za(3,6)*zb(6,3)+za(4,6)*zb(6,4))))

  end function A2pp

  function A3pp(za,zb) result(res)
    complex(dp) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res =(za(1,5)**2*za(2,3)**2*(zb(3,2)*zb(4,1)-zb(3,1)*zb(4,2))*&
         &(zb(5,2)*zb(6,1)-zb(5,1)*zb(6,2))+za(2,5)*(za(2,5)*za(1,3)**2*&
         &(zb(3,2)*zb(4,1)-zb(3,1)*zb(4,2))*(zb(5,2)*zb(6,1)-zb(5,1)*zb(6,2))&
         &+za(1,6)*za(2,4)*za(1,3)*(zb(4,2)*zb(6,1)-zb(4,1)*zb(6,2))**2&
         &-za(1,4)*za(1,6)*za(2,3)*(zb(4,2)*zb(6,1)-zb(4,1)*zb(6,2))**2)&
         &+za(1,5)*(za(1,4)*za(2,3)*za(2,6)*(zb(4,2)*zb(6,1)-zb(4,1)&
         &*zb(6,2))**2-za(1,3)*(za(2,4)*za(2,6)*(zb(4,2)*zb(6,1)&
         &-zb(4,1)*zb(6,2))**2+2*za(2,3)*za(2,5)*(zb(3,2)*zb(4,1)&
         &-zb(3,1)*zb(4,2))*(zb(5,2)*zb(6,1)-zb(5,1)*zb(6,2)))))/za(1,2)**2

  end function A3pp

  function A4pp(za,zb) result(res)
    complex(dp) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = (-za(2,3)*za(2,5)*zb(4,2)*(za(1,3)*zb(3,2)+za(1,4)*zb(4,2))&
         &*zb(6,1)*(za(1,5)*zb(5,1)+za(1,6)*zb(6,1))+za(1,5)*za(2,3)&
         &*zb(4,2)*(za(1,3)*zb(3,2)+za(1,4)*zb(4,2))*zb(6,1)&
         &*(za(2,5)*zb(5,1)+za(2,6)*zb(6,1))-za(2,3)*za(2,5)*zb(4,1)&
         &*(za(1,3)*zb(3,1)+za(1,4)*zb(4,1))*zb(6,2)*(za(1,5)*zb(5,2)&
         &+za(1,6)*zb(6,2))+za(1,3)*za(2,5)*zb(4,1)*(za(2,3)*zb(3,1)&
         &+za(2,4)*zb(4,1))*zb(6,2)*(za(1,5)*zb(5,2)+za(1,6)*zb(6,2))&
         &+za(1,5)*za(2,3)*zb(4,1)*(za(1,3)*zb(3,1)+za(1,4)*zb(4,1))&
         &*zb(6,2)*(za(2,5)*zb(5,2)+za(2,6)*zb(6,2))-za(1,3)*za(1,5)&
         &*zb(4,1)*(za(2,3)*zb(3,1)+za(2,4)*zb(4,1))*zb(6,2)*(za(2,5)&
         &*zb(5,2)+za(2,6)*zb(6,2))+za(1,2)*zb(2,1)*(za(1,3)*zb(3,2)&
         &+za(1,4)*zb(4,2))*zb(5,3)*(za(2,5)*zb(5,1)+za(2,6)*zb(6,1))&
         &*zb(6,4)+za(1,2)*zb(2,1)*(za(2,3)*zb(3,1)+za(2,4)*zb(4,1))&
         &*zb(5,3)*(za(1,5)*zb(5,2)+za(1,6)*zb(6,2))*zb(6,4)+za(1,2)&
         &*za(2,3)*zb(2,1)*zb(4,1)*(za(1,5)*zb(5,2)+za(1,6)*zb(6,2))&
         &*(za(3,5)*zb(6,3)+za(4,5)*zb(6,4))-za(1,2)*za(1,5)*za(2,3)&
         &*zb(2,1)*zb(4,1)*zb(6,2)*(za(3,5)*zb(5,3)+za(4,5)*zb(5,4)&
         &+za(3,6)*zb(6,3)+za(4,6)*zb(6,4))-za(1,2)*zb(2,1)*(za(3,5)&
         &*zb(5,4)+za(3,6)*zb(6,4))*(-za(2,5)*(za(1,3)*zb(3,2)+za(1,4)&
         &*zb(4,2))*zb(6,1)+za(1,5)*(za(2,3)*zb(3,2)+za(2,4)*zb(4,2))&
         &*zb(6,1)+za(2,5)*(za(1,3)*zb(3,1)+za(1,4)*zb(4,1))*zb(6,2)&
         &-za(1,5)*(za(2,3)*zb(3,1)+za(2,4)*zb(4,1))*zb(6,2)-2*za(1,2)&
         &*zb(2,1)*(za(3,5)*zb(6,3)+za(4,5)*zb(6,4)))-za(1,2)*zb(2,1)&
         &*(zb(5,3)*zb(6,4)*((za(2,3)*zb(3,2)+za(2,4)*zb(4,2))&
         &*(za(1,5)*zb(5,1)+za(1,6)*zb(6,1))+(za(1,3)*zb(3,1)&
         &+za(1,4)*zb(4,1))*(za(2,5)*zb(5,2)+za(2,6)*zb(6,2))&
         &-2*za(1,2)*zb(2,1)*(za(3,5)*zb(5,3)+za(4,5)*zb(5,4)&
         &+za(3,6)*zb(6,3)+za(4,6)*zb(6,4)))+za(2,3)*zb(4,2)&
         &*((za(1,5)*zb(5,1)+za(1,6)*zb(6,1))*(za(3,5)*zb(6,3)&
         &+za(4,5)*zb(6,4))-za(1,5)*zb(6,1)*(za(3,5)*zb(5,3)&
         &+za(4,5)*zb(5,4)+za(3,6)*zb(6,3)+za(4,6)*zb(6,4)))&
         &+za(1,3)*zb(4,1)*((za(2,5)*zb(5,2)+za(2,6)*zb(6,2))&
         &*(za(3,5)*zb(6,3)+za(4,5)*zb(6,4))-za(2,5)*zb(6,2)&
         &*(za(3,5)*zb(5,3)+za(4,5)*zb(5,4)+za(3,6)*zb(6,3)&
         &+za(4,6)*zb(6,4))))+za(1,3)*zb(4,2)*((za(1,6)*za(2,5)-za(1,5)&
         &*za(2,6))*(za(2,3)*zb(3,2)+za(2,4)*zb(4,2))*zb(6,1)**2&
         &+za(1,2)*zb(2,1)*(za(2,6)*zb(6,1)*(za(3,5)*zb(6,3)+za(4,5)*zb(6,4))&
         &-za(2,5)*(za(3,5)*(zb(5,3)*zb(6,1)-zb(5,1)*zb(6,3))+zb(6,1)&
         &*(za(3,6)*zb(6,3)+za(4,6)*zb(6,4))+za(4,5)*(zb(5,4)&
         &*zb(6,1)-zb(5,1)*zb(6,4))))))/za(1,2)**2
    
  end function A4pp

  function A5pp(za,zb) result(res)
    complex(dp) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = (za(1,2)*za(1,3)*za(2,5)*zb(2,1)*zb(4,1)*zb(6,2)&
         &-za(1,2)*zb(2,1)*(za(1,3)*za(2,5)*zb(4,2)*zb(6,1)&
         &+za(1,5)*za(2,3)*zb(4,1)*zb(6,2)))/za(1,2)**2

  end function A5pp

  function A6pp(za,zb) result(res)
    complex(dp) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = zb(2,1)**2*za(3,5)*zb(6,4)
    
  end function A6pp


  !-----------------------

  function A1pm(za,zb) result(res)
    complex(dp) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = -(2*(za(1,3)*za(2,4)-za(1,4)*za(2,3))*za(2,5)*zb(6,2)*zb(4,1)**2&
         &*(za(2,5)*(zb(5,1)-zb(5,2))+za(2,6)*(zb(6,1)-zb(6,2)))&
         &+2*za(2,3)*(za(1,5)*za(2,6)-za(1,6)*za(2,5))*zb(4,2)*zb(6,1)**2&
         &*(za(2,3)*(zb(3,1)-zb(3,2))+za(2,4)*(zb(4,1)-zb(4,2))))/&
         &(za(1,2)*zb(2,1))
    
  end function A1pm

  function A3pm(za,zb) result(res)
    complex(dp) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = za(1,5)*za(2,3)*(za(2,3)*(za(2,6)*(zb(4,1)-zb(4,2))*zb(6,1)&
         &*(zb(3,2)*zb(6,1)-zb(3,1)*zb(6,2))+za(2,5)*(zb(3,2)*zb(4,1)&
         &*zb(5,1)*(zb(6,1)-zb(6,2))+zb(3,1)*(zb(4,2)*(zb(5,2)-zb(5,1))&
         &*zb(6,1)+zb(4,1)*(zb(5,1)*zb(6,2)-zb(5,2)*zb(6,1)))))-za(2,4)&
         &*(zb(4,1)-zb(4,2))*(za(2,6)*zb(6,1)*(zb(4,1)*zb(6,2)-zb(4,2)*zb(6,1))&
         &+za(2,5)*zb(4,1)*(zb(5,2)*zb(6,1)-zb(5,1)*zb(6,2))))+za(2,5)&
         &*(za(2,3)*(zb(4,2)*zb(6,1)-zb(4,1)*zb(6,2))*(za(1,6)*zb(6,1)&
         &*(za(2,3)*(zb(3,2)-zb(3,1))+za(2,4)*(zb(4,2)-zb(4,1)))&
         &+za(1,4)*zb(4,1)*(za(2,5)*(zb(5,1)-zb(5,2))+za(2,6)&
         &*(zb(6,1)-zb(6,2))))+za(1,3)*(za(2,4)*zb(4,1)*(zb(6,1)-zb(6,2))&
         &*(za(2,5)*(zb(4,1)*zb(5,2)-zb(4,2)*zb(5,1))+za(2,6)&
         &*(zb(4,1)*zb(6,2)-zb(4,2)*zb(6,1)))+za(2,3)*(za(2,5)&
         &*(zb(3,2)*zb(4,1)*zb(5,1)*(zb(6,2)-zb(6,1))+zb(3,1)&
         &*(zb(4,2)*(zb(5,1)-zb(5,2))*zb(6,1)+zb(4,1)&
         &*(zb(5,2)*zb(6,1)-zb(5,1)*zb(6,2))))-za(2,6)&
         &*(zb(3,2)*zb(4,1)-zb(3,1)*zb(4,2))*zb(6,1)*(zb(6,1)-zb(6,2)))))&
         &/(za(1,2)*zb(2,1))

    
  end function A3pm

  function A4pm(za,zb) result(res)
    complex(dp) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = (-za(2,3)*za(2,5)*zb(4,2)*zb(6,1)*(za(2,3)*zb(3,1)+za(2,4)*zb(4,1))&
         &*(za(1,5)*zb(5,1)+za(1,6)*zb(6,1))+za(1,5)*za(2,3)*zb(4,2)*zb(6,1)&
         &*(za(2,3)*zb(3,1)+za(2,4)*zb(4,1))*(za(2,5)*zb(5,1)+za(2,6)*zb(6,1))&
         &-za(2,3)*za(2,5)*zb(4,1)*zb(6,2)*(za(1,3)*zb(3,1)+za(1,4)*zb(4,1))&
         &*(za(2,5)*zb(5,1)+za(2,6)*zb(6,1))+za(1,3)*za(2,5)*zb(4,1)*zb(6,2)&
         &*(za(2,3)*zb(3,1)+za(2,4)*zb(4,1))*(za(2,5)*zb(5,1)+za(2,6)*zb(6,1))&
         &+za(2,3)*za(2,5)*zb(4,1)*zb(6,2)*(za(1,3)*zb(3,1)+za(1,4)*zb(4,1))&
         &*(za(2,5)*zb(5,2)+za(2,6)*zb(6,2))-za(1,3)*za(2,5)*zb(4,1)*zb(6,2)&
         &*(za(2,3)*zb(3,1)+za(2,4)*zb(4,1))*(za(2,5)*zb(5,2)+za(2,6)*zb(6,2))&
         &+2*za(1,2)*zb(2,1)*zb(5,3)*zb(6,4)*(za(2,3)*zb(3,1)+za(2,4)*zb(4,1))&
         &*(za(2,5)*zb(5,1)+za(2,6)*zb(6,1))+za(1,2)*za(2,5)*zb(2,1)&
         &*(zb(6,1)+zb(6,2))*(za(2,3)*zb(3,1)+za(2,4)*zb(4,1))&
         &*(za(3,5)*zb(5,4)+za(3,6)*zb(6,4))+za(1,2)*za(2,3)*zb(2,1)*zb(4,1)&
         &*(za(2,5)*zb(5,1)+za(2,6)*zb(6,1))*(za(3,5)*zb(6,3)+za(4,5)*zb(6,4))&
         &-za(1,2)*za(2,3)*za(2,5)*zb(2,1)*zb(4,1)*zb(6,2)&
         &*(za(3,5)*zb(5,3)+za(4,5)*zb(5,4)+za(3,6)*zb(6,3)+za(4,6)*zb(6,4))&
         &+za(2,3)*zb(4,2)*((za(1,6)*za(2,5)-za(1,5)*za(2,6))*zb(6,1)**2&
         &*(za(2,3)*zb(3,2)+za(2,4)*zb(4,2))+za(1,2)*zb(2,1)&
         &*(za(2,6)*zb(6,1)*(za(3,5)*zb(6,3)+za(4,5)*zb(6,4))&
         &-za(2,5)*(za(3,5)*(zb(5,3)*zb(6,1)-zb(5,1)*zb(6,3))&
         &+zb(6,1)*(za(3,6)*zb(6,3)+za(4,6)*zb(6,4))+za(4,5)&
         &*(zb(5,4)*zb(6,1)-zb(5,1)*zb(6,4))))))/(za(1,2)*zb(2,1))
    
  end function A4pm

  function A5pm(za,zb) result(res)
    complex(dp) :: za(:,:), zb(:,:)
    complex(dp) :: res

    res = -(za(2,3)*za(2,5)*zb(4,2)*zb(6,1)+za(2,3)*za(2,5)*zb(4,1)*zb(6,2))
    
  end function A5pm

  
end module gg_ww_dim8
