      function ovdot(p1,p2)
      implicit none
      include 'types.f'
      real(dp):: ovdot,p1(4),p2(4)
      ovdot=p1(4)*p2(4)-p1(1)*p2(1)-p1(2)*p2(2)-p1(3)*p2(3)
      return
      end
