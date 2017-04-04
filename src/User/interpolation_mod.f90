module interpolation_mod
	implicit none
contains

	integer function binarysearch(length, array, value, delta)
		! Given an array and a value, returns the index of the element that
		! is closest to, but less than, the given value.
		! Uses a binary search algorithm.
		! "delta" is the tolerance used to determine if two values are equal
		! if ( abs(x1 - x2) <= delta) then
		!    assume x1 = x2
		! endif
	
		implicit none
		integer, intent(in) :: length
		double precision, dimension(length), intent(in) :: array
		double precision, intent(in) :: value
		double precision, intent(in), optional :: delta
	
		integer :: left, middle, right
		double precision :: d
	
		if (present(delta) .eqv. .true.) then
			d = delta
		else
			d = 1e-9
		endif
	
		left = 1
		right = length
		do
			if (left > right) then
				exit
			endif
			middle = nint((left+right) / 2.0)
			if ( abs(array(middle) - value) <= d) then
				binarysearch = middle
				return
			else if (array(middle) > value) then
				right = middle - 1
			else
				left = middle + 1
			endif
		end do
		binarysearch = right
	end function binarysearch
		
	double precision function interpolate(x_len, x_array, y_len, y_array, f, x, y, delta)
		! This function uses bilinear interpolation to estimate the value
		! of a function f at point (x,y)
		! f is assumed to be sampled on a regular grid, with the grid x values specified
		! by x_array and the grid y values specified by y_array
		! Reference: http://en.wikipedia.org/wiki/Bilinear_interpolation
		implicit none
		integer, intent(in) :: x_len, y_len           
		double precision, dimension(x_len), intent(in) :: x_array
		double precision, dimension(y_len), intent(in) :: y_array
		double precision, dimension(x_len, y_len), intent(in) :: f
		double precision, intent(in) :: x,y
		double precision, intent(in), optional :: delta
	
		double precision :: res
	
		double precision :: denom, x1, x2, y1, y2
		integer :: i,j
	
		i = binarysearch(x_len, x_array, x)
		j = binarysearch(y_len, y_array, y)
	
		x1 = x_array(i)
		x2 = x_array(i+1)
	
		y1 = y_array(j)
		y2 = y_array(j+1)
	
		denom = (x2 - x1)*(y2 - y1)
	
		interpolate = (f(i,j)*(x2-x)*(y2-y) + f(i+1,j)*(x-x1)*(y2-y) + &
			f(i,j+1)*(x2-x)*(y-y1) + f(i+1, j+1)*(x-x1)*(y-y1))/denom
	
	end function interpolate

end module interpolation_mod