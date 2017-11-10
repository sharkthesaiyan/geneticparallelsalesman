module length_measurement
    implicit none
    integer, parameter :: rk=selected_real_kind(10,40), &!double
     &                    clen = 80 !character length
    real(kind=rk), allocatable, private :: coordinates(:,:)
    integer, private :: n_of_cities

contains

    subroutine read_coordinates(fname)
        implicit none
        character(len=clen),intent(in) :: fname
        integer :: i, n, ios
        
        open(unit=1, file=fname, iostat=ios, status="old")
        
            if(ios/=0) then
                print *, "Error in reading coordinates file"
                stop
            end if
        
            read(1,*), n
            allocate(coordinates(n,2))
            
            do i=1,n
                read(1,*), coordinates(i,1), coordinates(i,2)
            end do
        
        close(1)
        
        n_of_cities = n
        
    end subroutine read_coordinates

    integer function get_city_count()
        implicit none
        get_city_count = n_of_cities
    end function get_city_count

    !order = at which order the route is walked
    real(kind=rk) function route_length(order,n)
        implicit none
        integer, intent(in) :: n
        integer, intent(in) :: order(n)
        integer :: i
        real(kind=rk) :: summ
        
        summ = 0.0d0
                
        !can be parallerized but probably should not
        do i=2,n
            summ = summ + &
     &             sqrt((coordinates(order(i),1) - coordinates(order(i-1),1))**2 &
     &             + (coordinates(order(i),2) - coordinates(order(i-1),2))**2)
        end do
        !to make a complete circle, connect the last and the first city
        summ = summ +&
     &         sqrt((coordinates(order(n),1) - coordinates(order(1),1))**2 &
     &         + (coordinates(order(n),2) - coordinates(order(1),2))**2)
        
        route_length = summ
                
    end function route_length

    real(kind=rk) function distance2(city1,city2)
        !returns the distance squared between two cities
        implicit none
        integer, intent(in) :: city1, city2
        
        distance2 = (coordinates(city1,1) - coordinates(city2,1))**2 &
     &              + (coordinates(city1,2) - coordinates(city2,2))**2
        
    end function distance2

end module length_measurement
