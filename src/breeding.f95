module breeding
    use length_measurement
    use randomgen
    use qsort_mod
    implicit none  
    integer, allocatable, private :: routes(:,:), routes_order(:)
    
contains

    subroutine init_routes(n_of_cities,m)
        implicit none
        integer, intent(in) :: n_of_cities, m
        
        allocate(routes(n_of_cities,m))
        allocate(routes_order(m))
        
    end subroutine init_routes
   
    function gen_route(n)
        implicit none
        integer, intent(in) :: n
        integer :: gen_route(1:n)
        integer :: i, rint
        
        gen_route = 0
        i = 1
        do
            if(i>n) exit
            rint = rand_int(1,n)
            if(any(gen_route==rint)) then
                cycle
            else
                gen_route(i) = rint
                i = i + 1
            end if
        end do
        
    end function gen_route
    
    subroutine gen_all_routes()
        implicit none
        integer :: i
        
        do i=1,size(routes(1,:))
            routes(:,i) = gen_route(size(routes(:,1)))
        end do
        
    end subroutine gen_all_routes
        
    subroutine sort_routes()
    !using rosetta code implementation of quicksort
        implicit none
        integer :: i,l
        type (group), allocatable :: A(:)
        l = size(routes(1,:))

        allocate(A(1:l))
 
        do i = 1, l
            A(i)%value = route_length(routes(:,i),size(routes(:,1)))
            A(i)%order = i
        end do
     
        call QSort(A,l)
        
        routes_order = A%order
                
    end subroutine sort_routes
    
    subroutine breed(m, n)
        implicit none
        integer, intent(in) :: m,n !n=number of cities
        integer :: child_route(n), i, j, selector, &
                   sugg1, sugg2
        
        call sort_routes()
        
        !take first half, replace them with breeded ones
        
        all_routes: do j = 1, m/2, 2

            child_route = 0
            child_route(1) = routes(1,routes_order(j))

            all_cities: do i = 2,n  
           
                sugg1 = get_next_city(routes(:,routes_order(j)), n, child_route(i-1))
                sugg2 = get_next_city(routes(:,routes_order(j+1)), n, child_route(i-1))

                selector = 0
                if(any(child_route == sugg1)) then
                    !sugg1 is in list
                    selector = selector + 1
                end if
        
                if(any(child_route == sugg2)) then
                    !sugg2 is in list
                    selector = selector + 2
                end if
                
                !which one is closer          
                if(distance2(sugg1,child_route(i-1)) <= distance2(sugg2,child_route(i-1))) then
                    !sugg1 is better
                    selector = selector + 10
                end if
                
                select case(selector)
                    case(0,1,11)
                        !sugg2 faster or only one not in list
                        child_route(i) = sugg2
                    case(2,10,12)
                        !sugg2 faster or only one not in list
                        child_route(i) = sugg1
                    case(3, 13)
                        !both on list
                        !take about random city, in this choose parent randomly
                        !and choose next city randomly using randomly
                        
                        do
                            if(rand_pm()>=0.5d0) then
                                sugg1 = get_next_city(routes(:,routes_order(j)), n, sugg1)
                                if(.not. any(child_route==sugg1)) then
                                    child_route(i) = sugg1
                                    exit
                                end if
                            else
                                sugg2 = get_next_city(routes(:,routes_order(j+1)), n, sugg2)
                                if(.not. any(child_route==sugg2)) then
                                    child_route(i) = sugg2
                                end if
                                    exit
                            end if
                        end do
                end select
                
            end do all_cities
            
            !if child is slower than parent, mutate parent instead
            if(route_length(routes(:,routes_order(j)), n) < route_length(child_route,n)) then
                child_route = routes(:,routes_order(j))
            end if
            
            !mutate (swap two random cities)
            routes(:,routes_order(j)) = child_route
            routes(:,routes_order(j+1)) = mutate(child_route, n)
            routes(:,routes_order(m/2 + j)) = mutate(mutate(child_route, n),n)
            routes(:,routes_order(m/2 + j+1)) = mutate(mutate(child_route, n),n)
!            routes(:,routes_order(m/2 + j+1)) = gen_route(size(routes(:,1)))

        end do all_routes        
        
        !replace bad ones with new random routes
!        do i=m/2 + 1, m
!            routes(:,routes_order(i)) = gen_route(size(routes(:,1)))
!        end do
                
    end subroutine breed
 
    function mutate(child,n)
        implicit none
        integer, intent(in) :: n, child(n) !n=n_of_cities
        integer :: mutate(n)
        integer :: i, j
        
        mutate = child
        
        i = rand_int(1,n)
        do
            j = rand_int(1,n)
            if(i/=j) exit
        end do
        mutate(i) = child(j)
        mutate(j) = child(i)
        
    end function mutate    

 
    !return the fastest n (amount) routes, expect that routes list is sorted   
    function get_n_fastest(amount,n_of_cities,m_of_routes)
        implicit none
        integer, intent(in) :: amount, n_of_cities, m_of_routes
        integer :: get_n_fastest(n_of_cities, amount), i
        
        get_n_fastest = routes(:,[(routes_order(i),i=1,4)])
        
    end function get_n_fastest
    
    subroutine replace_n_worst(better_ones, amount, n_of_cities, m_of_routes)
        implicit none
        integer, intent(in) :: better_ones(n_of_cities, amount),amount, n_of_cities, m_of_routes        
        integer :: i
        
        routes(:,[(routes_order(i),i=m_of_routes-amount + 1,m_of_routes)]) &
     &         = better_ones(:,1:amount)
        
    end subroutine replace_n_worst
    
    integer function get_next_city(route,n,this_city)
        implicit none
        integer, intent(in) :: this_city, n, route(n)
        integer :: index_now, i
        
        !get current index of the city in this particular route
        do i = 1, n
            if(route(i)==this_city) then
                index_now = i
                exit
            end if
        end do
        
        !next city (not the index in this route list but in the coordinates list)
        if(index_now==n) then
            get_next_city = route(1)
        else
            get_next_city = route(i+1)
        end if 
        
    end function get_next_city

    !Return length of the fastest route:    
    real(kind=rk) function get_fastest(n)
        implicit none
        integer, intent(in) :: n!(n=number of cities)
        
        get_fastest = route_length(routes(:,routes_order(1)),n)
        
    end function get_fastest
    
end module breeding
