program asd
    use randomgen
    implicit none
    integer(8) :: seed
    integer :: i, n
    character(len=80) :: arg
    
    if(command_argument_count()==2) then
        call get_command_argument(1,arg)
        read(arg,*), n
        call get_command_argument(2,arg)
        read(arg,*), seed
    else
        n = 100
        seed = 715791
    end if

    call seed_pm(seed)

    open(unit=1, file="default_input.dat", status="replace")

    write(1,*), n

    do i=1, n
        write(1,*), rand_pm()*50.0d0, rand_pm()*50.0d0
    end do

    close(1)

end program asd
