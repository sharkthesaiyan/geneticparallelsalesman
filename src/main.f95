program salesman
    use mpi
    use length_measurement
    use breeding
    use randomgen
    use qsort_mod
    implicit none
    integer,parameter :: tag = 50  
    integer :: i, j, rc, ntasks, src_id, dest_id, id, nlen
    integer :: max_rounds, max_breeds
    integer,dimension(mpi_status_size) :: status
    integer, allocatable :: root_messages(:,:), proc_messages(:,:)
    character(len=MPI_MAX_PROCESSOR_NAME) :: host
    character(len=clen) :: fname, arg
    real(kind=rk) :: time1, time2, best
    
    call mpi_init(rc)
    if (rc/=mpi_success) then
        if(id==0) then
            print *,'MPI initialization failed.'
        end if
        stop
    end if
    
    call mpi_comm_size(mpi_comm_world,ntasks,rc)
    call mpi_comm_rank(mpi_comm_world,id,rc)
    call mpi_get_processor_name(host,nlen,rc)
    
    time1 = mpi_wtime()
    
    select case(command_argument_count())
        case(1)
            max_rounds=100
            max_breeds=10
            if(id==0) then
                print *, "max_rounds not given, default=100"
                print *, "max_breeds not given, default=10"
            end if
            call get_command_argument(1,fname)
        case(2)
            max_breeds=10
            if(id==0) then
                print *, "max_breeds not given, default=10"
            end if
            call get_command_argument(1,fname)
            call get_command_argument(2,arg)
            read(arg,*) max_rounds
        case(3)
            call get_command_argument(1,fname)
            call get_command_argument(2,arg)
            read(arg,*) max_rounds
            call get_command_argument(3,arg)
            read(arg,*) max_breeds
         case default
            fname = "default_input.dat"
            max_rounds=100
            max_breeds=10
            if(id==0) then
                print *, "Input file not given, trying default_input.dat"
                print *, "max_rounds not given, default=100"
                print *, "max_breeds not given, default=10"
            end if
    end select

    call read_coordinates(fname)
    
    if(id==0) then
        allocate(root_messages(get_city_count(),ntasks*4))
    else
        allocate(proc_messages(get_city_count(),4))
    end if
    
    if(id/=0) then
        !different seeds for each process
        call seed_pm(id*2452619_8)
        call init_routes(get_city_count(), 16)
        call gen_all_routes()
    end if

    do j=1, max_rounds
        if(id/=0) then
        !all but root    
            !sort, kill worst, breed best, generate new ones to replace bad ones
            do i=1, max_breeds
                call breed(16,get_city_count())
            end do
            call sort_routes()
            !------------------
            proc_messages = get_n_fastest(4,get_city_count(),16)
            dest_id=0
            src_id=0
            !Process sends 4 of itse best children to root
            call mpi_send(proc_messages, 4*get_city_count(), mpi_integer, &
     &                    dest_id, tag, mpi_comm_world, rc) 

            !Process receives 4 new children from root (from precious process)
            call mpi_recv(proc_messages,4*get_city_count(), &
     &                    mpi_integer,src_id,tag,mpi_comm_world,status,rc)

            call replace_n_worst(proc_messages, 4, get_city_count(), 16)
        else
        !root
            !root receives data from all processes
            do i=1,ntasks-1
                src_id = i
                call mpi_recv(root_messages(:,(i-1)*4+1:i*4),4*get_city_count(), &
     &                        mpi_integer,src_id,tag,mpi_comm_world,status,rc)
            end do
            !root sends data to all processes
            do i=1, ntasks-1
                dest_id = i + 1
                if(dest_id==ntasks) then
                    dest_id=1
                end if
                call mpi_send(root_messages(:,(i-1)*4+1:i*4), 4*get_city_count(), mpi_integer, &
     &                        dest_id, tag, mpi_comm_world, rc) 
            end do
            
            !Print the length of the best one from each process:
            if(mod(j,max_rounds/10)==1 .or. j==max_rounds) then
                best = route_length(root_messages(:,1),get_city_count())
                do i=1,ntasks-1
                    best = min(best,route_length(root_messages(:,(i-1)*4+1),get_city_count()))
                end do
                print *, best, mpi_wtime() - time1
            end if
        end if

    end do
    
    !left this commented because I didn't use this for anything but in case
    !someone needs the actual routes:
    !do i=1,ntasks-1
    !   print *, root_messages(:,(i-1)*4+1)
    !end do

    call mpi_finalize(rc)


end program salesman
