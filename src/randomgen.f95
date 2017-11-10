module randomgen
  implicit none
  integer, parameter, private :: ik=selected_int_kind(18)
  integer, parameter, private :: rk=selected_real_kind(10,40)	!double
  integer(kind=ik), parameter, private :: a=16807
  integer(kind=ik), parameter, private :: r=2836
  integer(kind=ik), parameter, private :: q=127773
  integer(kind=ik), private :: seed

contains

  subroutine seed_pm(newseed)
    implicit none
    integer(kind=ik), intent(in) :: newseed
    real(kind=rk) :: dummy 

    if(newseed>0) then
      seed=newseed
    else
      seed=abs(newseed)+23	!to remove negative or 0 seed
    end if
    
      dummy=rand_pm()
    do
      if(dummy>1.0 .or. dummy <0.0) then      
      dummy=rand_pm() !if the first seed is too big this makes the first number not to be negative and out of scale
      else
        exit
      end if
    end do

  end subroutine seed_pm

  !Park and Miller standard generator with Shrage algorithm

  real(kind=rk) function rand_pm()
    implicit none
    integer(kind=ik) :: k
    k=seed/q
    seed = a*(seed-k*q)-r*k

    if(seed<0) then
      seed=seed+a*q+r
    end if

    rand_pm = seed/(1.0d0*(a*q+r))

  end function rand_pm
  
  !generate integer in range [mini,maxi]
  integer function rand_int(mini,maxi)
    integer, intent(in) :: mini, maxi
    
    rand_int = mini + rand_pm()*(maxi-mini+1)
  
  end function rand_int
  
  

end module randomgen
