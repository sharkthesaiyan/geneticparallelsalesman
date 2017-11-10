!Code from:
!http://rosettacode.org/wiki/Sorting_algorithms/Quicksort
!Site visited 19.12.2015. 
!Slight modifications (19.12.2015, randomgenerator changed)
module qsort_mod
    use randomgen   
    implicit none
    integer, parameter, private :: rk = selected_real_kind(10,40) !double
 
type group
    integer :: order    ! original order of unsorted data
    real(kind=rk) :: value       ! values to be sorted by
end type group
 
contains
 
recursive subroutine QSort(a,na)
 
! DUMMY ARGUMENTS
integer, intent(in) :: nA
type (group), dimension(nA), intent(in out) :: A
 
! LOCAL VARIABLES
integer :: left, right
real(kind=rk) :: random
real(kind=rk) :: pivot
type (group) :: temp
integer :: marker
 
    if (nA > 1) then
 
        random = rand_pm()
        pivot = A(int(random*real(nA-1))+1)%value   ! random pivor (not best performance, but avoids worst-case)
        left = 0
        right = nA + 1
 
        do while (left < right)
            right = right - 1
            do while (A(right)%value > pivot)
                right = right - 1
            end do
            left = left + 1
            do while (A(left)%value < pivot)
                left = left + 1
            end do
            if (left < right) then
                temp = A(left)
                A(left) = A(right)
                A(right) = temp
            end if
        end do
 
        if (left == right) then
            marker = left + 1
        else
            marker = left
        end if
 
        call QSort(A(:marker-1),marker-1)
        call QSort(A(marker:),nA-marker+1)
 
    end if
 
end subroutine QSort
 
end module qsort_mod
