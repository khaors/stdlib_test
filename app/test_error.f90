program test_error
    use stdlib_kinds, only: wp=>dp;
    use stdlib_error, only: error_stop,check;
    !
    real(kind=wp),parameter :: tolerance=1.0d-10
    integer :: a 
    real(kind=wp) :: b,c
    !
    write(*,*) 'Check 1: Failure expected';
    a=2;
    call check(a == 3,msg='Integer comparison failed',warn=.true.);
    !
    write(*,*) 'Check 2: Failure expected';
    b=1.0d-2;
    c=1.4d-2;
    call check(dabs(b-c) < tolerance,msg='Real comparison failed',warn=.true.);
    !
    call error_stop("Invalid argument",code=12);
!
end program test_error