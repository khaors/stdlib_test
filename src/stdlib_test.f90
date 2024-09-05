module stdlib_test
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, stdlib_test!"
  end subroutine say_hello
end module stdlib_test
