module fortran_function
    use iso_c_binding
    implicit none
contains
    ! Fortran function to be passed to C
    function my_function(x) result(res) bind(C)
        real(c_double), value :: x
        real(c_double) :: res

        ! A simple calculation
        res = x * x
    end function my_function
end module fortran_function

program pass_function
    use iso_c_binding
    use fortran_function
    implicit none

    interface
        ! Interface for the C function
        subroutine call_function(f, x) bind(C)
            import c_double, c_funptr
            type(c_funptr), value :: f
            real(c_double), value :: x
        end subroutine call_function
    end interface

    ! Declare a C-compatible function pointer
    type(c_funptr) :: fptr

    ! Get a C function pointer for my_function
    fptr = c_funloc(my_function)

    ! Call the C function with the Fortran function pointer
    call call_function(fptr, 3.0_c_double)
end program pass_function
