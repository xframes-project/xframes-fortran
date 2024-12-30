module c_interface
    use iso_c_binding
    implicit none

    interface
        subroutine OnInitCb() bind(C)
        end subroutine OnInitCb

        subroutine OnTextChangedCb(index, text) bind(C)
            import :: c_int, c_char
            integer(c_int), value :: index
            character(c_char), dimension(*), intent(in) :: text
        end subroutine OnTextChangedCb

        subroutine OnComboChangedCb(index, comboIndex) bind(C)
            import :: c_int
            integer(c_int), value :: index
            integer(c_int), value :: comboIndex
        end subroutine OnComboChangedCb

        subroutine OnNumericValueChangedCb(index, value) bind(C)
            import :: c_int, c_float
            integer(c_int), value :: index
            real(c_float), value :: value
        end subroutine OnNumericValueChangedCb

        subroutine OnBooleanValueChangedCb(index, value) bind(C)
            import :: c_int, c_bool
            integer(c_int), value :: index
            logical(c_bool), value :: value
        end subroutine OnBooleanValueChangedCb

        subroutine OnMultipleNumericValuesChangedCb(index, values, numValues) bind(C)
            import :: c_int, c_float
            integer(c_int), value :: index
            real(c_float), dimension(*), intent(in) :: values
            integer(c_int), value :: numValues
        end subroutine OnMultipleNumericValuesChangedCb

        subroutine OnClickCb(index) bind(C)
            import :: c_int
            integer(c_int), value :: index
        end subroutine OnClickCb
    end interface

    ! Declare the 'init' function
    interface
        subroutine init(assetsBasePath, rawFontDefinitions, rawStyleOverrideDefinitions, onInit, onTextChanged, onComboChanged, onNumericValueChanged, onBooleanValueChanged, onMultipleNumericValuesChanged, onClick) bind(C)
            import :: c_char, c_funptr
            character(c_char), dimension(*), intent(in) :: assetsBasePath
            character(c_char), dimension(*), intent(in) :: rawFontDefinitions
            character(c_char), dimension(*), intent(in) :: rawStyleOverrideDefinitions
            type(c_funptr), value :: onInit
            type(c_funptr), value :: onTextChanged
            type(c_funptr), value :: onComboChanged
            type(c_funptr), value :: onNumericValueChanged
            type(c_funptr), value :: onBooleanValueChanged
            type(c_funptr), value :: onMultipleNumericValuesChanged
            type(c_funptr), value :: onClick
        end subroutine init
    end interface
end module c_interface

program main
    use c_interface
    use iso_c_binding
    implicit none

    type(c_funptr) :: onInitPtr, onTextChangedPtr

    ! Assign callback function pointers
    onInitPtr = c_funloc(myInit)
    onTextChangedPtr = c_funloc(myTextChanged)

    ! Call the init function
    call init("assets/", "font_defs", "style_overrides", onInitPtr, onTextChangedPtr, c_null_funptr, c_null_funptr, c_null_funptr, c_null_funptr, c_null_funptr)

    print *, "Press Enter to exit the program..."
    read(*, *)
contains

    subroutine myInit() bind(C)
        print *, "Initialization callback invoked."
    end subroutine myInit

    subroutine myTextChanged(index, text) bind(C)
        use iso_c_binding, only: c_int, c_char
        integer(c_int), value :: index
        character(c_char), dimension(*), intent(in) :: text
        integer :: i

        ! Workaround to process the assumed-size character array
        print *, "Text changed in widget", index
        print *, "New text:"
        i = 1
        do while (text(i) /= c_null_char)
            write(*, '(A)', advance="no") text(i:i)
            i = i + 1
        end do
        print *  ! Finalize the line
    end subroutine myTextChanged

end program main

