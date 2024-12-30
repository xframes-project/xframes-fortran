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

module font_definitions
    implicit none

    type :: FontDef
        character(len=:), allocatable :: name
        integer :: size
    end type FontDef

end module font_definitions

program main
    use c_interface
    use iso_c_binding
    use json_module
    use font_definitions
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    type(c_funptr) :: onInitPtr, onTextChangedPtr

    type(json_core) :: json
    type(json_value), pointer :: p, inp, defs

    integer :: i

    type(FontDef), dimension(:), allocatable :: fontDefs

    character(len=:), allocatable :: fontDefsJson

    allocate(fontDefs(8))

    fontDefs(1) = FontDef("roboto-regular", 16)
    fontDefs(2) = FontDef("roboto-regular", 18)
    fontDefs(3) = FontDef("roboto-regular", 20)
    fontDefs(4) = FontDef("roboto-regular", 24)
    fontDefs(5) = FontDef("roboto-regular", 28)
    fontDefs(6) = FontDef("roboto-regular", 32)
    fontDefs(7) = FontDef("roboto-regular", 36)
    fontDefs(8) = FontDef("roboto-regular", 48)

    call json%initialize()

    call json%create_object(p, '')

    call json%create_object(defs, 'defs')

    do i = 1, size(fontDefs)
        call json%create_object(inp, 'fontDef')
        call json%add(inp, 'name', fontDefs(i)%name)
        call json%add(inp, 'size', fontDefs(i)%size)
        call json%add(defs, inp)
        nullify(inp)
    end do

    call json%add(p, defs)

    call json%serialize(p, fontDefsJson)

    print *, "JSON String: ", fontDefsJson

    ! Clean up
    call json%destroy(p)
    if (json%failed()) stop 1

    

    ! Assign callback function pointers
    onInitPtr = c_funloc(myInit)
    onTextChangedPtr = c_funloc(myTextChanged)

    ! Call the init function
    call init("assets/", fontDefsJson, "style_overrides", onInitPtr, onTextChangedPtr, c_null_funptr, c_null_funptr, c_null_funptr, c_null_funptr, c_null_funptr)

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
