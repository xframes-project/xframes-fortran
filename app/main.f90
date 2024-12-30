module imgui_col
    implicit none

    ! Define the enum type ImGuiCol as integer constants
    integer, parameter :: Text = 0
    integer, parameter :: TextDisabled = 1
    integer, parameter :: WindowBg = 2
    integer, parameter :: ChildBg = 3
    integer, parameter :: PopupBg = 4
    integer, parameter :: Border = 5
    integer, parameter :: BorderShadow = 6
    integer, parameter :: FrameBg = 7
    integer, parameter :: FrameBgHovered = 8
    integer, parameter :: FrameBgActive = 9
    integer, parameter :: TitleBg = 10
    integer, parameter :: TitleBgActive = 11
    integer, parameter :: TitleBgCollapsed = 12
    integer, parameter :: MenuBarBg = 13
    integer, parameter :: ScrollbarBg = 14
    integer, parameter :: ScrollbarGrab = 15
    integer, parameter :: ScrollbarGrabHovered = 16
    integer, parameter :: ScrollbarGrabActive = 17
    integer, parameter :: CheckMark = 18
    integer, parameter :: SliderGrab = 19
    integer, parameter :: SliderGrabActive = 20
    integer, parameter :: Button = 21
    integer, parameter :: ButtonHovered = 22
    integer, parameter :: ButtonActive = 23
    integer, parameter :: Header = 24
    integer, parameter :: HeaderHovered = 25
    integer, parameter :: HeaderActive = 26
    integer, parameter :: Separator = 27
    integer, parameter :: SeparatorHovered = 28
    integer, parameter :: SeparatorActive = 29
    integer, parameter :: ResizeGrip = 30
    integer, parameter :: ResizeGripHovered = 31
    integer, parameter :: ResizeGripActive = 32
    integer, parameter :: Tab = 33
    integer, parameter :: TabHovered = 34
    integer, parameter :: TabActive = 35
    integer, parameter :: TabUnfocused = 36
    integer, parameter :: TabUnfocusedActive = 37
    integer, parameter :: PlotLines = 38
    integer, parameter :: PlotLinesHovered = 39
    integer, parameter :: PlotHistogram = 40
    integer, parameter :: PlotHistogramHovered = 41
    integer, parameter :: TableHeaderBg = 42
    integer, parameter :: TableBorderStrong = 43
    integer, parameter :: TableBorderLight = 44
    integer, parameter :: TableRowBg = 45
    integer, parameter :: TableRowBgAlt = 46
    integer, parameter :: TextSelectedBg = 47
    integer, parameter :: DragDropTarget = 48
    integer, parameter :: NavHighlight = 49
    integer, parameter :: NavWindowingHighlight = 50
    integer, parameter :: NavWindowingDimBg = 51
    integer, parameter :: ModalWindowDimBg = 52
    integer, parameter :: COUNT = 53

end module imgui_col

module imgui_theme_colors
    implicit none

    character(len=*), parameter :: darkestGrey = "#141f2c"
    character(len=*), parameter :: darkerGrey = "#2a2e39"
    character(len=*), parameter :: darkGrey = "#363b4a"
    character(len=*), parameter :: lightGrey = "#5a5a5a"
    character(len=*), parameter :: lighterGrey = "#7A818C"
    character(len=*), parameter :: evenLighterGrey = "#8491a3"
    character(len=*), parameter :: black = "#0A0B0D"
    character(len=*), parameter :: green = "#75f986"
    character(len=*), parameter :: red = "#ff0062"
    character(len=*), parameter :: white = "#fff"

end module imgui_theme_colors


module imgui_theme
    use json_module  ! Assuming this module handles JSON serialization
    use imgui_col
    use imgui_theme_colors
    implicit none

    type :: Color
        character(len=:), allocatable :: hex_code
        real(4) :: alpha
    end type Color

    type :: ImGuiThemeEntry
        integer :: key  ! The ImGuiCol value, e.g., Text = 0
        type(Color) :: value  ! The corresponding color and alpha
    end type ImGuiThemeEntry

    type :: ImGuiTheme
        type(ImGuiThemeEntry), allocatable :: entries(:)  ! List of key-value pairs
    end type ImGuiTheme

contains

    subroutine init_theme(theme)
        type(ImGuiTheme), intent(out) :: theme
        integer :: i

        ! Initialize the entries with the COUNT of ImGuiCol
        allocate(theme%entries(0:COUNT-1))

        theme%entries(Text) = ImGuiThemeEntry(Text, Color(white, 1.0))
        theme%entries(TextDisabled) = ImGuiThemeEntry(TextDisabled, Color(lighterGrey, 1.0))
        theme%entries(WindowBg) = ImGuiThemeEntry(WindowBg, Color(black, 1.0))
        theme%entries(ChildBg) = ImGuiThemeEntry(ChildBg, Color(black, 1.0))
        theme%entries(PopupBg) = ImGuiThemeEntry(PopupBg, Color(white, 1.0))
        theme%entries(Border) = ImGuiThemeEntry(Border, Color(lightGrey, 1.0))
        theme%entries(BorderShadow) = ImGuiThemeEntry(BorderShadow, Color(darkestGrey, 1.0))
        theme%entries(FrameBg) = ImGuiThemeEntry(FrameBg, Color(black, 1.0))
        theme%entries(FrameBgHovered) = ImGuiThemeEntry(FrameBgHovered, Color(darkerGrey, 1.0))
        theme%entries(FrameBgActive) = ImGuiThemeEntry(FrameBgActive, Color(lightGrey, 1.0))
        theme%entries(TitleBg) = ImGuiThemeEntry(TitleBg, Color(lightGrey, 1.0))
        theme%entries(TitleBgActive) = ImGuiThemeEntry(TitleBgActive, Color(darkerGrey, 1.0))
        theme%entries(TitleBgCollapsed) = ImGuiThemeEntry(TitleBgCollapsed, Color(lightGrey, 1.0))
        theme%entries(MenuBarBg) = ImGuiThemeEntry(MenuBarBg, Color(lightGrey, 1.0))
        theme%entries(ScrollbarBg) = ImGuiThemeEntry(ScrollbarBg, Color(darkerGrey, 1.0))
        theme%entries(ScrollbarGrab) = ImGuiThemeEntry(ScrollbarGrab, Color(darkerGrey, 1.0))
        theme%entries(ScrollbarGrabHovered) = ImGuiThemeEntry(ScrollbarGrabHovered, Color(lightGrey, 1.0))
        theme%entries(ScrollbarGrabActive) = ImGuiThemeEntry(ScrollbarGrabActive, Color(darkestGrey, 1.0))
        theme%entries(CheckMark) = ImGuiThemeEntry(CheckMark, Color(darkestGrey, 1.0))
        theme%entries(SliderGrab) = ImGuiThemeEntry(SliderGrab, Color(darkerGrey, 1.0))
        theme%entries(SliderGrabActive) = ImGuiThemeEntry(SliderGrabActive, Color(lightGrey, 1.0))
        theme%entries(Button) = ImGuiThemeEntry(Button, Color(black, 1.0))
        theme%entries(ButtonHovered) = ImGuiThemeEntry(ButtonHovered, Color(darkerGrey, 1.0))
        theme%entries(ButtonActive) = ImGuiThemeEntry(ButtonActive, Color(black, 1.0))
        theme%entries(Header) = ImGuiThemeEntry(Header, Color(black, 1.0))
        theme%entries(HeaderHovered) = ImGuiThemeEntry(HeaderHovered, Color(black, 1.0))
        theme%entries(HeaderActive) = ImGuiThemeEntry(HeaderActive, Color(lightGrey, 1.0))
        theme%entries(Separator) = ImGuiThemeEntry(Separator, Color(darkestGrey, 1.0))
        theme%entries(SeparatorHovered) = ImGuiThemeEntry(SeparatorHovered, Color(lightGrey, 1.0))
        theme%entries(SeparatorActive) = ImGuiThemeEntry(SeparatorActive, Color(lightGrey, 1.0))
        theme%entries(ResizeGrip) = ImGuiThemeEntry(ResizeGrip, Color(black, 1.0))
        theme%entries(ResizeGripHovered) = ImGuiThemeEntry(ResizeGripHovered, Color(lightGrey, 1.0))
        theme%entries(ResizeGripActive) = ImGuiThemeEntry(ResizeGripActive, Color(darkerGrey, 1.0))
        theme%entries(Tab) = ImGuiThemeEntry(Tab, Color(black, 1.0))
        theme%entries(TabHovered) = ImGuiThemeEntry(TabHovered, Color(darkerGrey, 1.0))
        theme%entries(TabActive) = ImGuiThemeEntry(TabActive, Color(lightGrey, 1.0))
        theme%entries(TabUnfocused) = ImGuiThemeEntry(TabUnfocused, Color(black, 1.0))
        theme%entries(TabUnfocusedActive) = ImGuiThemeEntry(TabUnfocusedActive, Color(lightGrey, 1.0))
        theme%entries(PlotLines) = ImGuiThemeEntry(PlotLines, Color(darkerGrey, 1.0))
        theme%entries(PlotLinesHovered) = ImGuiThemeEntry(PlotLinesHovered, Color(lightGrey, 1.0))
        theme%entries(PlotHistogram) = ImGuiThemeEntry(PlotHistogram, Color(darkerGrey, 1.0))
        theme%entries(PlotHistogramHovered) = ImGuiThemeEntry(PlotHistogramHovered, Color(lightGrey, 1.0))
        theme%entries(TableHeaderBg) = ImGuiThemeEntry(TableHeaderBg, Color(black, 1.0))
        theme%entries(TableBorderStrong) = ImGuiThemeEntry(TableBorderStrong, Color(lightGrey, 1.0))
        theme%entries(TableBorderLight) = ImGuiThemeEntry(TableBorderLight, Color(darkerGrey, 1.0))
        theme%entries(TableRowBg) = ImGuiThemeEntry(TableRowBg, Color(darkGrey, 1.0))
        theme%entries(TableRowBgAlt) = ImGuiThemeEntry(TableRowBgAlt, Color(darkerGrey, 1.0))
        theme%entries(TextSelectedBg) = ImGuiThemeEntry(TextSelectedBg, Color(darkerGrey, 1.0))
        theme%entries(DragDropTarget) = ImGuiThemeEntry(DragDropTarget, Color(darkerGrey, 1.0))
        theme%entries(NavHighlight) = ImGuiThemeEntry(NavHighlight, Color(darkerGrey, 1.0))
        theme%entries(NavWindowingHighlight) = ImGuiThemeEntry(NavWindowingHighlight, Color(darkerGrey, 1.0))
        theme%entries(NavWindowingDimBg) = ImGuiThemeEntry(NavWindowingDimBg, Color(darkerGrey, 1.0))
        theme%entries(ModalWindowDimBg) = ImGuiThemeEntry(ModalWindowDimBg, Color(darkerGrey, 1.0))

    end subroutine init_theme

    subroutine serialize_theme_to_json(theme, json_string)
        type(ImGuiTheme), intent(in) :: theme
        character(len=:), allocatable :: json_string
        type(json_core) :: json
        type(json_value), pointer :: obj, color_tuple
        integer :: i

        call json%initialize()
        call json%create_object(obj, '')

        do i = 0, size(theme%entries) - 1
            call json%create_array(color_tuple, to_string(theme%entries(i)%key))
            call json%add(color_tuple, '', theme%entries(i)%value%hex_code)
            call json%add(color_tuple, '', int(theme%entries(i)%value%alpha))
            call json%add(obj, color_tuple)
 
            nullify(color_tuple)
        end do

        call json%serialize(obj, json_string)
        call json%destroy(obj)
    end subroutine serialize_theme_to_json

    function to_string(value) result(string)
        integer, intent(in) :: value
        character(len=16) :: string  ! Assuming the integer won't exceed this
        write(string, '(I0)') value
    end function to_string

end module imgui_theme


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
    use imgui_theme
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    type(c_funptr) :: onInitPtr, onTextChangedPtr

    type(json_core) :: json
    type(json_value), pointer :: p, inp, defs

    type(ImGuiTheme) :: theme

    integer :: i

    type(FontDef), dimension(:), allocatable :: fontDefs

    character(len=:), allocatable :: fontDefsJson
    character(len=:), allocatable :: themeJson

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

    call json%create_array(defs, 'defs')

    do i = 1, size(fontDefs)
        call json%create_object(inp, '')
        call json%add(inp, 'name', fontDefs(i)%name)
        call json%add(inp, 'size', fontDefs(i)%size)
        call json%add(defs, inp)
        nullify(inp)
    end do

    call json%add(p, defs)

    call json%serialize(p, fontDefsJson)

    print *, fontDefsJson

    ! Clean up
    call json%destroy(p)
    if (json%failed()) stop 1

    call init_theme(theme)
    call serialize_theme_to_json(theme, themeJson)

    print *, themeJson

    ! Assign callback function pointers
    onInitPtr = c_funloc(myInit)
    onTextChangedPtr = c_funloc(myTextChanged)

    ! Call the init function
    call init("./assets", fontDefsJson, themeJson, onInitPtr, onTextChangedPtr, c_null_funptr, c_null_funptr, c_null_funptr, c_null_funptr, c_null_funptr)

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

