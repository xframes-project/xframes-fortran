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

        call json%initialize(compact_reals=.true.,real_format='*')
        call json%create_object(obj, '')

        do i = 0, size(theme%entries) - 1
            call json%create_array(color_tuple, to_string(theme%entries(i)%key))
            call json%add(color_tuple, '', theme%entries(i)%value%hex_code)
            call json%add(color_tuple, '', theme%entries(i)%value%alpha)
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
    use, intrinsic :: iso_c_binding
    implicit none

    interface
        subroutine init(assetsBasePath, rawFontDefinitions, rawStyleOverrideDefinitions, onInit, onTextChanged, onComboChanged, onNumericValueChanged, onBooleanValueChanged, onMultipleNumericValuesChanged, onClick) bind(C, name="init")
            import :: c_char, c_funptr, c_ptr
            type (c_ptr), value :: assetsBasePath  
            type (c_ptr), value :: rawFontDefinitions
            type (c_ptr), value :: rawStyleOverrideDefinitions
            type(c_funptr), intent(in), value :: onInit
            type(c_funptr), intent(in), value :: onTextChanged
            type(c_funptr), intent(in), value :: onComboChanged
            type(c_funptr), intent(in), value :: onNumericValueChanged
            type(c_funptr), intent(in), value :: onBooleanValueChanged
            type(c_funptr), intent(in), value :: onMultipleNumericValuesChanged
            type(c_funptr), intent(in), value :: onClick
        end subroutine init
        
        ! subroutine setElement(elementJson) bind(C, name="setElement")
        !     import :: c_char
        !     character(c_char), dimension(*), intent(in) :: elementJson
        ! end subroutine setElement
        
        ! subroutine setChildren(id, childrenJson) bind(C, name="setChildren")
        !     use iso_c_binding, only: c_int, c_char
        !     integer(c_int), intent(in) :: id
        !     character(c_char), dimension(*), intent(in) :: childrenJson
        ! end subroutine setChildren
    end interface
end module c_interface

module font_definitions
    implicit none

    type :: FontDef
        character(len=:), allocatable :: name
        integer :: size
    end type FontDef

end module font_definitions

! module xframes
!     use c_interface
!     use iso_c_binding
!     use json_module
!     implicit none

    ! character(len=:), allocatable :: nodeJson

    ! type(json_core) :: xframesJson

! contains
    ! subroutine make_node()
        ! type(json_value), pointer :: p, inp, defs

        ! call xframesJson%create_object(p, '')
        ! call xframesJson%add(p, 'id', 0)

        ! call xframesJson%serialize(p, nodeJson)

        ! call xframesJson%destroy(p)
        
        ! call setElement("{'id': 0}")
        ! call setElement('{"id":1,"type":"unformatted-text","text":"Hello, world"}')
    ! end subroutine make_node
! end module xframes

program main
    use c_interface
    use iso_c_binding
    use json_module
    use font_definitions
    use imgui_theme
    ! use xframes
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    type(c_funptr) :: onInitPtr, onTextChangedPtr, onComboChangedPtr
    type(c_funptr) :: onNumericValueChangedPtr, onBooleanValueChangedPtr, onMultipleNumericValuesChangedPtr, onClickPtr

    type(json_core) :: json
    type(json_value), pointer :: p, inp, defs

    type(ImGuiTheme) :: theme

    character(len=40,kind=c_char), allocatable, target :: assetsBasePath
    type(c_ptr), allocatable :: assetsBasePath_ptr

    integer :: i

    type(FontDef), dimension(:), allocatable :: fontDefs

    character(len=:,kind=c_char), allocatable, target :: fontDefsJson
    character(len=:,kind=c_char), allocatable, target :: themeJson

    type(c_ptr), allocatable :: fontDefsJson_ptr
    type(c_ptr), allocatable :: themeJson_ptr

    allocate(fontDefs(8))

    allocate(assetsBasePath)
    assetsBasePath = "./assets"//C_NULL_CHAR
    assetsBasePath_ptr = c_loc(assetsBasePath)
    
    fontDefs(1) = FontDef("roboto-regular", 16)
    fontDefs(2) = FontDef("roboto-regular", 18)
    fontDefs(3) = FontDef("roboto-regular", 20)
    fontDefs(4) = FontDef("roboto-regular", 24)
    fontDefs(5) = FontDef("roboto-regular", 28)
    fontDefs(6) = FontDef("roboto-regular", 32)
    fontDefs(7) = FontDef("roboto-regular", 36)
    fontDefs(8) = FontDef("roboto-regular", 48)

    ! Both compact_reals and real_format parameters are crucial as otherwise `1.0` becomes `0.1E+1` when serialized
    call json%initialize(compact_reals=.true.,real_format='*')

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

    call init_theme(theme)
    call serialize_theme_to_json(theme, themeJson)

    onInitPtr = c_funloc(myInit)
    onTextChangedPtr = c_funloc(myTextChanged)
    onComboChangedPtr = c_funloc(myComboChanged)
    onNumericValueChangedPtr = c_funloc(myNumericValueChanged)
    onBooleanValueChangedPtr = c_funloc(myBooleanValueChanged)
    onMultipleNumericValuesChangedPtr = c_funloc(myMultipleNumericValuesChanged)
    onClickPtr = c_funloc(myClick)

    fontDefsJson = fontDefsJson // C_NULL_CHAR
    themeJson = themeJson // C_NULL_CHAR

    fontDefsJson_ptr = c_loc(fontDefsJson)
    themeJson_ptr = c_loc(themeJson)

    call init(assetsBasePath_ptr, fontDefsJson_ptr, themeJson_ptr, onInitPtr, onTextChangedPtr, onComboChangedPtr, onNumericValueChangedPtr, onBooleanValueChangedPtr, onMultipleNumericValuesChangedPtr, onClickPtr)

    call json%destroy(p)
    if (json%failed()) stop 1

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

        print *, "Text changed callback. Index:", index
    end subroutine myTextChanged

    subroutine myComboChanged(index, value) bind(C)
        use iso_c_binding, only: c_int
        integer(c_int), value :: index
        integer(c_int), value :: value

        print *, "Combo changed callback. Index:", index, "Value:", value
    end subroutine myComboChanged

    subroutine myNumericValueChanged(index, value) bind(C)
        use iso_c_binding, only: c_int, c_float
        integer(c_int), value :: index
        real(c_float), value :: value

        print *, "Numeric value changed callback. Index:", index, "Value:", value
    end subroutine myNumericValueChanged

    subroutine myBooleanValueChanged(index, value) bind(C)
        use iso_c_binding, only: c_int, c_bool
        integer(c_int), value :: index
        logical(c_bool), value :: value

        print *, "Boolean value changed callback. Index:", index, "Value:", value
    end subroutine myBooleanValueChanged

    subroutine myMultipleNumericValuesChanged(index, values, numValues) bind(C)
        use iso_c_binding, only: c_int, c_float
        integer(c_int), value :: index
        real(c_float), dimension(*), intent(in) :: values
        integer(c_int), value :: numValues

        print *, "Multiple numeric values changed callback. Index:", index, "Num values:", numValues
    end subroutine myMultipleNumericValuesChanged

    subroutine myClick(index) bind(C)
        use iso_c_binding, only: c_int
        integer(c_int), value :: index

        print *, "Click callback. Index:", index
    end subroutine myClick

end program main

