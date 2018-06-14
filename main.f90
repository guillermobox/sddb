PROGRAM TestingSDDB
    USE system
    USE io

    IMPLICIT NONE

    TYPE(attributes) :: att
    REAL(KIND=8), DIMENSION(100) :: x, y
    REAL, PARAMETER :: pi = 3.1415927

    INTEGER :: i

    PRINT *, 'Testing SDDB'

    att%temp = 77
    att%EES = .TRUE.

    x = (/ (REAL(i)*pi/50, i=0,99) /)
    y = sin(x)*sin(x)

    CALL init('data.h5')
    CALL new_simulation()
    CALL attach_attributes(att)
    CALL add_data(x, 'voltage', units="mV", fullname="Applied Voltage")
    CALL add_data(y, 'current', units='nA', fullname="Measured Current")
    CALL end()
CONTAINS

END PROGRAM