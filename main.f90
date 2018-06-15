PROGRAM TestingSDDB
    USE io

    IMPLICIT NONE

    REAL (KIND=8), DIMENSION(100) :: x, y
    REAL, PARAMETER :: pi = 3.1415927
    REAL (KIND=8) :: temp
    LOGICAL :: EES

    INTEGER :: i



    temp = 77
    EES = .TRUE.

    x = (/ (REAL(i)*pi/50, i=0,99) /)
    y = sin(x)*sin(x)*x*x

    CALL init('data.h5')

    CALL new_simulation()
    CALL attach("temperature", temp)
    CALL attach("EES", EES)
    CALL attach('iterations', 2000)
    CALL add_data(x, 'voltage', units="mV", fullname="Applied Voltage")
    CALL add_data(y, 'current', units='nA', fullname="Measured Current")

    CALL new_simulation()
    CALL attach("temperature", 200.0d0)
    CALL attach("EES", .FALSE.)
    CALL attach('iterations', 3000)
    CALL add_data(x, 'voltage', units="mV", fullname="New Applied Voltage")
    CALL add_data(y, 'current', units='nA', fullname="New Measured Current")

    CALL end()
CONTAINS

END PROGRAM