PROGRAM TestingSDDB
    USE system
    USE io

    IMPLICIT NONE

    TYPE(attributes) :: att
    REAL(KIND=8), DIMENSION(4) :: x

    PRINT *, 'Testing SDDB'

    att%temp = 77.0
    att%EES = .TRUE.

    x = (/ 1, 2, 3, 4 /)

    CALL init('data.h5')

    CALL attach_attribute(att)
    CALL end()
CONTAINS

END PROGRAM