PROGRAM TestingSDDB
    USE system
    USE io

    IMPLICIT NONE

    TYPE(attributes) :: att
    REAL(KIND=8), DIMENSION(4) :: x

    PRINT *, 'Testing SDDB'

    att%temp = 300
    att%EES = .FALSE.

    x = (/ 1, 2, 3, 4 /)

    CALL init('data.h5')

    CALL attach_attributes(att)
    CALL end()
CONTAINS

END PROGRAM