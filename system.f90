MODULE system
    USE io
    
    TYPE attributes
        REAL(kind=8) :: temp
        LOGICAL :: EES
    END TYPE attributes

CONTAINS

    SUBROUTINE show_attributes(att)
        TYPE(attributes), INTENT(IN) :: att
        WRITE (*, '(A20, 1X, F6.0)') "Temperature", att%temp
        WRITE (*, '(A20, 1X, L1)') "EES activated?", att%EES
    END SUBROUTINE

    SUBROUTINE attach_attributes(att)
        TYPE(attributes), INTENT(IN) :: att
        CALL attach("temperature", att%temp)
        CALL attach("EES", att%EES)
    END SUBROUTINE

END MODULE