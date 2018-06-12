MODULE io
    USE HDF5 ! This module contains all necessary modules
    USE system

    IMPLICIT NONE

    INTEGER(HID_T) :: fileid
    INTEGER(HID_T) :: activeid

CONTAINS

SUBROUTINE init(filename)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER :: error

    CALL h5open_f(error)
    CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, fileid, error)
    activeid = fileid
END SUBROUTINE

SUBROUTINE end()
    INTEGER :: error
    CALL h5fclose_f(fileid, error)
    CALL h5close_f(error)
END SUBROUTINE

SUBROUTINE attach_attribute(att)
    TYPE(attributes), INTENT(IN) :: att

    INTEGER(HID_T) :: attrid, aspace
    INTEGER :: error
    INTEGER(HSIZE_T), DIMENSION(1) :: data_dims
    INTEGER :: bool

    data_dims(1) = 1

    CALL h5screate_f(H5S_SCALAR_F, aspace, error)
    CALL h5acreate_f(activeid, "temperature", H5T_IEEE_F32BE,aspace,attrid,error)
    CALL h5awrite_f(attrid, H5T_NATIVE_DOUBLE, att%temp, data_dims, error)
    CALL h5aclose_f(attrid, error)

    CALL h5screate_f(H5S_SCALAR_F, aspace, error)
    CALL h5acreate_f(activeid, "EES", H5T_STD_I32LE,aspace,attrid,error)
    IF (att%EES) THEN
        bool = 1
    ELSE 
        bool = 0
    END IF

    CALL h5awrite_f(attrid, H5T_NATIVE_INTEGER, bool, data_dims, error)
    CALL h5aclose_f(attrid, error)
END SUBROUTINE

END MODULE