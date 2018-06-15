MODULE io
    USE HDF5

    IMPLICIT NONE

    INTEGER(HID_T) :: fileid
    INTEGER(HID_T) :: activeid

    INTERFACE attach
        MODULE PROCEDURE attach_double
        MODULE PROCEDURE attach_logical
        MODULE PROCEDURE attach_string
    END INTERFACE
CONTAINS

SUBROUTINE init(filename)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER :: error

    CALL h5open_f(error)
    !CALL h5eset_auto_f(0, error)

    CALL h5fopen_f(filename, H5F_ACC_RDWR_F, fileid, error)
    IF (error /= 0) THEN
        CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, fileid, error)
    END IF

    activeid = fileid
END SUBROUTINE

SUBROUTINE end()
    INTEGER :: error
    CALL h5gclose_f(activeid, error)

    CALL h5fclose_f(fileid, error)
    CALL h5close_f(error)
END SUBROUTINE

SUBROUTINE add_data(data, name, units, fullname)
    REAL(KIND=8), INTENT(IN), DIMENSION(:) :: data
    CHARACTER (*), INTENT(IN) :: name
    CHARACTER (*), INTENT(IN), OPTIONAL :: units, fullname

    INTEGER :: dataspace_id, dataset_id, error
    INTEGER(HSIZE_T), DIMENSION(1) :: dims1
    dims1(1) = SIZE(data)

    CALL h5screate_simple_f(1, dims1, dataspace_id, error)
    CALL h5dcreate_f(activeid, name, H5T_IEEE_F32BE, dataspace_id, &
    dataset_id, error)
    CALL h5dwrite_f(dataset_id, H5T_NATIVE_DOUBLE, data, dims1, error)

    IF (PRESENT(units)) THEN
        CALL attach_string("units", units, object=dataset_id)
    END IF
    IF (PRESENT(fullname)) THEN
        CALL attach_string("fullname", fullname, object=dataset_id)
    END IF
END SUBROUTINE

SUBROUTINE new_simulation()
    INTEGER :: nmembers
    INTEGER :: hdferr       
    CHARACTER(LEN=8) :: gname

    CALL h5gn_members_f(fileid, "/", nmembers, hdferr)           

    WRITE (gname, '(I4.4)') nmembers + 1

    CALL h5gcreate_f(fileid, gname, activeid, hdferr)

    CALL h5ldelete_f(fileid, "last", hdferr)
    CALL h5lcreate_soft_f(gname, fileid, "last", hdferr)

    CALL stamp_time()
END SUBROUTINE

SUBROUTINE stamp_time()
    INTEGER, DIMENSION(8) :: values
    CHARACTER (LEN=5) :: zone
    CHARACTER (LEN=26) :: timestamp

    CALL date_and_time(VALUES=values, ZONE=zone)

    WRITE (timestamp, '(I4.4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2," ",A)') values(1:3), values(5:7), zone

    CALL attach_string('creation_time', timestamp)
END SUBROUTINE

SUBROUTINE attach_string(name, value, object)
    CHARACTER(*), INTENT(IN) :: name, value
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: object
    INTEGER :: error
    INTEGER(HSIZE_T), DIMENSION(1) :: dims1
    INTEGER(HID_T) :: atype_id, attr_id, aspace_id, targetid
    INTEGER(HSIZE_T) :: l
    
    dims1(1) = 1
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
    CALL h5screate_f(H5S_SCALAR_F, aspace_id, error)

    l = LEN(value)

    CALL h5tset_size_f(atype_id, l, error)

    IF (PRESENT(object)) THEN
        targetid = object
    ELSE
        targetid = activeid
    END IF

    CALL h5acreate_f(targetid, name, atype_id, aspace_id, attr_id, error)
    CALL h5awrite_f(attr_id, atype_id, value, dims1, error)
    CALL h5aclose_f(attr_id, error)
END SUBROUTINE

SUBROUTINE attach_double(name, value, object)
    REAL(KIND=8), INTENT(IN) :: value
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: object

    INTEGER(HID_T) :: attrid, aspace, targetid
    INTEGER :: error
    INTEGER(HSIZE_T), DIMENSION(1) :: data_dims

    data_dims(1) = 1

    IF (PRESENT(object)) THEN
        targetid = object
    ELSE
        targetid = activeid
    END IF

    CALL h5screate_f(H5S_SCALAR_F, aspace, error)
    CALL h5acreate_f(targetid, name, H5T_IEEE_F32BE,aspace,attrid,error)
    CALL h5awrite_f(attrid, H5T_NATIVE_DOUBLE, value, data_dims, error)
    CALL h5aclose_f(attrid, error)
END SUBROUTINE

SUBROUTINE attach_logical(name, value, object)
    LOGICAL, INTENT(IN) :: value
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(HID_T), INTENT(IN), OPTIONAL :: object

    INTEGER(HID_T) :: attrid, aspace, targetid
    INTEGER :: error
    INTEGER(HSIZE_T), DIMENSION(1) :: data_dims
    INTEGER :: bool

    data_dims(1) = 1

    CALL h5screate_f(H5S_SCALAR_F, aspace, error)
    IF (PRESENT(object)) THEN
        targetid = object
    ELSE
        targetid = activeid
    END IF

    CALL h5acreate_f(targetid, name, H5T_STD_I32LE,aspace,attrid,error)
    IF (value) THEN
        bool = 1
    ELSE 
        bool = 0
    END IF  
    CALL h5awrite_f(attrid, H5T_NATIVE_INTEGER, bool, data_dims, error)
    CALL h5aclose_f(attrid, error)
END SUBROUTINE

END MODULE