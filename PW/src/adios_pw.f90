MODULE adios_pw

 USE adios_qe
 USE kinds,  ONLY : DP
 USE input_parameters, ONLY : adios_mode

 IMPLICIT NONE
 INTEGER            :: npw_g, off_npw, ierr, adios_err
 character(len=25)  :: filename_old  = "oldwfc1"
 character(len=25)  :: filename_old2 = "oldwfc2"
 character(len=25)  :: filename_old3 = "oldwfc3"
 character(len=25)  :: filename_old4 = "oldwfc4"



 !INTERFACE write_adios
 !  MODULE PROCEDURE write_adios_k_evc, write_adios_k_evcold
 !END INTERFACE



 CONTAINS

 SUBROUTINE write_adios_evc(ik,handle,group_name,filename,comm,mode)
 USE wvfct, ONLY : npwx, nbnd
 USE noncollin_module, ONLY : npol 
 USE mp_world, ONLY : mpime
 USE wavefunctions_module, ONLY : evc
 

 IMPLICIT NONE
 INTEGER, INTENT(IN) :: ik, comm, mode
 INTEGER*8, INTENT(INOUT) :: handle
 CHARACTER(len=*), INTENT(IN) :: group_name, filename
 CHARACTER(len=256) :: filename_ik
 INTEGER :: adios_err


 IF(ik.eq.1 .and. mode.eq.1)THEN
   write(filename_ik,'(a7,a3)') filename,'.bp'
   call adios_open (handle, group_name, filename_ik, "w", comm, adios_err)
 ELSE IF (mode.eq.1) THEN
   write(filename_ik,'(a7,a3)') filename,'.bp'
   call adios_open (handle, group_name, filename_ik, "a", comm, adios_err)
 ELSE 
   write(filename_ik,'(a7,a1,i2.2,a3)') filename,'-',ik,'.bp'
  call adios_open (handle, group_name, trim(filename_ik), "w", comm, adios_err)

 ENDIF
 include "gwrite_wfc.fh"
 call adios_close (adios_handle, adios_err)

 END SUBROUTINE write_adios_evc

 SUBROUTINE write_adios_evcold(ik,handle,group_name,filename,comm,evcold,mode)
 USE wvfct, ONLY : npwx, nbnd
 USE noncollin_module, ONLY : npol 
 USE mp_world, ONLY : mpime
 

 IMPLICIT NONE
 INTEGER, INTENT(IN) :: ik, comm, mode
 INTEGER*8, INTENT(INOUT) :: handle
 
 CHARACTER(len=*), INTENT(IN) :: group_name, filename
 INTEGER :: adios_err
 COMPLEX(DP), INTENT(IN) :: evcold(npol*npwx,nbnd)
 CHARACTER(len=256)      :: filename_ik       


 IF(ik.eq.1 .and. mode.eq.1)THEN
   write(filename_ik,'(a7,a3)') filename,'.bp'
   call adios_open (handle, group_name, filename_ik, "w", comm, adios_err)
 ELSE IF(mode.eq.1)THEN
   write(filename_ik,'(a7,a3)') filename,'.bp'
   call adios_open (handle, group_name, filename_ik, "a", comm, adios_err)
 ELSE 
   write(filename_ik,'(a7,a1,i2.2,a3)') filename,'-',ik,'.bp'
   call adios_open (handle, group_name, filename_ik, "w", comm, adios_err)
 ENDIF
 include "gwrite_oldwfc.fh"
 call adios_close (adios_handle, adios_err)

 END SUBROUTINE write_adios_evcold

 SUBROUTINE write_adios_aux(ik,handle,group_name,filename,comm,aux,mode)
 USE wvfct, ONLY : npwx, nbnd
 USE noncollin_module, ONLY : npol 
 USE mp_world, ONLY : mpime
 

 IMPLICIT NONE
 INTEGER, INTENT(IN) :: ik, comm, mode
 INTEGER*8, INTENT(INOUT) :: handle
 CHARACTER(len=*), INTENT(IN) :: group_name, filename
 CHARACTER(len=256) :: filename_ik
 INTEGER :: adios_err
 COMPLEX(DP), INTENT(IN) :: aux(npol*npwx,nbnd)


 IF(ik.eq.1 .and. mode.eq.1)THEN
   write(filename_ik,'(a7,a3)') filename,'.bp'
   call adios_open (handle, group_name, filename_ik, "w", comm, adios_err)
 ELSE IF(mode.eq.1)THEN
   write(filename_ik,'(a7,a3)') filename,'.bp'
   call adios_open (handle, group_name, filename_ik, "a", comm, adios_err)
 ELSE 
   write(filename_ik,'(a7,a1,i2.2,a3)') filename,'-',ik,'.bp'
   call adios_open (handle, group_name, filename_ik, "w", comm, adios_err)
 ENDIF
   include "gwrite_oldwfc2.fh"
 call adios_close (adios_handle, adios_err)

 END SUBROUTINE write_adios_aux


 SUBROUTINE adios_read_array(ik,fh, filename, comm, var,array,mode)

   IMPLICIT NONE
   INTEGER, INTENT(in) ::  comm, ik, mode
   INTEGER*8, INTENT(INOUT) :: fh
   CHARACTER(LEN=*), INTENT(in) :: filename, var
   CHARACTER(LEN=256) :: filename_ik
   COMPLEX(DP), INTENT(INOUT) :: array(:,:)
   INTEGER :: ierr


  IF(ik.eq.1 .and. mode.eq.1)THEN
   write(filename_ik,'(a7,a3)') filename,'.bp'
  ELSE IF(mode.eq.1)THEN
   write(filename_ik,'(a7,a3)') filename,'.bp'
  ELSE IF(mode.eq.0)THEN
   write(filename_ik,'(a7,a1,i2.2,a3)') filename,'-',ik,'.bp'
  ENDIF


   CALL adios_read_open_file(fh, filename_ik, method, comm, ierr);
   IF(mode.eq.1)THEN
    CALL adios_schedule_read(fh, sel, var, ik-1, 1, array, ierr)
   ELSE
    CALL adios_schedule_read(fh, sel, var, 0, 1, array, ierr)
   ENDIF
   CALL adios_perform_reads (fh, ierr)
   CALL adios_read_close (fh, ierr)

 END SUBROUTINE adios_read_array




END MODULE adios_pw
