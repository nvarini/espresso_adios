MODULE adios_qe
  USE adios_read_mod
  USE adios_write_mod
  IMPLICIT NONE
  INTEGER :: adios_comm, rank_adios, size_adios
  INTEGER*8, DIMENSION(2) :: offset=0, readsize=1
  INTEGER*8          :: adios_handle, adios_totalsize, adios_groupsize
  integer            :: method, lock
  INTEGER*8          :: f, sel, adios_handle2

  CONTAINS

  SUBROUTINE setup_adios()
    USE parallel_include
    IMPLICIT NONE
    INTEGER :: ierr
    CALL MPI_Comm_dup(MPI_COMM_WORLD, adios_comm, ierr)
    CALL MPI_Comm_rank (adios_comm, rank_adios, ierr)
    CALL MPI_Comm_size (adios_comm, size_adios, ierr)
    method = ADIOS_READ_METHOD_BP
    lock = ADIOS_LOCKMODE_ALL
  END SUBROUTINE setup_adios


 SUBROUTINE prepare_index_adios(sendm,recm,globalm,comm,nproc)

   USE parallel_include
   USE mp, ONLY : mp_sum
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: comm, nproc
   INTEGER, INTENT(INOUT) :: sendm, recm, globalm
   INTEGER :: errore

   call mpi_scan(sendm,recm,1,MPI_INTEGER,MPI_SUM,comm,errore)
   recm=recm-sendm
   globalm=sendm
   call mp_sum(globalm,comm)

 END SUBROUTINE prepare_index_adios

 SUBROUTINE initialize_adios_qe(comm,method)

 IMPLICIT NONE
 INTEGER, INTENT(IN) :: comm, method
 INTEGER :: adios_err 
 
 CALL adios_init("/home/nvarini/Dragoni_IO/espresso/PW/src/pwdesc.xml",comm,adios_err)
 CALL adios_read_init_method (method, comm, "verbose=3", adios_err);
 
 END SUBROUTINE initialize_adios_qe

 SUBROUTINE prepare_selection_adios(read1,read2,off1,off2)
 
 IMPLICIT NONE

 INTEGER, INTENT(IN) :: read1,read2,off1,off2
 readsize(1)=read1 
 readsize(2)=read2 
 offset(1)=off1
 offset(2)=off2
 CALL adios_selection_boundingbox (sel, 2, offset, readsize)
  
 END SUBROUTINE prepare_selection_adios


 SUBROUTINE read_adios_k
 END SUBROUTINE read_adios_k 


END MODULE adios_qe
