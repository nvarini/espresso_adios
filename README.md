# espresso_adios
Preliminary version of QE with adios.
In order to be executed with adios you need to specify -D__ADIOS in the make.sys
For the time being only update_pot.f90 has been ported. 
Adios can be executed in 2 ways, by specifying the parameter adios_mode.
adios_mode=0 generates 1 file for each k-point while adios_mode=1 generates the minimum number
of files.

