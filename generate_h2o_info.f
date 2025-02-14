program generate_h2o_info
       use hdf5
       IMPLICITE*8(A-H,O-X,Z),logical*1(y)
       character(len=100) :: file_in ='H2O.H5', file_out ='H2O.GenInfo'
       integer :: file_id, dataset_id
       integer :: i, num_atoms, num_molecular_orbitals
       real(8) :: nuclear_repulsion_enery
       real(8), allocatable :: coords(:,:)
       character(len=5), allocatable :: atom_labels(:)
       integer, allocatable :: isym(:)

       ! Ouverture du fichier HDF5 :

       call h5open_f(file_in, H5F_ACC_RDONLY, file_id)

       ! Lecture du nombre d'atoms :
       call h5dopen_f(file_id, 'chemin/num_atoms', dataset_id)
       call h5dread_f(dataset_id, H5T_NATIVE_INTEGER, num_atoms)
       call h5dclose_f(dataset_id)

       ! Lecture des labels des atomes :
       allocate(atom_labels(num_atoms))
       call h5dopen_f(file_id, 'chemin/labels', dataset_id)
       call h5dread_f(dataset_id, H5T_NATIVE_CHARACTER, atom_labels)
       call h5dclose_f(dataset_id)

       ! Lecture des coordonnées des atomes :
       allocate(coords(3, num_atoms))
       call h5dopen_f(file_id, 'chemin/coordinates', dataset_id)
       call h5dread_f(dataset_id, H5T_NATIVE_REAL, coords)
       call h5dclose_f(dataset_id)

       ! Lecture de l'énergie de répulsion nucléaire :
       call h5dopen_f(file_id, 'chemin/nuclear_repulsion_energy',dataset_id)
       call h5dread_f(dataset_id, H5T_NATIVE_REAL, nuclear_repulsion_energy)
       call h5dclose_f(dataset_id)


       ! Lecture des orbitales moléculaires :
       call h5dopen_f(file_id, 'chemin/num_orbitals', dataset_id)
       call h5dread_f(dataset_id, H5T_NATIVE_REAL, num_molecular_ortibals)
       call h5dclose_f(dataset_id)

       ! Fermeture du fichier HDF5 :
       call h5dclose_f(file_id)

       ! Génération du fichier H20.GenInfo :
       open(unit=10, file=file_out, status'new')

      
