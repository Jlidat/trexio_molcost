program trexioMain 
      use trexio
!   implicit none
!  character (len=100) :: input_filename
!  character(len=105) :: output_filename  

        call trexio2info
        call trexio2inporb
        call trexio2mono
!        call getarg(1, input_filename)
!        call generate_output_filename(input_filename, output_filename)
!        write(*,*) output_filename

end




