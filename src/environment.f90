module environment
   use ISO_Fortran_Env

   implicit none
    
   integer, parameter      :: I_ = INT32                             ! Разновидность типа для целочисленных переменных.
   integer, parameter      :: R_ = REAL32                            ! Разновидность типа для вещественных переменных.
   integer, parameter      :: C_ = R_                                ! Разновидность типа для компексных переменных.
   integer, parameter      :: CH_= selected_char_kind("ISO_10646")   ! Разновидность типа для символов.
   character(*), parameter :: E_ = "ASCII"                           ! Кодировка файлов.

   interface operator (//)
      module procedure int_plus_string
      module procedure string_plus_int
   end interface

contains

   pure function int_plus_string(int, str) result(res)
      integer, intent(in)                                         :: int
      character(*), intent(in)                                    :: str
      character(len(str)+max(floor(log10(real(int, I_*2)))+1, 1)) :: res

      write (res, '(i0, a)') int, str
   end function int_plus_string

   pure function string_plus_int(str, int) result(res)
      character(*), intent(in)                                    :: str
      integer, intent(in)                                         :: int
      character(len(str)+max(floor(log10(real(int, I_*2)))+1, 1)) :: res

      write (res, '(a, i0)') str, int
   end function string_plus_int

   ! Обработка статуса ввода/вывода.
   subroutine handle_IO_status(IO, where)
      integer, intent(in)        :: IO
      character(*), intent(in)   :: where

      open (ERROR_UNIT)
      select case(IO)
         case(0, IOSTAT_END, IOSTAT_EOR)
         case(1:)
            write (ERROR_UNIT, '(a, i0)') "Error " // where // ": ", IO
         case default
            write (ERROR_UNIT, '(a, i0)') "Undetermined behaviour has been reached while " // where // ": ", IO
      end select
      ! close (Out) ! Если не OUTPUT_UNIT.
   end subroutine handle_IO_status

end module environment
