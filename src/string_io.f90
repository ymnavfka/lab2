module String_IO
   use Environment

   implicit none
   
   ! Структура данных для хранения строки.
   type :: StringNode
      character(:, CH_), allocatable   :: String
      type(StringNode), pointer        :: Next  => Null()
   end type StringNode

   public :: Read_Lines, Output_Lines

contains
   ! Чтение исходного кода. 
   function Read_Lines(InputFile) result (StrNode)
      type(StringNode), pointer  :: StrNode
      character(*), intent(in)   :: InputFile
      integer  :: In
      
      open (file=InputFile, newunit=In)
         StrNode => Read_One_Line(in)
      close (In)
   end function Read_Lines

   ! Чтение строки исходного кода.
   recursive function Read_One_Line(in) result(StrNode)
      type(StringNode), pointer  :: StrNode
      integer, intent(in)        :: In
      integer, parameter         :: max_len = 1024
      character(max_len, CH_)    :: string
      integer :: IO

      ! Чтение строки во временную строку бОльшей длины.
      read (In, "(a)", iostat=IO) string
      call Handle_IO_Status(IO, "reading line from file")
      if (IO == 0) then
         allocate (StrNode)
         ! Хранение в размещаемом поле символов без завершающих пробелов.
         StrNode%String = Trim(string)
         StrNode%Next => Read_One_Line(In)
      else
         StrNode => Null()
      end if
   end function Read_One_Line


   ! Вывод исходного кода.
   subroutine Output_Lines(OutputFile, StrNode, List_name, Position)
      character(*), intent(in)      :: OutputFile, List_name, Position
      type(StringNode), intent(in)  :: StrNode 
      integer :: Out
      
      open (file=OutputFile, position=Position, newunit=Out)
         write(Out, '(/a)') List_name
         call Output_One_Line(Out, StrNode)
      close (Out)
   end subroutine Output_Lines

   ! Вывод строки исходного кода.
   recursive subroutine Output_One_Line(Out, StrNode)
      integer, intent(in)           :: Out
      type(StringNode), intent(in)  :: StrNode
      integer :: IO

      write (Out, "(a)", iostat=IO) StrNode%String
      call Handle_IO_Status(IO, "writing line")
      if (Associated(StrNode%next)) &
         call Output_One_Line(Out, StrNode%next)
   end subroutine Output_One_Line
end module String_IO 
