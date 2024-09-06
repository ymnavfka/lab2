module String
   use Environment

   implicit none
   
   ! Структура данных для хранения строки.
   type :: StringNode
      character(:, CH_), allocatable :: String
      type(StringNode), allocatable  :: Next
   end type StringNode

   public :: Read_Lines, Output_Lines, Sort_Lines

contains
   ! Чтение исходного кода. 
   function Read_Lines(InputFile) result (StrNode)
      type(StringNode), allocatable :: StrNode
      character(*), intent(in)      :: InputFile
      integer                       :: In
     
      open (file=InputFile, newunit=In)
         call Read_One_Line(in, StrNode)
      close (In)
   end function Read_Lines

   ! Чтение строки исходного кода.
   recursive subroutine Read_One_Line(in, StrNode)
      type(StringNode), allocatable :: StrNode
      integer, intent(in)           :: In
      integer, parameter            :: max_len = 1024
      character(max_len, CH_)       :: string
      integer                       :: IO

      allocate(StrNode)
      ! Чтение строки во временную строку бОльшей длины.
      read (In, "(a)", iostat=IO) string
      call Handle_IO_Status(IO, "reading line from file")
      if (IO == 0) then
         ! Хранение в размещаемом поле символов без завершающих пробелов.
         StrNode%String = trim(string)
         call Read_One_Line(In, StrNode%Next)
      else
         deallocate(StrNode)
      end if
   end subroutine Read_One_Line


   ! Вывод исходного кода.
   subroutine Output_Lines(OutputFile, StrNode, List_name, Position)
      character(*), intent(in)      :: OutputFile, List_name, Position
      type(StringNode), allocatable :: StrNode 
      integer                       :: Out
      
      open (file=OutputFile, position=Position, newunit=Out)
         write(Out, '(/a)') List_name
         call Output_One_Line(Out, StrNode)
      close (Out)
   end subroutine Output_Lines

   ! Вывод строки исходного кода.
   recursive subroutine Output_One_Line(Out, StrNode)
      integer, intent(in)           :: Out
      type(StringNode), allocatable, intent(in)  :: StrNode
      integer :: IO

      if (allocated(StrNode)) then
         write (Out, "(a)", iostat=IO) StrNode%String
         call Handle_IO_Status(IO, "writing line")
         call Output_One_Line(Out, StrNode%next)
      end if
   end subroutine Output_One_Line

   subroutine Sort_Lines(head)
      type(StringNode), allocatable :: head, sorted, temp

      if (allocated(head%Next)) then
         allocate(sorted)
         sorted%String = head%String
         temp = head%Next
         head%String = temp%String
         head%Next = temp%Next
      else
         return
      end if

      call Sort_Lines_By_Length(head, sorted)

      head = sorted
   end subroutine Sort_Lines

   ! Процедура сортировки списка вставками
   recursive subroutine Sort_Lines_By_Length(current, sorted)
      type(StringNode), allocatable :: current, sorted, next

      if (allocated(current)) then
         next = current%Next
         call InsertNodeSorted(current, sorted)
         call Sort_Lines_By_Length(next, sorted)  ! Хвостовая рекурсия
      end if
   end subroutine Sort_Lines_By_Length

   ! Процедура вставки элемента в отсортированный список
   recursive subroutine InsertNodeSorted(notSorted, sorted)
      type(StringNode), allocatable :: notSorted, sorted, temp

      ! Найдем место для вставки и вставим
      if (len_trim(sorted%String) < len_trim(notSorted%String)) then
         temp = sorted
         sorted%String = notSorted%String
         allocate(sorted%Next)
         sorted%Next = temp
      else if (.not. allocated(sorted%Next)) then
         allocate(sorted%Next)
         sorted%Next%String = notSorted%String
      else
         call InsertNodeSorted(notSorted, sorted%Next)
      end if
   end subroutine InsertNodeSorted

end module String
