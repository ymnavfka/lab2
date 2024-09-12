module String
   use Environment

   implicit none
   
   ! Структура данных для хранения строки.
   type :: StringNode
      character(:, CH_), allocatable :: String
      type(StringNode), allocatable  :: Next
   end type StringNode

   type StringList
      type(StringNode), allocatable  :: Head
   contains
      procedure :: Read_Lines
      procedure, nopass, private :: Read_One_Line
      procedure :: Output_Lines
      procedure, nopass, private :: Output_One_Line
      procedure :: Sort_Lines
      procedure, nopass, private :: Sort_Lines_By_Length, InsertNodeSorted
   end type StringList

contains
   ! Чтение исходного кода. 
   subroutine Read_Lines(StrList, InputFile)
      class(StringList) :: StrList
      character(*), intent(in) :: InputFile
      integer                  :: In
     
      open (file=InputFile, newunit=In)
         call Read_One_Line(in, StrList%Head)
      close (In)
   end subroutine Read_Lines

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
   subroutine Output_Lines(StrList, OutputFile, List_name, Position)
      character(*), intent(in)      :: OutputFile, List_name, Position
      class(StringList) :: StrList 
      integer                       :: Out
      
      open (file=OutputFile, position=Position, newunit=Out)
         write(Out, '(/a)') List_name
         call Output_One_Line(Out, StrList%Head)
      close (Out)
   end subroutine Output_Lines

   ! Вывод строки исходного кода.
   recursive subroutine Output_One_Line(Out, StrNode)
      integer, intent(in)           :: Out
      type(StringNode), allocatable :: StrNode
      integer :: IO

      if (allocated(StrNode)) then
         write (Out, "(a)", iostat=IO) StrNode%String
         call Handle_IO_Status(IO, "writing line")
         call Output_One_Line(Out, StrNode%next)
      end if
   end subroutine Output_One_Line

   subroutine Sort_Lines(StrList)
      class(StringList) :: StrList
      type(StringNode), allocatable :: sorted, temp

      if (allocated(StrList%Head%Next)) then
         ! Помещаем в sorted первый элемент изначального списка
         allocate(sorted)
         sorted%String = StrList%Head%String

         ! Сдвигаем изначальный список на 1 элемент 
         call move_alloc(StrList%Head%Next, temp)
         call move_alloc(temp, StrList%Head)
      else
         return
      end if

      call Sort_Lines_By_Length(StrList%Head, sorted)

      call move_alloc(sorted, StrList%Head)
   end subroutine Sort_Lines

   ! Процедура сортировки списка вставками
   recursive subroutine Sort_Lines_By_Length(current, sorted)
      type(StringNode), allocatable :: current, sorted

      if (allocated(current)) then
         call InsertNodeSorted(current, sorted)
         call Sort_Lines_By_Length(current%Next, sorted)  ! Хвостовая рекурсия
      end if
   end subroutine Sort_Lines_By_Length

   ! Процедура вставки элемента в отсортированный список
   recursive subroutine InsertNodeSorted(notSorted, sorted)
      type(StringNode), allocatable, intent(inout) :: notSorted, sorted
      type(StringNode), allocatable :: temp

      ! Найдем место для вставки и вставим
      if (len_trim(sorted%String) < len_trim(notSorted%String)) then
         ! Вставляем перед текущим элементом
         call move_alloc(sorted, temp)
         allocate(sorted)
         sorted%String = notSorted%String
         call move_alloc(temp, sorted%Next)
      else if (.not. allocated(sorted%Next)) then
         ! Вставляем в конец списка
         allocate(sorted%Next)
         sorted%Next%String = notSorted%String
      else
         ! Переходим к следующему элементу
         call InsertNodeSorted(notSorted, sorted%Next)
      end if
   end subroutine InsertNodeSorted

end module String
