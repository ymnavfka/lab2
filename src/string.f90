module string
   use environment

   implicit none
   
   ! Структура данных для хранения строки.
   type :: string_node
      character(:, CH_), allocatable :: string
      type(string_node), allocatable  :: next
   end type string_node

   type string_list
      type(string_node), allocatable  :: head
   contains
      procedure :: read_lines
      procedure, nopass, private :: read_one_line
      procedure :: output_lines
      procedure, nopass, private :: output_one_line
      procedure :: sort_lines
      procedure, nopass, private :: sort_lines_by_length, insert_node_sorted
   end type string_list

contains
   ! Чтение исходного кода. 
   subroutine read_lines(str_list, input_file)
      class(string_list) :: str_list
      character(*), intent(in) :: input_file
      integer                  :: in
     
      open (file=input_file, newunit=in)
         call read_one_line(in, str_list%head)
      close (in)
   end subroutine read_lines

   ! Чтение строки исходного кода.
   recursive subroutine read_one_line(in, str_node)
      type(string_node), allocatable :: str_node
      integer, intent(in)           :: in
      integer, parameter            :: max_len = 1024
      character(max_len, CH_)       :: string
      integer                       :: IO

      allocate(str_node)
      ! Чтение строки во временную строку бОльшей длины.
      read (in, "(a)", iostat=IO) string
      call handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
         ! Хранение в размещаемом поле символов без завершающих пробелов.
         str_node%string = trim(string)
         call read_one_line(in, str_node%next)
      else
         deallocate(str_node)
      end if
   end subroutine read_one_line


   ! Вывод исходного кода.
   subroutine output_lines(str_list, output_file, list_name, position)
      character(*), intent(in)      :: output_file, list_name, position
      class(string_list) :: str_list 
      integer                       :: out
      
      open (file=output_file, position=position, newunit=out)
         write(out, '(/a)') list_name
         call output_one_line(out, str_list%head)
      close (out)
   end subroutine output_lines

   ! Вывод строки исходного кода.
   recursive subroutine output_one_line(out, str_node)
      integer, intent(in)           :: out
      type(string_node), allocatable :: str_node
      integer :: IO

      if (allocated(str_node)) then
         write (out, "(a)", iostat=IO) str_node%string
         call handle_IO_status(IO, "writing line")
         call output_one_line(out, str_node%next)
      end if
   end subroutine output_one_line

   subroutine sort_lines(str_list)
      class(string_list) :: str_list
      type(string_node), allocatable :: sorted, temp

      if (allocated(str_list%head%next)) then
         ! Помещаем в sorted первый элемент изначального списка
         allocate(sorted)
         sorted%string = str_list%head%string

         ! Сдвигаем изначальный список на 1 элемент 
         call move_alloc(str_list%head%next, temp)
         call move_alloc(temp, str_list%head)
      else
         return
      end if

      call sort_lines_by_length(str_list%head, sorted)

      call move_alloc(sorted, str_list%head)
   end subroutine sort_lines

   ! Процедура сортировки списка вставками
   recursive subroutine sort_lines_by_length(current, sorted)
      type(string_node), allocatable :: current, sorted

      if (allocated(current)) then
         call insert_node_sorted(current, sorted)
         call sort_lines_by_length(current%next, sorted)  ! Хвостовая рекурсия
      end if
   end subroutine sort_lines_by_length

   ! Процедура вставки элемента в отсортированный список
   recursive subroutine insert_node_sorted(not_sorted, sorted)
      type(string_node), allocatable, intent(inout) :: not_sorted, sorted
      type(string_node), allocatable :: temp

      ! Найдем место для вставки и вставим
      if (len_trim(sorted%string) < len_trim(not_sorted%string)) then
         ! Вставляем перед текущим элементом
         call move_alloc(sorted, temp)
         allocate(sorted)
         sorted%string = not_sorted%string
         call move_alloc(temp, sorted%next)
      else if (.not. allocated(sorted%next)) then
         ! Вставляем в конец списка
         allocate(sorted%next)
         sorted%next%string = not_sorted%string
      else
         ! Переходим к следующему элементу
         call insert_node_sorted(not_sorted, sorted%next)
      end if
   end subroutine insert_node_sorted

end module string
