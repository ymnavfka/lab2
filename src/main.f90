! Морозов Н.Д., группа 30022.
! Лабораторная работа №2, вариант 14.

! Задача: Разработать чистую процедуру сортировки строк заданного текста
!         по убыванию длины строки. Использовать сортировку вставками.

! Указание: Элементом списка явлется строка. Применять функцию Len_trim

program lab_2
   use Environment
   use String

   implicit none
   character(:), allocatable :: input_file, output_file
   type(StringList) :: StrList
   
   input_file  = "../data/input.txt"
   output_file = "output.txt"

   call StrList%Read_Lines(input_file)

   call StrList%Output_Lines(output_file, "Исходный файл:", "rewind")

   call StrList%Sort_Lines() 
   
   call StrList%Output_Lines(output_file, "Отсортированный файл:", "append")
end program lab_2
