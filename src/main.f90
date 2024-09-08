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
   type(StringNode), allocatable :: StrNode
   
   input_file  = "../data/input.txt"
   output_file = "output.txt"

   StrNode = Read_Lines(input_file)

   call Output_Lines(output_file, StrNode, "Исходный файл:", "rewind")

   call Sort_Lines(StrNode) 
   
   call Output_Lines(output_file, StrNode, "Отсортированный файл:", "append")
end program lab_2
