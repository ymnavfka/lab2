! Морозов Н.Д., группа 30022.
! Лабораторная работа №2, вариант 14.

! Задача: Разработать чистую процедуру сортировки строк заданного текста
!         по убыванию длины строки. Использовать сортировку вставками.

! Указание: Элементом списка явлется строка. Применять функцию Len_trim

program lab_2
   use environment
   use string

   implicit none
   character(:), allocatable :: input_file, output_file
   type(string_list) :: str_list
   
   input_file  = "../data/input.txt"
   output_file = "output.txt"

   call str_list%read_lines(input_file)

   call str_list%output_lines(output_file, "исходный файл:", "rewind")

   call str_list%sort_lines() 
   
   call str_list%output_lines(output_file, "отсортированный файл:", "append")
end program lab_2
