module String_Process
   use Environment
   use String_IO

   implicit none
   
contains
   ! Добавляем новую процедуру Sort_Lines
   subroutine Sort_Lines(head)
      type(StringNode), pointer :: head
      type(StringNode), pointer :: sorted

      ! Используем рекурсивную процедуру для сортировки строк
      call Sort_Lines_By_Length(head, sorted)

      ! Обновляем параметр head для передачи отсортированных строк в программу
      head => sorted
   end subroutine Sort_Lines

   ! Процедура сортировки строк по убыванию длины с использованием хвостовой рекурсии.
   recursive subroutine Sort_Lines_By_Length(head, sorted)
    type(StringNode), pointer :: head
    type(StringNode), pointer :: sorted
    type(StringNode), pointer :: current
    type(StringNode), pointer :: nextNode
    type(StringNode), pointer :: temp

    ! Если список пуст или состоит из одного элемента, сортировка не требуется.
    if (.not. associated(head) .or. .not. associated(head%Next)) then
        sorted => head
        return
    end if

    current => head
    sorted => null()

    ! Основной цикл сортировки вставками.
    do while (associated(current))
        nextNode => current%Next

        ! Вставка элемента в отсортированную часть списка.
        if (.not. associated(sorted) .or. len_trim(current%String) >= len_trim(sorted%String)) then
            current%Next => sorted
            sorted => current
        else
            temp => sorted
            do while (associated(temp%Next) .and. len_trim(temp%Next%String) > len_trim(current%String))
                temp => temp%Next
            end do
            current%Next => temp%Next
            temp%Next => current
        end if

        current => nextNode
    end do
end subroutine Sort_Lines_By_Length
end module String_Process
