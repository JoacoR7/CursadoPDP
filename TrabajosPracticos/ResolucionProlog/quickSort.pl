concatenar([], L, L).
concatenar([H | R], L2, [H | L3]):-
    concatenar(R, L2, L3).

quickSort([X], [X]).
quickSort([], []).
quickSort([H1 | L1], L2) :- 
    separarListas(H1, L1, R1, R2),
    quickSort(R1, ListaMenor),
    quickSort(R2, ListaMayor),
    concatenar(ListaMenor, [H1 | ListaMayor], L2),!.

separarListas(_, [], [], []).
separarListas(Pivot, [H | T], [H | ListaMenor], ListaMayor):-  
    H =< Pivot,
    separarListas(Pivot, T, ListaMenor, ListaMayor).
separarListas(Pivot, [H | T], ListaMenor, [H | ListaMayor]):- 
    H > Pivot,
    separarListas(Pivot, T, ListaMenor, ListaMayor).

