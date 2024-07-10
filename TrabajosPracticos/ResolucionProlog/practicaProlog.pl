%select(Elem, List1, List2): Is true when List1, with Elem removed, results in List2

%TP6
%EJERCICIO 3

%1
primero([H|_],H).

%2
resto([_|T],T).

%3
construye(X, L1, [X|L1]).

%4
pertenece(X, [X|_]):-!.
pertenece(X, [_|T]):-pertenece(X, T).

%5
concatena([], L, L).
concatena([H|T1], L, [H|R]):-concatena(T1, L, R).

%6
palindromo([]).
palindromo([_]).
palindromo([X|L]) :- 
    concatena(Resto, [X], L),
    palindromo(Resto).

%7
ultimo(X, [X]).
ultimo(X, [_|T]):-ultimo(X, T).

ultimo_reverse(X, T):- reverse(T, [X|_]).

ultimo_append(X,T):- concatena(_, [X], T).

%8
inserta(X, L1, L2):- select(X , L2 ,L1).

%9
%sublista([] , _).
sublista (L1 , L2):- append(_ , L1 , Resto), append(Resto , _ , L2).

%10
subconjunto([], _).
subconjunto([X|T], L):- pertenece(X, L), subconjunto(T, L).


%11
maximo(X,Y,Z) :- X <= Y, Z is Y.
maximo(X,Y,Z) :- Y <= X, Z is X.

%12
%Version 1
% Caso base: El MCD de un número N consigo mismo es N.
mcd_euclides(X, X, Z) :- Z is X.

% Caso recursivo

mcd_euclides(X,Y,Resultado):-
    
    X > 0,
    Y > 0,
    (X >= Y ->
        Resto is X mod Y,
        (Resto = 0 ->
            Resultado is Y
        ;
            mcd_euclides(Y, Resto, Resultado)
        )
    ;
        mcd_euclides(Y, X, Resultado)
    ).

mcd(X,Y,Z):-
    mcd_euclides(X,Y,Z).

%Version 2
mcd2(X, 0, X).
mcd2(X, Y, Z):- Aux is mod(X, Y), mcd2(Y, Aux, Z).

%13
longitud([],N):- N is 0.
longitud([_|R],N):- longitud(R,N1) , N is N1 + 1.

%14
%Version 1
lista_Acotada_Recursive([] , Longitud_Lista):- Longitud_Lista >= 0.
lista_Acotada_Recursive([H|L] , Longitud_Lista):- Longitud_Lista >= H , lista_Acotada_Recursive(L , Longitud_Lista).

lista_acotada(L):-
    length(L , X1) , X is X1,
    lista_Acotada_Recursive(L, X).

%Version 2
lista_acotada2(L):- length(L, Longitud), forall(member(Elem, L), Elem < Longitud).

%15
%Version 1
maximo(X,Y,Z):- ( X >=Y  ->  Z=X ; Z=Y).
minimo(X,Y,Z):- ( X=<Y ->  Z=X ; Z=Y).

max_lista([],X):- maximo(0,X,_).
max_lista([H|L],X):- 
    
    maximo(H,X,Z) , 
    ( X == Z ->  max_lista(L,X) ).

min_lista([],X):- minimo(0,X,_).
min_lista([H|L],X):- 
    
    minimo(H,X,Z) , 
    ( X == Z ->  min_lista(L,X) ).

%Version 2
max_lista_recursive([], _).
max_lista_recursive([H|T], X):- X >= H, max_lista_recursive(T, X).
max_lista2([H|T], X):- pertenece(X, [H|T]), max_lista_recursive([H|T], X).

%16
suma_lista([],0).
suma_lista([H|L],X):- suma_lista(L,X1) , X is X1 + H.

%17
%Version 1
ordenada([]).
ordenada([H|L]):-
    min_lista([H|L],H),
    ordenada(L).

%Version 2
ordenada2([]).
ordenada2([_]).
ordenada2([H,A|T]):- ordenada2([A|T]), H =< A.

%18
%Version 1
llamada_recursiva(N, []):- N==N.
llamada_recursiva(N , [H|L]):- N == H , llamada_recursiva(N,L).

lista(N,[H|L]):-
 	length([H|L],N1) ,
    ( N == N1 ->  
    	llamada_recursiva(N , L) ),!,
     N == H.

%Version 2
lista2(N, L):- length(L, N), forall(member(Element, L), Element == N).

%19
lista_de_numeros(X, X, [X]):-!.
lista_de_numeros(N, M, [N|T]):-
    N =< M, R is N+1,
    lista_de_numeros(R, M, T).

%EJERCICIO 4
%1
rotacion(L1,L1, _).
rotacion([X|Y] , L1 , Long):- Long =\= 0, Aux is Long - 1,
	concatena(Y,[X],L2),rotacion(L2,L1,Aux).
	
multirot(L1,L2):- length(L1,X1),length(L2,X2) , X1==X2 , rotacion(L2,L1,X1).
multirot(L1,L2):- length(L1,Longitud),partirLista(L2,Longitud,Resultado1,Resultado2),
		rotacion(Resultado1 , L1, Longitud),multirot(L1,Resultado2).
		
partirLista(L,0,[],L).
partirLista([X|Y],Long,[X|L1],L2):- Aux is Long-1 , partirLista(Y,Aux,L1,L2).

%2
llamada_recursiva(_, []):-!.
llamada_recursiva(N , [N|L]):- llamada_recursiva(N,L).

verificarElementos(0, L):- not(pertenece(0, L)).
verificarElementos(N, L):- pertenece(N, L), Aux is N - 1, verificarElementos(Aux, L).

son_consecutivas(N, [H|T]):- N =\= 0, verificarElementos(N, [H|T]), son_consecutivas_recursive([H|T]), !.

son_consecutivas_recursive([]).
son_consecutivas_recursive([H|T]):- partirLista([H|T], H, R1, R2), lista(H, R1), son_consecutivas_recursive(R2).

%PARCIAL
%Tipo triángulo
tipoTriangulo(L, L, L, equilatero) :- esTriangulo(L, L, L) , !.
tipoTriangulo(L, L, R, isosceles) :- esTriangulo(L, L, R), !.
tipoTriangulo(R, L, L, isosceles) :- esTriangulo(L, L, R), !.
tipoTriangulo(L, R, L, isosceles) :- esTriangulo(L, L, R), !.
tipoTriangulo(L1, L2, L3, escaleno) :- esTriangulo(L1, L2, L3), L1 =\= L2, L2 =\= L3, L3 =\= L1, !.

esTriangulo(L1, L2, L3) :- L1 > 0, L2 > 0, L3 > 0, (L1 + L2) > L3, (L2 + L3) > L1, (L1 + L3) > L2.

%Familia
es_hermano(X,Y):- esPadre(Z, X), esPadre(Z,Y), X \= Y.
es_primo(X,Y):- esPadre(Z,X), esPadre(W,Y), es_hermano(Z,W).
es_nieto(X,Y):- esPadre(Y, Z), esPadre(Z, X).

es_descendiente(X,Y):- esPadre(Y,X).
es_descendiente(X,Y):- esPadre(Z,X), es_descendiente(Z, Y).






