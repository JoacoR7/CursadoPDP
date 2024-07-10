%%E1
es_padre(a, b).
es_padre(a, c).
es_padre(b, d).
es_padre(b, e).
es_padre(c, f).

es_hermano(X,Y):-es_padre(Z,X),es_padre(Z,Y),X\==Y.
es_primo(X,Y):- es_padre(Z, Y), es_padre(W, X), es_hermano(W, Z).
es_nieto(X,Y):- es_padre(Y,Z),es_padre(Z,X).

es_descendiente(X,Y):- es_padre(Y,X).
es_descendiente(X,Y):- es_padre(Y,Z), es_descendiente(X,Z).


%%E2
mayor_cero(X,Y,Z):- X>0, Y>0, Z>0.
suma_dos_lados(X,Y,Z):- X+Y >= Z, X+Z >= Y, Y+Z >= X.

triangulo(X,Y,Z):- mayor_cero(X,Y,Z), suma_dos_lados(X,Y,Z).

tipo_triangulo(X,X,X,T):- triangulo(X,X,X), T = "equilatero", !.
tipo_triangulo(X,X,Z,T):- triangulo(X,X,Z), X \== Z, T = "isosceles", !.
tipo_triangulo(X,Y,X,T):- triangulo(X,Y,X), X \== Y, T = "isosceles", !.
tipo_triangulo(X,Y,Y,T):- triangulo(X,Y,Y), Y \== X, T = "isosceles", !.
tipo_triangulo(X,Y,Z,T):- triangulo(X,Y,Z), T = "escaleno", !.

divFromTo([H|T], I , F, T, L):- I<F, I1 is I+1, divFromTo(T,I1,F,T,L).
divFromTo([H|T], I , F, T, [H|L]):- I>=F, I<T, ! , I1 is I+1, divFromTo(T,I1,F,T,L).
divFromTo(_,_,_,_,[]).
