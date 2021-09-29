% Noelia Pérez García-Consuegra

%-------------------------------------------------------------------------------------------------------
% Ejercicio 1

% (a) Usando igualdad sintactica:
elimina1([ ],X,[ ]).
elimina1([X|R],Y,NR) :- Y == X, elimina1(R,Y, NR).
elimina1([X|R],Y,[X|NR]) :- Y \== X, elimina1(R,Y,NR).
% (b) Usando unificacion:
elimina2([ ],X,[ ]).
elimina2([X|R],Y,NR) :- Y = X, elimina2(R,Y, NR).
elimina2([X|R],Y,[X|NR]) :- Y \= X, elimina2(R,Y,NR).
% (c) Combinando las dos anteriores:
elimina3([ ],X,[ ]).
elimina3([X|R],X,NR) :- elimina3(R,X,NR).
elimina3([X|R],Y,[X|NR]) :- Y \== X, elimina3(R,Y,NR).

% ?- elimina1([a,b,a,c],a,L).
% L = [b, c] .

% ?- elimina2([a,b,a,c],a,L).
% L = [b, c] .

% ?- elimina3([a,b,a,c],a,L).
% L = [b, c] .

% ?- elimina1([a,b,a,c],X,L). 
% L = [a, b, a, c].

% ?- elimina2([a,b,a,c],X,L).
% X = a,
% L = [b, c] 

% ?- elimina3([a,b,a,c],X,L).
% X = a,
% L = [b, c] 


%-------------------------------------------------------------------------------------------------------
% Ejercicio 2

% a)

sumatree(void, 0).
sumatree(arbol(E,I,D), N):- number(E), sumatree(I, N1), sumatree(D, N2), W is N1+N2, N is W+E.

% b)

arbolalista(void, []).
arbolalista(arbol(E,I,D), [E|L]) :- arbolalista(I, L1), arbolalista(D,L2), append(L1,L2,L).

% La siguiente funcion elimina un elemento de cada

elimina_uno_de_cada([],_,[]):-!.
elimina_uno_de_cada([X|Xs],Ac,[X|Z]) :- member(X,Ac), !, elimina_uno_de_cada(Xs,Ac,Z).
elimina_uno_de_cada([X|Xs],Ac,Z) :-  elimina_uno_de_cada(Xs,[X|Ac],Z).

% Si quito uno de cada mientras el mio este
% y acabo dejando la lista vacia el mio estaba >= veces que los de más

elimina_esta(_,[]).
elimina_esta(X,L):- member(X,L), elimina_uno_de_cada(L,[],L1),  elimina_esta(X,L1).

maximo(A,X):- arbolalista(A,L), elimina_esta(X,L).


%-------------------------------------------------------------------------------------------------------
% Ejercicio 3
% sublistas([],[]).
    
%     sublistas ([First|Rest],[First|Sub]):-
%         sublistas (Rest,Sub).
        
%     sublistas ([_|Rest],Sub):-
%         sublistas (Rest,Sub).



prefijo3([],_).
prefijo3([X|Xs],[X|Ys]) :-
	prefijo3(Xs,Ys).

es_sublista3([],_).
es_sublista3([X|Xs],XsYs) :-
	prefijo3([X|Xs],XsYs).
es_sublista3([X|Xs],[_|AsXsBs]) :-
	es_sublista3([X|Xs],AsXsBs).

sublistas3([X|Xs],L):- setof([Y|Ys],es_sublista3([Y|Ys],[X|Xs]),L).

%-------------------------------------------------------------------------------------------------------
% Ejercicio 4
torres_hanoi(1,A,B,_,[mov(A,B)]):- !.
torres_hanoi(N,A,B,C,L) :- N1 is N-1, torres_hanoi(N1, A, C, B, L1),
                               torres_hanoi(N1, C, B, A, L2), 
                               append(L1, [mov(A,B)|L2], L).




