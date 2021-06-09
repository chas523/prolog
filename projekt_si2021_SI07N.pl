%Hoishyk Siarhei, Vladyslav Bahlai , Vladyslav Semak

% LABORATORIUM 2
% X jest rodzicem Y
rodzic("Alexandr", "Olya").
rodzic("Masha","Olya").
rodzic("Alexandr","Alina").
rodzic("Masha","Alina").
rodzic("Alexandr","Leha").
rodzic("Masha","Leha").
rodzic("Leha","Gleb").
rodzic("Dasha","Gleb").
rodzic("Alina","Maks").
rodzic("Leha","Maks").
rodzic("Ira","Alina").
rodzic("Evgen","Alexandr").


% X jest menczyszna
menczyszna("Alexandr").
menczyszna("Leha").
menczyszna("Pavel").
menczyszna("Dima").
menczyszna("Evgen").

% X jest kobieta
kobieta("Olya").
kobieta("Alina").
kobieta("Masha").
kobieta("Ira").

% X jest ojcem Y, jeśli X jest rodzicem, i oraz jest mężczyzną 
ojciec(X,Y):-
    rodzic(X,Y),menczyszna(X).
% X jest matka Y, jezeli X jest rodzicem i rowniez jest kobieta 
matka(X,Y):-
    rodzic(X,Y),kobieta(X).

% X jest siostra Y jezeli oni ma wspolecznego rodzica Z, oraz jezeli X jest kobieta 
% i X nie jest rowny Y (czlowiek nie moze byc sam siebie siostre)
siostra(X,Y):-
    rodzic(Z,X),rodzic(Z,Y),kobieta(X), X\=Y.
% dziala podobnie jak siostra
braciszek(X,Y):-
    rodzic(Z,X),rodzic(Z,Y),menczyszna(X), X\=Y.
% X Jest bacia Y jezeli X jest matka Z, a Z jest matka Y 
babcia(X,Y):-
    matka(X,Z),matka(Z,Y).
% dziala podobnie jak babcia 
dziadek(X,Y):-
    ojciec(X,Z),ojciec(Z,Y).

% X jest dzieckiem Y jezeli Y jest rodzicem Y
dziecko(X,Y):-
    rodzic(Y,X).

% X jest pradziadkiem Y jezeli X jest ojciem Z a Z jest ojcem Y
pradziadek(X,Y):-
    ojciec(X,Z),ojciec(Z,Y).

% dziala podobnie jak pradziadek
prababcia(X,Y):-
    matka(X,Z),matka(Z,Y).

% jeżeli X jest rodzicem Y, to X jest przodkiem Y;
% jeżeli X jest rodzicem Y a Y jest przodkiem Z, to X jest przodkiem Z
przodek(X,Y):-rodzic(X,Y).
przodek(X,Z):-rodzic(X,Y),przodek(Y,Z).

% definicja krawędzi
krawedz((1,1),(2,2)).
krawedz((1,2),(2,2)).
krawedz((2,2),(3,1)).
krawedz((2,2),(2,3)).
krawedz((2,3),(3,3)).
krawedz((3,1),(2,1)).
% krawedz((2,1),(2,2)).

% jeżeli istnieje krawiędź między X a Y, to istnieje ścieżka między X a Y
dpath(X,Y):-
    krawedz(X,Y).
% ?- dpath((1,2),X).
% X =  (2, 2) .


% jeżeli istnieje krawiędź między X a Y, i między Y a Z to istnieje ścieżka między X a Z
dpath(X,Z):-
    krawedz(X,Y),
    dpath(Y,Z).

% ?- dpath((1,2),(2,1)).
% true .


% LABORATORIUM 3

% isnumber(X) <=> X jest liczba (symboliczna)
% s(x)  <=> oznacza funkcje nastepnika, "s(X)=X+1"
isnumber(zero). %zero jest liczbe 
isnumber(s(X):-isnumber(X)). %Jezeli X jest liczba, to X+1 tkaze jest liczba
% ?- isnumber(zero).
% yes 
% ?- isnumber(s(s(s(zero)))).
% yes
% ?- isnumber(s(s(2))).
% no
% X = zero; 
% X = s(zero); 
% X = s(s(zero)); 
% X = s(s(s(zero)))
% yes

isequal(X,X):-isnumber(X).  % rownowaznosc liczb 
isequal(s(X),s(Y)):-isequal(X,Y).  %  ?
% ?- isequal(zero,zero).
% yes 

% 0 jest mniejsze lub równe dowolnej innej liczbie 
% x + 1 <= y + 1 if x <= y
lessthanequal(zero,X):-isnumber(X). 
lessthanequal(s(X),s(Y)):-lessthanequal(X,Y).
% ?- lessthanequal(s(s(zero)), s(s(s(zero)))).
% yes 
% lessthanequal(s(s(zero)),s(zero)).
% no

% zero = X + X , dodowanie
add(zero,X,X):-isnumber(X).
add(s(X),Y,s(Z)):-add(X,Y,Z).

% ?- add(s(s(zero)),s(s(s(zero))),s(s(s(s(s(zero)))))).
% yes 

%odd jest prawdziwe jeżeli jest liczbą nieparzysta.
odd(s(zero)).
odd(s(s(X))):-odd(X).
% ?- even(s(zero)).
% yes 
% ?- odd(s(s(zero))).
% no

% even jest prawdziwe jeżeli jest liczbą parzysta.
even(zero).
even(s(s(X))):-odd(X).
% ?- even(s(zero)).
% no 
% ?- odd(s(s(zero))).
% yes

% jest prawdziwy jezeli XY = Z 
times(zero,X,zero) :- isnumber(X).
times(s(X),Y,Z) :- times(X,Y,Q), add(Y,Q,Z).
% ?- times(s(s(zero)), s(s(zero)),X).
% yes

% X/Y = Q (W arytmetyce liczb naturalnych )
quatient()

%  LABORATORIUM 5
% Użłamy binarnego członka predykatu (X,L), który ma być prawdziwy, jeśli element X jest członkiem listy L.
member(Head,[Head|Tail]).
member(X,[Head|Tail]) :- member(X,Tail).
% | ?- member(a,[a,b,c]).
%
% yes
% | ?- member(e,[a,b,c]).
%
%
% no
% aby znaleźć elementy listy możemy użyć tej operacji: 
% | ?- member(X,[a,b,c]).
%
% X = a ;
%
% X = b ;
%
% X = c ;
%
% no