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

% Laboratorium 4 listy 
% ?- G = 3, O=[4,5,6], L=[O|G].
% G = 3,
% O = [4, 5, 6],
% L = [[4, 5, 6]|3].

% ?- L = [2], L=[X|Y].
% L = [2],
% X = 2,
% Y = [].

%  L = [2], L=[X, Y|Z].
% false.

% jeśli L1 jest puste to L3 = L2
% jeśli L1 nie jest puste, to L3 jest uzyskiwane przez umieszczenie głowy L1 na początku listy uzyskanej przez połączenie ogona % L1 z L2
concat([],L2,L2).
concat([Head|Tail],L2,[Head|L3]):-concat(Tail,L2,L3).

% ?- concat([1,2],X,[1,2,3,4,5]).
% X = [3, 4, 5].

% ?- concat(X,Y,[p,r,l,o,g]).
% X = [],
% Y = [p, r, l, o, g] ;
% X = [p],
% Y = [r, l, o, g] ;
% X = [p, r],
% Y = [l, o, g] ;
% X = [p, r, l],
% Y = [o, g] ;
% X = [p, r, l, o],
% Y = [g] ;
% X = [p, r, l, o, g],
% Y = [] ;
% false.

% ?- concat([2,3,5],[7,11,13,17],X).
% X = [2, 3, 5, 7, 11, 13, 17].

% ?- concat([d,z,i,e,n],[d,o,b,r,y],X).
% X = [d, z, i, e, n, d, o, b, r|...].

% Ten program skutecznie usuwa wszystkie wystąpienia litery z listy
% Wstawianie elementu do listy jest dokładnym przeciwieństwem usuwania elementu. Dlatego nie jest nawet konieczne pisanie
% oddzielnego programu, a jedynie użycie predykatu delete w inny sposób.
delete1(Head,[Head|Tail],Tail).
delete1(X,[Y|Tail],[Y|L1]):-delete1(X,Tail,L1).

% ?- delete1(1,[4,8,5,1],X).
% X = [4, 8, 5] ,
% false.

% ?- delete1(43,X,[a,b,c,d]).
% X = [43, a, b, c, d] ;
% X = [a, 43, b, c, d] ;
% X = [a, b, 43, c, d] ;
% X = [a, b, c, 43, d] ;
% X = [a, b, c, d, 43] ;
% false.

% ?- delete1(X,[s,z,t,u,k,a],[s,z,u,k,a]).
% X = t ;
% false.

% Laboratorium 5

% Zadanie 1
% definicja predykatów(naszym celem jest, aby Prolog utworzył wystąpienie tego typu terminu z odpowiedzią, która 
% jest zgodna z naszymi wskazówkami)
lepszy(X,Y,podium(X,Y,_)).
lepszy(X,Y,podium(_,X,Y)).
lepszy(X,Y,podium(X,_,Y)).

% szukamy najlepszego
rozwiazanieSzukanePodium(P) :-
    lepszy(dziecko(piotr,_),dziecko(_,czerwona),P),
    lepszy(dziecko(jacek,zlota),dziecko(_,zielona),P).

% wyświetlamy kolejność na podium
wyswietlPodium(podium(X,Y,Z)):-
    write('Kolejność na podium:'),nl,
    write('1. '),writeln(X),
    write('2. '),writeln(Y),
    write('3. '),writeln(Z).

% Przykład zastosowania
% ?- rozwiazanieSzukanePodium(P),wyswietlPodium(P),false.
% Kolejność na podium:
% 1. dziecko(jacek,zlota)
% 2. dziecko(piotr,zielona)
% 3. dziecko(_16006,czerwona)
% false.

% Zadanie 2

% dom
dom(kolor, narodowosc, napoj, zwierz, sport).

% ulica(D1,D2,...,D5) - ....
ulica(d1, d2, d3, d4, d5).

% stoiNaUlicy(D,U) - na jakim miejscu stoi domek
stoiNaUlicy(D,ulica(D,_,_,_,_)).
stoiNaUlicy(D,ulica(_,D,_,_,_)).
stoiNaUlicy(D,ulica(_,_,D,_,_)).
stoiNaUlicy(D,ulica(_,_,_,D,_)).
stoiNaUlicy(D,ulica(_,_,_,_,D)).

% obokPoPrawej(X,Y,U) - domek X stoi po prawej stronie od domeka Y
obokPoPrawej(X,Y,ulica(Y,X,_,_,_)).
obokPoPrawej(X,Y,ulica(_,Y,X,_,_)).
obokPoPrawej(X,Y,ulica(_,_,Y,X,_)).
obokPoPrawej(X,Y,ulica(_,_,_,Y,X)).

% definicja pozostałych potrzebnych ...

% naSriodku(D,U) - osoba mieszka na śriodku ulicy
naSriodku(D, ulica(_,_,D,_,_)).

% naPierwszym(D,U) - osoba mieszka na początku ulicy
naPierwszym(D, ulica(D,_,_,_,_)).

% obok
% obok(X,Y,U) - osoba mieszka obok prawej osoby
obok(X,Y,U) :- obokPoPrawej(X,Y,U).
obok(X,Y,U) :- obokPoPrawej(Y,X,U).

% rozwiazanie
rozwiazanieSzukanaUlica(U):-
   stoiNaUlicy(dom(czerwony,anglik,_,_,_),U),
   stoiNaUlicy(dom(_,hiszpan,_,pies,_),U),
   stoiNaUlicy(dom(zielony,_,kawa,_,_),U),
   stoiNaUlicy(dom(_,ukrainiec,herbata,_,_),U),
   obokPoPrawej(dom(zielony,_,_,_,_),dom(bialy,_,_,_,_),U),
   stoiNaUlicy(dom(_,_,_,slimaki,tenis),U),
   stoiNaUlicy(dom(zolty,_,_,_,szachy),U),
   naSriodku(dom(_,_,mleko,_,_), U),
   naPierwszym(dom(norweg,_,_,_,_),U),
   obok(dom(_,_,_,_,rugby), dom(_,_,_,lis,_), U),
   obok(dom(_,_,_,_,rugby), dom(_,_,_,kon,_), U),
   stoiNaUlicy(dom(_,_,sok_pomaranczowy,_,siatkuwka),U),
   stoiNaUlicy(dom(_,japonczyk,_,_,go),U),
   obok(dom(norweg,_,_,_,_), dom(niebieski,_,_,_,_), U),
   obok(dom(_,_,herbata,_,_), dom(_,_,milo,_,_), U).

% wyświetlamy kolejność na podium
wyswietlWlasciciela(dom(X,Y,U,K,J)):-
   write('wlasciceil zebry:'),nl,
   write('kolor. '),writeln(X),
   write('narodowosc. '),writeln(Y),
   write('napoj. '),writeln(U),
   write('zwierz. '),writeln(K),
   write('sport. '),writeln(J).


% ⼀?- rozwiazanieSzukanaUlica(dom(_,_,_,zebra,_)).
% false.

% ?- wysietlWlasciciela(dom(_,_,_,zebra,_)).
% Correct to: "wyswietlWlasciciela(dom(_,_,_,zebra,_))"? yes
% wlasciceil zebry:
% kolor. _570
% narodowosc. _572
% napoj. _574
% zwierz. zebra
% sport. _578
% true.

% Część B

% osoba
osoba(profesja, przekaska, ubranie, gra, imie).

% kolejka
kolejka(os1, os2, os3, os4, os5).

% stoiWKolejce(Os,K) - na jakim miejscu czeka osoba
stoiWKolejce(Os,kolejka(Os,_,_,_,_)).
stoiWKolejce(Os,kolejka(_,Os,_,_,_)).
stoiWKolejce(Os,kolejka(_,_,Os,_,_)).
stoiWKolejce(Os,kolejka(_,_,_,Os,_)).
stoiWKolejce(Os,kolejka(_,_,_,_,Os)).

% naSriodku(Os,K) - osoba czeka na śriodku kolejki
naSriodku(Os, kolejka(_,_,Os,_,_)).

% obokTuzPrawej(X,Y,K) - osoba X stoi tuż stronie od osoby Y
obokTuzPrawej(X,Y,kolejka(Y,X,_,_,_)).
obokTuzPrawej(X,Y,kolejka(_,Y,X,_,_)).
obokTuzPrawej(X,Y,kolejka(_,_,Y,X,_)).
obokTuzPrawej(X,Y,kolejka(_,_,_,Y,X)).

% obok
% obok(X,Y,K) - osoba X czeka obok osoby Y
obok(X,Y,K) :- obokTuzPrawej(X,Y,K).
obok(X,Y,K) :- obokTuzPrawej(Y,X,K).

% pierwszaWKolejce(Os,K) - osoba stoi na początku koleji
pierwszaWKolejce(K, kolejka(Os,_,_,_,_)).

% rozwiazanie 
rozwiazanie(K):- 
   naSriodku(osoba(_,_,garnitura,_,_), K),
   stoiWKolejce(osoba(matematyk,_,_,_,jacek), K),
   obok(osoba(_,_,_,_,jacek), osoba(_,_,_,lola,_), K),
   pierwszaWKolejce(osoba(_,focacci,_,_,_), K),
   obok(osoba(_,_,_,_,weronika), osoba(_,_,_,valorant,_), K),
   stoiWKolejce(osoba(_,frytki,kapeluch,_,_), K),
   stoiWKolejce(osoba(_,_,_,cyberpank,krzysiek), K),
   stoiWKolejce(osoba(_,paluszki,_,_,andrzej), K),
   obok(osoba(kierowca,_,_,_,_), osoba(_,focaccie,_,_,_), K),
   stoiWKolejce(osoba(kucharz,_,sportowe,_,_), K),
   stoiWKolejce(osoba(_,chipsy,_,call_of_duty,_), K),
   obokPoPrawej(osoba(kucharz,_,_,_,_),osoba(operator_koparki,_,_,_,_),K),
   stoiWKolejce(osoba(_,_,kowbojki,_,zbyszek), K),
   obok(osoba(_,kapelusz,_,_,_), osoba(_,_szal,_,_,_), K),
   stoiWKolejce(osoba(programista,laguny,_,_,_), K).

% wyświetlamy kolejność w kolejce
wyswietlKtoGraWiedzmina(osoba(X,Y,U,K,J)):-
   write('profesja:'),nl,
   write('przekaska. '),writeln(X),
   write('ubranie. '),writeln(Y),
   write('gra. '),writeln(U),
   write('imie. '),writeln(K).

% wyświetlamy kolejność w kolejce
   wyswietlInformacje(kolejka(os1, os2, os3, os4, os5)):-
   wyswietlKtoGraWiedzmina(os1),
   wyswietlKtoGraWiedzmina(os2),
   wyswietlKtoGraWiedzmina(os3),
   wyswietlKtoGraWiedzmina(os4),
   wyswietlKtoGraWiedzmina(os5).

% ?- wyswietlKtoGraWiedzmina(osoba(_,_,_,wiedzmin,_)).
% kto gra w wiedzmina:
% profesja. _5840
% przekaska. _5842
% ubranie. _5844
% gra. wiedzmin
% imie. _5848
% true.

% ?- wyswietlInformacje(kolejka(_,_,_,_,_)),false.
% kto gra w wiedzmina:
% profesja. _10882
% przekaska. _10884
% ubranie. _10886
% gra. _10888
% imie. _10890
% kto gra w wiedzmina:
% profesja. _10894
% przekaska. _10896
% ubranie. _10898
% gra. _10900
% imie. _10902
% kto gra w wiedzmina:
% profesja. _10906
% przekaska. _10908
% ubranie. _10910
% gra. _10912
% imie. _10914
% kto gra w wiedzmina:
% profesja. _10918
% przekaska. _10920
% ubranie. _10922
% gra. _10924
% imie. _10926
% kto gra w wiedzmina:
% profesja. _10930
% przekaska. _10932
% ubranie. _10934
% gra. _10936
% imie. _10938
% false.
