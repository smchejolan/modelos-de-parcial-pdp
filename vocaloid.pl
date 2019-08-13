%sabeCantar(Vocaloid,Cancion).
sabeCantar(megurineLuka,cancion(nightFever,4)).
sabeCantar(megurineLuka,cancion(foreverYoung,5)).
sabeCantar(hatsuneMiku,cancion(tellYourWorld,4)).
sabeCantar(gumi,cancion(foreverYoung,4)).
sabeCantar(gumi,cancion(tellYourWorld,5)).
sabeCantar(seeU,cancion(novermberRain,4)).
sabeCantar(seeU,cancion(nightFever,5)).

esVocaloid(Vocaloid):-
    sabeCantar(Vocaloid,_).

%1
cantanteNovedoso(Vocaloid):-
    esVocaloid(Vocaloid),
    cantCancionesQueSabe(Vocaloid,Cant),
    tiempoTotalCanciones(Vocaloid,TiempoTotal),
    TiempoTotal < 15,
    Cant >= 2.

tiempoDeCanciones(Vocaloid,TiempoCanciones):-
    findall(TiempoCancion,sabeCantar(Vocaloid,cancion(_,TiempoCancion)),TiempoCanciones).

tiempoTotalCanciones(Vocaloid,TiempoTotal):-
    tiempoDeCanciones(Vocaloid,TiempoCanciones),
    sumlist(TiempoCanciones,TiempoTotal).

cantCancionesQueSabe(Vocaloid,Cant):-
    findall(Cancion,sabeCantar(Vocaloid,Cancion),Canciones),
    length(Canciones, Cant).
    
%2
cantanteAcelerado(Vocaloid):-
    esVocaloid(Vocaloid),
    tiempoDeCanciones(Vocaloid,Tiempos),
    maximoTiempoMenorA(Tiempos,4).

maximoTiempoMenorA(Tiempos,MenorA):-
    max_member(Maximo, Tiempos),
    Maximo =< MenorA.

%3
%concierto(Concierto,Pais,Fama,Tipo).
%gigante(CantMinimaCanciones,TiempoMinimo)
%mediano(TiempoMaximo)
%pequenio(MinimoParaUnaCancion)
concierto(mikuExpo,estadosUnidos,2000,gigante(2,6)).
concierto(magicalMirai,japon,3000,gigante(3,10)).
concierto(vocalektVisions,estadosUnidos,1000,mediano(9)).
concierto(mikuFest,argentina,100,pequenio(4)).

esConcierto(Concierto):-
    concierto(Concierto,_,_,_).

%4

tipoDeConcierto(Concierto,TipoDeConcierto):-
    concierto(Concierto,_,_,TipoDeConcierto).

puedeParticipar(hatsuneMiku,mikuExpo).
puedeParticipar(hatsuneMiku,magicalMirai).
puedeParticipar(hatsuneMiku,vocalektVisions).
puedeParticipar(hatsuneMiku,mikuFest).
puedeParticipar(Vocaloid,Concierto):-
    esVocaloid(Vocaloid),
    tipoDeConcierto(Concierto,TipoDeConcierto),
    cumpleCondiciones(Vocaloid,TipoDeConcierto).

cumpleCondiciones(Vocaloid,gigante(CantMinimaCanciones,TiempoMinimo)):-
    cantCancionesQueSabe(Vocaloid,Cant),
    tiempoTotalCanciones(Vocaloid,TiempoTotal),
    Cant >= CantMinimaCanciones,
    TiempoTotal >= TiempoMinimo.
cumpleCondiciones(Vocaloid,mediano(TiempoMaximo)):-
    tiempoTotalCanciones(Vocaloid,TiempoTotal),
    TiempoTotal =< TiempoMaximo.
cumpleCondiciones(Vocaloid,pequenio(MinimoParaUnaCancion)):-
    tiempoDeCanciones(Vocaloid,Tiempos),
    not(maximoTiempoMenorA(Tiempos,MinimoParaUnaCancion)).

%5
puntosDeFama(Concierto,PuntosDeFama):-
    concierto(Concierto,_,PuntosDeFama,_).

famaTotal(Vocaloid,Fama):-
    findall(PuntosDeFama,(puedeParticipar(Vocaloid,Concierto),puntosDeFama(Concierto,PuntosDeFama)),Puntos),
    sumlist(Puntos,Fama).

masFamoso(Vocaloid):-
    esVocaloid(Vocaloid),
    forall((esVocaloid(Vocaloid2),Vocaloid \= Vocaloid2),(famaTotal(Vocaloid,FamaTotal1),famaTotal(Vocaloid2,FamaTotal2),FamaTotal1>FamaTotal2)).
