:-set_prolog_flag(answer_write_options,[max_depth(0)]).
:-dynamic(ficha/2).

%-------------------------------------------------------------------------------Apartado 2.1, crear monton.--------------------------------------------------------------------------------------------
crear_monton(Monton):-crear_monton(0,0),setof(ficha(X,Y),ficha(X,Y),Monton).
crear_monton(7,7):-!.
crear_monton(X,0):-assert(ficha(0,X)),!,X1 is X+1,crear_monton(X1,X1).
crear_monton(X,Y):-assert(ficha(Y,X)),Y1 is Y-1,crear_monton(X,Y1).

%-------------------------------------------------------------------------------Apartado 2.2, coger ficha.---------------------------------------------------------------------------------------------
coger_ficha(Monton,Ficha,NuevoMonton):-length(Monton,Tamano),random(0,Tamano,PosFicha),calcular_ficha_posicion(PosFicha,Monton,Ficha,0),borrar_ficha_monton(Monton,Ficha,NuevoMonton,[]).

calcular_ficha_posicion(PosFicha,[X|_],X,Cont):-Cont==PosFicha,!.
calcular_ficha_posicion(PosFicha,[_|Y],Ficha,Cont):-ContAux is Cont+1,calcular_ficha_posicion(PosFicha,Y,Ficha,ContAux).

borrar_ficha_monton([X|Y],X,NuevoMonton,Aux):-concatenar(Aux,Y,NuevoMonton),!.
borrar_ficha_monton([X|Y],Ficha,NuevoMonton,Aux):-concatenar(Aux,[X],Aux1),borrar_ficha_monton(Y,Ficha,NuevoMonton,Aux1).

concatenar([],L,L).
concatenar([X|R],L,[X|Z]):-concatenar(R,L,Z).

%-------------------------------------------------------------------------------Apartado 2.3, repartir fichas.-----------------------------------------------------------------------------------------
repartir_fichas(Monton,Jugador1,Jugador2,NuevoMonton):-repartir_fichas_jugador1(Monton,Jugador1,Jugador2,NuevoMonton,0,0,0).

repartir_fichas_jugador1(Monton,Jugador1,Jugador2,MontonAux,_,_,13):-repartir_fichas_jugador2(Monton,Jugador1,Jugador2,MontonAux,0,0,14),!.
repartir_fichas_jugador1(Monton,[Ficha|Jugador1],Jugador2,MontonAux,2,0,Contador):-!,ContadorAux is Contador+1,coger_ficha(Monton,Ficha,NuevoMonton),repartir_fichas_jugador2(NuevoMonton,Jugador1,Jugador2,MontonAux,0,0,ContadorAux).
repartir_fichas_jugador1(Monton,[Ficha|Jugador1],Jugador2,MontonAux,X,0,Contador):-Xaux is X+1,ContadorAux is Contador+1,coger_ficha(Monton,Ficha,NuevoMonton),repartir_fichas_jugador1(NuevoMonton,Jugador1,Jugador2,MontonAux,Xaux,0,ContadorAux).

repartir_fichas_jugador2(Monton,[],[Ficha],NuevoMonton,_,_,14):-!,coger_ficha(Monton,Ficha,NuevoMonton).
repartir_fichas_jugador2(Monton,Jugador1,[Ficha|Jugador2],MontonAux,0,2,Contador):-!,ContadorAux is Contador+1,coger_ficha(Monton,Ficha,NuevoMonton),repartir_fichas_jugador1(NuevoMonton,Jugador1,Jugador2,MontonAux,0,0,ContadorAux).
repartir_fichas_jugador2(Monton,Jugador1,[Ficha|Jugador2],MontonAux,0,X,Contador):-Xaux is X+1,ContadorAux is Contador+1,coger_ficha(Monton,Ficha,NuevoMonton),repartir_fichas_jugador2(NuevoMonton,Jugador1,Jugador2,MontonAux,0,Xaux,ContadorAux).

%-------------------------------------------------------------------------------Apartado 2.4, turno de jugador.----------------------------------------------------------------------------------------
%-------------------------------------------------------------------------------2.4.1, calcular fichas posibles.---------------------------------------------------------------------------------------
%Caso inicial en el que la mesa está vacía. Habrá que ver cuál es el mayor doble.
mayor_doble([],Jugador,[DobleMax],[DobleMax]):-findall(ficha(X,X),(member(ficha(X,X),Jugador)),Dobles),calcularDobleMax(Dobles,DobleMax),!.
mayor_doble([],_,[-1],[-1]):-!.
%Casos de juego normal.
fichas_posibles([],[X|_],[X],[X]):-!.
fichas_posibles(Mesa,Jugador,FichasIzquierda,FichasDerecha):-obtenerPrimeroYUltimo(Mesa,Izquierda,Derecha),Izquierda=ficha(XI,_),Derecha=ficha(_,YD),
                                                             setof(ficha(X,Y),(member(ficha(X,Y),Jugador),(XI=X;XI=Y)),FichasIzquierda),
                                                             setof(ficha(X,Y),(member(ficha(X,Y),Jugador),(YD=X;YD=Y)),FichasDerecha),!.
fichas_posibles(Mesa,Jugador,[],FichasDerecha):-obtenerPrimeroYUltimo(Mesa,_,Derecha),Derecha=ficha(_,YD),setof(ficha(X,Y),(member(ficha(X,Y),Jugador),(YD=X;YD=Y)),FichasDerecha),!.
fichas_posibles(Mesa,Jugador,FichasIzquierda,[]):-obtenerPrimeroYUltimo(Mesa,Izquierda,_),Izquierda=ficha(XI,_),setof(ficha(X,Y),(member(ficha(X,Y),Jugador),(XI=X;XI=Y)),FichasIzquierda),!.
fichas_posibles(_,_,[],[]).

obtenerPrimeroYUltimo([Y],Y,Y).
obtenerPrimeroYUltimo([Primero|Y],Primero,Ultimo):-obtenerPrimeroYUltimo(Y,_,Ultimo).

calcularDobleMax([ficha(X,X)],ficha(X,X)).
calcularDobleMax([ficha(X,X)|R],ficha(X,X)):-calcularDobleMax(R,ficha(Xaux,Xaux)),X>Xaux.
calcularDobleMax([ficha(X,X)|R],ficha(Xaux,Xaux)):-calcularDobleMax(R,ficha(Xaux,Xaux)),X<Xaux.

%-------------------------------------------------------------------------------2.4.2, poner ficha.----------------------------------------------------------------------------------------------------
%Caso inicial. Poner el mayor doble.
poner_ficha([],Jugador,[Ficha],[Ficha],JugadorAux,[Ficha]):-borrar_ficha_monton(Jugador,Ficha,JugadorAux,[]).
%Caso general.
poner_ficha(Mesa,Jugador,_,_,JugadorAux,MesaAux):-obtenerPrimeroYUltimo(Mesa,Izquierda,Derecha),Izquierda=ficha(XI,_),Derecha=ficha(_,XD),findall(ficha(X,X),(member(ficha(X,X),Jugador),X==XI),DobleI),
                                                  findall(ficha(X,X),(member(ficha(X,X),Jugador),X==XD),DobleD),DobleI=[ficha(I,I)],DobleD=[ficha(D,D)],I\==D,!,
                                                  write('El jugador coloca dos dobles.\n'),colocar_ficha(Mesa,ficha(I,I),I,izquierda,Mesa1),
                                                  colocar_ficha(Mesa1,ficha(D,D),D,derecha,MesaAux),borrar_ficha_monton(Jugador,ficha(I,I),JugadorAux1,[]),
                                                  borrar_ficha_monton(JugadorAux1,ficha(D,D),JugadorAux,[]).
poner_ficha(Mesa,Jugador,FichasIzquierda,FichasDerecha,JugadorAux,MesaAux):-obtenerPrimeroYUltimo(Mesa,Izquierda,_),Izquierda=ficha(ValorIzq,_),lista_mas_larga(FichasIzquierda,FichasDerecha,FichasIzquierda),!,
                                                                            calcular_ficha(Mesa,ValorIzq,FichasIzquierda,Ficha),!,borrar_ficha_monton(Jugador,Ficha,JugadorAux,[]),colocar_ficha(Mesa,Ficha,ValorIzq,izquierda,MesaAux).
poner_ficha(Mesa,Jugador,FichasIzquierda,FichasDerecha,JugadorAux,MesaAux):-obtenerPrimeroYUltimo(Mesa,_,Derecha),Derecha=ficha(_,ValorDer),lista_mas_larga(FichasIzquierda,FichasDerecha,FichasDerecha),
                                                                            calcular_ficha(Mesa,ValorDer,FichasDerecha,Ficha),!,borrar_ficha_monton(Jugador,Ficha,JugadorAux,[]),colocar_ficha(Mesa,Ficha,ValorDer,derecha,MesaAux).

lista_mas_larga(ListaIzquierda,ListaDerecha,ListaIzquierda):-length(ListaIzquierda,LI),length(ListaDerecha,LD),LI>=LD.
lista_mas_larga(ListaIzquierda,ListaDerecha,ListaDerecha):-length(ListaIzquierda,LI),length(ListaDerecha,LD),LI<LD.

calcular_ficha(Mesa,Valor,ListaFichas,Ficha):-((Ficha=ficha(X,Valor),member(Ficha,ListaFichas));(Ficha=ficha(Valor,X),member(Ficha,ListaFichas))),contar_ficha_mesa(Mesa,X,NumVeces),
                                                      forall(member(FichaAux,ListaFichas),
                                                      ((FichaAux=ficha(Valor,Y2),contar_ficha_mesa(Mesa,Y2,VecesAux),NumVeces>=VecesAux);
                                                       (FichaAux=ficha(X2,Valor),contar_ficha_mesa(Mesa,X2,VecesAux),NumVeces>=VecesAux))).
                                       
/*contar_ficha_mesa(Mesa,Valor,Numveces):-findall(1,(member(ficha(X,Y),Mesa),(X=Valor;Y=Valor)),L),length(L,Numveces).  */
contar_ficha_mesa(Mesa,Valor,Numveces):-findall(1,((Ficha=ficha(X,Valor),member(Ficha,Mesa));(Ficha=ficha(Valor,X),member(Ficha,Mesa))),L),length(L,Numveces).

colocar_ficha(Mesa,ficha(X,Valor),Valor,izquierda,MesaAux):-concatenar([ficha(X,Valor)],Mesa,MesaAux),! .
colocar_ficha(Mesa,ficha(Valor,X),Valor,izquierda,MesaAux):-concatenar([ficha(X,Valor)],Mesa,MesaAux),!.
colocar_ficha(Mesa,ficha(X,Valor),Valor,derecha,MesaAux):-concatenar(Mesa,[ficha(Valor,X)],MesaAux),!.
colocar_ficha(Mesa,ficha(Valor,X),Valor,derecha,MesaAux):-concatenar(Mesa,[ficha(Valor,X)],MesaAux).

%-------------------------------------------------------------------------------2.4.3, coger ficha.----------------------------------------------------------------------------------------------------
conseguir_ficha(Monton,Mesa,Jugador,JugadorAux,MesaAux,MontonAux):-conseguir_ficha(Monton,Mesa,Jugador,JugadorAux,MesaAux,MontonAux,0,_).

conseguir_ficha(Monton,Mesa,Jugador,Jugador,Mesa,Monton,2,_):-write('No puede colocar la ficha, asi que pasa.\n'),!.
conseguir_ficha([],Mesa,Jugador,Jugador,Mesa,[],_,_):-write('No puede robar ficha, asi que pasa.\n'),!.
conseguir_ficha(Monton,Mesa,Jugador,JugadorAux,MesaAux,MontonAux,Contador,_):-coger_ficha(Monton,Ficha,NuevoMonton),se_puede_poner(NuevoMonton,Mesa,Jugador,JugadorAux,MesaAux,MontonAux,Contador,Ficha).

%Se puede poner la ficha en la izquierda o en la derecha.
se_puede_poner(Monton,Mesa,Jugador,Jugador,MesaAux,Monton,_,Ficha):-obtenerPrimeroYUltimo(Mesa,_,Derecha),Derecha=ficha(_,YD),(Ficha=ficha(_,YD);Ficha=ficha(YD,_)),
                                                                    colocar_ficha(Mesa,Ficha,YD,derecha,MesaAux),write('El jugador ha colocado la ficha robada.\n'),!.
se_puede_poner(Monton,Mesa,Jugador,Jugador,MesaAux,Monton,_,Ficha):-obtenerPrimeroYUltimo(Mesa,Izquierda,_),Izquierda=ficha(XI,_),(Ficha=ficha(_,XI);Ficha=ficha(XI,_)),
                                                                    colocar_ficha(Mesa,Ficha,XI,izquierda,MesaAux),write('El jugador ha colocado la ficha robada.\n'),!.
%No se puede poner la ficha.
se_puede_poner(Monton,Mesa,Jugador,JugadorAux,MesaAux,MontonAux,Contador,Ficha):-write('El jugador roba una ficha del monton.\n'),concatenar(Jugador,[Ficha],NuevoJugador),ContadorAux is Contador+1,
                                                                                 conseguir_ficha(Monton,Mesa,NuevoJugador,JugadorAux,MesaAux,MontonAux,ContadorAux,Ficha).

%-------------------------------------------------------------------------------JUGAR------------------------------------------------------------------------------------------------------------------

jugar:-consult('dibujaMesa.pl'),crear_monton(Monton),repartir_fichas(Monton,Jugador1,Jugador2,NuevoMonton),comparar_dobles(Jugador1,Jugador2,jugador1,Doble),!,write('..... EMPIEZA EL JUEGO .....\n'),empezar(jugador1,Doble,NuevoMonton,Jugador1,Jugador2).
jugar:-consult('dibujaMesa.pl'),crear_monton(Monton),repartir_fichas(Monton,Jugador1,Jugador2,NuevoMonton),comparar_dobles(Jugador1,Jugador2,jugador2,Doble),write('..... EMPIEZA EL JUEGO .....\n'),empezar(jugador2,Doble,NuevoMonton,Jugador1,Jugador2).

comparar_dobles(Jugador1,Jugador2,jugador1,Doble):-mayor_doble([],Jugador1,[Doble],[Doble]),Doble=ficha(X1,X1),mayor_doble([],Jugador2,[ficha(X2,X2)],[ficha(X2,X2)]),X1>=X2,!.
comparar_dobles(_,Jugador2,jugador2,Doble):-mayor_doble([],Jugador2,[Doble],[Doble]).

empezar(jugador1,Doble,Monton,Jugador1,Jugador2):-write('Empieza: JUGADOR1\n'),poner_ficha([],Jugador1,[Doble],[Doble],Jugador1Aux,Mesa),
                                                  escribir(Mesa,Monton,Jugador1,Jugador2),
                                                  write('............................\n'),turno(jugador2,Monton,Mesa,Jugador1Aux,Jugador2,Doble).
empezar(jugador2,Doble,Monton,Jugador1,Jugador2):-write('Empieza: JUGADOR2\n'),poner_ficha([],Jugador2,[Doble],[Doble],Jugador2Aux,Mesa),
                                                  escribir(Mesa,Monton,Jugador1,Jugador2),
                                                  write('............................\n'),turno(jugador1,Monton,Mesa,Jugador1,Jugador2Aux,Doble).

turno(jugador1,Monton,Mesa,Jugador1,Jugador2,Doble):-fichas_posibles(Mesa,Jugador1,FichasIzquierda,FichasDerecha),(FichasIzquierda\==[];FichasDerecha\==[]),!,write('Turno del Jugador1\n'),poner_ficha(Mesa,Jugador1,FichasIzquierda,FichasDerecha,JugadorAux,MesaAux),
                                               comprobar_estado(jugador1,JugadorAux,Monton,MesaAux,Jugador2,Doble).
turno(jugador1,Monton,Mesa,Jugador1,Jugador2,Doble):-write('Turno del Jugador1\n'),fichas_posibles(Mesa,Jugador1,[],[]),conseguir_ficha(Monton,Mesa,Jugador1,JugadorAux,MesaAux,MontonAux),
                                               comprobar_estado(jugador1,JugadorAux,MontonAux,MesaAux,Jugador2,Doble).
turno(jugador2,Monton,Mesa,Jugador1,Jugador2,Doble):-fichas_posibles(Mesa,Jugador2,FichasIzquierda,FichasDerecha),(FichasIzquierda\==[];FichasDerecha\==[]),!,write('Turno del Jugador2\n'),poner_ficha(Mesa,Jugador2,FichasIzquierda,FichasDerecha,JugadorAux,MesaAux),
                                               comprobar_estado(jugador2,JugadorAux,Monton,MesaAux,Jugador1,Doble).
turno(jugador2,Monton,Mesa,Jugador1,Jugador2,Doble):-write('Turno del Jugador2\n'),fichas_posibles(Mesa,Jugador2,[],[]),conseguir_ficha(Monton,Mesa,Jugador2,JugadorAux,MesaAux,MontonAux),
                                               comprobar_estado(jugador2,JugadorAux,MontonAux,MesaAux,Jugador1,Doble).

comprobar_estado(jugador1,[],Monton,Mesa,JugadorOtro,ficha(X,Y)):-escribir(Mesa,Monton,[],JugadorOtro),
                                                                  write('El JUGADOR1 ha ganado\n'),length(JugadorOtro,Tamano),write('Al otro jugador le quedaban '),write(Tamano),write(' fichas y ha obtenido un total de '),calcularPuntos(JugadorOtro,Puntos),write(Puntos),write(' puntos.\n'),!,transformar(Mesa,MesaTrans),dibujaMesa(MesaTrans,fich(X,Y)).
comprobar_estado(jugador2,[],Monton,Mesa,JugadorOtro,ficha(X,Y)):-escribir(Mesa,Monton,JugadorOtro,[]),
                                                                  write('El JUGADOR2 ha ganado\n'),length(JugadorOtro,Tamano),write('Al otro jugador le quedaban '),write(Tamano),write(' fichas y ha obtenido un total de '),calcularPuntos(JugadorOtro,Puntos),write(Puntos),write(' puntos.\n'),!,transformar(Mesa,MesaTrans),dibujaMesa(MesaTrans,fich(X,Y)).
comprobar_estado(jugador1,Jugador1,[],Mesa,JugadorOtro,ficha(X,Y)):-fichas_posibles(Mesa,Jugador1,[],[]),fichas_posibles(Mesa,JugadorOtro,[],[]),!,
                                                                    escribir(Mesa,[],Jugador1,JugadorOtro),
                                                                    write('Se ha realizado un cierre.\n'),length(Jugador1,Tamano1),length(JugadorOtro,Tamano2),write('Al jugador 1 le quedaban '),write(Tamano1),write(' fichas con un total de '),calcularPuntos(Jugador1,Puntos1),write(Puntos1),write(' puntos.\n'),write('Al jugador 2 le quedaban '),write(Tamano2),write(' fichas con un total de '),calcularPuntos(JugadorOtro,Puntos2),write(Puntos2),write(' puntos.\n'),transformar(Mesa,MesaTrans),dibujaMesa(MesaTrans,fich(X,Y)).
comprobar_estado(jugador2,Jugador2,[],Mesa,JugadorOtro,ficha(X,Y)):-fichas_posibles(Mesa,Jugador2,[],[]),fichas_posibles(Mesa,JugadorOtro,[],[]),!,
                                                                    escribir(Mesa,[],JugadorOtro,Jugador2),
                                                                    write('Se ha realizado un cierre.\n'),length(Jugador2,Tamano1),length(JugadorOtro,Tamano2),write('Al jugador 1 le quedaban '),write(Tamano1),write(' fichas con un total de '),calcularPuntos(JugadorOtro,Puntos1),write(Puntos1),write(' puntos.\n'),write('Al jugador 2 le quedaban '),write(Tamano2),write(' fichas con un total de '),calcularPuntos(Jugador2,Puntos2),write(Puntos2),write(' puntos.\n'),transformar(Mesa,MesaTrans),dibujaMesa(MesaTrans,fich(X,Y)).
comprobar_estado(jugador1,Jugador1,Monton,Mesa,Jugador2,Doble):-escribir(Mesa,Monton,Jugador1,Jugador2),
                                                                write('............................\n'),!,turno(jugador2,Monton,Mesa,Jugador1,Jugador2,Doble).
comprobar_estado(jugador2,Jugador2,Monton,Mesa,Jugador1,Doble):-escribir(Mesa,Monton,Jugador1,Jugador2),
                                                                write('............................\n'),turno(jugador1,Monton,Mesa,Jugador1,Jugador2,Doble).
                                                                
calcularPuntos([],0).
calcularPuntos([ficha(X,Y)|R],Puntos):-calcularPuntos(R,PuntosAux),Puntos is PuntosAux+X+Y.

transformar(Mesa,MesaTrans):-findall(fich(X,Y),member(ficha(X,Y),Mesa),MesaTrans).

escribir(Mesa,Monton,Jugador1,Jugador2):-write('Estado de la mesa: '),write(Mesa),nl,write('Estado del montón: '),write(Monton),nl,
                                         write('Fichas del jugador 1: '),write(Jugador1),nl,write('Fichas del jugador 2: '),write(Jugador2),nl,nl.


















