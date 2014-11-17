/*  Foderkalkylaton

   Author:        Madeleine Malmsten
   E-mail:        madde@easyrider.nu
   WWW:           http://www.easyrider.nu
   Copyright (C): 2014, Easyrider



*/

:- module(chat_server,
	  [ server/0,
	    server/1,			% ?Port
	    create_chat_room/0
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(debug)).
:- use_module(library(http/http_files)).

:- use_module(hub).
:- use_module(library(threadutil)).

:- use_module(library(error)).

%%	server is det.
%%	server(?Port) is det.
%
%	Create the chat room and start the   server. The default port is
%	8000.

:- dynamic foder/12.


server :-
	server(80).

server(Port) :-
	(   debugging(chat),
	    current_prolog_flag(gui, true)
	->  prolog_ide(thread_monitor)
	;   true
	),
	create_chat_room,
	http_server(http_dispatch, [port(Port)]),
	%thread_signal(chatroom, (attach_console, trace)),
	init_foder("foder.pl"),
	format(user_error, 'Started server at http://localhost:~d/~n', [Port]).

% setup the HTTP location. The  first   (/)  loads  the application. The
% loaded application will create  a   websocket  using  /chat. Normally,
% http_upgrade_to_websocket/3 runs call(Goal, WebSocket)  and closes the
% connection if Goal terminates. Here, we use guarded(false) to tell the
% server we will take responsibility for the websocket.

%:- http_handler(root(.), serve_files, [prefix]).
:- http_handler(root(.), http_reply_from_files('assets', []), [prefix]).
:- http_handler(root(foder),
		http_upgrade_to_websocket(
		    accept_chat,
		    [ guarded(false),
		      subprotocols([chat])
		    ]),
		[ id(chat_websocket)
		]).


% this serves files from the directory assets
% under the working directory

serve_files(Request) :-
	 http_reply_from_files('assets', [], Request).
serve_files(Request) :-
	  http_404([], Request).



%%	accept_chat(+WebSocket) is det.
%
%	Normally,  the  goal  called    by   http_upgrade_to_websocket/3
%	processes all communication with the   websocket in a read/write
%	loop. In this case however,  we tell http_upgrade_to_websocket/3
%	that we will take responsibility for   the websocket and we hand
%	it to the chat room.

accept_chat(WebSocket) :-
	hub_add(chat, WebSocket, _Id).

%%	create_chat_room
%
%	Create our actual chat room.

:- dynamic
	utterance/3,			% messages
	visitor/1.			% joined visitors

create_chat_room :-
	hub_create(chat, Room, _{}),
	thread_create(chatroom(Room), _, [alias(chatroom)]).

%%	chatroom(+Room)
%
%	Realise the chatroom main loop: listen  for an event, update the
%	state and possibly broadcast status updates.

chatroom(Room) :-
	thread_get_message(Room.queues.event, Message),
	handle_message(Message, Room),
	chatroom(Room).


chathistory(Client,Name,Message,Date):-
		format(atom(Javascript), 'updatechat(~p,~p,~p);', [Name,Message,Date]),
		hub_send(Client, websocket{client:Client,data:Javascript,format:text,hub:chat,opcode:text}).



%****************************************************************************************
% handle_message(+Message, +Room)
% Change to ws_receive(WebSocket, Dict, [format(json)]). in hub.pl to receive json
%****************************************************************************************

handle_message(Message, Room) :- 
	websocket{client:Client,data:Data,format:string,hub:chat,opcode:text} :< Message, !,
	atom_string(Data1,Data),
	json:atom_json_dict(Data1, Json, []),
	handle_json_message(Json, Client, Room).
handle_message(Message, _Room) :-
	hub{joined:Id} :< Message, !,
	assertz(visitor(Id)),
	forall(utterance(Name,Utterance,Date),chathistory(Id,Name,Utterance,Date)).
handle_message(Message, _Room) :-
	hub{left:Id} :< Message, !,
	retractall(visitor(Id)).
handle_message(Message, _Room) :-
	debug(chat, 'Ignoring message ~p', [Message]).


%****************************************************************************************
% handle_json_message(+Data, +Client, +Room)
%****************************************************************************************

handle_json_message(_{pid:"foder",type:"make",values:[]}, _Client, _Room) :- % Web page foder.html opened
	debug(chat, 'Make recieved.', []).
handle_json_message(_{pid:"chat",type:"post",values:["","",_]}, _Client, _Room) :- 
	debug(chat, 'Empty Post recieved', []).
handle_json_message(_{pid:"chat",type:"post",values:["admin","update",Date]}, Client, Room) :- 
	consult("chat.pl"),
	format(atom(Javascript), 'updatechat(~p,~p,~p);', ["","uppdatering klar",Date]),
	hub_broadcast(Room.name, websocket{client:Client,data:Javascript,format:string,hub:chat,opcode:text}),
	debug(chat, 'Reload chat.pl', []).
handle_json_message(_{pid:"chat",type:"post",values:["admin","clear",_]}, Client, Room) :- 
	retractall(utterance(_,_,_)),
	format(atom(Clearchat), 'var element = document.getElementById("newchatbox");while (element.firstChild) element.removeChild(element.firstChild);',[]),
	hub_broadcast(Room.name, websocket{client:Client,data:Clearchat,format:string,hub:chat,opcode:text}),
	debug(chat, 'Clear All Post recieved', []).
handle_json_message(_{pid:"chat",type:"post",values:["admin",Message,_]}, _Client, _Room) :- 
	retractall(utterance(_,Message,_)),
	debug(chat, 'Clear A certain Post recieved, ~p', [Message]).
handle_json_message(_{pid:"chat",type:"post",values:[Name,Message,Date]}, Client, Room) :- 
	format(atom(Javascript), 'updatechat(~p,~p,~p);', [Name,Message,Date]),
	assertz(utterance(Name,Message,Date)),
	hub_broadcast(Room.name, websocket{client:Client,data:Javascript,format:string,hub:chat,opcode:text}),
	debug(chat, 'Post recieved ~p', [Message]).
%% handle_json_message(_{pid:"manufacturer",type:"put",values:[Ma]}, _Client, _Room) :- 
%% 	init_selected_foder("foder.pl",Ma),
%% 	debug(chat, 'Changed manufacturer ~p', [Ma]).
handle_json_message(_{pid:"foder",type:"get",values:[Brand, Hayekg, Hayprkg, Haycakg, Haypkg, Finalenergy, Finalprotein, 
		Finalca, Finalp, Hayneed, Hayneedtwo]}, Client, _Room) :- 
	format(atom(Mytext), 'document.getElementById("loading").style.display = "block";', []),
	hub_send(Client, websocket{client:Client,data:Mytext,format:text,hub:chat,opcode:text}),
	init_selected_foder("foder.pl",Brand),
	suggest(Client, Hayekg, Hayprkg, Haycakg, Haypkg, Finalenergy, Finalprotein, Finalca, Finalp, Hayneed, Hayneedtwo),
	format(atom(Mytext1), 'document.getElementById("hayneedis").value = document.getElementById("hayamount").value;', []),
	hub_send(Client, websocket{client:Client,data:Mytext1,format:text,hub:chat,opcode:text}),
	init_selected_foder("foder.pl","").
handle_json_message(V, _Client, _Room) :-
	debug(chat, 'Other recieved: ~p', [V]).


%************************************************************************************************************************
% suggest(+Client, +Hayekg, +Hayprkg, +Haycakg, +Haypkg, +Finalenergy, +Finalprotein, +Finalca, +Finalp, +Hayneed, +Hayneedtwo
% Föreslå rätt mängd hö så att energi och fosfor som kommer från höet är mindre än hästens totala behov. 
% 
% Hayekg är energiinnehållet i 1 kg hö
% Hayprkg är proteininnehållet i 1 kg hö
% Haycakg är calciuminnehållet i 1 kg hö
% Haypkg är fosforinnehållet i 1 kg hö
% Finalenergy är hästens totala behov av energi
% Finalprotein är hästens totala behov av protein
% Finalca är hästens totala behov av calcium
% Finalp är hästens totala behov av fosfor
% Hayneed är hästens lägsta behov av grovfoder
% Hayneedtwo är hästens rekommenderat lägsta behov av grovfoder
%*************************************************************************************************************************


suggest(Client, Hayekg, Hayprkg, Haycakg, Haypkg, Finalenergy, Finalprotein, Finalca, Finalp, _Hayneed, Hayneedtwo):-
	numlist(1, 40, L),
	member(X,L),
	X1 is X * 0.5,
	Hayekg * X1 >= Finalenergy - 2,		
	Hayekg * X1 =< Finalenergy + 2,		
	Hayprkg * X1 >= Finalprotein,
	Haycakg * X1 >= Finalca - 1,
	Haypkg * X1 >= Finalp - 0.6,
	Haypkg * X1 =< Finalp + 0.6,
	X1 >= Hayneedtwo, !,
	update_gui_suggest(Client, X1, Hayekg, Hayprkg, Haycakg, Haypkg).

suggest(Client, Hayekg, Hayprkg, Haycakg, Haypkg, Finalenergy, Finalprotein, Finalca, Finalp, _Hayneed, Hayneedtwo):-
	numlist(1, 40, L),
	member(X,L),
	X1 is X * 0.5,
	Hayekg * X1 =< Finalenergy,		
	Hayprkg * X1 =< Finalprotein,
	Haycakg * X1 =< Finalca,
	Haypkg * X1 =< Finalp,
	X1 >= Hayneedtwo, !,
	update_gui_suggest(Client, X1, Hayekg, Hayprkg, Haycakg, Haypkg),
	esuggest(Client, Hayekg * X1, Hayprkg * X1, Haycakg * X1, Haypkg * X1,
		Finalenergy - (Hayekg * X1), Finalprotein - (Hayprkg * X1), Finalca - (Haycakg * X1), Finalp - (Haypkg * X1)).


suggest(Client, Hayekg, Hayprkg, Haycakg, Haypkg, Finalenergy, Finalprotein, Finalca, Finalp, Hayneed, _Hayneedtwo):-
	numlist(1, 40, L),
	member(X,L),
	X1 is X * 0.5,
	Hayekg * X1 =< Finalenergy,		
	Hayprkg * X1 =< Finalprotein,
	Haycakg * X1 =< Finalca,
	Haypkg * X1 =< Finalp,
	X1 >= Hayneed, !,
	update_gui_suggest(Client, X1, Hayekg, Hayprkg, Haycakg, Haypkg),
	esuggest(Client, Hayekg * X1, Hayprkg * X1, Haycakg * X1, Haypkg * X1, 
		Finalenergy - (Hayekg * X1), Finalprotein - (Hayprkg * X1), Finalca - (Haycakg * X1), Finalp - (Haypkg * X1)).
	
% Energin som kommer från höet ska vara mindre än totala behovet och större än hästens totala behov minus 7
suggest(Client, Hayekg, Hayprkg, Haycakg, Haypkg, Finalenergy, Finalprotein, Finalca, Finalp, _Hayneed, Hayneedtwo):-
	numlist(1, 40, L),
	member(X,L),
	X1 is X * 0.5,
	Hayekg * X1 =< Finalenergy,		
	Hayekg * X1 >= Finalenergy - 7,
	Haypkg * X1 =< Finalp,
	X1 >= Hayneedtwo, !,
    update_gui_suggest(Client, X1, Hayekg, Hayprkg, Haycakg, Haypkg),	
	esuggest(Client, Hayekg * X1, Hayprkg * X1, Haycakg * X1, Haypkg * X1, 
		Finalenergy - (Hayekg * X1), Finalprotein - (Hayprkg * X1), Finalca - (Haycakg * X1), Finalp - (Haypkg * X1)).

% Energin som kommer från höet ska vara mindre än totala behovet och större än hästens totala behov minus 7
suggest(Client, Hayekg, Hayprkg, Haycakg, Haypkg, Finalenergy, Finalprotein, Finalca, Finalp, Hayneed, _Hayneedtwo):-
	numlist(1, 40, L),
	member(X,L),
	X1 is X * 0.5,
	Hayekg * X1 =< Finalenergy,		
	Hayekg * X1 >= Finalenergy - 7,	
	Haypkg * X1 =< Finalp,
	X1 >= Hayneed, !,
    update_gui_suggest(Client, X1, Hayekg, Hayprkg, Haycakg, Haypkg),
	esuggest(Client, Hayekg * X1, Hayprkg * X1, Haycakg * X1, Haypkg * X1, 
		Finalenergy - (Hayekg * X1), Finalprotein - (Hayprkg * X1), Finalca - (Haycakg * X1), Finalp - (Haypkg * X1)).
	
suggest(_Client, _Hayekg, _Hayprkg, _Haycakg, _Haypkg, _Finalenergy, _Finalprotein, _Finalca, _Finalp, _Hayneed, _Hayneedtwo):-
	debug(chat, 'Fail suggest, should not happen~n', []).


	
%****************************************************************************************
% esuggest(+Client, +FE, +FP, +FC, +FF, +LE, +LP, +LC, +LF)
% Föreslå ett kraftfoder som
%
% FE är mängden energi från hö
% FP är mängden protein från hö
% FC är mängden Ca från hö
% FF är mängden P från hö
% LE är ännu ej tillgodosedd mängd energi
% LP är ännu ej tillgodosedd mängd protein
% LC är ännu ej tillgodosedd mängd Ca
% LF är ännu ej tillgodosedd mängd P
%****************************************************************************************

esuggest(Client, FE, FP, FC, FF, LE, LP, LC, LF):-  % Finns det ett helt perfekt foder?
	debug(chat, 'Finns det ett helt perfekt foder?', []),
	numlist(1, 30, L),
	numlist(0, 30, L1),
	member(X,L),
	member(X2,L1),
	foder(Nu,Na,En,Pr,Ca,Ph,_Ma,_M1,_M2,_Ty,Te,_Sh),
	X1 is X * 0.1,
	X3 is X2 * 0.1,	% +/- X3
%	debug(chat, 'pNa ~p ~p ~p', [Na,X1,X3]),
	En * X1 >= LE - X3,
	En * X1 =< LE + X3,
	Pr * X1 >= LP - 50,
	Pr * X1 =< LP + 50,
	Ca * X1 >= LC,
	Ca * X1 =< LC * 1.5,
	Ph * X1 >= LF - 0.5,
	Ph * X1 =< LF + 0.75, !,
	update_gui_esuggest1(Client,first,Na,Te,Nu,X1,En,Pr,Ca,Ph),	
	update_gui_esuggest2(Client,X1,En,Pr,Ca,Ph,FE,FP,FC,FF),
	format(atom(Mytext), 'extrastraw();', []),
	hub_send(Client, websocket{client:Client,data:Mytext,format:text,hub:chat,opcode:text}),	
	format(atom(Mytext1), 'document.getElementById("loading").style.display = "none";', []),
	hub_send(Client, websocket{client:Client,data:Mytext1,format:text,hub:chat,opcode:text}).


esuggest(Client, FE, FP, FC, FF, LE, LP, LC, LF):-  % Finns det ett nästan perfekt foder?
	debug(chat, 'Finns det ett nästan perfekt foder?', []),
	numlist(1, 30, L),
	numlist(0, 50, L1),
	member(X,L),
	member(X2,L1),
	foder(Nu,Na,En,Pr,Ca,Ph,_Ma,_M1,_M2,_Ty,Te,_Sh),
	X1 is X * 0.1,
	X3 is X2 * 0.3,	
%	debug(chat, 'npNa ~p ~p ~p', [Na,X1,X3]),
	En * X1 >= LE - 5,
	En * X1 =< LE + 5,
	Pr * X1 >= LP,
	Pr * X1 =< LP + X2,
	Ca * X1 >= LC,
	Ca * X1 =< LC + X3,
	Ph * X1 >= LF - 0.75,
	Ph * X1 =< LF + 0.75, !,
%	debug(chat, 'npNa1 ~p ~p ~p', [Na,X1,X3]),
	update_gui_esuggest1(Client,first,Na,Te,Nu,X1,En,Pr,Ca,Ph),	
	update_gui_esuggest2(Client,X1,En,Pr,Ca,Ph,FE,FP,FC,FF),
	format(atom(Mytext), 'extrastraw();', []),
	hub_send(Client, websocket{client:Client,data:Mytext,format:text,hub:chat,opcode:text}),	
	format(atom(Mytext1), 'document.getElementById("loading").style.display = "none";', []),
	hub_send(Client, websocket{client:Client,data:Mytext1,format:text,hub:chat,opcode:text}).



% Om en nästan perfekt matchning inte finns, föreslå två kraftfoder som tillsammans uppfyller kraven
esuggest(Client, FE, FP, FC, FF, LE, LP, LC, LF):- 
%gtrace,
	debug(chat, 'Om en perfekt matchning inte finns1', []),
	numlist(1, 10, L), % Testar mängden för fodersort 1
	numlist(1, 10, L0), % Testar mängden för fodersort 2
	numlist(0, 50, L1),
	member(X,L),
	member(X2,L1),
	foder(Nu,Na,En,Pr,Ca,Ph,_Ma,_M1,_M2,_Ty,Te,_Sh),
	member(Y,L0),
	foder(Nu1,Na1,En1,Pr1,Ca1,Ph1,_Ma1,_M11,_M21,_Ty1,Te1,_Sh1),
	X1 is X * 0.1,
	Y1 is Y * 0.1,
	X3 is X2 * 0.5,	
%	debug(chat, 'Energy ~p', [LE]),
%	debug(chat, 'Energyfromfood ~p', [(En * Y1) + (En1 * X1)]),
	(En * Y1) + (En1 * X1) >= (LE - 2),
	(En * Y1) + (En1 * X1) =< (LE + 2),
	(Pr * Y1) + (Pr1 * X1) >= LP,
	(Pr * Y1) + (Pr1 * X1) =< (LP + X2),
	(Ca * Y1) + (Ca1 * X1) >= LC,
	(Ca * Y1) + (Ca1 * X1) =< (LC + X3),
	(Ph * Y1) + (Ph1 * X1) >= (LF - 0.6),
	(Ph * Y1) + (Ph1 * X1) =< (LF + 0.6), !,
	update_gui_esuggest1(Client,first,Na,Te,Nu,X1,En,Pr,Ca,Ph),
	update_gui_esuggest1(Client,second,Na1,Te1,Nu1,X1,En1,Pr1,Ca1,Ph1),	
	update_gui_esuggest2(Client,X1,En + En1,Pr + Pr1,Ca + Ca1,Ph + Ph1,FE,FP,FC,FF),
	format(atom(Mytext), 'extrastraw();', []),
	hub_send(Client, websocket{client:Client,data:Mytext,format:text,hub:chat,opcode:text}),	
	format(atom(Mytext1), 'document.getElementById("loading").style.display = "none";', []),
	hub_send(Client, websocket{client:Client,data:Mytext1,format:text,hub:chat,opcode:text}).


% Näst sista alternativet
esuggest(Client, FE, FP, FC, FF, LE, LP, LC, LF):-
	debug(chat, 'Näst sista alternativet', []),
	numlist(1, 30, L),
	numlist(0, 15, L1),
	member(X,L),
	member(X2,L1),
	foder(Nu,Na,En,Pr,Ca,Ph,_Ma,_M1,_M2,_Ty,Te,_Sh),
	X1 is X * 0.1,
	X3 is X2 * 25,	
%	debug(chat, 'npNa ~p ~p ~p', [Na,X1,X3]),
	En * X1 >= (LE - 5),
	En * X1 =< (LE + 5),
	Pr * X1 >= LP,
	Pr * X1 =< (LP + X3),
	Ca * X1 >= LC,
	Ca * X1 =< (LC + X2),
	Ph * X1 >= (LF - 1),
	Ph * X1 =< (LF + 1), !,
%	debug(chat, 'npNa1 ~p ~p ~p', [Na,X1,X3]),
	update_gui_esuggest1(Client,first,Na,Te,Nu,X1,En,Pr,Ca,Ph),	
	update_gui_esuggest2(Client,X1,En,Pr,Ca,Ph,FE,FP,FC,FF),
	format(atom(Mytext), 'extrastraw();', []),
	hub_send(Client, websocket{client:Client,data:Mytext,format:text,hub:chat,opcode:text}),	
	format(atom(Mytext1), 'document.getElementById("loading").style.display = "none";', []),
	hub_send(Client, websocket{client:Client,data:Mytext1,format:text,hub:chat,opcode:text}).


% Absolut sista alternativet
esuggest(Client, FE, FP, FC, FF, LE, LP, LC, LF):- 
%gtrace,
	debug(chat, 'Absolut sista alternativet', []),
	numlist(1, 10, L), % Testar mängden för fodersort 1
	numlist(1, 10, L0), % Testar mängden för fodersort 2
	numlist(0, 15, L1),
	member(X,L),
	member(X2,L1),
	foder(Nu,Na,En,Pr,Ca,Ph,_Ma,_M1,_M2,_Ty,Te,_Sh),
	member(Y,L0),
	foder(Nu1,Na1,En1,Pr1,Ca1,Ph1,_Ma1,_M11,_M21,_Ty1,Te1,_Sh1),
	X1 is X * 0.1,
	Y1 is Y * 0.1,
	X3 is X2 * 25,	
%	debug(chat, 'Energy ~p', [LE]),
%	debug(chat, 'Energyfromfood ~p', [(En * Y1) + (En1 * X1)]),
	(En * Y1) + (En1 * X1) >= (LE - 5),
	(En * Y1) + (En1 * X1) =< (LE + 5),
	(Pr * Y1) + (Pr1 * X1) >= LP,
	(Pr * Y1) + (Pr1 * X1) =< (LP + X3),
	(Ca * Y1) + (Ca1 * X1) >= LC,
	(Ca * Y1) + (Ca1 * X1) =< (LC + X2),
	(Ph * Y1) + (Ph1 * X1) >= (LF - 1),
	(Ph * Y1) + (Ph1 * X1) =< (LF + 1), !,
	update_gui_esuggest1(Client,first,Na,Te,Nu,X1,En,Pr,Ca,Ph),
	update_gui_esuggest1(Client,second,Na1,Te1,Nu1,X1,En1,Pr1,Ca1,Ph1),	
	update_gui_esuggest2(Client,X1,En + En1,Pr + Pr1,Ca + Ca1,Ph + Ph1,FE,FP,FC,FF),
	format(atom(Mytext), 'extrastraw();', []),
	hub_send(Client, websocket{client:Client,data:Mytext,format:text,hub:chat,opcode:text}),	
	format(atom(Mytext1), 'document.getElementById("loading").style.display = "none";', []),
	hub_send(Client, websocket{client:Client,data:Mytext1,format:text,hub:chat,opcode:text}).

				
esuggest(Client, _FE, _FP, _FC, _FF, _LE, _LP, _LC, _LF):-
	debug(chat, 'Fail esuggest, it should not happen~n', []),
	format(atom(Mytext), 'extrastraw();', []),
	hub_send(Client, websocket{client:Client,data:Mytext,format:text,hub:chat,opcode:text}),	
	format(atom(Mytext1), 'document.getElementById("loading").style.display = "none";', []),
	hub_send(Client, websocket{client:Client,data:Mytext1,format:text,hub:chat,opcode:text}).


%****************************************************************************************
% update_gui_suggest(+Client, +X1, +Hayekg, +Hayprkg, +Haycakg, +Haypkg)
%
% X1 är mängden hö
% Hayekg * X1 är mängden energi från hö
% Hayprkg * X1 är mängden protein från hö
% Haycakg * X1 är mängden Ca från hö 
% Haypkg * X1 är mängden Ca från hö 
%****************************************************************************************

update_gui_suggest(Client, X1, Hayekg, Hayprkg, Haycakg, Haypkg) :-
    format(atom(Mytext),'document.getElementById("hayamount").value = ~1f', [X1]),
	hub_send(Client, websocket{client:Client,data:Mytext,format:text,hub:chat,opcode:text}),
    format(atom(Mytext1),'document.getElementById("hayenergy").value = ~1f', [Hayekg * X1]),
	hub_send(Client, websocket{client:Client,data:Mytext1,format:text,hub:chat,opcode:text}),
    format(atom(Mytext2),'document.getElementById("hayprotein").value = ~1f', [Hayprkg * X1]),
	hub_send(Client, websocket{client:Client,data:Mytext2,format:text,hub:chat,opcode:text}),
    format(atom(Mytext3),'document.getElementById("hayca").value = ~1f', [Haycakg * X1]),
	hub_send(Client, websocket{client:Client,data:Mytext3,format:text,hub:chat,opcode:text}),
    format(atom(Mytext4),'document.getElementById("hayp").value = ~1f', [Haypkg * X1]),
	hub_send(Client, websocket{client:Client,data:Mytext4,format:text,hub:chat,opcode:text}).


%****************************************************************************************
% update_gui_esuggest1(+Client,+first,+Na,+Te,+Nu,+X1,+En,+Pr,+Ca,+Ph)
%
% first eller second anger första eller andra kraftfoderförslaget
% Na är kraftfoder namn
% Te är kraftfoder-info
% Nu är kraftfoder nummer
% X1 är mängden kraftfoder som krävs
% En * X1 är mängden energi från kraftfoder
% Pr * X1 är mängden protein från kraftfoder
% Ca * X1 är mängden Ca från kraftfoder
% Ph * X1 är mängden P från kraftfoder
%****************************************************************************************

update_gui_esuggest1(Client,first,Na,Te,Nu,X1,En,Pr,Ca,Ph) :- !,
%gtrace,
	update_gui_clear(Client),
	format(atom(Mytext), 'document.getElementById("extrafoodname").innerHTML = "~s"', [Na]),
	hub_send(Client, websocket{client:Client,data:Mytext,format:text,hub:chat,opcode:text}),
	format(atom(Mytextinfo), 'document.getElementById("extrafoodinfo").innerHTML = "~s"', [Te]),
	hub_send(Client, websocket{client:Client,data:Mytextinfo,format:text,hub:chat,opcode:text}),
	format(atom(Myimg), 'var myimg = document.createElement("img"); myimg.src = "foodimg/~p.jpg"; 
		myimg.setAttribute("width", "100%"); document.getElementById("image").appendChild(myimg);', [Nu]),
	hub_send(Client, websocket{client:Client,data:Myimg,format:text,hub:chat,opcode:text}),
	format(atom(Mytext1), 'document.getElementById("extrafoodamount").value = ~1f', [X1]),
	hub_send(Client, websocket{client:Client,data:Mytext1,format:text,hub:chat,opcode:text}),
	format(atom(Mytext2), 'document.getElementById("extrafoodenergy").value = ~1f', [En * X1]),
	hub_send(Client, websocket{client:Client,data:Mytext2,format:text,hub:chat,opcode:text}),
	format(atom(Mytext3), 'document.getElementById("extrafoodprotein").value = ~1f', [Pr * X1]),
	hub_send(Client, websocket{client:Client,data:Mytext3,format:text,hub:chat,opcode:text}),
	format(atom(Mytext4), 'document.getElementById("extrafoodca").value = ~1f', [Ca * X1]),
	hub_send(Client, websocket{client:Client,data:Mytext4,format:text,hub:chat,opcode:text}),
	format(atom(Mytext5), 'document.getElementById("extrafoodp").value = ~1f', [Ph * X1]),
	hub_send(Client, websocket{client:Client,data:Mytext5,format:text,hub:chat,opcode:text}).

update_gui_esuggest1(Client,second,Na,Te,Nu,X1,En,Pr,Ca,Ph) :-
	format(atom(Mytext), 'document.getElementById("extrafoodnamea").innerHTML = "~s"', [Na]),
	hub_send(Client, websocket{client:Client,data:Mytext,format:text,hub:chat,opcode:text}),
	format(atom(Mytextinfo), 'document.getElementById("extrafoodinfoa").innerHTML = "~s"', [Te]),
	hub_send(Client, websocket{client:Client,data:Mytextinfo,format:text,hub:chat,opcode:text}),
	format(atom(Myimg), 'var myimg = document.createElement("img"); myimg.src = "foodimg/~p.jpg"; 
		myimg.setAttribute("width", "100%"); document.getElementById("imagea").appendChild(myimg);', [Nu]),
	hub_send(Client, websocket{client:Client,data:Myimg,format:text,hub:chat,opcode:text}),
	format(atom(Mytext1), 'document.getElementById("extrafoodamounta").value = ~1f', [X1]),
	hub_send(Client, websocket{client:Client,data:Mytext1,format:text,hub:chat,opcode:text}),
	format(atom(Mytext2), 'document.getElementById("extrafoodenergya").value = ~1f', [En * X1]),
	hub_send(Client, websocket{client:Client,data:Mytext2,format:text,hub:chat,opcode:text}),
	format(atom(Mytext3), 'document.getElementById("extrafoodproteina").value = ~1f', [Pr * X1]),
	hub_send(Client, websocket{client:Client,data:Mytext3,format:text,hub:chat,opcode:text}),
	format(atom(Mytext4), 'document.getElementById("extrafoodcaa").value = ~1f', [Ca * X1]),
	hub_send(Client, websocket{client:Client,data:Mytext4,format:text,hub:chat,opcode:text}),
	format(atom(Mytext5), 'document.getElementById("extrafoodpa").value = ~1f', [Ph * X1]),
	hub_send(Client, websocket{client:Client,data:Mytext5,format:text,hub:chat,opcode:text}).

update_gui_clear(Client):-
	format(atom(Del), 'var element = document.getElementById("image");while (element.firstChild) element.removeChild(element.firstChild);', []),
	hub_send(Client, websocket{client:Client,data:Del,format:text,hub:chat,opcode:text}),
	format(atom(Del1), 'var element = document.getElementById("imagea");while (element.firstChild) element.removeChild(element.firstChild);', []),
	hub_send(Client, websocket{client:Client,data:Del1,format:text,hub:chat,opcode:text}),
	format(atom(Mytext), 'document.getElementById("extrafoodnamea").innerHTML = ""', []),
	hub_send(Client, websocket{client:Client,data:Mytext,format:text,hub:chat,opcode:text}),
	format(atom(Mytextinfo), 'document.getElementById("extrafoodinfoa").innerHTML = ""', []),
	hub_send(Client, websocket{client:Client,data:Mytextinfo,format:text,hub:chat,opcode:text}),
	format(atom(Mytext1), 'document.getElementById("extrafoodamounta").value = ~1f', [0]),
	hub_send(Client, websocket{client:Client,data:Mytext1,format:text,hub:chat,opcode:text}),
	format(atom(Mytext2), 'document.getElementById("extrafoodenergya").value = ~1f', [0]),
	hub_send(Client, websocket{client:Client,data:Mytext2,format:text,hub:chat,opcode:text}),
	format(atom(Mytext3), 'document.getElementById("extrafoodproteina").value = ~1f', [0]),
	hub_send(Client, websocket{client:Client,data:Mytext3,format:text,hub:chat,opcode:text}),
	format(atom(Mytext4), 'document.getElementById("extrafoodcaa").value = ~1f', [0]),
	hub_send(Client, websocket{client:Client,data:Mytext4,format:text,hub:chat,opcode:text}),
	format(atom(Mytext5), 'document.getElementById("extrafoodpa").value = ~1f', [0]),
	hub_send(Client, websocket{client:Client,data:Mytext5,format:text,hub:chat,opcode:text}).



%****************************************************************************************
% update_gui_esuggest2(+Client,+X1,+En,+Pr,+Ca,+Ph,+FE,+FP,+FC,+FF)
% Uppdatera totalen
%
% FE + (En * X1) är mängden energi från hö och kraftfoder e
% FP + (Pr * X1) är mängden protein från kraftfoder e
% FC + (Ca * X1) är mängden Ca från hö och kraftfoder e
% FF + (Ph * X1) är mängden P från hö och kraftfoder e
% (FP + (Pr * X1)) / (FE + (En * X1)) är
% (FC + (Ca * X1)) / (FF + (Ph * X1)) är
%****************************************************************************************

update_gui_esuggest2(Client,X1,En,Pr,Ca,Ph,FE,FP,FC,FF) :-
	format(atom(Mytext14), 'document.getElementById("totalenergy").value = ~1f', [FE + (En * X1)]),
	hub_send(Client, websocket{client:Client,data:Mytext14,format:text,hub:chat,opcode:text}),
	format(atom(Mytext15), 'document.getElementById("totalprotein").value = ~1f', [FP + (Pr * X1)]),
	hub_send(Client, websocket{client:Client,data:Mytext15,format:text,hub:chat,opcode:text}),	
	format(atom(Mytext16), 'document.getElementById("totalca").value = ~1f', [FC + (Ca * X1)]),
	hub_send(Client, websocket{client:Client,data:Mytext16,format:text,hub:chat,opcode:text}),	
	format(atom(Mytext17), 'document.getElementById("totalp").value = ~1f', [FF + (Ph * X1)]),
	hub_send(Client, websocket{client:Client,data:Mytext17,format:text,hub:chat,opcode:text}),	
	format(atom(Mytext6), 'document.getElementById("smbrpandmj").value = ~1f', [(FP + (Pr * X1)) / (FE + (En * X1))]),
	hub_send(Client, websocket{client:Client,data:Mytext6,format:text,hub:chat,opcode:text}),
	format(atom(Mytext7), 'document.getElementById("caandp").value = ~1f', [(FC + (Ca * X1)) / (FF + (Ph * X1))]),
	hub_send(Client, websocket{client:Client,data:Mytext7,format:text,hub:chat,opcode:text}).



%****************************************************************************************
% load_foder(+File)
% foder(Number,Name,12.0,80.0,7.0,4.0,Manufacturer,6.7,1.8,Type,Text,Shape).
%****************************************************************************************

init_foder(File) :-
        retractall(foder(_,_,_,_,_,_,_,_,_,_,_,_)),
        open(File, read, Stream),
        call_cleanup(load_foder(Stream),
                     close(Stream)).

load_foder(Stream) :-
        read(Stream, T0),
        load_foder(T0, Stream).

load_foder(end_of_file, _) :- !.
load_foder(foder(Nu,Na,En,Pr,Ca,Ph,Ma,M1,M2,Ty,Te,Sh), Stream) :- !,
        assertz(foder(Nu,Na,En,Pr,Ca,Ph,Ma,M1,M2,Ty,Te,Sh)),
        read(Stream, T2),
        load_foder(T2, Stream).
load_foder(Term, _Stream) :-
        type_error(foder, Term).

%****************************************************************************************
% load_selected_foder(+File,+Manufacturer)
% foder(Number,Name,12.0,80.0,7.0,4.0,Manufacturer,6.7,1.8,Type,Text,Shape).
%****************************************************************************************

init_selected_foder(File,Manufacturer) :-
        retractall(foder(_,_,_,_,_,_,_,_,_,_,_,_)),
        open(File, read, Stream),
        call_cleanup(load_selected_foder(Manufacturer,Stream),
                     close(Stream)).

load_selected_foder(Manufacturer,Stream) :-
        read(Stream, T0),!,
        load_selected_foder(Manufacturer,T0, Stream).

load_selected_foder(_, end_of_file, _) :- !.

/*load_selected_foder(Manufacturer,foder(Nu,Na,En,Pr,Ca,Ph,Ma,M1,M2,Ty,Te,Sh), Stream) :- !,
		Ma = Manufacturer -> 	debug(chat, 'asserta ~p', [Ma]), 
		asserta(foder(Nu,Na,En,Pr,Ca,Ph,Ma,M1,M2,Ty,Te,Sh)),
		read(Stream, T2),!,
        load_selected_foder(Manufacturer,T2, Stream) ;
        assertz(foder(Nu,Na,En,Pr,Ca,Ph,Ma,M1,M2,Ty,Te,Sh)),
        read(Stream, T2),!,
        load_selected_foder(Manufacturer,T2, Stream).
*/
load_selected_foder("",foder(Nu,Na,En,Pr,Ca,Ph,Ma,M1,M2,Ty,Te,Sh), Stream) :- !,
		asserta(foder(Nu,Na,En,Pr,Ca,Ph,Ma,M1,M2,Ty,Te,Sh)),
		read(Stream, T2),
        load_selected_foder("",T2, Stream).
load_selected_foder(Manufacturer,foder(Nu,Na,En,Pr,Ca,Ph,Manufacturer,M1,M2,Ty,Te,Sh), Stream) :- !,
		asserta(foder(Nu,Na,En,Pr,Ca,Ph,Manufacturer,M1,M2,Ty,Te,Sh)),
		read(Stream, T2),
        load_selected_foder(Manufacturer,T2, Stream).
load_selected_foder(Manufacturer,foder(_Nu,_Na,_En,_Pr,_Ca,_Ph,_Ma,_M1,_M2,_Ty,_Te,_Sh), Stream) :- !,
		read(Stream, T2),
        load_selected_foder(Manufacturer,T2, Stream).
load_selected_foder(_Manufacturer,Term, _Stream) :-
        type_error(foder, Term).

