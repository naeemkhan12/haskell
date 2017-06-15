:- dynamic queue/1.
queue([adam,eve,tracy,pete,john,james,ruth,david]).


findLength([],0).
findLength([_|T],N):-
     findLength(T,M),N is M+1.

queue_length(K):-
   queue(X), findLength(X,K).
remove_head:-
     queue([_|T]),retractall(queue(_)),assert(queue(T)).

findLast([Last|[]],Last).
findLast([_|Tail],Last):-
    findLast(Tail,Last).
remove_last:-
     queue(L),findLast(L,Last),delete(L,Last,M),retractall(queue(_)),assert(queue(M)).

joinf(K,M):-
     queue(X),append([K],X,M).
joinl(K,M):-
     queue(X),append(X,[K],M).

join_front(K):-
     queue(X),joinf(K,M),retractall(queue(_)),assert(queue(M)).
join_as_last(K):-
     queue(X),joinl(K,M),retractall(queue(_)),assert(queue(M)).
sendback(M):-
     queue([H|T]),append(T,[H],M).
send_to_back:-
     queue(X),sendback(M),retractall(queue(_)),assert(queue(M)).

next_pervious(X, Y, [X,Y|_]).
next_pervious(X, Y, [_|T]) :-
     next_pervious(X, Y, T).
behind(K,M):-
     queue(L),next_pervious(K,M,L).

jump_to_front(K):-
     queue(L),delete(L,K,M),retractall(queue(_)),assert(queue(M)),join_front(K).

removeSecond([],[]).
removeSecond([H|[]],[H]).
removeSecond([H1,H2|T], [H1|M]):-
	removeSecond(T,M).

every_second_out:-
     queue(L),removeSecond(L,M),retractall(queue(_)),assert(queue(M)).




