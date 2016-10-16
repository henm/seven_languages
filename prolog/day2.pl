% Day 2

% Reverse elements of a list---------------------------------------------------

% First attempt. Works, but if you want to see all results it doesn't stop...
custom_reverse_first([], []).
custom_reverse_first([Head|Tail], List) :-
	append(X, [Head], List),
	custom_reverse_first(Tail, X).

% Second attempt using an accumulator
custom_reverse_second(List, Res) :-
	custom_reverse_accumulator(List, [], Res).

custom_reverse_accumulator([], Acc, Acc).
custom_reverse_accumulator([Head|Tail], Acc, Res) :-
	custom_reverse_accumulator(Tail, [Head|Acc], Res).


% Find smallest element of a list----------------------------------------------

smallest([X], X).
smallest([Head|Tail], Head) :- smallest(Tail, Temp), Head @=< Temp.
smallest([Head|Tail], Res) :- smallest(Tail, Res), Head @> Res.


% Sort elements of a list------------------------------------------------------

custom_sort([], []).
custom_sort(List, Res) :- custom_sort_accumulator(List, [], Res).

% Use an accumulator and add elements one by one
custom_sort_accumulator([], Res, Res).
custom_sort_accumulator([X|Xs], Acc, Res) :-
	add_element(X, Acc, Temp),
	custom_sort_accumulator(Xs, Temp, Res).

% Add an element to an ordered list
add_element(X, [], [X]).
add_element(X, [Y|Ys], Res) :-
	X @< Y, append([X], [Y|Ys], Res).
add_element(X, [Y|Ys], Res) :-
	Y @=< X, append([Y], Temp, Res),
	add_element(X, Ys, Temp).
