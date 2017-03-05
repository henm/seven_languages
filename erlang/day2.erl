%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Day 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(day2).
-export([get_value_keyfind/2, get_value/2, compute_total/1, result/1]).

% Consider a list of keyword-value tuples, such as [{erlang, "a functional
% language"}, {ruby, "an OO language"}]. Write a function that accepts the list
% and a keyword and returns the associated value for the keyword.

% Using lists:keyfind
get_value_keyfind(L, Keyword) -> {_, Text} = lists:keyfind(Keyword, 1, L), Text.

% Own implementation
% Only return the first match
get_value([{Keyword, Text}|_], Keyword) -> Text;
get_value([_|T], Keyword) -> get_value(T, Keyword);
get_value([], _) -> false.


% Consider a shopping list that looks like [{item, quantity, price}, ...].
% Write a list comprehension that builds a list of items of the form
% [{item, total_price}, ...], where total_price is quantity times price.

compute_total(L) -> [{Item, Quantity * Price} || {Item, Quantity, Price} <- L].

% Write a program that reads a tic-tac-toe board presented as a list or a tuple
% of size nine. Return the winner (x or o) if a winner has been determined, cat
% if there are no more possible moves, or no_winner if no player has won yet.

% Determine if there is already a winner
winner({X, X, X, _, _, _, _, _, _}) -> X;
winner({_, _, _, X, X, X, _, _, _}) -> X;
winner({_, _, _, _, _, _, X, X, X}) -> X;
winner({X, _, _, X, _, _, X, _, _}) -> X;
winner({_, X, _, _, X, _, _, X, _}) -> X;
winner({_, _, X, _, _, X, _, _, X}) -> X;
winner({X, _, _, _, X, _, _, _, X}) -> X;
winner({_, _, X, _, X, _, X, _, _}) -> X;
winner(_) -> false.

% Determine if a field is already set
is_defined(x) -> true;
is_defined(o) -> true;
is_defined(_) -> false.

% Determine if there are still possible moves
move_possible(Board) ->
	Board_list = tuple_to_list(Board),
	lists:any(fun(Elem) -> not is_defined(Elem) end, Board_list).

% Couldn't find a better name...
result(Board) -> 
	case {winner(Board), move_possible(Board)} of
		{x, _} -> x;
		{o, _} -> o;
		{_, false} -> cat;
		{_, _} -> no_winner
	end.
