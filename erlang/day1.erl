%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Day1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(day1).
-export([count_letters/1]).
-export([count_to_ten/0]).
-export([print_cases/1]).

% Write a function that uses recursion to return the number of words in a
% string
count_letters([]) -> 0;
count_letters([_|Rest]) -> 1 + count_letters(Rest).

% Write a funtion that uses recursion to count to ten
count_to_ten() -> count_to_ten_helper(1).
count_to_ten_helper(10) -> io:format("10~n");
count_to_ten_helper(X) -> io:format("~p~n", [X]), count_to_ten_helper(X + 1).

% Write a function that uses matching to selectively print "success" or
% "error: message" given input of the form {error, Message} or success
print_cases(success) -> io:format("success~n");
print_cases({error, Message}) -> io:format("error: ~s~n", [Message]).
