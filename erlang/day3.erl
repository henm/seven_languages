%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Day 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(day3).
-export([watch_translate/0, watch_doctor/0]).

% Monitor the translate_service and restart it should it die.
watch_translate() ->
	process_flag(trap_exit, true),
	receive
		new ->
			io:format("Creating and monitoring process.~n"),
			register(translate_service, spawn_link(fun translate_service:loop/0)),
			watch_translate();
		{'EXIT', From, Reason} ->
			io:format("Translate_service ~p died with reason ~p.", [From, Reason]),
			io:format("Restarting.~n"),
			self() ! new,
			watch_translate()
	end.

% It's called with:
% Translator = spawn(fun day3:watch_translate/0).
% Translator ! new.
% translate_service:translate(translate_service, "casa").

% Make the Doctor process restart itself if it should die.
watch_doctor() ->
	process_flag(trap_exit, true),
	receive
		new ->
			io:format("Creating and monitoring process.~n"),
			register(doctor, spawn_link(fun doctor:loop/0)),
			doctor ! new,
			watch_doctor();
		{'EXIT', From, Reason} ->
			io:format("Doctor ~p died with reason ~p.", [From, Reason]),
			io:format("Restarting.~n"),
			self() ! new,
			watch_doctor()
	end.

% Usage:
% Doctor_Doctor = spawn(fun day3:watch_doctor/0).
% Doctor_Doctor ! new.
% revolver ! 1.
% doctor.erl and revolver.erl from the book must be compiled an in the same folder!
