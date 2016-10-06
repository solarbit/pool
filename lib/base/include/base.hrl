% Copyright 2016 solarbit.cc <steve@solarbit.cc>
% See MIT LICENSE

-ifndef(TTY).
-define(TTY(Term), io:format(user, "[~p:~p] ~p~n", [?MODULE, ?LINE, Term])).
-endif.

-ifndef(is_string).
% This is a semi-valid guard used to guess whether a list is a "string".
-define(is_string(S), (is_list(S) andalso S =/= [] andalso is_integer(hd(S)))).
-endif.

-ifndef(is_record).
% This is a semi-valid guard used to guess whether a tuple is a record when the runtime type isn't known.
-define(is_record(X), is_tuple(X) andalso is_atom(element(1, X)) andalso size(X) > 1).
-endif.
