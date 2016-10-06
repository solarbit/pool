% Copyright 2012-2016 solarbit.cc <steve@solarbit.cc>
% See MIT LICENSE

-module(user_default).

-export([api/1, new/1, make/0, make/1, print/1, makeapp/1]).


api(Module) when is_atom(Module) ->
	case code:ensure_loaded(Module) of
	{module, Module} ->
		Exports = lists:sort(Module:module_info(exports)),
		io:format("~p~n", [Exports]);
	Error ->
		Error
	end.


make() ->
	{ok, WorkingDir} = file:get_cwd(),
	case file:read_file_info("Emakefile") of
	{ok, _} ->
		io:format("Emake ~p~n", [WorkingDir]);
	{error, _} ->
		io:format("~p~n", [WorkingDir])
	end,
	make:all([load]).

make(App) when is_atom(App) ->
	{ok, WorkingDir} = file:get_cwd(),
	AppDir = code:lib_dir(App),
	file:set_cwd(AppDir),
	AppFile =
		case file:consult(AppDir ++ "/ebin/" ++ atom_to_list(App) ++ ".app") of
		{ok, [Term]} ->
			Term;
		{error, enoent} ->
			undefined
		end,
	case file:read_file_info("Emakefile") of
	{ok, _} ->
		io:format("Emake ~p~n", [AppDir]);
	{error, _} ->
		io:format("~p~n", [AppDir])
	end,
	io:format("~p~n", [AppFile]),
	R = make:all([load]),
	%io:format("~p~n", [file:list_dir(AppDir ++ "/ebin")]),
	file:set_cwd(WorkingDir),
	R.


print(X) ->
	io:format("~p~n", [X]).


new(Module) when is_atom(Module) ->
	Name = atom_to_binary(Module, utf8),
	Src = <<"\n-module(", Name/binary, ").\n\n-compile(export_all).\n\n">>,
	Bin = [copyright(), Src],
	{ok, CD} = file:get_cwd(),
	Path = list_to_binary(CD),
	File = binary_to_list(<<Path/binary, "/src/", Name/binary, ".erl">>),
	ok = file:write_file(File, Bin),
	{ok, File}.


makeapp(App) when is_atom(App) ->
	try begin
		Name = atom_to_list(App),
		{ok, CD} = file:get_cwd(),

		AppDir = filename:join(CD, Name),
		io:format("Creating directories...~n", []),
		ok = file:make_dir(AppDir),
		ok = file:make_dir(filename:join(AppDir, "ebin")),
		ok = file:make_dir(filename:join(AppDir, "include")),
		ok = file:make_dir(filename:join(AppDir, "priv")),
		ok = file:make_dir(filename:join(AppDir, "src")),

		User = case os:getenv("USERNAME") of
			Value when is_list(Value) -> Value;
			_ -> "?"
			end,
		{Year, _, _} = date(),
		Copyright = io_lib:format("Copyright (c) ~p " ++ User ++ ". All rights reserved.~n", [Year]),

		io:format("Creating readmes...~n", []),
		LicenseFile = filename:join([AppDir, "LICENSE"]),
		ok = file:write_file(LicenseFile, lists:append([
			"LICENSE\n\n", Copyright])),

		ok = file:write_file(filename:join(AppDir, "README"), "README\n"),

		io:format("Creating makefiles...~n", []),
		EmakeFile = filename:join([AppDir, "Emakefile"]),
		ok = file:write_file(EmakeFile, lists:append([
			"% -*- mode:erlang -*-\n"
			"{\"src/*\", [{i, \"include\"}, {outdir, \"ebin\"}, debug_info, strict_record_tests]}.\n"])),

		MakeFile = filename:join([AppDir, "Makefile"]),
		ok = file:write_file(MakeFile, lists:append([
			"compile:\n",
			"    erl -pz ./ebin -make\n\n",
			"clean:\n",
			"    rm -rf ./ebin/*.beam\n"])),

		io:format("Creating application.app...~n", []),
		AppFile = filename:join([AppDir, "ebin", Name ++ ".app"]),
		ok = file:write_file(AppFile, lists:append([
			"% -*- mode:erlang -*-\n",
			"{application, ", Name, ", [\n",
			"  {description, \"", Name, ".\"},\n",
			"  {vsn, \"0.1\"},\n",
			"%  {mod, {", Name, "_app, []}},\n",
			"  {env, [\n",
			"    %\n",
			"  ]},\n",
			"  {modules, [\n",
			"    ", Name, "\n",
			"  ]},\n",
			"  {applications, [kernel, stdlib]}\n",
			"]}.\n"])),


		ok
	end catch
	error:Reason -> Reason
	end.


copyright() ->
	{Year, _, _} = date(),
	Year0 = list_to_binary(integer_to_list(Year)),
<<"%% Copyright ", Year0/binary, " Steve Davis <steve@solarbit.cc>
%
% Licensed under the Apache License, Version 2.0 (the \"License\");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an \"AS IS\" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
">>.
