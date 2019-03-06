#!/usr/bin/env escript

-module(dep).
-export([main/1]).
    

main(Args) ->
    {ok, Constraints} = readfile(lists:nth(3, Args)),
    {ok, Initial} = readfile(lists:nth(2, Args)),
    {ok, Repository} = readfile(lists:nth(1, Args)),
    Repo = jiffy:decode(Repository, [return_maps]),
    S = jiffy:decode(Initial, [return_maps]),
    % io:fwrite("State: ~p", [S]),
    Const = jiffy:decode(Constraints, [return_maps]),
    Consts = interpret_constraints(Const, Repo, []),
    % io:fwrite("Interpreted constraints: ~p", [Consts]),
    State = convert_state_to_json(S, Repo),
    % io:fwrite("Current JSON state: ~p", [State]),
    Graph = digraph:new(),
    case are_there_multiple_paths(Consts) of
        true ->
            Paths = get_all_combinations_of_nested_lists(Consts),
            {_, _, Instruction_list} = compare_paths(Paths, [], State, Repo, Graph, 0, [], none);
        false ->
            {_, _, Instruction_list} = satisfy_constraints(Consts, Repo, State, Graph)
    end,
    Output = lists:map(fun(E) -> binary_to_list(E) end, Instruction_list),
    io:fwrite("~p", [Output]).
    % io:fwrite("\n Result : ~p, \n Size: ~p", [Instruction_list, Size]).

satisfy_constraints(Constraints, Repo, State, Graph) ->
    Result = function_name(Constraints, State, Repo, Graph, 0, []),
    Result.

function_name([], State, _, Graph, Size, Instruction_list) ->
    digraph:delete(Graph),
    {State, Size, Instruction_list};
function_name([{<<"+">>, Package}|Packages_to_change], State, Repo, Graph, Size, Instruction_list) ->
    % io:fwrite("\n Instruction list: ~p, \n State: ~p, \n Current instruction: ~p", [Instruction_list, State, {<<"+">>, Package}]),
    case is_package_installed(State, Package) of
        true ->
            function_name(Packages_to_change, State, Repo, Graph, Size, Instruction_list);
        false ->
            case get_required_operations({<<"+">>, Package}, State, Repo) of    
                [] ->
                    % io:fwrite("\n No required operations: ~p", [Package]),
                    Package_name = maps:get(<<"name">>, Package),
                    Package_size = maps:get(<<"size">>, Package),
                    Package_version = maps:get(<<"version">>, Package),
                    Instruction = <<"+", Package_name/binary, "=", Package_version/binary>>,
                    Vertex = digraph:add_vertex(Graph, Instruction),
                    case Instruction_list of 
                        [] ->
                            function_name(
                                Packages_to_change, 
                                install(State, Package), 
                                Repo, 
                                Graph,
                                Size+Package_size, 
                                Instruction_list ++ [Instruction]
                            );
                        _ ->
                            Last_instruction = lists:last(Instruction_list),                       
                            brutally_add_edge_to_digraph(Graph, Last_instruction, Vertex),
                            % io:fwrite("Adding edge between: ~p, ~p", [Last_instruction, Vertex]),
                            % io:fwrite("\n Edges: ~p", [digraph:edges(Graph)]),
                            case digraph:get_cycle(Graph, Vertex) of
                                false ->
                                    function_name(
                                        Packages_to_change, 
                                        install(State, Package), 
                                        Repo, 
                                        Graph,
                                        Size+Package_size, 
                                        Instruction_list ++ [Instruction]
                                    );
                                _ ->
                                    digraph:delete(Graph),
                                    {error, cycle_found}
                            end
                    end;
                Required_operations ->
                    % io:fwrite("\n New operations: ~p", [Required_operations]),
                    case are_there_multiple_paths(Required_operations) of 
                        true ->
                            Paths = get_all_combinations_of_nested_lists(Required_operations),
                            % io:fwrite("\n Possible paths: ~p", [Paths]),
                            compare_paths(Paths, [{<<"+">>, Package}|Packages_to_change], State, Repo, Graph, Size, Instruction_list, none);
                        false ->
                            New_operations = Required_operations ++ [{<<"+">>, Package}|Packages_to_change],
                            function_name(New_operations, State, Repo, Graph, Size, Instruction_list)
                    end
            end
    end;
function_name([{<<"-">>, Package}|Packages_to_change], State, Repo, Graph, Size, Instruction_list) ->
    % io:fwrite("\n Instruction list: ~p, \n State: ~p", [Instruction_list, State]),
    case is_package_installed(State, Package) of
        true ->
            case get_required_operations({<<"-">>, Package}, State, Repo) of    
                [] ->
                    Package_name = maps:get(<<"name">>, Package),
                    Package_version = maps:get(<<"version">>, Package),
                    Instruction = <<"-", Package_name/binary, "=", Package_version/binary>>,
                    Vertex = digraph:add_vertex(Graph, Instruction),
                    case Instruction_list of 
                        [] ->
                            function_name(
                                Packages_to_change, 
                                uninstall(State, Package), 
                                Repo, 
                                Graph,
                                Size+1000000, 
                                Instruction_list ++ [Instruction]
                            );
                        _ ->
                            Last_instruction = lists:last(Instruction_list),
                            brutally_add_edge_to_digraph(Graph, Last_instruction, Vertex),
                            % io:fwrite("Adding edge between: ~p, ~p", [Last_instruction, Vertex]),
                            case digraph:get_cycle(Graph, Vertex) of
                                false ->
                                    function_name(
                                        Packages_to_change, 
                                        uninstall(State, Package), 
                                        Repo, 
                                        Graph,
                                        Size+1000000, 
                                        Instruction_list ++ [Instruction]
                                    );
                                _ ->
                                    digraph:delete(Graph),
                                    {error, cycle_found}
                            end
                    end;
                Required_operations ->
                    % io:fwrite("New operations: ~p", [Required_operations]),
                    New_operations = Required_operations ++ [{<<"-">>, Package}|Packages_to_change],
                    function_name(New_operations, State, Repo, Graph, Size, Instruction_list)
            end;
        false ->
            function_name(Packages_to_change, State, Repo, Graph, Size, Instruction_list)
    end.

compare_paths([], _, _, _, _, _, _, Best_path) ->
    Best_path;
compare_paths([Commands|Rest], Packages_to_change, State, Repo, Graph, Size, Instruction_list, Best_path) ->
    % io:fwrite("\nInstructions: ~p", [Commands]),
    New_graph = digraph_utils:subgraph(Graph, digraph:vertices(Graph)),
    % io:fwrite("\n Creating new graph: ~p, ~p", [digraph:vertices(New_graph), digraph:edges(New_graph)]),
    case compare(New_path = function_name(Commands ++ Packages_to_change, State, Repo, New_graph, Size, Instruction_list), Best_path) of 
        true ->
            % io:fwrite("\nNew path wins: ~p, ~p", [New_path, Best_path]),
            compare_paths(Rest, Packages_to_change, State, Repo, Graph, Size, Instruction_list, New_path);
        false ->
            % io:fwrite("\nOld path wins: ~p, ~p", [New_path, Best_path]),
            compare_paths(Rest, Packages_to_change, State, Repo, Graph, Size, Instruction_list, Best_path)
    end.

brutally_add_edge_to_digraph(Graph, Start, End) ->
    case digraph:add_edge(Graph, Start, End) of
        {error, {bad_edge, _}} ->
            brutally_add_edge_to_digraph(Graph, Start, End);
        _ ->
            ok
    end.

are_there_multiple_paths(List) ->
    lists:any(fun(Elem) ->  
        case is_list(Elem) of 
            true -> 
                true;
            _ ->
                false
        end 
    end, List).

compare({error, cycle_found}, _) ->
    false;
compare(none, _) ->
    false;
compare({_, New_size, _}, Current) ->
    case Current of
        none -> true;
        {error, cycle_found} -> true;
        {_, Size, _} -> New_size < Size
    end. 


get_all_combinations_of_nested_lists(Nested_list) ->
    Required_iterations = get_max_size_of_nested_list_combos(Nested_list, 1),
    get_all_combinations_of_nested_lists(Nested_list, Required_iterations, []).

get_all_combinations_of_nested_lists(_, 0, Acc) ->
    Acc;
get_all_combinations_of_nested_lists(Nested_list, Iteration, Acc) ->
    List = get_combination_of_nested_lists(Nested_list, Iteration, []),
    get_all_combinations_of_nested_lists(Nested_list, Iteration-1, Acc++[List]).

get_combination_of_nested_lists([], _, Acc) ->
    Acc;
get_combination_of_nested_lists([X|Xs], Iteration, Acc) ->
    case is_list(X) of
        true ->
            get_combination_of_nested_lists(Xs, Iteration, Acc ++ [lists:nth(Iteration rem length(X) + 1, X)]);
        false ->
            get_combination_of_nested_lists(Xs, Iteration, Acc ++ [X])
    end.
            
get_max_size_of_nested_list_combos([], Acc) ->
    Acc;
get_max_size_of_nested_list_combos([X|Xs], Acc) ->
    case is_list(X) of
        true ->
            get_max_size_of_nested_list_combos(Xs, Acc * length(X));
        false ->
            get_max_size_of_nested_list_combos(Xs, Acc)
    end.

get_required_operations({Operation, Package}, State, Repo) ->
    case Operation of
        <<"+">> ->
            %io:fwrite("\n Install operation for: ~p", [Package]),
            State_conflicts = get_conflicting_packages(State, Package),
            Self_conflicts = get_self_conflicts(State, Package),
            %io:fwrite("\nConflicts: ~p", [Conflicts]),
            Cl = lists:map(fun(Elem) -> {<<"-">>, Elem} end, State_conflicts ++ Self_conflicts),
            Deps = get_missing_deps(State, Package),
            % io:fwrite("\n Dependencies: ~p", [Deps]),            
            Dl = lists:map(fun(Elem) -> {<<"+">>, Elem} end, Deps),
            % io:fwrite("\n Dependency list: ~p", [Dl]),            
            convert_package_ref_to_json(Dl, Repo) ++ Cl;
        <<"-">> ->
            Dependents = get_dependent_packages(State, Package),
            lists:map(fun(Elem) -> {<<"-">>, Elem} end, Dependents)
    end.

get_missing_deps(State, Package) ->
    case maps:get(<<"depends">>, Package, none) of 
        none ->
            [];
        Value ->
            lists:filtermap(fun(Elem) -> is_dependency_met(Elem, State) end, Value)
    end.

interpret_constraints([], _, Acc) ->
    Acc;
interpret_constraints([<<"+", Constraint/binary>>|Rest], Repo, Acc) ->
    case binary:split(Constraint, <<"=">>) of 
        [Package, Version] ->
            Pack = get_package_from_repo({Package, Version}, Repo),
            interpret_constraints(Rest, Repo, Acc ++ [{<<"+">>, Pack}]);
        [Constraint] ->
            Pack = get_all_packages_with_name(Constraint, Repo, []),
            % io:fwrite("\n Packages with name: ~p", [Pack]),
            interpret_constraints(Rest, Repo, Acc ++ [lists:map(fun(Elem) -> {<<"+">>, Elem} end, Pack)])
    end;
interpret_constraints([<<"-", Constraint/binary>>|Rest], Repo, Acc) ->
    case binary:split(Constraint, <<"=">>) of 
        [Package, Version] ->
            Pack = get_package_from_repo({Package, Version}, Repo),
            interpret_constraints(Rest, Repo, Acc ++ [{<<"-">>, Pack}]);
        [Constraint] ->
            %% Means uninstall all insances of package. Add all instances of package to list, taken from state? 
            % Unsure if this belongs here
            interpret_constraints(Rest, Repo, Acc ++ [{<<"-">>, Constraint}])
    end.


convert_package_ref_to_json(Command_list, Repo) ->
    lists:map(
        fun E({Op, Packages}) -> 
            % io:fwrite("\nPackages to convert to json: ~p", [Packages]),
            case Packages of
                [X|_] ->
                    % Package_ref = X;
                    % io:fwrite("\n Xs: ~p", [Xs]),
                    case length(Packages) of
                        1 ->
                            E({Op, X});
                        _ ->
                            lists:map(fun(Elem) -> E({Op, Elem}) end, Packages)
                    end;
                A ->
                    Package_ref = A,
                    % io:fwrite("\nPackage to convert: ~p", [Package_ref]),
                    case binary:split(Package_ref, [<<">=">>]) of
                        [Name, Y] ->
                            Result = lists:map(fun(Elem) -> {Op, Elem} end, get_all_package_greater_than_equal_version({Name, Y}, Repo, [])),
                            return_list_or_tuple(Result);
                            % {Op, get_first_package_greater_than_equal_version({Name, Y}, Repo)};
                        _ -> 
                            case binary:split(Package_ref, [<<"<=">>]) of
                                [Name, Y] ->
                                    Result = lists:map(fun(Elem) -> {Op, Elem} end, get_all_package_less_than_equal_version({Name, Y}, Repo, [])),
                                    return_list_or_tuple(Result);
                                    % {Op, get_first_package_less_than_equal_version({Name, Y}, Repo)};
                                _ ->
                                    case binary:split(Package_ref, <<"=">>) of
                                        [Name, Version] -> 
                                            {Op, get_package_from_repo({Name, Version}, Repo)};
                                        _ ->
                                            case binary:split(Package_ref, <<">">>) of
                                                [Name, Y] -> 
                                                    Result = lists:map(fun(Elem) -> {Op, Elem} end, get_all_package_greater_than_version({Name, Y}, Repo, [])),
                                                    return_list_or_tuple(Result);
                                                    % {Op, get_first_package_greater_than_version({Name, Y}, Repo)};
                                                _ ->
                                                    case binary:split(Package_ref, <<"<">>) of
                                                        [Name, Y] ->
                                                            Result = lists:map(fun(Elem) -> {Op, Elem} end, get_all_package_less_than_version({Name, Y}, Repo, [])),
                                                            return_list_or_tuple(Result);
                                                            % {Op, get_first_package_less_than_version({Name, Y}, Repo)};
                                                        _ ->
                                                            Result = lists:map(fun(Elem) -> {Op, Elem} end, get_all_packages_with_name(Package_ref, Repo, [])),
                                                            return_list_or_tuple(Result)
                                                            % {Op, get_first_instance_of_package(Package_ref, Repo)}
                                                    end        
                                            end
                                    end
                            end
                    end
            end
        end,
    Command_list).

return_list_or_tuple([X]) ->
    X;
return_list_or_tuple(X) ->
    X.

% ----------------------------------------------------------------------------------------------------------------------------------------------------------
% Takes package references ({Name, Version}) and depending on method returns appropriate packages, all of these are naive only returning the first they find
% ----------------------------------------------------------------------------------------------------------------------------------------------------------
get_package_from_repo(_, []) ->
    {error, not_found};
get_package_from_repo({Name, Version}, [#{<<"name">> := N, <<"version">> := V} = Package|_]) when N == Name andalso V == Version ->
    Package;
get_package_from_repo({Name, Version}, [_|Repo]) ->
    get_package_from_repo({Name, Version}, Repo).

get_all_packages_with_name(_, [], Acc) ->
    Acc;
get_all_packages_with_name(Pack_name, [#{<<"name">> := N} = Package|Rest], Acc) when N == Pack_name ->
    get_all_packages_with_name(Pack_name, Rest, Acc ++ [Package]);
get_all_packages_with_name(Pack_name, [_|Rest], Acc) ->
    get_all_packages_with_name(Pack_name, Rest, Acc).

get_all_package_greater_than_version(_, [], Acc) ->
    Acc;
get_all_package_greater_than_version({Pack_name, Ver}, [#{<<"version">> := V, <<"name">> := N} = Package|Rest], Acc) when N == Pack_name andalso V > Ver ->
    get_all_package_greater_than_version({Pack_name, Ver}, Rest, Acc ++ [Package]);
get_all_package_greater_than_version(Pack_ref, [_|Repo], Acc) ->
    get_all_package_greater_than_version(Pack_ref, Repo, Acc).

get_all_package_greater_than_equal_version(_, [], Acc) ->
    Acc;
get_all_package_greater_than_equal_version({Pack_name, Ver}, [#{<<"version">> := V, <<"name">> := N} = Package|Rest], Acc) when N == Pack_name andalso V >= Ver ->
    get_all_package_greater_than_equal_version({Pack_name, Ver}, Rest, Acc ++ [Package]);
get_all_package_greater_than_equal_version(Pack_ref, [_|Repo], Acc) ->
    get_all_package_greater_than_equal_version(Pack_ref, Repo, Acc).

get_all_package_less_than_version(_, [], Acc) ->
    Acc;
get_all_package_less_than_version({Pack_name, Ver}, [#{<<"version">> := V, <<"name">> := N} = Package|Rest], Acc) when N == Pack_name andalso V < Ver ->
    get_all_package_less_than_version({Pack_name, Ver}, Rest, Acc ++ [Package]);
get_all_package_less_than_version(Pack_ref, [_|Repo], Acc) ->
    get_all_package_less_than_version(Pack_ref, Repo, Acc).

get_all_package_less_than_equal_version(_, [], Acc) ->
    Acc;
get_all_package_less_than_equal_version({Pack_name, Ver}, [#{<<"version">> := V, <<"name">> := N} = Package|Rest], Acc) when N == Pack_name andalso V =< Ver ->
    get_all_package_less_than_equal_version({Pack_name, Ver}, Rest, Acc ++ [Package]);
get_all_package_less_than_equal_version(Pack_ref, [_|Repo], Acc) ->
    get_all_package_less_than_equal_version(Pack_ref, Repo, Acc).

% get_first_instance_of_package(_, []) ->
%     {error, not_found};
% get_first_instance_of_package(Pack_name, [#{<<"name">> := N} = Package|_]) when N == Pack_name ->
%     Package;
% get_first_instance_of_package(Pack_name, [_|Repo]) ->
%     get_first_instance_of_package(Pack_name, Repo).

% get_first_package_greater_than_equal_version(_, []) ->
%     {error, not_found};
% get_first_package_greater_than_equal_version({Pack_name, Ver}, [#{<<"version">> := V, <<"name">> := N} = Package|_]) when N == Pack_name andalso V >= Ver ->
%     Package;
% get_first_package_greater_than_equal_version(Pack_ref, [_|Repo]) ->
%     get_first_package_greater_than_equal_version(Pack_ref, Repo).

% get_first_package_less_than_equal_version(_, []) ->
%     {error, not_found};
% get_first_package_less_than_equal_version({Pack_name, Ver}, [#{<<"version">> := V, <<"name">> := N} = Package|_]) when N == Pack_name andalso V =< Ver ->
%     Package;
% get_first_package_less_than_equal_version(Pack_ref, [_|Repo]) ->
%     get_first_package_less_than_equal_version(Pack_ref, Repo).

% get_first_package_less_than_version(_, []) ->
%     {error, not_found};
% get_first_package_less_than_version({Pack_name, Ver}, [#{<<"version">> := V, <<"name">> := N} = Package|_]) when N == Pack_name andalso V < Ver ->
%     Package;
% get_first_package_less_than_version(Pack_ref, [_|Repo]) ->
%     get_first_package_less_than_version(Pack_ref, Repo).

% get_first_package_greater_than_version(_, []) ->
%     {error, not_found};
% get_first_package_greater_than_version({Pack_name, Ver}, [#{<<"version">> := V, <<"name">> := N} = Package|_]) when N == Pack_name andalso V > Ver ->
%     Package;
% get_first_package_greater_than_version(Pack_ref, [_|Repo]) ->
%     get_first_package_greater_than_version(Pack_ref, Repo).
% -------------------------------------------------------------------------------------------------------------------------------------------------------------------
%
% -------------------------------------------------------------------------------------------------------------------------------------------------------------------

is_package_installed(State, Package) ->
    lists:member(Package, State).

% can_package_be_installed(State, #{<<"version">> := Version, <<"name">> := Name} = Package) ->
%     lists:all(fun(Elem) -> 
%         case maps:get(<<"conflicts">>, Elem, none) of
%             none -> true;
%             {error, _} -> true;
%             Value -> check_conflict_list({Name, Version}, Value)
%         end
%     end, State).

% can_package_be_uninstalled(State, #{<<"version">> := Version, <<"name">> := Name} = Package) ->
%     lists:all(fun(Elem) -> 
%         case maps:get(<<"depends">>, Elem, none) of
%             none -> true;
%             {error, _} -> true;
%             Value -> check_dependencies({Name, Version}, Value)
%         end
%     end, State).

% Returns all packages that conflict with a package to be installed (Used to check if package can be installed)
get_conflicting_packages(State, #{<<"version">> := Version, <<"name">> := Name}) ->
    lists:filtermap(fun(Elem) -> 
        case maps:get(<<"conflicts">>, Elem, none) of
            none -> false;
            {error, _} -> false;
            Value -> check_conflict_list({Name, Version}, Value) 
        end
    end, State).

get_self_conflicts(State, #{<<"conflicts">> := Conflicts}) ->
    lists:filtermap(
        fun(#{<<"name">> := Name, <<"version">> := Version})->  
            check_conflict_list({Name, Version}, Conflicts)
        end, 
    State);
get_self_conflicts(_, _) ->
    [].

% Returns all packages in current state that have a dependecny on the passed package (Used to check if package can be uninstalled)
get_dependent_packages(State, #{<<"version">> := Version, <<"name">> := Name}) ->
    lists:filtermap(fun(Elem) -> 
        case maps:get(<<"depends">>, Elem, none) of
            none -> false;
            {error, _} -> false;
            Value -> check_dependencies({Name, Version}, Value) 
        end
    end, State).

check_dependencies(_, []) ->
    false;
check_dependencies({Name, Version}, [Dependency|Xs]) ->
    case binary:split(Dependency, [<<">=">>]) of
        [Name, Y] -> 
            Version >= Y;
        _ -> 
            case binary:split(Dependency, [<<"<=">>]) of
                [Name, Y] ->
                    Version =< Y;
                _ ->
                    case binary:split(Dependency, <<"=">>) of
                        [Name, Version] -> 
                            true;
                        _ ->
                            case binary:split(Dependency, <<">">>) of
                                [Name, Y] -> 
                                    Version > Y;
                                _ ->
                                    case binary:split(Dependency, <<"<">>) of
                                        [Name, Y] ->
                                            Version < Y;
                                        _ ->
                                            case Dependency of 
                                                Name ->
                                                    false;
                                                _ ->
                                                    check_dependencies({Name, Version}, Xs)
                                            end
                                    end        
                            end
                    end
            end
    end.

check_conflict_list(_, []) ->
    false;
check_conflict_list({Name, Version}, [Conflict|Xs]) ->
    case binary:split(Conflict, [<<">=">>]) of
        [Name, Y] -> 
            Version >= Y;
        _ -> 
            case binary:split(Conflict, [<<"<=">>]) of
                [Name, Y] ->
                    Version =< Y;
                _ ->
                    case binary:split(Conflict, <<"=">>) of
                        [Name, Version] -> 
                            true;
                        _ ->
                            case binary:split(Conflict, <<">">>) of
                                [Name, Y] -> 
                                    Version > Y;
                                _ ->
                                    case binary:split(Conflict, <<"<">>) of
                                        [Name, Y] ->
                                            Version < Y;
                                        _ ->
                                            case Conflict of
                                                Name ->
                                                    true;
                                                _ ->
                                                    check_conflict_list({Name, Version}, Xs)
                                            end
                                    end        
                            end
                    end
            end
    end.

is_dependency_met(_, []) ->
    true;
is_dependency_met([Dep|List], State) ->
    lists:all(fun(Elem) -> is_dependency_met(Elem, State) end, [Dep|List]);
is_dependency_met(Dependency, [#{<<"name">> := Name, <<"version">> := Version}|State]) ->
    case binary:split(Dependency, [<<">=">>]) of
        [Name, Y] -> 
            case Version >= Y of
                true ->
                    false;
                false ->
                    is_dependency_met(Dependency, State)
            end;
        _ -> 
            case binary:split(Dependency, [<<"<=">>]) of
                [Name, Y] ->
                    case Version =< Y of
                        true ->
                            false;
                        false ->
                            is_dependency_met(Dependency, State)
                    end;
                _ ->
                    case binary:split(Dependency, <<"=">>) of
                        [Name, Version] -> 
                            false;
                        _ ->
                            case binary:split(Dependency, <<">">>) of
                                [Name, Y] -> 
                                    case Version > Y of 
                                        true -> 
                                            false;
                                        false ->
                                            is_dependency_met(Dependency, State)
                                    end;
                                _ ->
                                    case binary:split(Dependency, <<"<">>) of
                                        [Name, Y] ->
                                            case Version < Y of
                                                true ->
                                                    false;
                                                false ->
                                                    is_dependency_met(Dependency, State)
                                            end;
                                        _ ->
                                            % io:fwrite("Calling check_dependencies with: ~p, ~p", [Dependency, State]),
                                            % check_dependencies(Dependency, State)
                                            case Dependency of 
                                                Name ->
                                                    false;
                                                _ ->
                                                    is_dependency_met(Dependency, State)
                                            end
                                    end        
                            end
                    end
            end
    end.

% convert_json_to_state(Json_state) ->
%     lists:map(fun(#{<<"name">> := Name, <<"version">> := Version}) -> <<Name/binary, "=", Version/binary>> end, Json_state).

convert_state_to_json(State, Repo) ->
    lists:map(fun(Elem) -> 
        [Name, Ver] = binary:split(Elem, <<"=">>), 
        get_package_from_repo({Name, Ver}, Repo)  
    end, State).

install(State, Package) ->
    State ++ [Package].

uninstall(State, Package) ->
    lists:delete(Package, State).

readfile(FileName) ->
    {ok, _} = file:read_file(FileName).