
-module(calc).

-export([data/0, calculate_max/2, number_of_readings/2, calculate_mean/2]).

data() ->
    [
        {"StacjaA", {{2025, 3, 12}, {12,35,18}}, [{"pm10", 40}, {"pm2.5", 31}, {"temp", 18}]},
        {"StacjaB", {{2025, 3, 12}, {12,31,43}}, [{"pm2.5", 38}, {"temp", 18}, {"hum", 12}]},
        {"StacjaC", {{2025, 3, 12}, {12,40,05}}, [{"pm2.5", 29}, {"pm1", 23}, {"temp", 16}]},

        {"StacjaA", {{2025, 3, 13}, {14,20,00}}, [{"pm10", 45}, {"pm2.5", 33}, {"temp", 19}]},
        {"StacjaB", {{2025, 3, 13}, {14,18,55}}, [{"pm2.5", 40}, {"temp", 20}, {"hum", 15}]},
        {"StacjaC", {{2025, 3, 13}, {14,35,30}}, [{"pm2.5", 31}, {"pm1", 24}, {"temp", 17}]},

        {"StacjaA", {{2025, 3, 14}, {08,15,10}}, [{"pm10", 42}, {"pm2.5", 30}, {"temp", 18}]},
        {"StacjaB", {{2025, 3, 14}, {08,10,50}}, [{"pm2.5", 35}, {"temp", 19}, {"hum", 18}]},
        {"StacjaC", {{2025, 3, 14}, {08,45,30}}, [{"pm2.5", 32}, {"pm1", 22}, {"temp", 17}]}
    ].

number_of_readings(Readings, Date) when is_list(Readings) ->
    Correct = [ 1 || {_, {D, _}, _} <- Readings, D == Date],
    length(Correct);
number_of_readings(_, _) -> {error, wrong_readings_format}.

calculate_max(Readings, Type) when is_list(Readings) ->
    Correct = [ V || {_, _, R} <- Readings, {T, V} <- R, T == Type ],

    case Correct of
        [] -> undefined;
        _ -> lists:max(Correct)
    end;
calculate_max(_, _) -> {error, wrong_readings_format}.

calculate_mean(Readings, Type) when is_list(Readings) ->
    Correct = [ V || {_, _, R} <- Readings, {T, V} <- R, T == Type ],

    case Correct of
        [] -> undefined;
        _ -> lists:sum(Correct) / length(Correct)
    end;
calculate_mean(_, _) -> {error, wrong_readings_format}.
