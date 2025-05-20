# Run as: iex --dot-iex path/to/notebook.exs

# Title: Untitled notebook

# ── Section ──

defmodule PollutionDataLoader do
  def parse_line(line) do
    [datetime, type, value, station_id, station_name, coords] = line |> String.split(";")
    date = datetime |> String.slice(0..9) |> String.split("-") |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()
    time = datetime |> String.slice(11..18) |> String.split(":") |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()

    %{:datetime => {date, time}, 
      :location => coords |> String.split(",") |> Enum.map(&String.to_float/1) |> List.to_tuple(),
      :stationId => station_id |> String.to_integer(),
      :stationName => station_name,
      :pollutionType => type,
      :pollutionLevel => value |> String.to_float()}
  end

  def get_data(filepath) do
    File.read!(filepath)
    |> String.trim()
    |> String.split("\r\n")
    |> Enum.map(&parse_line/1)
  end

  def get_stations(data) do
    data
    |> Enum.map(&Map.take(&1, [:location, :stationId, :stationName]))
    |> Enum.uniq_by(& &1.stationId)
  end
end

path = "C:\\Users\\Maciej\\Desktop\\Programowanie\\Erlang-i-Elixir\\src\\lab5\\AirlyData-ALL-50k.csv"
data = PollutionDataLoader.get_data(path)
stations = PollutionDataLoader.get_stations(data)

Application.stop(:makota)

Code.append_path("C:\\Users\\Maciej\\Desktop\\Programowanie\\Erlang-i-Elixir\\src\\lab4\\makota\\_build\\default\\lib\\makota\\ebin")
Application.start(:makota)

fn () -> for s <- stations
  do :pollution_gen_server.add_station("#{s.stationId} #{s.stationName}", s.location) end end
|> :timer.tc() |> elem(0) |> Kernel./(1_000_000)

fn () -> for r <- data 
  do :pollution_gen_server.add_value(r.location, r.datetime, r.pollutionType, r.pollutionLevel) end end
|> :timer.tc() |> elem(0) |> Kernel./(1_000_000)

:pollution_gen_server.get_station_min("9910 Polska, Kraków, Studencka", "PM10")

fn () -> 
  :pollution_gen_server.get_station_min("9910 Polska, Kraków, Studencka", "PM10") end
|> :timer.tc() |> elem(0) |> Kernel./(1_000_000)

:pollution_gen_server.get_daily_mean({2024, 2, 10}, "PM25")

fn () -> 
  :pollution_gen_server.get_daily_mean("{2024, 2, 10}", "PM25") end
|> :timer.tc() |> elem(0) |> Kernel./(1_000_000)

:pollution_gen_server.get_station_mean("9910 Polska, Kraków, Studencka", "PM10")

:pollution_gen_server.get_moving_mean("57570 Polska, Kraków, Floriana Straszewskiego", {2024, 2, 10}, "PM1")

