defmodule Pollutiondb.PollutionDataLoader do
  def parse_line(line) do
    [datetime, type, value, station_id, station_name, coords] = line |> String.split(";")

    [year, month, day] = datetime
           |> String.slice(0..9)
           |> String.split("-")
           |> Enum.map(&String.to_integer/1)
    {:ok, date} = Date.new(year, month, day)

    [hour, minute, second] = datetime
           |> String.slice(11..18)
           |> String.split(":")
           |> Enum.map(&String.to_integer/1)
    {:ok, time} = Time.new(hour, minute, second)

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

  def load_data(filepath) do
    data = get_data(filepath)
    stations = get_stations(data)

    for s <- stations do
      {lon, lat} = s.location
      name = "#{s.stationId} #{s.stationName}"
      Pollutiondb.Station.add(name, lon, lat)
    end

    for r <- data do
      {lon, lat} = r.location
      station = Pollutiondb.Station.find_by_location(lon, lat)

      {date, time} = r.datetime
      Pollutiondb.Reading.add(station, date, time, r.pollutionType, r.pollutionLevel)
    end
  end
end