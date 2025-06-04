defmodule Pollutiondb.Reading do
  use Ecto.Schema
  require Ecto.Query

  schema "readings" do
    field :date, :date
    field :time, :time
    field :type, :string
    field :value, :float
    belongs_to :station, Pollutiondb.Station
  end

  def add_now(station, type, value) do
    date = Date.utc_today()
    time = Time.utc_now() |> Time.truncate(:second)

    add(station, date, time, type, value)
  end

  def add(station, date, time, type, value) do
    %Pollutiondb.Reading{}
    |> changeset(%{date: date, time: time, type: type, value: value, station_id: station.id})
    |> Pollutiondb.Repo.insert
  end

  def find_by_date(date) do
    Ecto.Query.from(r in Pollutiondb.Reading,
      where: r.date == ^date)
    |> Pollutiondb.Repo.all
  end

  def find_10_by_date(date) do
    Ecto.Query.from(r in Pollutiondb.Reading,
      where: r.date == ^date,
      order_by: [desc: r.time],
      limit: 10
    )
    |> Pollutiondb.Repo.all()
    |> Pollutiondb.Repo.preload(:station)
  end

  def get_last_10() do
    Ecto.Query.from(r in Pollutiondb.Reading,
      limit: 10, order_by: [desc: r.date, desc: r.time])
    |> Pollutiondb.Repo.all()
    |> Pollutiondb.Repo.preload(:station)
  end

  defp changeset(reading, changesmap) do
    Ecto.Changeset.cast(reading, changesmap, [:date, :time, :type, :value, :station_id])
    |> Ecto.Changeset.validate_required([:date, :time, :type, :value, :station_id])
    |> Ecto.Changeset.unique_constraint(:data, name: :readings_data_index)
  end

end