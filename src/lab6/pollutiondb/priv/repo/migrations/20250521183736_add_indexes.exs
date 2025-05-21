defmodule Pollutiondb.Repo.Migrations.AddIndexes do
  use Ecto.Migration

  def change do
    create unique_index(:stations, [:name])
    create unique_index(:stations, [:lon, :lat], name: :stations_lon_lat_index)
    create unique_index(:readings, [:station_id, :date, :time, :type], name: :readings_data_index)
  end
end
