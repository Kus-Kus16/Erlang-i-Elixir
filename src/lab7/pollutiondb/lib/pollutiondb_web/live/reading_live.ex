defmodule PollutiondbWeb.ReadingLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Reading
  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, readings: Reading.get_last_10(), date: Date.utc_today(),
      stations: Station.get_all(), station_id: nil, type: "PM1", value: 0)
    {:ok, socket}
  end

  def render(assigns) do
    ~H"""
    Add current reading
    <form phx-submit="insert">
      Station:<br/>
        <select name="station_id" required>
          <%= for station <- @stations do %>
          <option label={station.name} value={station.id} selected={station.id == @station_id}/>
          <% end %>
        </select><br/>
      Type: <input type="text" name="type" value={@type} required /><br/>
      Value: <input type="number" name="value" step="0.1" value={@value} required /><br/>
      <input type="submit" />
    </form>
    <hr>
    Find readings
    <form phx-change="update">
      <input type="date" name="date" value={@date} />
    </form>
    <hr>
    <table>
      <tr>
        <th>Name</th><th>Date</th><th>Time</th><th>Type</th><th>Value</th>
      </tr>
      <%= for reading <- @readings do %>
        <tr>
          <td><%= reading.station.name %></td>
          <td><%= reading.date %></td>
          <td><%= reading.time %></td>
          <td><%= reading.type %></td>
          <td><%= reading.value %></td>
        </tr>
      <% end %>
    </table>
    """
  end

  def handle_event("insert", %{"station_id" => station_id, "type" => type, "value" => value}, socket) do
    mock_station = %Station{id: to_int(station_id, 1)}
    Reading.add_now(mock_station, type, to_float(value, 0))
    socket = assign(socket, station_id: nil, type: type, value: 0)
    {:noreply, socket}
  end

  def handle_event("update", %{"date" => given_date}, socket) do
    date = to_date(given_date)
    readings = case date do
      nil -> Reading.get_last_10()
      date -> Reading.find_10_by_date(date)
    end

    socket = assign(socket, readings: readings, date: given_date)
    {:noreply, socket}
  end

  defp to_date(date) do
    case Date.from_iso8601(date) do
      {:ok, result} -> result
      _ -> nil
    end
  end

  defp to_float(value, default_value) do
    case Float.parse(to_string(value)) do
      {result, _} -> result
      _ -> default_value
    end
  end

  defp to_int(value, default_value) do
    case Integer.parse(to_string(value)) do
      {result, _} -> result
      _ -> default_value
    end
  end

end