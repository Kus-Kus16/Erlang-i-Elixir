defmodule PollutiondbWeb.StationLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.get_all(), name: "", lat: "", lon: "", query: "")
    {:ok, socket}
  end

  def render(assigns) do
    ~H"""
    Create a new station
    <form phx-submit="insert">
    Name: <input type="text" name="name" value={@name} required /><br/>
    Lon: <input type="number" name="lon" step="0.01" value={@lon} required /><br/>
    Lat: <input type="number" name="lat" step="0.01" value={@lat} required /><br/>
    <input type="submit" />
    </form>
    <hr>
    Find station
    <form phx-change="search">
    Name: <input type="text" name="query" value={@query} required /><br/>
    </form>
    <hr>
    <table>
      <tr>
        <th>Name</th><th>Longitude</th><th>Latitude</th>
      </tr>
      <%= for station <- @stations do %>
        <tr>
          <td><%= station.name %></td>
          <td><%= station.lon %></td>
          <td><%= station.lat %></td>
        </tr>
      <% end %>
    </table>
    """
  end

  def handle_event("insert", %{"name" => name, "lat" => lat, "lon" => lon}, socket) do
    Station.add(%Station{name: name, lat: to_float(lat, 0.0), lon: to_float(lon, 0.0)})
    socket = assign(socket, stations: Station.get_all(), name: "", lat: "", lon: "")
    {:noreply, socket}
  end

  def handle_event("search", %{"query" => query}, socket) do
    stations = case query do
      "" -> Station.get_all()
      name -> Station.find_by_name(name)
    end

    socket = assign(socket, stations: stations, query: query)
    {:noreply, socket}
  end

  defp to_float(value, default_value) do
    case Float.parse(to_string(value)) do
      {result, _} -> result
      _ -> default_value
    end
  end
end