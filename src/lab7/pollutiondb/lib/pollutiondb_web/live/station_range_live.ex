defmodule PollutiondbWeb.StationRangeLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.get_all(), lat_min: 18, lat_max: 21, lon_min: 49, lon_max: 51)
    {:ok, socket}
  end

  def render(assigns) do
    ~H"""
    Find stations
    <form phx-change="update">
    <label>
    Lon min:
    <input type="range" min="49" max="51" step="0.01" name="lon_min" value={@lon_min}/>
    <span><%= @lon_min %></span>
    </label><br/>

    <label>
    Lon max:
    <input type="range" min="49" max="51" step="0.01" name="lon_max" value={@lon_max}/>
    <span><%= @lon_max %></span>
    </label><br/>

    <label>
    Lat min:
    <input type="range" min="18" max="21" step="0.01" name="lat_min" value={@lat_min}/>
    <span><%= @lat_min %></span>
    </label><br/>

    <label>
    Lat max:
    <input type="range" min="18" max="21" step="0.01" name="lat_max" value={@lat_max}/>
    <span><%= @lat_max %></span>
    </label><br/>
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

  def handle_event("update",
        %{"lat_min" => lat_min, "lat_max" => lat_max, "lon_min" => lon_min, "lon_max" => lon_max}, socket) do
    stations = Station.find_by_location_range(
      to_float(lon_min, 0.0), to_float(lon_max, 0.0), to_float(lat_min, 0.0), to_float(lat_max, 0.0))

    socket = assign(socket, stations: stations, lat_min: lat_min, lat_max: lat_max, lon_min: lon_min, lon_max: lon_max)
    {:noreply, socket}
  end

  defp to_float(value, default_value) do
    case Float.parse(to_string(value)) do
      {result, _} -> result
      _ -> default_value
    end
  end
end