defmodule FlameWeb.PageController do
  use FlameWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
