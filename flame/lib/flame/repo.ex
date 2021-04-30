defmodule Flame.Repo do
  use Ecto.Repo,
    otp_app: :flame,
    adapter: Ecto.Adapters.Postgres
end
