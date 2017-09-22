defmodule SerialFramingProtocol.ProcessSocket do
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, [])
  end

  def peer(pid, peer) when is_pid(peer) do
    :ok = GenServer.call(pid, {:peer, peer}, :infinity)
    :ok = GenServer.call(peer, {:peer, pid}, :infinity)
    :ok
  end

  def connect(pid) do
    GenServer.call(pid, :connect, :infinity)
  end

  def connected?(pid) do
    GenServer.call(pid, :is_connected, :infinity)
  end

  def send(pid, term) do
    iodata = :erlang.term_to_binary(term)
    write(pid, iodata)
  end

  def write(pid, iodata) do
    GenServer.call(pid, {:write, iodata}, :infinity)
  end

  defmodule State do
    defstruct [
      peer: nil,
      socket: nil
    ]
  end

  alias __MODULE__.State

  require Logger

  def init([]) do
    state = %State{
      socket: :serial_framing_protocol_nif.open()
    }
    {:ok, state}
  end

  def handle_call({:peer, peer}, _from, state = %State{ peer: nil, socket: socket }) do
    state = %{ state | peer: peer }
    :ok = :serial_framing_protocol_nif.init(socket)
    {:reply, :ok, state}
  end
  def handle_call(:connect, _from, state = %State{ peer: nil }) do
    {:reply, {:error, :no_peer}, state}
  end
  def handle_call(:connect, _from, state = %State{ socket: socket }) do
    :ok = :serial_framing_protocol_nif.connect(socket)
    {:reply, :ok, state}
  end
  def handle_call(:is_connected, _from, state = %State{ socket: socket }) do
    reply = :serial_framing_protocol_nif.is_connected(socket)
    {:reply, reply, state}
  end
  def handle_call({:write, _}, _from, state = %State{ peer: nil }) do
    {:reply, {:error, :no_peer}, state}
  end
  def handle_call({:write, iodata}, _from, state = %State{ socket: socket }) do
    :ok = :serial_framing_protocol_nif.write(socket, iodata)
    {:reply, :ok, state}
  end

  def handle_cast({:peer, peer, data}, state = %State{ peer: peer, socket: socket }) when is_pid(peer) do
    Logger.info("[p] #{inspect peer} -> #{inspect self()} = #{inspect data}")
    :ok = :serial_framing_protocol_nif.read(socket, data)
    {:noreply, state}
  end

  def handle_info({:sfp, :write, data}, state = %State{ peer: peer }) when is_pid(peer) do
    Logger.info("[w] #{inspect self()} = #{inspect data}")
    :ok = GenServer.cast(peer, {:peer, :erlang.self(), data})
    {:noreply, state}
  end
  def handle_info({:sfp, :read, data}, state = %State{ peer: peer }) when is_pid(peer) do
    Logger.info("[r] #{inspect self()} = #{inspect data}")
    try do
      term = :erlang.binary_to_term(data)
      Logger.info("[t] #{inspect self()} = #{inspect term}")
    catch _,_ ->
      :ok
    end
    {:noreply, state}
  end
end