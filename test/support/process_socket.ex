defmodule ProcessSocket do
  @behaviour :gen_statem

  def start_link() do
    :gen_statem.start_link(__MODULE__, [], [])
  end

  def connect(pid) do
    :gen_statem.cast(pid, :connect)
  end

  def connected?(pid) do
    :gen_statem.call(pid, :is_connected)
  end

  def disconnect(pid) do
    :gen_statem.cast(pid, :disconnect)
  end

  def flush(pid) do
    :gen_statem.call(pid, :flush)
  end

  def peer(a, b) do
    :ok = :gen_statem.cast(a, {:peer, b})
    :ok = :gen_statem.cast(b, {:peer, a})
    true = peered?(a)
    true = peered?(b)
    :ok
  end

  def peered?(pid) do
    :gen_statem.call(pid, :is_peered)
  end

  def read(pid, iodata) do
    :gen_statem.cast(pid, {:read, iodata})
  end

  def write(pid, iodata) do
    :gen_statem.cast(pid, {:write, iodata})
  end

  def stop(pid) do
    :gen_statem.cast(pid, :stop)
  end

  def wait_until_connected(pid, timeout \\ 5000)
  def wait_until_connected(_pid, timeout) when timeout <= 0 do
    false
  end
  def wait_until_connected(pid, timeout) when timeout > 0 do
    t0 = :erlang.monotonic_time(:millisecond)
    if connected?(pid) do
      true
    else
      :timer.sleep(10)
      t1 = :erlang.monotonic_time(:millisecond)
      wait_until_connected(pid, timeout - (t1 - t0))
    end
  end

  def wait_until_flush(pid, size, timeout \\ 5000) do
    wait_until_flush(pid, size, timeout, <<>>)
  end

  @doc false
  defp wait_until_flush(_pid, _size, timeout, _data) when timeout <= 0 do
    :error
  end
  defp wait_until_flush(pid, size, timeout, data) do
    t0 = :erlang.monotonic_time(:millisecond)
    data = << data :: binary(), (:erlang.iolist_to_binary(flush(pid))) :: binary() >>
    if byte_size(data) >= size do
      data
    else
      :timer.sleep(10)
      t1 = :erlang.monotonic_time(:millisecond)
      wait_until_flush(pid, size, timeout - (t1 - t0))
    end
  end

  defmodule Data do
    defstruct [
      messages: [],
      monitor: nil,
      peer: nil,
      sfp: nil
    ]
  end

  alias __MODULE__.Data, as: Data

  @impl :gen_statem
  def callback_mode() do
    [:handle_event_function, :state_enter]
  end

  @impl :gen_statem
  def init([]) do
    sfp = SerialFramingProtocol.open()
    data = %Data{
      sfp: sfp
    }
    {:ok, :unpeered, data}
  end

  @impl :gen_statem
  # State Enter Events
  def handle_event(:enter, old_state, :unpeered, data) do
    data =
      if old_state == :unpeered do
        data
      else
        case data do
          %{ monitor: nil, peer: nil, sfp: sfp } ->
            :ok = SerialFramingProtocol.init(sfp)
            %{ data | messages: [] }
          %{ monitor: monitor, peer: peer, sfp: sfp } ->
            _ = :erlang.demonitor(monitor, [:flush])
            :ok = :gen_statem.cast(peer, :unpeer)
            :ok = SerialFramingProtocol.init(sfp)
            %{ data | messages: [], monitor: nil, peer: nil }
        end
      end
    {:keep_state, data}
  end
  def handle_event(:enter, _old_state, :disconnected, data = %Data{ sfp: sfp }) do
    :ok = SerialFramingProtocol.init(sfp)
    data = %{ data | messages: [] }
    {:keep_state, data}
  end
  def handle_event(:enter, _old_state, :connected, _data) do
    :keep_state_and_data
  end
  # Call Events
  def handle_event({:call, from}, :flush, _state, data = %Data{ messages: messages }) do
    messages = :lists.reverse(messages)
    data = %{ data | messages: [] }
    actions = [{:reply, from, messages}]
    {:keep_state, data, actions}
  end
  def handle_event({:call, from}, :is_connected, state, _data) do
    reply =
      if state == :connected do
        true
      else
        false
      end
    {:keep_state_and_data, [{:reply, from, reply}]}
  end
  def handle_event({:call, from}, :is_peered, state, _data) do
    reply =
      if state == :unpeered do
        false
      else
        true
      end
    {:keep_state_and_data, [{:reply, from, reply}]}
  end
  # Cast Events
  def handle_event(:cast, {:peer, peer}, :unpeered, data = %Data{ monitor: nil, peer: nil }) do
    monitor = :erlang.monitor(:process, peer)
    data = %{ data | monitor: monitor, peer: peer }
    {:next_state, :disconnected, data}
  end
  def handle_event(:cast, {:peer, peer}, _state, _data = %Data{ peer: peer }) when is_pid(peer) do
    :keep_state_and_data
  end
  def handle_event(:cast, {:peer, _}, _state, data) do
    actions = [{:postpone, true}]
    {:next_state, :unpeered, data, actions}
  end
  def handle_event(:cast, :connect, :connected, _data) do
    :keep_state_and_data
  end
  def handle_event(:cast, :connect, :disconnected, _data = %Data{ sfp: sfp }) do
    :ok = SerialFramingProtocol.init(sfp)
    :ok = SerialFramingProtocol.connect(sfp)
    :keep_state_and_data
  end
  def handle_event(:cast, :disconnect, :disconnected, _data) do
    :keep_state_and_data
  end
  def handle_event(:cast, :disconnect, :connected, data = %Data{}) do
    {:next_state, :disconnected, data}
  end
  def handle_event(:cast, {operation, _}, :unpeered, _data) when operation in [:read, :write] do
    :keep_state_and_data
  end
  def handle_event(:cast, {:read, iodata}, state, data = %Data{ sfp: sfp }) when state != :unpeered do
    :ok = SerialFramingProtocol.read(sfp, iodata)
    new_state =
      if SerialFramingProtocol.is_connected(sfp) do
        :connected
      else
        :disconnected
      end
    if state == new_state do
      :keep_state_and_data
    else
      {:next_state, new_state, data}
    end
  end
  def handle_event(:cast, {:write, iodata}, state, data = %Data{ sfp: sfp }) when state != :unpeered do
    :ok = SerialFramingProtocol.write(sfp, iodata)
    new_state =
      if SerialFramingProtocol.is_connected(sfp) do
        :connected
      else
        :disconnected
      end
    if state == new_state do
      :keep_state_and_data
    else
      {:next_state, new_state, data}
    end
  end
  def handle_event(:cast, :stop, _state, _data) do
    :stop
  end
  # Info Events
  def handle_event(:cast, {:sfp, operation, _}, :unpeered, _data) when operation in [:read, :write] do
    :keep_state_and_data
  end
  def handle_event(:info, {:sfp, :read, iodata}, state, data = %Data{ messages: messages }) when state != :unpeered do
    messages = [iodata | messages]
    data = %{ data | messages: messages }
    {:keep_state, data}
  end
  def handle_event(:info, {:sfp, :write, iodata}, state, _data = %Data{ peer: peer }) when state != :unpeered do
    :ok = ProcessSocket.read(peer, iodata)
    :keep_state_and_data
  end

end