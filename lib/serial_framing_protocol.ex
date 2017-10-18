defmodule SerialFramingProtocol do
  @moduledoc ~S"""
  Reduction counting NIF for Erlang and Elixir based on [BaroboRobotics/libsfp](https://github.com/BaroboRobotics/libsfp) library, which is &ldquo;a reliable, message-oriented, point-to-point communications protocol suitable for embedding in systems with severely constrained resources.&rdquo;

  ## Example

      iex> # Let's open and initialize two paired sockets:
      iex> sock1 = SerialFramingProtocol.open()
      #Reference<0.2174601288.3794927620.67855>
      iex> sock2 = SerialFramingProtocol.open()
      #Reference<0.2174601288.3794927617.66764>
      iex> SerialFramingProtocol.init(sock1)
      :ok
      iex> SerialFramingProtocol.init(sock2)
      :ok
      iex> # Initially, neither one is connected:
      iex> SerialFramingProtocol.is_connected(sock1)
      false
      iex> SerialFramingProtocol.is_connected(sock2)
      false
      iex> # There are a series of 3 packets sent back and forth:
      iex> SerialFramingProtocol.connect(sock1)
      :ok
      iex> packet1 = receive do {:sfp, :write, packet} -> packet end
      <<126, 192, 116, 54, 126>>
      iex> SerialFramingProtocol.read(sock2, packet1)
      :ok
      iex> packet2 = receive do {:sfp, :write, packet} -> packet end
      <<126, 193, 253, 39, 126>>
      iex> SerialFramingProtocol.read(sock1, packet2)
      :ok
      iex> packet3 = receive do {:sfp, :write, packet} -> packet end
      <<126, 194, 102, 21, 126>>
      iex> SerialFramingProtocol.read(sock2, packet3)
      :ok
      iex> # And now we're connected:
      iex> SerialFramingProtocol.is_connected(sock1)
      true
      iex> SerialFramingProtocol.is_connected(sock2)
      true
      iex> # Let's send a message from sock1 to sock2:
      iex> SerialFramingProtocol.write(sock1, "hello")
      :ok
      iex> packet4 = receive do {:sfp, :write, packet} -> packet end
      <<126, 0, 104, 101, 108, 108, 111, 69, 12, 126>>
      iex> SerialFramingProtocol.read(sock2, packet4)
      :ok
      iex> # We received the message!
      iex> message = receive do {:sfp, :read, message} -> message end
      "hello"

  ## ProcessSocket Example

  The module in [`test/support/process_socket.ex`](https://github.com/potatosalad/erlang-serial_framing_protocol/blob/master/test/support/process_socket.ex) provides an easier to understand example of the peering process and message passing.

      iex> {:ok, sock1} = ProcessSocket.start_link()
      #PID<0.155.0>
      iex> {:ok, sock2} = ProcessSocket.start_link()
      #PID<0.156.0>
      iex> ProcessSocket.peer(sock1, sock2)
      :ok
      iex> ProcessSocket.connect(sock1)
      :ok
      iex> Process.write(sock1, "hello")
      :ok
      iex> Process.write(sock1, "world")
      :ok
      iex> Process.flush(sock2)
      ["hello", "world"]

  """

  @type socket() :: :serial_framing_protocol.socket()

  @doc """
  Returns the size in bytes of the internal `SFPcontext` struct (should be 2976).
  """
  @spec getsizeof() :: non_neg_integer()
  defdelegate getsizeof(), to: :serial_framing_protocol

  @doc """
  Opens a new SFP socket and returns the magic reference.

  The socket will remain open until the calling process dies.
  """
  @spec open() :: socket()
  defdelegate open(), to: :serial_framing_protocol

  @doc """
  Initializes a new SFP socket or disconnect an old one.
  """
  @spec init(socket()) :: :ok
  defdelegate init(socket), to: :serial_framing_protocol

  @doc """
  Starts the connection process, which consists of a series of 3 packets.

  Packets will be sent to the calling process in the form of `{:sfp, :write, packet}`.
  """
  @spec connect(socket()) :: :ok
  defdelegate connect(socket), to: :serial_framing_protocol

  @doc """
  Returns whether a SFP socket is connected or not.
  """
  @spec is_connected(socket()) :: boolean()
  defdelegate is_connected(socket), to: :serial_framing_protocol

  @doc """
  Reads raw packet data into a SFP socket.

  For each message emitted, the calling process will receive `{:sfp, :read, message}`.
  """
  @spec read(socket(), iodata()) :: :ok
  defdelegate read(socket, iodata), to: :serial_framing_protocol

  @doc """
  Writes raw data into a SFP socket.

  For each packet emitted, the calling process will receive `{:sfp, :write, packet}`.
  """
  @spec write(socket(), iodata()) :: :ok
  defdelegate write(socket, iodata), to: :serial_framing_protocol

end
