defmodule SerialFramingProtocol do

  @type socket() :: :serial_framing_protocol.socket()

  @spec getsizeof() :: non_neg_integer()
  defdelegate getsizeof(), to: :serial_framing_protocol

  @spec open() :: socket()
  defdelegate open(), to: :serial_framing_protocol

  @spec init(socket()) :: :ok
  defdelegate init(socket), to: :serial_framing_protocol

  @spec connect(socket()) :: :ok
  defdelegate connect(socket), to: :serial_framing_protocol

  @spec is_connected(socket()) :: boolean()
  defdelegate is_connected(socket), to: :serial_framing_protocol

  @spec read(socket(), iodata()) :: :ok
  defdelegate read(socket, iodata), to: :serial_framing_protocol

  @spec write(socket(), iodata()) :: :ok
  defdelegate write(socket, iodata), to: :serial_framing_protocol

end
