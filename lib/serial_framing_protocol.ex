defmodule SerialFramingProtocol do

  defdelegate getsizeof(), to: :serial_framing_protocol_nif
  defdelegate open(), to: :serial_framing_protocol_nif
  defdelegate init(socket), to: :serial_framing_protocol_nif
  defdelegate connect(socket), to: :serial_framing_protocol_nif
  defdelegate is_connected(socket), to: :serial_framing_protocol_nif
  defdelegate read(socket, iodata), to: :serial_framing_protocol_nif
  defdelegate write(socket, iodata), to: :serial_framing_protocol_nif

end
