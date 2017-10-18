defmodule SerialFramingProtocolTest do
  use ExUnit.Case, async: true
  use PropCheck
  doctest SerialFramingProtocol

  property "process socket pair" do
    forall message in binary() do
      {:ok, sock0} = ProcessSocket.start_link()
      {:ok, sock1} = ProcessSocket.start_link()
      :ok = ProcessSocket.peer(sock0, sock1)
      :ok = ProcessSocket.connect(sock0)
      true = ProcessSocket.wait_until_connected(sock0)
      true = ProcessSocket.wait_until_connected(sock1)
      :ok = ProcessSocket.write(sock0, message)
      :ok = ProcessSocket.write(sock1, message)
      (message == ProcessSocket.wait_until_flush(sock0, byte_size(message)) && message == ProcessSocket.wait_until_flush(sock1, byte_size(message)))
    end
  end

end
