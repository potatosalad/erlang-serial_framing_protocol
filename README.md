# Serial Framing Protocol (SFP)

[![Build Status](https://travis-ci.org/potatosalad/erlang-serial_framing_protocol.svg?branch=master)](https://travis-ci.org/potatosalad/erlang-serial_framing_protocol) [![Hex.pm](https://img.shields.io/hexpm/v/serial_framing_protocol.svg)](https://hex.pm/packages/serial_framing_protocol)

Reduction counting NIF for Erlang and Elixir based on [BaroboRobotics/libsfp](https://github.com/BaroboRobotics/libsfp) library, which is &ldquo;a reliable, message-oriented, point-to-point communications protocol suitable for embedding in systems with severely constrained resources.&rdquo;

Designed to play nicely with [Nerves](http://nerves-project.org/), especially [Nerves.UART](https://github.com/nerves-project/nerves_uart).

## Installation

If [using Hex](https://hex.pm/), the package can be installed
by adding `serial_framing_protocol` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:serial_framing_protocol, "~> 1.1.0"}]
end
```

If [using rebar3](http://www.rebar3.org/), the package can be installed by adding `http_signature ` to you list of dependencies in `rebar.config`:

```erlang
{deps, [
  {serial_framing_protocol, "1.1.0"}
]}.
```

The [HexDocs](https://hexdocs.pm) can
be found at [https://hexdocs.pm/serial_framing_protocol](https://hexdocs.pm/serial_framing_protocol).

## Usage

See [`test/support/process_socket.ex`](https://github.com/potatosalad/erlang-serial_framing_protocol/blob/master/test/support/process_socket.ex) or [the docs](https://hexdocs.pm/serial_framing_protocol) for examples.

Real world examples are available as part of [inthezone_nerves](https://github.com/TopSecretRobotics/inthezone_nerves) for the VEX Robotics Competition:

 * [`Vex.Robot.NervesSocket`](https://github.com/TopSecretRobotics/inthezone_nerves/blob/master/vex/lib/vex/robot/nerves_socket.ex) &mdash; handles raw serial data from UART port.
 * [`Vex.Robot.UdpSocket`](https://github.com/TopSecretRobotics/inthezone_nerves/blob/master/vex/lib/vex/robot/udp_socket.ex) &mdash; handles UDP packets forwarded from Raspberry Pi 3.
 * [`Vex.Local.Server.Socket`](https://github.com/TopSecretRobotics/inthezone_nerves/blob/master/vex/lib/vex/local/server/socket.ex) &mdash; interfaces with VEX Cortex-M3 over UART or UDP connection.
