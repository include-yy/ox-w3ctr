#!/usr/bin/env python3
"""Content-Length framed JSON-RPC stdio server, a stand-in for jstools."""
import sys
from jsonrpcserver import method, dispatch, Success, Error


@method
def ping():
    return Success("pong")


@method
def add(a, b):
    return Success(a + b)


@method
def echo(text=None):
    return Success(text)


@method
def tex2mml(fragment):
    return Success(f"<math>{fragment}</math>")


@method
def tex2svg(fragment):
    return Success(f"<svg>{fragment}</svg>")


@method
def nope():
    return Error(-32000, "boom", "data")


def read_exact(stream, n):
    buf = b""
    while len(buf) < n:
        chunk = stream.read(n - len(buf))
        if not chunk:
            return None
        buf += chunk
    return buf


def main():
    inp, out = sys.stdin.buffer, sys.stdout.buffer
    while True:
        headers = {}
        while True:
            line = inp.readline()
            if not line:
                return
            line = line.strip()
            if not line:
                break
            k, _, v = line.partition(b":")
            headers[k.strip().lower()] = v.strip()
        n = int(headers.get(b"content-length", b"0"))
        body = read_exact(inp, n)
        if body is None:
            return
        response = dispatch(body.decode("utf-8"))
        if response:
            data = response.encode("utf-8")
            out.write(b"Content-Length: %d\r\n\r\n" % len(data))
            out.write(data)
            out.flush()


if __name__ == "__main__":
    main()
