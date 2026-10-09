#!/usr/bin/env python3
"""Content-Length stdio JSON-RPC server that also writes to stderr.

Used to check whether `jsonrpc-process-connection' captures the child's
stderr into its own stderr buffer.
"""
import sys

sys.stderr.write("HELLO-STDERR\n")
sys.stderr.flush()

from jsonrpcserver import method, dispatch, Success


@method
def ping():
    return Success("pong")


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
