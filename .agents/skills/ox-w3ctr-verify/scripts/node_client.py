import subprocess, json, sys

p = subprocess.Popen(["node", "jstools/index.js", "--timeout", "60000"],
                     stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                     stderr=subprocess.DEVNULL)

def call(obj):
    data = json.dumps(obj).encode()
    p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(data) + data)
    p.stdin.flush()
    n = None
    while True:
        line = p.stdout.readline()
        if line in (b"\r\n", b"\n"):
            break
        if not line:
            raise RuntimeError("server closed")
        k, _, v = line.partition(b":")
        if k.strip().lower() == b"content-length":
            n = int(v.strip())
    return json.loads(p.stdout.read(n))

print("add     ", call({"jsonrpc": "2.0", "method": "add", "params": [2, 3], "id": 1}))
print("echo    ", call({"jsonrpc": "2.0", "method": "echo", "params": {"text": "hi"}, "id": 2}))
print("tex2mml ", str(call({"jsonrpc": "2.0", "method": "tex2mml", "params": {"fragment": r"\(x^2\)"}, "id": 3}))[:80])
print("tex2svg ", str(call({"jsonrpc": "2.0", "method": "tex2svg", "params": {"fragment": r"\(x^2\)"}, "id": 4}))[:80])
print("notfound", call({"jsonrpc": "2.0", "method": "nope", "id": 5}))
p.terminate()
