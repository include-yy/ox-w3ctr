import subprocess, json, sys, os

HERE = os.path.dirname(os.path.abspath(__file__))
p = subprocess.Popen([sys.executable, os.path.join(HERE, "server.py")],
                     stdin=subprocess.PIPE, stdout=subprocess.PIPE)

def send(obj):
    data = json.dumps(obj).encode()
    p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(data) + data)
    p.stdin.flush()

def recv():
    while True:
        line = p.stdout.readline()
        if line in (b"\r\n", b"\n"):
            break
        k, _, v = line.partition(b":")
        if k.strip().lower() == b"content-length":
            n = int(v.strip())
    return json.loads(p.stdout.read(n))

for i, obj in enumerate([
    {"jsonrpc": "2.0", "method": "ping", "id": 1},
    {"jsonrpc": "2.0", "method": "add", "params": [2, 3], "id": 2},
    {"jsonrpc": "2.0", "method": "echo", "params": {"text": "hi"}, "id": 3},
    {"jsonrpc": "2.0", "method": "tex2mml", "params": {"fragment": "x^2"}, "id": 4},
    {"jsonrpc": "2.0", "method": "nope", "id": 5},
    {"jsonrpc": "2.0", "method": "nothere", "id": 6},
    {"jsonrpc": "2.0", "method": "ping", "params": "x", "id": 7},
], 1):
    send(obj)
    print(f"{i}: {recv()}")

# notification (no id) must produce no response; a following request still works
send({"jsonrpc": "2.0", "method": "echo", "params": {"text": "notify"}})
send({"jsonrpc": "2.0", "method": "ping", "id": 8})
print(f"after-notify: {recv()}")
p.terminate()
