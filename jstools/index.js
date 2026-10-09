// ox-w3ctr JSON-RPC helper.
//
// JSON-RPC 2.0 over stdin/stdout, framed with `Content-Length: <bytes>\r\n\r\n`
// (the framing `jsonrpc.el` reads and writes; see lib/rpc.js).  The Emacs side
// starts this process once and reuses it for a whole export; it exits when
// stdin closes, or by itself once idle.
//
// Methods used by ox-w3ctr: `tex2mml' / `tex2svg' (lib/math.js) and
// `highlight' / `highlightLanguages' (lib/highlight.js).  `echo' and `add' are
// for manual testing.  Each library is loaded on its first call.

import { stdin, stdout } from 'process'

import { JSONRPCServer } from 'json-rpc-2.0'
import { Command } from 'commander'

import { frame, createParser } from './lib/rpc.js'
import { tex2mml, tex2svg } from './lib/math.js'
import { highlight, highlightLanguages } from './lib/highlight.js'

// ---------------------------------------------------------------------------
// Command line
// ---------------------------------------------------------------------------

const program = new Command()
program.option('--timeout <ms>', 'exit after this long without a request', '30000')
program.parse(process.argv)

const idleTimeout = Number(program.opts().timeout) || 30000

// ---------------------------------------------------------------------------
// Idle watchdog
//
// The process lives as long as Emacs keeps talking to it.  If Emacs dies
// without closing us, leave after `idleTimeout` ms with no request.
// ---------------------------------------------------------------------------

let idleTimer = null
const touch = () => {
    clearTimeout(idleTimer)
    idleTimer = setTimeout(() => process.exit(0), idleTimeout)
}
touch()

// ---------------------------------------------------------------------------
// RPC
// ---------------------------------------------------------------------------

// Errors are already returned to Emacs as JSON-RPC error responses, so silence
// the default `console.warn`: its stderr output would be mixed into the stdout
// stream and corrupt the protocol.
const server = new JSONRPCServer({ errorListener: () => {} })

server.addMethod('tex2mml', tex2mml)
server.addMethod('tex2svg', tex2svg)
server.addMethod('highlight', highlight)
server.addMethod('highlightLanguages', highlightLanguages)
server.addMethod('echo', ({ text }) => text)
server.addMethod('add', ([a, b]) => a + b)

const respond = (message) => {
    touch()
    server.receiveJSON(message).then((response) => {
        if (!response) return              // a notification gets no reply
        stdout.write(frame(JSON.stringify(response)))
    })
}

stdin.on('data', createParser(respond))
// Emacs closed the pipe: nobody is left to answer.
stdin.on('end', () => process.exit(0))
