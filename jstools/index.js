// ox-w3ctr MathJax RPC helper.
//
// JSON-RPC 2.0 over stdin/stdout, framed with `Content-Length: <bytes>\r\n\r\n`
// (the framing `jsonrpc.el` reads and writes).  The Emacs side starts this
// process once and reuses it for a whole export, then it exits by itself once
// idle.
//
// Only `tex2mml` / `tex2svg` are used by ox-w3ctr; `echo` and `add` are for
// manual testing.

import { stdin, stdout } from 'process'

import * as Mathjax from 'mathjax'
import { JSONRPCServer } from 'json-rpc-2.0'
import { Command } from 'commander'

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
// MathJax
// ---------------------------------------------------------------------------

// `ui/safe` filters the HTML attributes that TeX macros can inject.  Without
// it, the `html` extension (auto-loaded on demand) lets `\href{javascript:...}`
// and friends through unfiltered.
// `output/svg` is what creates `tex2svg' / `tex2svgPromise'; `tex2mml' is
// available regardless of the output jax.
const mathjax = await Mathjax.init({
    loader: { load: ['input/tex', 'output/svg', 'ui/safe', 'adaptors/liteDOM'] }
})

// MathJax expects bare TeX, but ox-w3ctr hands over a whole fragment with its
// delimiters.  Pull out the math and note whether it was inline or display.
// A `\begin{...}...\end{...}` environment matches nothing and is display.
const unwrap = (fragment) => {
    const match = fragment.match(/\\\(([\s\S]*?)\\\)|\\\[([\s\S]*?)\\\]/)
    if (!match) return { tex: fragment, display: true }
    const inline = match[1] !== undefined
    return { tex: inline ? match[1] : match[2], display: !inline }
}

// MathJax 4 tags every node with `data-latex`, declares the namespace, and
// pretty-prints.  None of that is wanted in the exported HTML, so strip it and
// collapse the tree onto one line.  Attribute values are escaped, so neither
// `"` nor `>` can occur inside them.
const stripNoise = (mml) => mml
    .replace(/\s+data-latex(?:-item)?="[^"]*"/g, '')
    .replace(/\s+xmlns="[^"]*"/g, '')
    .replace(/\s+display="inline"/g, '')
    .replace(/>\s+</g, '><')
    .trim()

// Use the promise-based conversion: the synchronous `tex2mml` throws
// "MathJax retry" as soon as the input needs an extension or extra font data,
// which v4 loads lazily.
const tex2mml = async ({ fragment }) => {
    const { tex, display } = unwrap(fragment)
    const mml = await mathjax.tex2mmlPromise(tex, { display })
    return stripNoise(mml)
}

const escapeAttr = (s) => s
    .replace(/&/g, '&amp;')
    .replace(/"/g, '&quot;')
    .replace(/</g, '&lt;')

// MathJax wraps the SVG in a non-standard <mjx-container>, which would fail HTML
// validation, so keep only the <svg>.  Drop the `data-latex` annotations and
// give the image an accessible name.  Display math gets a phrasing wrapper
// (`.math-display'), so it stays valid inside a <p>.
const tex2svg = async ({ fragment }) => {
    const { tex, display } = unwrap(fragment)
    const node = await mathjax.tex2svgPromise(tex, { display })
    const label = escapeAttr(tex.replace(/\s+/g, ' ').trim())
    const svg = mathjax.startup.adaptor.outerHTML(node)
        .replace(/^<mjx-container\b[^>]*>/, '')
        .replace(/<\/mjx-container>$/, '')
        .replace(/\s+data-latex(?:-item)?="[^"]*"/g, '')
        .replace(/^<svg /, `<svg aria-label="${label}" `)
    return display ? `<span class="math-display">${svg}</span>` : svg
}

// ---------------------------------------------------------------------------
// RPC
// ---------------------------------------------------------------------------

// Errors are already returned to Emacs as JSON-RPC error responses, so silence
// the default `console.warn`: its stderr output would be mixed into the stdout
// stream and corrupt the protocol.
const server = new JSONRPCServer({ errorListener: () => {} })

server.addMethod('tex2mml', tex2mml)
server.addMethod('tex2svg', tex2svg)
server.addMethod('echo', ({ text }) => text)
server.addMethod('add', ([a, b]) => a + b)

// Content-Length framing: the length is in UTF-8 BYTES, and there is no
// trailing newline.  A chunk may carry several messages, or only part of one,
// so keep the remainder and re-parse.
let pending = Buffer.alloc(0)

const respond = (message) => {
    touch()
    server.receiveJSON(message).then((response) => {
        if (!response) return              // a notification gets no reply
        const json = JSON.stringify(response)
        const bytes = Buffer.byteLength(json, 'utf8')
        stdout.write(`Content-Length: ${bytes}\r\n\r\n${json}`)
    })
}

stdin.on('data', (chunk) => {
    pending = Buffer.concat([pending, chunk])
    for (;;) {
        const headerEnd = pending.indexOf('\r\n\r\n')
        if (headerEnd < 0) return          // header not complete yet
        const header = pending.subarray(0, headerEnd).toString('ascii')
        const match = header.match(/content-length:\s*(\d+)/i)
        if (!match) {                      // unknown header: resynchronise
            pending = pending.subarray(headerEnd + 4)
            continue
        }
        const start = headerEnd + 4
        const end = start + Number(match[1])
        if (pending.length < end) return   // body not complete yet
        const message = pending.subarray(start, end).toString('utf8')
        pending = pending.subarray(end)
        respond(message)
    }
})
