// Content-Length framing for JSON-RPC 2.0 over stdin/stdout: the framing
// `jsonrpc.el` reads and writes.  The length counts UTF-8 BYTES, and there is
// no trailing newline.

// Return MESSAGE (a JSON string) with its header.
export const frame = (message) =>
    `Content-Length: ${Buffer.byteLength(message, 'utf8')}\r\n\r\n${message}`

// Return a function to feed the input stream to, chunk by chunk.  It calls
// `onMessage` with each complete message body, as a string.  A chunk may
// carry several messages, or only part of one, so the remainder is kept for
// the next chunk.  A header without a Content-Length is skipped.
export const createParser = (onMessage) => {
    let pending = Buffer.alloc(0)
    return (chunk) => {
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
            onMessage(message)
        }
    }
}
