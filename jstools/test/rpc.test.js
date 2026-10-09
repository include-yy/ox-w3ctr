import { test } from 'node:test'
import assert from 'node:assert/strict'

import { frame, createParser } from '../lib/rpc.js'

test('frame counts UTF-8 bytes, not characters', () => {
    assert.equal(frame('{}'), 'Content-Length: 2\r\n\r\n{}')
    assert.equal(frame('"é中"'), 'Content-Length: 7\r\n\r\n"é中"')
})

const collect = () => {
    const seen = []
    return { seen, feed: createParser((m) => seen.push(m)) }
}

test('a message split at every byte is still parsed once', () => {
    const { seen, feed } = collect()
    const bytes = Buffer.from(frame('{"a":"é中"}'))
    for (const b of bytes) feed(Buffer.from([b]))
    assert.deepEqual(seen, ['{"a":"é中"}'])
})

test('several messages in one chunk, and a partial one kept', () => {
    const { seen, feed } = collect()
    const two = frame('1') + frame('22')
    const third = frame('333')
    feed(Buffer.from(two + third.slice(0, 5)))
    assert.deepEqual(seen, ['1', '22'])
    feed(Buffer.from(third.slice(5)))
    assert.deepEqual(seen, ['1', '22', '333'])
})

test('a header without Content-Length is skipped', () => {
    const { seen, feed } = collect()
    feed(Buffer.from('X-Junk: 1\r\n\r\n' + frame('ok')))
    assert.deepEqual(seen, ['ok'])
})
