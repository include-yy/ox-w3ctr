import { test } from 'node:test'
import assert from 'node:assert/strict'

import { unwrap, stripNoise, escapeAttr, tex2mml, tex2svg } from '../lib/math.js'

test('unwrap reads the delimiters', () => {
    assert.deepEqual(unwrap('\\(a+b\\)'), { tex: 'a+b', display: false })
    assert.deepEqual(unwrap('\\[a\\]'), { tex: 'a', display: true })
    assert.deepEqual(unwrap('\\begin{x}y\\end{x}'),
                     { tex: '\\begin{x}y\\end{x}', display: true })
})

test('stripNoise and escapeAttr', () => {
    assert.equal(stripNoise('<math xmlns="m" data-latex="x">\n  <mi>x</mi>\n</math>'),
                 '<math><mi>x</mi></math>')
    assert.equal(escapeAttr('a<"&'), 'a&lt;&quot;&amp;')
})

test('tex2mml and tex2svg still convert (MathJax loads lazily)', async () => {
    assert.match(await tex2mml({ fragment: '\\(x^2\\)' }), /^<math><msup>/)
    const svg = await tex2svg({ fragment: '\\[x\\]' })
    assert.match(svg, /^<span class="math-display"><svg aria-label="x" /)
})
