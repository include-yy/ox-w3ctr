import { test } from 'node:test'
import assert from 'node:assert/strict'

import { stripNoise, escapeAttr, tex2mml, tex2svg } from '../lib/math.js'

test('stripNoise and escapeAttr', () => {
    assert.equal(stripNoise('<math xmlns="m" data-latex="x">\n  <mi>x</mi>\n</math>'),
                 '<math><mi>x</mi></math>')
    assert.equal(escapeAttr('a<"&'), 'a&lt;&quot;&amp;')
})

test('tex2mml and tex2svg convert explicit { tex, display }', async () => {
    assert.match(await tex2mml({ tex: 'x^2', display: false }), /^<math><msup>/)
    assert.match(await tex2mml({ tex: 'x^2', display: true }),
                 /^<math display="block"><msup>/)
    // Inner \(...\) inside an environment must not truncate the environment.
    const env = '\\begin{equation}\\text{if } \\(x\\) \\text{ then } y\\end{equation}'
    const envMml = await tex2mml({ tex: env, display: true })
    assert.match(envMml, /<math display="block">/)
    assert.match(envMml, /then/)
    assert.match(await tex2svg({ tex: 'x', display: false }),
                 /^<svg aria-label="x" /)
    const svg = await tex2svg({ tex: 'x', display: true })
    assert.match(svg, /^<span class="math-display"><svg aria-label="x" /)
})
