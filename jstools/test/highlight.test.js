import { test } from 'node:test'
import assert from 'node:assert/strict'

import {
    highlight, highlightLanguages, toPairs, decode, theme, slugs, rules,
    UNKNOWN_LANGUAGE
} from '../lib/highlight.js'

const spell = ({ tokens }) => tokens.map(([text]) => text).join('')
const slugFor = ({ tokens }, text) => tokens.find(([t]) => t === text)?.[1]

test('the synthetic theme decodes back to every slug', () => {
    assert.equal(theme.tokenColors.length, rules.length)
    for (const { settings } of theme.tokenColors) {
        assert.ok(slugs.includes(decode(settings.foreground)))
        // Shiki may report the colour in upper case.
        assert.equal(decode(settings.foreground.toUpperCase()),
                     decode(settings.foreground))
    }
    assert.equal(decode('#000000'), null)
    assert.equal(decode(undefined), null)
})

test('languages include aliases', async () => {
    const { engine, languages } = await highlightLanguages()
    assert.equal(engine, 'shiki')
    for (const lang of ['python', 'emacs-lisp', 'elisp', 'sh', 'yaml', 'rust']) {
        assert.ok(languages.includes(lang), lang)
    }
})

test('python: tokens spell the code and carry the expected slugs', async () => {
    const code = 'def f(self, x: int = 3):\n    """doc"""\n    return x + 1  # c\n'
    const result = await highlight({ code, lang: 'python' })
    assert.equal(spell(result), code)
    assert.equal(slugFor(result, 'def'), 'k')
    assert.equal(slugFor(result, 'f'), 'f')
    assert.equal(slugFor(result, '"""doc"""'), 'd')
    assert.equal(slugFor(result, '+'), 'op')
    assert.equal(slugFor(result, '1'), 'n')
    assert.equal(slugFor(result, '#'), 'cd')
    for (const [, slug] of result.tokens) {
        assert.ok(slug === null || slugs.includes(slug))
    }
})

test('line breaks of every kind survive', async () => {
    for (const code of ['a = 1\r\nb = 2\r\n', 'a = 1\rb = 2', 'x', '', '\n\n']) {
        assert.equal(spell(await highlight({ code, lang: 'python' })), code)
    }
})

test('an unknown language is a distinct JSON-RPC error', async () => {
    await assert.rejects(highlight({ code: 'x', lang: 'no-such-lang' }),
                         (e) => e.code === UNKNOWN_LANGUAGE)
})

test('toPairs refuses tokens that do not spell the code', () => {
    assert.throws(() => toPairs('ab', [[{ content: 'x', color: '#000000' }]]))
    assert.throws(() => toPairs('ab', [[{ content: 'a', color: '#000000' }]]))
    assert.deepEqual(toPairs('a\nb', [[{ content: 'a' }], [{ content: 'b' }]]),
                     [['a', null], ['\n', null], ['b', null]])
})
