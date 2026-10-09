// Code highlighting with Shiki (TextMate grammars), as ox-w3ctr tokens.
//
// The result is a list of [TEXT, SLUG] pairs, never HTML: Emacs escapes the
// text and builds the markup itself.  The slug of a token is found by Shiki's
// own scope matching: a synthetic theme gives each slug of slugs.json a fake
// colour (#000001, #000002, ...), and the colour Shiki assigns to a token is
// decoded back into its slug.  Shiki is loaded on the first call, with the
// JavaScript regex engine (no WASM), and each grammar when first needed.

import { readFileSync } from 'node:fs'
import { JSONRPCErrorException } from 'json-rpc-2.0'

// The JSON-RPC error code for a language Shiki has no grammar for.  Emacs
// reads it as "decline", not as a failure.
export const UNKNOWN_LANGUAGE = -32010

const THEME = 'ox-w3ctr-slugs'

export const rules = JSON.parse(
    readFileSync(new URL('./slugs.json', import.meta.url), 'utf8')).rules

export const slugs = [...new Set(rules.map(([, slug]) => slug))]

// The fake colour of the Nth slug, and the table back.
const colour = (n) => '#' + (n + 1).toString(16).padStart(6, '0')
const slugOf = new Map(slugs.map((slug, n) => [colour(n), slug]))

// Return the slug for a colour Shiki reports, or null for the default colour.
export const decode = (c) => slugOf.get((c ?? '').slice(0, 7).toLowerCase()) ?? null

export const theme = {
    name: THEME,
    type: 'light',
    fg: '#000000',
    bg: '#ffffff',
    tokenColors: rules.map(([scope, slug]) => ({
        scope,
        settings: { foreground: colour(slugs.indexOf(slug)) }
    }))
}

let shikiPromise = null
const shiki = () => shikiPromise ??= (async () => {
    const lib = await import('shiki')
    const highlighter = await lib.createHighlighter({
        themes: [theme],
        langs: [],
        engine: lib.createJavaScriptRegexEngine({ forgiving: true })
    })
    return { lib, highlighter }
})()

// The `highlightLanguages' method: the language names (aliases included)
// that `highlight' accepts.
export const highlightLanguages = async () => {
    const { lib } = await shiki()
    return {
        engine: 'shiki',
        languages: Object.keys(lib.bundledLanguages).sort()
    }
}

// Rebuild CODE as [TEXT, SLUG] pairs from Shiki's LINES of tokens.  Shiki
// drops the line breaks; they are taken back from CODE itself, so "\r\n" and
// "\r" survive.  Throw if the tokens do not spell CODE.
export const toPairs = (code, lines) => {
    const pairs = []
    let pos = 0
    lines.forEach((line, i) => {
        for (const token of line) {
            if (token.content === '') continue
            if (!code.startsWith(token.content, pos)) {
                throw new Error(`token ${JSON.stringify(token.content)} does not match the code at ${pos}`)
            }
            pairs.push([token.content, decode(token.color)])
            pos += token.content.length
        }
        if (i < lines.length - 1) {
            const sep = code.startsWith('\r\n', pos) ? '\r\n' : code[pos]
            if (sep !== '\n' && sep !== '\r' && sep !== '\r\n') {
                throw new Error(`no line break in the code at ${pos}`)
            }
            pairs.push([sep, null])
            pos += sep.length
        }
    })
    if (pos !== code.length) throw new Error('the tokens do not cover the code')
    return pairs
}

// The `highlight' method: CODE in LANG as [TEXT, SLUG] pairs.
export const highlight = async ({ code, lang }) => {
    const { lib, highlighter } = await shiki()
    if (typeof code !== 'string' || typeof lang !== 'string') {
        throw new Error('highlight needs a string code and lang')
    }
    if (!Object.hasOwn(lib.bundledLanguages, lang)) {
        throw new JSONRPCErrorException(`Unknown language: ${lang}`, UNKNOWN_LANGUAGE)
    }
    if (!highlighter.getLoadedLanguages().includes(lang)) {
        await highlighter.loadLanguage(lang)
    }
    const lines = highlighter.codeToTokensBase(code, { lang, theme: THEME })
    return { tokens: toPairs(code, lines) }
}
