import { isAbsolute } from 'node:path'
import { pathToFileURL } from 'node:url'

// MathJax conversion: TeX fragments to MathML or SVG.
//
// MathJax is loaded on the first call, so a run that only highlights code
// never pays for its start-up.

let mathjaxPromise = null
let lastError = null
let queue = Promise.resolve()

// `ui/safe` filters the HTML attributes that TeX macros can inject.  Without
// it, the `html` extension (auto-loaded on demand) lets `\href{javascript:...}`
// and friends through unfiltered.
// `output/svg` is what creates `tex2svg' / `tex2svgPromise'; `tex2mml' is
// available regardless of the output jax.
// Drop `noundefined' so unknown macros reach `formatError' instead of
// rendering silently as red text.  Record the error and let MathJax finish
// formatting rather than throwing inside `formatError' -- throwing there
// poisons MathJax 4's internal startup promise for all subsequent calls.
// Wrap `loader.require' with `pathToFileURL' for absolute paths: MathJax 4.1
// passes raw Windows paths (`C:/...') to `import()', which Node ESM rejects.
const mathjax = () => mathjaxPromise ??= (async () => {
    const MathJax = (await import('mathjax')).default
    await MathJax.init({
        loader: {
            load: ['input/tex', 'output/svg', 'ui/safe'],
            require: (file) => import(
                isAbsolute(file) ? pathToFileURL(file).href : file),
        },
        tex: {
            packages: { '[-]': ['noundefined'] },
            formatError: (texJax, err) => {
                lastError = err
                return texJax.formatError(err)
            },
        },
    })
    return MathJax
})()

// Serialize conversions so `lastError' always belongs to the active call.
const runSerialized = (fn) => {
    const next = queue.then(async () => {
        lastError = null
        const result = await fn(await mathjax())
        if (lastError) {
            const err = lastError
            lastError = null
            throw new Error(err.message || String(err))
        }
        return result
    })
    queue = next.catch(() => {})
    return next
}

// MathJax 4 tags every node with `data-latex`, declares the namespace, and
// pretty-prints.  None of that is wanted in the exported HTML, so strip it and
// collapse the tree onto one line.  Attribute values are escaped, so neither
// `"` nor `>` can occur inside them.
export const stripNoise = (mml) => mml
    .replace(/\s+data-latex(?:-item)?="[^"]*"/g, '')
    .replace(/\s+xmlns="[^"]*"/g, '')
    .replace(/\s+display="inline"/g, '')
    .replace(/>\s+</g, '><')
    .trim()

// Use the promise-based conversion: the synchronous `tex2mml` throws
// "MathJax retry" as soon as the input needs an extension or extra font data,
// which v4 loads lazily.
export const tex2mml = ({ tex, display = true }) => runSerialized(async (jax) => {
    const mml = await jax.tex2mmlPromise(tex, {
        display: Boolean(display),
    })
    return stripNoise(mml)
})

export const escapeAttr = (s) => s
    .replace(/&/g, '&amp;')
    .replace(/"/g, '&quot;')
    .replace(/</g, '&lt;')

// MathJax wraps the SVG in a non-standard <mjx-container>, which would fail HTML
// validation, so keep only the <svg>.  Drop the `data-latex` annotations and
// give the image an accessible name.  Display math gets a phrasing wrapper
// (`.math-display'), so it stays valid inside a <p>.
export const tex2svg = ({ tex, display = true }) => runSerialized(async (jax) => {
    const isDisplay = Boolean(display)
    const node = await jax.tex2svgPromise(tex, { display: isDisplay })
    const label = escapeAttr(tex.replace(/\s+/g, ' ').trim())
    const svg = jax.startup.adaptor.outerHTML(node)
        .replace(/^<mjx-container\b[^>]*>/, '')
        .replace(/<\/mjx-container>$/, '')
        .replace(/\s+data-latex(?:-item)?="[^"]*"/g, '')
        .replace(/^<svg /, `<svg aria-label="${label}" `)
    return isDisplay ? `<span class="math-display">${svg}</span>` : svg
})
