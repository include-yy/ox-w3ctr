// MathJax conversion: TeX fragments to MathML or SVG.
//
// MathJax is loaded on the first call, so a run that only highlights code
// never pays for its start-up.

let mathjaxPromise = null

// `ui/safe` filters the HTML attributes that TeX macros can inject.  Without
// it, the `html` extension (auto-loaded on demand) lets `\href{javascript:...}`
// and friends through unfiltered.
// `output/svg` is what creates `tex2svg' / `tex2svgPromise'; `tex2mml' is
// available regardless of the output jax.
const mathjax = () => mathjaxPromise ??= (async () => {
    const Mathjax = await import('mathjax')
    return Mathjax.init({
        loader: { load: ['input/tex', 'output/svg', 'ui/safe', 'adaptors/liteDOM'] }
    })
})()

// MathJax expects bare TeX, but ox-w3ctr hands over a whole fragment with its
// delimiters.  Pull out the math and note whether it was inline or display.
// A `\begin{...}...\end{...}` environment matches nothing and is display.
export const unwrap = (fragment) => {
    const match = fragment.match(/\\\(([\s\S]*?)\\\)|\\\[([\s\S]*?)\\\]/)
    if (!match) return { tex: fragment, display: true }
    const inline = match[1] !== undefined
    return { tex: inline ? match[1] : match[2], display: !inline }
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
export const tex2mml = async ({ fragment }) => {
    const { tex, display } = unwrap(fragment)
    const mml = await (await mathjax()).tex2mmlPromise(tex, { display })
    return stripNoise(mml)
}

export const escapeAttr = (s) => s
    .replace(/&/g, '&amp;')
    .replace(/"/g, '&quot;')
    .replace(/</g, '&lt;')

// MathJax wraps the SVG in a non-standard <mjx-container>, which would fail HTML
// validation, so keep only the <svg>.  Drop the `data-latex` annotations and
// give the image an accessible name.  Display math gets a phrasing wrapper
// (`.math-display'), so it stays valid inside a <p>.
export const tex2svg = async ({ fragment }) => {
    const { tex, display } = unwrap(fragment)
    const jax = await mathjax()
    const node = await jax.tex2svgPromise(tex, { display })
    const label = escapeAttr(tex.replace(/\s+/g, ' ').trim())
    const svg = jax.startup.adaptor.outerHTML(node)
        .replace(/^<mjx-container\b[^>]*>/, '')
        .replace(/<\/mjx-container>$/, '')
        .replace(/\s+data-latex(?:-item)?="[^"]*"/g, '')
        .replace(/^<svg /, `<svg aria-label="${label}" `)
    return display ? `<span class="math-display">${svg}</span>` : svg
}
