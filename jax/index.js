import * as Mathjax from 'mathjax'

const Jax2ML = await Mathjax.init({
    loader: {load: ['input/tex', 'adaptors/liteDOM']}
})

const str2MathML = (str) => {
    const regex = /^\s*\\\(/
    const mre = /\\\((.*?)\\\)|\\\[(.*?)\\\]/g;
    const display = regex.test(str)
    const match = mre.exec(str)
    const text = (match != null) ? (match[1] || match[2]) : str
    const result = Jax2ML.tex2mml(text, {display: !display})
    return result
}

//console.log(JSON.stringify({a:1, b:2}));

process.stdin.resume();
process.stdin.setEncoding('utf8')

process.stdin.on('data', (input) => {
    const res = JSON.parse(input)
    console.log(JSON.stringify({result: res.method}))
})

/*
process.stdin.on('data', (input) => {
    //    const data = input.trim();
    //    console.log(str2MathML(data))

})

*/
