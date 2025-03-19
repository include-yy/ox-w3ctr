require('mathjax').init({
    loader: {load: ['input/tex', 'output/chtml']}
}).then((MathJax) => {
    console.log(MathJax)
})
