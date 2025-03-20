import * as Mathjax from 'mathjax'
import { JSONRPC, JSONRPCServer } from 'json-rpc-2.0'

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

const server = new JSONRPCServer()

server.addMethod("echo", ({ text }) => text)
server.addMethod("add", ([a, b]) => a + b)

process.stdin.resume();
process.stdin.setEncoding('utf8')

process.stdin.on('data', (data) => {
    //const rpc_data = JSON.parse(input)
    server.receiveJSON(data).then((response) => {
	if (response) {
	    console.log(JSON.stringify(response));
	}
    })


    // if (res.method === 'add')
    // 	console.log(JSON.stringify({result: res.params[0] + res.params[1]}))
    // else
    // 	console.log(JSON.stringify({result: res.method}))
})

/*
process.stdin.on('data', (input) => {
    //    const data = input.trim();
    //    console.log(str2MathML(data))

})

*/
