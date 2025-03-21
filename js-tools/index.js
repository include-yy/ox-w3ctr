import * as readline from 'readline'
import { stdin as input, stdout as output} from 'process'

import * as Mathjax from 'mathjax'
import { JSONRPCServer } from 'json-rpc-2.0'

const server = new JSONRPCServer()
const TIMEOUT = 1000 * 10

const procTimeout = () => {
    let handle = setTimeout(() => {
	process.exit(0)
    }, TIMEOUT)
    return () => {
	clearTimeout(handle)
	handle = setTimeout(() => {
	    process.exit(0)
	}, TIMEOUT)
    }
}

const main = () => {
    const rl = readline.createInterface({input, output})
    const keepAlive = procTimeout()
    rl.on('line', (line) => {
	server.receiveJSON(line).then((response) => {

	    if (response) {
		console.log(JSON.stringify(response));
	    }
	})
    })
}

const str2MathML = await (async () => {
    const Jax2ML = await Mathjax.init({
	loader: {load: ['input/tex', 'adaptors/liteDOM']}
    })
    return (str) => {
	const regex = /^\s*\\\(/
	const mre = /\\\((.*?)\\\)|\\\[(.*?)\\\]/g;
	const display = regex.test(str)
	const match = mre.exec(str)
	const text = (match != null) ? (match[1] || match[2]) : str
	const result = Jax2ML.tex2mml(text, {display: !display})
	return result
    }
})()

server.addMethod("echo", ({ text }) => text)
server.addMethod("add", ([a, b]) => a + b)
server.addMethod("tex2mml", str2MathML)

main()
